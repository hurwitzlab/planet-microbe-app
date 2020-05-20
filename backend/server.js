'use strict';

const fs = require('fs');
const express = require('express');
const config = require('./config.json');
const client = require('./postgres');

class TermIndex {
    constructor(props) {
        this.searchableTerms = props.searchableTerms;
    }

    async build(db, ontologies) {
        this.index = await generateTermIndex(db, ontologies);
    }

    getTerms() {
        return this.index;
    }

    getSearchableTerms() {
        return this.searchableTerms.map(purl => this.index[purl])
    }

    getTerm(nameOrId) {
        if (nameOrId) {
            if (nameOrId in this.index)
                return this.index[nameOrId];

            nameOrId = nameOrId.toLowerCase();
            for (let term of Object.values(this.index)) {
                let id = term.id ? term.id.toLowerCase() : '';
                let label = term.label ? term.label.toLowerCase() : '';

                if (nameOrId === id || id.endsWith(nameOrId) || nameOrId === label || label.includes(nameOrId))
                    return term;
            }
        }

        return null;
    }

    getLabelForValue(val) {
        if (val in this.index)
            return this.getLabelForValue(this.index[val].label);
        else
            return val;
    }
}

async function generateTermIndex(db, ontologyDescriptors) {
    const schemas = await db.query("SELECT schema_id,name,type,fields FROM schema");

    let index = {};
    ontologyDescriptors.forEach(desc => {
        console.log("Indexing ontology", desc.name);
        loadOntology(desc.type, desc.path, index);
    });

    schemas.rows.forEach(schema => {
        console.log(`Indexing schema ${schema.name} (id=${schema.schema_id})`);

        for (let i = 0; i < schema.fields.fields.length; i++) {
            const field = schema.fields.fields[i];

            const purl = field.rdfType;
            const unitPurl = field['pm:unitRdfType'];
            if (!purl) // || ('pm:searchable' in field && !field['pm:searchable']))
                continue; // skip this field if no PURL or not searchable

            if (!(purl in index)) {
                console.log("WARNING: missing", purl, "in index (not defined in an ontology)");
                index[purl] = {};
            }
            else { // Check type consistency
                if (index[purl].type && index[purl].type != field.type)
                    console.log("WARNING: type mismatch for", purl, index[purl].type, "!=", field.type);
                if (index[purl].unitId && index[purl].unitId != unitPurl)
                    console.log(`WARNING: unit mismatch for term ${purl} (${index[purl].label}): ${index[purl].unitId} (${index[purl].unitLabel}) != ${unitPurl} (${index[unitPurl].label})`);
            }

            if (!('schemas' in index[purl]))
                index[purl]['schemas'] = {};
            if (!(schema.schema_id in index[purl]['schemas']))
                index[purl]['schemas'][schema.schema_id] = []

            index[purl]['schemas'][schema.schema_id].push({
                schemaId: schema.schema_id,
                schemaName: schema.name,
                schemaType: schema.type,
                position: i + 1,
                //type: field.type,
                name: field.name,
                sourceName: field.name, //FIXME
                sourceUrl: field['pm:sourceUrl'] || ''
            });

            index[purl]['type'] = field.type;

            if (unitPurl && unitPurl in index) {
                index[purl]['unitId'] = unitPurl;
                index[purl]['unitLabel'] = index[unitPurl]['label'];
            }
        }
    });

    return index;
}

function loadOntology(type, path, index) {
    if (path.endsWith(".json")) {
        let ontology = JSON.parse(fs.readFileSync(path));

        if (type == "term") { // pmo.json
            ontology.graphs.forEach( g => {
                g.nodes.forEach(node => {
                    let annotations = [];
                    if (node.meta && node.meta.basicPropertyValues)
                        annotations = node.meta.basicPropertyValues.map(prop => { return { id: prop.pred, value: prop.val }; });

                    if (!(node.id in index))
                        index[node.id] = {};
                    index[node.id] = Object.assign(index[node.id],
                        {
                            id: node.id,
                            label: node.lbl,
                            definition: (node.meta && node.meta.definition ? node.meta.definition.val : null),
                            annotations: annotations
                        }
                    );
                });
            });
        }
        else {
            throw("Error: unsupported ontology type");
        }
    }
    else if (path.endsWith(".csv")) {
        let data = fs.readFileSync(path, { encoding: "UTF8" });
        data.split('\n').forEach(line => {
            let fields = line.split(',');
            let purl = fields[0];
            let label = fields[1];
            index[purl] = {
                id: purl,
                label : label
            }
        })
    }
    else {
        throw("Error: unsupported ontology file format");
    }

    return index;
}

function loadSearchableTerms(path) { // TSV file
    const data = fs.readFileSync(path, { encoding: "UTF8" });
    const terms =
        data
        .split('\n')
        .filter(line => !line.startsWith('#'))
        .map(line => line.split('\t')[0]);
    return terms;
}

// Initialize
(async function() {
    const searchableTerms = loadSearchableTerms(config.searchableTerms);
    const termIndex = new TermIndex({ searchableTerms: searchableTerms });
    await termIndex.build(client, config.ontologies);

    const app = express();
    app.set('termIndex', termIndex);
    require('./routes/routes.js')(app);

    app.listen(config.serverPort, () => console.log('Server listening on port', config.serverPort));
})();