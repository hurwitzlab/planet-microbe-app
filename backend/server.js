'use strict';

const fs = require('fs');
const express = require('express');
const config = require('./config.json');
const db = require('./db.js')(config);

class TermIndex {
    constructor() {}

    async build(db, ontologies) {
        this.index = await generateTermIndex(db, ontologies);
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
    //let result = await query('select fields[1].string from sample limit 1')
    let schemas = await db.query("SELECT schema_id,name,fields FROM schema");
    //console.log(schemas.rows);

    let index = {};
    ontologyDescriptors.forEach( desc => {
        console.log("Indexing ontology", desc.name);
        loadOntology(desc.type, desc.path, index);
    });

    schemas.rows.forEach( schema => {
        console.log("Indexing schema", schema.name);

        for (let i = 0; i < schema.fields.fields.length; i++) {
            let field = schema.fields.fields[i];

            let purl = field.rdfType;
            let unitPurl = field['pm:unitRdfType'];
            if (!purl || ('pm:searchable' in field && !field['pm:searchable']))
                continue; // skip this field if no PURL or not searchable

            if (!(purl in index)) {
                console.log("WARNING: missing", purl, "in index (not defined in an ontology)");
                index[purl] = {};
            }
            else { // Check type consistency
                if (index[purl].type && index[purl].type != field.type)
                    console.log("WARNING: type mismatch for", purl, index[purl].type, field.type);
                if (index[purl].unitId && index[purl].unitId != unitPurl)
                    console.log("WARNING: unit mismatch for", purl, index[purl].unitId, unitPurl);
            }

            if (!('schemas' in index[purl]))
                index[purl]['schemas'] = {};
            if (!(schema.schema_id in index[purl]['schemas']))
                index[purl]['schemas'][schema.schema_id] = []

            index[purl]['schemas'][schema.schema_id].push({
                schemaId: schema.schema_id,
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
// Removed 11/27/19 -- no longer needed, pmo.json has all term definitions
//        else if (type == "term_owl") { // pmo_owl.json
//            ontology.classAttribute.forEach(node => {
//                let label = "<unknown>";
//                if (node["label"]) {
//                    if (node["label"]["en"])
//                        label = node["label"]["en"];
//                    else if (node["label"]["undefined"])
//                        label = node["label"]["undefined"];
//                }
//
//                if (!(node.iri in index))
//                    index[node.iri] = {};
//                index[node.iri] = Object.assign(index[node.iri],
//                    {
//                        id: node.iri,
//                        label: label,
//                    }
//                );
//            });
//        }
        else {
            throw("Error: unsupported ontology type");
        }
    }
    else if (path.endsWith(".csv")) {
        var data = fs.readFileSync(path, { encoding: "UTF8" });
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

// Initialize
(async function() {
    let termIndex = new TermIndex();
    await termIndex.build(db, config.ontologies);

    const app = express();
    app.set('termIndex', termIndex);
    require('./routes/routes.js')(app);

    app.listen(config.serverPort, () => console.log('Server listening on port', config.serverPort));
})();