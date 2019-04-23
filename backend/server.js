const express = require('express');
const app = express();
const cors = require('cors');
const logger = require('morgan');
const Promise = require('promise');
const {Client} = require('pg');
const shortid = require('shortid');
const fs = require('fs');
const stringSimilarity = require('string-similarity');
const config = require('./config.json');

var rdfTermIndex = {};
var db;

// Initialize
(async function() {
    db = new Client({
        user: config.postgres.username,
        host: 'localhost',
        database: config.postgres.db,
        password: config.postgres.password
    });
    await db.connect();

    rdfTermIndex = await generateTermIndex(db, config.ontologies);
    //console.log("index:", JSON.stringify(rdfTermIndex, null, 4));

    app.listen(config.serverPort, () => console.log('Server listening on port', config.serverPort));
})();

//----------------------------------------------------------------------------------------------------------------------

app.use(logger('dev'));
app.use(cors());

app.get('/index', (req, res) => { //TODO rename to "catalog", as in a catalog of terms
    res.json(rdfTermIndex);
});

app.get('/searchTerms', (req, res) => {
    let query = req.query.query;

    if (query) {
        const MIN_MATCH_RATING = 0.1;
        let labels = Object.values(rdfTermIndex).map(term => term.label).filter(label => label);
        let matches = stringSimilarity.findBestMatch(query, labels);
        let bestLabels = matches.ratings.filter(m => m.rating > MIN_MATCH_RATING).map(m => m.target);
        let terms = Object.values(rdfTermIndex).filter(term => bestLabels.includes(term.label));
        res.json(terms);
    }
    else {
        res.json(
            Object.values(rdfTermIndex)
            .filter(term => term.id && term.schemas)
            .map(term => {
                let aliases = Array.from(new Set(Object.values(term.schemas).reduce((acc, schema) => acc.concat(Object.keys(schema)), [])));
                return {
                    id: term.id,
                    label: term.label,
                    unitLabel: term.unitLabel,
                    type: term.type,
                    aliases: aliases
                };
            })
        );
    }
});

app.get('/searchTerms/:id(\\S+)', async (req, res) => {
    let id = req.params.id;
    let term = getTerm(id);

    if (!term) {
        res.status(404).json({ error: "Term not found" });
        return;
    }
    console.log("term:", term);

    let aliases = Array.from(new Set(Object.values(term.schemas).reduce((acc, schema) => acc.concat(Object.keys(schema)), [])));

    if (term.type == "string") {
        let uniqueVals = {};
        for (let schemaId in term.schemas) {
            console.log("schemaId:", schemaId);
            for (let alias in term.schemas[schemaId]) {
                let arrIndex = term.schemas[schemaId][alias];
                let vals = await query({ text: "SELECT string_vals[$1],count(string_vals[$1]) FROM sample WHERE schema_id=$2 GROUP BY string_vals[$1]", values: [arrIndex,schemaId*1], rowMode: 'array'});
                vals.rows.forEach(row => {
                    let val = row[0];
                    if (!uniqueVals[val])
                        uniqueVals[val] = 0;
                    uniqueVals[val] += 1*row[1];
                });
            }
        }

        res.json({
            id: term.id,
            label: term.label,
            unitLabel: term.unitLabel,
            type: term.type,
            aliases: aliases,
            values: uniqueVals
        });
// TODO
//        })
//        .catch(err => {
//            console.log(err);
//            res.send(err);
//        });
    }
    else if (term.type == "number") { // numeric
        let queries = [];
        for (let schemaId in term.schemas) {
            for (let alias in term.schemas[schemaId]) {
                let arrIndex = term.schemas[schemaId][alias];
                queries.push({ text: "SELECT MIN(number_vals[$1]),MAX(number_vals[$1]) FROM sample WHERE schema_id=$2", values: [arrIndex,schemaId*1], rowMode: 'array' });
            }
        }

        let min, max;
        let results = await batchQuery(queries); // FIXME would a single query be faster?
        results.forEach(vals => {
            vals.rows.forEach(row => {
                if (typeof row[0] !== "undefined" && typeof row[1] !== "undefined") {
                    min = (typeof min === "undefined" ? row[0] : Math.min(min, row[0]));
                    max = (typeof max === "undefined" ? row[1] : Math.max(max, row[1]));
                }
            });
        });

        res.json({
            id: term.id,
            label: term.label,
            unitLabel: term.unitLabel,
            type: term.type,
            aliases: aliases,
            min: min,
            max: max
        });
// TODO
//        .catch(err => {
//            console.log(err);
//            res.send(err);
//        });
    }
    else if (term.type == "datetime") {
        let queries = [];
        for (let schemaId in term.schemas) {
            for (let alias in term.schemas[schemaId]) {
                let arrIndex = term.schemas[schemaId][alias];
                queries.push({ text: "SELECT MIN(datetime_vals[$1]),MAX(datetime_vals[$1]) FROM sample WHERE schema_id=$2", values: [arrIndex,schemaId*1], rowMode: 'array' });
            }
        }

        let min, max;
        let results = await batchQuery(queries); // FIXME would a single query be faster?
        results.forEach(vals => {
            vals.rows.forEach(row => {
                if (typeof row[0] !== "undefined" && typeof row[1] !== "undefined") {
                    min = (typeof min === "undefined" ? row[0].getTime() : Math.min(min, row[0].getTime()));
                    max = (typeof max === "undefined" ? row[1].getTime() : Math.max(max, row[1].getTime()));
                }
            });
        });


        res.json({
            id: term.id,
            label: term.label,
            unitLabel: term.unitLabel,
            type: term.type,
            aliases: aliases,
            min: new Date(min).toISOString(),
            max: new Date(max).toISOString()
        });
// TODO
//        .catch(err => {
//            console.log(err);
//            res.send(err);
//        });
    }
    else {
        console.log("ERROR: unknown term type '" + term.type + "'");
        res.status(500).json({
            error: "Unknown term type"
        });
    }
});

app.get('/search', async (req, res) => {
    let results = await search(db, req.query);
    res.json(results);
//    .catch(err => {
//        console.log(err);
//        res.send(err);
//    });
});

app.get('/projects', async (req, res) => {
    let result = await query({ text: "SELECT name,count(sample_id) FROM project JOIN project_to_sample ON project.project_id=project_to_sample.project_id GROUP BY name"});
    result.rows.forEach(row => row.count *= 1); // convert count to int
    res.json(result.rows);
//    .catch(err => {
//        console.log(err);
//        res.send(err);
//    });
});

//----------------------------------------------------------------------------------------------------------------------

async function generateTermIndex(db, ontologyDescriptors) {
    //let result = await query('select fields[1].string from sample limit 1')
    let schemas = await query("SELECT schema_id,name,fields FROM schema");
    //console.log(schemas.rows);

    let index = {};
    ontologyDescriptors.forEach( desc => {
        console.log("Indexing ontology", desc.name);
        load_ontology(desc.type, desc.path, index);
    });

    schemas.rows.forEach( schema => {
        console.log("Indexing schema", schema.name);

        for (let i = 0; i < schema.fields.fields.length; i++) {
            let field = schema.fields.fields[i];

            let purl = field.rdfType;
            if (!purl || !field["pm:searchable"])
                continue; // skip this field if no PURL value (not in term catalog)

            if (!(purl in index))
                index[purl] = {};
            if (!('schemas' in index[purl]))
                index[purl]['schemas'] = {};
            if (!index[purl]['schemas'][schema.schema_id])
                index[purl]['schemas'][schema.schema_id] = {};

            index[purl]['schemas'][schema.schema_id][field.name] = i+1
            index[purl]['type'] = field.type;

            let unitPurl = field['pm:unitRdfType'];
            if (unitPurl && unitPurl in index)
                index[purl]['unitLabel'] = index[unitPurl]['label'];
        }
    });

    return index;
}

function load_ontology(type, path, index) {
    if (path.endsWith(".json")) {
        var json = fs.readFileSync(path);
        ontology = JSON.parse(json);
//          ontology.graphs.forEach( g => {
//            g.nodes.forEach(node => {
//                if (node.id in index)
//                    throw("Error: duplicate PURL");
//
//                index[node.id] = {
//                    id: node.id,
//                    label: node.lbl
//                };
//            })
//          });
        ontology.classAttribute.forEach(node => {
            let label = "<unknown>";
            if (node["label"]) {
                if (node["label"]["en"])
                    label = node["label"]["en"];
                else if (node["label"]["undefined"])
                    label = node["label"]["undefined"];
            }

            index[node.iri] = {
                id: node.iri,
                label: label
            }
        });
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

async function search(db, params) {
    console.log("params:", params);

    let limit = 20, offset, sort;
    let gisClause, projectClause;
    let clauses = {};
    let selections = [];

    for (param in params) {
        param = param.replace(/\+/gi, ''); // work around for Elm uri encoding
        let val = params[param];

        if (param == 'limit')
            limit = val * 1; // convert to int
        else if (param == 'offset')
            offset = val * 1; // convert to int
        else if (param == 'sort')
            sort = val * 1; // convert to int
        else if (param == 'location') {
//            if (val.match(/\[-?\d*(\.\d+)?\,-?\d*(\.\d+)?\]/)) { // [lat, lng] exact match
//                //TODO
//            }
            if (val.match(/\[-?\d*(\.\d+)?\,-?\d*(\.\d+)?,-?\d*(\.\d+)?\]/)) { // [lat, lng, radius] in meters
                let bounds = JSON.parse(val);
                console.log("location:", bounds);
                selections.push("ST_AsText(locations::geography)");
                gisClause = "ST_DWithin(ST_MakePoint(" + bounds[0] + "," + bounds[1] + ")::geography, locations, " + bounds[2] + ")";
            }
        }
        else if (param == 'project') {
            let vals = val.split("|");
            console.log("project match", vals);
            projectClause = "LOWER(project.name) IN (" + vals.map(s => "'" + s + "'").join(",").toLowerCase() + ")";
        }
        else {
            let term = getTerm(param);
            if (!term) {
                console.log("Error: term not found for param '" + param + "'");
                return;
            }
            console.log("term:", term);

            let selectStr = "";

            for (schemaId in term.schemas) {
                for (alias in term.schemas[schemaId]) {
                    let arrIndex = term.schemas[schemaId][alias];

                    // FIXME should use query substitution here -- SQL injection risk
                    let field, clause, bounds;
                    if (val === '') { // empty - don't query just show in results
                        if (term.type == "string")
                            field = "string_vals[" + arrIndex + "]";
                        else if (term.type == "number")
                            field = "number_vals[" + arrIndex + "]";
                        else if (term.type == "datetime")
                            field = "datetime_vals[" + arrIndex + "]";
                    }
                    else if (!isNaN(val)) { // numeric exact match
                        if (term.type == "number") {
                            console.log("numeric exact match");
                            field = "number_vals[" + arrIndex + "]";
                            clause = field + "=" + parseFloat(val);
                        }
                        else {
                            //TODO error
                            console.log("Error: numeric exact query not supported for type", term.type);
                        }
                    }
                    else if (val.match(/^\[-?\d*(\.\d+)?\,-?\d*(\.\d+)?\]/)) { // numeric range query
                        if (term.type == "number") {
                            console.log("numeric range query");
                            bounds = JSON.parse(val);
                            field = "number_vals[" + arrIndex + "]";
                            clause = field + " BETWEEN " + bounds[0] + " AND " + bounds[1];
                        }
                        else {
                            //TODO error
                            console.log("Error: numeric range query not supported for type", term.type);
                        }
                    }
                    else if (val.match(/^-?\d*(\.\d+)?\,-?\d*(\.\d+)?$/)) { // numeric offset query
                        if (term.type == "number") {
                            console.log("numeric offset query");
                            bounds = val.split(",");
                            field = "number_vals[" + arrIndex + "]";
                            clause = field + " BETWEEN " + (1*bounds[0] - 1*bounds[1]) + " AND " + (1*bounds[0] + 1*bounds[1]);
                        }
                        else {
                            //TODO error
                            console.log("Error: numeric offset query not supported for type", term.type);
                        }
                    }
                    else if (val.match(/^\~(\w+)(\|\w+)*/)) { // partial string match on one or more values
                        if (term.type == "string") {
                            let vals = val.substr(1).split("|");
                            console.log("partial string match on multiple values", vals);
                            field = "string_vals[" + arrIndex + "]";
                            if (vals.length == 1)
                                clause = "LOWER(" + field + ") LIKE " + "'%" + vals[0].toLowerCase() + "%'"; // may not be necessary if equivalent to SIMILAR TO
                            else
                                clause = "LOWER(" + field + ") SIMILAR TO " + "'%(" + vals.join("|").toLowerCase() + ")%'";
                        }
                        else {
                            //TODO error
                            console.log("Error: string similarity query not supported for type", term.type);
                        }
                    }
                    else if (val.match(/^(\w+)(\|\w+)*/)) { // literal string match on one or more values
                        if (term.type == "string") {
                            let vals = val.split("|");
                            console.log("literal string match", vals);
                            field = "string_vals[" + arrIndex + "]";
                            clause = field + "=" + "'" + val + "'";
                            if (vals.length == 1)
                                clause = field + "=" + "'" + val + "'"; // may not be necessary if equivalent to IN()
                            else
                                clause = "LOWER(" + field + ") IN (" + vals.map(s => "'" + s + "'").join(",").toLowerCase() + ")";
                        }
                        else {
                            //TODO error
                            console.log("Error: string literal query not supported for type", term.type);
                        }
                    }
                    else if (bounds = val.match(/^\[(\d{4}\-\d{2}\-\d{2})\,(\d{4}\-\d{2}\-\d{2})\]$/)) { // date/time range query
                        if (term.type == "datetime" || term.type == "date") {
                            console.log("datetime range query");
                            field = "datetime_vals[" + arrIndex + "]";
                            clause = field + ">=timestamp'" + bounds[1] + "' AND " + field + "<=timestamp'" + bounds[2] + "'";
                        }
                        else {
                            //TODO error
                            console.log("Error: datetime range query not supported for type", term.type);
                        }
                    }
                    else if (val.match(/^(\d{4}\-\d{2}\-\d{2})/)) { // date/time exact match
                        if (term.type == "datetime" || term.type == "date") {
                            console.log("exact datetime match");
                            field = "datetime_vals[" + arrIndex + "]";
                            clause = field + "=timestamp'" + val + "'";
                        }
                        else {
                            //TODO error
                            console.log("Error: datetime exact query not supported for type", term.type);
                        }
                    }
                    else {
                        console.log("Error: invalid query syntax");
                        throw("Invalid query syntax")
                    }

                    selectStr += " WHEN schema_id=" + schemaId + " THEN " + field;
                    if (clause) {
                        if (!clauses[schemaId])
                            clauses[schemaId] = []
                        clauses[schemaId].push(clause);
                    }
                }
            }

            if (selectStr)
                selections.push("CASE" + selectStr + " END");
        }
    }
    console.log("selections:", selections);
    console.log("clauses:", clauses);

    let clauseStr =
        Object.keys(clauses).map(schemaId =>
            "(schema_id=" + schemaId + " AND " + Object.values(clauses[schemaId]).join(" AND ") + ")"
        )
        .join(" OR ");

    if (gisClause)
        clauseStr = gisClause + (clauseStr ? " AND (" + clauseStr + ")" : "");

    if (projectClause)
        clauseStr = projectClause + (clauseStr ? " AND (" + clauseSTr + ")": "");

    if (clauseStr)
        clauseStr = "WHERE " + clauseStr;

    let sortDir = (typeof sort !== 'undefined' && sort > 0 ? "ASC" : "DESC");
    let sortStr = (typeof sort !== 'undefined' ? " ORDER BY " + (Math.abs(sort)+3) + " " + sortDir : "");

    let countQueryStr = "SELECT count(*) FROM sample JOIN project_to_sample ON project_to_sample.sample_id=sample.sample_id JOIN project ON project.project_id=project_to_sample.project_id " + clauseStr;

    if (!limit)
        limit = 50;
    let limitStr = (limit ? " LIMIT " + limit : "");
    let offsetStr = (offset ? " OFFSET " + offset : "");

    let queryStr = "SELECT " + ["schema_id", "sample.sample_id", "project.project_id", "project.name"].concat(selections).join(",") +
        " FROM sample JOIN project_to_sample ON project_to_sample.sample_id=sample.sample_id JOIN project ON project.project_id=project_to_sample.project_id " +
        clauseStr + sortStr + offsetStr + limitStr;

    let count = await query({
        text: countQueryStr,
        values: [],
        rowMode: 'array',
    });

    let results = await query({
        text: queryStr,
        values: [],
        rowMode: 'array',
    });

    return {
        count: count.rows[0][0]*1,
        results: results.rows.map(r => {
            return {
                schemaId: r[0],
                sampleId: r[1],
                projectId: r[2],
                projectName: r[3],
                values: r.slice(4).map(v => typeof v == "undefined" ? "" : v ) // kludge to convert null to empty string
            }
        })
    };
}

function getTerm(nameOrId) {
    for (term of Object.values(rdfTermIndex)) {
        if ((term.id && (nameOrId === term.id || term.id.endsWith(nameOrId))) || (term.label && (nameOrId === term.label || term.label.includes(nameOrId)))) {
            return term;
        }
    }

    return null;
}

function query(queryStrOrObj, params) {
    console.log(queryStrOrObj, params || "");
    return db.query(queryStrOrObj, params);
}

async function batchQuery(queryObjArray) {
  const promises = queryObjArray.map(obj => query(obj));
  return await Promise.all(promises);
}
