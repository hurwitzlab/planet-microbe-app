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

    let ontologies = config.ontologies.map(path => {
        var json = fs.readFileSync(path);
        return JSON.parse(json);
    });

    rdfTermIndex = await generateTermIndex(db, ontologies);
    console.log("index:", JSON.stringify(rdfTermIndex, null, 4));

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

//----------------------------------------------------------------------------------------------------------------------

async function generateTermIndex(db, ontologies) {
    //let result = await query('select fields[1].string from sample limit 1')
    let schemas = await query("SELECT schema_id,name,fields FROM schema");
    //console.log(schemas.rows);

    let index = {};

    ontologies.forEach( ontology => {
        console.log("Indexing ontology", ontology.name);
        ontology.graphs.forEach( g => {
            g.nodes.forEach(n => {
                if (n.id in index)
                    throw("Error: duplicate PURL");

                index[n.id] = {
                    id: n.id,
                    label: n.lbl
                };
            })
        });
    });

    schemas.rows.forEach( schema => {
        console.log("Indexing schema", schema.name);

        for (let i = 0; i < schema.fields.fields.length; i++) {
            let field = schema.fields.fields[i];
            let rdf = field.rdfType;
            if (!rdf)
                rdf = 'http://planetmicrobe.org/temppurl/PM_'+ shortid.generate(); //FIXME hardcoded URL
            if (!(rdf in index))
                index[rdf] = {};
            if (!('schemas' in index[rdf]))
                index[rdf]['schemas'] = {};
            if (!index[rdf]['schemas'][schema.schema_id])
                index[rdf]['schemas'][schema.schema_id] = {};
            index[rdf]['schemas'][schema.schema_id][field.name] = i+1 
            index[rdf]['type'] = field.type;
        }
    });

    return index;
}

async function search(db, params) {
    console.log("params:", params);

    let limit = 20, offset, sort;
    let gisClause;
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
            if (val.match(/\[-?\d*(\.\d+)?\,-?\d*(\.\d+)?\]/)) { // [lat, lng] exact match
                //TODO
            }
            else if (val.match(/\[-?\d*(\.\d+)?\,-?\d*(\.\d+)?,-?\d*(\.\d+)?\]/)) { // [lat, lng, radius] in meters
                let bounds = JSON.parse(val);
                //selections.push("ST_AsText(location::geometry)");
                selections.push("ST_AsText(location::geography)");
                //clauses[0].push("ST_Distance_Sphere(location::geometry, ST_MakePoint(" + bounds[0] + "," + bounds[1] + ")) <= " + bounds[2]);
                gisClause = "ST_DWithin(ST_MakePoint(" + bounds[0] + "," + bounds[1] + ")::geography, location, " + bounds[2] + ")";
            }
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
                    if (val === '') // empty - show in results
                        continue;
                    else if (!isNaN(val)) { // numeric exact match
                        field = "number_vals[" + arrIndex + "]";
                        clause = field + "=" + parseFloat(val);
                    }
                    else if (val.match(/\[-?\d*(\.\d+)?\,-?\d*(\.\d+)?\]/)) { // numeric range query
                        if (term.type == "number") {
                            bounds = JSON.parse(val);
                            field = "number_vals[" + arrIndex + "]";
                            clause = field + " BETWEEN " + bounds[0] + " AND " + bounds[1]; //field + ">" + bounds[0] + " AND " + field + "<" + bounds[1];
                        }
                        else {
                            //TODO error
                        }
                    }
                    else if (bounds = val.match(/\[([\d\-\ ]+)\,([\d\-\ ]+)\]/)) { // date/time range query
                        if (term.type == "datetime") {
                            field = "datetime_vals[" + arrIndex + "]";
                            clause = field + ">=timestamp'" + bounds[1] + "' AND " + field + "<=timestamp'" + bounds[2] + "'";
                        }
                        else {
                            //TODO error
                        }
                    }
                    else if (val.match(/^\d+[\d\-]+/)) { // date/time exact match
                        field = "datetime_vals[" + arrIndex + "]";
                        clause = field + "=timestamp'" + val + "'";
                    }
                    else if (val.match(/^\~\w+/)) { // partial string match
                        val = val.substr(1);
                        field = "string_vals[" + arrIndex + "]";
                        clause = "LOWER(" + field + ") LIKE " + "'%" + val.toLowerCase() + "%'";
                    }
                    else if (val.match(/\w+/)) { // literal string match
                        field = "string_vals[" + arrIndex + "]";
                        clause = field + "=" + "'" + val + "'";
                    }
                    else {
                        console.log("Error: invalid query");
                        //TODO
                    }

                    selectStr += " WHEN schema_id=" + schemaId + " THEN " + field;
                    if (!clauses[schemaId])
                        clauses[schemaId] = []
                    clauses[schemaId].push(clause);
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

    if (clauseStr)
        clauseStr = "WHERE " + clauseStr;

    let sortDir = (typeof sort !== 'undefined' && sort > 0 ? "ASC" : "DESC");
    let sortStr = (typeof sort !== 'undefined' ? " ORDER BY " + (Math.abs(sort) + 2) + " " + sortDir : "");

    let countQueryStr = "SELECT count(*) FROM sample " + clauseStr;

    if (!limit)
        limit = 50;
    let limitStr = (limit ? " LIMIT " + limit : "");
    let offsetStr = (offset ? " OFFSET " + offset : "");

    let queryStr = "SELECT " + ["schema_id", "sample_id"].concat(selections).join(",") + " FROM sample " + clauseStr + sortStr + offsetStr + limitStr;

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
        results: results.rows.map(r => { return { schemaId: r[0], sampleId: r[1], values: r.slice(2) } })
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
