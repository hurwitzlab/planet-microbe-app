const express = require('express');
const app = express();
const cors = require('cors');
const logger = require('morgan');
const Promise = require('promise');
const {Client} = require('pg');
const shortid = require('shortid');
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

    rdfTermIndex = await generateTermIndex(db);
    console.log("index:", JSON.stringify(rdfTermIndex, null, 4));

    app.listen(config.serverPort, () => console.log('Server listening on port', config.serverPort));
})();

//----------------------------------------------------------------------------------------------------------------------

app.use(logger('dev'));
app.use(cors());

app.get('/index', (req, res) => {
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
                let vals = await query({ text: "SELECT DISTINCT(string_vals[$1]) FROM sample WHERE schema_id=$2", values: [arrIndex,schemaId*1], rowMode: 'array'});
                vals.rows.forEach(row => {
                    uniqueVals[row[0]] = 1;
                });
            }
        }

        res.json({
            id: term.id,
            label: term.label,
            type: term.type,
            aliases: aliases,
            values: Object.keys(uniqueVals).sort()
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
                console.log(row);
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
// TODO
//    else if (term.type == "datetime") {
//    }
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

async function generateTermIndex(db) {
    const ontologies = [
        {
          "name": "envo",
          "terms": [
            {
              "id": "http://purl.obolibrary.org/obo/IAO_0000578",
              "label": "centrally registered identifier"
            },
            {
              "id": "http://purl.obolibrary.org/obo/IAO_0000577",
              "label": "centrally registered identifier symbol"
            },
            {
              "id": "http://purl.obolibrary.org/obo/BFO_0000148",
              "label": "zero-dimensional temporal region"
            },
            {
              "id": "http://purl.obolibrary.org/obo/OBI_0001620",
              "label": "latitude coordinate measurement datum"
            },
            {
              "id": "http://purl.obolibrary.org/obo/OBI_0001621",
              "label": "longitude coordinate measurement datum"
            },
            {
              "id": "http://purl.obolibrary.org/obo/ENVO_00000428",
              "label": "biome"
            },
            {
              "id": "http://purl.obolibrary.org/obo/ENVO_00002297",
              "label": "environmental feature"
            },
            {
              "id": "http://purl.obolibrary.org/obo/ENVO_00010483",
              "label": "environmental material"
            },
            {
              "id": "http://purl.obolibrary.org/obo/ENVO_09200014",
              "label": "temperature of water"
            },
            {
              "id": "http://planetmicrobe.org/purl/PM_00000001",
              "label": "salinity of water"
            },
            {
              "id": "http://purl.obolibrary.org/obo/ENVO_01001215",
              "label": "visible spectrum stellar radiation"
            },
            {
              "id": "http://planetmicrobe.org/temppurl/PM_1",
              "label": "depth"
            },
            {
              "id": "http://planetmicrobe.org/temppurl/PM_2",
              "label": "oxygen"
            }
          ]
        }
    ];

    //let result = await query('select fields[1].string from sample limit 1')
    let schemas = await query("SELECT schema_id,name,fields FROM schema");
    console.log(schemas.rows);

    let index = {};

    ontologies.forEach( ontology => {
        console.log("Indexing ontology", ontology.name);
        ontology.terms.forEach( term => {
            index[term.id] = term;
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

//db.sample.find({ location: { $near : { $geometry: { type: "Point",  coordinates: [ -158, 22 ] }, $maxDistance: 5000 } } } ).count()
//function search(db, params) {
//    let query = {
//        project: "HOT",
//    };
//    console.log("params:", params);
//
//    if (params.lat && params.lng) {
//        query.location = {
//            $geoNear: {
//                $geometry: {
//                    type: "Point",
//                    coordinates: [ 1*params.lng, 1*params.lat ]
//                },
//                $maxDistance: (params.radius ? 1*params.radius : 5000)
//            }
//        };
//    }
//
//    if (params.minDepth || params.maxDepth) {
//        query.depth = { $gte: 1*params.minDepth, $lte: 1*params.maxDepth  };
//    }
//
//    if (params.startDate || params.endDate) {
//        query.collected = { $gte: new Date(params.startDate), $lte: new Date(params.endDate) };
//    }
//
//    console.log("query:", JSON.stringify(query));
//
//    return db.collection('sample').find(query).count()
//    .then(count => {
//        return new Promise(function (resolve, reject) {
//            db.collection('sample')
//            .find(query)
//            .sort({ sample: 1 })
//            .skip(1*params.skip)   // no skip if undefined
//            .limit(1*params.limit) // returns all docs if zero or undefined
//            .toArray((err, docs) => {
//                if (err)
//                    reject(err);
//                else
//                    resolve({
//                        count: count,
//                        results: docs
//                    });
//            });
//        })
//    });
//}

async function search(db, params) {
    console.log("params:", params);

    /* CASES

    http://localhost:3010/search?longitude%20coordinate%20measurement%20datum=[-90,90]&latitude%20coordinate%20measurement%20datum=[0,10]

    http://localhost:3010/search?longitude%20coordinate%20measurement%20datum=[-90,90]&OBI_0001620=[0,10]

    http://localhost:3010/search?longitude=[-90,90]&latitude%20coordinate%20measurement%20datum=[0,30]&biome=marine%20pelagic%20biome%20(ENVO:1000023)

    http://localhost:3010/search?longitude=[-90,90]&latitude=[0,30]&biome=~marine

    FIXME:
    http://localhost:3010/search?IAO_0000577=%22ABOKM42%22
    query: {"$or":[{"__schema_id":"5c0570963914e94f86574a7c","Sample label (BioSample)":"\"ABOKM42\"","Sample label (BioArchive)":"\"ABOKM42\"","Sample label (ENA)":"\"ABOKM42\""}]}
    */

    let limit, offset, sort;

    let clauses = {};
    let fields = [];
    let fields2 = {};
    let schemas = {};
    let schemaCount = 0;
    let fieldCount = 0;

    for (param in params) {
        param = param.replace(/\+/gi, ''); // work around for Elm uri encoding
        if (param == 'limit')
            limit = params['limit'] * 1; // convert to int
        else if (param == 'offset')
            offset = params['offset'] * 1; // convert to int
        else if (param == 'sort')
            sort = params['sort'] * 1; // convert to int
        else {
            let term = getTerm(param);
            console.log("term:", term);
            if (!term) {
                //res.status(404).json({ error: "Term not found" });
                console.log("Error: term not found:", term); 
                return;
            }

            for (schemaId in term.schemas) {
                if (!(schemaId in schemas))
                    schemas[schemaId] = schemaCount++;
                if (!clauses[schemaId])
                    clauses[schemaId] = {};
                for (alias in term.schemas[schemaId]) {
                    let arrIndex = term.schemas[schemaId][alias];
                    let val = params[param];
                    console.log("val:", val);

                    // FIXME should use query substitution here -- SQL injection risk
                    let field;
                    if (val === '') // empty - show in results
                        ;
                    else if (!isNaN(val)) { // literal number match
                        field = "number_vals[" + arrIndex + "]";
                        clauses[schemaId][alias] = field + "=" + parseFloat(val);
                    }
                    else if (val.match(/\[-?\d*(\.\d+)?\,-?\d*(\.\d+)?\]/)) { // range query
                        field = "number_vals[" + arrIndex + "]";
                        let bounds = JSON.parse(val);
                        clauses[schemaId][alias] = field + ">" + bounds[0] + " AND " + field + "<" + bounds[1];
                    }
                    else if (val.match(/\~\w+/)) { // partial string match
                        val = val.substr(1);
                        field = "string_vals[" + arrIndex + "]";
                        clauses[schemaId][alias] = field + " LIKE " + "'%" + val + "%'";
                    }
                    else { // literal string match
                        field = "string_vals[" + arrIndex + "]";
                        clauses[schemaId][alias] = field + "=" + "'" + val + "'";
                    }

                    if (field) {
                        fields.push(field);
                        if (!fields2[fieldCount])
                            fields2[fieldCount] = {};
                        fields2[fieldCount][schemaId] = field;
                    }
                }
            }

            fieldCount++;
        }
    }
    //console.log("clauses:", clauses);
    //console.log("fields:", fields);
    //console.log("fields2:", fields2);

    let subClauses = [];
    for (schemaId in clauses) {
        let subClauseStr = "(schema_id=" + schemaId + " AND " + Object.values(clauses[schemaId]).join(" AND ") + ")";
        subClauses.push(subClauseStr);
    }
    let clauseStr = subClauses.join(" OR ");

    let selectStr = "";
    Object.values(fields2).forEach(f => {
        selectStr += ",CASE";
        Object.keys(f).forEach(schemaId => {
            selectStr += " WHEN schema_id=" + schemaId + " THEN " + f[schemaId]
        });
        selectStr += " END"
    });

    let sortDir = (typeof sort !== 'undefined' && sort > 0 ? "ASC" : "DESC");
    let sortStr = (typeof sort !== 'undefined' ? " ORDER BY " + (Math.abs(sort) + 2) + " " + sortDir : "");

    let countQueryStr = "SELECT count(*) FROM sample WHERE " + clauseStr;

    if (!limit)
        limit = 50;
    let limitStr = (limit ? " LIMIT " + limit : "");
    let offsetStr = (offset ? " OFFSET " + offset : "");

    let queryStr = "SELECT schema_id,sample_id" + selectStr + " FROM sample WHERE " + clauseStr + sortStr + offsetStr + limitStr;

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
        results: results.rows
    };
}

function getTerm(nameOrId) {
    for (term of Object.values(rdfTermIndex)) {
        if ((term.id && (nameOrId === term.id || term.id.endsWith(nameOrId))) || (term.label && (nameOrId === term.label || term.label.startsWith(nameOrId)))) {
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
