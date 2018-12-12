const express = require('express');
const app = express();
const cors = require('cors');
const logger = require('morgan');
const Promise = require('promise');
const {promisify} = require('util');
const {Client} = require('pg');
const shortid = require('shortid');
const stringSimilarity = require('string-similarity');
const config = require('./config.json');

var rdfTermIndex = {};
var db;

// Initialize
(async function() {
    db = new Client({
        user: 'mbomhoff',
        host: 'localhost',
        database: 'jsontest',
        password: ''
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
    let term = rdfTermIndex[id];

    if (!term) {
        let matches = Object.keys(rdfTermIndex).filter(key => key.endsWith(id));
        if (matches && matches.length)
            term = rdfTermIndex[matches[0]];
        else {
            res.json({}); // TODO term not found error
            return;
        }
    }

    let aliases = Array.from(new Set(Object.values(term.schemas).reduce((acc, schema) => acc.concat(Object.keys(schema)), [])));

    if (term.type == "string") {
        let uniqueVals = {};
        for (let schemaId in term.schemas) {
            for (let alias in term.schemas[schemaId]) {
                let arrIndex = term.schemas[schemaId][alias];
                let vals = await db.query({ text: "SELECT DISTINCT(fields[$1].string) FROM sample", values: [ arrIndex ], rowMode: 'array'});
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
//        })
//        .catch(err => {
//            console.log(err);
//            res.send(err);
//        });
    }
    else if (term.type == "number") { // numeric -- TODO:datetime
        let min, max;
        for (let schemaId in term.schemas) {
            for (let alias in term.schemas[schemaId]) {
                let arrIndex = term.schemas[schemaId][alias];
                let vals = await db.query("SELECT MAX(fields[$1].number),MIN(fields[$2].number) FROM sample", [arrIndex,arrIndex]);
                vals.rows.forEach(row => {
                    min = Math.min(row.min);
                    max = Math.max(row.max);
                });
            }
        }

        res.json({
            id: term.id,
            label: term.label,
            type: term.type,
            aliases: aliases,
            min: min,
            max: max
        });
//        .catch(err => {
//            console.log(err);
//            res.send(err);
//        });
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
            }
          ]
        }
    ];

    //let result = await db.query('select fields[1].string from sample limit 1')
    let schemas = await db.query("SELECT schema_id,name,fields->'fields' as fields FROM schema LIMIT 1");
//    console.log(schemas.rows[0]);

    let index = {};

    ontologies.forEach( ontology => {
        console.log("Indexing ontology", ontology.name);
        ontology.terms.forEach( term => {
            index[term.id] = term;
        });
    });

    schemas.rows.forEach( schema => {
        console.log("Indexing schema", schema.name);

        for (let i = 0; i < schema.fields.length; i++) {
            let field = schema.fields[i];
            let rdf = field.rdfType;
            if (!rdf)
                rdf = 'http://planetmicrobe.org/temppurl/PM_'+ shortid.generate(); //FIXME hardcoded URL
            if (!(rdf in index))
                index[rdf] = {};
            if (!('schemas' in index[rdf]))
                index[rdf]['schemas'] = {};
            if (!index[rdf]['schemas'][schema.schema_id])
                index[rdf]['schemas'][schema.schema_id] = {};
            index[rdf]['schemas'][schema.schema_id][field.name] = i+1;
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

    let clauses = {};
    let fields = [];

    for (param in params) {
        for (term of Object.values(rdfTermIndex)) {
            if ((term.id && (param === term.id || term.id.endsWith(param))) || (term.label && (param === term.label || term.label.startsWith(param)))) {
                for (schemaId in term.schemas) {
                    if (!clauses[schemaId])
                        clauses[schemaId] = {};
                    for (alias in term.schemas[schemaId]) {
                        console.log("alias:", alias)
                        let arrIndex = term.schemas[schemaId][alias];
                        let val = params[param];

                        // FIXME should use query substitution here -- SQL injection risk
                        if (val === '') // empty - show in results
                            ;
                        else if (!isNaN(val)) { // literal number match
                            clauses[schemaId][alias] = alias + "=" + parseFloat(val);
                            fields.push("fields[" + arrIndex + "].number");
                        }
                        else if (val.match(/\[-?\d+\,-?\d+\]/)) { // range query
                            let bounds = JSON.parse(val);
                            clauses[schemaId][alias] = "fields[" + arrIndex + "].number" + ">=" + bounds[0] + " AND " + "fields[" + arrIndex + "].number" + "<=" + bounds[1];
                            fields.push("fields[" + arrIndex + "].number");
                        }
                        else if (val.match(/\~\w+/)) { // partial string match
                            val = val.substr(1);
                            clauses[schemaId][alias] = "fields[" + arrIndex + "].string" + " LIKE " + "'%" + val + "%'";
                            fields.push("fields[" + arrIndex + "].string");
                        }
                        else { // literal string match
                            clauses[schemaId][alias] = "fields[" + arrIndex + "].string" + "=" + "'" + val + "'";
                            fields.push("fields[" + arrIndex + "].string");
                        }
                    }
                }
            }
        }
    }
    console.log("clauses:", clauses);
    console.log("fields:", fields);

    let clauseStr = "1=1";
    for (schemaId in clauses) {
        for (alias in clauses[schemaId]) {
            clauseStr += " AND " + clauses[schemaId][alias];
        }
    }

    let queryStr = "SELECT sample_id," + fields.join(',') + " FROM sample WHERE " + clauseStr;
    console.log(queryStr);

    let results = await db.query({
        text: queryStr,
        values: [],
        rowMode: 'array',
    });
    return {
        count: 0,
        results: results.rows
    };
}
