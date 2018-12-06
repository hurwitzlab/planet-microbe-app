const express = require('express');
const app = express();
const cors = require('cors');
const Promise = require('promise');
const mongodb = require('mongodb');
const shortid = require('shortid');
const stringSimilarity = require('string-similarity');
const config = require('./config.json');


var rdfTermIndex = {};
var termIndex = {};

mongo()
.then( db => generateTermIndex(db) )
.then( index => {
    console.log('index:', JSON.stringify(index, null, 4));
    rdfTermIndex = index;
})
.then( () =>
    app.listen(config.serverPort, () => console.log('Server listening on port', config.serverPort))
)
.catch(err => {
    console.error(err);
});

//----------------------------------------------------------------------------------------------------------------------

app.use(cors());
app.use(requestLogger);

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
            .filter(term => term.id)
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

app.get('/searchTerms/:id(\\S+)', (req, res) => {
    let id = req.params.id;
    console.log("searchTerms:", id);

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
    //let aliases = Object.values(term.schemas).reduce((acc, s) => Object.keys(s), []);
    let aliases = Array.from(new Set(Object.values(term.schemas).reduce((acc, schema) => acc.concat(Object.keys(schema)), [])));

    if (term.type == "string") {
        mongo()
        .then( db => {
            let group = { '_id': '$__schema_id' };
            aliases.forEach(alias => group[alias] = { '$addToSet': '$'+alias });
            return aggregate(db, [
                { $group: group },
                { $project: { _id: false, values: { $setUnion: aliases.map(alias => '$'+alias) } } }
            ])
        })
        .then(results => {
            res.json({
                id: term.id,
                label: term.label,
                type: term.type,
                aliases: aliases,
                values: results.sort()
            })
        })
        .catch(err => {
            console.log(err);
            res.send(err);
        });
    }
    else {
        mongo()
        .then( db => {
            return Promise.all(
                Object.keys(term.schemas).map(schema_id => {
                    return Promise.all(
                        Object.keys(term.schemas[schema_id]).map(alias => {
                            return Promise.all([
                                findMinOrMax(db, schema_id, alias, 1),
                                findMinOrMax(db, schema_id, alias, -1)
                            ]);
                        })
                    )
                })
            )
        })
        .then( results => {
            console.log(results);
            let min = Math.min( ...results.map(a => Math.min(...a[0])) );
            let max = Math.max( ...results.map(a => Math.max(...a[0])) );
            res.json({
                id: term.id,
                label: term.label,
                type: term.type,
                aliases: aliases,
                min: min,
                max: max
            });
        })
        .catch(err => {
            console.log(err);
            res.send(err);
        });
    }
});

app.get('/search', (req, res) => {
    mongo()
    .then( db => {
        return search(db, req.query);
    })
    .then( results => {
        res.json(results);
    })
    .catch(err => {
        console.log(err);
        res.send(err);
    });
});

//----------------------------------------------------------------------------------------------------------------------

// Connect to MongoDB
function mongo() {
    return new Promise(function (resolve, reject) {
        mongodb.MongoClient.connect(config.mongo.url, { useNewUrlParser: true }, (err, db) => {
          if (err)
            reject(err)
          else
            resolve(db.db(config.mongo.db))
        });
    });
}

function generateTermIndex(db) {
    return new Promise(function (resolve, reject) {
        let index = {};

        db.collection('ontologies').find().toArray((err, ontologies) => {
            if (err)
                reject(err);

            ontologies.forEach( ontology => {
                console.log("Indexing ontology", ontology.name);
                ontology.terms.forEach( term => {
                    index[term.id] = term;
                });
            });

            db.collection('schemas').find().toArray((err, schemas) => {
                if (err)
                    reject(err);

                schemas.forEach( schema => {
                    console.log("Indexing schema", schema.name);

                    schema.fields.forEach(field => {
                        let rdf = field.rdfType;
                        if (!rdf)
                            rdf = 'http://planetmicrobe.org/temppurl/PM_'+ shortid.generate(); //FIXME hardcoded URL
                        if (!(rdf in index))
                            index[rdf] = {};
                        if (!('schemas' in index[rdf]))
                            index[rdf]['schemas'] = {};
                        if (!index[rdf]['schemas'][schema._id])
                            index[rdf]['schemas'][schema._id] = {};
                        index[rdf]['schemas'][schema._id][field.name] = 1;
                        index[rdf]['type'] = field.type;
                    })
                });

                resolve(index);
            });
        })
    })
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

function search(db, params) {
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

    let terms = {};
    let fields = {};

    Object.keys(params).forEach(param => {
        Object.values(rdfTermIndex).forEach(term => {
            if ((term.id && (param === term.id || term.id.endsWith(param))) || (term.label && (param === term.label || term.label.startsWith(param)))) {
                Object.keys(term.schemas).forEach(schema_id => {
                    if (!terms[schema_id])
                        terms[schema_id] = {};
                    Object.keys(term.schemas[schema_id]).forEach(alias => {
                        let val = params[param];
                        fields[alias] = true;

                        if (val === '') // empty - show in results
                            ;
                        else if (!isNaN(val)) // literal number match
                            terms[schema_id][alias] = parseFloat(val);
                        else if (val.match(/\[-?\d+\,-?\d+\]/)) { // range query
                            let bounds = JSON.parse(val);
                            terms[schema_id][alias] = { $gt: bounds[0], $lt: bounds[1] };
                        }
                        else if (val.match(/\~\w+/)) { // partial string match
                            val = val.substr(1);
                            terms[schema_id][alias] = { $regex: val };
                        }
                        else // literal string match
                            terms[schema_id][alias] = val;
                    })
                })
            }
        })
    });
    console.log("terms:", terms);

    let query = {};
    query['$or'] = [];
    Object.keys(terms).forEach(schema_id => {
        let clause = {};
        clause['__schema_id'] = mongodb.ObjectID(schema_id);
        Object.keys(terms[schema_id]).forEach(alias => {
            clause[alias] = terms[schema_id][alias];
        });
        query['$or'].push(clause);
    });

    console.log("query:", JSON.stringify(query));
    console.log("fields:", fields);

    return db.collection('samples').find(query).count()
    .then(count => {
        return new Promise(function (resolve, reject) {
            db.collection('samples')
            .find(query)
            .project(fields)
//          .sort({ sample: 1 })
//          .skip(1*params.skip)   // no skip if undefined
          .limit(1*params.limit) // returns all docs if zero or undefined
            .toArray((err, docs) => {
                if (err)
                    reject(err);
                else {
                    let keys = ['_id'].concat(Object.keys(fields));
                    resolve({
                        count: count,
                        results: docs.map(doc => keys.filter(k => k in doc).map(k => doc[k]))
                    });
                }
            });
        })
    });
}

function findMinOrMax(db, schema_id, alias, minOrMax) {
    console.log("findMinMax:", schema_id, alias, minOrMax);
    let query = {};
    query[alias] = {$ne:null};
    query['__schema_id'] = mongodb.ObjectID(schema_id);
    filter = {};
    filter[alias] = true;
    sort = {}
    sort[alias] = minOrMax;
    console.log("query:", query);
    return new Promise(function (resolve, reject) {
        db.collection('samples')
        .find(query, filter)
        .project(filter)
        .sort(sort)
        .limit(1)
        .toArray((err, docs) => {
            console.log("docs:", docs[0][alias])
            if (err)
                reject(err);
            else
                resolve(docs[0][alias]);
        });
    });
}

function aggregate(db, query) {
    console.log('aggregate:', JSON.stringify(query));
    return new Promise(function (resolve, reject) {
        db.collection('samples')
        .aggregate(query)
        .toArray((err, docs) => {
            console.log("docs:", docs.length)
            if (err)
                reject(err);
            else {
                let results = Array.from(new Set(docs.reduce((acc, doc) => acc.concat(doc.values), [])));
                resolve(results);
            }
        });
    });
}

function requestLogger(req, res, next) {
    console.log(["REQUEST:", req.method, req.url].join(" ").concat(" ").padEnd(80, "-"));
    next();
}