const express = require('express');
const app = express();
const cors = require('cors');
const Promise = require('promise');
const MongoClient = require('mongodb').MongoClient;
const config = require('./config.json');

app.use(cors());

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

app.listen(config.serverPort, () => console.log('Server listening on port', config.serverPort));

//----------------------------------------------------------------------------------------------------------------------

// Connect to MongoDB
function mongo() {
    return new Promise(function (resolve, reject) {
        MongoClient.connect(config.mongo.url, { useNewUrlParser: true }, (err, db) => {
          if (err)
            reject(err)
          else
            resolve(db.db(config.mongo.db))
        });
    });
}

//db.sample.find({ location: { $near : { $geometry: { type: "Point",  coordinates: [ -158, 22 ] }, $maxDistance: 5000 } } } ).count()
function search(db, params) {
    var query = {
        project: "HOT",
    };
    console.log("params:", params);

    if (params.lat && params.lng) {
        query.location = {
            $geoNear: {
                $geometry: {
                    type: "Point",
                    coordinates: [ 1*params.lng, 1*params.lat ]
                },
                $maxDistance: (params.radius ? 1*params.radius : 5000)
            }
        };
    }

    if (params.minDepth || params.maxDepth) {
        query.depth = { $gte: 1*params.minDepth, $lte: 1*params.maxDepth  };
    }

    if (params.startDate || params.endDate) {
        query.collected = { $gte: new Date(params.startDate), $lte: new Date(params.endDate) };
    }

    console.log("query:", JSON.stringify(query));

    return db.collection('sample').countDocuments(query)
    .then(count => {
        return new Promise(function (resolve, reject) {
            db.collection('sample')
            .find(query)
            .sort({ sample: 1 })
            .skip(1*params.skip)   // no skip if undefined
            .limit(1*params.limit) // returns all docs if zero or undefined
            .toArray((err, docs) => {
                if (err)
                    reject(err);
                else
                    resolve({
                        count: count,
                        results: docs
                    });
            });
        })
    });
}
