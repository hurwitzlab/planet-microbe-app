'use strict';

const logger = require('morgan');
const cors = require('cors');
const bodyParser = require('body-parser');
const { authenticate } = require('../auth')

module.exports = function(app) {
    // Express extensions
    app.use(logger('dev'));
    app.use(cors()); // support CORS requests
    app.use(bodyParser.json()); // support json encoded bodies

    // Agave authentication
    app.use(authenticate);

    // API
    app.use(require('./apps'));
    app.use(require('./campaigns'));
    app.use(require('./experiments'));
    app.use(require('./misc'));
    app.use(require('./projects'));
    app.use(require('./samples'));
    app.use(require('./samplingEvents'));
    app.use(require('./search'));

    // Catch-all function
    app.get('*', function(req, res){
        res.status(404).send("Unknown route: " + req.path);
    });

    // Catch errors
    app.use(errorHandler);
}

function errorHandler(error, req, res) {
    console.log("ERROR ".padEnd(80, "!"));
    console.log(error.stack);

    const statusCode = error.statusCode || 500;
    const message = error.message || "Unknown error";

    res.status(statusCode).send(message);
}
