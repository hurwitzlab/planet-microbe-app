'use strict';

const logger = require('morgan');
const cors = require('cors');
const bodyParser = require('body-parser');
const requestp = require('request-promise');
const client = require('../postgres');

module.exports = function(app) {
    app.use(logger('dev'));
    app.use(cors());
    app.use(bodyParser.json()); // support json encoded bodies
    app.use(agaveTokenValidator);

    app.use(require('./apps'));
    app.use(require('./campaigns.js'));
    app.use(require('./experiments.js'));
    app.use(require('./misc.js'));
    app.use(require('./projects.js'));
    app.use(require('./samples.js'));
    app.use(require('./samplingEvents.js'));
    app.use(require('./search.js'));

    app.use(errorHandler);

    // Catch-all function
    app.get('*', function(req, res, next){
        res.status(404).send("Unknown route: " + req.path);
    });

    function errorHandler(error, req, res, next) {
        console.log("ERROR ".padEnd(80, "!"));
        console.log(error.stack);

        let statusCode = error.statusCode || 500;
        let message = error.message || "Unknown error";

        res.status(statusCode).send(message);
    }

    async function agaveTokenValidator(req, res, next) {
        var token;
        if (req && req.headers)
            token = req.headers.authorization;
        console.log("validateAgaveToken: token:", token);

        req.auth = {
            validToken: false
        };

        if (token) {
            try {
                let response = await getAgaveProfile(token);
                if (!response || response.status != "success") {
                    console.log('validateAgaveToken: !!!! Bad profile status: ' + response.status);
                }
                else {
                    response.result.token = token;
                }

                let profile = response.result;
                if (profile) {
                    console.log("validateAgaveToken: *** success ***  username:", profile.username);

                    req.auth = {
                        validToken: true,
                        profile: profile
                    };

                    // Add user if not already present
                    let user = await client.query({
                        text: "SELECT * FROM \"user\" WHERE user_name=$1",
                        values: [profile.username]
                    });

                    if (user.rowCount == 0) {
                        user = await client.query({
                            text: "INSERT INTO \"user\" (user_name) VALUES ($1) RETURNING *",
                            values: [profile.username]
                        });
                    }

                    user = user.rows[0];
                    user.first_name = profile.first_name;
                    user.last_name = profile.last_name;
                    user.email = profile.email

                    if (user)
                        req.auth.user = user;
                }
            }
            catch(error) {
                console.log("validateAgaveToken: !!!!", error.message);
            }
            finally {
                next;
            }
        }

        next();
    }

    async function getAgaveProfile(token) {
        return await requestp({
            method: "GET",
            uri: "https://agave.iplantc.org/profiles/v2/me", // FIXME hardcoded
            headers: {
                Authorization: "Bearer " + token,
                Accept: "application/json"
            },
            json: true
        });
    }
}
