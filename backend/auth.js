'use strict';

const requestp = require('request-promise');
const client = require('./postgres');
const config = require('./config.json');

// Middleware to force authentication
function requireAuth(req, res, next) {
    console.log("REQUIRE AUTH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    if (!req || !req.auth || !req.auth.validToken || !req.auth.user) {
        const err = new Error('Unauthorized');
        err.status = 401;
        next(err);
    }
    else {
        next();
    }
}

// Middleware to validate Agave bearer token
async function authenticate(req, res, next) {
    let token;
    if (req && req.headers)
        token = req.headers.authorization;
    console.log("authenticate: token =", token);

    req.auth = {
        validToken: false
    };

    if (token) {
        try {
            let response = await getProfile(token);
            if (!response || response.status != "success") {
                console.log('authenticate: error: bad profile status: ' + response.status);
            }
            else {
                response.result.token = token;
            }

            let profile = response.result;
            if (profile) {
                console.log("authenticate: *** success ***  username =", profile.username);

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
            console.log("authenticate: error:", error.message);
        }
        finally {
            next();
        }
    }
    else {
        next();
    }
}

function getProfile(token) {
    return requestp({
        method: "GET",
        uri: "https://agave.iplantc.org/profiles/v2/me", //FIXME hardcoded
        headers: {
            Authorization: "Bearer " + token,
            Accept: "application/json"
        },
        json: true
    });
}

function getToken(provider, code) {
    const url = config.oauthProviders[provider].tokenUrl;
    const options = {
        method: "POST",
        uri: url,
        form: {
            grant_type: "authorization_code",
            client_id: config.oauthProviders.agave.clientId,
            client_secret: config.oauthProviders.agave.clientSecret,
            redirect_uri: config.oauthProviders.agave.redirectUrl,
            code: code
        }
    };

    console.log(provider, ": sending authorization POST", url);
    return requestp(options);
}

if (!config || !config.oauthProviders || !config.oauthProviders.agave)
    throw('Error: Missing OAUTH configuration');

module.exports = { requireAuth, authenticate, getToken };