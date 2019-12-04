'use strict';

const requestp = require('request-promise');
const sendmail = require('sendmail')();
const express = require('express');
const router  = express.Router();
const config = require('../config.json');
const db = require('../db.js')(config);
const requireAuth = require('../util.js').requireAuth;

router.post('/contact', (req, res) => {
    console.log(req.body);

    if (!config.supportEmail) {
        console.log("Error: missing supportEmail in config");
        res.status(500).json({
            status: "failed"
        });
        return;
    }

    var name = req.body.name || "Unknown";
    var email = req.body.email || "Unknown";
    var message = req.body.message || "";

    sendmail({
        from: email,
        to: config.supportEmail,
        subject: 'Support request',
        html: message,
    }, (err, reply) => {
        console.log(err && err.stack);
        console.dir(reply);
    });

    res.json({
        status: "success"
    });
});

router.post('/token', async (req, res) => {
    let provider = req.body.provider;
    let code = req.body.code;
    let tokenResponse = await agaveGetToken(provider, code);
    res.send(tokenResponse);
});

router.post('/users/login', async (req, res) => { // TODO add try/catch error handling
    requireAuth(req);

    var username = req.auth.user.user_name;

    // Add user if not already present
    let user = await db.query({
        text: "SELECT * FROM \"user\" WHERE user_name=$1",
        values: [username]
    });

    if (user.rowCount == 0) {
        user = await db.query({
            text: "INSERT INTO \"user\" (user_name) VALUES ($1) RETURNING *",
            values: [username]
        });
    }

    // For new user set first_name/last_name/email, or update for existing user (in case they changed any of those fields)
    // Only do this once at login and not in agaveTokenValidator
    user = await db.query({
        text: "UPDATE \"user\" SET first_name=$1, last_name=$2, email=$3 WHERE user_name=$4",
        values: [req.auth.user.first_name,req.auth.user.last_name, req.auth.user.email, username]
    });

    user = await db.query({
        text: "SELECT * FROM \"user\" WHERE user_name=$1",
        values: [username]
    });

    let login = await db.query({
        text: "INSERT INTO login (user_id) VALUES ($1) RETURNING *",
        values: [user.rows[0].user_id]
    });

    res.json(user.rows[0]);
});

async function agaveGetToken(provider, code) {
    let url = config.oauthProviders[provider].tokenUrl;
    let options = {
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
    let response = await requestp(options);
    console.log(response);
    return(response);
}

module.exports = router;