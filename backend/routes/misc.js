'use strict';

const https = require('https');
const path = require('path');
const sendmail = require('sendmail')();
const router = require('express').Router();
const config = require('../config.json');
const { asyncHandler } = require('../util');
const { requireAuth, getToken } = require('../auth');
const client = require('../postgres');

router.post('/contact', (req, res) => {
    console.log(req.body);

    if (!config.supportEmail) {
        console.log("Error: missing supportEmail in config");
        res.status(500).json({
            status: "failed"
        });
        return;
    }

    const email = req.body.email || "Unknown";
    const message = req.body.message || "";

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

router.get('/download/:filepath(\\S+)', async (req, res, next) => {
    // Can be done wit res.download(filename) but we want to send content directly
    res.setHeader('Content-disposition', 'attachment;filename=' + path.basename(req.params.filepath));
    res.setHeader('Content-type', 'application/octet-stream');

    var options = {
        host: config.agaveBaseUrl.replace(/^https?:\/\//,''), // remove protocol
        path: "/files/v2/media/" + req.params.filepath,
        headers: {
            Accept: "application/octet-stream",
            Authorization: req.query.token
        }
    }

    // Request file from Agave and stream response to client
    try {
        https.get(options,
            function(response) {
                // Stream to client
                response.on('data', function(data) {
                    res.write(data);
                });
                // Handle end of transaction
                response.on('end', function() {
                    res.end();
                });
            });
    }
    catch(error) {
        console.log(error);
        res.send(500, error)
    }
});

// Get token from authorization code as part of OAUTH2 Authorization Code flow
router.post('/token', async (req, res) => {
    const provider = req.body.provider;
    const code = req.body.code;
    const response = await getToken(provider, code);
    console.log(response);
    res.send(response);
});

router.post('/users/login', requireAuth, asyncHandler(async (req, res) => {
    const username = req.auth.user.user_name;

    // Add user if not already present
    let user = await client.query({
        text: "SELECT * FROM \"user\" WHERE user_name=$1",
        values: [username]
    });

    if (user.rowCount == 0) {
        user = await client.query({
            text: "INSERT INTO \"user\" (user_name) VALUES ($1) RETURNING *",
            values: [username]
        });
    }

    // For new user set first_name/last_name/email, or update for existing user (in case they changed any of those fields)
    // Only do this once at login and not in agaveTokenValidator
    user = await client.query({
        text: "UPDATE \"user\" SET first_name=$1, last_name=$2, email=$3 WHERE user_name=$4",
        values: [req.auth.user.first_name,req.auth.user.last_name, req.auth.user.email, username]
    });

    user = await client.query({
        text: "SELECT * FROM \"user\" WHERE user_name=$1",
        values: [username]
    });

    await client.query({
        text: "INSERT INTO login (user_id) VALUES ($1) RETURNING *",
        values: [user.rows[0].user_id]
    });

    res.json(user.rows[0]);
}));

module.exports = router;