'use strict';

const express = require('express');
const router  = express.Router();
const config = require('../config.json');
const db = require('../postgres.js')(config);

router.get('/sampling_events/:id(\\d+)', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT se.sampling_event_id,se.sampling_event_type,se.name,ST_AsGeoJson(se.locations)::json->'coordinates' AS locations,se.start_time,se.end_time,c.campaign_id,c.campaign_type,c.name AS campaign_name,p.project_id,p.name AS project_name
            FROM sampling_event se
            LEFT JOIN campaign c ON c.campaign_id=se.campaign_id
            JOIN sample_to_sampling_event stse ON stse.sampling_event_id=se.sampling_event_id
            JOIN sample s ON s.sample_id=stse.sample_id
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id
            JOIN project p ON p.project_id=pts.project_id
            WHERE se.sampling_event_id=$1`,
        values: [id]
    });

    res.json(result.rows[0]);
});

router.get('/sampling_events/:id(\\d+)/samples', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations
            FROM sample s
            JOIN sample_to_sampling_event stse ON stse.sample_id=s.sample_id
            WHERE stse.sampling_event_id=$1`,
        values: [id]
    });

    res.json(result.rows);
});

module.exports = router;