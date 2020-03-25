'use strict';

const express = require('express');
const router  = express.Router();
const config = require('../config.json');
const db = require('../postgres.js')(config);

router.get('/campaigns/:id(\\d+)', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT c.campaign_id,c.campaign_type,c.name,c.description,c.deployment,c.start_location,c.end_location,c.start_time,c.end_time,c.urls,p.project_id,p.name AS project_name
            FROM campaign c
            JOIN sampling_event USING(campaign_id)
            JOIN sample_to_sampling_event USING(sampling_event_id)
            JOIN sample USING(sample_id)
            JOIN project_to_sample USING(sample_id)
            JOIN project p USING(project_id)
            WHERE c.campaign_id=$1`,
        values: [id]
    });
    res.json(result.rows[0]);
});

router.get('/campaigns/:id(\\d+)/sampling_events', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT se.sampling_event_id,se.sampling_event_type,se.name,ST_AsGeoJson(se.locations)::json->'coordinates' AS locations,se.start_time,se.end_time
            FROM sampling_event se
            JOIN campaign c USING(campaign_id)
            WHERE c.campaign_id=$1`,
        values: [id]
    });

    res.json(result.rows);
});

router.get('/campaigns/:id(\\d+)/samples', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations
            FROM sample s
            JOIN sample_to_sampling_event USING(sample_id)
            JOIN sampling_event se USING(sampling_event_id)
            WHERE se.campaign_id=$1
            GROUP BY s.sample_id`,
        values: [id]
    });

    res.json(result.rows);
});

module.exports = router;