'use strict';

const express = require('express');
const router  = express.Router();
const config = require('../config.json');
const db = require('../postgres.js')(config);

router.get('/projects', async (req, res) => {
    let result = await db.query(
        `SELECT p.project_id,p.name,p.accn,p.description,p.datapackage_url,p.url,pt.name AS type,
            (SELECT count(*) FROM project_to_sample pts WHERE pts.project_id=p.project_id) AS sample_count
        FROM project p
        JOIN project_type pt USING(project_type_id)`
    );
    result.rows.forEach(row => row.sample_count *= 1); // convert count to int
    res.json(result.rows);
//    .catch(err => {
//        console.log(err);
//        res.send(err);
//    });
});

router.get('/projects/:id(\\d+)', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT
                p.project_id,p.name,p.accn,p.description,p.datapackage_url,p.url AS project_url,pt.name AS type,f.file_id,f.url,ft.name AS file_type,ff.name AS file_format,
                (SELECT count(*) FROM project_to_sample pts WHERE pts.project_id=p.project_id) AS sample_count
            FROM project p
            JOIN project_type pt USING(project_type_id)
            LEFT JOIN project_to_file USING(project_id)
            LEFT JOIN file f USING(file_id)
            LEFT JOIN file_type ft USING(file_type_id)
            LEFT JOIN file_format ff USING(file_format_id)
            WHERE p.project_id=$1`,
        values: [id]
    });

    // FIXME kludgey
    let filesById = {};
    result.rows.forEach(row => {
        row.sample_count *= 1

        if (row.file_id)
            filesById[row.file_id] = {
                file_id: row.file_id,
                file_type: row.file_type,
                file_format: row.file_format,
                url: row.url,
            };
    })

    let result2 = result.rows[0];
    result2.files = Object.values(filesById);
    res.json(result2);
});

router.get('/projects/:id(\\d+)/campaigns', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT c.campaign_id,c.campaign_type,c.name,c.description,c.deployment,c.start_location,c.end_location,c.start_time,c.end_time,c.urls
            FROM project_to_sample pts
            JOIN sample USING(sample_id)
            JOIN sample_to_sampling_event USING(sample_id)
            JOIN sampling_event USING(sampling_event_id)
            JOIN campaign c USING(campaign_id)
            WHERE pts.project_id=$1
            GROUP BY c.campaign_id`,
        values: [id]
    });
    res.json(result.rows);
});

router.get('/projects/:id(\\d+)/sampling_events', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT se.sampling_event_id,se.sampling_event_type,se.name,ST_AsGeoJson(se.locations)::json->'coordinates' AS locations,se.start_time,se.end_time
            FROM project_to_sample pts
            JOIN sample USING(sample_id)
            JOIN sample_to_sampling_event USING(sample_id)
            JOIN sampling_event se USING(sampling_event_id)
            WHERE pts.project_id=$1
            GROUP BY se.sampling_event_id`,
        values: [id]
    });

    res.json(result.rows);
});

router.get('/projects/:id(\\d+)/samples', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations
            FROM sample s
            JOIN project_to_sample pts USING(sample_id)
            WHERE pts.project_id=$1`,
        values: [id]
    });

    res.json(result.rows);
});

module.exports = router;