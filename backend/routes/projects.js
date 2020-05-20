'use strict';

const router = require('express').Router();
const client = require('../postgres');
const { asyncHandler } = require('../util');

router.get('/projects', asyncHandler(async (req, res) => {
    const result = await client.query(
        `SELECT p.project_id,p.name,p.accn,p.description,p.datapackage_url,p.url,pt.name AS type,
            (SELECT count(*) FROM project_to_sample pts WHERE pts.project_id=p.project_id)::int AS sample_count
        FROM project p
        JOIN project_type pt USING(project_type_id)`
    );
    res.json(result.rows);
}));

router.get('/projects/:id(\\d+)', asyncHandler(async (req, res) => {
    const id = req.params.id;
    const projectResult = await client.query({
        text:
            `SELECT
                p.project_id,p.name,p.accn,p.description,p.datapackage_url,p.url AS project_url,pt.name AS type,
                (SELECT count(*)::int FROM project_to_sample pts WHERE pts.project_id=p.project_id) AS sample_count
            FROM project p
            JOIN project_type pt USING(project_type_id)
            WHERE p.project_id=$1`,
        values: [id]
    });

    const fileResult = await client.query({
        text:
            `SELECT file_id,file_type.name AS file_type,file_format.name AS file_format,url AS file_url
                FROM project_to_file
                LEFT JOIN file USING(file_id)
                LEFT JOIN file_type USING(file_type_id)
                LEFT JOIN file_format USING(file_format_id)
                WHERE project_id=$1`,
        values: [id]
    });

    let result = projectResult.rows[0];
    result.files = fileResult.rows;
    res.json(result);
}));

router.get('/projects/:id(\\d+)/campaigns', asyncHandler(async (req, res) => {
    const id = req.params.id;
    const result = await client.query({
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
}));

router.get('/projects/:id(\\d+)/sampling_events', asyncHandler(async (req, res) => {
    const id = req.params.id;
    const result = await client.query({
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
}));

router.get('/projects/:id(\\d+)/samples', asyncHandler(async (req, res) => {
    const id = req.params.id;
    const result = await client.query({
        text:
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations
            FROM sample s
            JOIN project_to_sample pts USING(sample_id)
            WHERE pts.project_id=$1`,
        values: [id]
    });

    res.json(result.rows);
}));

module.exports = router;