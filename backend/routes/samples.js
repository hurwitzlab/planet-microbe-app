'use strict';

const express = require('express');
const router  = express.Router();
const config = require('../config.json');
const db = require('../db.js')(config);

router.post('/samples', async (req, res) => {
    let result;
    let ids = req.body.ids;
    console.log("ids:", ids);

    if (ids) {
        result = await db.query(
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations,p.project_id,p.name AS project_name
            FROM sample s
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id
            JOIN project p ON p.project_id=pts.project_id
            WHERE s.sample_id IN (${ids})`
        );
    }
    else {
        result = await db.query(
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations,p.project_id,p.name AS project_name
            FROM sample s
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id
            JOIN project p ON p.project_id=pts.project_id`
        );
    }

    if (result.rowCount == 0) {
        res.status(404).send();
        return;
    }

    res.json(result.rows);
});

router.get('/samples/:id(\\d+)', async (req, res) => {
    let id = req.params.id;

    let result = await db.query({
        text:
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations,p.project_id,p.name AS project_name
            FROM sample s
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id
            JOIN project p ON p.project_id=pts.project_id
            WHERE s.sample_id=$1`,
        values: [id]
    });

    if (result.rowCount == 0) {
        res.status(404).send();
        return;
    }

    res.json(result.rows[0]);
});

router.get('/samples/:id(\\d+)/sampling_events', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT se.sampling_event_id,se.sampling_event_type,se.name,c.campaign_id,c.campaign_type,c.name AS campaign_name
            FROM sample s
            JOIN sample_to_sampling_event stse ON stse.sample_id=s.sample_id
            JOIN sampling_event se ON se.sampling_event_id=stse.sampling_event_id
            LEFT JOIN campaign c ON c.campaign_id=se.campaign_id
            WHERE s.sample_id=$1`,
        values: [id]
    });

    res.json(result.rows);
});

router.get('/samples/:id(\\d+)/experiments', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT e.experiment_id,e.name,e.accn
            FROM experiment e
            WHERE e.sample_id=$1`,
        values: [id]
    });

    res.json(result.rows);
});

router.post('/samples/files', async (req, res) => {
    let ids;
    if (req.body.ids) {
        ids = req.body.ids.split(',');
        console.log("ids:", ids);
    }

    let result;
    if (ids)
        result = await db.query(
            `SELECT e.sample_id,f.file_id,f.url,ft.name AS file_type,ff.name AS file_format
            FROM experiment e
            LEFT JOIN run r ON (r.experiment_id=e.experiment_id)
            LEFT JOIN run_to_file rtf ON rtf.run_id=r.run_id
            LEFT JOIN file f ON f.file_id=rtf.file_id
            LEFT JOIN file_type ft ON ft.file_type_id=f.file_type_id
            LEFT JOIN file_format ff ON ff.file_format_id=f.file_format_id
            WHERE e.sample_id IN (${ids})`
        );
    else
        result = await db.query(
            `SELECT e.sample_id,f.file_id,f.url,ft.name AS file_type,ff.name AS file_format
            FROM experiment e
            LEFT JOIN run r ON (r.experiment_id=e.experiment_id)
            JOIN run_to_file rtf ON rtf.run_id=r.run_id
            JOIN file f ON f.file_id=rtf.file_id
            LEFT JOIN file_type ft ON ft.file_type_id=f.file_type_id
            LEFT JOIN file_format ff ON ff.file_format_id=f.file_format_id`
        );

    res.json(result.rows);
});

router.get('/samples/files/formats', async (req, res) => {
    let result = await db.query(
        `SELECT ff.file_format_id,ff.name,ff.description,ff.extensions,COUNT(f.file_id)::int AS file_count
        FROM experiment e
        LEFT JOIN run r ON (r.experiment_id=e.experiment_id)
        JOIN run_to_file rtf ON rtf.run_id=r.run_id
        JOIN file f ON f.file_id=rtf.file_id
        LEFT JOIN file_format ff ON ff.file_format_id=f.file_format_id
        GROUP BY ff.file_format_id`
    );

//    res.json(result.rows.map(row => { row.file_count *= 1; return row })); // convert to int
    res.json(result.rows);
});

router.get('/samples/files/types', async (req, res) => {
    let result = await db.query(
        `SELECT ft.file_type_id,ft.name,ft.description,COUNT(f.file_id)::int as file_count
        FROM experiment e
        LEFT JOIN run r ON (r.experiment_id=e.experiment_id)
        JOIN run_to_file rtf ON rtf.run_id=r.run_id
        JOIN file f ON f.file_id=rtf.file_id
        LEFT JOIN file_type ft ON ft.file_type_id=f.file_type_id
        GROUP BY ft.file_type_id`
    );

//    res.json(result.rows.map(row => { row.file_count *= 1; return row })); // convert to int
    res.json(result.rows);
});

router.get('/samples/:id(\\d+)/metadata', async (req, res) => {
    let id = req.params.id;
    let termIndex = req.app.get('termIndex');

    let result = await db.query({
        text:
            `SELECT s.schema_id,schema.fields->'fields' AS fields,s.number_vals,s.string_vals,s.datetime_vals
            FROM sample s
            JOIN schema ON schema.schema_id=s.schema_id
            WHERE s.sample_id=$1`,
        values: [id]
    });
    if (result.rowCount == 0) {
        res.status(404).send();
        return;
    }

    let row = result.rows[0];
    let terms = [];
    let values = [];
    for (let i = 0; i < row.fields.length; i++) {
        let field = row.fields[i];

        let term = {};
        let term2 = termIndex.getTerm(field.rdfType) || {};
        if (term2.annotations) { // TODO move into function
            term.annotations = term2.annotations.map(a => {
                let label = termIndex.getLabelForValue(a.id);
                let value = termIndex.getLabelForValue(a.value);
                return {
                    id: a.id,
                    label: label,
                    value: value
                };
            });
        }

        term.id = field.rdfType;
        term.type = field.type;
        term.alias = field.name;
        term.label = term2.label || "";
        term.definition = term2.definition || "";
        term.unitId = term2.unitId || "";
        term.unitLabel = term2.unitLabel || "";
        term.sourceUrl = field['pm:sourceUrl'];
        terms.push(term);

        let val = "";
        if (field.type == "number")
            val = row.number_vals[i];
            if (isNaN(val)) // below detection limit values are stored as NaN in DB
                val = "Below Detection Limit"
        else if (field.type == "string")
            val = row.string_vals[i];
        else if (field.type == "datetime" || field.type == "date")
            val = row.datetime_vals[i];
        else
            ; //TODO error
        values.push(val);
    }

    res.json({
        schema_id: row.schema_id,
        terms: terms,
        values: values
    });
});

module.exports = router;