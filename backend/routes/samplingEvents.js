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

router.get('/sampling_events/:id(\\d+)/data/(:type(\\w+))', async (req, res) => {
    let id = req.params.id;
    let type = req.params.type;
    let termIndex = req.app.get('termIndex');

    let result = await db.query({
        text:
            `SELECT s.schema_id,s.type,s.fields->'fields' AS fields,sed.number_vals,sed.string_vals,sed.datetime_vals
            FROM sampling_event se
            JOIN sampling_event_data sed ON sed.sampling_event_id=se.sampling_event_id
            JOIN schema s ON s.schema_id=sed.schema_id
            WHERE se.sampling_event_id=$1 AND s.type=$2`,
        values: [id, type]
    });
    if (result.rowCount == 0) {
        res.status(404).send();
        return;
    }

    //FIXME code below is dup'ed in samples.js
    let row = result.rows[0];
    let terms = [];
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
    }

    let values = [];
    for (row of result.rows) {
        let values2 = [];
        for (let i = 0; i < terms.length; i++) {
            let term = terms[i];
            let val = "";
            if (term.type == "number")
                val = row.number_vals[i];
                if (isNaN(val)) // below detection limit values are stored as NaN in DB
                    val = "Below Detection Limit"
            else if (term.type == "string")
                val = termIndex.getLabelForValue(row.string_vals[i]); // translate purl value to label
            else if (term.type == "datetime" || term.type == "date")
                val = row.datetime_vals[i];
            else
                ; //TODO error
            values2.push(val)
        }
        values.push(values2);
    }

    res.json({
        schema_id: row.schema_id,
        terms: terms,
        values: values
    });
});

module.exports = router;