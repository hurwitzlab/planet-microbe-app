'use strict';

const router = require('express').Router();
const client = require('../postgres');
const { asyncHandler } = require('../util');

router.get('/sampling_events/:id(\\d+)', asyncHandler(async (req, res) => {
    const id = req.params.id;
    const result = await client.query({
        text:
            `SELECT se.sampling_event_id,se.sampling_event_type,se.name,se.start_time,se.end_time,
                ST_AsGeoJson(se.locations)::json->'coordinates' AS locations,
                c.campaign_id,c.campaign_type,c.name AS campaign_name,
                p.project_id,p.name AS project_name
            FROM sampling_event se
            LEFT JOIN campaign c USING(campaign_id)
            JOIN sample_to_sampling_event USING(sampling_event_id)
            JOIN sample USING(sample_id)
            JOIN project_to_sample USING(sample_id)
            JOIN project p USING(project_id)
            WHERE se.sampling_event_id=$1`,
        values: [id]
    });

    res.json(result.rows[0]);
}));

router.get('/sampling_events/:id(\\d+)/samples', asyncHandler(async (req, res) => {
    const id = req.params.id;
    const result = await client.query({
        text:
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations
            FROM sample s
            JOIN sample_to_sampling_event stse USING(sample_id)
            WHERE stse.sampling_event_id=$1`,
        values: [id]
    });

    res.json(result.rows);
}));

router.get('/sampling_events/:id(\\d+)/data/(:type(\\w+))', asyncHandler(async (req, res) => {
    const id = req.params.id;
    const type = req.params.type;
    const termIndex = req.app.get('termIndex');

    let result = await client.query({
        text:
            `SELECT s.schema_id,s.type,s.fields->'fields' AS fields,sed.number_vals,sed.string_vals,sed.datetime_vals
            FROM sampling_event se
            JOIN sampling_event_data sed USING(sampling_event_id)
            JOIN schema s USING(schema_id)
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
}));

module.exports = router;