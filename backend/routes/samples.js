'use strict';

const express = require('express');
const router  = express.Router();
const config = require('../config.json');
const db = require('../postgres.js')(config);

router.post('/samples', async (req, res) => {
    let result;
    let ids;

    if (req.body.ids) {
        ids = req.body.ids.split(',');
        console.log("ids:", ids);
    }

    if (ids) {
        result = await db.query({
            text:
                `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations,p.project_id,p.name AS project_name
                FROM sample s
                JOIN project_to_sample pts ON pts.sample_id=s.sample_id
                JOIN project p ON p.project_id=pts.project_id
                WHERE s.sample_id = ANY($1)`,
            values: [ids]
        });
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

router.post('/samples/experiments', async (req, res) => {
    let ids;
    if (req.body.ids) {
        ids = req.body.ids.split(',');
        console.log("ids:", ids);
    }

    let result;
    if (ids)
        result = await db.query({
            text: `SELECT e.experiment_id,e.name,e.accn,l.name AS library_name,l.strategy AS library_strategy, l.source AS library_source, l.selection AS library_selection, l.protocol AS library_protocol, l.layout AS library_layout, l.length AS library_length
                FROM experiment e
                LEFT JOIN library l ON l.experiment_id=e.experiment_id
                WHERE e.sample_id = ANY($1)`,
            values: [ids]
        });
    else
        result = await db.query(
            `SELECT e.experiment_id,e.name,e.accn
            FROM experiment e`
        );

    res.json(result.rows);
});

router.post('/samples/runs', async (req, res) => {
    let ids;
    if (req.body.ids) {
        ids = req.body.ids.split(',');
        console.log("ids:", ids);
    }

    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT r.run_id,r.accn,r.total_spots,r.total_bases,f.file_id,f.url,ft.name AS file_type,ff.name AS file_format
            FROM experiment e
            LEFT JOIN run r ON r.experiment_id=e.experiment_id
            LEFT JOIN run_to_file rtf ON rtf.run_id=r.run_id
            LEFT JOIN file f ON f.file_id=rtf.file_id
            LEFT JOIN file_type ft ON ft.file_type_id=f.file_type_id
            LEFT JOIN file_format ff ON ff.file_format_id=f.file_format_id
            WHERE e.sample_id = ANY($1)`,
        values: [ids]
    });

    // FIXME kludgey
    let rowsById = {};
    result.rows.forEach(row => {
        if (!(row.row_id in rowsById)) {
            rowsById[row.row_id] = {
                run_id: row.run_id,
                accn: row.accn,
                total_spots: row.total_spots * 1, // convert to int
                total_bases: row.total_bases * 1, // convert to int
                files: []
            }
        }
        if (row.file_id)
            rowsById[row.row_id]['files'].push({
                file_id: row.file_id,
                file_type: row.file_type,
                file_format: row.file_format,
                url: row.url,
            });
    })

    res.json(
        Object.values(rowsById)
    );
});

router.post('/samples/files', async (req, res) => {
    let ids; // file IDs
    if (req.body.ids) {
        ids = req.body.ids.split(',');
        console.log("ids:", ids);
    }

    let query =
        `SELECT f.file_id,f.url AS file_url,ff.name AS file_format,ft.name AS file_type,r.accn AS run_accn,
            l.layout,l.source,l.strategy,l.selection,e.experiment_id,e.accn AS experiment_accn,
            s.sample_id,s.accn AS sample_accn,p.project_id,p.name AS project_name
        FROM project p
        JOIN project_to_sample pts ON pts.project_id=p.project_id
        JOIN sample s ON s.sample_id=pts.sample_id
        JOIN experiment e ON e.sample_id=s.sample_id
        JOIN library l ON l.experiment_id=e.experiment_id
        JOIN run r ON r.experiment_id=e.experiment_id
        JOIN run_to_file rtf ON rtf.run_id=r.run_id
        JOIN file f ON f.file_id=rtf.file_id
        LEFT JOIN file_format ff ON f.file_format_id=ff.file_format_id
        LEFT JOIN file_type ft ON f.file_type_id=ft.file_type_id`;

    let result;
    if (ids)
        result = await db.query({
            text: query + ' WHERE f.file_id = ANY($1)',
            values: [ids]
        });
    else
        result = await db.query(query);

    res.json(result.rows);
});

router.get('/samples/files/properties', async (req, res) => {
    let results =
        await Promise.all([
// Not needed for now
//            db.query({
//                text:
//                    `SELECT 'format' AS field,ff.name,COUNT(f.file_id)::int
//                    FROM experiment e
//                    LEFT JOIN run r ON (r.experiment_id=e.experiment_id)
//                    JOIN run_to_file rtf ON rtf.run_id=r.run_id
//                    JOIN file f ON f.file_id=rtf.file_id
//                    LEFT JOIN file_format ff ON ff.file_format_id=f.file_format_id
//                    GROUP BY ff.file_format_id`,
//                rowMode: 'array'
//            }),
//            db.query({
//                text:
//                    `SELECT 'type' AS field,ft.name,COUNT(f.file_id)::int
//                    FROM experiment e
//                    LEFT JOIN run r ON (r.experiment_id=e.experiment_id)
//                    JOIN run_to_file rtf ON rtf.run_id=r.run_id
//                    JOIN file f ON f.file_id=rtf.file_id
//                    LEFT JOIN file_type ft ON ft.file_type_id=f.file_type_id
//                    GROUP BY ft.file_type_id`,
//                rowMode: 'array'
//            }),
        ].concat(
            [ 'source', 'strategy', 'selection', 'layout'].map(col =>
                db.query({
                    text:
                        `SELECT '${col}' AS field,COALESCE(${col},'none'),COUNT(rtf.file_id)::int
                        FROM experiment e
                        JOIN library l ON l.experiment_id=e.experiment_id
                        JOIN run r ON r.experiment_id=e.experiment_id
                        JOIN run_to_file rtf ON rtf.run_id=r.run_id
                        GROUP BY ${col}`,
                    rowMode: 'array'
                })
            )
        ));

    let dist = {};

    results.forEach(result => {
        result.rows.forEach(row => {
            if (!(row[0] in dist))
                dist[row[0]] = [];
            dist[row[0]].push(row.slice(1));
        })
    });

    res.json(dist);
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
            val = termIndex.getLabelForValue(row.string_vals[i]); // translate purl value to label
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