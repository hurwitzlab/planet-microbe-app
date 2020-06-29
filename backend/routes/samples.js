'use strict';

const router = require('express').Router();
const client = require('../postgres');
const { asyncHandler } = require('../util');

router.post('/samples', asyncHandler(async (req, res) => {
    let result;
    let ids;

    if (req.body.ids) {
        ids = req.body.ids.split(',');
        console.log("ids:", ids);
    }

    if (ids) {
        result = await client.query({
            text:
                `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations,p.project_id,p.name AS project_name
                FROM sample s
                JOIN project_to_sample USING(sample_id)
                JOIN project p USING(project_id)
                WHERE s.sample_id = ANY($1)`,
            values: [ids]
        });
    }
    else {
        result = await client.query(
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations,p.project_id,p.name AS project_name
            FROM sample s
            JOIN project_to_sample USING(sample_id)
            JOIN project p USING(project_id)`
        );
    }

    if (result.rowCount == 0) {
        res.status(404).send();
        return;
    }

    res.json(result.rows);
}));

router.get('/samples/:id(\\d+)', asyncHandler(async (req, res) => {
    const id = req.params.id;

    const result = await client.query({
        text:
            `SELECT s.sample_id,s.accn,
                ST_AsGeoJson(s.locations)::json->'coordinates' AS locations,
                p.project_id,p.name AS project_name,
                array_agg(file_id) AS files
            FROM sample s
            JOIN project_to_sample USING(sample_id)
            JOIN project p USING(project_id)
            LEFT JOIN experiment USING(sample_id)
            LEFT JOIN run USING(experiment_id)
            LEFT JOIN run_to_file USING(run_id)
            WHERE s.sample_id=$1
            GROUP BY s.sample_id,p.project_id`,
        values: [id]
    });

    if (result.rowCount == 0) {
        res.status(404).send();
        return;
    }

    res.json(result.rows[0]);
}));

router.get('/samples/:id(\\d+)/sampling_events', asyncHandler(async (req, res) => {
    const id = req.params.id;
    const result = await client.query({
        text:
            `SELECT se.sampling_event_id,se.sampling_event_type,se.name,
                c.campaign_id,c.campaign_type,c.name AS campaign_name
            FROM sample s
            JOIN sample_to_sampling_event USING(sample_id)
            JOIN sampling_event se USING(sampling_event_id)
            LEFT JOIN campaign c USING(campaign_id)
            WHERE s.sample_id=$1`,
        values: [id]
    });

    res.json(result.rows);
}));

router.get('/samples/:id(\\d+)/experiments', asyncHandler(async (req, res) => {
    const id = req.params.id;
    const result = await client.query({
        text:
            `SELECT e.experiment_id,e.name,e.accn
            FROM experiment e
            WHERE e.sample_id=$1`,
        values: [id]
    });

    res.json(result.rows);
}));

router.post('/samples/experiments', asyncHandler(async (req, res) => {
    let ids;
    if (req.body.ids) {
        ids = req.body.ids.split(',');
        console.log("ids:", ids);
    }

    let result;
    if (ids)
        result = await client.query({
            text: `SELECT e.experiment_id,e.name,e.accn,l.name AS library_name,l.strategy AS library_strategy, l.source AS library_source, l.selection AS library_selection, l.protocol AS library_protocol, l.layout AS library_layout, l.length AS library_length
                FROM experiment e
                LEFT JOIN library l USING(experiment_id)
                WHERE e.sample_id = ANY($1)`,
            values: [ids]
        });
    else
        result = await client.query(
            `SELECT e.experiment_id,e.name,e.accn
            FROM experiment e`
        );

    res.json(result.rows);
}));

router.post('/samples/files', asyncHandler(async (req, res) => {
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
        JOIN project_to_sample USING(project_id)
        JOIN sample s USING(sample_id)
        JOIN experiment e USING(sample_id)
        JOIN library l USING(experiment_id)
        JOIN run r USING(experiment_id)
        JOIN run_to_file USING(run_id)
        JOIN file f USING(file_id)
        LEFT JOIN file_format ff USING(file_format_id)
        LEFT JOIN file_type ft USING(file_type_id)`;

    let result;
    if (ids)
        result = await client.query({
            text: query + ' WHERE f.file_id = ANY($1)',
            values: [ids]
        });
    else
        result = await client.query(query);

    res.json(result.rows);
}));

router.get('/samples/files/properties', asyncHandler(async (req, res) => {
    let results =
        await Promise.all([
// Not needed for now
//            client.query({
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
//            client.query({
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
                client.query({
                    text:
                        `SELECT '${col}' AS field,COALESCE(${col},'none'),COUNT(rtf.file_id)::int
                        FROM experiment e
                        JOIN library USING(experiment_id)
                        JOIN run USING(experiment_id)
                        JOIN run_to_file rtf USING(run_id)
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
}));

router.get('/samples/:id(\\d+)/metadata', asyncHandler(async (req, res) => {
    const id = req.params.id;
    const termIndex = req.app.get('termIndex');

    let result = await client.query({
        text:
            `SELECT s.schema_id,schema.fields->'fields' AS fields,s.number_vals,s.string_vals,s.datetime_vals
            FROM sample s
            JOIN schema USING(schema_id)
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
}));

router.get('/samples/:id(\\d+)/taxonomy', asyncHandler(async (req, res) => {
    const id = req.params.id;

    const result = await client.query({
        text:
            `SELECT e.experiment_id,e.accn AS experiment_accn,r.run_id,r.accn AS run_accn,t.tax_id,t.name,c.num_reads,c.num_unique_reads,c.abundance
            FROM sample s
            JOIN experiment e USING(sample_id)
            JOIN run r USING(experiment_id)
            LEFT JOIN run_to_taxonomy c USING(run_id)
            LEFT JOIN taxonomy t USING(tax_id)
            WHERE c.abundance > 0 AND s.sample_id=$1`,
        values: [id]
    });

    res.json(result.rows);
}));

module.exports = router;