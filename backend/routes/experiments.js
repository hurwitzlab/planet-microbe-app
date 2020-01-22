'use strict';

const express = require('express');
const router  = express.Router();
const config = require('../config.json');
const db = require('../postgres.js')(config);

router.get('/experiments/:id(\\d+)', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT e.experiment_id,e.name,e.accn,l.name AS library_name,l.strategy AS library_strategy, l.source AS library_source, l.selection AS library_selection, l.protocol AS library_protocol, l.layout AS library_layout, l.length AS library_length,s.sample_id,s.accn AS sample_accn,p.project_id,p.name AS project_name
            FROM experiment e
            LEFT JOIN library l ON l.experiment_id=e.experiment_id
            JOIN sample s ON s.sample_id=e.sample_id
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id
            JOIN project p ON p.project_id=pts.project_id
            WHERE e.experiment_id=$1`,
        values: [id]
    });

    res.json(result.rows[0]);
});

router.get('/experiments/:id(\\d+)/runs', async (req, res) => {
    let id = req.params.id;
    let result = await db.query({
        text:
            `SELECT r.run_id,r.accn,r.total_spots,r.total_bases,f.file_id,f.url,ft.name AS file_type,ff.name AS file_format
            FROM run r
            LEFT JOIN run_to_file rtf ON rtf.run_id=r.run_id
            LEFT JOIN file f ON f.file_id=rtf.file_id
            LEFT JOIN file_type ft ON ft.file_type_id=f.file_type_id
            LEFT JOIN file_format ff ON ff.file_format_id=f.file_format_id
            WHERE r.experiment_id=$1`,
        values: [id]
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

module.exports = router;