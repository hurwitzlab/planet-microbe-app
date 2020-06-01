'use strict';

const router = require('express').Router();
const { asyncHandler } = require('../util');
const { requireAuth } = require('../auth');
const client = require('../postgres');

router.get('/apps', asyncHandler(async (req, res) => {
    let result = await client.query("SELECT app_id,name,provider,is_active,is_maintenance FROM app WHERE is_active=TRUE");
    res.json(result.rows);
}));

router.get('/apps/:id(\\d+)', asyncHandler(async (req, res) => {
    const id = req.params.id;
    const app = await getApp(id);
    if (!app)
        res.status(404);
    else
        res.json(app);
}));

router.get('/apps/:name([\\w\\.\\-\\_]+)', asyncHandler(async (req, res) => {
    const name = req.params.name;
    const result = await client.query("SELECT app_id,name FROM app ORDER BY name DESC");

    for (let row of result.rows) {
        let nameWithoutVersion = row.name.replace(/-(\d+\.)?\d+\.\d+(u\d+)?$/, '');
        if (row.name.toLowerCase() == name || nameWithoutVersion.toLowerCase() == name) {
            const app = await getApp(row.app_id);
            res.json(app);
            return;
        }
    }

    res.status(404).json([]);
}));

router.post('/apps/runs', requireAuth, asyncHandler(async (req, res) => {
    const app_id = req.body.app_id;
    const params = req.body.params;
    const user_id = req.auth.user.user_id;

    const result = await client.query({
        text: "INSERT INTO app_run (app_id,user_id,params) VALUES ($1,$2,$3) RETURNING *",
        values: [app_id,user_id,params]
    });

    res.json(result.rows[0]);
}));

async function getApp(id) {
    let result = await client.query({
        text:
            `SELECT app_id,app.name,provider,is_active,is_maintenance
            FROM app
            WHERE app_id=$1`,
        values: [id]
    });

    if (result.rowCount == 0)
        return;

    let app = result.rows[0];

    result = await client.query({
        text:
            `SELECT app_result_id,path,adt.name
            FROM app_result
            JOIN app_data_type adt USING (app_data_type_id)
            WHERE app_id=$1`,
        values: [id]
    });

    app.app_results = result.rows;

    return app;
}

module.exports = router;