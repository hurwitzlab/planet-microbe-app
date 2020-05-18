'use strict';

const router = require('express').Router();
const { requireAuth, asyncHandler } = require('../util');
const client = require('../postgres');

router.get('/apps', asyncHandler(async (req, res) => {
    let result = await client.query("SELECT app_id,name,provider,is_active,is_maintenance FROM app WHERE is_active=TRUE");
    res.json(result.rows);
}));

router.get('/apps/:id(\\d+)', asyncHandler(async (req, res) => {
    let id = req.params.id;
    let result = await client.query({
        text: "SELECT app_id,name,provider,is_active,is_maintenance FROM app WHERE app_id=$1",
        values: [id]
    });
    if (result.rowCount == 0)
        res.status(404);
    else
        res.json(result.rows[0]);
}));

router.get('/apps/:name([\\w\\.\\-\\_]+)', asyncHandler(async (req, res) => {
    let name = req.params.name;
    let result = await client.query("SELECT app_id,name,provider,is_active,is_maintenance FROM app ORDER BY name DESC");

    for (let app of result.rows) {
        let nameWithoutVersion = app.name.replace(/-(\d+\.)?\d+\.\d+(u\d+)?$/, '');
        if (app.name.toLowerCase() == name || nameWithoutVersion.toLowerCase() == name) {
            res.json(app);
            return;
        }
    }

    res.status(404).json([]);
}));

router.post('/apps/runs', asyncHandler(async (req, res) => {
    let app_id = req.body.app_id;
    let params = req.body.params;

    //errorOnNull(app_id, params); // TODO

    requireAuth(req);

    let user_id = req.auth.user.user_id;

    let result = await client.query({
        text: "INSERT INTO app_run (app_id,user_id,params) VALUES ($1,$2,$3) RETURNING *",
        values: [app_id,user_id,params]
    });

    res.json(result.rows[0]);
}));

module.exports = router;