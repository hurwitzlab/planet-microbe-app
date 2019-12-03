const express = require('express');
const app = express();
const cors = require('cors');
const logger = require('morgan');
const bodyParser = require('body-parser');
const Promise = require('promise');
const {Client} = require('pg');
const shortid = require('shortid');
const fs = require('fs');
const stringSimilarity = require('string-similarity');
const sendmail = require('sendmail')();
const requestp = require('request-promise');
const config = require('./config.json');


// Create error types
class MyError extends Error {
    constructor(message, statusCode) {
        super(message);
        this.statusCode = statusCode;
    }
}

const ERR_BAD_REQUEST = new MyError("Bad request", 400);
const ERR_UNAUTHORIZED = new MyError("Unauthorized", 401);
const ERR_PERMISSION_DENIED = new MyError("Permission denied", 403);
const ERR_NOT_FOUND = new MyError("Not found", 404);

//var schemaIndex = {};
var rdfTermIndex = {};
var db;

// Initialize
(async function() {
    db = new Client({
        user: config.postgres.username,
        host: 'localhost',
        database: config.postgres.db,
        password: config.postgres.password
    });
    await db.connect();

    rdfTermIndex = await generateTermIndex(db, config.ontologies);

    app.listen(config.serverPort, () => console.log('Server listening on port', config.serverPort));
})();

//----------------------------------------------------------------------------------------------------------------------

app.use(logger('dev'));
app.use(cors());
app.use(bodyParser.json()); // support json encoded bodies
app.use(agaveTokenValidator);

app.get('/index', (req, res) => { //TODO rename to "catalog", as in a catalog of terms
    res.json(rdfTermIndex);
});

app.get('/schema', async (req, res) => {
    let fields = await query("SELECT schema_id,name,fields->'fields' AS fields FROM schema");
    res.json(fields.rows);
});

app.get('/searchTerms', (req, res) => {
    let query = req.query.query;

    if (query) {
        const MIN_MATCH_RATING = 0.1;
        let labels = Object.values(rdfTermIndex).map(term => term.label).filter(label => label);
        let matches = stringSimilarity.findBestMatch(query, labels);
        let bestLabels = matches.ratings.filter(m => m.rating > MIN_MATCH_RATING).map(m => m.target);
        let terms = Object.values(rdfTermIndex).filter(term => bestLabels.includes(term.label));
        res.json(terms);
    }
    else {
        res.json(
            Object.values(rdfTermIndex)
            .filter(term => term.id && term.schemas)
            .map(term => {
                let aliases = [].concat.apply([], Object.values(term.schemas)).map(schema => { return { name: schema.name, sourceName: schema.sourceName, sourceUrl: schema.sourceUrl } });

                let annotations = [];
                if (term.annotations) { // TODO move into function
                    annotations = term.annotations.map(a => {
                        let label = getLabelForValue(a.id);
                        let value = getLabelForValue(a.value);

                        return {
                            id: a.id,
                            label: label,
                            value: value
                        };
                    });
                }

                return {
                    id: term.id,
                    label: term.label,
                    definition: term.definition,
                    unitLabel: term.unitLabel,
                    type: term.type,
                    aliases: aliases,
                    annotations: annotations
                };
            })
        );
    }
});

app.get('/searchTerms/:id(*)', async (req, res) => {
    let id = decodeURIComponent(req.params.id);

    // Workaround for http:// in id changed to http:/ on MYO (NGINX?)
    if (id.startsWith('http:/'))
	    id = id.replace(/^http:\//, '');

    let term = getTerm(id);
    if (!term) {
        res.status(404).json({ error: "Term not found" });
        return;
    }
    console.log("term:", term);

    let aliases = [];
    if (term.schemas) // TODO move into function
        aliases = [].concat.apply([],
            Object.values(term.schemas)).map(schema => {
                return {
                    name: schema.name,
                    sourceName: schema.sourceName,
                    sourceUrl: schema.sourceUrl
                }
            });

    let annotations = [];
    if (term.annotations) { // TODO move into function
        annotations = term.annotations.map(a => {
            let label = getLabelForValue(a.id);
            let value = getLabelForValue(a.value);

            return {
                id: a.id,
                label: label,
                value: value
            };
        });
    }

    if (term.type == "string") {
        let cases = [].concat.apply([], Object.values(term.schemas)).map(schema => `WHEN schema_id=${schema.schemaId} THEN string_vals[${schema.position}]`);
        let caseStr = "CASE " + cases.join(" ") + " END";
        let result = await query({ text: `SELECT COALESCE(LOWER(${caseStr}),'none') AS val,COUNT(*)::int FROM sample GROUP BY val ORDER BY val`, rowMode: 'array' });

        // Convert purl values to labels
        let purlLabels = {};
        for (row of result.rows) { //TODO move into function
            let val = row[0];
            if (val && val.startsWith("http://")) { // is this a purl?
                let term2 = getTerm(val);
                if (term2 && term2.label) {
                    purlLabels[val] = term2.label;
                }
            }
        }

        res.json({
            id: term.id,
            label: term.label,
            definition: term.definition,
            unitLabel: term.unitLabel,
            type: term.type,
            distribution: result.rows,
            aliases: aliases,
            annotations: annotations,
            purlLabels: purlLabels
        });
// TODO
//        })
//        .catch(err => {
//            console.log(err);
//            res.send(err);
//        });
    }
    else if (term.type == "number") {
        let cases = [].concat.apply([], Object.values(term.schemas)).map(schema => `WHEN schema_id=${schema.schemaId} THEN number_vals[${schema.position}]`);
        let caseStr = "CASE " + cases.join(" ") + " END";
        let clauses = [].concat.apply([], Object.values(term.schemas)).map(schema => `(schema_id=${schema.schemaId} AND number_vals[${schema.position}] != 'NaN')`);
        let clauseStr = clauses.join(' OR ');
        let rangeResult = await query({ text: `SELECT MIN(${caseStr}),MAX(${caseStr}) FROM sample WHERE ${clauseStr}`, rowMode: "array" });
        let [min, max] = rangeResult.rows[0];
        let range = max - min;
        let binLen = range/10;

        let binResult = await query({
            text:
                `SELECT label, count FROM
                    (WITH min_max AS (
                        SELECT
                            MIN(${caseStr}) AS min_val,
                            MAX(${caseStr}) AS max_val
                        FROM sample
                        WHERE ${clauseStr}
                    )
                    SELECT
                        REGEXP_REPLACE(REGEXP_REPLACE(CONCAT(MIN(${caseStr}),' - ',MAX(${caseStr})),'^ - $','None'),'NaN - NaN','Below Detection Limit') AS label,
                        WIDTH_BUCKET(NULLIF(${caseStr},'NaN'),min_val,max_val,10) AS bucket,COUNT(*)::int
                    FROM sample, min_max
                    GROUP BY bucket
                    ORDER BY bucket) AS foo`,
            rowMode: 'array'
        });

        res.json({
            id: term.id,
            label: term.label,
            definition: term.definition,
            unitLabel: term.unitLabel,
            type: term.type,
            min: min,
            max: max,
            distribution: binResult.rows,
            aliases: aliases,
            annotations: annotations
        });
// TODO
//        .catch(err => {
//            console.log(err);
//            res.send(err);
//        });
    }
    else if (term.type == "datetime") {
        let queries = term.schema.map(schema => {
            return {
                text: "SELECT MIN(datetime_vals[$1]),MAX(datetime_vals[$1]) FROM sample WHERE schema_id=$2",
                values: [schema.position,schema.schemaId*1],
                rowMode: 'array'
            };
        });

        let min, max;
        let results = await batchQuery(queries); // FIXME would a single query be faster?
        results.forEach(vals => {
            vals.rows.forEach(row => {
                if (typeof row[0] !== "undefined" && typeof row[1] !== "undefined") {
                    min = (typeof min === "undefined" ? row[0].getTime() : Math.min(min, row[0].getTime()));
                    max = (typeof max === "undefined" ? row[1].getTime() : Math.max(max, row[1].getTime()));
                }
            });
        });

        res.json({
            id: term.id,
            label: term.label,
            definition: term.definition,
            unitLabel: term.unitLabel,
            type: term.type,
            aliases: aliases,
            min: new Date(min).toISOString(),
            max: new Date(max).toISOString(),
            annotations: annotations
        });
// TODO
//        .catch(err => {
//            console.log(err);
//            res.send(err);
//        });
    }
    else {
        console.log("ERROR: unknown term type '" + term.type + "'");
        res.status(500).json({
            error: "Unknown term type"
        });
    }
});

function getLabelForValue(val) {
    if (val in rdfTermIndex)
        return getLabelForValue(rdfTermIndex[val].label);
    else
        return val;
}

app.get('/search', async (req, res) => {
    try {
        let results = await search(db, req.query);
        res.json(results);
    }
    catch(err) {
        console.log(err);
        res.status(500).json({ error: err });
    }
});

app.get('/projects', async (req, res) => {
    let result = await query(
        `SELECT p.project_id,p.name,p.accn,p.description,p.datapackage_url,p.url,pt.name AS type,
            (SELECT count(*) FROM project_to_sample pts WHERE pts.project_id=p.project_id) AS sample_count
        FROM project p
        JOIN project_type pt ON p.project_type_id=pt.project_type_id`
    );
    result.rows.forEach(row => row.sample_count *= 1); // convert count to int
    res.json(result.rows);
//    .catch(err => {
//        console.log(err);
//        res.send(err);
//    });
});

app.get('/projects/:id(\\d+)', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text:
            `SELECT
                p.project_id,p.name,p.accn,p.description,p.datapackage_url,p.url AS project_url,pt.name AS type,f.file_id,f.url,ft.name AS file_type,ff.name AS file_format,
                (SELECT count(*) FROM project_to_sample pts WHERE pts.project_id=p.project_id) AS sample_count
            FROM project p
            JOIN project_type pt ON p.project_type_id=pt.project_type_id
            LEFT JOIN project_to_file ptf ON ptf.project_id=p.project_id
            LEFT JOIN file f ON f.file_id=ptf.file_id
            LEFT JOIN file_type ft ON ft.file_type_id=f.file_type_id
            LEFT JOIN file_format ff ON ff.file_format_id=f.file_format_id
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

app.get('/projects/:id(\\d+)/campaigns', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text:
            `SELECT c.campaign_id,c.campaign_type,c.name,c.description,c.deployment,c.start_location,c.end_location,c.start_time,c.end_time,c.urls
            FROM project_to_sample pts
            JOIN sample s ON s.sample_id=pts.sample_id
            JOIN sample_to_sampling_event stse ON stse.sample_id=s.sample_id
            JOIN sampling_event se ON se.sampling_event_id=stse.sampling_event_id
            JOIN campaign c ON c.campaign_id=se.campaign_id
            WHERE pts.project_id=$1
            GROUP BY c.campaign_id`,
        values: [id]
    });
    res.json(result.rows);
});

app.get('/projects/:id(\\d+)/sampling_events', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text:
            `SELECT se.sampling_event_id,se.sampling_event_type,se.name,ST_AsGeoJson(se.locations)::json->'coordinates' AS locations,se.start_time,se.end_time
            FROM project_to_sample pts
            JOIN sample s ON s.sample_id=pts.sample_id
            JOIN sample_to_sampling_event stse ON stse.sample_id=s.sample_id
            JOIN sampling_event se ON se.sampling_event_id=stse.sampling_event_id
            WHERE pts.project_id=$1
            GROUP BY se.sampling_event_id`,
        values: [id]
    });

    res.json(result.rows);
});

app.get('/projects/:id(\\d+)/samples', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text:
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations
            FROM sample s
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id
            WHERE pts.project_id=$1`,
        values: [id]
    });

    res.json(result.rows);
});

app.post('/samples', async (req, res) => {
    let result;
    let ids = req.body.ids;
    console.log("ids:", ids);

    if (ids) {
        result = await query(
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations,p.project_id,p.name AS project_name
            FROM sample s
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id
            JOIN project p ON p.project_id=pts.project_id
            WHERE s.sample_id IN (${ids})`
        );
    }
    else {
        result = await query(
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations,p.project_id,p.name AS project_name
            FROM sample s
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id
            JOIN project p ON p.project_id=pts.project_id`
        );
    }

    res.json(result.rows);
});

app.get('/samples/:id(\\d+)', async (req, res) => {
    let id = req.params.id;

    let result = await query({
        text:
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations,p.project_id,p.name AS project_name
            FROM sample s
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id
            JOIN project p ON p.project_id=pts.project_id
            WHERE s.sample_id=$1`,
        values: [id]
    });

    res.json(result.rows[0]);
});

app.get('/samples/:id(\\d+)/sampling_events', async (req, res) => {
    let id = req.params.id;
    let result = await query({
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

app.get('/samples/:id(\\d+)/experiments', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text:
            `SELECT e.experiment_id,e.name,e.accn
            FROM experiment e
            WHERE e.sample_id=$1`,
        values: [id]
    });

    res.json(result.rows);
});

app.post('/samples/files', async (req, res) => {
    let ids;
    if (req.body.ids) {
        ids = req.body.ids.split(',');
        console.log("ids:", ids);
    }

    let result;
    if (ids)
        result = await query(
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
        result = await query(
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

app.get('/samples/files/formats', async (req, res) => {
    let result = await query(
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

app.get('/samples/files/types', async (req, res) => {
    let result = await query(
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

app.get('/samples/:id(\\d+)/metadata', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text:
            `SELECT s.schema_id,schema.fields->'fields' AS fields,s.number_vals,s.string_vals,s.datetime_vals
            FROM sample s
            JOIN schema ON schema.schema_id=s.schema_id
            WHERE s.sample_id=$1`,
        values: [id]
    });

    let row = result.rows[0];
    let terms = [];
    let values = [];
    for (i = 0; i < row.fields.length; i++) {
        let field = row.fields[i];

        let term = {};
        let term2 = getTerm(field.rdfType) || {};
        if (term2.annotations) { // TODO move into function
            term.annotations = term2.annotations.map(a => {
                let label = getLabelForValue(a.id);
                let value = getLabelForValue(a.value);
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

app.get('/campaigns/:id(\\d+)', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text:
            `SELECT c.campaign_id,c.campaign_type,c.name,c.description,c.deployment,c.start_location,c.end_location,c.start_time,c.end_time,c.urls,p.project_id,p.name AS project_name
            FROM campaign c
            JOIN sampling_event se ON se.campaign_id=c.campaign_id
            JOIN sample_to_sampling_event stse ON stse.sampling_event_id=se.sampling_event_id
            JOIN sample s ON s.sample_id=stse.sample_id
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id
            JOIN project p ON p.project_id=pts.project_id
            WHERE c.campaign_id=$1`,
        values: [id]
    });
    res.json(result.rows[0]);
});

app.get('/campaigns/:id(\\d+)/sampling_events', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text:
            `SELECT se.sampling_event_id,se.sampling_event_type,se.name,ST_AsGeoJson(se.locations)::json->'coordinates' AS locations,se.start_time,se.end_time
            FROM sampling_event se
            JOIN campaign c ON c.campaign_id=se.campaign_id
            WHERE c.campaign_id=$1`,
        values: [id]
    });

    res.json(result.rows);
});

app.get('/campaigns/:id(\\d+)/samples', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text:
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations
            FROM sample s
            JOIN sample_to_sampling_event stse ON stse.sample_id=s.sample_id
            JOIN sampling_event se ON se.sampling_event_id=stse.sampling_event_id
            WHERE se.campaign_id=$1
            GROUP BY s.sample_id`,
        values: [id]
    });

    res.json(result.rows);
});

app.get('/sampling_events/:id(\\d+)', async (req, res) => {
    let id = req.params.id;
    let result = await query({
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

app.get('/sampling_events/:id(\\d+)/samples', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text:
            `SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations
            FROM sample s
            JOIN sample_to_sampling_event stse ON stse.sample_id=s.sample_id
            WHERE stse.sampling_event_id=$1`,
        values: [id]
    });

    res.json(result.rows);
});

app.get('/experiments/:id(\\d+)', async (req, res) => {
    let id = req.params.id;
    let result = await query({
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

app.get('/experiments/:id(\\d+)/runs', async (req, res) => {
    let id = req.params.id;
    let result = await query({
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

app.post('/contact', (req, res) => {
    console.log(req.body);

    if (!config.supportEmail) {
        console.log("Error: missing supportEmail in config");
        res.status(500).json({
            status: "failed"
        });
        return;
    }

    var name = req.body.name || "Unknown";
    var email = req.body.email || "Unknown";
    var message = req.body.message || "";

    sendmail({
        from: email,
        to: config.supportEmail,
        subject: 'Support request',
        html: message,
    }, (err, reply) => {
        console.log(err && err.stack);
        console.dir(reply);
    });

    res.json({
        status: "success"
    });
});

app.get('/apps', async (req, res) => {
    let result = await query("SELECT app_id,name,provider,is_active,is_maintenance FROM app WHERE is_active=TRUE");
    res.json(result.rows);
//    .catch(err => {
//        console.log(err);
//        res.send(err);
//    });
});

app.get('/apps/:id(\\d+)', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text: "SELECT app_id,name,provider,is_active,is_maintenance FROM app WHERE app_id=$1",
        values: [id]
    });

    res.json(result.rows[0]);
});

app.get('/apps/:name([\\w\\.\\-\\_]+)', async (req, res) => {
    let name = req.params.name;
    let result = await query("SELECT app_id,name,provider,is_active,is_maintenance FROM app ORDER BY name DESC");

    for (let app of result.rows) {
        let nameWithoutVersion = app.name.replace(/-(\d+\.)?\d+\.\d+(u\d+)?$/, '');
        if (app.name.toLowerCase() == name || nameWithoutVersion.toLowerCase() == name) {
            res.json(app);
            return;
        }
    }

    res.status(404).json([]);
});

app.post('/apps/runs', async (req, res) => {
    let app_id = req.body.app_id;
    let params = req.body.params;

    //errorOnNull(app_id, params); // TODO

    requireAuth(req);

    let user_id = req.auth.user.user_id;

    let result = await query({
        text: "INSERT INTO app_run (app_id,user_id,params) VALUES ($1,$2,$3) RETURNING *",
        values: [app_id,user_id,params]
    });

    res.json(result.rows[0]);
});

app.post('/token', async (req, res) => {
    let provider = req.body.provider;
    let code = req.body.code;
    let tokenResponse = await agaveGetToken(provider, code);
    res.send(tokenResponse);
});

app.post('/users/login', async (req, res) => { // TODO add try/catch error handling
    requireAuth(req);

    var username = req.auth.user.user_name;

    // Add user if not already present
    let user = await query({
        text: "SELECT * FROM \"user\" WHERE user_name=$1",
        values: [username]
    });

    if (user.rowCount == 0) {
        user = await query({
            text: "INSERT INTO \"user\" (user_name) VALUES ($1) RETURNING *",
            values: [username]
        });
    }

    // For new user set first_name/last_name/email, or update for existing user (in case they changed any of those fields)
    // Only do this once at login and not in agaveTokenValidator
    user = await query({
        text: "UPDATE \"user\" SET first_name=$1, last_name=$2, email=$3 WHERE user_name=$4",
        values: [req.auth.user.first_name,req.auth.user.last_name, req.auth.user.email, username]
    });

    user = await query({
        text: "SELECT * FROM \"user\" WHERE user_name=$1",
        values: [username]
    });

    let login = await query({
        text: "INSERT INTO login (user_id) VALUES ($1) RETURNING *",
        values: [user.rows[0].user_id]
    });

    res.json(user.rows[0]);
});

app.use(errorHandler);

// Catch-all function
app.get('*', function(req, res, next){
    res.status(404).send("Unknown route: " + req.path);
});

//----------------------------------------------------------------------------------------------------------------------

function requireAuth(req) {
    if (!req || !req.auth || !req.auth.validToken || !req.auth.user)
        throw(ERR_UNAUTHORIZED);
}

function errorHandler(error, req, res, next) {
    console.log("ERROR ".padEnd(80, "!"));
    console.log(error.stack);

    let statusCode = error.statusCode || 500;
    let message = error.message || "Unknown error";

    res.status(statusCode).send(message);
}

async function agaveTokenValidator(req, res, next) {
    var token;
    if (req && req.headers)
        token = req.headers.authorization;
    console.log("validateAgaveToken: token:", token);

    req.auth = {
        validToken: false
    };

    if (token) {
//        try {
            let response = await getAgaveProfile(token);
            if (!response || response.status != "success") {
                console.log('validateAgaveToken: !!!! Bad profile status: ' + response.status);
            }
            else {
                response.result.token = token;
            }

            let profile = response.result;
            if (profile) {
                console.log("validateAgaveToken: *** success ***  username:", profile.username);

                req.auth = {
                    validToken: true,
                    profile: profile
                };

                // Add user if not already present
                let user = await query({
                    text: "SELECT * FROM \"user\" WHERE user_name=$1",
                    values: [profile.username]
                });

                if (user.rowCount == 0) {
                    user = await query({
                        text: "INSERT INTO \"user\" (user_name) VALUES ($1) RETURNING *",
                        values: [profile.username]
                    });
                }

                user = user.rows[0];
                user.first_name = profile.first_name;
                user.last_name = profile.last_name;
                user.email = profile.email

                if (user)
                    req.auth.user = user;
            }
//        }
//        catch( error => {
//            console.log("validateAgaveToken: !!!!", error.message);
//        })
//        .finally(next);
    }

    next();
}

async function getAgaveProfile(token) {
    return await requestp({
        method: "GET",
        uri: "https://agave.iplantc.org/profiles/v2/me", // FIXME hardcoded
        headers: {
            Authorization: "Bearer " + token,
            Accept: "application/json"
        },
        json: true
    });
}

async function agaveGetToken(provider, code) {
    let url = config.oauthProviders[provider].tokenUrl;
    let options = {
        method: "POST",
        uri: url,
        form: {
            grant_type: "authorization_code",
            client_id: config.oauthProviders.agave.clientId,
            client_secret: config.oauthProviders.agave.clientSecret,
            redirect_uri: config.oauthProviders.agave.redirectUrl,
            code: code
        }
    };

    console.log(provider, ": sending authorization POST", url);
    let response = await requestp(options);
    console.log(response);
    return(response);
}

// TODO refactor/simplify this ugly af code
async function search(db, params) {
    console.log("params:", params);

    let limit = params['limit'] || 20;
    let limitStr = (limit ? " LIMIT " + limit : "");

    let offset = params['offset'] || 0;
    let offsetStr = (offset ? " OFFSET " + offset : "");

    let sort = params['sort'] || 1;

    let result = params['result'] || 'sample';

    let map = (params['map'] || 1) == 1;

//    let columns;
//    if (params['columns'])
//        columns = params['columns'].split(',');

    let summaryColumns = [];
    if (params['summary'])
        summaryColumns = params['summary'].split(',');

    let gisClause, gisSelect;
    if (params['location']) {
        let val = params['location'];
        if (val.match(/\[-?\d*(\.\d+)?\,-?\d*(\.\d+)?,-?\d*(\.\d+)?\]/)) { // [lat, lng, radius] in meters
            let bounds = JSON.parse(val);
            console.log("location:", bounds);
            gisSelect = "replace(replace(replace(replace(ST_AsGeoJson(ST_FlipCoordinates(locations::geometry))::json->>'coordinates', '[[', '['), ']]', ']'), '[', '('), ']', ')')"; //FIXME ugly af
            gisClause = "ST_DWithin(ST_MakePoint(" + bounds[1] + "," + bounds[0] + ")::geography, locations, " + bounds[2] + ")";
        }
        else if (val) {
            //TODO error
            console.log("Error: invalid location query", val);
        }
    }

    let projectClause;
    if (params['project']) {
        let vals = params['project'].split("|");
        console.log("project match", vals);
        projectClause = "LOWER(p.name) IN (" + vals.map(s => "'" + s + "'").join(",").toLowerCase() + ")";
    }

    let fileFormatClause;
    if (params['fileFormat']) {
        let vals = params['fileFormat'].split("|");
        console.log("fileFormat match", vals);
        fileFormatClause = "LOWER(ff.name) IN (" + vals.map(s => "'" + s + "'").join(",").toLowerCase() + ")";
    }

    let fileTypeClause;
    if (params['fileType']) {
        let vals = params['fileType'].split("|");
        console.log("fileType match", vals);
        fileTypeClause = "LOWER(ft.name) IN (" + vals.map(s => "'" + s + "'").join(",").toLowerCase() + ")";
    }

    let orTerms = [], andTerms = [];
    let terms = {}, termOrder = [];
    let termsById = {};
    for (param of Object.keys(params).filter(p => p.startsWith("http") || p.startsWith("|http"))) {
        param = param.replace(/\+/gi, ''); // workaround for Elm uri encoding
        let val = params[param];

        let orFlag = false;
        if (param.startsWith("|")) {
            param = param.replace(/^\|/, '');
            orFlag = true;
        }

        let term = getTerm(param);
        if (!term) {
            console.log("Error: term not found for param '" + param + "'");
            continue;
        }

        if (!term.schemas || term.schemas.length == 0) {
            console.log("Error: Schema not found for term", param);
            continue;
        }

        termOrder.push(term.id);
        terms[term.id] = val;
        termsById[term.id] = term;
        if (orFlag || !val)
            orTerms.push(term);
        else
            andTerms.push(term);
        //console.log("term:", term);
    }

    console.log("andTerms:", andTerms.map(t => t.id).join(","));
    console.log("orTerms:", orTerms.map(t => t.id).join(","));

    let schemas = getSchemasForTerms(andTerms);
    console.log("schemas:", schemas);

    let selections = {};
    let clauses = {};

    let andClauses = {};
    for (term of andTerms) {
        let selectStr = "";
        for (schemaId of schemas) {
            let fields = [];
            for (schema of term.schemas[schemaId]) {
                let [field, clause] = buildTermSQL(schema.position, terms[term.id]);

                if (clause) {
                    if (!andClauses[schemaId])
                        andClauses[schemaId] = []
                    andClauses[schemaId].push(clause);

                    if (!clauses[term.id])
                        clauses[term.id] = [];
                    clauses[term.id].push(clause);
                }

                fields.push(field);
            }

            let fieldsStr = "ARRAY[" + fields.join(",") + "]";
            selectStr += " WHEN schema_id=" + schemaId + " THEN " + fieldsStr;
        }

        if (selectStr)
            selections[term.id] = "CASE" + selectStr + " END";
    }

    let orClauses = {};
    for (term of orTerms) {
        let selectStr = "";

        let schemas2 = [];
        if (andTerms.length == 0)
            schemas2 = Object.keys(term.schemas);
        else
            schemas2 = schemas;

        for (schemaId of schemas2) {
            let fields = [];
            if (!(schemaId in term.schemas) || term.schemas[schemaId].length == 0) {
                console.log("!!!! Skipping", schemaId, "for", term.id);
                continue;
            }
            for (schema of term.schemas[schemaId]) {
                let [field, clause] = buildTermSQL(schema.position, terms[term.id]);

                if (clause) {
                    if (!orClauses[schemaId])
                        orClauses[schemaId] = []
                    orClauses[schemaId].push(clause);

                    if (!clauses[term.id])
                        clauses[term.id] = [];
                    clauses[term.id].push(clause);
                }

                fields.push(field);
            }

            let fieldsStr = "ARRAY[" + fields.join(",") + "]";
            selectStr += " WHEN schema_id=" + schemaId + " THEN " + fieldsStr;
        }

        if (selectStr)
            selections[term.id] = "CASE" + selectStr + " END";
    }

//TODO column selection
//    if (columns && columns.length > 0) {
//        selections = [];
//
//        for (param of columns) {
//            let term = getTerm(param);
//            if (!term) {
//                console.log("Error: term not found for column", param);
//                return;
//            }
//            console.log("term:", term);
//
//            let selectStr = "";
//            if (!term.schemas || term.schemas.length == 0)
//                console.log("Error: schema not found for column", param);
//
//            for (schemaId in term.schemas) {
//                for (alias in term.schemas[schemaId]) {
//                    let arrIndex = term.schemas[schemaId][alias];
//                    let [field, clause] = buildTermSQL(arrIndex, "");
//
//                    selectStr += " WHEN schema_id=" + schemaId + " THEN " + field;
//                }
//            }
//
//            if (selectStr)
//                selections.push("CASE" + selectStr + " END");
//        }
//    }

    console.log("selections:", selections);
    console.log("andClauses:", andClauses);
    console.log("orClauses:", orClauses);
    console.log("clauses:", clauses);

    // Build clauses part of query string
    let clauseStr =
        Object.keys(andClauses).concat(Object.keys(orClauses)).filter((v, i, a) => a.indexOf(v) === i).map(schemaId =>
            "(schema_id=" + schemaId + " AND (" +
                (andClauses[schemaId] ? Object.values(andClauses[schemaId]).join(" AND ") : "") +
                (andClauses[schemaId] && orClauses[schemaId] ? " AND " : "") +
                (orClauses[schemaId] ? "(" + Object.values(orClauses[schemaId]).join(" OR ") + ")" : "") + "))"
        )
        .join(" OR ");

    [gisClause, projectClause, fileFormatClause, fileTypeClause].forEach(clause => {
        if (clause)
            clauseStr = clause + (clauseStr ? " AND (" + clauseStr + ")" : "");
    });

    if (clauseStr)
        clauseStr = "WHERE " + clauseStr;

    // Build query
    let tableStr =
        `FROM sample s
        JOIN project_to_sample pts ON pts.sample_id=s.sample_id
        JOIN project p ON p.project_id=pts.project_id `;
// Temporarily removed until Tara SRA files are finished loading. Move to into file query block when uncommented.
//        LEFT JOIN experiment e ON e.sample_id=s.sample_id \
//        LEFT JOIN run r ON r.experiment_id=e.experiment_id \
//        LEFT JOIN run_to_file rtf ON rtf.run_id=r.run_id \
//        LEFT JOIN file f ON f.file_id=rtf.file_id \
//        LEFT JOIN file_type ft ON ft.file_type_id=f.file_type_id \
//        LEFT JOIN file_format ff ON ff.file_format_id=f.file_format_id ";

    let results = [], count = 0, summaries = [], clusters = [];

    if (result == "sample") {
        let groupByStr = " GROUP BY s.sample_id,p.project_id ";

        // Build total count query
        let countQueryStr =
            `SELECT COUNT(foo) FROM (SELECT s.sample_id ${tableStr} ${clauseStr} ${groupByStr}) AS foo`;

        // Build summary queries (for charts)
        let projectSummaryQueryStr =
            `SELECT p.name,COUNT(pts.sample_id)::int
            FROM project p JOIN project_to_sample pts ON pts.project_id=p.project_id JOIN sample s ON pts.sample_id=s.sample_id
            ${clauseStr} GROUP BY p.project_id ORDER BY p.name`;

        let summaryQueryStrs = [ projectSummaryQueryStr ];
        for (termId of summaryColumns) { //FIXME dup'ed in /searchTerms/:id(*) endpoint above
            let term = termsById[termId];
            let queryStr = "";
            if (term.type == 'string') {
                let cases = [].concat.apply([], Object.values(term.schemas)).map(schema => `WHEN schema_id=${schema.schemaId} THEN string_vals[${schema.position}]`);
                let caseStr = "CASE " + cases.join(" ") + " END";
                queryStr = `SELECT COALESCE(LOWER(${caseStr}),'none') AS val,count(*)::int ${tableStr} ${clauseStr} GROUP BY val ORDER BY val`;
            }
            else if (term.type == 'number') {
                let cases = [].concat.apply([], Object.values(term.schemas)).map(schema => `WHEN schema_id=${schema.schemaId} THEN number_vals[${schema.position}]`);
                let caseStr = "CASE " + cases.join(" ") + " END";
                let subClauses = [].concat.apply([], Object.values(term.schemas)).map(schema => `(schema_id=${schema.schemaId} AND number_vals[${schema.position}] != 'NaN')`);
                let subClauseStr = subClauses.join(' OR ');

                queryStr =
                    `SELECT label, count FROM
                        (WITH min_max AS (
                            SELECT
                                MIN(${caseStr}) AS min_val,
                                MAX(${caseStr}) AS max_val
                            FROM sample
                            WHERE ${subClauseStr}
                        )
                        SELECT
                            REGEXP_REPLACE(REGEXP_REPLACE(CONCAT(MIN(${caseStr}),' - ',MAX(${caseStr})),'^ - $','None'),'NaN - NaN','Below Detection Limit') AS label,
                            WIDTH_BUCKET(NULLIF(${caseStr},'NaN'),min_val,max_val,10) AS bucket,COUNT(*)::int
                        ${tableStr}, min_max
                        ${clauseStr}
                        GROUP BY bucket
                        ORDER BY bucket) AS foo`;
            }
//TODO
//            else if (term.type == 'datetime') {
//                let cases = [].concat.apply([], Object.values(term.schemas)).map(schema => "WHEN schema_id=" + schema.schemaId + " THEN datetime_vals[" + schema.position + "]");
//                let caseStr = "CASE " + cases.join(" ") + " END";
//                queryStr = "SELECT COALESCE(" + caseStr + ") AS val,count(*)::int " + tableStr + clauseStr + " GROUP BY val ORDER BY val ";
//            }
            summaryQueryStrs.push(queryStr);
        }

        // Build sample query
        let selections2 = termOrder.map(tid => selections[tid]).filter(s => typeof s != "undefined");
        if (gisSelect)
            selections2.unshift(gisSelect);

        let sortDir = (typeof sort !== 'undefined' && sort > 0 ? "ASC" : "DESC");
        let sortStr = (typeof sort !== 'undefined' ? " ORDER BY " + (Math.abs(sort) + 3) + " " + sortDir : "");

        let sampleQueryStr =
            "SELECT " + ["schema_id", "s.sample_id", "p.project_id", "p.name", "s.accn"].concat(selections2).join(",") + " " +
            tableStr +
            clauseStr + groupByStr + sortStr + offsetStr + limitStr;

        // Execute queries and format results //TODO make queries run in parallel
        console.log("Count Query:");
        count = await query({
            text: countQueryStr,
            rowMode: 'array'
        });
        count = count.rows[0][0]*1;

        console.log("Summary Queries:");
        summaries = await Promise.all(summaryQueryStrs.map(s =>
            query({
                text: s,
                rowMode: 'array'
            })
        ));
        summaries = summaries.map(res => res.rows || []);
        for (summary of summaries) {
            for (row of summary) { //TODO move into function (dup'ed elsewhere)
                if (row[0].startsWith("http://")) { // is this a purl?
                    let term2 = getTerm(row[0]);
                    if (term2 && term2.label)
                        row[0] = term2.label;
                }
            }
        }

        console.log("Sample Query:");
        results = await query({
            text: sampleQueryStr,
            rowMode: 'array'
        });

        results = results.rows.map(r => {
            return {
                schemaId: r[0],
                sampleId: r[1],
                projectId: r[2],
                projectName: r[3],
                sampleAccn: r[4],
                values: r.slice(5).map(val => { //TODO move into function
                    if (typeof val == "undefined")
                        return ""; // kludge to convert null to empty string
                    else if (Array.isArray(val)) {
                        return val.map(v => {
                            if (typeof v == "number" && isNaN(v))
                                return "Below Detection Limit" // kludge to convert NaN to "Below Detection Limit"
                            else if (typeof v == "string" && v.startsWith("http://")) { // is this a purl? //TODO move into funtion (dup'ed elsewhere)
                                let term = getTerm(v);
                                if (!term)
                                    return v;
                                else
                                    return term.label;
                            }
                            else
                                return v;
                        });
                    }
                    else
                        return val;
                })
            }
        });

        // Build and execute location query (for map)
        let locationClusterQuery =
            `SELECT
                s.sample_id,s.accn as sample_accn,p.name AS project_name,
                ST_X(ST_GeometryN(locations::geometry, 1)) AS longitude,
                ST_Y(ST_GeometryN(locations::geometry, 1)) AS latitude
            ${tableStr}
            ${clauseStr}
            ${groupByStr}`;

        //if (map)
            clusters = await query(locationClusterQuery);
    }
// Temporarily removed until Tara SRA files are finished loading
//    else if (result == "file") {
//        let sortDir = (typeof sort !== 'undefined' && sort > 0 ? "ASC" : "DESC");
//        let sortStr = (typeof sort !== 'undefined' ? " ORDER BY " + (Math.abs(sort)+1) + " " + sortDir : "");
//
//        let fileClause = " AND f.file_id IS NOT NULL ";
//        let groupByStr = " GROUP BY f.file_id,ff.file_format_id,ft.file_type_id,s.sample_id,p.project_id ";
//
//        let countQueryStr =
//            "SELECT COUNT(foo) FROM (SELECT f.file_id " +
//            tableStr +
//            clauseStr + fileClause + groupByStr + ") AS foo";
//
//        let queryStr =
//            "SELECT f.file_id,p.name,s.accn,ff.name,ft.name,f.url,s.sample_id,p.project_id " + // FIXME order of fields should match sample query above in order for sorting to work
//            tableStr +
//            clauseStr + fileClause + groupByStr + sortStr + offsetStr + limitStr;
//
//        count = await query({
//            text: countQueryStr,
//            values: [],
//            rowMode: 'array'
//        });
//        count = count.rows[0][0]*1;
//
//        results = await query({
//            text: queryStr,
//            values: [],
//            rowMode: 'array'
//        });
//
//        results = results.rows.map(r => {
//            return {
//                fileId: r[0],
//                sampleId: r[6],
//                projectId: r[7],
//                projectName: r[1],
//                sampleAccn: r[2],
//                fileFormat: r[3],
//                fileType: r[4],
//                fileUrl: r[5]
//            }
//        })
//    }
    else {
        console.log("Error: invalid result specifier:", result);
    }

    return {
        count: count,
        summary: summaries,
        results: results,
        map: (clusters ? clusters.rows : {})
    };
}

function getTerm(nameOrId) {
    if (nameOrId) {
        if (nameOrId in rdfTermIndex)
            return rdfTermIndex[nameOrId];

        nameOrId = nameOrId.toLowerCase();
        for (term of Object.values(rdfTermIndex)) {
            let id = term.id ? term.id.toLowerCase() : '';
            let label = term.label ? term.label.toLowerCase() : '';

            if (nameOrId === id || id.endsWith(nameOrId) || nameOrId === label || label.includes(nameOrId))
                return term;
        }
    }

    return null;
}

async function generateTermIndex(db, ontologyDescriptors) {
    //let result = await query('select fields[1].string from sample limit 1')
    let schemas = await query("SELECT schema_id,name,fields FROM schema");
    //console.log(schemas.rows);

    let index = {};
    ontologyDescriptors.forEach( desc => {
        console.log("Indexing ontology", desc.name);
        loadOntology(desc.type, desc.path, index);
    });

    schemas.rows.forEach( schema => {
        console.log("Indexing schema", schema.name);

        for (let i = 0; i < schema.fields.fields.length; i++) {
            let field = schema.fields.fields[i];

            let purl = field.rdfType;
            let unitPurl = field['pm:unitRdfType'];
            if (!purl || ('pm:searchable' in field && !field['pm:searchable']))
                continue; // skip this field if no PURL or not searchable

            if (!(purl in index)) {
                console.log("WARNING: missing", purl, "in index (not defined in an ontology)");
                index[purl] = {};
            }
            else { // Check type consistency
                if (index[purl].type && index[purl].type != field.type)
                    console.log("WARNING: type mismatch for", purl, index[purl].type, field.type);
                if (index[purl].unitId && index[purl].unitId != unitPurl)
                    console.log("WARNING: unit mismatch for", purl, index[purl].unitId, unitPurl);
            }

            if (!('schemas' in index[purl]))
                index[purl]['schemas'] = {};
            if (!(schema.schema_id in index[purl]['schemas']))
                index[purl]['schemas'][schema.schema_id] = []

            index[purl]['schemas'][schema.schema_id].push({
                schemaId: schema.schema_id,
                position: i + 1,
                //type: field.type,
                name: field.name,
                sourceName: field.name, //FIXME
                sourceUrl: field['pm:sourceUrl'] || ''
            });

            index[purl]['type'] = field.type;

            if (unitPurl && unitPurl in index) {
                index[purl]['unitId'] = unitPurl;
                index[purl]['unitLabel'] = index[unitPurl]['label'];
            }
        }
    });

    return index;
}

function loadOntology(type, path, index) {
    if (path.endsWith(".json")) {
        ontology = JSON.parse(fs.readFileSync(path));

        if (type == "term") { // pmo.json
            ontology.graphs.forEach( g => {
                g.nodes.forEach(node => {
                    let annotations = [];
                    if (node.meta && node.meta.basicPropertyValues)
                        annotations = node.meta.basicPropertyValues.map(prop => { return { id: prop.pred, value: prop.val }; });

                    if (!(node.id in index))
                        index[node.id] = {};
                    index[node.id] = Object.assign(index[node.id],
                        {
                            id: node.id,
                            label: node.lbl,
                            definition: (node.meta && node.meta.definition ? node.meta.definition.val : null),
                            annotations: annotations
                        }
                    );
                });
            });
        }
// Removed 11/27/19 -- no longer needed, pmo.json has all term definitions
//        else if (type == "term_owl") { // pmo_owl.json
//            ontology.classAttribute.forEach(node => {
//                let label = "<unknown>";
//                if (node["label"]) {
//                    if (node["label"]["en"])
//                        label = node["label"]["en"];
//                    else if (node["label"]["undefined"])
//                        label = node["label"]["undefined"];
//                }
//
//                if (!(node.iri in index))
//                    index[node.iri] = {};
//                index[node.iri] = Object.assign(index[node.iri],
//                    {
//                        id: node.iri,
//                        label: label,
//                    }
//                );
//            });
//        }
        else {
            throw("Error: unsupported ontology type");
        }
    }
    else if (path.endsWith(".csv")) {
        var data = fs.readFileSync(path, { encoding: "UTF8" });
        data.split('\n').forEach(line => {
            let fields = line.split(',');
            let purl = fields[0];
            let label = fields[1];
            index[purl] = {
                id: purl,
                label : label
            }
        })
    }
    else {
        throw("Error: unsupported ontology file format");
    }

    return index;
}

//async function generateSchemaIndex() {
//    let results = await query("SELECT schema_id,name,fields FROM schema");
//    let schemas = {};
//
//    results.rows.forEach(row => {
//        let id = row.schema_id;
//        schemas[id] = {};
//        for (f of row.fields.fields) {
//            let purl = f['rdfType'];
//            if (!purl) continue;
//
//            if (!(purl in schemas[id]))
//                schemas[id][purl] = []
//            schemas[id][purl].push(f);
//        }
//    });
//
//    return schemas;
//}

function getSchemasForTerms(terms) {
    let schemas = [];
    for (term of terms) {
        if (!term.schemas || term.schemas.length == 0)
            continue;

        if (schemas.length == 0)
            schemas = Object.keys(term.schemas);
        else
            schemas = intersect(schemas, Object.keys(term.schemas));
    }
    return schemas;
}

function intersect(a, b) {
    var t;
    if (b.length > a.length) t = b, b = a, a = t; // indexOf to loop over shorter
    return a.filter(function (e) {
        return b.indexOf(e) > -1;
    });
}

function buildTermSQL(arrIndex, val) {
    // FIXME should use query substitution here -- SQL injection risk
    let field, clause, bounds;
    if (val === '') { // empty - don't query just show in results
        console.log("empty val match");
        if (term.type == "string")
            field = "string_vals[" + arrIndex + "]";
        else if (term.type == "number")
            field = "number_vals[" + arrIndex + "]";
        else if (term.type == "datetime")
            field = "datetime_vals[" + arrIndex + "]";
    }
    else if (!isNaN(val)) { // numeric exact match
        if (term.type == "number") {
            console.log("numeric exact match");
            field = "number_vals[" + arrIndex + "]";
            clause = field + "=" + parseFloat(val);
        }
        else {
            //TODO error
            console.log("Error: numeric exact query not supported for type", term.type);
        }
    }
    else if (val.match(/^\[-?\d*(\.\d+)?\,-?\d*(\.\d+)?\]/)) { // numeric range query
        if (term.type == "number") {
            console.log("numeric range query");
            bounds = val.substr(1, val.length-2).split(',');
            field = "number_vals[" + arrIndex + "]";
            if (present(bounds[0]) && present(bounds[1]))
                clause = field + " BETWEEN " + bounds[0] + " AND " + bounds[1];
            else if (present(bounds[0]))
                clause = field + " >= " + bounds[0];
            else if (present(bounds[1]))
                clause = field + " <= " + bounds[1];
            else //TODO error
                console.log("Error: numeric range query with no bounds");
        }
        else {
            //TODO error
            console.log("Error: numeric range query not supported for type", term.type);
        }
    }
    else if (val.match(/^-?\d*(\.\d+)?\,-?\d*(\.\d+)?$/)) { // numeric offset query
        if (term.type == "number") {
            console.log("numeric offset query");
            bounds = val.split(",");
            field = "number_vals[" + arrIndex + "]";
            clause = field + " BETWEEN " + (1*bounds[0] - 1*bounds[1]) + " AND " + (1*bounds[0] + 1*bounds[1]);
        }
        else {
            //TODO error
            console.log("Error: numeric offset query not supported for type", term.type);
        }
    }
    else if (val.match(/^\~(\w+)(\|\w+)*/)) { // partial string match on one or more values (case-insensitive)
        if (term.type == "string") {
            let vals = val.substr(1).split("|");
            console.log("partial string match on multiple values", vals);
            field = "string_vals[" + arrIndex + "]";
            if (vals.length == 1)
                clause = "LOWER(" + field + ") LIKE " + "'%" + vals[0].toLowerCase() + "%'"; // may not be necessary if equivalent to SIMILAR TO
            else
                clause = "LOWER(" + field + ") SIMILAR TO " + "'%(" + vals.join("|").toLowerCase() + ")%'";
        }
        else {
            //TODO error
            console.log("Error: string similarity query not supported for type", term.type);
        }
    }
    else if (val.match(/^(\d{4}\-\d{2}\-\d{2})/)) { // date/time exact match
        if (term.type == "datetime" || term.type == "date") {
            console.log("exact datetime match");
            field = "CAST (datetime_vals[" + arrIndex + "] as DATE)";
            clause = field + "='" + val + "'";
        }
        else {
            //TODO error
            console.log("Error: datetime exact query not supported for type", term.type);
        }
    }
    else if (val.match(/^(\w+)(\|\w+)*/)) { // literal string match on one or more values (case-insensitive)
        if (term.type == "string") {
            let vals = val.split("|");
            console.log("literal string match", vals);
            field = "string_vals[" + arrIndex + "]";
            clause = field + "=" + "'" + val + "'";
            if (vals.length == 1)
                clause = "LOWER(" + field + ") =" + "'" + val.toLowerCase() + "'"; // may not be necessary if equivalent to IN()
            else
                clause = "LOWER(" + field + ") IN (" + vals.map(s => "'" + s + "'").join(",").toLowerCase() + ")";
        }
        else {
            //TODO error
            console.log("Error: string literal query not supported for type", term.type);
        }
    }
    else if (bounds = val.match(/^\[(\d{4}\-\d{2}\-\d{2})?\,(\d{4}\-\d{2}\-\d{2})?\]$/)) { // date/time range query
        if (term.type == "datetime" || term.type == "date") {
            console.log("datetime range query:", bounds);
            field = "datetime_vals[" + arrIndex + "]";
            clause = (bounds[1] ? field + ">=timestamp'" + bounds[1] + "'" : '') +
                (bounds[1] && bounds[2] ? " AND " : '') +
                (bounds[2] ? field + "<=timestamp'" + bounds[2] + "'" : '');
        }
        else {
            //TODO error
            console.log("Error: datetime range query not supported for type", term.type);
        }
    }
    else {
        console.log("Error: invalid query syntax");
        throw("Invalid query syntax")
    }

    return [field, clause];
}

function query(queryStrOrObj, params) {
    console.log(queryStrOrObj, params || "");
    return db.query(queryStrOrObj, params);
}

async function batchQuery(queryObjArray) {
  const promises = queryObjArray.map(obj => query(obj));
  return await Promise.all(promises);
}

function defined(val) {
    return (typeof val !== "undefined");
}

function present(val) {
    return defined(val) && val != "";
}
