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
const config = require('./config.json');

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
    //console.log("index:", JSON.stringify(rdfTermIndex, null, 4));

    app.listen(config.serverPort, () => console.log('Server listening on port', config.serverPort));
})();

//----------------------------------------------------------------------------------------------------------------------

app.use(logger('dev'));
app.use(cors());
app.use(bodyParser.json()); // support json encoded bodies

app.get('/index', (req, res) => { //TODO rename to "catalog", as in a catalog of terms
    res.json(rdfTermIndex);
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
                let aliases = Array.from(new Set(Object.values(term.schemas).reduce((acc, schema) => acc.concat(Object.keys(schema)), [])));
                return {
                    id: term.id,
                    label: term.label,
                    unitLabel: term.unitLabel,
                    type: term.type,
                    aliases: aliases
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
    if (term.schemas)
        aliases = Array.from(new Set(Object.values(term.schemas).reduce((acc, schema) => acc.concat(Object.keys(schema)), [])));

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
        let uniqueVals = {};
        for (let schemaId in term.schemas) {
            console.log("schemaId:", schemaId);
            for (let alias in term.schemas[schemaId]) {
                let arrIndex = term.schemas[schemaId][alias];
                let vals = await query({ text: "SELECT string_vals[$1],count(string_vals[$1]) FROM sample WHERE schema_id=$2 GROUP BY string_vals[$1]", values: [arrIndex,schemaId*1], rowMode: 'array'});
                vals.rows.forEach(row => {
                    let val = row[0];
                    if (!uniqueVals[val])
                        uniqueVals[val] = 0;
                    uniqueVals[val] += 1*row[1];
                });
            }
        }

        res.json({
            id: term.id,
            label: term.label,
            unitLabel: term.unitLabel,
            type: term.type,
            aliases: aliases,
            values: uniqueVals,
            annotations: annotations
        });
// TODO
//        })
//        .catch(err => {
//            console.log(err);
//            res.send(err);
//        });
    }
    else if (term.type == "number") { // numeric
        let queries = [];
        for (let schemaId in term.schemas) {
            for (let alias in term.schemas[schemaId]) {
                let arrIndex = term.schemas[schemaId][alias];
                queries.push({ text: "SELECT MIN(number_vals[$1]),MAX(number_vals[$1]) FROM sample WHERE schema_id=$2", values: [arrIndex,schemaId*1], rowMode: 'array' });
            }
        }

        let min, max;
        let results = await batchQuery(queries); // FIXME would a single query be faster?
        results.forEach(vals => {
            vals.rows.forEach(row => {
                if (typeof row[0] !== "undefined" && typeof row[1] !== "undefined") {
                    min = (typeof min === "undefined" ? row[0] : Math.min(min, row[0]));
                    max = (typeof max === "undefined" ? row[1] : Math.max(max, row[1]));
                }
            });
        });

        res.json({
            id: term.id,
            label: term.label,
            unitLabel: term.unitLabel,
            type: term.type,
            aliases: aliases,
            min: min,
            max: max,
            annotations: annotations
        });
// TODO
//        .catch(err => {
//            console.log(err);
//            res.send(err);
//        });
    }
    else if (term.type == "datetime") {
        let queries = [];
        for (let schemaId in term.schemas) {
            for (let alias in term.schemas[schemaId]) {
                let arrIndex = term.schemas[schemaId][alias];
                queries.push({ text: "SELECT MIN(datetime_vals[$1]),MAX(datetime_vals[$1]) FROM sample WHERE schema_id=$2", values: [arrIndex,schemaId*1], rowMode: 'array' });
            }
        }

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
    let results = await search(db, req.query);
    res.json(results);
//    .catch(err => {
//        console.log(err);
//        res.send(err);
//    });
});

app.get('/projects', async (req, res) => {
    let result = await query({
        text: "SELECT p.project_id,p.name,p.accn,p.description,pt.name AS type, \
                (SELECT count(*) FROM project_to_sample pts WHERE pts.project_id=p.project_id) AS sample_count \
            FROM project p \
            JOIN project_type pt ON p.project_type_id=pt.project_type_id"
    });
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
        text: "SELECT p.project_id,p.name,p.accn,p.description,pt.name AS type,(SELECT count(*) FROM project_to_sample pts WHERE pts.project_id=p.project_id) AS sample_count FROM project p JOIN project_type pt ON p.project_type_id=pt.project_type_id WHERE p.project_id=$1",
        values: [id]
    });
    result.rows.forEach(row => row.sample_count *= 1); // convert count to int
    res.json(result.rows[0]);
});

app.get('/projects/:id(\\d+)/campaigns', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text: "SELECT c.campaign_id,c.campaign_type,c.name,c.description,c.deployment,c.start_location,c.end_location,c.start_time,c.end_time,c.urls \
            FROM project_to_sample pts \
            JOIN sample s ON s.sample_id=pts.sample_id \
            JOIN sample_to_sampling_event stse ON stse.sample_id=s.sample_id \
            JOIN sampling_event se ON se.sampling_event_id=stse.sampling_event_id \
            JOIN campaign c ON c.campaign_id=se.campaign_id \
            WHERE pts.project_id=$1 \
            GROUP BY c.campaign_id",
        values: [id]
    });
    res.json(result.rows);
});

app.get('/projects/:id(\\d+)/sampling_events', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text: "SELECT se.sampling_event_id,se.sampling_event_type,se.name,ST_AsGeoJson(se.locations)::json->'coordinates' AS locations,se.start_time,se.end_time \
            FROM project_to_sample pts \
            JOIN sample s ON s.sample_id=pts.sample_id \
            JOIN sample_to_sampling_event stse ON stse.sample_id=s.sample_id \
            JOIN sampling_event se ON se.sampling_event_id=stse.sampling_event_id \
            WHERE pts.project_id=$1 \
            GROUP BY se.sampling_event_id",
        values: [id]
    });

    res.json(result.rows);
});

app.get('/projects/:id(\\d+)/samples', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text: "SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations \
            FROM sample s \
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id \
            WHERE pts.project_id=$1",
        values: [id]
    });

    res.json(result.rows);
});

app.get('/samples', async (req, res) => {
    let result = await query({
        text: "SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations,p.project_id,p.name AS project_name \
            FROM sample s \
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id \
            JOIN project p ON p.project_id=pts.project_id"
    });

    res.json(result.rows);
});

app.get('/samples/:id(\\d+)', async (req, res) => {
    let id = req.params.id;

    let result = await query({
        text: "SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations,p.project_id,p.name AS project_name \
            FROM sample s \
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id \
            JOIN project p ON p.project_id=pts.project_id \
            WHERE s.sample_id=$1",
        values: [id]
    });

    res.json(result.rows[0]);
});

app.get('/samples/:id(\\d+)/sampling_events', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text: "SELECT se.sampling_event_id,se.sampling_event_type,se.name,c.campaign_id,c.campaign_type,c.name AS campaign_name \
            FROM sample s \
            JOIN sample_to_sampling_event stse ON stse.sample_id=s.sample_id \
            JOIN sampling_event se ON se.sampling_event_id=stse.sampling_event_id \
            LEFT JOIN campaign c ON c.campaign_id=se.campaign_id \
            WHERE s.sample_id=$1",
        values: [id]
    });

    res.json(result.rows);
});

app.get('/samples/:id(\\d+)/metadata', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text: "SELECT s.schema_id,schema.fields->'fields' AS fields,s.number_vals,s.string_vals,s.datetime_vals \
            FROM sample s \
            JOIN schema ON schema.schema_id=s.schema_id \
            WHERE s.sample_id=$1",
        values: [id]
    });

    let row = result.rows[0];
    let terms = [];
    let values = [];
    for (i = 0;  i < row.fields.length;  i++) {
        let field = row.fields[i];
        let val = "";
        if (field.type == "number")
            val = row.number_vals[i];
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
        fields: row.fields,
        values: values
    });
});

app.get('/campaigns/:id(\\d+)', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text: "SELECT c.campaign_id,c.campaign_type,c.name,c.description,c.deployment,c.start_location,c.end_location,c.start_time,c.end_time,c.urls,p.project_id,p.name AS project_name \
            FROM campaign c \
            JOIN sampling_event se ON se.campaign_id=c.campaign_id \
            JOIN sample_to_sampling_event stse ON stse.sampling_event_id=se.sampling_event_id \
            JOIN sample s ON s.sample_id=stse.sample_id \
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id \
            JOIN project p ON p.project_id=pts.project_id \
            WHERE c.campaign_id=$1",
        values: [id]
    });
    res.json(result.rows[0]);
});

app.get('/campaigns/:id(\\d+)/sampling_events', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text: "SELECT se.sampling_event_id,se.sampling_event_type,se.name,ST_AsGeoJson(se.locations)::json->'coordinates' AS locations,se.start_time,se.end_time \
            FROM sampling_event se \
            JOIN campaign c ON c.campaign_id=se.campaign_id \
            WHERE c.campaign_id=$1",
        values: [id]
    });

    res.json(result.rows);
});

app.get('/campaigns/:id(\\d+)/samples', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text: "SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations \
            FROM sample s \
            JOIN sample_to_sampling_event stse ON stse.sample_id=s.sample_id \
            JOIN sampling_event se ON se.sampling_event_id=stse.sampling_event_id \
            WHERE se.campaign_id=$1 \
            GROUP BY s.sample_id",
        values: [id]
    });

    res.json(result.rows);
});

app.get('/sampling_events/:id(\\d+)', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text: "SELECT se.sampling_event_id,se.sampling_event_type,se.name,ST_AsGeoJson(se.locations)::json->'coordinates' AS locations,se.start_time,se.end_time,c.campaign_id,c.campaign_type,c.name AS campaign_name,p.project_id,p.name AS project_name \
            FROM sampling_event se \
            LEFT JOIN campaign c ON c.campaign_id=se.campaign_id \
            JOIN sample_to_sampling_event stse ON stse.sampling_event_id=se.sampling_event_id \
            JOIN sample s ON s.sample_id=stse.sample_id \
            JOIN project_to_sample pts ON pts.sample_id=s.sample_id \
            JOIN project p ON p.project_id=pts.project_id \
            WHERE se.sampling_event_id=$1",
        values: [id]
    });

    res.json(result.rows[0]);
});

app.get('/sampling_events/:id(\\d+)/samples', async (req, res) => {
    let id = req.params.id;
    let result = await query({
        text: "SELECT s.sample_id,s.accn,ST_AsGeoJson(s.locations)::json->'coordinates' AS locations \
            FROM sample s \
            JOIN sample_to_sampling_event stse ON stse.sample_id=s.sample_id \
            WHERE stse.sampling_event_id=$1",
        values: [id]
    });

    res.json(result.rows);
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

//----------------------------------------------------------------------------------------------------------------------

async function generateTermIndex(db, ontologyDescriptors) {
    //let result = await query('select fields[1].string from sample limit 1')
    let schemas = await query("SELECT schema_id,name,fields FROM schema");
    //console.log(schemas.rows);

    let index = {};
    ontologyDescriptors.forEach( desc => {
        console.log("Indexing ontology", desc.name);
        load_ontology(desc.type, desc.path, index);
    });

    schemas.rows.forEach( schema => {
        console.log("Indexing schema", schema.name);

        for (let i = 0; i < schema.fields.fields.length; i++) {
            let field = schema.fields.fields[i];

            let purl = field.rdfType;
            let unitPurl = field['pm:unitRdfType'];
            if (!purl || !field["pm:searchable"])
                continue; // skip this field if no PURL value (not in term catalog)

            if (!(purl in index))
                index[purl] = {};
            else { // Check type consistency
                if (index[purl].type && index[purl].type != field.type)
                    console.log("WARNING: type mismatch for", purl, index[purl].type, field.type);
                if (index[purl].unitId && index[purl].unitId != unitPurl)
                    console.log("WARNING: unit mismatch for", purl, index[purl].unitId, unitPurl);
            }

            if (!('schemas' in index[purl]))
                index[purl]['schemas'] = {};
            if (!index[purl]['schemas'][schema.schema_id])
                index[purl]['schemas'][schema.schema_id] = {};

            index[purl]['schemas'][schema.schema_id][field.name] = i+1
            index[purl]['type'] = field.type;

            if (unitPurl && unitPurl in index) {
                index[purl]['unitId'] = unitPurl;
                index[purl]['unitLabel'] = index[unitPurl]['label'];
            }
        }
    });

    return index;
}

function load_ontology(type, path, index) {
    if (path.endsWith(".json")) {
        ontology = JSON.parse(fs.readFileSync(path));

        if (type == "term") {
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
                            annotations: annotations
                        }
                    );
                });
            });
        }
        else if (type == "term_owl") {
            ontology.classAttribute.forEach(node => {
                let label = "<unknown>";
                if (node["label"]) {
                    if (node["label"]["en"])
                        label = node["label"]["en"];
                    else if (node["label"]["undefined"])
                        label = node["label"]["undefined"];
                }

                if (!(node.iri in index))
                    index[node.iri] = {};
                index[node.iri] = Object.assign(index[node.iri],
                    {
                        id: node.iri,
                        label: label,
                    }
                );
            });
        }
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

async function search(db, params) {
    console.log("params:", params);

    let limit = 20, offset, sort, map;
    let gisClause, projectClause;
    let clauses = {};
    let selections = [];

    for (param in params) {
        param = param.replace(/\+/gi, ''); // work around for Elm uri encoding
        let val = params[param];

        if (param == 'limit')
            limit = val * 1; // convert to int
        else if (param == 'offset')
            offset = val * 1; // convert to int
        else if (param == 'sort')
            sort = val * 1; // convert to int
        else if (param == 'map')
            map = val == 1;
        else if (param == 'location') {
//            if (val.match(/\[-?\d*(\.\d+)?\,-?\d*(\.\d+)?\]/)) { // [lat, lng] exact match
//                //TODO
//            }
            if (val.match(/\[-?\d*(\.\d+)?\,-?\d*(\.\d+)?,-?\d*(\.\d+)?\]/)) { // [lat, lng, radius] in meters
                let bounds = JSON.parse(val);
                console.log("location:", bounds);
                selections.push("replace(replace(replace(replace(ST_AsGeoJson(ST_FlipCoordinates(locations::geometry))::json->>'coordinates', '[[', '['), ']]', ']'), '[', '('), ']', ')')"); // ugly af
                gisClause = "ST_DWithin(ST_MakePoint(" + bounds[1] + "," + bounds[0] + ")::geography, locations, " + bounds[2] + ")";
            }
        }
        else if (param == 'project') {
            let vals = val.split("|");
            console.log("project match", vals);
            projectClause = "LOWER(project.name) IN (" + vals.map(s => "'" + s + "'").join(",").toLowerCase() + ")";
        }
        else {
            let term = getTerm(param);
            if (!term) {
                console.log("Error: term not found for param '" + param + "'");
                return;
            }
            console.log("term:", term);

            let selectStr = "";

            for (schemaId in term.schemas) {
                for (alias in term.schemas[schemaId]) {
                    let arrIndex = term.schemas[schemaId][alias];

                    // FIXME should use query substitution here -- SQL injection risk
                    let field, clause, bounds;
                    if (val === '') { // empty - don't query just show in results
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
                    else if (val.match(/^\~(\w+)(\|\w+)*/)) { // partial string match on one or more values
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
                    else if (val.match(/^(\w+)(\|\w+)*/)) { // literal string match on one or more values
                        if (term.type == "string") {
                            let vals = val.split("|");
                            console.log("literal string match", vals);
                            field = "string_vals[" + arrIndex + "]";
                            clause = field + "=" + "'" + val + "'";
                            if (vals.length == 1)
                                clause = field + "=" + "'" + val + "'"; // may not be necessary if equivalent to IN()
                            else
                                clause = "LOWER(" + field + ") IN (" + vals.map(s => "'" + s + "'").join(",").toLowerCase() + ")";
                        }
                        else {
                            //TODO error
                            console.log("Error: string literal query not supported for type", term.type);
                        }
                    }
                    else if (bounds = val.match(/^\[(\d{4}\-\d{2}\-\d{2})\,(\d{4}\-\d{2}\-\d{2})\]$/)) { // date/time range query
                        if (term.type == "datetime" || term.type == "date") {
                            console.log("datetime range query");
                            field = "datetime_vals[" + arrIndex + "]";
                            clause = field + ">=timestamp'" + bounds[1] + "' AND " + field + "<=timestamp'" + bounds[2] + "'";
                        }
                        else {
                            //TODO error
                            console.log("Error: datetime range query not supported for type", term.type);
                        }
                    }
                    else if (val.match(/^(\d{4}\-\d{2}\-\d{2})/)) { // date/time exact match
                        if (term.type == "datetime" || term.type == "date") {
                            console.log("exact datetime match");
                            field = "datetime_vals[" + arrIndex + "]";
                            clause = field + "=timestamp'" + val + "'";
                        }
                        else {
                            //TODO error
                            console.log("Error: datetime exact query not supported for type", term.type);
                        }
                    }
                    else {
                        console.log("Error: invalid query syntax");
                        throw("Invalid query syntax")
                    }

                    selectStr += " WHEN schema_id=" + schemaId + " THEN " + field;
                    if (clause) {
                        if (!clauses[schemaId])
                            clauses[schemaId] = []
                        clauses[schemaId].push(clause);
                    }
                }
            }

            if (selectStr)
                selections.push("CASE" + selectStr + " END");
        }
    }
    console.log("selections:", selections);
    console.log("clauses:", clauses);

    let clauseStr =
        Object.keys(clauses).map(schemaId =>
            "(schema_id=" + schemaId + " AND " + Object.values(clauses[schemaId]).join(" AND ") + ")"
        )
        .join(" OR ");

    if (gisClause)
        clauseStr = gisClause + (clauseStr ? " AND (" + clauseStr + ")" : "");

    if (projectClause)
        clauseStr = projectClause + (clauseStr ? " AND (" + clauseStr + ")": "");

    if (clauseStr)
        clauseStr = "WHERE " + clauseStr;

    let sortDir = (typeof sort !== 'undefined' && sort > 0 ? "ASC" : "DESC");
    let sortStr = (typeof sort !== 'undefined' && Math.abs(sort) <= selections.length+1 ? " ORDER BY " + (Math.abs(sort)+3) + " " + sortDir : "");

    let countQueryStr = "SELECT count(*) FROM sample JOIN project_to_sample ON project_to_sample.sample_id=sample.sample_id JOIN project ON project.project_id=project_to_sample.project_id " + clauseStr;

    if (!limit)
        limit = 50;
    let limitStr = (limit ? " LIMIT " + limit : "");
    let offsetStr = (offset ? " OFFSET " + offset : "");

    let queryStr = "SELECT " + ["schema_id", "sample.sample_id", "project.project_id", "project.name"].concat(selections).join(",") +
        " FROM sample JOIN project_to_sample ON project_to_sample.sample_id=sample.sample_id JOIN project ON project.project_id=project_to_sample.project_id " +
        clauseStr + sortStr + offsetStr + limitStr;

//    let locationClusterQuery = "SELECT ST_NumGeometries(gc) AS count, ST_AsGeoJSON(gc) AS collection, ST_AsGeoJSON(ST_Centroid(gc)) AS centroid, ST_AsGeoJSON(ST_MinimumBoundingCircle(gc)) AS circle, sqrt(ST_Area(ST_MinimumBoundingCircle(gc)) / pi()) AS radius " +
//        "FROM (SELECT unnest(ST_ClusterWithin(locations::geometry, 100)) gc FROM sample " + clauseStr + ") f;"

//    let locationClusterQuery = "SELECT ST_AsGeoJSON(ST_Union(ST_GeometryN(locations::geometry, 1))) AS points " +
//        "FROM sample JOIN project_to_sample ON project_to_sample.sample_id=sample.sample_id JOIN project ON project.project_id=project_to_sample.project_id " +
//        clauseStr;

    let locationClusterQuery = "SELECT sample.sample_id,sample.accn as sample_accn,project.name AS project_name,ST_X(ST_GeometryN(locations::geometry, 1)) AS longitude, ST_Y(ST_GeometryN(locations::geometry, 1)) AS latitude " +
        "FROM sample JOIN project_to_sample ON project_to_sample.sample_id=sample.sample_id JOIN project ON project.project_id=project_to_sample.project_id " +
        clauseStr;

    let count = await query({
        text: countQueryStr,
        values: [],
        rowMode: 'array'
    });

    let results = await query({
        text: queryStr,
        values: [],
        rowMode: 'array'
    });

    let clusters;
    //if (map)
        clusters = await query(locationClusterQuery);

    return {
        count: count.rows[0][0]*1,
        results: results.rows.map(r => {
            return {
                schemaId: r[0],
                sampleId: r[1],
                projectId: r[2],
                projectName: r[3],
                values: r.slice(4).map(v => typeof v == "undefined" ? "" : v ) // kludge to convert null to empty string
            }
        }),
        map: (clusters ? clusters.rows : {})
    };
}

function getTerm(nameOrId) {
    for (term of Object.values(rdfTermIndex)) {
        if ((term.id && (nameOrId === term.id || term.id.endsWith(nameOrId))) || (term.label && (nameOrId === term.label || term.label.includes(nameOrId)))) {
            return term;
        }
    }

    return null;
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
