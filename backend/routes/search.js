'use strict';

const stringSimilarity = require('string-similarity');
const Promise = require('promise');
const express = require('express');
const router  = express.Router();
const config = require('../config.json');
const db = require('../postgres.js')(config);

router.get('/index', (req, res) => { //TODO rename to "catalog", as in a catalog of terms
    let termIndex = req.app.get('termIndex');
    res.json(termIndex);
});

router.get('/schema', async (req, res) => {
    let fields = await db.query("SELECT schema_id,name,type,fields->'fields' AS fields FROM schema");
    res.json(fields.rows);
});

router.get('/searchTerms', (req, res) => {
    let query = req.query.query;
    let termIndex = req.app.get('termIndex');

    if (query) {
        const MIN_MATCH_RATING = 0.1;
        let labels = Object.values(termIndex).map(term => term.label).filter(label => label);
        let matches = stringSimilarity.findBestMatch(query, labels);
        let bestLabels = matches.ratings.filter(m => m.rating > MIN_MATCH_RATING).map(m => m.target);
        let terms = Object.values(termIndex).filter(term => bestLabels.includes(term.label));
        res.json(terms);
    }
    else {
        res.json(
            Object.values(termIndex.getSearchableTerms())
            .filter(term => term && term.id && term.schemas) // Ignore terms that aren't referenced in datapackages
            .map(term => {
                let aliases = [].concat.apply([], Object.values(term.schemas))
                    .map(schema => {
                        return {
                            name: schema.name,
                            sourceName: schema.sourceName,
                            sourceUrl: schema.sourceUrl
                        }
                    });

                let annotations = [];
                if (term.annotations) { // TODO move into function
                    annotations = term.annotations.map(a => {
                        let label = termIndex.getLabelForValue(a.id);
                        let value = termIndex.getLabelForValue(a.value);

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
                    unitId: term.unitId,
                    unitLabel: term.unitLabel,
                    type: term.type,
                    aliases: aliases,
                    annotations: annotations
                };
            })
        );
    }
});

router.get('/searchTerms/:ids(*)', async (req, res) => {
    let ids = decodeURIComponent(req.params.ids);

    let terms = await Promise.all(
        ids.split(",").map(id => {
            // Workaround for http:// in id changed to http:/ on MYO (NGINX?)
            if (id.startsWith('http:/'))
                id = id.replace(/^http:\//, '');

            return getSearchTerm(id, req.app.get('termIndex'));
        })
    );
    res.json(terms);
});

async function getSearchTerm(id, termIndex) {
    let term = termIndex.getTerm(id);
    if (!term)
        return;
    console.log("getSearchTerm:", term);

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
            let label = termIndex.getLabelForValue(a.id);
            let value = termIndex.getLabelForValue(a.value);

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
        let result = await db.query({ text: `SELECT COALESCE(LOWER(${caseStr}),'none') AS val,COUNT(*)::int FROM sample GROUP BY val ORDER BY val`, rowMode: 'array' });

        // Convert purl values to labels
        let purlLabels = {};
        for (let row of result.rows) { //TODO move into function
            let val = row[0];
            if (val && val.startsWith("http://")) { // is this a purl?
                let term2 = termIndex.getTerm(val);
                if (term2 && term2.label) {
                    purlLabels[val] = term2.label;
                }
            }
        }

        return {
            id: term.id,
            label: term.label,
            definition: term.definition,
            unitId: term.unitId,
            unitLabel: term.unitLabel,
            type: term.type,
            distribution: result.rows,
            aliases: aliases,
            annotations: annotations,
            purlLabels: purlLabels
        };
    }
    else if (term.type == "number") {
        let cases = [].concat.apply([], Object.values(term.schemas)).map(schema => `WHEN schema_id=${schema.schemaId} THEN number_vals[${schema.position}]`);
        let caseStr = "CASE " + cases.join(" ") + " END";
        let clauses = [].concat.apply([], Object.values(term.schemas)).map(schema => `(schema_id=${schema.schemaId} AND number_vals[${schema.position}] != 'NaN')`);
        let clauseStr = clauses.join(' OR ');
        let rangeResult = await db.query({ text: `SELECT MIN(${caseStr}),MAX(${caseStr}) FROM sample WHERE ${clauseStr}`, rowMode: "array" });
        let [min, max] = rangeResult.rows[0];
        let range = max - min;
        let binLen = range/10;

        let binResult = await db.query({
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
                        WIDTH_BUCKET(NULLIF(${caseStr},'NaN'),min_val,max_val+1e-9,10) AS bucket,COUNT(*)::int
                    FROM sample, min_max
                    GROUP BY bucket
                    ORDER BY bucket) AS foo`,
                    // The max_val+1e-9 in the width_bucket() call is a kludge to prevent the error
                    // "lower bound cannot equal upper bound" when all data values are zero, as is the case for
                    // hydrogen sulfide (http://purl.obolibrary.org/obo/ENVO_3100017)
            rowMode: 'array'
        });

        return {
            id: term.id,
            label: term.label,
            definition: term.definition,
            unitId: term.unitId,
            unitLabel: term.unitLabel,
            type: term.type,
            min: min,
            max: max,
            distribution: binResult.rows,
            aliases: aliases,
            annotations: annotations
        };
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

        return {
            id: term.id,
            label: term.label,
            definition: term.definition,
            unitId: term.unitId,
            unitLabel: term.unitLabel,
            type: term.type,
            aliases: aliases,
            min: new Date(min).toISOString(),
            max: new Date(max).toISOString(),
            annotations: annotations
        };
    }
    else {
        throw("Unknown term type '" + term.type + "'");
    }
}

// Support POST and GET methods for search endpoint
// POST is easier to debug in browser debugger, GET is easier to use with curl
router.get('/search', async (req, res) => {
    handleSearchRequest(req, res, req.query);
});

router.post('/search', async (req, res) => {
    handleSearchRequest(req, res, req.body);
});

async function handleSearchRequest(req, res, params) {
    let termIndex = req.app.get('termIndex');
    let download = params.download;

    if (download) {
        req.query['limit'] = 999999;
        req.query['offset'] = 0;
    }

    try {
        let results = await search(db, termIndex, params);

        if (download) {
            let table =
                [].concat(
                    [ result.fields.join("\t") ],
                    result.results.map(r => [r.projectName, r.sampleAccn].concat(r.values).join("\t"))
                ).join("\n");

            res.send(table);
        }
        else
            res.json(results);
    }
    catch(err) {
        console.log(err);
        res.status(500).json({ error: err });
    }
}

//FIXME refactor this ugly af code
async function search(db, termIndex, params) {
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
        if (val.match(/\[\S+\]/)) { // [lat, lng, radius] in meters
            let bounds = [];
            try {
                bounds = JSON.parse(val);
            }
            catch (e) {
                console.log("Error: invalid location value", val);
                return {
                    count: 0,
                    summary: [],
                    results: [],
                    error: "Invalid location value"
                };
            }

            let lat = bounds[0];
            let lng = bounds[1];
            let radius = bounds[2] * 1000; // convert from km to m
            console.log("location:", bounds);
            gisSelect = "replace(replace(replace(replace(ST_AsGeoJson(ST_FlipCoordinates(locations::geometry))::json->>'coordinates', '[[', '['), ']]', ']'), '[', '('), ']', ')')"; //FIXME ugly af
            gisClause = `ST_DWithin(ST_MakePoint(${lng},${lat})::geography, locations, ${radius})`;
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
        projectClause = "LOWER(project.name) IN (" + vals.map(s => "'" + s + "'").join(",").toLowerCase() + ")"; //FIXME use bind param instead
    }

    let libraryClauses =
        [ 'source', 'strategy', 'selection', 'layout' ]
            .filter(field => params[field]) // make sure value is defined
            .map(field => {
                let vals = params[field].split("|");
                console.log(field, "match", vals);
                return `LOWER(library.${field}) IN (` + vals.map(s => "'" + s + "'").join(",").toLowerCase() + ")"; //FIXME use bind param instead
            });

    let termIds = Object.keys(params).filter(p => p.startsWith("http") || p.startsWith("|http"));
    let orTerms = [], andTerms = [];
    let terms = {}, termOrder = [];
    let termsById = {};
    for (let param of termIds) {
        param = param.replace(/\+/gi, ''); // workaround for Elm URI encoding
        let val = params[param];

        let orFlag = false;
        if (param.startsWith("|")) {
            param = param.replace(/^\|/, '');
            orFlag = true;
        }

        let term = termIndex.getTerm(param);
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
    }

    console.log("andTerms:", andTerms.map(t => t.id).join(","));
    console.log("orTerms:", orTerms.map(t => t.id).join(","));

    let schemas = getSchemasForTerms(andTerms);
    console.log("schemas:", schemas);

    let selections = {};
    let clauses = {};

    let andClauses = {};
    for (let term of andTerms) {
        let selectStr = "";
        for (let schemaId of schemas) {
            let fields = [];
            for (let schema of term.schemas[schemaId]) {
                if (schema.schemaType != 'sample')
                    continue;

                let [field, clause] = buildTermSQL(schema.position, term, terms[term.id]);

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

            if (fields.length > 0) {
                let fieldsStr = "ARRAY[" + fields.join(",") + "]";
                selectStr += " WHEN schema_id=" + schemaId + " THEN " + fieldsStr;
            }
        }

        if (selectStr)
            selections[term.id] = "CASE" + selectStr + " END";
    }

    let orClauses = {};
    for (let term of orTerms) {
        let selectStr = "";

        let schemas2 = [];
        if (andTerms.length == 0)
            schemas2 = Object.keys(term.schemas);
        else
            schemas2 = schemas;

        for (let schemaId of schemas2) {
            let fields = [];
            if (!(schemaId in term.schemas) || term.schemas[schemaId].length == 0) {
                console.log("!!!! Skipping", schemaId, "for", term.id);
                continue;
            }
            for (let schema of term.schemas[schemaId]) {
                if (schema.schemaType != 'sample')
                    continue;

                let [field, clause] = buildTermSQL(schema.position, term, terms[term.id]);

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

            if (fields.length > 0) {
                let fieldsStr = "ARRAY[" + fields.join(",") + "]";
                selectStr += " WHEN schema_id=" + schemaId + " THEN " + fieldsStr;
            }
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

    [gisClause, projectClause].concat(libraryClauses).forEach(clause => {
        if (clause)
            clauseStr = clause + (clauseStr ? " AND (" + clauseStr + ")" : "");
    });

    if (clauseStr)
        clauseStr = "WHERE " + clauseStr;

    // Build query
    let tableStr =
        `FROM sample
        JOIN project_to_sample ON project_to_sample.sample_id=sample.sample_id
        JOIN project ON project.project_id=project_to_sample.project_id
        LEFT JOIN experiment ON experiment.sample_id=sample.sample_id
        LEFT JOIN library ON library.experiment_id=experiment.experiment_id
        LEFT JOIN run ON run.experiment_id=experiment.experiment_id
        LEFT JOIN run_to_file ON run_to_file.run_id=run.run_id
        LEFT JOIN file ON file.file_id=run_to_file.file_id `;

    let results = [], count = 0, summaries = [], clusters = [];

    if (result == "sample") {
        let groupByStr = " GROUP BY sample.sample_id,project.project_id ";

        // Build total count query
        let countQueryStr =
            `SELECT COUNT(foo) FROM (SELECT sample.sample_id ${tableStr} ${clauseStr} ${groupByStr}) AS foo`;

        // Build summary queries (for charts)
        let projectSummaryQueryStr =
            `SELECT project.name,COUNT(project_to_sample.sample_id)::int
            FROM project
            JOIN project_to_sample ON project_to_sample.project_id=project.project_id
            JOIN sample ON project_to_sample.sample_id=sample.sample_id
            LEFT JOIN experiment ON experiment.sample_id=sample.sample_id
            LEFT JOIN library ON library.experiment_id=experiment.experiment_id
            ${clauseStr} GROUP BY project.project_id ORDER BY project.name`;

        let summaryQueryStrs = [ projectSummaryQueryStr ];
        for (let termId of termIds) { //FIXME dup'ed in /searchTerms/:id(*) endpoint above
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
                            WIDTH_BUCKET(NULLIF(${caseStr},'NaN'),min_val,max_val+1e-9,10) AS bucket,COUNT(*)::int
                        ${tableStr}, min_max
                        ${clauseStr}
                        GROUP BY bucket) AS foo`;
                        // The max_val+1e-9 in the width_bucket() call is a kludge to prevent the error
                        // "lower bound cannot equal upper bound" when all data values are zero, as is the case for
                        // hydrogen sulfide (http://purl.obolibrary.org/obo/ENVO_3100017)
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
            "SELECT " + ["schema_id", "sample.sample_id", "project.project_id", "project.name", "sample.accn"].concat(selections2).join(",") + " " +
            tableStr +
            clauseStr + groupByStr + sortStr + offsetStr + limitStr;

        // Execute queries and format results //TODO make queries run in parallel
        console.log("Count Query:");
        count = await db.query({
            text: countQueryStr,
            rowMode: 'array'
        });
        count = count.rows[0][0]*1;

        console.log("Summary Queries:");
        summaries = await Promise.all(summaryQueryStrs.map(s =>
            db.query({
                text: s,
                rowMode: 'array'
            })
        ));
        summaries = summaries.map(res => res.rows || []);
        for (let summary of summaries) {
            let sort = false;
            for (let row of summary) { //TODO move into function (dup'ed elsewhere)
                if (row[0].startsWith("http://")) { // is this a purl?
                    sort = true;
                    let term2 = termIndex.getTerm(row[0]);
                    if (term2 && term2.label)
                        row[0] = term2.label;
                }
            }
            if (sort)
                summary = summary.sort((a, b) => a[0].toLowerCase().localeCompare(b[0].toLowerCase())); // sort converted PURL labels
        }

        console.log("Sample Query:");
        results = await db.query({
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
                                let term = termIndex.getTerm(v);
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
                sample.sample_id,sample.accn as sample_accn,project.name AS project_name,
                ST_X(ST_GeometryN(locations::geometry, 1)) AS longitude,
                ST_Y(ST_GeometryN(locations::geometry, 1)) AS latitude
            ${tableStr}
            ${clauseStr}
            ${groupByStr}`;

        //if (map)
            clusters = await db.query(locationClusterQuery);
    }
    else if (result == "file") { //-------------------------------------------------------------------------------------
        let sortDir = (typeof sort !== 'undefined' && sort > 0 ? "ASC" : "DESC");
        let sortStr = (typeof sort !== 'undefined' ? " ORDER BY " + (Math.abs(sort)+1) + " " + sortDir : "");

        let fileClause = " AND file.file_id IS NOT NULL ";
        let groupByStr = " GROUP BY file.file_id,sample.sample_id,project.project_id ";

        let countQueryStr =
            `SELECT COUNT(foo) FROM (SELECT file.file_id
            ${tableStr}
            ${clauseStr} ${fileClause} ${groupByStr}) AS foo`;

        let queryStr =
            "SELECT file.file_id,project.name,sample.accn,file.url,sample.sample_id,project.project_id " + //FIXME order of fields should match sample query above in order for sorting to work
            tableStr +
            clauseStr + fileClause +
            groupByStr + sortStr +
            offsetStr + limitStr;

        count = await db.query({
            text: countQueryStr,
            values: [],
            rowMode: 'array'
        });
        count = count.rows[0][0]*1;

        results = await db.query({
            text: queryStr,
            values: [],
            rowMode: 'array'
        });

        results = results.rows.map(r => {
            return {
                fileId: r[0],
                sampleId: r[4],
                projectId: r[5],
                projectName: r[1],
                sampleAccn: r[2],
                fileUrl: r[3]
            }
        })
    }
    else {
        console.log("Error: invalid result specifier:", result);
    }

    return {
        count: count,
        fields: ['Project Name', 'Sample ID'].concat(Object.keys(selections)),
        summary: summaries,
        results: results,
        map: (clusters ? clusters.rows : {})
    };
}

function getSchemasForTerms(terms) {
    let schemas = [];
    for (let term of terms) {
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

function buildTermSQL(arrIndex, term, val) {
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

async function batchQuery(queryObjArray) {
  const promises = queryObjArray.map(obj => db.query(obj));
  return await Promise.all(promises);
}

function defined(val) {
    return (typeof val !== "undefined");
}

function present(val) {
    return defined(val) && val != "";
}

module.exports = router;