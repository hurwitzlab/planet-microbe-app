const requestp = require('request-promise');
const config = require('../config.json');

const apiBaseUrl = `http://localhost:${config.serverPort}`;
const get = route => requestp({ method: 'GET', uri: `${apiBaseUrl}/${route}`, json: true });
const post = (route, data) => requestp({ method: 'POST', uri: `${apiBaseUrl}/${route}`, body: data, json: true });


/*
 * Search
 *   - all tests were written so to not be invalidated by adding new datasets
 */

const getSearchTerm = id => get(`searchTerms/${id}`);
const search = constraints => post(`search`, constraints);

it('search: should return search term descriptor for "depth of water"', async () => {
    const data = await getSearchTerm('http://purl.obolibrary.org/obo/ENVO_3100031');
    expect(data).toBeDefined();
    expect(data.length).toEqual(1);
    expect(data[0].id).toEqual('http://purl.obolibrary.org/obo/ENVO_3100031');
});

it('search: should return search results for project filter', async () => {
    const data = await search({ 'project': 'Tara Oceans' });
    expect(data).toBeDefined();
    expect(data.sampleCount).toEqual(1250)
});

it('search: should return search results for location filter', async () => {
    const data = await search({
        'project': 'Tara Oceans',
        'location': '[35,-36,1400]'
    });
    expect(data).toBeDefined();
    expect(data.sampleCount).toEqual(33)
});

it('search: should return search results for depth filter', async () => {
    const data = await search({
        'project': 'GOS',
        'http://purl.obolibrary.org/obo/ENVO_3100031': '[0,10]' //depth
    });
    expect(data).toBeDefined();
    expect(data.sampleCount).toEqual(41)
});

it('search: should return search results for date filter', async () => {
    const data = await search({
        'project': 'Tara Oceans',
        '|http://purl.obolibrary.org/obo/OBI_0001619': '[2010-01-01,2010-06-01]',  // date
        '|http://purl.obolibrary.org/obo/PMO_00000008': '[2010-01-01,2010-06-01]', // start date
        '|http://purl.obolibrary.org/obo/PMO_00000009': '[2010-01-01,2010-06-01]', // end date
    });
    expect(data).toBeDefined();
    expect(data.sampleCount).toEqual(216)
});

it('search: should return search results for string filter', async () => {
    const data = await search({
        'project': 'OSD',
        'http://purl.obolibrary.org/obo/ENVO_00000428': 'http://purl.obolibrary.org/obo/envo_00000447' // biome = "Marine biome"
    });
    expect(data).toBeDefined();
    expect(data.sampleCount).toEqual(93)
});

it('search: should return sorted search results for numeric range filter', async () => {
    const data = await search({
        'project': 'HOT ALOHA time/depth series',
        'http://purl.obolibrary.org/obo/ENVO_09200014': '[0,10]', // temperature of water
        'sampleSort': '3'
    });
    expect(data).toBeDefined();
    expect(data.sampleCount).toEqual(68);
    expect(data.sampleResults).toBeDefined();
    expect(data.sampleResults[0]).toBeDefined();
    expect(data.sampleResults[0].sampleAccn).toEqual('SAMN05991720');
    expect(data.sampleResults[0].values[0]).toEqual([3.7316, 3.72, 3.72, 3.72]);
});

it('search: should return sorted search results for bunch of filters', async () => {
    const data = await search({
        'project': "Amazon Plume Metagenomes|Amazon Plume Metatranscriptomes|Amazon PolyA Metatranscriptomes|Amazon River Metagenomes|Amazon River Metatranscriptomes|BATS Chisholm|CDEBI Juan de Fuca Ridge Flank|GOS|HOT ALOHA time/depth series|HOT Chisholm|HOT DeLong|HOT DeLong Metatranscriptomes|OSD|Tara Oceans|Tara Polar Circle Expedition",
        'location': "[46,2.7,2265]",
        'http://purl.obolibrary.org/obo/ENVO_00000428': "http://purl.obolibrary.org/obo/envo_00000447", // biome = "Marine biome"
        'http://purl.obolibrary.org/obo/ENVO_00002297': "http://purl.obolibrary.org/obo/envo_00000015", // environmental material = "Coastal sea water"
        'http://purl.obolibrary.org/obo/ENVO_00010483': "http://purl.obolibrary.org/obo/envo_00002150", // environmental feature = "Ocean"
        'http://purl.obolibrary.org/obo/ENVO_3100031': "[0,100]",  // depth
        'http://purl.obolibrary.org/obo/ENVO_09200014': "[20,22]", // temperature of water
        'http://purl.obolibrary.org/obo/PMO_00000014': "[40,41]",  // salinity
        '|http://purl.obolibrary.org/obo/OBI_0001619': "[2010-01-01,2015-01-01]",  // date
        '|http://purl.obolibrary.org/obo/PMO_00000008': "[2010-01-01,2015-01-01]", // start date
        '|http://purl.obolibrary.org/obo/PMO_00000009': "[2010-01-01,2015-01-01]", // end date
    });
    expect(data).toBeDefined();
    expect(data.sampleCount).toEqual(1);
    expect(data.sampleResults).toBeDefined();
    expect(data.sampleResults[0]).toBeDefined();
    expect(data.sampleResults[0].sampleAccn).toEqual('SAMEA3275534');
});


/*
 * Projects
 */

const getProject = id => get(`projects/${id}`);

it('projects: should return all projects', async () => {
    const projects = await getProject('');
    expect(projects).toBeDefined();
    expect(projects.length).toBeGreaterThanOrEqual(15);

    for (const project of projects) {
        const data = await getProject(project.project_id);
        expect(data).toBeDefined();
        expect(data.datapackage_url).toBeDefined();
        expect(data.sample_count).toBeGreaterThan(0);
    }
});


/*
 * Samples
 */

const getSamples = () => post('samples', {});
const getSample = id => get(`samples/${id}`);

it('samples: should return all samples', async () => {
    const samples = await getSamples();
    expect(samples).toBeDefined();
    expect(samples.length).toBeGreaterThanOrEqual(2371);

    for (const sample of samples) {
        const data = await getSample(sample.sample_id);
        expect(data).toBeDefined();
    }
});

