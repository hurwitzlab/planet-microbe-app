const requestp = require('request-promise');
const config = require('../config.json');

const apiBaseUrl = `http://localhost:${config.serverPort}`;
const get = route => requestp({ method: 'GET', uri: `${apiBaseUrl}/${route}`, json: true });
const post = (route, data) => requestp({ method: 'POST', uri: `${apiBaseUrl}/${route}`, body: data, json: true });

const getSearchTerm = id => get(`searchTerms/${id}`);
const search = constraints => post(`search`, constraints);


/*
NOTES:
    - these tests are not affected by adding new datasets
*/

it('should return search term descriptor for "depth of water"', async () => {
    const data = await getSearchTerm('http://purl.obolibrary.org/obo/ENVO_3100031');
    expect(data).toBeDefined();
    expect(data.length).toEqual(1);
    expect(data[0].id).toEqual('http://purl.obolibrary.org/obo/ENVO_3100031');
});

it('should return search results for project filter', async () => {
    const data = await search({ 'project': 'Tara Oceans' });
    expect(data).toBeDefined();
    expect(data.sampleCount).toEqual(1250)
});

it('should return search results for location filter', async () => {
    const data = await search({
        'project': 'Tara Oceans',
        'location': '[35,-36,1400]'
    });
    expect(data).toBeDefined();
    expect(data.sampleCount).toEqual(33)
});

it('should return search results for depth filter', async () => {
    const data = await search({
        'project': 'GOS',
        'http://purl.obolibrary.org/obo/ENVO_3100031': '[0,10]' //depth
    });
    expect(data).toBeDefined();
    expect(data.sampleCount).toEqual(41)
});

it('should return search results for date filter', async () => {
    const data = await search({
        'project': 'Tara Oceans',
        '|http://purl.obolibrary.org/obo/OBI_0001619': '[2010-01-01,2010-06-01]',  // date
        '|http://purl.obolibrary.org/obo/PMO_00000008': '[2010-01-01,2010-06-01]', // start date
        '|http://purl.obolibrary.org/obo/PMO_00000009': '[2010-01-01,2010-06-01]', // end date
    });
    expect(data).toBeDefined();
    expect(data.sampleCount).toEqual(216)
});

it('should return search results for string filter', async () => {
    const data = await search({
        'project': 'OSD',
        'http://purl.obolibrary.org/obo/ENVO_00000428': 'http://purl.obolibrary.org/obo/envo_00000447' // biome = "Marine biome"
    });
    expect(data).toBeDefined();
    expect(data.sampleCount).toEqual(93)
});

it('should return sorted search results for numeric range filter', async () => {
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

it('should return sorted search results for bunch of filters', async () => {
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
