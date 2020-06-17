const request = require('request');

const getSearchTerm = id => request(`http://localhost:3020/searchTerms/${id}`);

it('should return search term', async () => {
    const data = await getSearchTerm('http://purl.obolibrary.org/obo/ENVO_3100031');
    expect(data).toBeDefined();
    //expect(data.entity.name).toEqual('Koen van Gilst');
});