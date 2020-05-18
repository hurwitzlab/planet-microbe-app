'use strict';

const { Client } = require('pg');
const config = require('./config.json');

module.exports = function() {
    if (!('postgres' in config))
        throw('Missing postgres configuration');

    const client = new Client({
        user: config.postgres.username,
        host: 'localhost',
        database: config.postgres.db,
        password: config.postgres.password
    });
    client.connect();

    // Monkey patch query() to log to stdout
    const oldQuery = client.query;
    client.query = (...args) => {
      console.log('QUERY:', args);
      return oldQuery.apply(client, args);
    };

    return client;
}();
