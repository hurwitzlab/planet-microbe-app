'use strict';

const { Client } = require('pg');

module.exports = function(config) {
    let client = new Client({
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
}