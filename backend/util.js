'use strict';

// Handle promise exceptions in Express
// From https://zellwk.com/blog/async-await-express/
const asyncHandler = fn => (req, res, next) => {
    return Promise
        .resolve(fn(req, res, next))
        .catch(next);
};

module.exports = { asyncHandler };