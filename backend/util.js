'use strict';

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

function requireAuth(req) {
    if (!req || !req.auth || !req.auth.validToken || !req.auth.user)
        throw(ERR_UNAUTHORIZED);
}

module.exports.requireAuth = requireAuth;