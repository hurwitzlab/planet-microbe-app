# Planet Microbe Search App

## Backend 

Quick Start:
```
cd backend
npm install
dev: $npm bin)/nodemon
  *or*
prod: pm2 start --name planet-microbe-search-app server.js
```

## Frontend

Quick Start:
```
cd frontend
npm install
elm package install
dev: $(npm bin)/elm-live src/Main --output=elm.js --open
  *or*
prod: node_modules/elm/bin/elm make --output=elm.js src/Main.elm
```
