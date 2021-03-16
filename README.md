# Planet Microbe

## Installation
### Backend 
```
cd backend
npm install
$(npm bin)/nodemon # dev
pm2 start --name planet-microbe-app server.js # prod
```

### Frontend
```
cd frontend
npm install
elm package install
npm start # dev
npm run build # prod
```

## Search API Examples

### Query by PURL attribute
```
curl -s -H 'Content-Type: application/json' -d '{"http://purl.obolibrary.org/obo/ENVO_00000428":"http://purl.obolibrary.org/obo/envo_01000048"}' https://www.planetmicrobe.org/api/search | jq .sampleResults
```

### Query by project
```
curl -s -H 'Content-Type: application/json' -d '{"project": "OSD"}' https://www.planetmicrobe.org/api/search | jq .sampleResults
```

### Query by numeric attribute
```
curl -s -H 'Content-Type: application/json' -d '{"http://purl.obolibrary.org/obo/ENVO_09200014":"[0,10]"}' https://www.planetmicrobe.org/api/search | jq .sampleResults
```



