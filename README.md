# Planet Microbe Search App

## Backend 

For development:
```
cd backend
npm install
nodemon server.js
```

For production:
```
cd backend
sudo npm install pm2@latest -g
pm2 start --name node-imicrobe server.js
sudo pm2 startup systemd
```

## Frontend

For development:
```
cd frontend
elm package install
npm install
elm-live src/Main
```

For production:
```
cd frontend
elm package install
npm install
npm run build
```
