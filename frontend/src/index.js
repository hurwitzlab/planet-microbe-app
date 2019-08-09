import '../assets/scss/bootstrap_custom.scss'
import '../assets/css/landing-page.css';
//import '../assets/css/elm-datepicker.css';
import $ from "jquery";
//import popper from "popper.js"; // for Bootstrap but not used
import bootstrap from "bootstrap";
//import "@fortawesome/fontawesome-free/js/all.min.js" // DO NOT USE!  Include CSS as below
import "@fortawesome/fontawesome-free/css/all.min.css"
import { Elm } from './Main.elm';



/*
 * Generate random code for OAuth2 state (from https://github.com/truqu/elm-oauth2/blob/master/examples/authorization-code/index.html)
 */

//function fromLocalStorage(name, newItem) {
//    const item = localStorage.getItem(name) || newItem();
//    localStorage.setItem(name, item);
//    return item;
//}
//
//const randomCode = (name) =>
//    fromLocalStorage(name, () => {
//        const buffer = new Uint8Array(256);
//        crypto.getRandomValues(buffer);
//        return Array
//          .from(buffer)
//          .map(x => String.fromCharCode((x % 26) + 65))
//          .join("");
//    });


/*
 * Initialize Elm app
 */

const COOKIE_NAME = 'planetmicrobe-0.0.1';
const CRED_COOKIE_NAME = COOKIE_NAME + '.cred';
const CART_COOKIE_NAME = COOKIE_NAME + '.cart';
//const CODE_COOKIE_NAME = COOKIE_NAME + '.code';

var app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: {
    cred: JSON.parse(localStorage.getItem(CRED_COOKIE_NAME)) || null,
    cart: JSON.parse(localStorage.getItem(CART_COOKIE_NAME)) || null
//    randomCode: randomCode(CODE_COOKIE_NAME)
  }
});


/*
 * Define ports for storing/watching cookies
 */

app.ports.storeCredentials.subscribe(function(session) {
    console.log("storeCredentials: ", session);
    localStorage.setItem(CRED_COOKIE_NAME, JSON.stringify(session));
});

// This event is only triggered when cookie is modified in another tab/window
window.addEventListener("storage",
    function(event) {
        if (event.storageArea === localStorage && event.key === CRED_COOKIE_NAME) {
            console.log("storage listener:", event.newValue);
            app.ports.onCredentialsChange.send(event.newValue);
        }
    },
    false
);

app.ports.storeCart.subscribe(function(session) {
    console.log("storeCart: ", session);
    localStorage.setItem(CART_COOKIE_NAME, JSON.stringify(session));
});

// This event is only triggered when cookie is modified in another tab/window
window.addEventListener("storage",
    function(event) {
        if (event.storageArea === localStorage && event.key === CART_COOKIE_NAME) {
            console.log("storage listener:", event.newValue);
            app.ports.onSessionChange.send(event.newValue);
        }
    },
    false
);


/*
 * Google Analytics
 */

window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());

// Log route changes from Elm
app.ports.updateAnalytics.subscribe(function (event) {
  if (!event || !event.page || !event.trackingId) {
    console.error("updateAnalytics: invalid event");
    return;
  }

  console.log("updateAnalytics:", event.page);
  gtag('config', event.trackingId, {
    'page_path': event.page
  });
});


/*
 * Google Maps
 */

var gmap,
  drawingManager,
  markerClusterer,
  markers = [],
  shapes = [],
  features = [],
  mapSettings = {
    showDrawingManager: true,
    showMarkerClusters: true,
    fitBounds: false,
    fullscreenControl: true
  };

function initMap() {
  console.log("initMap");

  var mapDiv = document.getElementsByTagName('gmap')[0]; // TODO add support for more than one map in a page
  if (!mapDiv)
    throw("initMap: map div not found");

  gmap = new google.maps.Map(mapDiv, {
    zoom: 2,
    //minZoom: 1,
    center: new google.maps.LatLng(0, 0),
    streetViewControl: false,
    fullscreenControl: mapSettings.fullscreenControl,
    //mapTypeId: 'satellite'
  });

  drawingManager = new google.maps.drawing.DrawingManager({
    drawingMode: null,
    drawingControl: mapSettings.showDrawingManager,
    drawingControlOptions: {
      position: google.maps.ControlPosition.TOP_CENTER,
      drawingModes: ['circle'] //['marker', 'circle', 'polygon', 'polyline', 'rectangle']
    },
    circleOptions: {
      fillColor: 'lightgray',
      fillOpacity: 0.4,
      strokeColor: 'gray',
      strokeOpacity: 0.8,
      strokeWeight: 1,
      clickable: false,
      editable: true,
      draggable: true,
      zIndex: 1
    }
  });
  drawingManager.setMap(gmap);

  google.maps.event.addListener(drawingManager, 'circlecomplete',
    function(circle) {
      console.log("circlecomplete");
      // Remove current circles, only show latest one on the map at a time
      for (var i = 0; i < shapes.length; i++) {
        shapes[i].setMap(null);
      }
      shapes.push(circle);

      handleCircleEvent.apply(circle);
      circle.addListener('radius_changed', handleCircleEvent.bind(circle));
      circle.addListener('center_changed', handleCircleEvent.bind(circle));
    }
  );

  // Listen on circle selection button to clear selection
  google.maps.event.addListener(drawingManager, "drawingmode_changed", function() {
    console.log("drawing mode changed:"+drawingManager.getDrawingMode());
    if (drawingManager.getDrawingMode() == "circle" && shapes.length > 0) {
      for (var i = 0; i < shapes.length; i++) {
        shapes[i].setMap(null);
      }
      app.ports.getLocation.send(null);
    }
  });

  gmap.addListener('zoom_changed',
    function() {
      let zoom = gmap.getZoom();
      if (zoom >= 4) {
        for (var i = 0; i < markers.length; i++) {
          let marker = markers[i];
          let label = "";
          if (marker.customData.project_name && marker.customData.sample_accn)
            label = marker.customData.project_name + " - " + marker.customData.sample_accn
          marker.setLabel(label);
        }
      }
      else {
        for (var i = 0; i < markers.length; i++) {
          markers[i].setLabel("");
        }
      }
    }
  );

  markerClusterer = new MarkerClusterer(gmap, [],
    { imagePath: 'https://developers.google.com/maps/documentation/javascript/examples/markerclusterer/m',
      //gridSize: 50,
      maxZoom: 15,
      averageCenter: true
    }
  );
}

function resetMap(results) { // TODO only clear markers that aren't in new results
  // Clear all markers
  for (var i = 0; i < markers.length; i++) {
    markers[i].setMap(null);
  }
  markers = [];
  if (markerClusterer)
    markerClusterer.clearMarkers();

  /*for (var i = 0; i < features.length; i++) {
    gmap.data.remove(features[i]);
  }
  features = [];*/

  if (gmap)
    google.maps.event.clearListeners(gmap, 'radius_changed');
}

app.ports.removeMap.subscribe(function() {
  console.log("removeMap");
  resetMap();
  gmap = null;
});

app.ports.changeMapSettings.subscribe(function(settings) {
  console.log("changeMapSettings");
  mapSettings = settings;
});

app.ports.loadMap.subscribe(function(results) {
  console.log("loadMap");//: results:", results);

  if (!gmap)
    initMap();
  else
    resetMap(results);

  if (!results || results.length == 0) {
    console.log("loadMap: no results");
    return;
  }

  var bounds = new google.maps.LatLngBounds();

  /*model.forEach(cluster => {
    let circle = gmap.data.addGeoJson({ type: "Feature", id: 123, "geometry": JSON.parse(cluster.circle) });
    features = features.concat(circle);

    let centroid = JSON.parse(cluster.centroid)

    let marker = new google.maps.Marker({
        position: new google.maps.LatLng(centroid.coordinates[1], centroid.coordinates[0]),
        map: gmap,
        label: { text: cluster.count.toString(), color: "white" }
    });
    markers.push(marker);

    bounds.extend(marker.position);
  });*/

  results.forEach(result => {
    let marker = new google.maps.Marker({
        position: new google.maps.LatLng(result.latitude, result.longitude),
        customData: {
          sample_accn: result.sample_accn,
          project_name: result.project_name
        },
        url: (result.sample_id ? "#/samples/" + result.sample_id : null)
        //map: gmap // set below
    });
    marker.addListener("click",
      function() {
        console.log("click");
        if (this.url)
          //window.open(this.url, '_blank');
          window.location.href = this.url;
      }
    );
    markers.push(marker);

    bounds.extend(marker.position);
  });

  if (mapSettings.showMarkerClusters)
    markerClusterer.addMarkers(markers);
  else {
    for (var i = 0; i < markers.length; i++)
      markers[i].setMap(gmap);
  }
  console.log("markers:", markers.length); //markerClusterer.getMarkers().length);

  if (mapSettings.fitBounds) {
    //gmap.fitBounds(bounds);
    gmap.setZoom(0);
  }

  app.ports.mapLoaded.send(true);
});

function handleCircleEvent() {
  console.log("handleCircleEvent:", this);

  // Send selected circle coords to Elm
  app.ports.getLocation.send({
    lat: this.center.lat(),
    lng: this.center.lng(),
    radius: this.getRadius()
  });
}

app.ports.setLocation.subscribe(function(location) {
  console.log("setLocation:", location);
  if (!location) {
    for (var i = 0; i < shapes.length; i++) {
      shapes[i].setMap(null);
    }
  }
  else {
    for (var i = 0; i < shapes.length; i++) {
      circle = shapes[i];
      console.log(circle);
      google.maps.event.clearListeners(circle, 'radius_changed');
      google.maps.event.clearListeners(circle, 'center_changed');
      circle.setCenter(new google.maps.LatLng(location.lat,location.lng));
      circle.setRadius(location.radius);
      circle.addListener('radius_changed', handleCircleEvent.bind(circle));
      circle.addListener('center_changed', handleCircleEvent.bind(circle));
    }
  }
});
