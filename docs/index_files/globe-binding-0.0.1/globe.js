HTMLWidgets.widget({

  name: 'globe',

  type: 'output',

  factory: function(el, width, height) {

    var dom,
        globe;

    return {

      renderValue: function(x) {

        dom = document.getElementById(el.id);
        globe = Globe(x.init)(dom);

        // general options
        if(x.hasOwnProperty("globeImageUrl"))
          globe.globeImageUrl(x.globeImageUrl);

        if(x.hasOwnProperty("bumpImageUrl"))
          globe.bumpImageUrl(x.bumpImageUrl);

        if(x.hasOwnProperty("showAtmosphere"))
          globe.showAtmosphere(x.showAtmosphere);

        if(x.hasOwnProperty("showGraticules"))
          globe.showGraticules(x.showGraticules);

        if(x.hasOwnProperty("width"))
          globe.width(x.width);
        else 
          globe.width(dom.offsetWidth);

        if(x.hasOwnProperty("height"))
          globe.height(x.height);
        else
          globe.height(dom.offsetHeight);

        if(x.hasOwnProperty("backgroundColor"))
          globe.backgroundColor(x.backgroundColor);

        // points
        if(x.hasOwnProperty("pointsData"))
          globe.pointsData(x.pointsData);
        if(x.hasOwnProperty("pointLabel"))
          globe.pointLabel(x.pointLabel);
        if(x.hasOwnProperty("pointLat"))
          globe.pointLat(x.pointLat);
        if(x.hasOwnProperty("pointLng"))
          globe.pointLng(x.pointLng);
        if(x.hasOwnProperty("pointColor"))
          globe.pointColor(x.pointColor);
        if(x.hasOwnProperty("pointAltitude"))
          globe.pointAltitude(x.pointAltitude);
        if(x.hasOwnProperty("pointResolution"))
          globe.pointResolution(x.pointResolution);
        if(x.hasOwnProperty("pointRadius"))
          globe.pointRadius(x.pointRadius);
        if(x.hasOwnProperty("pointsMerge"))
          globe.pointsMerge(x.pointsMerge);
        if(x.hasOwnProperty("pointsTransitionDuration"))
          globe.pointsTransitionDuration(x.pointsTransitionDuration);
        if(x.hasOwnProperty("onPointClick"))
          globe.onPointClick(x.onPointClick);  
        if(x.hasOwnProperty("onPointRightClick"))
          globe.onPointRightClick(x.onPointRightClick);  
        if(x.hasOwnProperty("onPointHover"))
          globe.onPointHover(x.onPointHover);  

        // arcs
        if(x.hasOwnProperty("arcsData"))
          globe.arcsData(x.arcsData);
        if(x.hasOwnProperty("arcColor"))
          globe.arcColor(x.arcColor);
        if(x.hasOwnProperty("arcAltitude"))
          globe.arcAltitude(x.arcAltitude);
        if(x.hasOwnProperty("arcAltitudeAutoScale"))
          globe.arcAltitudeAutoScale(x.arcAltitudeAutoScale);
        if(x.hasOwnProperty("arcStroke"))
          globe.arcStroke(x.arcStroke);
        if(x.hasOwnProperty("arcCurveResolution"))
          globe.arcCurveResolution(x.arcCurveResolution);
        if(x.hasOwnProperty("arcCircularResolution"))
          globe.arcCircularResolution(x.arcCircularResolution);
        if(x.hasOwnProperty("arcDashLength"))
          globe.arcDashLength(x.arcDashLength);
        if(x.hasOwnProperty("arcDashGap"))
          globe.arcDashGap(x.arcDashGap);
        if(x.hasOwnProperty("arcDashInitialGap"))
          globe.arcDashInitialGap(x.arcDashInitialGap);
        if(x.hasOwnProperty("arcDashAnimateTime"))
          globe.arcDashAnimateTime(x.arcDashAnimateTime);  
        if(x.hasOwnProperty("arcsTransitionDuration"))
          globe.arcsTransitionDuration(x.arcsTransitionDuration);  
        if(x.hasOwnProperty("onArcClick"))
          globe.onArcClick(x.onArcClick); 
        if(x.hasOwnProperty("onArcRightClick"))
          globe.onArcRightClick(x.onArcRightClick); 
        if(x.hasOwnProperty("onArcHover"))
          globe.onArcHover(x.onArcHover); 

        // polygon
        if(x.hasOwnProperty("polygonsData"))
          globe.polygonsData(x.polygonsData); 
        if(x.hasOwnProperty("polygonLabel"))
          globe.polygonLabel(x.polygonLabel); 
        if(x.hasOwnProperty("polygonGeoJsonGeometry"))
          globe.polygonGeoJsonGeometry(x.polygonGeoJsonGeometry); 
        if(x.hasOwnProperty("polygonCapColor"))
          globe.polygonCapColor(x.polygonCapColor);
        if(x.hasOwnProperty("polygonSideColor"))
          globe.polygonSideColor(x.polygonSideColor);  
        if(x.hasOwnProperty("polygonAltitude"))
          globe.polygonAltitude(x.polygonAltitude); 
        if(x.hasOwnProperty("polygonsTransitionDuration"))
          globe.polygonsTransitionDuration(x.polygonsTransitionDuration); 
        if(x.hasOwnProperty("onPolygonClick"))
          globe.onPolygonClick(x.onPolygonClick); 
        if(x.hasOwnProperty("onPolygonRightClick"))
          globe.onPolygonRightClick(x.onPolygonRightClick); 
        if(x.hasOwnProperty("onPolygonHover"))
          globe.onPolygonHover(x.onPolygonHover); 

        // label
        if(x.hasOwnProperty("labelsData"))
          globe.labelsData(x.labelsData); 
        if(x.hasOwnProperty("labelText"))
          globe.labelText(x.labelText); 
        if(x.hasOwnProperty("labelLabel"))
          globe.labelLabel(x.labelLabel); 
        if(x.hasOwnProperty("labelColor"))
          globe.labelColor(x.labelColor); 
        if(x.hasOwnProperty("labelAltitude"))
          globe.labelAltitude(x.labelAltitude); 
        if(x.hasOwnProperty("labelSize"))
          globe.labelSize(x.labelSize); 
        if(x.hasOwnProperty("labelTypeFace"))
          globe.labelTypeFace(x.labelTypeFace); 
        if(x.hasOwnProperty("labelRotation"))
          globe.labelRotation(x.labelRotation); 
        if(x.hasOwnProperty("labelResolution"))
          globe.labelResolution(x.labelResolution); 
        if(x.hasOwnProperty("labelIncludeDot"))
          globe.labelIncludeDot(x.labelIncludeDot); 
        if(x.hasOwnProperty("labelDotRadius"))
          globe.labelDotRadius(x.labelDotRadius); 
        if(x.hasOwnProperty("labelDotOrientation"))
          globe.labelDotOrientation(x.labelDotOrientation); 
        if(x.hasOwnProperty("labelsTransitionDuration"))
          globe.labelsTransitionDuration(x.labelsTransitionDuration); 
        if(x.hasOwnProperty("onLabelClick"))
          globe.onLabelClick(x.onLabelClick); 
        if(x.hasOwnProperty("onLabelRightClick"))
          globe.onLabelRightClick(x.onLabelRightClick); 
        if(x.hasOwnProperty("onLabelHover"))
          globe.onLabelHover(x.onLabelHover); 

        if(x.hasOwnProperty("pointOfView")){
          globe.pointOfView(x.pointOfView, x.pointOfViewMs);
        }

      },

      getGlobe: function(){
        return globe;
      },

      resize: function(width, height) {

        if(globe){
          globe
            .width(width)
            .height(height)
        }

      }

    };
  }
});

function get_globe(id){

  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  var g;

  if (typeof htmlWidgetsObj != 'undefined') {
    g = htmlWidgetsObj.getGlobe();
  }

  return(g);
}

if (HTMLWidgets.shinyMode) {
  
  Shiny.addCustomMessageHandler('globeImageUrl',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.globeImageUrl(data.url);
      }
  });

  Shiny.addCustomMessageHandler('bumpImageUrl',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.bumpImageUrl(data.url);
      }
  });

  Shiny.addCustomMessageHandler('showAtmosphere',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.showAtmosphere(data.show);
      }
  });

  Shiny.addCustomMessageHandler('resume_animation',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.resumeAnimation();
      }
  });

  Shiny.addCustomMessageHandler('pause_animation',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pauseAnimation();
      }
  });

  Shiny.addCustomMessageHandler('showGraticules',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.showGraticules(data.show);
      }
  });

  Shiny.addCustomMessageHandler('backgroundColor',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.backgroundColor(data.color);
      }
  });

  Shiny.addCustomMessageHandler('dimensions',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        if(data.hasOwnProperty("width"))
          globe.width(data.width);

        if(data.hasOwnProperty("height"))
          globe.height(data.height);
      }
  });

  Shiny.addCustomMessageHandler('globe_pov',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pointOfView(data.pointOfView, data.ms);
      }
  });

  Shiny.addCustomMessageHandler('points_data',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pointsData(data.pointsData);
      }
  });

  Shiny.addCustomMessageHandler('points_lon',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pointLng(data.pointLng);
      }
  });

  Shiny.addCustomMessageHandler('points_lat',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pointLat(data.pointLat);
      }
  });

  Shiny.addCustomMessageHandler('points_color',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pointColor(data.pointColor);
      }
  });

  Shiny.addCustomMessageHandler('points_altitude',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pointAltitude(data.pointAltitude);
      }
  });

  Shiny.addCustomMessageHandler('points_radius',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pointRadius(data.pointRadius);
      }
  });

  Shiny.addCustomMessageHandler('points_resolution',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pointResolution(data.pointResolution);
      }
  });

  Shiny.addCustomMessageHandler('points_merge',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pointMerge(data.pointMerge);
      }
  });

  Shiny.addCustomMessageHandler('points_transition',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pointsTransitionDuration(data.pointsTransitionDuration);
      }
  });

  Shiny.addCustomMessageHandler('points_on_click',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onPointClick(data.onPointClick);
      }
  });

  Shiny.addCustomMessageHandler('points_on_right_click',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onPointHover(data.onPointHover);
      }
  });

  Shiny.addCustomMessageHandler('points_on_hover',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onPointRightClick(data.onPointRightClick);
      }
  });

  Shiny.addCustomMessageHandler('arcs_data',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcsData(data.arcsData);
      }
  });

  Shiny.addCustomMessageHandler('globe_points',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        if(data.hasOwnProperty("pointsData"))
          globe.pointsData(data.pointsData);
        if(data.hasOwnProperty("pointLabel"))
          globe.pointLabel(data.pointLabel);
        if(data.hasOwnProperty("pointLat"))
          globe.pointLat(data.pointLat);
        if(data.hasOwnProperty("pointLng"))
          globe.pointLng(data.pointLng);
        if(data.hasOwnProperty("pointColor"))
          globe.pointColor(data.pointColor);
        if(data.hasOwnProperty("pointAltitude"))
          globe.pointAltitude(data.pointAltitude);
        if(data.hasOwnProperty("pointResolution"))
          globe.pointResolution(data.pointResolution);
        if(data.hasOwnProperty("pointRadius"))
          globe.pointRadius(data.pointRadius);
        if(data.hasOwnProperty("pointsMerge"))
          globe.pointsMerge(data.pointsMerge);
        if(data.hasOwnProperty("pointsTransitionDuration"))
          globe.pointsTransitionDuration(data.pointsTransitionDuration);
        if(data.hasOwnProperty("onPointClick"))
          globe.onPointClick(data.onPointClick);  
        if(data.hasOwnProperty("onPointRightClick"))
          globe.onPointRightClick(data.onPointRightClick);  
        if(data.hasOwnProperty("onPointHover"))
          globe.onPointHover(data.onPointHover); 
      }
  });

  Shiny.addCustomMessageHandler('globe_arcs',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        if(data.hasOwnProperty("arcsData"))
          globe.arcsData(data.arcsData);
        if(data.hasOwnProperty("arcColor"))
          globe.arcColor(data.arcColor);
        if(data.hasOwnProperty("arcAltitude"))
          globe.arcAltitude(data.arcAltitude);
        if(data.hasOwnProperty("arcAltitudeAutoScale"))
          globe.arcAltitudeAutoScale(data.arcAltitudeAutoScale);
        if(data.hasOwnProperty("arcStroke"))
          globe.arcStroke(data.arcStroke);
        if(data.hasOwnProperty("arcCurveResolution"))
          globe.arcCurveResolution(data.arcCurveResolution);
        if(data.hasOwnProperty("arcCircularResolution"))
          globe.arcCircularResolution(data.arcCircularResolution);
        if(data.hasOwnProperty("arcDashLength"))
          globe.arcDashLength(data.arcDashLength);
        if(data.hasOwnProperty("arcDashGap"))
          globe.arcDashGap(data.arcDashGap);
        if(data.hasOwnProperty("arcDashInitialGap"))
          globe.arcDashInitialGap(data.arcDashInitialGap);
        if(data.hasOwnProperty("arcDashAnimateTime"))
          globe.arcDashAnimateTime(data.arcDashAnimateTime);  
        if(data.hasOwnProperty("arcsTransitionDuration"))
          globe.arcsTransitionDuration(data.arcsTransitionDuration);  
        if(data.hasOwnProperty("onArcClick"))
          globe.onArcClick(data.onArcClick); 
        if(data.hasOwnProperty("onArcRightClick"))
          globe.onArcRightClick(data.onArcRightClick); 
        if(data.hasOwnProperty("onArcHover"))
          globe.onArcHover(data.onArcHover); 
      }
  });

  Shiny.addCustomMessageHandler('globe_choropleth',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        if(data.hasOwnProperty("polygonsData"))
          globe.polygonsData(data.polygonsData); 
        if(data.hasOwnProperty("polygonLabel"))
          globe.polygonLabel(data.polygonLabel); 
        if(data.hasOwnProperty("polygonGeoJsonGeometry"))
          globe.polygonGeoJsonGeometry(data.polygonGeoJsonGeometry); 
        if(data.hasOwnProperty("polygonCapColor"))
          globe.polygonCapColor(data.polygonCapColor);
        if(data.hasOwnProperty("polygonSideColor"))
          globe.polygonSideColor(data.polygonSideColor);  
        if(data.hasOwnProperty("polygonAltitude"))
          globe.polygonAltitude(data.polygonAltitude); 
        if(data.hasOwnProperty("polygonsTransitionDuration"))
          globe.polygonsTransitionDuration(data.polygonsTransitionDuration); 
        if(data.hasOwnProperty("onPolygonClick"))
          globe.onPolygonClick(data.onPolygonClick); 
        if(data.hasOwnProperty("onPolygonRightClick"))
          globe.onPolygonRightClick(data.onPolygonRightClick); 
        if(data.hasOwnProperty("onPolygonHover"))
          globe.onPolygonHover(data.onPolygonHover);
      }
  });


  Shiny.addCustomMessageHandler('globe_labels',
    function(data) {
      var globe = get_globe(data.id);
      if(data.hasOwnProperty("labelsData"))
        globe.labelsData(data.labelsData); 
      if(data.hasOwnProperty("labelTedatat"))
        globe.labelTedatat(data.labelTedatat); 
      if(data.hasOwnProperty("labelLabel"))
        globe.labelLabel(data.labelLabel); 
      if(data.hasOwnProperty("labelColor"))
        globe.labelColor(data.labelColor); 
      if(data.hasOwnProperty("labelAltitude"))
        globe.labelAltitude(data.labelAltitude); 
      if(data.hasOwnProperty("labelSize"))
        globe.labelSize(data.labelSize); 
      if(data.hasOwnProperty("labelTypeFace"))
        globe.labelTypeFace(data.labelTypeFace); 
      if(data.hasOwnProperty("labelRotation"))
        globe.labelRotation(data.labelRotation); 
      if(data.hasOwnProperty("labelResolution"))
        globe.labelResolution(data.labelResolution); 
      if(data.hasOwnProperty("labelIncludeDot"))
        globe.labelIncludeDot(data.labelIncludeDot); 
      if(data.hasOwnProperty("labelDotRadius"))
        globe.labelDotRadius(data.labelDotRadius); 
      if(data.hasOwnProperty("labelDotOrientation"))
        globe.labelDotOrientation(data.labelDotOrientation); 
      if(data.hasOwnProperty("labelsTransitionDuration"))
        globe.labelsTransitionDuration(data.labelsTransitionDuration); 
      if(data.hasOwnProperty("onLabelClick"))
        globe.onLabelClick(data.onLabelClick); 
      if(data.hasOwnProperty("onLabelRightClick"))
        globe.onLabelRightClick(data.onLabelRightClick); 
      if(data.hasOwnProperty("onLabelHover"))
        globe.onLabelHover(data.onLabelHover); 
  });

}