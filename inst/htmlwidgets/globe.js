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

        if(x.hasOwnProperty("height"))
          globe.height(x.height);

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

}