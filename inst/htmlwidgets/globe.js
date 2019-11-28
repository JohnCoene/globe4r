HTMLWidgets.widget({

  name: 'globe',

  type: 'output',

  factory: function(el, width, height) {

    var dom,
        globe,
        shared_data;

    return {

      renderValue: function(x) {

        dom = document.getElementById(el.id);
        globe = Globe(x.init)(dom);
        shared_data = x;

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
        if(!typeof x.onPointClick === "boolean") globe.onPointClick(x.onPointClick); 
        if(!typeof x.onPointRightClick === "boolean") globe.onPointRightClick(x.onPointRightClick);
        if(!typeof x.onPointHover === "boolean") globe.onPointHover(x.onPointHover);

        if (HTMLWidgets.shinyMode) {
          if(x.onPointClick === true)
            globe.onPointClick(function (e) {
              Shiny.setInputValue(el.id + '_click_bar' + ":globe4rParseJS", e);
            })
          if(x.onPointRightClick == true)
            globe.onPointRightClick(function (e) {
              Shiny.setInputValue(el.id + '_right_click_bar' + ":globe4rParseJS", e);
            })
          if(x.onPointHover == true)
            globe.onPointHover(function (e) {
              Shiny.setInputValue(el.id + '_hover_bar' + ":globe4rParseJS", e);
            })
        }

        // arcs
        if(x.hasOwnProperty("arcsData"))
          globe.arcsData(x.arcsData);
        if(x.hasOwnProperty("arcStartLat"))
          globe.arcStartLat(x.arcStartLat);
        if(x.hasOwnProperty("arcStartLng"))
          globe.arcStartLng(x.arcStartLng);
        if(x.hasOwnProperty("arcEndLat"))
          globe.arcEndLat(x.arcEndLat);
        if(x.hasOwnProperty("arcEndLng"))
          globe.arcEndLng(x.arcEndLng);
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
        if(x.hasOwnProperty("arcStartLng"))
          globe.arcStartLng(x.arcStartLng);
        if(x.hasOwnProperty("arcStartLat"))
          globe.arcStartLat(x.arcStartLat);
        if(x.hasOwnProperty("arcEndLat"))
          globe.arcEndLat(x.arcEndLat);
        if(x.hasOwnProperty("arcEndLng"))
          globe.arcEndLng(x.arcEndLng);

        if(!typeof x.onArcClick === "boolean") globe.onArcClick(x.onArcClick); 
        if(!typeof x.onArcRightClick === "boolean") globe.onArcRightClick(x.onArcRightClick);
        if(!typeof x.onArcHover === "boolean") globe.onArcHover(x.onArcHover);

        if (HTMLWidgets.shinyMode) {
          if(x.onArcClick === true)
            globe.onArcClick(function (e) {
              Shiny.setInputValue(el.id + '_click_arc' + ":globe4rParseJS", e);
            })
          if(x.onArcRightClick == true)
            globe.onArcRightClick(function (e) {
              Shiny.setInputValue(el.id + '_right_click_arc' + ":globe4rParseJS", e);
            })
          if(x.onArcHover == true)
            globe.onArcHover(function (e) {
              Shiny.setInputValue(el.id + '_hover_arc' + ":globe4rParseJS", e);
            })
        }

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
        if(typeof x.onPolygonClick != "boolean") globe.onPolygonClick(x.onPolygonClick); 
        if(typeof x.onPolygonRightClick != "boolean") globe.onPolygonRightClick(x.onPolygonRightClick);
        if(typeof x.onPolygonHover != "boolean") globe.onPolygonHover(x.onPolygonHover);

        if (HTMLWidgets.shinyMode) {
          if(x.onPolygonClick == true){
            globe.onPolygonClick(function (e) {
              Shiny.setInputValue(el.id + '_click_polygon' + ":globe4rParseJS", e);
            })
          }
          if(x.onPolygonRightClick == true)
            globe.onPolygonRightClick(function (e) {
              Shiny.setInputValue(el.id + '_right_click_polygon' + ":globe4rParseJS", e);
            })
          if(x.onArcHover == true)
            globe.onPolygonHover(function (e) {
              Shiny.setInputValue(el.id + '_hover_polygon' + ":globe4rParseJS", e);
            })
        }

        // label
        if(x.hasOwnProperty("labelsData"))
          globe.labelsData(x.labelsData); 
        if(x.hasOwnProperty("labelText"))
          globe.labelText(x.labelText); 
        if(x.hasOwnProperty("labelLat"))
          globe.labelLat(x.labelLat); 
        if(x.hasOwnProperty("labelLng"))
          globe.labelLng(x.labelLng); 
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
        if(!typeof x.onLabelClick === "boolean") globe.onLabelClick(x.onLabelClick); 
        if(!typeof x.onLabelRightClick === "boolean") globe.onLabelRightClick(x.onLabelRightClick);
        if(!typeof x.onLabelHover === "boolean") globe.onLabelHover(x.onLabelHover);

        if (HTMLWidgets.shinyMode) {
          if(x.onLabelClick === true)
            globe.onLabelClick(function (e) {
              Shiny.setInputValue(el.id + '_click_label' + ":globe4rParseJS", e);
            })
          if(x.onLabelRightClick == true)
            globe.onLabelRightClick(function (e) {
              Shiny.setInputValue(el.id + '_right_click_label' + ":globe4rParseJS", e);
            })
          if(x.onLabelHover == true)
            globe.onLabelHover(function (e) {
              Shiny.setInputValue(el.id + '_hover_label' + ":globe4rParseJS", e);
            })
        }

        if(x.hasOwnProperty("pointOfView"))
          globe.pointOfView(x.pointOfView, x.pointOfViewMs);

        if(x.hasOwnProperty("autoRotate")){
          globe.controls().autoRotate = x.autoRotate;
          globe.controls().autoRotateSpeed = x.autoRotateSpeed;
        }

        if(x.hasOwnProperty("hexBinPointsData"))
          globe.hexBinPointsData(x.hexBinPointsData);
        if(x.hasOwnProperty("hexLabel"))
          globe.hexLabel(x.hexLabel);
        if(x.hasOwnProperty("hexBinPointLat"))
          globe.hexBinPointLat(x.hexBinPointLat);
        if(x.hasOwnProperty("hexBinPointLng"))
          globe.hexBinPointLng(x.hexBinPointLng);
        if(x.hasOwnProperty("hexBinPointWeight"))
          globe.hexBinPointWeight(x.hexBinPointWeight);
        if(x.hasOwnProperty("hexBinResolution"))
          globe.hexBinResolution(x.hexBinResolution);
        if(x.hasOwnProperty("hexMargin"))
          globe.hexMargin(x.hexMargin);
        if(x.hasOwnProperty("hexAltitude"))
          globe.hexAltitude(x.hexAltitude);
        if(x.hasOwnProperty("hexTopColor"))
          globe.hexTopColor(x.hexTopColor);
        if(x.hasOwnProperty("hexSideColor"))
          globe.hexSideColor(x.hexSideColor);
        if(x.hasOwnProperty("hexBinMerge"))
          globe.hexBinMerge(x.hexBinMerge);
        if(x.hasOwnProperty("hexTransitionDuration"))
          globe.hexTransitionDuration(x.hexTransitionDuration);
        if(!typeof x.onHexClick === "boolean") globe.onHexClick(x.onHexClick); 
        if(!typeof x.onHexRightClick === "boolean") globe.onHexRightClick(x.onHexRightClick);
        if(!typeof x.onHexHover === "boolean") globe.onHexHover(x.onHexHover);

        if (HTMLWidgets.shinyMode) {
          if(x.onHexClick === true)
            globe.onHexClick(function (e) {
              Shiny.setInputValue(el.id + '_click_hex' + ":globe4rParseJS", e);
            })
          if(x.onHexRightClick == true)
            globe.onHexRightClick(function (e) {
              Shiny.setInputValue(el.id + '_right_click_hex' + ":globe4rParseJS", e);
            })
          if(x.onHexHover == true)
            globe.onHexHover(function (e) {
              Shiny.setInputValue(el.id + '_hover_hex' + ":globe4rParseJS", e);
            })
        }


        if(x.hasOwnProperty("pathsData")){
          globe
            .pathsData(x.pathsData)
            .pathPoints('coords')
            .pathPointLat(p => p[1])
            .pathPointLng(p => p[0])
        }

        if(x.hasOwnProperty("pathColor"))
          globe.pathColor(x.pathColor);

        if(x.hasOwnProperty("pathResolution"))
          globe.pathResolution(x.pathResolution);

        if(x.hasOwnProperty("pathStroke"))
          globe.pathStroke(x.pathStroke);

        if(x.hasOwnProperty("pathDashGap"))
          globe.pathDashGap(x.pathDashGap);

        if(x.hasOwnProperty("pathDashLength"))
          globe.pathDashLength(x.pathDashLength);

        if(x.hasOwnProperty("pathDashInitialGap"))
          globe.pathDashInitialGap(x.pathDashInitialGap);

        if(x.hasOwnProperty("pathDashAnimateTime"))
          globe.pathDashAnimateTime(x.pathDashAnimateTime);

        if(x.hasOwnProperty("pathTransitionDuration"))
          globe.pathTransitionDuration(x.pathTransitionDuration);

        if(!typeof x.onPathClick === "boolean") globe.onPathClick(x.onPathClick); 
        if(!typeof x.onPathRightClick === "boolean") globe.onPathRightClick(x.onPathRightClick);
        if(!typeof x.onPathHover === "boolean") globe.onPathHover(x.onPathHover);

        if (HTMLWidgets.shinyMode) {
          if(x.onHexClick === true)
            globe.onPathClick(function (e) {
              Shiny.setInputValue(el.id + '_click_path' + ":globe4rParseJS", e);
            })
          if(x.onPathRightClick == true)
            globe.onPathRightClick(function (e) {
              Shiny.setInputValue(el.id + '_right_click_path' + ":globe4rParseJS", e);
            })
          if(x.onPathHover == true)
            globe.onPathHover(function (e) {
              Shiny.setInputValue(el.id + '_hover_path' + ":globe4rParseJS", e);
            })
        }

      },

      getGlobe: function(){
        return globe;
      },

      getData: function(dataset){

        var col;
        
        switch(dataset){
          case "polygons":
            col = "polygonsData";
            break;
          case "choropleth":
            col = "polygonsData";
            break;
          case "labels":
            col = "labelsData";
            break;
          case "arcs":
            col = "arcsData";
            break;
          case "hex":
            col = "hexBinPointsData";
            break;
          case "hexbin":
            col = "hexBinPointsData";
            break;
          case "default":
            col = "pointsData";
        }

        console.log(col);

        return(shared_data[col])
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

function get_data(id, variable){

  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  var dat;

  if (typeof htmlWidgetsObj != 'undefined') {
    dat = htmlWidgetsObj.getData(variable);
  }

  return(dat);
}

if (HTMLWidgets.shinyMode) {

  Shiny.addCustomMessageHandler('globe_rotate',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.controls().autoRotate = msg.autoRotate;
        globe.controls().autoRotateSpeed = msg.autoRotateSpeed;
      }
  });
  
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

  Shiny.addCustomMessageHandler('points_label',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pointLabel(data.pointLabel);
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

  Shiny.addCustomMessageHandler('arcs_label',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcLabel(data.arcLabel);
      }
  });

  Shiny.addCustomMessageHandler('arcs_start_lat',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcStartLat(data.arcStartLat);
      }
  });

  Shiny.addCustomMessageHandler('arcs_start_lon',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcStartLng(data.arcStartLng);
      }
  });

  Shiny.addCustomMessageHandler('arcs_end_lat',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcEndLat(data.arcEndLat);
      }
  });

  Shiny.addCustomMessageHandler('arcs_end_lon',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcEndLng(data.arcEndLng);
      }
  });

  Shiny.addCustomMessageHandler('arcs_color',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcColor(data.arcColor);
      }
  });


  Shiny.addCustomMessageHandler('arcs_altitude',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcAltitude(data.arcAltitude);
      }
  });

  Shiny.addCustomMessageHandler('arcs_stroke',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcStroke(data.arcStroke);
      }
  });

  Shiny.addCustomMessageHandler('arcs_curve_resolution',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcCurveResolution(data.arcCurveResolution);
      }
  });

  Shiny.addCustomMessageHandler('arcs_circular_resolution',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcCircularResolution(data.arcCircularResolution);
      }
  });

  Shiny.addCustomMessageHandler('arcs_altitude_scale',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcAltitudeAutoScale(data.arcAltitudeAutoScale);
      }
  });

  Shiny.addCustomMessageHandler('arcs_dash_length',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcDashLength(data.arcDashLength);
      }
  });

  Shiny.addCustomMessageHandler('arcs_dash_animate',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcDashAnimateTime(data.arcDashAnimateTime);
      }
  });

  Shiny.addCustomMessageHandler('arcs_dash_gap',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcDashGap(data.arcDashGap);
      }
  });

  Shiny.addCustomMessageHandler('arcs_transition',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcsTransitionDuration(data.arcsTransitionDuration);
      }
  });

  Shiny.addCustomMessageHandler('arcs_dash_initial_gap',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcDashInitialGap(data.arcDashInitialGap);
      }
  });

  Shiny.addCustomMessageHandler('arcs_dash_initial_gap',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.arcDashInitialGap(data.arcDashInitialGap);
      }
  });

  Shiny.addCustomMessageHandler('arcs_on_click',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onArcClick(data.onArcClick);
      }
  });

  Shiny.addCustomMessageHandler('arcs_on_right_click',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onArcRightClick(data.onArcRightClick);
      }
  });

  Shiny.addCustomMessageHandler('arcs_on_hover',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onArcHover(data.onArcHover);
      }
  });

  Shiny.addCustomMessageHandler('polygons_data',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.polygonsData(data.polygonsData);
      }
  });

  Shiny.addCustomMessageHandler('polygons_label',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.polygonLabel(data.polygonLabel);
      }
  });

  Shiny.addCustomMessageHandler('polygons_geometry',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.polygonGeoJsonGeometry(data.polygonGeoJsonGeometry);
      }
  });

  Shiny.addCustomMessageHandler('polygons_cap_color',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.polygonCapColor(data.polygonCapColor);
      }
  });

  Shiny.addCustomMessageHandler('polygons_side_color',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.polygonSideColor(data.polygonSideColor);
      }
  });

  Shiny.addCustomMessageHandler('polygons_altitude',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.polygonAltitude(data.polygonAltitude);
      }
  });

  Shiny.addCustomMessageHandler('polygons_transition',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.polygonsTransitionDuration(data.polygonsTransitionDuration);
      }
  });

  Shiny.addCustomMessageHandler('polygons_on_click',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onPolygonClick(data.onPolygonClick);
      }
  });

  Shiny.addCustomMessageHandler('polygons_on_right_click',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onPolygonRightClick(data.onPolygonRightClick);
      }
  });

  Shiny.addCustomMessageHandler('polygons_on_hover',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onPolygonHover(data.onPolygonHover);
      }
  });

  Shiny.addCustomMessageHandler('labels_data',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelsData(data.labelsData);
      }
  });

  Shiny.addCustomMessageHandler('labels_lon',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelLng(data.labelLng);
      }
  });

  Shiny.addCustomMessageHandler('labels_lat',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelLat(data.labelLat);
      }
  });

  Shiny.addCustomMessageHandler('labels_text',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelText(data.labelText);
      }
  });

  Shiny.addCustomMessageHandler('labels_color',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelColor(data.labelColor);
      }
  });

  Shiny.addCustomMessageHandler('labels_altitude',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelAltitude(data.labelAltitude);
      }
  });

  Shiny.addCustomMessageHandler('labels_size',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelSize(data.labelSize);
      }
  });

  Shiny.addCustomMessageHandler('labels_typeface',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelTypeFace(data.labelTypeFace);
      }
  });

  Shiny.addCustomMessageHandler('labels_rotation',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelRotation(data.labelRotation);
      }
  });

  Shiny.addCustomMessageHandler('labels_resolution',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelResolution(data.labelResolution);
      }
  });

  Shiny.addCustomMessageHandler('labels_include_dot',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelIncludeDot(data.labelIncludeDot);
      }
  });

  Shiny.addCustomMessageHandler('labels_dot_radius',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelDotRadius(data.labelDotRadius);
      }
  });

  Shiny.addCustomMessageHandler('labels_dot_orientation',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelDotOrientation(data.labelDotOrientation);
      }
  });

  Shiny.addCustomMessageHandler('labels_transition',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.labelsTransitionDuration(data.labelsTransitionDuration);
      }
  });

  Shiny.addCustomMessageHandler('labels_on_click',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onLabelClick(data.onLabelClick);
      }
  });

  Shiny.addCustomMessageHandler('labels_on_right_click',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onLabelRightClick(data.onLabelRightClick);
      }
  });

  Shiny.addCustomMessageHandler('labels_on_hover',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onLabelHover(data.onLabelHover);
      }
  });

  Shiny.addCustomMessageHandler('hex_data',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.hexBinPointsData(data.hexBinPointsData);
      }
  });

  Shiny.addCustomMessageHandler('hex_lat',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.hexBinPointLat(data.hexBinPointLat);
      }
  });

  Shiny.addCustomMessageHandler('hex_lon',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.hexBinPointLng(data.hexBinPointLng);
      }
  });

  Shiny.addCustomMessageHandler('hex_weight',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.hexBinPointWeight(data.hexBinPointWeight);
      }
  });

  Shiny.addCustomMessageHandler('hex_label',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.hexLabel(data.hexLabel);
      }
  });

  Shiny.addCustomMessageHandler('hex_resolution',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.hexBinResolution(data.hexBinResolution);
      }
  });

  Shiny.addCustomMessageHandler('hex_margin',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.hexMargin(data.hexMargin);
      }
  });

  Shiny.addCustomMessageHandler('hex_cap_color',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.hexTopColor(data.hexTopColor);
      }
  });

  Shiny.addCustomMessageHandler('hex_side_color',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.hexSideColor(data.hexSideColor);
      }
  });

  Shiny.addCustomMessageHandler('hex_merge',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.hexBinMerge(data.hexBinMerge);
      }
  });

  Shiny.addCustomMessageHandler('hex_merge',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.hexTransitionDuration(data.hexTransitionDuration);
      }
  });

  Shiny.addCustomMessageHandler('hex_transition',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.hexTransitionDuration(data.hexTransitionDuration);
      }
  });

  Shiny.addCustomMessageHandler('hex_on_click',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onHexClick(data.onHexClick);
      }
  });

  Shiny.addCustomMessageHandler('hex_on_right_click',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onHexRightClick(data.onHexRightClick);
      }
  });

  Shiny.addCustomMessageHandler('hex_on_hover',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.onHexHover(data.onHexHover);
      }
  });

  Shiny.addCustomMessageHandler('scale_bars_color',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        globe.pointColor(data.pointColor);
      }
  });

  Shiny.addCustomMessageHandler('globe_hex',
    function(data) {
      var globe = get_globe(data.id);
      if (typeof globe != 'undefined') {
        if(data.hasOwnProperty("hexBinPointsData"))
          globe.hexBinPointsData(data.hexBinPointsData);
        if(data.hasOwnProperty("hexLabel"))
          globe.hexLabel(data.hexLabel);
        if(data.hasOwnProperty("hexBinPointLat"))
          globe.hexBinPointLat(data.hexBinPointLat);
        if(data.hasOwnProperty("hexBinPointLng"))
          globe.hexBinPointLng(data.hexBinPointLng);
        if(data.hasOwnProperty("hexBinPointWeight"))
          globe.hexBinPointWeight(data.hexBinPointWeight);
        if(data.hasOwnProperty("hexBinResolution"))
          globe.hexBinResolution(data.hexBinResolution);
        if(data.hasOwnProperty("hexMargin"))
          globe.hexMargin(data.hexMargin);
        if(data.hasOwnProperty("hexAltitude"))
          globe.hexAltitude(data.hexAltitude);
        if(data.hasOwnProperty("hexTopColor"))
          globe.hexTopColor(data.hexTopColor);
        if(data.hasOwnProperty("hexSideColor"))
          globe.hexSideColor(data.hexSideColor);
        if(data.hasOwnProperty("hexBinMerge"))
          globe.hexBinMerge(data.hexBinMerge);
        if(data.hasOwnProperty("hexTransitionDuration"))
          globe.hexTransitionDuration(data.hexTransitionDuration);
        if(data.hasOwnProperty("onHexClick"))
          globe.onHexClick(data.onHexClick);
        if(data.hasOwnProperty("onHexRightClick"))
          globe.onHexRightClick(data.onHexRightClick);
        if(data.hasOwnProperty("onHexHover"))
          globe.onHexHover(data.onHexHover); 
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
        if(data.hasOwnProperty("arcStartLat"))
          globe.arcStartLat(data.arcStartLat);
        if(data.hasOwnProperty("arcStartLng"))
          globe.arcStartLng(data.arcStartLng);
        if(data.hasOwnProperty("arcEndLat"))
          globe.arcEndLat(data.arcEndLat);
        if(data.hasOwnProperty("arcEndLng"))
          globe.arcEndLng(data.arcEndLng);
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
      if(data.hasOwnProperty("labelLat"))
        globe.labelLat(data.labelLat); 
      if(data.hasOwnProperty("labelLng"))
        globe.labelLng(data.labelLng); 
      if(data.hasOwnProperty("labelText"))
        globe.labelText(data.labelText); 
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