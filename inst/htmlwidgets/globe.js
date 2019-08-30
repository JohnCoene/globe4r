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

        if(x.hasOwnProperty("globeImageUrl"))
          globe.globeImageUrl(x.globeImageUrl);

        if(x.hasOwnProperty("bumpImageUrl"))
          globe.bumpImageUrl(x.bumpImageUrl);

        if(x.hasOwnProperty("showAtmosphere"))
          globe.showAtmosphere(x.showAtmosphere);

        if(x.hasOwnProperty("showGraticules"))
          globe.showGraticules(x.showGraticules);

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

}