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

        if(x.hasOwnProperty("width"))
          globe.width(x.width);

        if(x.hasOwnProperty("height"))
          globe.height(x.height);


        if(x.hasOwnProperty("backgroundColor"))
          globe.backgroundColor(x.backgroundColor);
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

}