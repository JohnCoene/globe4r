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

      },

      getGlobe: function(){
        return globe;
      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

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

}