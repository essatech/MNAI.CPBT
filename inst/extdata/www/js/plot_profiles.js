/*
The purpose of this demo is to demonstrate how multiple charts on the same page
can be linked through DOM and Highcharts events and API methods. It takes a
standard Highcharts config with a small variation for each data set, and a
mouse/touch event handler to bind the charts together.
*/


/**
 * In order to synchronize tooltips and crosshairs, override the
 * built-in events with handlers defined on the parent element.
 */
['mousemove', 'touchmove', 'touchstart'].forEach(function (eventType) {
  document.getElementById('container').addEventListener(
    eventType,
    function (e) {
      var chart,
        point,
        i,
        event;

      var cross_sd = 0;
	  for (i = 0; i < Highcharts.charts.length; i = i + 1) {
        chart = Highcharts.charts[i];
        // Find coordinates within the chart
        event = chart.pointer.normalize(e);
        // Get the hovered point
        point = chart.series[0].searchPoint(event, true);
        
        if (point) {
    			// High charts highlighting
    			point.highlight(e);
    		  
    		  
    			// Get cursor X postion for leaflet
    			cross_sd = point.category; 
    			//console.log(point);
    			var bearing = tran_feat.bearing[0];
    			var pt = turf.point([tran_feat.start_long[0], tran_feat.start_lat[0]], {"marker-color": "F00"});
    			var destination = turf.rhumbDestination(pt, cross_sd, bearing, {units: 'meters'});
    			var crd_move = destination.geometry.coordinates;
    			// Mover the marker
    			//wiggle_marker.setLatLng([crd_move[1], crd_move[0]]); 
    			//console.log(wiggle_line_lat_s);
    			wiggle_line_lat_s.setLatLngs([[crd_move[1], -180], [crd_move[1], 0]]);
    			wiggle_line_lng_s.setLatLngs([[0, crd_move[0]], [90, crd_move[0]]]);
          mymap.panTo(new L.LatLng(crd_move[1], crd_move[0]));

        }
      }
    }
  );
});


/**
 * Override the reset function, we don't need to hide the tooltips and
 * crosshairs.
 */
Highcharts.Pointer.prototype.reset = function () {
  return undefined;
};

/**
 * Highlight a point by showing tooltip, setting hover state and draw crosshair
 */
Highcharts.Point.prototype.highlight = function (event) {
  event = this.series.chart.pointer.normalize(event);
  this.onMouseOver(); // Show the hover marker
  this.series.chart.tooltip.refresh(this); // Show the tooltip
  this.series.chart.xAxis[0].drawCrosshair(event, this); // Show the crosshair
};

/**
 * Synchronize zooming through the setExtremes event handler.
 */
function syncExtremes(e) {
  var thisChart = this.chart;

  if (e.trigger !== 'syncExtremes') { // Prevent feedback loop
    Highcharts.each(Highcharts.charts, function (chart) {
      if (chart !== thisChart) {
        if (chart.xAxis[0].setExtremes) { // It is null while updating
          chart.xAxis[0].setExtremes(
            e.min,
            e.max,
            undefined,
            false,
            { trigger: 'syncExtremes' }
          );
        }
      }
    });
  }
}

// Get the data. The contents of the data file can be viewed at


    activity = tran_data;
    activity.datasets.forEach(function (dataset, i) {


      console.log(dataset.name);

      // Add X values
      dataset.data = Highcharts.map(dataset.data, function (val, j) {
        return [activity.xData[j], val];
      });

      var chartDiv = document.createElement('div');
      chartDiv.className = 'chart';
      document.getElementById('container').appendChild(chartDiv);

      
      function SeriesColSet(dsn){
         dsn = dsn.toString();
         console.log(dsn);
         switch(dsn) {
          case "Wave Height: Without Vegetation":
            return('#BF463F');
          case 'Wave Height: With Vegetation':
            return('#3FBF46');
          case "Elevation (Chart Datum)":
            return('#3FB8BF');
          case 'Vegetation Height':
            return('#BF853F');
          default:
            return('black');
        }
      }

      var series_color = SeriesColSet(dataset.name);
      //console.log(series_color);




      Highcharts.chart(chartDiv, {
        chart: {
          marginLeft: 40, // Keep all charts left aligned
          spacingTop: 10,
          spacingBottom: 10
        },
        title: {
          text: dataset.name,
          align: 'left',
          margin: 0,
          x: 30
        },
        credits: {
          enabled: false
        },
        legend: {
          enabled: false
        },
        xAxis: {
          crosshair: true,
          events: {
            setExtremes: syncExtremes
          },
          labels: {
            format: '{value} m'
          }
        },
        yAxis: {
          title: {
            text: null
          }
        },
        tooltip: {
          positioner: function () {
            return {
              // right aligned
              x: this.chart.chartWidth - this.label.width,
              y: 10 // align to title
            };
          },
          borderWidth: 0,
          backgroundColor: 'none',
          pointFormat: '{point.y}',
          headerFormat: '',
          shadow: false,
          style: {
            fontSize: '18px'
          },
          valueDecimals: dataset.valueDecimals
        },
        series: [{
          data: dataset.data,
          name: dataset.name,
          type: dataset.type,
          color: series_color,
          fillOpacity: 0.3,
          tooltip: {
            valueSuffix: ' ' + dataset.unit
          }
        }
        ]
      });
    });
  

