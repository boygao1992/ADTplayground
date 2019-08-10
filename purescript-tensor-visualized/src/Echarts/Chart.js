"use strict";

var echarts;
if ( typeof process !== 'undefined' &&
  process.versions != null &&
  process.versions.node != null ) {
  echarts = require( "echarts" );
} else {
  echarts = window.echarts;
}

exports._init = function ( dom ) {
  return echarts.init( dom );
}

exports._setOption = function ( chart, option ) {
  chart.setOption( option );
}

exports._debugChart = function ( chart ) {
  return chart
}
