require('style.scss');
require('register-sw.js');

var Elm = require('Main');
var node = document.getElementById('main')
var app = Elm.Main.embed(node);
