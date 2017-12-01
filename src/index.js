require('style.scss');
require('register-sw.js');


var Elm = require('Main');
var app = Elm.Main.fullscreen();

window.addEventListener("load", function() {
    document.querySelector(".notification").remove();
})
