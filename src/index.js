require('style.scss');
require('register-sw.js');

var Elm = require('Main');
var app = Elm.Main.fullscreen();

window.addEventListener("load", () => {
    document.querySelector(".loader").remove()
})
