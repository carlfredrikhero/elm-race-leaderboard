'use strict';

require('./app.scss')

// Require index.html so it gets copied to dist
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// .embed() can take an optional second argument.
// This would be an object describing the data we need to start a program,
// i.e.a userID or some token

var token = localStorage.getItem('token')

var app = Elm.Main.embed(mountNode, {
  token: token
});

app.ports.saveToken.subscribe(function (token) {
  localStorage.setItem('token', token)
})

app.ports.removeToken.subscribe(function () {
  localStorage.removeItem("token")
})
