'use strict';

require('./index.html');
var Elm = require('./../src/Main');

Elm.embed(Elm.Main, document.getElementById('main'));
