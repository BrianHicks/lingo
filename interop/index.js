'use strict';

require('./index.html');
require('../style/main.scss');

var Elm = require('./../src/Main');

Elm.embed(Elm.Main, document.getElementById('main'));
