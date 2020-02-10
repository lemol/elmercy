const { buildApp } = require('elm-framework');

module.exports = function (bundler) {
  bundler.on('buildStart', (a,b) => {
    buildApp('.');
  })
};