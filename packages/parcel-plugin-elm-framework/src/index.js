const { buildApp } = require("elmercy");

module.exports = function(bundler) {
  bundler.on("buildStart", (a, b) => {
    buildApp(".");
  });
};
