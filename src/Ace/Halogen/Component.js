"use strict";

exports.completeFns = { value: {} };

exports.initialized = { value: false };

exports.focused = { value: "" };

exports.dataset = function (node) {
  return function () {
    return node.dataset;
  };
};
