// module Ace.Halogen.Component

exports.completeFns = {value: {}};

exports.initialized = {value: false};

exports.focused = {value: ""};

exports.keys = {value: []};

exports.dataset = function(node) {
    return function() {
        return node.dataset;
    };
};
