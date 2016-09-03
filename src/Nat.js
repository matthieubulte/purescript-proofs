
exports.mkExistsNat = function(dictNat) {
    return function(prop) {
        return { dictNat: dictNat, prop: prop };
    };
};

exports.runExistsNat = function(consumer) {
    return function(existsNat) {
        return consumer(existsNat.dictNat)(existsNat.prop);
    };
};
