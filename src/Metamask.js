//requires web3 js and truffle-contract js
"use strict";

exports.web3DefinedImpl = function(unit) {
    return function() {
        if ( !web3 )
            return false;
        else
            return true;
    };
};

exports.checkStatusImpl = function(dummyVal) {
    return function() {
        //check whether metamask is logged in
        if (web3.eth.accounts[0] == undefined) {
            return false;
        }
        else {
            return true;
        }
    };
};

exports.currentUserImpl = function(dummyVal) {
    return function() {
        return web3.eth.accounts[0];
    };
};

exports.getNetworkImpl = function(callback) {
    return function() {
        web3.version.getNetwork(function(err, netId) {
            if ( !err )
                callback(netId)();
            else
                callback(err)();
        });
    };
};

exports.checkTxStatusImpl = function(callback) {
    return function(txHash) {
        return function() {
            web3.eth.getTransaction(txHash, function(err, result) {
                if ( err ) {
                    console.log(err);
                    console.log("got a network error");
                    callback("NetworkError")();
                }
                else {
                    callback(txResultToRaw(result))();
                }
            });
        };
    };
};

var txResultToRaw = function(txResult) {
    if ( txResult == null)
        return "Pending";
    else if ( txResult.blockNumber == null )
        return "Pending";
    else
        return "Done";
    return "BadTx";
};


    /*
Transaction result reference: use this

//bad
{blockHash: "0x0000000000000000000000000000000000000000000000000000000000000000", blockNumber: null, from: "0x406dd5315e6b63d6f1bad0c4ab9cd8eba6bb1bd2", gas: 109327, gasPrice: e, …}

//good
{blockHash: "0x6e37786ca1d9f803d5ca0fef34707dca912c5c0e2c12d5f9be4b32264dc36ca2", blockNumber: 1456258, from: "0x406dd5315e6b63d


    */
