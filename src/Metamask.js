//requires web3 js and truffle-contract js
"use strict";

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
    if ( txResult == null )
        return "Pending";
    else
        return "Done";
    return "BadTx";
};
