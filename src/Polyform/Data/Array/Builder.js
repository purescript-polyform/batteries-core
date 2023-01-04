/* global exports, require */
/* jshint -W097 */
"use strict";

export const unsafeCons = function(a) {
  return function(arr) {
    arr.unshift(a);
    return arr;
  };
};

export const unsafeSnoc = function(a) {
  return function(arr) {
    arr.push(a);
    return arr;
  };
};
