/* global exports, require */
/* jshint -W097 */

"use strict";

export const reverseCodeUnits = function(s) {
  var i = s.length, result = "";

  while (i) {
    result += s[--i];
  }

  return result;
};
