/* globals exports, JSON */

'use strict';

exports.decodeURIComponentImpl = function(s) {
  try {
    return decodeURIComponent(s);
  } catch(error) {
    return null;
  }
};
