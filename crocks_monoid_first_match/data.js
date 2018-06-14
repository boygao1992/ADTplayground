const { test } = require( 'ramda' )

// Credit Card Types
module.exports = [
  [ 'Diners - Carte Blanche|diners', test( /^30[0-5]/ ) ],
  [ 'Diners|diners', test( /^30[6-9]|36|38/ ) ],
  [ 'JCB|jcb', test( /^35(2[89]|[3-8][0-9])/ ) ],
  [ 'AMEX|american-express', test( /^3[47]/ ) ],
  [ 'Visa Electron|visa', test( /4026|417500|4508|4844|491(3|7)/ ) ],
  [ 'Visa|visa', test( /^4/ ) ],
  [ 'Mastercard|master-card', test( /^5[1-5]/ ) ],
  [ 'Discover|discover', test(
    /6011|622(12[6-9]|1[3-9][0-9][2-8][0-9]{2}|9[0-1][0-9]|92[0-5]|64[4-9]|65)/
  ) ],
]
