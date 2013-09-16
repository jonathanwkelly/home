/** @type {Array.<number>} */
var times = [
  1000,
  60000,
  3600000,
  86400000,
  604800000,
  2629743830,
  31556900000
];

/** @type {Array.<string>} */
var labels = [
  "second",
  "minute",
  "hour",
  "day",
  "week",
  "month",
  "year"
];

/**
 *  @param  {number}  t
 *  @return {string}
 */
function timeAgo(t) {
  var rem = Date.now() - t;
  var abs = Math.abs(rem);
  var idx = times.length;
  var suf = rem < 0 ? " from now" : " ago";
  var num = 0;

  if (abs < 1000) {
    return "now";
  }

  while (idx--) {
    num = Math.round(abs / times[idx]);

    if (num) {
      return cardinal(num) + " " + labels[idx] + (num > 1 ? "s" : "") + suf;
    }
  }

  return "";
}

/** @type {Array.<string>} */
var nums = [
  "zero",
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine"
];

/**
 *  @param  {number}  n
 *  @return {string}
 */
function cardinal(n) {
  return n < nums.length ? nums[n] : String(n);
}

/** @type {NodeList.<Node>} */
var timeElements = document.getElementsByTagName("time");

setInterval(function updateDates() {
  for (var i = 0, l = timeElements.length; i < l; ++i) {
    var timeEl = timeElements[i];
    var dateString = timeEl.getAttribute("datetime");
    timeEl.textContent = timeAgo(new Date(dateString));
  }

  return updateDates;
}(), 10000);
