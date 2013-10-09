/** @type {Array.<number>} */
var times = [
  60000,
  3600000,
  86400000,
  604800000,
  2629743830,
  31556900000
];

/** @type {Array.<string>} */
var timeLabels = [
  "minute",
  "hour",
  "day",
  "week",
  "month",
  "year"
];

/** @type {Array.<string>} */
var cardinalStrings = [
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

/** @type {NodeList.<Node>} */
var timeElements = document.getElementsByTagName("time");

/**
 *  @param  {number}  time
 *  @return {string}
 */
function timeAgo(time) {
  var remainder = Date.now() - time;
  var absoluteValue = Math.abs(remainder);
  var index = times.length;
  var suffix = remainder < 0 ? " from now" : " ago";
  var count = 0;

  if (absoluteValue < 60000) {
    return "now";
  }

  while (index--) {
    count = Math.round(absoluteValue / times[index]);

    if (count >= 1) {
      return cardinal(count) + " " + timeLabels[index] + (count > 1 ? "s" : "") + suffix;
    }
  }

  return new Date(time).toLocaleString();
}

/**
 *  @param  {number}  n
 *  @return {string}
 */
function cardinal(n) {
  return n < cardinalStrings.length ? cardinalStrings[n] : n.toString();
}

function updateDates() {
  for (var i = 0, l = timeElements.length; i < l; ++i) {
    var timeEl = timeElements[i];
    var dateString = timeEl.getAttribute("datetime");
    timeEl.textContent = timeAgo(Date.parse(dateString));
  }
}

updateDates();

setInterval(updateDates, 60000);
