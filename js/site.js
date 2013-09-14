var times = [
  1000,
  60000,
  3600000,
  86400000,
  604800000,
  2629743830,
  31556900000
];

var labels = [
  "second",
  "minute",
  "hour",
  "day",
  "week",
  "month",
  "year"
];

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

function cardinal(n) {
  return n >= nums.length ? String(n) : nums[n];
}

setInterval(function updateDates() {
  var timeElements = document.getElementsByTagName("time");

  for (var i = 0, l = timeElements.length; i < l; ++i) {
    var timeEl = timeElements[i];
    var ds = timeEl.getAttribute("datetime");
    timeEl.textContent = timeAgo(new Date(ds));
  }

  return updateDates;
}(), 10000);
