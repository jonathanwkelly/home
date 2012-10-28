var timeFormats = [
  [2,           "second",       1],
  [60,          "seconds",      1],
  [120,         "1 minute ago", "1 minute from now"],
  [3600,        "minutes",      60],
  [7200,        "1 hour ago",   "1 hour from now"],
  [86400,       "hours",        3600],
  [172800,      "Yesterday",    "Tomorrow"],
  [604800,      "days",         86400],
  [1209600,     "Last week",    "Next week"],
  [2419200,     "weeks",        604800],
  [4838400,     "Last month",   "Next month"],
  [29030400,    "months",       2419200],
  [58060800,    "Last year",    "Next year"],
  [2903040000,  "years",        29030400],
  [5806080000,  "Last century", "Next century"],
  [58060800000, "centuries",    2903040000]
];

function prettyDate(ds) {
  var time        = ds.replace(/-/g, "/").replace(/[TZ]/g, " ");
  var seconds     = (Date.now() - new Date(time)) / 1000;
  var token       = "ago";
  var listChoice  = 1;
  if (seconds < 0) {
    seconds     = Math.abs(seconds);
    token       = "from now";
    listChoice  = 2;
  }
  var i = 0;
  var format;
  while ((format = timeFormats[i++]))
    if (seconds < format[0]) {
      if (typeof format[2] === "string")
        return format[listChoice];
      else
        return Math.floor(seconds / format[2]) + " " + format[1] + " " + token;
    }
  return time;
}

setInterval(function updateDates() {
  var timeElements = document.getElementsByTagName("time");
  for (var i = 0, l = timeElements.length; i < l; ++i)
    timeElements[i].textContent = prettyDate(timeElements[i].getAttribute("datetime"));
  return updateDates;
}(), 10000);

var _gaq = [["_setAccount","UA-23004920-1"], ["_trackPageview"]];
(function(d, t) {
  var g = d.createElement(t), s = d.getElementsByTagName(t)[0];
  g.async = 1;
  g.src = ("https:" === location.protocol ? "//ssl" : "//www") + ".google-analytics.com/ga.js";
  s.parentNode.insertBefore(g, s);
}(document, "script"));
