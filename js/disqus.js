/* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
var disqus_shortname = "jonaswesterlund"; // required: replace example with your forum shortname

// For Closure Compiler
window["disqus_shortname"] = disqus_shortname;

// The following are highly recommended additional parameters. Remove the slashes in front to use.
// var disqus_identifier = 'unique_dynamic_id_1234';
// var disqus_url = 'http://example.com/permalink-to-page.html';

/* * * DON'T EDIT BELOW THIS LINE * * */
(function() {
  var dsq = document.createElement("script");
//  dsq.type = "text/javascript";
  dsq.async = true;
  dsq.src = "//" + disqus_shortname + ".disqus.com/embed.js?https";
//  (document.getElementsByTagName("head")[0] || document.getElementsByTagName("body")[0]).appendChild(dsq);
  document.body.appendChild(dsq);
})();
