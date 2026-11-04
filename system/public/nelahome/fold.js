// The opener's next meeting and dues, and folds that open for #anchors.
(function () {
  // script.js fills #nextMeeting on DOMContentLoaded; this listener runs after it.
  document.addEventListener("DOMContentLoaded", function () {
    var next = document.getElementById("nextMeeting");
    if (next && next.textContent) document.getElementById("openerNext").textContent = next.textContent;
  });

  // Every other Tuesday, so "every meeting" is twice the amount monthly.
  var dues = document.getElementById("dues");
  var amount = document.getElementById("duesAmount");
  function show() {
    document.getElementById("duesShown").textContent = "$" + amount.value;
    document.getElementById("duesMonthly").textContent = "($" + amount.value * 2 + "/mo)";
  }
  amount.addEventListener("input", show);
  show();
  dues.addEventListener("submit", function (e) {
    e.preventDefault();
    var monthly = e.submitter && e.submitter.value === "month";
    var url = new URL(dues.action);
    url.searchParams.set("amount", monthly ? amount.value * 2 : amount.value);
    url.searchParams.set("interval", monthly ? "month" : "oneTime");
    url.searchParams.set("redirect", "https://nela.computer/?thanks=1#support");
    url.searchParams.set("tags", "dues");
    window.location.href = url.toString();
  });

  function reveal() {
    if (!location.hash) return;
    var el = document.getElementById(decodeURIComponent(location.hash.slice(1)));
    var fold = el && el.closest("details");
    if (fold) { fold.open = true; el.scrollIntoView(); }
  }
  window.addEventListener("hashchange", reveal);
  reveal();
})();
