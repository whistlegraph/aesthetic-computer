// Dues, and folds that open for #anchors.
(function () {
  var dues = document.getElementById("dues");
  var amount = document.getElementById("duesAmount");
  function show() {
    document.getElementById("duesShown").textContent = "$" + amount.value;
  }
  amount.addEventListener("input", show);
  show();
  dues.addEventListener("submit", function (e) {
    e.preventDefault();
    var monthly = e.submitter && e.submitter.value === "month";
    var url = new URL(dues.action);
    url.searchParams.set("amount", amount.value);
    url.searchParams.set("interval", monthly ? "month" : "oneTime");
    url.searchParams.set("redirect", "https://nela.computer/?thanks=1#support");
    url.searchParams.set("tags", "dues");
    window.location.href = url.toString();
  });

  // Past meetings from script.js's EVENTS, newest first; special ones get their page.
  var SPECIAL = { "2026-10-27": ["Reset In Peace", "https://nela.computer/reset/"] };
  var today = new Date(); today.setHours(0, 0, 0, 0);
  var past = document.getElementById("pastList");
  if (past && typeof EVENTS !== "undefined") {
    EVENTS.filter(function (e) {
      var p = e.split("-").map(Number);
      return new Date(p[0], p[1] - 1, p[2]) < today;
    }).reverse().forEach(function (e) {
      var p = e.split("-").map(Number);
      var li = document.createElement("li");
      li.textContent = new Date(p[0], p[1] - 1, p[2]).toLocaleDateString("en-US", { month: "short", day: "numeric", year: "numeric" });
      var special = SPECIAL[e];
      if (special) {
        var a = document.createElement("a");
        a.href = special[1]; a.textContent = special[0];
        li.append(" \u00b7 ", a);
      }
      past.appendChild(li);
    });
  }

  function reveal() {
    if (!location.hash) return;
    var el = document.getElementById(decodeURIComponent(location.hash.slice(1)));
    var fold = el && el.closest("details");
    if (fold) { fold.open = true; el.scrollIntoView(); }
  }
  window.addEventListener("hashchange", reveal);
  reveal();
})();
