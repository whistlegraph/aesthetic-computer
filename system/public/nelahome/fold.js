// The opener's next meeting and gift count, and folds that open for #anchors.
(function () {
  // script.js fills #nextMeeting on DOMContentLoaded; this listener runs after it.
  document.addEventListener("DOMContentLoaded", function () {
    var next = document.getElementById("nextMeeting");
    if (next && next.textContent) document.getElementById("openerNext").textContent = next.textContent;
  });

  // How many gifts the club has had, from its public Open Collective ledger.
  fetch("https://api.opencollective.com/graphql/v2", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ query: 'query { project(slug: "nelacc") { transactions(type: CREDIT, kind: CONTRIBUTION, limit: 1) { totalCount } } }' })
  })
    .then(function (r) { return r.json(); })
    .then(function (d) {
      var n = d && d.data && d.data.project && d.data.project.transactions.totalCount;
      if (n) document.getElementById("openerGifts").textContent = n + (n === 1 ? " gift" : " gifts") + " so far. Support NCC";
    })
    .catch(function () {});

  function reveal() {
    if (!location.hash) return;
    var el = document.getElementById(decodeURIComponent(location.hash.slice(1)));
    var fold = el && el.closest("details");
    if (fold) { fold.open = true; el.scrollIntoView(); }
  }
  window.addEventListener("hashchange", reveal);
  reveal();
})();
