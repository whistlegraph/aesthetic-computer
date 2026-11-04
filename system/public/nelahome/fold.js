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

  function reveal() {
    if (!location.hash) return;
    var el = document.getElementById(decodeURIComponent(location.hash.slice(1)));
    var fold = el && el.closest("details");
    if (fold) { fold.open = true; el.scrollIntoView(); }
  }
  window.addEventListener("hashchange", reveal);
  reveal();
})();
