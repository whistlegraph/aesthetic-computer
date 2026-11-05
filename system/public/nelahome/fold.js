// Amount shortcuts, and folds that open for #anchors.
(function () {
  var dues = document.getElementById("dues");
  var amount = document.getElementById("duesAmount");
  var presets = document.getElementById("duesPresets");
  var buttons = presets.querySelectorAll("button[data-amount]");
  function show() {
    buttons.forEach(function (button) {
      button.setAttribute("aria-pressed", String(Number(button.dataset.amount) === amount.valueAsNumber));
    });
    var valid = amount.validity.valid;
    var price = valid ? "$" + Number(amount.value).toLocaleString("en-US", { maximumFractionDigits: 2 }) : "";
    dues.querySelector('[value="oneTime"]').textContent = valid ? "> " + price + " once_" : "> Once_";
    dues.querySelector('[value="month"]').textContent = valid ? "> " + price + " / month_" : "> Monthly_";
  }
  buttons.forEach(function (button) {
    button.addEventListener("click", function () {
      amount.value = button.dataset.amount;
      show();
    });
  });
  presets.hidden = false;
  amount.addEventListener("input", show);
  show();

  // Past meetings from script.js's EVENTS as little month calendars, newest
  // first: a filled box per meeting, colored by where we met.
  var PLOT = { name: "Plot", href: "https://plot.place", kind: "plot" };
  var SPECIAL = {
    "2026-10-27": { name: "Reset In Peace, Echo Park", href: "https://nela.computer/reset/", kind: "event" }
  };
  var today = new Date(); today.setHours(0, 0, 0, 0);
  var cal = document.getElementById("pastList");
  if (cal && typeof EVENTS !== "undefined") {
    var met = {}, places = {};
    EVENTS.forEach(function (e) {
      var p = e.split("-").map(Number);
      if (new Date(p[0], p[1] - 1, p[2]) < today) {
        met[e] = SPECIAL[e] || PLOT;
        places[met[e].name] = met[e];
      }
    });
    var days = Object.keys(met).sort();
    if (days.length) {
      var months = document.createElement("div");
      months.className = "pastMonths";
      var first = days[0].split("-").map(Number), last = days[days.length - 1].split("-").map(Number);
      for (var y = last[0], m = last[1] - 1; y > first[0] || (y === first[0] && m >= first[1] - 1); m < 1 ? (y--, m = 11) : m--) {
        var month = document.createElement("table");
        month.createCaption().textContent = new Date(y, m, 1).toLocaleDateString("en-US", { month: "short", year: "numeric" });
        var row = month.insertRow();
        row.className = "dow";
        "SMTWTFS".split("").forEach(function (d) { row.insertCell().textContent = d; });
        var lead = new Date(y, m, 1).getDay(), count = new Date(y, m + 1, 0).getDate();
        for (var i = 0; i < lead + count; i++) {
          if (i % 7 === 0) row = month.insertRow();
          var cell = row.insertCell();
          if (i < lead) continue;
          var d = i - lead + 1;
          var key = y + "-" + String(m + 1).padStart(2, "0") + "-" + String(d).padStart(2, "0");
          var place = met[key];
          var box = document.createElement(place ? "a" : "span");
          box.className = "box" + (place ? " met " + place.kind : "");
          if (place) {
            box.href = place.href;
            box.title = new Date(y, m, d).toLocaleDateString("en-US", { month: "short", day: "numeric" }) + " \u00b7 " + place.name;
          }
          cell.appendChild(box);
        }
        months.appendChild(month);
      }
      cal.appendChild(months);
      var key = document.createElement("p");
      key.className = "pastKey";
      Object.keys(places).forEach(function (n) {
        var item = document.createElement("a");
        item.href = places[n].href;
        item.innerHTML = '<span class="box met ' + places[n].kind + '"></span> ';
        item.append(n);
        key.appendChild(item);
      });
      cal.appendChild(key);
    }
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
