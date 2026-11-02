// nela bells sampler board. The clip store runs on Aesthetic Computer for now;
// the page is plain HTML and can move anywhere. No framework, no build.
//
// An entry can be made without audio. The server hands back a code and a key;
// they are kept in this browser so the same person can come back and attach
// their clip later. Lose the browser, and the clip can still be sent in chat.
(function () {
  var API = "https://aesthetic.computer/api/nelabel-drop";
  var STORE = "nela-bells-mine";
  var form = document.getElementById("drop");
  var clip = document.getElementById("clip");
  var choose = clip.closest(".bel-choose");
  var clipName = document.getElementById("clipName");
  var clipHint = clipName.textContent;
  var send = document.getElementById("send");
  var bar = document.getElementById("bar");
  var status = document.getElementById("status");
  var list = document.getElementById("list");
  var empty = document.getElementById("empty");
  var wordField = document.getElementById("wordField");
  var mine = document.getElementById("mine");
  var mineList = document.getElementById("mineList");

  // The link can carry #word to show the club-word field.
  if (location.hash === "#word") wordField.hidden = false;

  function say(message, tone, el) {
    el = el || status;
    el.textContent = message || "";
    el.className = "bel-status" + (tone ? " " + tone : "");
  }

  function meter(fraction, el) {
    el = el || bar;
    var cells = 24, on = Math.round(fraction * cells);
    el.textContent = "[" + "█".repeat(on) + "░".repeat(cells - on) + "] " + Math.round(fraction * 100) + "%";
  }

  function fmtWhen(iso) {
    var d = new Date(iso);
    return isNaN(d) ? "" : d.toLocaleDateString(undefined, { month: "short", day: "numeric" });
  }

  // Entries made from this browser: [{ code, key, title, clip }].
  function remembered() {
    try { return JSON.parse(localStorage.getItem(STORE) || "[]"); } catch (e) { return []; }
  }
  function remember(entries) {
    try { localStorage.setItem(STORE, JSON.stringify(entries)); } catch (e) {}
  }

  function render(drops) {
    list.textContent = "";
    empty.hidden = drops.length > 0;
    var known = {};
    drops.forEach(function (d) { known[d.code] = d; });
    drops.forEach(function (d, i) {
      var li = document.createElement("li");
      var n = document.createElement("span"); n.className = "n"; n.textContent = String(i + 1).padStart(2, "0");
      var t = document.createElement("span");
      t.textContent = d.title + " ";
      var who = document.createElement("span"); who.className = "who"; who.textContent = "· " + d.who;
      t.appendChild(who);
      if (d.url) {
        var a = document.createElement("a"); a.href = d.url; a.textContent = " [ " + (d.file || "clip") + " ]";
        t.appendChild(a);
      }
      var len = document.createElement("span"); len.className = "len";
      len.textContent = (d.clip ? "♪ " : "") + (d.length || fmtWhen(d.when));
      li.appendChild(n); li.appendChild(t); li.appendChild(len);
      list.appendChild(li);
    });
    // Keep the remembered entries in step with the board.
    var entries = remembered().filter(function (e) { return known[e.code]; });
    entries.forEach(function (e) { e.clip = !!known[e.code].clip; e.title = known[e.code].title; });
    remember(entries);
    renderMine(entries);
  }

  function renderMine(entries) {
    var waiting = entries.filter(function (e) { return !e.clip; });
    mine.hidden = waiting.length === 0;
    mineList.textContent = "";
    waiting.forEach(function (e) {
      var li = document.createElement("li");
      var title = document.createElement("span"); title.className = "who"; title.textContent = e.title;
      var pick = document.createElement("label"); pick.className = "bel-choose";
      var input = document.createElement("input"); input.type = "file"; input.accept = clip.accept;
      var span = document.createElement("span"); span.textContent = "[ Attach your clip ]";
      pick.appendChild(input); pick.appendChild(span);
      var meterEl = document.createElement("pre"); meterEl.className = "bel-bar"; meterEl.hidden = true;
      var statusEl = document.createElement("p"); statusEl.className = "bel-status";
      li.appendChild(title); li.appendChild(document.createTextNode(" ")); li.appendChild(pick);
      li.appendChild(meterEl); li.appendChild(statusEl);
      input.addEventListener("change", function () {
        var file = input.files && input.files[0];
        if (!file) return;
        pick.classList.add("has"); span.textContent = "[ " + file.name + " ]";
        meterEl.hidden = false; meter(0, meterEl);
        say("> Sending the clip…", null, statusEl);
        post({ action: "attach", code: e.code, key: e.key, file: { name: file.name, bytes: file.size } })
          .then(function (j) { return putWithProgress(j.upload, file, meterEl); })
          .then(function () { say("> Making sure it landed…", null, statusEl); return post({ action: "commit", code: e.code }); })
          .then(function () { meter(1, meterEl); say("> It's on the board.", "good", statusEl); load(); })
          .catch(function (err) { say("> " + err.message, "bad", statusEl); pick.classList.remove("has"); span.textContent = "[ Attach your clip ]"; });
      });
      mineList.appendChild(li);
    });
  }

  function load() {
    fetch(API)
      .then(function (r) { return r.json(); })
      .then(function (data) { render(data.drops || []); })
      .catch(function () { empty.hidden = false; empty.textContent = "Couldn't load the board."; });
  }

  clip.addEventListener("change", function () {
    var f = clip.files && clip.files[0];
    choose.classList.toggle("has", !!f);
    clipName.textContent = f ? f.name + " · " + (f.size / 1024 / 1024).toFixed(1) + " MB" : clipHint;
  });

  function putWithProgress(upload, file, meterEl) {
    return new Promise(function (resolve, reject) {
      var xhr = new XMLHttpRequest();
      xhr.open(upload.method, upload.url);
      Object.keys(upload.headers || {}).forEach(function (k) { xhr.setRequestHeader(k, upload.headers[k]); });
      xhr.upload.onprogress = function (e) { if (e.lengthComputable) meter(e.loaded / e.total, meterEl); };
      xhr.onload = function () { (xhr.status >= 200 && xhr.status < 300) ? resolve() : reject(new Error("Upload failed (" + xhr.status + ")")); };
      xhr.onerror = function () { reject(new Error("Upload failed.")); };
      xhr.send(file);
    });
  }

  function post(body) {
    return fetch(API, { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) })
      .then(function (r) { return r.json().then(function (j) { if (!r.ok) throw new Error(j.error || "Something went wrong."); return j; }); });
  }

  form.addEventListener("submit", function (e) {
    e.preventDefault();
    var file = clip.files && clip.files[0];
    var body = {
      action: "begin",
      who: form.who.value, title: form.title.value, length: form.length.value,
      eta: form.eta.value, note: form.note.value, word: form.word.value,
      file: file ? { name: file.name, bytes: file.size } : null
    };
    if (!body.who.trim()) return say("> Who are you?", "bad");
    if (!body.title.trim()) return say("> Give the track a working title.", "bad");
    if (!body.eta.trim()) return say("> Say roughly when you could finish it.", "bad");

    send.disabled = true;
    say("> Putting you on the board…");
    var entry;
    post(body)
      .then(function (j) {
        entry = { code: j.code, key: j.key, title: body.title.trim(), clip: false };
        remember(remembered().concat([entry]));
        if (!j.upload) return;
        bar.hidden = false; meter(0);
        say("> Sending the clip…");
        return putWithProgress(j.upload, file)
          .then(function () { say("> Making sure it landed…"); return post({ action: "commit", code: j.code }); })
          .then(function () { meter(1); entry.clip = true; });
      })
      .then(function () {
        say(entry.clip ? "> You're on the board, clip and all." : "> You're on the board. Come back here for the clip.", "good");
        form.reset(); clip.dispatchEvent(new Event("change"));
        load();
      })
      .catch(function (err) { say("> " + err.message, "bad"); })
      .then(function () { send.disabled = false; setTimeout(function () { bar.hidden = true; }, 1500); });
  });

  load();
})();
