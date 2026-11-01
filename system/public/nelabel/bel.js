// nela bells sampler drop. The clip store runs on Aesthetic Computer for now;
// the page is plain HTML and can move anywhere. No framework, no build.
(function () {
  var API = "https://aesthetic.computer/api/nelabel-drop";
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

  // The link can carry #word to show the club-word field.
  if (location.hash === "#word") wordField.hidden = false;

  function say(message, tone) {
    status.textContent = message || "";
    status.className = "bel-status" + (tone ? " " + tone : "");
  }

  function meter(fraction) {
    var cells = 24, on = Math.round(fraction * cells);
    bar.textContent = "[" + "█".repeat(on) + "░".repeat(cells - on) + "] " + Math.round(fraction * 100) + "%";
  }

  function fmtWhen(iso) {
    var d = new Date(iso);
    return isNaN(d) ? "" : d.toLocaleDateString(undefined, { month: "short", day: "numeric" });
  }

  function render(drops) {
    list.textContent = "";
    empty.hidden = drops.length > 0;
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
      len.textContent = d.length || fmtWhen(d.when);
      li.appendChild(n); li.appendChild(t); li.appendChild(len);
      list.appendChild(li);
    });
  }

  function load() {
    fetch(API)
      .then(function (r) { return r.json(); })
      .then(function (data) { render(data.drops || []); })
      .catch(function () { empty.hidden = false; empty.textContent = "Couldn't load the list."; });
  }

  clip.addEventListener("change", function () {
    var f = clip.files && clip.files[0];
    choose.classList.toggle("has", !!f);
    clipName.textContent = f ? f.name + " · " + (f.size / 1024 / 1024).toFixed(1) + " MB" : clipHint;
  });

  function putWithProgress(upload, file) {
    return new Promise(function (resolve, reject) {
      var xhr = new XMLHttpRequest();
      xhr.open(upload.method, upload.url);
      Object.keys(upload.headers || {}).forEach(function (k) { xhr.setRequestHeader(k, upload.headers[k]); });
      xhr.upload.onprogress = function (e) { if (e.lengthComputable) meter(e.loaded / e.total); };
      xhr.onload = function () { (xhr.status >= 200 && xhr.status < 300) ? resolve() : reject(new Error("Upload failed (" + xhr.status + ")")); };
      xhr.onerror = function () { reject(new Error("Upload failed")); };
      xhr.send(file);
    });
  }

  function post(body) {
    return fetch(API, { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) })
      .then(function (r) { return r.json().then(function (j) { if (!r.ok) throw new Error(j.error || "Something went wrong"); return j; }); });
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
    if (!body.who.trim()) return say("> Tell us who you are.", "bad");
    if (!body.title.trim()) return say("> Give the track a working title.", "bad");
    if (!body.eta.trim()) return say("> Say roughly when you could hand in a finished version.", "bad");
    if (!file) return say("> Choose a clip.", "bad");

    send.disabled = true; bar.hidden = false; meter(0);
    say("> Asking for a slot…");
    var code;
    post(body)
      .then(function (j) { code = j.code; say("> Sending the clip…"); return putWithProgress(j.upload, file); })
      .then(function () { say("> Checking it landed…"); return post({ action: "commit", code: code }); })
      .then(function () {
        meter(1);
        say("> You're on the sampler. Thank you.", "good");
        form.reset(); clip.dispatchEvent(new Event("change"));
        load();
      })
      .catch(function (err) { say("> " + err.message, "bad"); })
      .then(function () { send.disabled = false; setTimeout(function () { bar.hidden = true; }, 1500); });
  });

  load();
})();
