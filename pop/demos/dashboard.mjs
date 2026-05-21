// dashboard.mjs — songs + instruments sections for pop/demos.html.
// Browser-only. Fetches ./demos/manifest.json and renders two sections
// above the tool-demo masonry. No Node imports.

// ── accent colours (match tool-demo CATS) ────────────────────────────
const LANE_ACCENT = {
  dance:       "#c0631a",
  hippyhayzard:"#c0631a",
  marimba:     "#8a5223",
  gradus:      "#5a6b1f",
  "big-pictures": "#1d7a8a",
  chillwave:   "#1d7a8a",
  jungle:      "#5a6b1f",
  gecs:        "#b5331f",
  hellsine:    "#b5331f",
};
const DEFAULT_ACCENT = "#1a1712";

function laneAccent(lane) { return LANE_ACCENT[lane] || DEFAULT_ACCENT; }

// ── status badge ─────────────────────────────────────────────────────
const STATUS_COLOR = {
  released: "#3f7d2e",
  render:   "#c0631a",
  wip:      "#6b6353",
};

function badge(status) {
  const d = document.createElement("span");
  d.className = "db-badge";
  d.textContent = status;
  d.style.setProperty("--badge", STATUS_COLOR[status] || "#6b6353");
  return d;
}

// ── tiny DOM helpers ──────────────────────────────────────────────────
function el(tag, cls, ...kids) {
  const n = document.createElement(tag);
  if (cls) n.className = cls;
  for (const k of kids) {
    if (k == null) continue;
    n.append(typeof k === "string" ? document.createTextNode(k) : k);
  }
  return n;
}
// ── audio player with fallback ────────────────────────────────────────
function audioEl(src) {
  if (!src) {
    const p = el("p", "db-pending", "no preview yet");
    return p;
  }
  const a = document.createElement("audio");
  a.controls = true;
  a.preload = "none";
  a.src = src;
  const wrap = el("div", "db-audio-wrap", a);
  const note = el("p", "db-pending", "sample pending");
  note.style.display = "none";
  a.onerror = () => {
    a.style.display = "none";
    note.style.display = "";
  };
  wrap.append(note);
  return wrap;
}

// ── songs section — a jukebox ─────────────────────────────────────────
// One dense clickable row per track + a single shared player. Clicking a
// row plays it; tracks auto-advance to the next previewable one.
function renderSongs(songs, container) {
  const box = el("div", "db-jukebox");

  // shared "now playing" bar (sticky)
  const audio = document.createElement("audio");
  audio.controls = true;
  audio.preload = "none";
  const nowLabel = el("span", "db-now-label", "pick a track ↓");
  const nowBar = el("div", "db-jukebox-now",
    el("span", "db-now-tag", "now playing"), nowLabel, audio);

  const list = el("div", "db-jukebox-list");
  const rows = [];
  let current = -1;

  const glyphOf = (i) => rows[i] && rows[i].querySelector(".db-jb-glyph");
  const playIndex = (i) => {
    const s = songs[i];
    if (!s || !s.audio) return;
    if (current >= 0 && rows[current]) {
      rows[current].classList.remove("playing");
      const g = glyphOf(current); if (g) g.textContent = "▸";
    }
    current = i;
    rows[i].classList.add("playing");
    nowLabel.textContent = s.title + (s.duration ? "  ·  " + s.duration : "");
    audio.src = s.audio;
    audio.play().catch(() => {});
  };

  audio.addEventListener("ended", () => {
    for (let j = current + 1; j < songs.length; j++) {
      if (songs[j].audio) { playIndex(j); return; }
    }
  });
  audio.addEventListener("play",  () => { const g = glyphOf(current); if (g) g.textContent = "♪"; });
  audio.addEventListener("pause", () => { const g = glyphOf(current); if (g) g.textContent = "▸"; });

  songs.forEach((s, i) => {
    const accent = laneAccent(s.lane);
    const row = el("div", "db-jukebox-row" + (s.audio ? "" : " noaudio"));
    row.style.setProperty("--accent", accent);

    const metaBits = [s.lane];
    if (s.duration) metaBits.push(s.duration);
    if (s.status === "released" && s.released) metaBits.push(s.released);

    row.append(
      el("span", "db-jb-glyph", s.audio ? "▸" : "·"),
      el("span", "db-jb-title", s.title),
      badge(s.status),
      el("span", "db-jb-meta", metaBits.join(" · ")),
      el("span", "db-jb-blurb", s.blurb || ""),
    );
    if (!s.audio) row.append(el("span", "db-jb-no", "no preview"));

    if (s.audio) {
      row.onclick = () => {
        if (current === i) { audio.paused ? audio.play() : audio.pause(); }
        else playIndex(i);
      };
    }
    rows.push(row);
    list.append(row);
  });

  box.append(nowBar, list);
  container.append(box);
}

// ── instruments section ───────────────────────────────────────────────
function renderInstruments(instruments, container) {
  const grid = el("div", "db-inst-grid");
  for (const inst of instruments) {
    const accent = laneAccent(inst.lane);
    const card = el("div", "db-inst-card");
    card.style.setProperty("--accent", accent);

    card.append(el("div", "db-inst-label", inst.label));
    if (inst.blurb) card.append(el("p", "db-blurb", inst.blurb));

    // presets
    if (inst.presets && inst.presets.length) {
      const chips = el("div", "db-chips");
      for (const p of inst.presets) chips.append(el("span", "db-chip", p));
      card.append(chips);
    }

    card.append(audioEl(inst.sample));
    card.append(el("p", "db-caption",
      "sample rendered by pop/bin/render-instrument-samples.mjs"));

    grid.append(card);
  }
  container.append(grid);
}

// ── section heading ───────────────────────────────────────────────────
function sectionHeading(text) {
  const h = document.createElement("h2");
  h.className = "db-section";
  h.textContent = text;
  return h;
}

// ── main entry ───────────────────────────────────────────────────────
// Inserts the two sections before `beforeEl` (the #demos masonry container).
export async function mountDashboard(beforeEl) {
  let manifest;
  try {
    const r = await fetch("./demos/manifest.json");
    if (!r.ok) throw new Error(`HTTP ${r.status}`);
    manifest = await r.json();
  } catch (err) {
    const errEl = el("div", "db-load-err",
      "could not load manifest.json — " + err.message);
    beforeEl.parentNode.insertBefore(errEl, beforeEl);
    return;
  }

  const wrapper = el("div", "db-dashboard");

  // songs
  const songsSection = el("div", "db-section-wrap");
  songsSection.append(sectionHeading("song projects"));
  renderSongs(manifest.songs || [], songsSection);
  wrapper.append(songsSection);

  // instruments
  const instSection = el("div", "db-section-wrap");
  instSection.append(sectionHeading("instruments"));
  renderInstruments(manifest.instruments || [], instSection);
  wrapper.append(instSection);

  beforeEl.parentNode.insertBefore(wrapper, beforeEl);
}
