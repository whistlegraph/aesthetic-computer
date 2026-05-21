// dashboard.mjs — songs + sounds sections for pop/demos.html.
// Browser-only. Fetches ./demos/manifest.json and renders two picker-driven
// sections above the tool-demo masonry. No Node imports.

// ── accent colours (match tool-demo CATS) ────────────────────────────
const LANE_ACCENT = {
  dance:           "#c0631a",
  hippyhayzard:    "#c0631a",
  marimba:         "#8a5223",
  gradus:          "#5a6b1f",
  "big-pictures":  "#1d7a8a",
  chillwave:       "#1d7a8a",
  jungle:          "#5a6b1f",
  gecs:            "#b5331f",
  hellsine:        "#b5331f",
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

// ── section heading ───────────────────────────────────────────────────
function sectionHeading(text) {
  const h = document.createElement("h2");
  h.className = "db-section";
  h.textContent = text;
  return h;
}

// ── songs section — picker-driven ────────────────────────────────────
// A <select> lists every song. The shared "now playing" bar holds the
// single <audio>. Selecting a track loads its audio and plays it.
// Tracks auto-advance to the next one with audio when the current ends.
function renderSongs(songs, container) {
  // shared player audio element (lives in now-bar)
  const audio = document.createElement("audio");
  audio.controls = true;
  audio.preload = "none";

  const nowLabel = el("span", "db-now-label", "—");
  const nowBar = el("div", "db-jukebox-now",
    el("span", "db-now-tag", "now playing"), nowLabel, audio);

  // picker row
  const sel = document.createElement("select");
  sel.className = "demo-sel db-song-sel";
  songs.forEach((s, i) => {
    const opt = document.createElement("option");
    opt.value = i;
    opt.textContent = s.title + "  —  " + s.status + (s.duration ? "  · " + s.duration : "");
    sel.append(opt);
  });

  const pickerRow = el("div", "db-picker-row",
    el("span", "db-picker-lbl", "track"), sel);

  // detail panel (below picker)
  const detail = el("div", "db-song-detail");

  let currentIdx = -1;

  function loadSong(i, autoplay) {
    const s = songs[i];
    currentIdx = i;
    sel.value = i;

    // update detail panel
    detail.innerHTML = "";

    // meta line: lane · duration · released date
    const metaBits = [s.lane];
    if (s.duration)  metaBits.push(s.duration);
    if (s.status === "released" && s.released) metaBits.push(s.released);

    const metaRow = el("div", "db-song-meta-row",
      badge(s.status),
      el("span", "db-jb-meta", metaBits.join(" · ")),
    );
    detail.append(metaRow);

    if (s.blurb) {
      detail.append(el("p", "db-song-blurb", s.blurb));
    }

    if (s.status === "released" && s.distributor) {
      detail.append(el("p", "db-song-distrib", "distributed via " + s.distributor));
    }

    // load audio
    if (s.audio) {
      nowLabel.textContent = s.title;
      audio.src = s.audio;
      if (autoplay) audio.play().catch(() => {});
    } else {
      nowLabel.textContent = s.title;
      audio.removeAttribute("src");
      audio.load();
      detail.append(el("p", "db-pending", "no preview yet"));
    }
  }

  // advance to next track with audio on ended
  audio.addEventListener("ended", () => {
    for (let j = currentIdx + 1; j < songs.length; j++) {
      if (songs[j].audio) { loadSong(j, true); return; }
    }
  });

  sel.addEventListener("change", () => {
    loadSong(Number(sel.value), false);
  });

  // default: first song (no autoplay)
  if (songs.length) loadSong(0, false);

  container.append(nowBar, pickerRow, detail);
}

// ── sounds section — two-step picker (category → sound) ──────────────
const SOUND_CATS = [
  { key: "synths",     label: "custom synths" },
  { key: "notepat",    label: "notepat voices" },
  { key: "percussion", label: "percussion" },
  { key: "beds",       label: "beds" },
  { key: "samples",    label: "sourced samples" },
];

function soundAudioSrc(sound, catKey) {
  // samples category uses `audio`; everything else uses `sample`
  return catKey === "samples" ? sound.audio : sound.sample;
}

function renderSounds(sounds, container) {
  // category select
  const catSel = document.createElement("select");
  catSel.className = "demo-sel db-cat-sel";
  for (const c of SOUND_CATS) {
    const opt = document.createElement("option");
    opt.value = c.key;
    opt.textContent = c.label;
    catSel.append(opt);
  }

  // sound select (rebuilt on cat change)
  const soundSel = document.createElement("select");
  soundSel.className = "demo-sel db-sound-sel";

  const pickerRow = el("div", "db-picker-row",
    el("span", "db-picker-lbl", "category"), catSel,
    el("span", "db-picker-sep", "/"),
    el("span", "db-picker-lbl", "sound"), soundSel,
  );

  // detail panel
  const panel = el("div", "db-sound-panel");

  function buildSoundOptions(catKey) {
    soundSel.innerHTML = "";
    const items = sounds[catKey] || [];
    for (const item of items) {
      const opt = document.createElement("option");
      opt.value = item.id;
      opt.textContent = item.label || item.id;
      soundSel.append(opt);
    }
  }

  function renderSoundPanel(catKey, soundId) {
    const items = sounds[catKey] || [];
    const sound = items.find(s => s.id === soundId) || items[0];
    if (!sound) { panel.innerHTML = ""; return; }

    panel.innerHTML = "";

    const accent = laneAccent(sound.lane);
    panel.style.setProperty("--accent", accent);

    panel.append(el("div", "db-sound-label", sound.label || sound.id));

    if (sound.blurb) panel.append(el("p", "db-blurb", sound.blurb));

    // letter key (percussion only)
    if (sound.letter) {
      panel.append(el("p", "db-sound-meta", "key: " + sound.letter));
    }

    // preset chips (synths only)
    if (sound.presets && sound.presets.length) {
      const chips = el("div", "db-chips");
      for (const p of sound.presets) chips.append(el("span", "db-chip", p));
      panel.append(chips);
    }

    // license + source (samples category)
    if (catKey === "samples") {
      const licLine = [sound.license, sound.source].filter(Boolean).join(" · ");
      if (licLine) panel.append(el("p", "db-sound-meta", licLine));
    }

    // audio player
    const src = soundAudioSrc(sound, catKey);
    if (src) {
      const a = document.createElement("audio");
      a.controls = true;
      a.preload = "none";
      a.src = src;
      const note = el("p", "db-pending", "sample pending");
      note.style.display = "none";
      a.onerror = () => { a.style.display = "none"; note.style.display = ""; };
      const wrap = el("div", "db-audio-wrap", a, note);
      panel.append(wrap);
    } else {
      panel.append(el("p", "db-pending", "sample pending"));
    }
  }

  function pickSound() {
    const catKey = catSel.value;
    const soundId = soundSel.value;
    renderSoundPanel(catKey, soundId);
  }

  catSel.addEventListener("change", () => {
    buildSoundOptions(catSel.value);
    pickSound();
  });

  soundSel.addEventListener("change", pickSound);

  // default: synths, first sound
  buildSoundOptions("synths");
  pickSound();

  container.append(pickerRow, panel);
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

  // sounds
  const soundsSection = el("div", "db-section-wrap");
  soundsSection.append(sectionHeading("sounds"));
  renderSounds(manifest.sounds || {}, soundsSection);
  wrapper.append(soundsSection);

  beforeEl.parentNode.insertBefore(wrapper, beforeEl);
}
