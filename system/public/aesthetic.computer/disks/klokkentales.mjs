// Klokkentales, 2026.8.07
// Dramatic storybook dispatches from the public Lær Klokken chat.

const STREAM_ID = "klokkentales";
const CATALOG_URL = "https://assets.aesthetic.computer/klokkentales/index.json";

let catalog = {
  title: "Klokkentales",
  feed: "https://assets.aesthetic.computer/klokkentales/feed.xml",
  episodes: [],
};
let selected = 0;
let loading = true;
let loadError = false;
let isPlaying = false;
let isLoadingAudio = false;
let hasStarted = false;
let currentTime = 0;
let duration = 0;
let playBtn = null;
let prevBtn = null;
let nextBtn = null;
let rssBtn = null;

function currentEpisode() {
  return catalog.episodes[selected] || null;
}

function stop(send) {
  if (hasStarted && typeof send === "function") {
    send({ type: "stream:stop", content: { id: STREAM_ID } });
  }
  isPlaying = false;
  isLoadingAudio = false;
  hasStarted = false;
  currentTime = 0;
  duration = 0;
}

function choose(index, send) {
  if (!catalog.episodes.length) return;
  stop(send);
  selected = (index + catalog.episodes.length) % catalog.episodes.length;
}

function boot({ params, ui, screen, hud, net: { preload } }) {
  hud.label("klokkentales", "white");
  playBtn = new ui.TextButton("PLAY", { center: "x", screen, y: 0 });
  prevBtn = new ui.TextButton("<", { x: 8, y: 0, screen });
  nextBtn = new ui.TextButton(">", { right: 8, y: 0, screen });
  rssBtn = new ui.TextButton("RSS", { right: 8, bottom: 8, screen });

  preload(CATALOG_URL)
    .then((data) => {
      if (!data || !Array.isArray(data.episodes)) throw new Error("bad catalog");
      catalog = data;
      if (params[0]) {
        const requested = catalog.episodes.findIndex((episode) => episode.slug === params[0]);
        if (requested >= 0) selected = requested;
      }
      loading = false;
    })
    .catch(() => {
      loading = false;
      loadError = true;
    });
}

function paintClock({ ink, screen }, centerX, centerY, radius) {
  ink(23, 21, 42).circle(centerX, centerY, radius, true);
  ink(255, 232, 182).circle(centerX, centerY, radius);
  const now = new Date();
  const minute = now.getMinutes() + now.getSeconds() / 60;
  const hour = (now.getHours() % 12) + minute / 60;
  const minuteAngle = minute / 60 * Math.PI * 2 - Math.PI / 2;
  const hourAngle = hour / 12 * Math.PI * 2 - Math.PI / 2;
  ink(255, 207, 96).line(
    centerX, centerY,
    centerX + Math.cos(hourAngle) * radius * 0.48,
    centerY + Math.sin(hourAngle) * radius * 0.48,
  );
  ink(255, 207, 96).line(
    centerX, centerY,
    centerX + Math.cos(minuteAngle) * radius * 0.72,
    centerY + Math.sin(minuteAngle) * radius * 0.72,
  );
  ink(255, 207, 96).circle(centerX, centerY, 2, true);
}

function paint({ wipe, ink, screen }) {
  wipe(180, 100, 60);
  const centerX = Math.floor(screen.width / 2);
  const compact = screen.width < 420;
  const clockRadius = compact ? 34 : 48;
  paintClock({ ink, screen }, centerX, 58, clockRadius);

  if (loading) {
    ink(255, 232, 182).write("OPENING...", { center: "xy", screen });
    return;
  }

  const episode = currentEpisode();
  if (!episode) {
    ink(255, 232, 182).write("THE FIRST TALE IS BEING RECORDED", {
      center: "x", screen, y: 150,
    }, undefined, Math.max(120, screen.width - 40), true);
    if (loadError) ink(23, 21, 42).write("CATALOG OFFLINE", { center: "x", screen, y: 198 });
    rssBtn.paint({ ink }, [[145, 72, 42], [255, 207, 96], [23, 21, 42]]);
    return;
  }

  const bookTop = compact ? 112 : 128;
  const bookHeight = Math.max(150, Math.min(compact ? 250 : 230, screen.height - 230));
  ink(255, 232, 182).box(18, bookTop, screen.width - 36, bookHeight, "fill");
  ink(23, 21, 42).box(18, bookTop, screen.width - 36, bookHeight, "outline");
  const cast = Array.isArray(episode.cast) ? episode.cast.join(" + ") : "Jeffrey + Prutti";
  if (compact) {
    ink(23, 21, 42).write(episode.title.toUpperCase(), {
      center: "x", screen, y: bookTop + 38,
    }, undefined, screen.width - 72, true);
    ink(145, 72, 42).write(cast, { center: "x", screen, y: bookTop + 112 });
    ink(145, 72, 42).write(episode.date || "", { center: "x", screen, y: bookTop + 136 });
  } else {
    ink(23, 21, 42).line(centerX, bookTop, centerX, bookTop + bookHeight);
    ink(23, 21, 42).write(episode.title.toUpperCase(), {
      x: 46, y: bookTop + 42,
    }, undefined, Math.max(100, centerX - 78), true);
    ink(145, 72, 42).write(cast, { x: centerX + 34, y: bookTop + 42 });
    ink(145, 72, 42).write(episode.date || "", { x: centerX + 34, y: bookTop + 72 });
  }

  const controlsY = bookTop + bookHeight + 16;
  playBtn.reposition({ center: "x", screen, y: controlsY });
  playBtn.txt = isLoadingAudio ? "..." : isPlaying ? "PAUSE" : "PLAY";
  playBtn.paint({ ink }, isPlaying
    ? [[23, 21, 42], [255, 207, 96], [255, 232, 182]]
    : [[145, 72, 42], [255, 207, 96], [23, 21, 42]]);

  if (catalog.episodes.length > 1) {
    prevBtn.reposition({ x: 18, y: controlsY });
    nextBtn.reposition({ right: 18, y: controlsY });
    prevBtn.paint({ ink }, [[145, 72, 42], [255, 207, 96], [23, 21, 42]]);
    nextBtn.paint({ ink }, [[145, 72, 42], [255, 207, 96], [23, 21, 42]]);
  }

  ink(255, 232, 182).write(`${formatTime(currentTime)} / ${formatTime(duration)}`, {
    center: "x", screen, y: controlsY + 34,
  });
  rssBtn.paint({ ink }, [[145, 72, 42], [255, 207, 96], [23, 21, 42]]);
}

function toggle(send) {
  const episode = currentEpisode();
  if (!episode?.audio || isLoadingAudio) return;
  if (isPlaying) send({ type: "stream:pause", content: { id: STREAM_ID } });
  else if (hasStarted) send({ type: "stream:resume", content: { id: STREAM_ID } });
  else {
    isLoadingAudio = true;
    send({ type: "stream:play", content: { id: STREAM_ID, url: episode.audio, volume: 0.86 } });
  }
}

function act({ event: e, send, jump }) {
  playBtn?.act(e, { push: () => toggle(send) });
  prevBtn?.act(e, { push: () => choose(selected - 1, send) });
  nextBtn?.act(e, { push: () => choose(selected + 1, send) });
  rssBtn?.act(e, { push: () => jump(catalog.feed) });
  if (e.is("keyboard:down:space") || e.is("keyboard:down:enter")) toggle(send);
  if (e.is("keyboard:down:arrowleft")) choose(selected - 1, send);
  if (e.is("keyboard:down:arrowright")) choose(selected + 1, send);
}

function sim({ send }) {
  if (hasStarted) send({ type: "stream:time", content: { id: STREAM_ID } });
}

function receive({ type, content }) {
  if (content?.id !== STREAM_ID) return;
  if (type === "stream:playing") {
    isPlaying = true;
    isLoadingAudio = false;
    hasStarted = true;
  }
  if (type === "stream:paused") isPlaying = false;
  if (type === "stream:stopped") stop();
  if (type === "stream:error") {
    isPlaying = false;
    isLoadingAudio = false;
  }
  if (type === "stream:time-data") {
    currentTime = content.currentTime || 0;
    duration = content.duration || 0;
    if (content.ended) isPlaying = false;
  }
}

function leave({ send }) {
  stop(send);
}

function formatTime(seconds) {
  if (!Number.isFinite(seconds) || seconds <= 0) return "0:00";
  return `${Math.floor(seconds / 60)}:${String(Math.floor(seconds % 60)).padStart(2, "0")}`;
}

function meta() {
  return {
    title: "Klokkentales",
    desc: "Dramatic storybook dispatches from the Lær Klokken chat.",
  };
}

export { boot, paint, act, sim, receive, leave, meta };
