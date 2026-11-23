const ticker = document.getElementById("ticker");
const strip = document.getElementById("code-strip");
const current = document.getElementById("current-work");
const toggle = document.getElementById("ticker-toggle");
const motion = matchMedia("(prefers-reduced-motion: reduce)");
const glyphs = new Set(["imab", "l8ly", "grow", "idni", "ppl", "wiyh", "lonr", "sdog", "w0w", "puzz"]);
const works = [];
const codes = new Set();
let selected = 0;
let cursor = null;
let loading;
let paused = motion.matches;

function setPaused(value) {
  paused = value;
  toggle.textContent = value ? "▷" : "Ⅱ";
  toggle.setAttribute("aria-label", value ? "Play code ticker" : "Pause code ticker");
}

function reveal(button) {
  const item = button.getBoundingClientRect();
  const view = strip.getBoundingClientRect();
  const offset = item.left < view.left ? item.left - view.left - 3 :
    item.right > view.right ? item.right - view.right + 3 : 0;
  if (offset) strip.scrollBy({ left: offset, behavior: paused || motion.matches ? "instant" : "smooth" });
}

function select(index) {
  const previous = strip.children[selected];
  previous?.setAttribute("aria-pressed", "false");
  if (previous) previous.tabIndex = -1;
  selected = index;
  const work = works[index];
  const button = strip.children[index];
  button.setAttribute("aria-pressed", "true");
  button.tabIndex = 0;
  reveal(button);
  current.href = "/" + work.code;
  current.title = work.title;
  current.setAttribute("aria-label", "Open " + work.title + " in the Index");

}

function makeChip(work, index) {
  const button = document.createElement("button");
  button.type = "button";
  button.className = "code-chip";
  button.tabIndex = -1;
  button.setAttribute("aria-pressed", "false");
  button.setAttribute("aria-label", work.code + ", " + work.title);
  const badge = document.createElement("span");
  badge.className = "code-badge";
  badge.setAttribute("aria-hidden", "true");
  badge.style.setProperty("--graph-color", /^#[0-9a-f]{6}$/i.test(work.c) ? work.c : "var(--pink)");
  badge.textContent = "♪";
  const src = glyphs.has(work.code) ? "/glyphs/" + work.code + ".jpg" : work.thumb;
  if (src) {
    const img = new Image();
    img.alt = "";
    img.loading = "lazy";
    img.onerror = () => badge.replaceChildren("♪");
    img.src = src;
    badge.replaceChildren(img);
  }
  button.append(badge, work.code);
  button.onclick = () => {
    setPaused(true);
    select(index);
    ticker.dispatchEvent(new CustomEvent("work-selected", { detail: work }));
  };
  return button;
}

async function fetchCatalog() {
  do {
    const query = new URLSearchParams({ action: "works", limit: "100" });
    if (cursor) query.set("cursor", cursor);
    const response = await fetch("/api/whistlegraph-query?" + query);
    if (!response.ok) throw new Error("Index unavailable");
    const page = await response.json();
    const first = !works.length;
    for (const work of page.items) {
      if (!/^[a-z0-9]+$/i.test(work.code) || codes.has(work.code)) continue;
      codes.add(work.code);
      strip.append(makeChip(work, works.length));
      works.push(work);
    }
    cursor = page.nextCursor;
    if (first && works.length) {
      ticker.hidden = false;
      select(Math.max(0, works.findIndex(work => work.code === "imab")));
    }
  } while (cursor);
}

function catalog() {
  loading ??= fetchCatalog().catch(() => { loading = undefined; });
  return loading;
}

export async function lookupWork(text) {
  const match = /^([a-z0-9]{1,24})$/i.exec(text.trim());
  if (!match) return;
  const code = match[1].toLowerCase();
  if (!codes.has(code)) await catalog();
  return works.find(work => work.code === code);
}

export function highlightWork(work) {
  setPaused(true);
  select(works.indexOf(work));
}

strip.addEventListener("keydown", event => {
  const next = { ArrowLeft: selected - 1, ArrowRight: selected + 1, Home: 0, End: works.length - 1 }[event.key];
  if (next === undefined) return;
  event.preventDefault();
  setPaused(true);
  select((next + works.length) % works.length);
  strip.children[selected].focus({ preventScroll: true });
});
strip.addEventListener("pointerdown", () => setPaused(true));
strip.addEventListener("wheel", () => setPaused(true), { passive: true });
strip.addEventListener("focusin", () => setPaused(true));
toggle.onclick = () => setPaused(!paused);
motion.addEventListener("change", () => setPaused(motion.matches));
setPaused(paused);
catalog();
setInterval(() => {
  if (document.hidden) return;
  if (!works.length) { catalog(); return; }
  if (!paused && !current.matches(":focus") && !strip.matches(":hover")) select((selected + 1) % works.length);
}, 5500);
