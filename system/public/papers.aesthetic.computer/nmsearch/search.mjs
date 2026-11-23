import { json, live } from "./sources.mjs";

const $ = (selector) => document.querySelector(selector);
const catalogues = new Map();
function catalogue(path) {
  if (!catalogues.has(path)) {
    const request = json(path, AbortSignal.timeout(10000)).catch((error) => {
      catalogues.delete(path);
      throw error;
    });
    catalogues.set(path, request);
  }
  return catalogues.get(path);
}

function element(tag, label, className) {
  const el = document.createElement(tag);
  if (label) el.textContent = label;
  if (className) el.className = className;
  return el;
}
function link(label, url, className) {
  const el = element("a", label, className);
  if (url && /^https?:$/.test(new URL(url, location.origin).protocol)) {
    el.href = url;
    el.target = "_blank";
    el.rel = "noopener";
  }
  return el;
}

const fold = (str) => String(str ?? "").normalize("NFKD").replace(/\p{M}/gu, "").toLowerCase();
function ranked(records, query) {
  const phrase = fold(query);
  const words = phrase.split(/\s+/);
  const matches = [];
  for (const record of records) {
    const title = fold(record.title);
    const description = fold([record.sub, record.text, record.id].filter(Boolean).join(" "));
    if (!words.every((word) => title.includes(word) || description.includes(word))) continue;
    const score = (title === phrase ? 100 : 0) + (title.includes(phrase) ? 20 : 0)
      + words.filter((word) => title.includes(word)).length * 4 + Number(description.includes(phrase));
    matches.push({ record, score });
  }
  return matches.sort((a, b) => b.score - a.score || a.record.title.localeCompare(b.record.title)).map(({ record }) => record);
}

function renderHits(name, hits, total, more, query) {
  const terms = query.split(/\s+/).map((word) => word.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"));
  const pattern = new RegExp(terms.join("|"), "giu");
  function highlighted(value, className) {
    const span = element("span", "", className);
    let offset = 0;
    for (const match of String(value).matchAll(pattern)) {
      span.append(value.slice(offset, match.index), element("mark", match[0]));
      offset = match.index + match[0].length;
    }
    span.append(value.slice(offset));
    return span;
  }
  const card = element("article", "", "card");
  const header = element("header");
  const count = element("span", "", "state");
  header.append(element("h2", name), count);
  const list = element("ol");
  const next = element("button", "Show more", "more");
  card.append(header, list);
  let shown = 0;
  function append() {
    for (const r of hits.slice(shown, shown + 5)) {
      const li = element("li");
      const a = link("", r.url);
      a.append(highlighted(String(r.title), "t"));
      let sub = r.sub || "";
      if (r.text && !fold(r.title + " " + sub).includes(fold(query))) {
        const offset = Math.max(0, fold(r.text).indexOf(fold(query.split(/\s+/)[0])) - 45);
        const space = r.text.indexOf(" ", offset);
        const start = offset && space >= 0 && space - offset < 25 ? space + 1 : offset;
        sub = (start ? "…" : "") + r.text.slice(start, start + 190);
        if (start + 190 < r.text.length) sub += "…";
      }
      if (sub) a.append(highlighted(sub, "s"));
      if (r.id) a.append(element("span", r.id, "id"));
      li.append(a);
      list.append(li);
    }
    shown = list.children.length;
    const found = total ?? hits.length;
    count.textContent = shown < found ? `${shown} of ${found.toLocaleString()}` : String(shown);
    if (shown < hits.length) card.append(next);
    else {
      next.remove();
      if (more) card.append(link(more === "/platter" ? "Open Platter →" : "Search at source →", more, "more"));
    }
  }
  next.addEventListener("click", append);
  append();
  return card;
}

async function searchPlatter(query) {
  const { items } = await catalogue("/nmsearch/local.json");
  const records = items.map((r) => ({
    title: r.title, url: r.url, text: r.text, sub: [r.note, r.meta].filter(Boolean).join(" · "),
    id: r.kind === "platter" && r.id === r.meta ? "" : [r.kind, r.id].join(" · "),
  }));
  return { hits: ranked(records, query) };
}

async function searchArtBase(source, query) {
  const { items, snapshot } = await catalogue(source.catalog);
  const records = items.map((r) => ({
    title: r.title, id: r.id, sub: [...r.artists, r.year].filter(Boolean).join(" · "),
    url: `https://artbase.rhizome.org/wiki/${r.id}`,
  }));
  return { name: source.name + " · " + snapshot.slice(0, 4) + " index", hits: ranked(records, query) };
}

let active;
async function search(value, push = true) {
  const q = value.trim().slice(0, 300);
  active?.abort();
  const controller = new AbortController();
  active = controller;
  const current = () => !controller.signal.aborted;
  const url = new URL(location.href);
  if (q) url.searchParams.set("q", q);
  else url.searchParams.delete("q");
  if (push && location.href !== url.href) history.pushState(null, "", url);
  $("#q").value = q;
  document.title = q ? q + " · nmsearch" : "nmsearch · Aesthetic Computer";
  $("#results").hidden = !q;
  if (!q) return;
  for (const selector of ["#hits", "#states", "#ramps"]) $(selector).replaceChildren();
  $("#filter").replaceChildren(new Option("All matching sources", ""));
  $("#choose").hidden = true;
  $("#elsewhere").hidden = true;
  let pending = 0, found = 0, unavailable = 0, loading = true;
  const summary = () => {
    const suffix = pending || loading ? " · Searching…" : "";
    const errors = unavailable ? ` · ${unavailable} unavailable` : "";
    $("#summary").textContent = found
      ? `Matches in ${found} ${found === 1 ? "source" : "sources"}${suffix}${errors}`
      : pending || loading ? "Searching…" : `No matches returned for “${q}”${errors}. Try another term or archive.`;
  };
  async function source(name, external, get) {
    pending++;
    const state = element("li");
    const label = external ? link(name, external) : element("span", name);
    const status = document.createTextNode(" · searching");
    state.append(label, status);
    $("#states").append(state);
    try {
      const result = await get();
      if (!current()) return;
      if (result.hits.length) {
        const card = renderHits(result.name || name, result.hits, result.total, external, q);
        card.dataset.source = name;
        card.hidden = Boolean($("#filter").value && $("#filter").value !== name);
        $("#hits").append(card);
        $("#filter").add(new Option(name, name));
        $("#choose").hidden = $("#filter").options.length < 3;
        found++;
      }
      status.data = result.hits.length ? " · matches" : " · no matches";
    } catch (error) {
      if (!current()) return;
      unavailable++;
      status.data = error.name === "TimeoutError" ? " · timed out" : " · unavailable";
    } finally {
      pending--;
      if (current()) summary();
    }
  }

  summary();
  source("AC Platter", "/platter", () => searchPlatter(q));
  try {
    const field = await catalogue("/nmsearch/field.json");
    if (!current()) return;
    for (const s of field.sources) {
      const external = s.ramp.replace("{q}", encodeURIComponent(q));
      if (s.tier === "ramp") {
        const chip = link(s.name, external, "chip");
        chip.title = s.covers;
        $("#ramps").append(chip);
      } else if (s.tier === "indexed") {
        source(s.name, external, () => searchArtBase(s, q));
      } else if (s.tier === "live") {
        source(s.name, external, () => live[s.api](s, q, AbortSignal.any([controller.signal, AbortSignal.timeout(10000)]), field));
      }
    }
    $("#elsewhere").hidden = !$("#ramps").children.length;
  } catch {
    if (!current()) return;
    unavailable++;
    $("#states").append(element("li", "Archive directory unavailable. Submit the search again to retry."));
  } finally {
    loading = false;
    if (current()) summary();
  }
}

$("#filter").addEventListener("change", () => {
  const selected = $("#filter").value;
  for (const card of $("#hits").children) card.hidden = Boolean(selected && card.dataset.source !== selected);
});

$("#f").addEventListener("submit", (event) => {
  event.preventDefault();
  search($("#q").value);
});
document.querySelector(".examples").addEventListener("click", (event) => {
  const anchor = event.target.closest("a");
  if (!anchor || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
  event.preventDefault();
  search(new URL(anchor.href).searchParams.get("q"));
});
const fromAddress = () => search(new URLSearchParams(location.search).get("q") || "", false);
window.addEventListener("popstate", fromAddress);
fromAddress();
