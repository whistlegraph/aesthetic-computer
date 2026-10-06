import { validateCatalog, filterItems, renderCard, makeCalendar, deadlineState } from "./core.mjs";

const form = document.querySelector("#filters");
const root = document.querySelector("#opportunities");
const count = document.querySelector("#result-count");
const empty = document.querySelector("#empty");
const viewLinks = [...document.querySelectorAll("[data-view]")];
const keys = ["q", "discipline", "kind", "paid", "free", "remote", "international", "sort"];
const flags = ["paid", "free", "remote", "international"];
const timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone;
let catalog, view = "upcoming";

function readURL() {
  const p = new URLSearchParams(location.search);
  view = ["upcoming", "rolling", "archive"].includes(p.get("view")) ? p.get("view") : "upcoming";
  for (const key of keys) {
    const input = form.elements.namedItem(key);
    if (flags.includes(key)) input.checked = p.get(key) === "1";
    else input.value = p.get(key) || (key === "sort" ? "deadline" : "");
  }
}
function filters() {
  const data = new FormData(form);
  return {view, ...Object.fromEntries(keys.map((key) => [key, flags.includes(key) ? data.has(key) : data.get(key)]))};
}
function writeURL() {
  const f = filters(), params = new URLSearchParams();
  for (const [key, value] of Object.entries(f)) {
    if (!value || (key === "view" && value === "upcoming") || (key === "sort" && value === "deadline")) continue;
    params.set(key, value === true ? "1" : value);
  }
  history.replaceState(null, "", location.pathname + (params.size ? "?" + params : "") + location.hash);
}
function render({updateURL = true, focusHash = false} = {}) {
  if (!catalog) return;
  const items = filterItems(catalog.opportunities, filters());
  const expanded = [...root.querySelectorAll("article:has(details[open])")].map((x) => x.id);
  root.innerHTML = items.map((item) => renderCard(item, new Date(), timeZone)).join("");
  for (const id of expanded) {
    const details = document.getElementById(id)?.querySelector("details");
    if (details) details.open = true;
  }
  count.textContent = `${items.length} ${items.length === 1 ? "opportunity" : "opportunities"}`;
  empty.hidden = items.length > 0;
  for (const link of viewLinks) {
    if (link.dataset.view === view) link.setAttribute("aria-current", "page");
    else link.removeAttribute("aria-current");
  }
  if (updateURL) writeURL();
  if (focusHash && location.hash) {
    const target = document.getElementById(location.hash.slice(1));
    if (target?.classList.contains("opportunity")) { target.querySelector("details").open = true; target.scrollIntoView(); }
  }
}
function revealHash() {
  if (!catalog || !location.hash) return;
  const item = catalog.opportunities.find((x) => "#" + x.id === location.hash);
  if (!item) return;
  if (!filterItems(catalog.opportunities, filters()).some((x) => x.id === item.id)) {
    form.reset();
    view = deadlineState(item) === "closed" ? "archive" : "upcoming";
  }
  render({focusHash: true});
}
function reset() {
  form.reset(); view = "upcoming";
  history.replaceState(null, "", location.pathname);
}
form.addEventListener("submit", (event) => event.preventDefault());
form.addEventListener("input", () => render());
form.addEventListener("change", () => render());
form.elements.namedItem("sort").addEventListener("change", () => render());
// Native button resets finish after event microtasks; read the new values next task.
form.addEventListener("reset", () => { view = "upcoming"; setTimeout(() => render(), 0); });
document.querySelector("#clear-empty").addEventListener("click", reset);
for (const link of viewLinks) link.addEventListener("click", (event) => {
  if (!catalog || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
  event.preventDefault(); view = link.dataset.view; render();
});
window.addEventListener("popstate", () => { readURL(); render({updateURL: false, focusHash: true}); });
window.addEventListener("hashchange", revealHash);
root.addEventListener("click", (event) => {
  const button = event.target.closest("[data-calendar]");
  if (!button || !catalog) return;
  const item = catalog.opportunities.find((x) => x.id === button.dataset.calendar);
  if (!item?.deadline) return;
  const url = URL.createObjectURL(new Blob([makeCalendar([item], catalog.updated)], {type: "text/calendar;charset=utf-8"}));
  const link = document.createElement("a");
  link.href = url; link.download = `${item.id}.ics`; link.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
});
async function load() {
  try {
    const response = await fetch(new URL("./opportunities.json", import.meta.url), {cache: "no-cache"});
    if (!response.ok) throw new Error(`Catalog returned ${response.status}`);
    catalog = validateCatalog(await response.json());
    readURL(); render({updateURL: false}); revealHash();
    form.hidden = false; document.querySelector("#sort-label").hidden = false;
    document.querySelector("#load-error").hidden = true;
  } catch (error) {
    console.error("Deadlines catalog:", error.message);
    document.querySelector("#load-error").hidden = false;
  }
}
document.querySelector("#retry").addEventListener("click", load);
document.addEventListener("visibilitychange", () => { if (!document.hidden && catalog) render({updateURL: false}); });
await load();
