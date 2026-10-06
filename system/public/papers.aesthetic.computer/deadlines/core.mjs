// Shared by the public board and the feed builder. No personal application data.
export const BASE = "https://papers.aesthetic.computer/deadlines/";
export const DISCIPLINES = ["Creative coding", "Digital art", "Sound", "Performance", "Moving image", "Games", "Open source", "Research"];
export const KINDS = ["Open call", "Commission", "Performance", "Residency", "Fellowship", "Grant", "Research"];
export const escapeHTML = (s) => String(s).replace(/[&<>"']/g, (c) => ({"&":"&amp;", "<":"&lt;", ">":"&gt;", '"':"&quot;", "'":"&#39;"})[c]);
const day = 86_400_000;
const isoDate = (s) => /^\d{4}-\d{2}-\d{2}$/.test(s) && !Number.isNaN(Date.parse(s)) && new Date(s).toISOString().slice(0, 10) === s;

function fields(value, required, optional = []) {
  if (!value || typeof value !== "object" || Array.isArray(value)) throw new Error("Expected an object");
  for (const key of required) if (!(key in value)) throw new Error(`Missing field: ${key}`);
  for (const key of Object.keys(value)) if (![...required, ...optional].includes(key)) throw new Error(`Unapproved public field: ${key}`);
}
function text(value, label) {
  if (typeof value !== "string" || !value.trim() || value.length > 1800) throw new Error(`Invalid ${label}`);
  if (/<[^>]+>|vscode:|\/Users\/|draftPath|aesthetic-computer-vault/i.test(value)) throw new Error(`Private path or markup in ${label}`);
}
function sourceURL(value) {
  const url = new URL(value);
  if (url.protocol !== "https:" || url.username || url.password || /^(localhost|127\.|\[::1\])/.test(url.hostname)) throw new Error("Public sources must use HTTPS");
}
export function validateCatalog(catalog) {
  fields(catalog, ["version", "updated", "opportunities"]);
  if (catalog.version !== 1 || !isoDate(catalog.updated) || !Array.isArray(catalog.opportunities)) throw new Error("Invalid catalog metadata");
  const ids = new Set();
  for (const item of catalog.opportunities) {
    fields(item, ["id", "title", "kind", "disciplines", "summary", "location", "remote", "dates", "deadline", "support", "fee", "eligibility", "requirements", "terms", "source", "checked", "published"], ["sources"]);
    if (!/^[a-z][a-z0-9-]+$/.test(item.id) || ids.has(item.id)) throw new Error(`Invalid or duplicate id: ${item.id}`);
    ids.add(item.id);
    for (const key of ["title", "summary", "location", "dates", "requirements", "terms"]) text(item[key], key);
    if (!KINDS.includes(item.kind) || !Array.isArray(item.disciplines) || !item.disciplines.length || item.disciplines.some((d) => !DISCIPLINES.includes(d))) throw new Error(`Invalid classification: ${item.id}`);
    if (typeof item.remote !== "boolean") throw new Error("Invalid remote flag");
    for (const key of ["checked", "published"]) if (!isoDate(item[key]) || item[key] > catalog.updated) throw new Error(`Invalid ${key}: ${item.id}`);
    sourceURL(item.source);
    if (item.sources !== undefined && !Array.isArray(item.sources)) throw new Error("Invalid sources");
    item.sources?.forEach(sourceURL);
    fields(item.support, ["paid", "text", "detail"]);
    if (typeof item.support.paid !== "boolean") throw new Error("Invalid funding flag");
    text(item.support.text, "support"); text(item.support.detail, "support detail");
    fields(item.fee, ["status", "text"]);
    if (!["free", "paid", "unknown"].includes(item.fee.status)) throw new Error("Invalid fee status");
    text(item.fee.text, "fee");
    fields(item.eligibility, ["international", "text"]);
    if (typeof item.eligibility.international !== "boolean") throw new Error("Invalid eligibility flag");
    text(item.eligibility.text, "eligibility");
    if (item.deadline !== null) {
      fields(item.deadline, ["date", "at", "note"], ["rolling"]);
      if (item.deadline.rolling !== undefined && typeof item.deadline.rolling !== "boolean") throw new Error("Invalid rolling flag");
      if (!isoDate(item.deadline.date)) throw new Error(`Invalid deadline date: ${item.id}`);
      text(item.deadline.note, "deadline note");
      if (item.deadline.at !== null && (!/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(Z|[+-]\d{2}:\d{2})$/.test(item.deadline.at) || !Number.isFinite(Date.parse(item.deadline.at)) || item.deadline.at.slice(0, 10) !== item.deadline.date)) throw new Error(`Invalid deadline instant: ${item.id}`);
    }
  }
  return catalog;
}

export function deadlineState(item, now = new Date()) {
  if (!item.deadline) return "rolling";
  // A date-only deadline is kept visible through that date everywhere on Earth.
  // Its exact cutoff is never inferred or represented as a midnight timestamp.
  const cutoff = item.deadline.at ? Date.parse(item.deadline.at) : Date.parse(item.deadline.date + "T00:00:00Z") + 36 * 3_600_000;
  return now.getTime() >= cutoff ? "closed" : item.deadline.rolling ? "rolling" : "open";
}

export function filterItems(items, filters = {}, now = new Date()) {
  const query = (filters.q || "").normalize("NFKD").toLowerCase();
  return items.filter((item) => {
    const state = deadlineState(item, now);
    if (filters.view === "archive" ? state !== "closed" : filters.view === "rolling" ? state !== "rolling" : state === "closed") return false;
    if (filters.discipline && !item.disciplines.includes(filters.discipline)) return false;
    if (filters.kind && item.kind !== filters.kind) return false;
    if (filters.paid && !item.support.paid) return false;
    if (filters.free && item.fee.status !== "free") return false;
    if (filters.remote && !item.remote) return false;
    if (filters.international && !item.eligibility.international) return false;
    const haystack = [item.title, item.summary, item.location, item.kind, ...item.disciplines, item.eligibility.text, item.support.text].join(" ").normalize("NFKD").toLowerCase();
    return !query || haystack.includes(query);
  }).sort((a, b) => {
    if (filters.sort === "newest") return b.published.localeCompare(a.published) || a.title.localeCompare(b.title);
    const instant = (x) => !x.deadline ? Number.MAX_SAFE_INTEGER : x.deadline.at
      ? Date.parse(x.deadline.at) : Date.parse(x.deadline.date) + 36 * 3_600_000;
    const cmp = instant(a) - instant(b);
    return filters.view === "archive" ? -cmp : cmp;
  });
}

export function calendarDate(date) {
  return new Intl.DateTimeFormat("en", {month: "short", day: "numeric", year: "numeric", timeZone: "UTC"}).format(new Date(date + "T12:00:00Z"));
}
export function dateParts(item, timeZone = "UTC") {
  if (!item.deadline) return {month: "ANY", number: "∞", year: "Rolling"};
  const date = new Date(item.deadline.at || item.deadline.date + "T12:00:00Z");
  const parts = new Intl.DateTimeFormat("en", {month: "short", day: "numeric", year: "numeric", timeZone: item.deadline.at ? timeZone : "UTC"}).formatToParts(date);
  return {month: parts.find((p) => p.type === "month").value, number: parts.find((p) => p.type === "day").value, year: parts.find((p) => p.type === "year").value};
}
export function countdown(item, now = new Date()) {
  const state = deadlineState(item, now);
  if (state === "closed") return "Closed";
  if (state === "rolling") return "Rolling";
  if (item.deadline.at) {
    const hours = Math.ceil((Date.parse(item.deadline.at) - now.getTime()) / 3_600_000);
    if (hours < 24) return hours <= 1 ? "Under 1 hour" : `${hours} hours left`;
    return `${Math.ceil(hours / 24)} days left`;
  }
  const remaining = Math.round((Date.parse(item.deadline.date) - Date.parse(now.toISOString().slice(0, 10))) / day);
  if (remaining <= 0) return "Check cutoff";
  return remaining === 1 ? "Tomorrow · check time" : `${remaining} days · check time`;
}

export function renderCard(item, now = new Date(), timeZone = "UTC") {
  const e = escapeHTML, p = dateParts(item, timeZone), state = deadlineState(item, now);
  const soon = item.deadline && state !== "closed" && Date.parse(item.deadline.date) - now < 7 * day;
  const deadline = item.deadline?.at
    ? `${new Intl.DateTimeFormat("en", {dateStyle: "medium", timeStyle: "short", timeZone}).format(new Date(item.deadline.at))} (${timeZone}). Source: ${item.deadline.note}.`
    : item.deadline ? `${calendarDate(item.deadline.date)}. ${item.deadline.note}.` : "Rolling applications. No fixed deadline published.";
  const sources = [item.source, ...(item.sources || [])].map((url, i) => `<a href="${e(url)}" rel="noopener noreferrer">${i ? "Additional source" : "Official call"} ↗</a>`).join(" · ");
  return `<article class="opportunity${state === "closed" ? " closed" : ""}" id="${item.id}">
    <div class="date-block" aria-label="${e(deadline)}"><span class="month">${e(p.month)}</span><span class="day">${e(p.number)}</span><span class="year">${e(p.year)}</span></div>
    <div class="opportunity-main"><div class="row-meta"><span>${e(item.kind)}</span><span class="countdown${soon ? " soon" : ""}">${e(countdown(item, now))}</span></div>
      <h2><a href="${e(item.source)}" rel="noopener noreferrer">${e(item.title)} <span aria-hidden="true">↗</span></a></h2>
      <p class="summary">${e(item.summary)}</p><p class="location">${e(item.location)} <span aria-hidden="true">·</span> ${e(item.dates)}</p>
      <ul class="tags" aria-label="Disciplines">${item.disciplines.map((d) => `<li>${e(d)}</li>`).join("")}</ul>
    </div>
    <div class="support"><p class="amount${item.support.paid ? " funded" : ""}">${e(item.support.text)}</p><p class="fee">${e(item.fee.text)}</p><a class="permalink" href="#${item.id}" aria-label="Link to ${e(item.title)}">#</a></div>
    <details><summary>Eligibility & details</summary><div class="detail-grid">
      <div><h3>Who can apply</h3><p>${e(item.eligibility.text)}</p><h3>Support</h3><p>${e(item.support.detail)}</p></div>
      <div><h3>Application</h3><p>${e(item.requirements)}</p><h3>Terms</h3><p>${e(item.terms)}</p></div>
      <div class="detail-bottom"><p><strong>Deadline:</strong> ${e(deadline)}</p><p>${sources} <span class="checked">· Checked ${e(calendarDate(item.checked))}</span></p>${item.deadline ? `<button type="button" class="add-calendar" data-calendar="${item.id}">Add deadline to calendar ↓</button>` : ""}</div>
    </div></details>
  </article>`;
}

export const escapeICS = (s) => String(s).replace(/\\/g, "\\\\").replace(/\r?\n/g, "\\n").replace(/;/g, "\\;").replace(/,/g, "\\,");
export function foldICS(line) {
  const encoder = new TextEncoder();
  let result = "", current = "", bytes = 0;
  for (const c of line) {
    const length = encoder.encode(c).length;
    if (bytes + length > 75) { result += current + "\r\n"; current = " "; bytes = 1; }
    current += c; bytes += length;
  }
  return result + current;
}
const compact = (date) => new Date(date).toISOString().replace(/[-:]/g, "").replace(/\.\d{3}Z$/, "Z");
export function makeCalendar(items) {
  const lines = ["BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//Aesthetic Computer//Deadlines//EN", "CALSCALE:GREGORIAN", "METHOD:PUBLISH", "X-WR-CALNAME:AC — New media deadlines", "X-PUBLISHED-TTL:PT12H"];
  for (const item of items.filter((x) => x.deadline)) {
    lines.push("BEGIN:VEVENT", `UID:${item.id}@papers.aesthetic.computer`, `DTSTAMP:${compact(item.published + "T00:00:00Z")}`, `LAST-MODIFIED:${compact(item.checked + "T00:00:00Z")}`);
    if (item.deadline.at) lines.push(`DTSTART:${compact(item.deadline.at)}`);
    else {
      lines.push(`DTSTART;VALUE=DATE:${item.deadline.date.replaceAll("-", "")}`);
      lines.push(`DTEND;VALUE=DATE:${new Date(Date.parse(item.deadline.date) + day).toISOString().slice(0, 10).replaceAll("-", "")}`);
    }
    const description = `${item.summary}\n\n${item.support.detail}\n${item.fee.text}\n\nEligibility: ${item.eligibility.text}\n\nDeadline: ${item.deadline.date}. ${item.deadline.note}${item.deadline.at ? "" : ". This calendar entry is all-day; verify the cutoff with the organizer."}\n\n${item.terms}\n\nOfficial call: ${item.source}\nChecked ${item.checked}`;
    lines.push(`SUMMARY:${escapeICS(item.title + " — deadline" + (item.deadline.at ? "" : " (check time)"))}`, `DESCRIPTION:${escapeICS(description)}`, `URL:${item.source}`, `LOCATION:${escapeICS(item.location)}`, "TRANSP:TRANSPARENT", "END:VEVENT");
  }
  lines.push("END:VCALENDAR");
  return lines.map(foldICS).join("\r\n") + "\r\n";
}

export function makeRSS(catalog) {
  const e = escapeHTML;
  return `<?xml version="1.0" encoding="UTF-8"?>\n<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom"><channel><title>AC — New media deadlines</title><link>${BASE}</link><description>Reviewed opportunities for creative coding, digital art, experimental sound and new media.</description><language>en</language><lastBuildDate>${new Date(catalog.updated + "T12:00:00Z").toUTCString()}</lastBuildDate><atom:link href="${BASE}feed.xml" rel="self" type="application/rss+xml"/>
${[...catalog.opportunities].sort((a, b) => b.published.localeCompare(a.published)).map((item) => `<item><title>${e(item.title)}</title><link>${BASE}#${item.id}</link><guid isPermaLink="true">${BASE}#${item.id}</guid><pubDate>${new Date(item.published + "T12:00:00Z").toUTCString()}</pubDate><description>${e(`${item.summary}\n\nDeadline: ${item.deadline ? item.deadline.date + ". " + item.deadline.note : "Rolling"}\n${item.support.detail}\n${item.fee.text}\n\nEligibility: ${item.eligibility.text}\n\n${item.terms}\n\nOfficial call: ${item.source}\nChecked ${item.checked}`)}</description>${item.disciplines.map((d) => `<category>${e(d)}</category>`).join("")}</item>`).join("\n")}
</channel></rss>\n`;
}
