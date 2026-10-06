import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { validateCatalog, filterItems, deadlineState, dateParts, makeCalendar, makeRSS, foldICS, escapeHTML } from "../../system/public/papers.aesthetic.computer/deadlines/core.mjs";

const catalog = JSON.parse(await readFile(new URL("../../system/public/papers.aesthetic.computer/deadlines/opportunities.json", import.meta.url), "utf8"));
const find = (id) => catalog.opportunities.find((x) => x.id === id);
const now = new Date("2026-10-06T19:00:00Z");

test("public schema rejects private fields, paths and malformed dates", () => {
  assert.equal(validateCatalog(catalog), catalog);
  for (const mutate of [
    (x) => { x.opportunities[0].draftPath = "private"; },
    (x) => { x.opportunities[0].summary = "/Users/example/application"; },
    (x) => { x.opportunities[0].source = "javascript:alert(1)"; },
    (x) => { x.opportunities[0].deadline.date = "2026-02-30"; },
    (x) => { x.opportunities[1].id = x.opportunities[0].id; },
    (x) => { x.opportunities[0].fee.status = "probably free"; },
  ]) {
    const copy = structuredClone(catalog); mutate(copy);
    assert.throws(() => validateCatalog(copy));
  }
});

test("verified cutoffs expire at the actual instant across time zones", () => {
  const item = find("interplay-2027");
  assert.equal(deadlineState(item, new Date("2026-10-09T06:58:59Z")), "open");
  assert.equal(deadlineState(item, new Date("2026-10-09T06:59:00Z")), "closed");
  assert.equal(dateParts(item, "America/Los_Angeles").number, "8");
  assert.equal(dateParts(item, "Europe/London").number, "9");
});

test("unknown cutoffs retain their source date and eventually archive", () => {
  const item = find("djerassi-2027");
  assert.equal(dateParts(item, "Pacific/Auckland").number, "9");
  assert.equal(dateParts(item, "Pacific/Honolulu").number, "9");
  assert.equal(deadlineState(item, new Date("2026-10-10T11:59:59Z")), "open");
  assert.equal(deadlineState(item, new Date("2026-10-10T12:00:00Z")), "closed");
});

test("rolling review with a closing date appears until the window ends", () => {
  const ids = filterItems(catalog.opportunities, {view: "rolling"}, now).map((x) => x.id);
  assert.deepEqual(ids, ["openfeed-2026-2027", "sequoia-open-source"]);
  const future = new Date("2027-03-02T12:00:00Z");
  assert.deepEqual(filterItems(catalog.opportunities, {view: "rolling"}, future).map((x) => x.id), ["sequoia-open-source"]);
  assert(filterItems(catalog.opportunities, {view: "archive"}, future).some((x) => x.id === "openfeed-2026-2027"));
});

test("unknown fees do not pass the free filter; combined filters and search work", () => {
  const items = filterItems(catalog.opportunities, {free: true, paid: true, remote: true}, now);
  assert.deepEqual(items.map((x) => x.id), ["eyebeam-agency-2026", "sequoia-open-source"]);
  assert.deepEqual(filterItems(catalog.opportunities, {q: "VIENNA", discipline: "Sound"}, now).map((x) => x.id), ["smallforms-2027"]);
  assert.deepEqual(filterItems(catalog.opportunities, {q: "nothing matches"}, now), []);
});

test("deadline ordering compares absolute instants, not offset strings", () => {
  const base = find("interplay-2027");
  const items = [
    {...base, id: "later", deadline: {date: "2026-10-08", at: "2026-10-08T23:00:00-07:00"}},
    {...base, id: "earlier", deadline: {date: "2026-10-09", at: "2026-10-09T01:00:00+01:00"}},
  ];
  assert.deepEqual(filterItems(items, {}, now).map((x) => x.id), ["earlier", "later"]);
});

test("calendar exports exact UTC instants and honest all-day placeholders", () => {
  const ics = makeCalendar(catalog.opportunities);
  assert.match(ics, /DTSTART:20261009T065900Z/);
  assert.match(ics, /DTSTART;VALUE=DATE:20261009\r\nDTEND;VALUE=DATE:20261010/);
  assert.match(ics, /UID:djerassi-2027@papers.aesthetic.computer/);
  assert.match(ics, /\(check time\)/);
  assert(!ics.includes("UID:sequoia-open-source"));
  assert.equal((ics.match(/BEGIN:VEVENT/g) || []).length, 9);
  for (const line of ics.split("\r\n")) assert(Buffer.byteLength(line) <= 75);
  const unicode = "DESCRIPTION:" + "é💻,;".repeat(50);
  assert.equal(foldICS(unicode).replace(/\r\n /g, ""), unicode);
});

test("RSS has stable public permalinks and escaped text", () => {
  const copy = structuredClone(catalog);
  copy.opportunities[0].title = 'Art & "sound"';
  const xml = makeRSS(copy);
  assert.match(xml, /<title>Art &amp; &quot;sound&quot;<\/title>/);
  assert.match(xml, /<guid isPermaLink="true">https:\/\/papers.aesthetic.computer\/deadlines\/#interplay-2027<\/guid>/);
  assert(!xml.includes("/Users/") && !xml.includes("draftPath"));
  assert.equal(escapeHTML('<a href="x">&'), "&lt;a href=&quot;x&quot;&gt;&amp;");
});
