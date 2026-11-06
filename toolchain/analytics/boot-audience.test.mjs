import { test } from "node:test";
import assert from "node:assert/strict";
import { botFamily, browserFamily, foldBootAudience, foldHandleUsage } from "./boot-audience.mjs";

const CHROME = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/152.0.0.0 Safari/537.36";
const GOOGLE = "Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko; compatible; Googlebot/2.1; +http://www.google.com/bot.html) Chrome/150.0.0.0 Safari/537.36";
const HEADLESS = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) HeadlessChrome/152.0.0.0 Safari/537.36";
const boot = (over = {}) => ({ status: "success", createdAt: new Date("2026-11-06T18:00:00Z"),
  server: { country: "US" }, ...over,
  meta: { host: "aesthetic.computer", path: "/prompt", search: "", referrer: "", userAgent: CHROME, browser: "Chrome 152",
    mobile: false, language: "en-US", timezone: "America/Los_Angeles", embedded: false, packMode: false, localDev: false, user: null, ...over.meta } });

test("botFamily names known bots, our own render flags, and generic -bot words", () => {
  assert.equal(botFamily(GOOGLE), "Googlebot");
  assert.equal(botFamily(HEADLESS), "Headless Chrome");
  assert.equal(botFamily(CHROME, "?social-preview=1"), "own: social-preview");
  assert.equal(botFamily("Mozilla/5.0 (compatible; GPTBot/1.2)"), "GPTBot");
  assert.equal(botFamily("SomethingNewBot/0.1 (+https://example.org)"), "SomethingNewBot");
  assert.equal(botFamily(""), "empty user agent");
  assert.equal(botFamily(CHROME), "WebDriver or unlabeled");
});

test("browserFamily drops the version and names webviews", () => {
  assert.equal(browserFamily("Chrome 152"), "Chrome");
  assert.equal(browserFamily(null, "Mozilla/5.0 (iPhone; CPU iPhone OS 19_0 like Mac OS X) AppleWebKit/605.1.15 Mobile/15E148"), "iOS WebView");
  assert.equal(browserFamily(null, "whatever"), "unknown");
});

test("foldBootAudience splits humans, automated and walls and ranks the breakdowns", () => {
  const boots = [
    boot({ meta: { user: { handle: "jeffrey" } } }),
    boot({ meta: { path: "/laklok", referrer: "https://laklok.com/x", mobile: true, browser: "Safari 19" }, server: { country: "DK" } }),
    boot({ status: "started", meta: { path: "/prompt" } }),
    boot({ meta: { userAgent: GOOGLE, browser: null, path: "/notepat" }, server: { country: "US" } }),
    boot({ meta: { userAgent: HEADLESS, browser: null, search: "?offline-render=1", path: "/$abc" }, server: { country: null } }),
    boot({ meta: { embedded: true, host: "kidlisp.com", referrer: "https://top.kidlisp.com/" } }),
    boot({ meta: { localDev: true } }),
    boot({ meta: { host: "example.org" } }),
  ];
  const out = foldBootAudience(boots, { properties: ["aesthetic.computer", "kidlisp.com"], limit: 5 });
  assert.equal(out.humans.boots, 3);
  assert.equal(out.humans.completed, 2);
  assert.equal(out.humans.signedIn, 1);
  assert.equal(out.humans.anonymous, 2);
  assert.equal(out.humans.mobile, 1);
  assert.deepEqual(out.humans.byBrowser, [{ key: "Chrome", n: 2 }, { key: "Safari", n: 1 }]);
  assert.deepEqual(out.humans.byCountry, [{ key: "US", n: 2 }, { key: "DK", n: 1 }]);
  assert.deepEqual(out.humans.byReferrer, [{ key: "(direct or unknown)", n: 2 }, { key: "laklok.com", n: 1 }]);
  assert.equal(out.automated.boots, 2);
  assert.deepEqual(out.automated.byFamily, [{ key: "Googlebot", n: 1 }, { key: "own: offline-render", n: 1 }]);
  assert.deepEqual(out.automated.byCountry, [{ key: "US", n: 1 }, { key: "(unknown)", n: 1 }]);
  assert.ok(out.automated.examples.Googlebot.includes("Googlebot"));
  assert.equal(out.walls.boots, 1);
  assert.deepEqual(out.walls.byReferrer, [{ key: "top.kidlisp.com", n: 1 }]);
  assert.deepEqual(out.excluded, { outsideScope: 1, localDev: 1, packMode: 0 });
  assert.equal(out.automatedShare, 40);
});

test("foldHandleUsage ranks signed-in handles and skips bots, walls and anonymous loads", () => {
  const boots = [
    boot({ meta: { user: { handle: "jeffrey" } }, createdAt: new Date("2026-11-06T17:00:00Z") }),
    boot({ meta: { user: { handle: "@jeffrey" }, path: "/laer-klokken", mobile: true }, createdAt: new Date("2026-11-06T18:30:00Z") }),
    boot({ status: "started", meta: { user: { handle: "pruttipl" }, path: "/laklok:5" } }),
    boot({ meta: { user: { handle: "bot" }, userAgent: GOOGLE } }),
    boot({ meta: { user: { handle: "wall" }, embedded: true } }),
    boot(),
  ];
  const out = foldHandleUsage(boots, { properties: ["aesthetic.computer"], limit: 1 });
  assert.equal(out.handles, 2);
  assert.equal(out.anonymousBoots, 1);
  assert.equal(out.truncated, true);
  const [top] = out.rows;
  assert.equal(top.handle, "@jeffrey");
  assert.equal(top.boots, 2);
  assert.equal(top.completed, 2);
  assert.equal(top.mobile, 1);
  assert.deepEqual(top.pieces, [{ key: "prompt", n: 1 }, { key: "laer-klokken", n: 1 }]);
  assert.equal(top.firstSeen.toISOString(), "2026-11-06T17:00:00.000Z");
  assert.equal(top.lastSeen.toISOString(), "2026-11-06T18:30:00.000Z");
});
