// piece-hits, 26.09.09
// The filter decides what the analytics mean, so it is worth pinning down.
// A false negative inflates a number; a false positive erases a reader.

import { looksAutomated } from "../backend/piece-hits.mjs";

const ua = (value) => ({ "user-agent": value });

describe("looksAutomated", () => {
  test("a request with no user agent is not a browser", () => {
    expect(looksAutomated({})).toBe(true);
    expect(looksAutomated(ua(""))).toBe(true);
  });

  test("real browsers are counted", () => {
    const readers = [
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/152.0.0.0 Safari/537.36",
      "Mozilla/5.0 (iPhone; CPU iPhone OS 18_7 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1",
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:155.0) Gecko/20100101 Firefox/155.0",
      "Mozilla/5.0 (X11; Linux x86_64; rv:146.0) Gecko/20100101 Firefox/146.0",
    ];
    for (const reader of readers) expect(looksAutomated(ua(reader))).toBe(false);
  });

  test("the machines actually seen in the access log are not", () => {
    const machines = [
      "got (https://github.com/sindresorhus/got)",
      "Go-http-client/2.0",
      "facebookexternalhit/1.1 (+http://www.facebook.com/externalhit_uatext.php)",
      "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) HeadlessChrome/120.0.0.0 Safari/537.36",
      "Googlebot/2.1 (+http://www.google.com/bot.html)",
      "curl/8.4.0",
      "python-requests/2.31.0",
    ];
    for (const machine of machines) expect(looksAutomated(ua(machine))).toBe(true);
  });

  test("the header name's capitalization does not decide the answer", () => {
    expect(looksAutomated({ "User-Agent": "Googlebot/2.1" })).toBe(true);
    expect(looksAutomated({ "user-agent": "Googlebot/2.1" })).toBe(true);
  });
});
