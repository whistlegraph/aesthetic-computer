import test from "node:test";
import assert from "node:assert/strict";
import { formatRichText } from "../lib/imessage-rich-text.mjs";

test("formats Loopboy correspondence without RTF attachments", () => {
  const result = formatRichText([
    "# Update",
    "**Done:** shipped the fix.",
    "- *Verified* locally",
    "- [Open report](https://example.com/report)",
    "~~stale~~",
  ].join("\n"));
  assert.match(result, /𝐔𝐩𝐝𝐚𝐭𝐞/);
  assert.match(result, /𝐃𝐨𝐧𝐞/);
  assert.match(result, /• 𝑉𝑒𝑟𝑖𝑓𝑖𝑒𝑑 locally/);
  assert.match(result, /Open report — https:\/\/example\.com\/report/);
  assert.match(result, /s̶t̶a̶l̶e̶/);
  assert.doesNotMatch(result, /\\rtf|\*\*|\]\(/);
});

test("formatProse sets the whole body and keeps links, handles and tags plain", async () => {
  const { formatProse } = await import("../lib/imessage-rich-text.mjs");
  const result = formatProse("the *Plata* set is **done**: see https://example.com/x, @jeffrey #NAPM `ok1`", "sans");
  assert.match(result, /𝗍𝗁𝖾 𝘗𝘭𝘢𝘵𝘢 𝗌𝖾𝗍 𝗂𝗌 𝗱𝗼𝗻𝗲/);
  assert.match(result, /https:\/\/example\.com\/x, @jeffrey #NAPM 𝚘𝚔𝟷$/);
  assert.equal(formatProse("stays put", "plain"), "stays put");
  assert.throws(() => formatProse("x", "nope"), /unknown prose style/);
});
