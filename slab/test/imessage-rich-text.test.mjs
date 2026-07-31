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
