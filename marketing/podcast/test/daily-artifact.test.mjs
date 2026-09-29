// The daily token's live artifact: the switch, the metadata, the bundle.
// node --test marketing/podcast/test/daily-artifact.test.mjs

import { test } from "node:test";
import assert from "node:assert/strict";
import { gunzipSync, gzipSync } from "node:zlib";
import { artifactMode, tokenMetadata, crawlBundle, checkBundle, CRAWL_STYLE } from "../lib/artifact.mjs";
import { crawlLayout, crawlPiece, readablePeriod } from "../lib/crawl.mjs";

const SIGNER = "tz1gkf8EexComFBJvjtT1zdsisdah791KwBE";
const uris = { html: "ipfs://QmHtml", gif: "ipfs://QmGif", thumb: "ipfs://QmThumb" };
const episode = {
  title: "a door; (for letters)",
  body: `It said "hello", then (quietly) left; a colon: fine?`,
  date: "2026-09-28",
  episodeUrl: "https://www.buzzsprout.com/2628235",
  code: "dly",
  creator: SIGNER,
  uris,
};

test("the switch: html by default, gif on request, anything else refused", () => {
  assert.equal(artifactMode({}), "html");
  assert.equal(artifactMode({ DAILY_ARTIFACT: "gif" }), "gif");
  assert.equal(artifactMode({ DAILY_ARTIFACT: " HTML " }), "html");
  assert.throws(() => artifactMode({ DAILY_ARTIFACT: "svg" }), /html or gif/);
});

test("html metadata: the bundle is the artifact, the GIF the display", () => {
  const m = tokenMetadata({ ...episode, artifact: "html" });
  assert.equal(m.artifactUri, uris.html);
  assert.equal(m.displayUri, uris.gif);
  assert.equal(m.thumbnailUri, uris.thumb);
  // The HTML format entry is a Keep's, field for field.
  assert.deepEqual(m.formats, [
    { uri: uris.html, mimeType: "text/html", dimensions: { value: "responsive", unit: "viewport" } },
    { uri: uris.gif, mimeType: "image/gif", dimensions: { value: "512x512", unit: "px" } },
  ]);
  assert.deepEqual(m.creators, [SIGNER]);
  assert.equal(m.decimals, 0);
  assert.equal(m.symbol, "OBJKT");
  assert.ok(m.description.startsWith(episode.body));
  assert.equal(m.date, "2026-09-29T00:30:00.000Z");
});

test("gif metadata is exactly what the daily minted before the switch", () => {
  const m = tokenMetadata({ ...episode, artifact: "gif" });
  assert.deepEqual(m, {
    name: episode.title,
    description: `${episode.body}\n\n— Aesthetic Dot Computer, ${episode.date}. Listen: ${episode.episodeUrl}\nThe page as a live KidLisp piece: https://aesthetic.computer/dly`,
    tags: ["aesthetic.computer", "kidlisp", "podcast", "devlog", "pixelfont"],
    symbol: "OBJKT",
    artifactUri: uris.gif,
    displayUri: uris.gif,
    thumbnailUri: uris.thumb,
    creators: [SIGNER],
    formats: [{ uri: uris.gif, mimeType: "image/gif" }],
    decimals: 0,
    isBooleanAmount: false,
    shouldPreferSymbol: false,
    date: "2026-09-29T00:30:00.000Z",
  });
});

const layout = crawlLayout({ title: episode.title, body: episode.body, date: episode.date });
const source = crawlPiece(layout);

test("the crawl is responsive, paced for reading, and keeps its punctuation", () => {
  // No pinned resolution: every size and place comes from the live w and h.
  assert.ok(source.startsWith("(wipe black)"));
  assert.doesNotMatch(source, /\(resolution /);
  assert.match(source, /\(min \(\/ \(\* \.9 w\) \d+\) \(\/ h [\d.]+\)\)/, "text sized from w and h");
  assert.ok(readablePeriod(layout) >= 30000, "a pass is at least 30 s");
  assert.ok(source.includes(`(write "A DOOR; (FOR LETTERS)"`));
  const written = [...source.matchAll(/\(write "((?:[^"\\]|\\.)*)"/g)].map((m) => m[1]).join(" ");
  assert.equal(written, `A DOOR; (FOR LETTERS) It said \\"hello\\", then (quietly) left; a colon: fine?`);
});

test("the bundle is one offline, self-extracting PACK-mode page", async () => {
  const outer = await crawlBundle("dly", source);
  assert.ok(Buffer.byteLength(outer) < 2 * 1024 * 1024, "under 2 MB");
  assert.doesNotMatch(outer, /\s(src|href)=["']?https?:/i, "no external loads in the shell");
  const b64 = outer.match(/const b64='([A-Za-z0-9+/=]+)'/)[1];
  const inner = gunzipSync(Buffer.from(b64, "base64")).toString("utf8");
  assert.match(inner, /window\.acPACK_MODE = true;/);
  assert.match(inner, /window\.acSTARTING_PIECE = "\$dly";/);
  assert.ok(inner.includes(`window.acKIDLISP_SOURCE = ${JSON.stringify(source)};`), "the exact source");
  assert.ok(inner.includes(CRAWL_STYLE), "the wrapper style");
  assert.doesNotMatch(inner, /\s(src|href)=["']?https?:/i, "no external loads in the page");
  const vfs = JSON.parse(inner.match(/window\.VFS = (\{.*?\});\n/s)[1].replace(/<\\\/script>/g, "</script>"));
  for (const f of ["boot.mjs", "bios.mjs", "lib/disk.mjs", "lib/kidlisp.mjs", "disks/dly.lisp"]) assert.ok(vfs[f], `VFS has ${f}`);
  assert.ok(Object.keys(vfs).some((f) => f.startsWith("disks/drawings/font_1/")), "font_1 glyphs inlined");
  assert.equal(vfs["disks/dly.lisp"].content, source);

  // The gate daily-token runs before pinning: this bundle passes it...
  assert.deepEqual(checkBundle(outer, { code: "dly", source }), []);
  assert.deepEqual(checkBundle(outer, { code: "xyz", source }), ["doesn't start $xyz"]);
  // ...and one packed from a runtime without the highlighter fixes fails it.
  const stale = { ...vfs, "lib/kidlisp.mjs": { ...vfs["lib/kidlisp.mjs"], content: vfs["lib/kidlisp.mjs"].content.replace(/tokenScan\(/g, "scan(").replace(/window\.acPACK_MODE\s*&&\s*!window\.acKEEP_LABEL\)\s*return/g, "") } };
  const staleInner = inner.replace(/window\.VFS = \{.*?\};\n/s, () => `window.VFS = ${JSON.stringify(stale)};\n`);
  const staleOuter = outer.replace(b64, gzipSync(staleInner).toString("base64"));
  assert.deepEqual(checkBundle(staleOuter, { code: "dly", source }), [
    "runtime lacks the linear syntax highlighter",
    "runtime lacks the hidden pack label not coloured",
  ]);
  assert.deepEqual(checkBundle("<html>gif</html>", { code: "dly", source }), ["not a self-extracting gzip pack"]);
});
