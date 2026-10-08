// The daily token's live artifact: the switch, the metadata, the bundle.
// node --test marketing/podcast/test/daily-artifact.test.mjs

import { test } from "node:test";
import assert from "node:assert/strict";
import { gunzipSync, gzipSync } from "node:zlib";
import { artifactMode, tokenMetadata, crawlBundle, checkBundle, crawlPackage, dailyPackage, contentFiles, checkPublishedContent, checkPinnedArtifact, CRAWL_STYLE } from "../lib/artifact.mjs";
import AdmZip from "adm-zip";
import { crawlLayout, crawlPiece, readablePeriod } from "../lib/crawl.mjs";

const SIGNER = "tz1gkf8EexComFBJvjtT1zdsisdah791KwBE";
const uris = { directory: "ipfs://QmDirectory", gif: "ipfs://QmGif", thumb: "ipfs://QmThumb" };
const episode = {
  title: "a door; (for letters)",
  body: `It said "hello", then (quietly) left; a colon: fine?`,
  date: "2026-09-28",
  episodeUrl: "https://www.buzzsprout.com/2628235",
  code: "dly",
  creator: SIGNER,
  uris,
};

test("the switch: ZIP by default and for legacy HTML configs, GIF on request", () => {
  assert.equal(artifactMode({}), "zip");
  assert.equal(artifactMode({ DAILY_ARTIFACT: "zip" }), "zip");
  assert.equal(artifactMode({ DAILY_ARTIFACT: "gif" }), "gif");
  assert.equal(artifactMode({ DAILY_ARTIFACT: " HTML " }), "zip");
  assert.throws(() => artifactMode({ DAILY_ARTIFACT: "svg" }), /zip or gif/);
});

test("HEN metadata selects the directory viewer, with the GIF as display", () => {
  const m = tokenMetadata(episode);
  assert.equal(m.artifactUri, uris.directory);
  assert.equal(m.displayUri, uris.gif);
  assert.equal(m.thumbnailUri, uris.thumb);
  assert.deepEqual(m.formats, [
    { uri: uris.directory, mimeType: "application/x-directory", dimensions: { value: "responsive", unit: "viewport" } },
    { uri: uris.gif, mimeType: "image/gif", dimensions: { value: "512x512", unit: "px" } },
  ]);
  assert.deepEqual(m.creators, [SIGNER]);
  assert.equal(m.decimals, 0);
  assert.equal(m.symbol, "OBJKT");
  assert.ok(m.description.startsWith(episode.body));
  assert.equal(m.date, "2026-09-29T00:30:00.000Z");
});

test("bare HTML cannot be passed as a directory; old configs use the new format", () => {
  assert.throws(() => tokenMetadata({ ...episode, uris: { html: "ipfs://QmHtml" } }), /directory URI/);
  assert.equal(tokenMetadata({ ...episode, artifact: "html" }).formats[0].mimeType, "application/x-directory");
});

test("an unminted legacy receipt cannot resume into another HTML mint", () => {
  const legacy = { artifact: "html", metadataUri: "ipfs://QmMetadata" };
  assert.throws(() => checkPinnedArtifact(legacy, "zip"), /Unminted receipt/);
  assert.throws(() => checkPinnedArtifact({ ...legacy, artifact: "zip" }, "zip"), /Unminted receipt/);
  assert.doesNotThrow(() => checkPinnedArtifact({ ...legacy, mintOp: "opPending" }, "zip"));
  assert.doesNotThrow(() => checkPinnedArtifact({ ...legacy, tokenId: "885470" }, "zip"));
  assert.doesNotThrow(() => checkPinnedArtifact({}, "zip"));
  assert.doesNotThrow(() => checkPinnedArtifact({ artifact: "zip", artifactMimeType: "application/x-directory", metadataUri: "ipfs://QmNew" }, "zip"));
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

test("canonical prose remains readable without the canvas, including unsupported glyphs", () => {
  const e = { ...episode, body: `${episode.body}\n\nCafé, <script> & 🌑 \\ path.` };
  e.source = crawlPiece(crawlLayout(e));
  const files = contentFiles(e);
  assert.equal(files[0].content.toString(), `${e.title}\n\n${e.body}\n`);
  assert.equal(JSON.parse(files[1].content).body, e.body);
  assert.equal(files[2].content.toString(), e.source);
  assert.throws(() => contentFiles({ ...e, source: e.source.replace('It said', 'We lost') }), /differs/);
  assert.throws(() => contentFiles({ ...e, source: e.source.replace(/\(write .*\n?/, '') }), /differs/);
});

test("read-back rejects missing or modified published text and mismatched metadata", async () => {
  const files = contentFiles({ ...episode, source });
  const metadata = tokenMetadata(episode);
  const responses = new Map([["QmMetadata", Buffer.from(JSON.stringify(metadata))], ...files.map(f => [`QmDirectory/${f.name}`, f.content])]);
  const fetch = async url => {
    const bytes = responses.get(url.split('/ipfs/')[1]);
    return bytes ? new Response(bytes) : new Response('missing', { status: 404 });
  };
  const args = { metadataUri: "ipfs://QmMetadata", metadata, files, fetch };
  const qa = await checkPublishedContent(args);
  assert.equal(Object.keys(qa.files).length, 3);
  assert.match(qa.files['transcript.txt'], /^[a-f0-9]{64}$/);
  responses.set('QmDirectory/transcript.txt', Buffer.from('truncated'));
  await assert.rejects(checkPublishedContent(args), /Pinned content differs: transcript.txt/);
  responses.delete('QmDirectory/transcript.txt');
  await assert.rejects(checkPublishedContent(args), /404/);
  await assert.rejects(checkPublishedContent({ ...args, metadata: { ...metadata, description: 'different' } }), /metadata differs: description/);
});

test("the crawl is responsive, paced for reading, and keeps its punctuation", () => {
  // No pinned resolution: every size and place comes from the live w and h.
  assert.ok(source.startsWith("(wipe black)"));
  assert.doesNotMatch(source, /\(resolution /);
  assert.match(source, /\(max 1 \(- \(\/ \(\* \.92 w\) \d+\) \(mod/, "whole glyph pixels sized from width");
  assert.doesNotMatch(source, /\(floor /, "use arithmetic supported by the production interpreter");
  assert.doesNotMatch(source, /\(\/ h /, "short windows must not shrink the font");
  assert.ok(readablePeriod(layout) >= 30000, "a pass is at least 30 s");
  assert.ok(source.includes(`(write "A DOOR; (FOR"`));
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
  const packed = crawlPackage(outer, { gif: Buffer.from("gif"), thumbnail: Buffer.from("png"), frames: 300 });
  const zip = new AdmZip(packed.zip);
  assert.deepEqual(zip.getEntries().map(entry => entry.entryName).sort(), ["cover.gif", "index.html", "thumbnail.png"]);
  for (const file of packed.files) assert.deepEqual(zip.readFile(file.name), file.content, "IPFS gets the exact ZIP contents");
  assert.deepEqual(checkBundle(zip.readAsText("index.html"), { code: "dly", source }), []);
  const daily = dailyPackage(outer, { gif: Buffer.from("gif"), thumbnail: Buffer.from("png"), frames: 300 }, { ...episode, source });
  const dailyZip = new AdmZip(daily.zip);
  assert.equal(JSON.parse(dailyZip.readAsText("content.json")).body, episode.body);
  assert.equal(dailyZip.readAsText("transcript.txt"), `${episode.title}\n\n${episode.body}\n`);
  const jsonLD = dailyZip.readAsText('index.html').match(/<script type="application\/ld\+json">(.*?)<\/script>/s)[1];
  assert.equal(JSON.parse(jsonLD).text, episode.body);
  const hostile = { ...episode, body: 'literal </script><script>alert(1)</script>' };
  hostile.source = crawlPiece(crawlLayout(hostile));
  const safe = new AdmZip(dailyPackage(outer, { gif: Buffer.from('gif'), thumbnail: Buffer.from('png'), frames: 2 }, hostile).zip).readAsText('index.html');
  assert.equal(JSON.parse(safe.match(/<script type="application\/ld\+json">(.*?)<\/script>/s)[1]).text, hostile.body);
  for (const file of daily.files) assert.deepEqual(dailyZip.readFile(file.name), file.content);
  assert.match(zip.readAsText("index.html"), /property="og:image" content="cover.gif"/);
  assert.deepEqual(checkBundle(outer, { code: "xyz", source }), ["doesn't start $xyz", "VFS crawl source differs from the stored $code"]);
  const wrongVfs = inner.replace(/window\.VFS = \{.*?\};\n/s, () => `window.VFS = ${JSON.stringify({ ...vfs, "disks/dly.lisp": { ...vfs["disks/dly.lisp"], content: '(wipe black)' } })};\n`);
  assert.deepEqual(checkBundle(outer.replace(b64, gzipSync(wrongVfs).toString("base64")), { code: "dly", source }), ["VFS crawl source differs from the stored $code"]);
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
