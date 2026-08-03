#!/usr/bin/env node
// from-docs — turn a fuser docs page into a screenplay draft.
//
//   node bin/from-docs.mjs apps/quickstart
//
// The docs page IS the script. This is the whole point: a tutorial written
// separately from the docs drifts from the docs, and then you have two truths.
// Derive the narration from the page and the video can be re-rendered whenever
// the page changes — the words on screen and the words in your ear stay the
// same words.
//
// What it can and cannot do:
//
//   CAN   — lift the prose, clean it for speech, and cut it into beats at the
//           page's own `##` headings. Prose is already ordered for a reader, and
//           that order is almost always the right order for a viewer.
//   CAN   — find every ![alt](/shot.png) and hand you the alt text. Those images
//           mark precisely the moments the page's author thought needed a
//           picture, and the alt text describes what should be on screen. They
//           become the `do:` stubs.
//   CANNOT — know which button to click. Prose says "open focus mode"; only a
//           selector can say HOW. You fill those in (vault/fuser/skills/drive-ui.md
//           has the map), and that is the real authoring work.
//
// The output is a draft, not a deliverable. Read every `say:` aloud before you
// render — writing meant for the eye often needs a comma the ear can hear.

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, join, resolve, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const FUSER = process.env.FUSER_REPO || `${process.env.HOME}/Developer/fuser`;
const DOCS = join(FUSER, "apps", "docs", "content", "docs");

/// Strip a line of MDX down to something a voice can say.
function speakable(md) {
  return md
    // Self-closing JSX (<ShortcutKey modifiers={['CMD']} keyCharacter="Enter" />).
    // Recover the key names — "press CMD Enter" is the instruction; "press" alone
    // is useless narration.
    .replace(/<ShortcutKey[^>]*modifiers=\{\[([^\]]*)\]\}[^>]*keyCharacter="([^"]+)"[^>]*\/>/g,
      (_, mods, key) => `${mods.replace(/['"]/g, "").split(",").join(" ")} ${key}`.trim())
    .replace(/<[^>]+\/>/g, "")          // any other self-closing component
    .replace(/<\/?[A-Za-z][^>]*>/g, "") // component open/close tags, keep inner text
    .replace(/!\[[^\]]*\]\([^)]*\)/g, "")   // images (captured separately)
    .replace(/\[([^\]]+)\]\([^)]*\)/g, "$1") // links → their text
    .replace(/\*\*([^*]+)\*\*/g, "$1")       // bold
    .replace(/_([^_]+)_/g, "$1")             // italic
    .replace(/`([^`]+)`/g, "$1")             // code
    .replace(/\s+/g, " ")
    .trim();
}

const page = process.argv[2];
if (!page) {
  console.error("usage: from-docs <section/page>   e.g. apps/quickstart");
  process.exit(1);
}

const src = join(DOCS, `${page}.mdx`);
if (!existsSync(src)) {
  console.error(`no such page: ${src}`);
  process.exit(1);
}

const raw = readFileSync(src, "utf8");

// Frontmatter → title/description.
const fm = raw.match(/^---\n([\s\S]*?)\n---\n/);
const meta = {};
if (fm) {
  for (const line of fm[1].split("\n")) {
    const m = line.match(/^(\w+):\s*(.*)$/);
    if (m) meta[m[1]] = m[2].replace(/^["']|["']$/g, "");
  }
}

let body = raw.slice(fm ? fm[0].length : 0)
  .replace(/^import .*$/gm, "")     // MDX imports are not narration
  .replace(/```[\s\S]*?```/g, "");  // code fences are shown, not spoken

// Split at `##` headings. Everything before the first one is the intro.
const parts = body.split(/^##\s+(.+)$/m);
const sections = [{ heading: null, text: parts[0] }];
for (let i = 1; i < parts.length; i += 2) {
  sections.push({ heading: parts[i].trim(), text: parts[i + 1] || "" });
}

const beats = [];
for (const sec of sections) {
  // The screenshots the page's author placed — these are the visual cues.
  const shots = [...sec.text.matchAll(/!\[([^\]]*)\]\(([^)]*)\)/g)].map((m) => ({
    alt: m[1], src: m[2],
  }));

  const prose = sec.text
    .split(/\n\s*\n/)                       // paragraphs
    .map(speakable)
    .filter((p) => p.length > 24);          // drop scraps and stray punctuation

  for (const [i, p] of prose.entries()) {
    beats.push({
      say: p,
      heading: sec.heading,
      // Attach a section's screenshots to its first beat — that is where the
      // page shows the picture, so that is where the video should be looking.
      hint: i === 0 ? shots.map((s) => s.alt).filter(Boolean) : [],
    });
  }
}

const slug = page.replace(/\//g, "-");
const outDir = join(HERE, "..", "screenplays");
mkdirSync(outDir, { recursive: true });
const out = join(outDir, `${slug}.mjs`);

const body_ = beats.map((b) => {
  const head = b.heading ? `    // ── ${b.heading}\n` : "";
  const hint = b.hint.length
    ? b.hint.map((h) => `    // on screen (from the page's own screenshot): ${h}\n`).join("")
    : "";
  return `${head}${hint}    {
      say: ${JSON.stringify(b.say)},
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },`;
}).join("\n");

writeFileSync(out, `// ${slug} — DRAFT, generated from apps/docs/content/docs/${page}.mdx
//
// The narration below is the docs page's own prose. The blocking is not written
// yet: every \`do:\` is an empty stub. Fill them in, then:
//
//   node captutor.mjs narrate ${slug}   # hear the pacing before you film
//   node captutor.mjs render  ${slug}
//
// Re-running from-docs OVERWRITES this file — copy it aside once you have done
// the blocking, or teach from-docs to preserve it.

export default {
  slug: ${JSON.stringify(slug)},
  title: ${JSON.stringify(meta.title || slug)},
  voice: "jeffrey",
  window: "Chrome",
  match: "localhost:3200",
  fps: 60,

  // Sign in / seed / open the right project here — setup runs BEFORE recording,
  // so none of it lands in the tutorial.
  setup: async ({ cdp }) => {
    await cdp.nav("http://localhost:3200/");
  },

  beats: [
${body_}
  ],
};
`);

console.log(`→ ${out}`);
console.log(`  ${beats.length} beats from ${sections.length} sections`);
const withShots = beats.filter((b) => b.hint.length).length;
console.log(`  ${withShots} beat(s) carry a screenshot hint from the page`);
console.log(`\nnext: fill in the \`do:\` stubs, then \`node captutor.mjs narrate ${slug}\``);
