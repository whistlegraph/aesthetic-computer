// whistlegraph songs — cluster transcribed performances into songs
//
//   node songs.mjs            # cluster everything transcribed so far
//   node songs.mjs --min 3    # only print clusters with ≥N performances
//
// Many of the 963 TikToks are re-performances of the same song. This
// joins downloads/transcripts/*.json with CATALOG.json and groups them:
// two clips belong to the same song when their lyrics share enough word
// 3-grams (Jaccard ≥ 0.3). Clusters are labeled by a known-song phrase
// match when one hits, otherwise by their most-viewed member's opening
// words. Wordless clips (whistled, no sung words) are set aside — they
// still count, they just can't cluster on lyrics.
//
// Output: downloads/SONGS.json + a console report, biggest songs first.
// Safe to run while lyrics.mjs is still transcribing; it clusters what
// exists and the numbers grow on the next run.

import { readFileSync, readdirSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const DOWNLOADS = join(HERE, "downloads");
const TRANSCRIPTS = join(DOWNLOADS, "transcripts");
const OUT = join(DOWNLOADS, "SONGS.json");

// Known songs — a phrase that appears in the sung lyric (lowercased,
// punctuation-stripped) claims the cluster. Ten + strong registry names.
const KNOWN = [
  ["Butterfly Cosplayer", "im a butterfly"],
  ["Lately When I Fly", "lately when i fly"],
  ["Time To Grow", "time to grow"],
  ["I Don't Need an iPhone", "dont need an iphone"],
  ["People Pleaser", "people pleaser"],
  ["What's Inside Your Heart?", "inside your heart"],
  ["Loner", "curled up in myself"],
  ["Slinky Dog", "slinky dog"],
  ["Mommy Wow", "love mommy wow"], // covers "x-my circle"/"here's my circle" variants
  ["Puzzle", "look at all my special edges"],
  ["Triangles", "angle deep inside"],
  ["Triangles", "bang go deep"], // the chalk-overdub take (whisper heard "BOO! DEEP!")
  ["Hey There, Apple", "hey there apple"],
  ["Cheerleader", "cheerleader"],
  ["Sad Mushroom", "keep me in this dark place"],
  ["Scared of Stairs", "come down the stairs"],
  ["Frog Tiara", "frog tiara"],
  ["Sad Campfire", "chilly but i keep burning"],
  ["I'm a Ghost", "im a ghost"],
  ["Bandaged Heart", "bandaged heart"],
  ["Five Ghosts", "five ghosts"],
  ["Bunny in a Bowl", "bunny in a bowl"],
  ["Flower Eater", "looked for so long for the one"],
  ["It's Too Hot, No It's Not", "its too hot no its not"],
  ["Empty Soda Cup", "empty soda cup"],
  ["Crawling in the Corner", "crawling in the corner"],
  ["Distant Hills", "distant hills"],
  ["Giant's Building", "giants building"],
  ["Sprout in the Grass", "sprout in the grass"],
  ["My Neighbor is My Best Friend", "neighbor is my best friend"],
  ["Battle Between Smiley Faces", "battle between smiley"],
  ["Dog Bite", "bit a hole right in your hand"],
  ["Some of the Time", "some of the time im feeling"],
  ["The Three of Us Are in a Cult", "three of us are in"], // merges the beatboxed take
  ["Going Down to South Park", "going down to south park"],
];

const norm = (s) =>
  s
    .toLowerCase()
    .replace(/\(.*?\)|\[.*?\]|♪/g, " ") // (upbeat music), [Music], notes
    .replace(/[^a-z0-9\s]/g, "")
    .replace(/\s+/g, " ")
    .trim();

const shingles = (text) => {
  const words = text.split(" ");
  const set = new Set();
  for (let i = 0; i < words.length - 2; i += 1) {
    set.add(`${words[i]} ${words[i + 1]} ${words[i + 2]}`);
  }
  return set;
};

const jaccard = (a, b) => {
  if (!a.size || !b.size) return 0;
  let hit = 0;
  for (const s of a) if (b.has(s)) hit += 1;
  return hit / (a.size + b.size - hit);
};

// Load transcripts, split sung from wordless.
const clips = readdirSync(TRANSCRIPTS)
  .filter((f) => f.endsWith(".json") && !f.startsWith("_"))
  .map((f) => JSON.parse(readFileSync(join(TRANSCRIPTS, f), "utf8")));
const sung = [];
const wordless = [];
for (const clip of clips) {
  const text = norm(clip.text ?? "");
  if (text.split(" ").length >= 4) sung.push({ ...clip, lyr: text, sh: shingles(text) });
  else wordless.push(clip);
}

// Greedy clustering: each clip joins the first cluster it resembles.
const clusters = [];
for (const clip of sung.sort((a, b) => (b.views ?? 0) - (a.views ?? 0))) {
  const home = clusters.find((c) => c.members.some((m) => jaccard(clip.sh, m.sh) >= 0.3));
  if (home) home.members.push(clip);
  else clusters.push({ members: [clip] });
}

// Label each cluster: a KNOWN phrase in any member's lyrics claims the title.
for (const cluster of clusters) {
  const allText = cluster.members.map((m) => m.lyr).join(" ");
  const known = KNOWN.find(([, phrase]) => allText.includes(phrase));
  cluster.knownTitle = known ? known[0] : null;
}

// Merge clusters that resolved to the same known title — greedy shingle
// clustering can split one song across a few takes whose wording drifts
// (the studio version vs a beatboxed intro); the title is ground truth.
const merged = [];
const byTitle = new Map();
for (const cluster of clusters) {
  if (cluster.knownTitle) {
    const home = byTitle.get(cluster.knownTitle);
    if (home) {
      home.members.push(...cluster.members);
      continue;
    }
    byTitle.set(cluster.knownTitle, cluster);
  }
  merged.push(cluster);
}

// Finalize each surviving cluster.
for (const cluster of merged) {
  const top = cluster.members.sort((a, b) => (b.views ?? 0) - (a.views ?? 0))[0];
  cluster.title = cluster.knownTitle ?? `? ${top.lyr.split(" ").slice(0, 6).join(" ")}`;
  cluster.known = Boolean(cluster.knownTitle);
  cluster.performances = cluster.members.length;
  cluster.views = cluster.members.reduce((sum, m) => sum + (m.views ?? 0), 0);
  cluster.span = [
    cluster.members.reduce((a, m) => (m.date < a ? m.date : a), "9999"),
    cluster.members.reduce((a, m) => (m.date > a ? m.date : a), "0000"),
  ];
}
const clustersFinal = merged.sort((a, b) => b.performances - a.performances || b.views - a.views);

writeFileSync(
  OUT,
  JSON.stringify(
    {
      clustered: new Date().toISOString(),
      transcribed: clips.length,
      sung: sung.length,
      wordless: wordless.length,
      songs: clustersFinal.map((c) => ({
        title: c.title,
        known: c.known,
        performances: c.performances,
        views: c.views,
        span: c.span,
        clips: c.members.map((m) => ({ id: m.id, date: m.date, views: m.views, text: m.text })),
      })),
      wordlessClips: wordless.map((m) => ({ id: m.id, date: m.date, views: m.views, desc: m.desc })),
    },
    null,
    1,
  ),
);

const min = (() => {
  const i = process.argv.indexOf("--min");
  return i >= 0 ? Number(process.argv[i + 1]) : 1;
})();
console.log(
  `${clips.length} transcribed → ${sung.length} sung / ${wordless.length} wordless → ${clustersFinal.length} songs\n`,
);
for (const c of clustersFinal.filter((c) => c.performances >= min)) {
  const views = c.views >= 1e6 ? `${(c.views / 1e6).toFixed(1)}M` : `${Math.round(c.views / 1e3)}K`;
  console.log(
    `${String(c.performances).padStart(3)}× ${views.padStart(7)}  ${c.span[0]}→${c.span[1]}  ${c.title}`,
  );
}
console.log(`\n→ ${OUT}`);
