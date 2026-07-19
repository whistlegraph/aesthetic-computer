// pronounce.mjs — definitive pronunciations for spinging (round 3).
//
// word → curated IPA + syllable structure {onset, nucleus, coda} — the ground
// truth the choral notation and the guided aligner sing from, so phonemes are
// never guessed from spectra alone.
//
// Source chain (each hit cached forever in spinging/cache/pronounce.json):
//   1 · en.wiktionary.org — MediaWiki parse API, English section, first
//       {{IPA|en|…}} transcription (US/GA variant preferred). Curated by
//       humans; usually carries syllable dots.
//   2 · espeak --ipa (brew espeak) — machine G2P, still real IPA.
//   3 · a tiny built-in table for anything both miss.
//
// CLI:  node spinging/lib/pronounce.mjs word [word …]
// API:  await pronounce("synthesizer") →
//   { word, ipa, source, syllables: [{ onset:[{ipa,cls,voiced}],
//     nucleus:{ipa,cls:"vowel",voiced:true}, coda:[…], stress }] }

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const CACHE_DIR = resolve(HERE, "..", "cache");
const CACHE_FILE = `${CACHE_DIR}/pronounce.json`;
const UA = "aesthetic-computer-spinging/1.0 (mail@aesthetic.computer)";

// ── phone inventory ────────────────────────────────────────────────────────
// Multi-char phones first (longest match wins in the tokenizer).
const DIPHTHONGS = ["aɪ", "aʊ", "ɔɪ", "eɪ", "oʊ", "əʊ", "ɪə", "eə", "ʊə", "ɑɪ", "ɑʊ"];
const LONG_VOWELS = ["iː", "uː", "ɑː", "ɔː", "ɜː", "æː", "ɛː", "oː", "eː", "aː"];
const VOWELS = [
  ...DIPHTHONGS, ...LONG_VOWELS,
  "i", "ɪ", "e", "ɛ", "æ", "a", "ɑ", "ɒ", "ɔ", "o", "ʊ", "u", "ʌ", "ə", "ɜ",
  "ɝ", "ɚ", "ɐ", "ɘ", "l̩", "n̩", "m̩", "ɹ̩",
];
const AFFRICATES = ["t͡ʃ", "d͡ʒ", "tʃ", "dʒ"];
const CONSONANTS = [
  ...AFFRICATES,
  "p", "b", "t", "d", "k", "ɡ", "g", "ʔ", "ɾ",
  "f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ", "h", "x",
  "m", "n", "ŋ", "l", "ɫ", "ɹ", "r", "ɻ", "j", "w", "ʍ",
];
const PHONES = [...VOWELS, ...CONSONANTS].sort((a, b) => b.length - a.length);

// phone → { cls, voiced } — the classes the guided aligner anchors on.
export function phoneClass(p) {
  const bare = p.replace(/[ːˑ̩͡]/g, "");
  if (VOWELS.includes(p) || VOWELS.includes(bare) || /^[iɪeɛæaɑɒɔoʊuʌəɜɝɚɐɘ]/.test(bare))
    return { cls: "vowel", voiced: true };
  if (["tʃ", "t͡ʃ"].includes(p)) return { cls: "affricate", voiced: false };
  if (["dʒ", "d͡ʒ"].includes(p)) return { cls: "affricate", voiced: true };
  if (["p", "t", "k", "ʔ"].includes(bare)) return { cls: "plosive", voiced: false };
  if (["b", "d", "ɡ", "g", "ɾ"].includes(bare)) return { cls: "plosive", voiced: true };
  if (["f", "θ", "s", "ʃ", "h", "x", "ʍ"].includes(bare)) return { cls: "fricative", voiced: false };
  if (["v", "ð", "z", "ʒ"].includes(bare)) return { cls: "fricative", voiced: true };
  if (["m", "n", "ŋ"].includes(bare)) return { cls: "nasal", voiced: true };
  return { cls: "approximant", voiced: true }; // l ɫ ɹ r j w …
}

const tag = (p) => ({ ipa: p, ...phoneClass(p) });

// ── IPA tokenizer + syllabifier ────────────────────────────────────────────
export function tokenizeIPA(ipa) {
  // strip slashes/brackets, keep stress + dots as markers
  const s = ipa.replace(/^[/\[]|[/\]]$/g, "").replace(/[()]/g, "");
  const out = [];
  let i = 0;
  while (i < s.length) {
    const ch = s[i];
    if (ch === "ˈ" || ch === "ˌ" || ch === ".") { out.push(ch); i++; continue; }
    if (/[\s‿]/.test(ch)) { i++; continue; }
    let hit = null;
    for (const p of PHONES) {
      if (s.startsWith(p, i)) { hit = p; break; }
    }
    if (hit) {
      // absorb trailing length/syllabic/tie diacritics
      let j = i + hit.length;
      while (j < s.length && /[ːˑ̩̯̃ʰʷ]/.test(s[j])) { hit += s[j]; j++; }
      out.push(hit);
      i = j;
    } else i++; // unknown mark — skip
  }
  return out;
}

// legal English onsets (joined bare-phone strings) for maximal-onset syllabification
const ONSETS = new Set([
  "sp", "st", "sk", "sm", "sn", "sf", "sl", "sw", "sj",
  "pl", "pr", "pɹ", "bl", "br", "bɹ", "tr", "tɹ", "tw", "tj", "dr", "dɹ", "dw", "dj",
  "kl", "kr", "kɹ", "kw", "kj", "ɡl", "ɡr", "ɡɹ", "ɡw", "gl", "gr", "gɹ", "gw",
  "fl", "fr", "fɹ", "fj", "θr", "θɹ", "θw", "ʃr", "ʃɹ", "vj", "mj", "nj", "lj", "hj",
  "spr", "spɹ", "spl", "str", "stɹ", "stj", "skr", "skɹ", "skw", "skj",
]);
const bare = (p) => p.replace(/[ːˑ̩̯̃ʰʷ͡]/g, "");

export function syllabify(tokens) {
  // If the transcription carries dots, trust them outright.
  const hasDots = tokens.includes(".");
  const sylls = [];
  let cur = { phones: [], stress: 0 };
  const push = () => { if (cur.phones.length) sylls.push(cur); cur = { phones: [], stress: 0 }; };
  if (hasDots) {
    for (const t of tokens) {
      if (t === ".") push();
      else if (t === "ˈ") { if (cur.phones.length) push(); cur.stress = 1; }
      else if (t === "ˌ") { if (cur.phones.length) push(); cur.stress = 2; }
      else cur.phones.push(t);
    }
    push();
  } else {
    // stress marks are syllable boundaries too; otherwise maximal onset
    const phones = [];
    const stressAt = new Map();
    for (const t of tokens) {
      if (t === "ˈ") stressAt.set(phones.length, 1);
      else if (t === "ˌ") stressAt.set(phones.length, 2);
      else if (t !== ".") phones.push(t);
    }
    const isV = (p) => phoneClass(p).cls === "vowel";
    const nuclei = phones.map((p, i) => (isV(p) ? i : -1)).filter((i) => i >= 0);
    if (nuclei.length === 0) return [{ phones, stress: 0 }].map(structure);
    const bounds = [0];
    for (let k = 1; k < nuclei.length; k++) {
      const a = nuclei[k - 1], b = nuclei[k];
      let cut = b; // give the next syllable a maximal legal onset
      while (cut > a + 1) {
        const cand = phones.slice(cut - 1, b).map(bare).join("");
        if (cut - 1 === b) break;
        if (cand.length === 1 || ONSETS.has(cand)) cut--;
        else break;
      }
      // explicit stress mark inside (a,b] wins as the boundary
      for (let i = a + 1; i <= b; i++) if (stressAt.has(i)) cut = i;
      bounds.push(cut);
    }
    bounds.push(phones.length);
    for (let k = 0; k + 1 < bounds.length; k++) {
      const seg = phones.slice(bounds[k], bounds[k + 1]);
      let stress = 0;
      for (let i = bounds[k]; i < bounds[k + 1]; i++) if (stressAt.has(i)) stress = stressAt.get(i);
      if (stressAt.has(bounds[k])) stress = stressAt.get(bounds[k]);
      sylls.push({ phones: seg, stress });
    }
  }
  return sylls.map(structure);
}

function structure({ phones, stress }) {
  const isV = (p) => phoneClass(p).cls === "vowel";
  const vi = phones.findIndex(isV);
  if (vi < 0) {
    // consonant-only "syllable" (shouldn't survive syllabify) — treat last as nucleus
    return { onset: phones.slice(0, -1).map(tag), nucleus: tag(phones[phones.length - 1]), coda: [], stress };
  }
  // nucleus = the vowel (possibly a following syllabic ɹ̩ merges — keep simple)
  return {
    onset: phones.slice(0, vi).map(tag),
    nucleus: tag(phones[vi]),
    coda: phones.slice(vi + 1).map(tag),
    stress,
  };
}

// ── sources ────────────────────────────────────────────────────────────────
async function fromWiktionary(word) {
  const url = `https://en.wiktionary.org/w/api.php?action=parse&page=${encodeURIComponent(word)}` +
    `&prop=wikitext&format=json&formatversion=2&redirects=1`;
  const res = await fetch(url, { headers: { "User-Agent": UA } });
  if (!res.ok) return null;
  const json = await res.json();
  const wt = json?.parse?.wikitext;
  if (!wt) return null;
  const en = wt.split(/^==English==/m)[1]?.split(/^==[A-Z]/m)[0];
  if (!en) return null;
  const hits = [...en.matchAll(/\{\{IPA\|en\|([^}]+)\}\}/g)].map((m) => m[1]);
  if (!hits.length) return null;
  // each hit = pipe-separated transcriptions + qualifiers like a=US
  const parsed = [];
  for (const h of hits) {
    const parts = h.split("|");
    const accents = parts.filter((p) => p.startsWith("a=")).map((p) => p.slice(2));
    for (const p of parts) {
      if (/^[/\[].+[/\]]$/.test(p.trim()))
        parsed.push({ ipa: p.trim(), us: /US|GA|GenAm/i.test(accents.join(",")) });
    }
  }
  if (!parsed.length) return null;
  const pick = parsed.find((p) => p.us) || parsed[0];
  return { ipa: pick.ipa, source: "wiktionary" };
}

function fromEspeak(word) {
  const r = spawnSync("espeak", ["-q", "--ipa", word], { encoding: "utf8" });
  if (r.status !== 0) return null;
  const ipa = (r.stdout || "").trim();
  if (!ipa) return null;
  return { ipa: `/${ipa}/`, source: "espeak" };
}

const BUILTIN = {
  // last-resort seeds for words both sources might fumble
  "it's": "/ɪts/", sus: "/sʌs/", app: "/æp/", dot: "/dɑt/", yeah: "/jɛə/",
};

// ── cache + public API ─────────────────────────────────────────────────────
function loadCache() {
  try { return JSON.parse(readFileSync(CACHE_FILE, "utf8")); } catch { return {}; }
}
function saveCache(c) {
  mkdirSync(CACHE_DIR, { recursive: true });
  writeFileSync(CACHE_FILE, JSON.stringify(c, null, 1));
}

export const sourceCounts = { wiktionary: 0, espeak: 0, builtin: 0, cache: 0 };

export async function pronounce(rawWord) {
  const word = rawWord.toLowerCase().replace(/[^a-z']/g, "");
  const cache = loadCache();
  let entry = cache[word];
  if (entry) sourceCounts.cache++;
  if (!entry) {
    entry = await fromWiktionary(word).catch(() => null);
    if (!entry && BUILTIN[word]) entry = { ipa: BUILTIN[word], source: "builtin" };
    if (!entry) entry = fromEspeak(word);
    if (!entry) throw new Error(`pronounce: no IPA for "${word}" from any source`);
    entry.fetchedAt = new Date().toISOString();
    cache[word] = entry;
    saveCache(cache);
  }
  sourceCounts[entry.source] = (sourceCounts[entry.source] || 0) + 1;
  const syllables = syllabify(tokenizeIPA(entry.ipa));
  return { word, ipa: entry.ipa, source: entry.source, syllables };
}

export async function pronounceAll(words) {
  const out = {};
  for (const w of [...new Set(words.map((x) => x.toLowerCase()))]) out[w] = await pronounce(w);
  return out;
}

// ── CLI ────────────────────────────────────────────────────────────────────
if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const words = process.argv.slice(2);
  if (!words.length) { console.log("usage: pronounce.mjs word [word …]"); process.exit(2); }
  for (const w of words) {
    const p = await pronounce(w);
    const syl = p.syllables.map((s) =>
      `${s.onset.map((x) => x.ipa).join("")}·${s.nucleus.ipa}·${s.coda.map((x) => x.ipa).join("")}${s.stress === 1 ? "ˈ" : s.stress === 2 ? "ˌ" : ""}`
    ).join(" | ");
    console.log(`${w.padEnd(14)} ${p.ipa.padEnd(24)} [${p.source}]  ${syl}`);
  }
}
