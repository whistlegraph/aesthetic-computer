// i18n — speak the same words the viewer is looking at.
//
// A localized tutorial is not just a translated voiceover. If the narration says
// "click Add a Node" while the button on screen reads "Agregar un Nodo", the
// video is worse than useless — it teaches a label that does not exist. So the
// narration and the SELECTORS both come from fuser's own translation files
// (packages/core/src/i18n/locales/<lng>). Their word is the word.
//
// Locale is switched the way the app itself decides it: i18next is configured
// with `detection: { order: ['localStorage', 'navigator'] }`, so writing
// `i18nextLng` and reloading is not a hack around the app — it IS the app's
// mechanism. Hirad's translations therefore drive the whole recording.
//
// Refresh the string dumps with `node bin/pull-strings.mjs` when fuser's locales
// change.

import { readFileSync, existsSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = fileURLToPath(new URL(".", import.meta.url));

// Fuser's own language table (packages/core/src/i18n/languages.ts). `voice` is
// what ElevenLabs needs: the jeffrey PVC runs on eleven_multilingual_v2, so one
// voice reads every one of these — it is his voice speaking Spanish, not a
// substitute narrator.
export const LANGUAGES = {
  "en":    { name: "English",    native: "English" },
  "es":    { name: "Spanish",    native: "Español" },
  "fr":    { name: "French",     native: "Français" },
  "hi":    { name: "Hindi",      native: "हिन्दी" },
  "pt-BR": { name: "Portuguese", native: "Português" },
  "zh-CN": { name: "Chinese",    native: "简体中文" },
};

export function strings(locale) {
  const path = join(HERE, "..", "assets", `strings.${locale}.json`);
  if (!existsSync(path)) {
    throw new Error(
      `no strings for "${locale}" — run: node bin/pull-strings.mjs ${locale}`);
  }
  return JSON.parse(readFileSync(path, "utf8"));
}

/// Resolve a dotted translation key, e.g. t("flow.emptyBanner.addNode").
///
/// Throws on a miss rather than falling back to English. A silent fallback is
/// how you end up filming a Spanish tutorial that clicks an English button and
/// only notice after it is published.
export function translator(locale) {
  const table = strings(locale);
  const en = locale === "en" ? table : strings("en");
  return function t(key) {
    const walk = (o) => key.split(".").reduce((a, k) => (a == null ? a : a[k]), o);
    const hit = walk(table);
    if (typeof hit === "string") return hit;
    const fallback = walk(en);
    throw new Error(
      `translation key "${key}" is missing for "${locale}"` +
      (typeof fallback === "string" ? ` (en has: ${JSON.stringify(fallback)})` : ""));
  };
}

/// Selectors that survive translation.
///
/// Three kinds, in order of preference:
///   1. A KEY into fuser's translations — the button really is labelled by them.
///   2. An English aria-label they never translated (`Create blank project`) —
///      accidentally locale-proof, and we lean on it.
///   3. STRUCTURE, when a control has no translated label and no test id at all:
///      "the big overlay button on the App node", "the widest textarea". Ugly to
///      read, but it is the same element in every language, which is the point.
export function selectors(t) {
  return {
    // (2) untranslated aria-label — identical in every locale.
    blankProject: '[aria-label="Create blank project"]',

    // (1) their words, whatever they are today.
    addNode: `text=${t("flow.emptyBanner.addNode")}`,
    nodeSearch: `input[placeholder="${t("flow.nodeSearch.search.placeholder")}"]`,

    // What to TYPE to find the App node. Not the literal string "app": the node
    // is named "应用" in Chinese, so typing "app" matched nothing and a trusted
    // Enter cheerfully committed whatever was top of the list instead (we filmed
    // a whole Chinese tutorial that added a Kling video node). Search for the
    // node by the name the app itself gives it.
    appNodeQuery: t("flow.nodes.AppNode.name"),

    // (3) structure. The App node's pre-deploy invitation ("Start an app") is
    // simply the BIGGEST button on the node — no key, no test id.
    //
    // Deliberately no minimum-size filter. react-flow scales nodes by the canvas
    // zoom transform, so the same button measures ~200×60 at 100% and ~62×18 at
    // 31%. Any absolute pixel threshold is really a hidden assertion about the
    // zoom level, and it silently stops matching the moment the canvas fits a
    // different view. Relative size is the only thing that holds.
    startApp: `js=(() => {
      const n = document.querySelector('.react-flow__node');
      if (!n) return null;
      return [...n.querySelectorAll('button')]
        .sort((a, b) => {
          const A = a.getBoundingClientRect(), B = b.getBoundingClientRect();
          return B.width * B.height - A.width * A.height;
        })[0];
    })()`,

    // (3) the canvas zoom control — the button whose label is a percentage.
    // react-flow fits the view when a node lands, which can drop the canvas to
    // ~31%; at that size the App node is an unreadable smudge. Zooming back to
    // 100% is what makes the node legible on camera.
    zoomButton: `js=[...document.querySelectorAll('button')].find((b) => /^\\d+\\s*%$/.test((b.innerText||'').trim()))`,
    zoom100: `js=[...document.querySelectorAll('[role=menuitem],[role=option],button')].find((b) => /\\b100\\s*%/.test(b.innerText||''))`,

    // (3) the focus composer is simply the widest textarea on the page.
    composer: `js=(() => {
      return [...document.querySelectorAll('textarea')]
        .sort((a, b) => b.getBoundingClientRect().width - a.getBoundingClientRect().width)[0];
    })()`,

    // (3) submit is an unlabelled icon button — name it by where it sits:
    // rightmost on the composer's control row.
    send: `js=(() => {
      const ta = [...document.querySelectorAll('textarea')]
        .sort((a, b) => b.getBoundingClientRect().width - a.getBoundingClientRect().width)[0];
      if (!ta) return null;
      const t = ta.getBoundingClientRect();
      return [...document.querySelectorAll('button')]
        .filter((b) => {
          const q = b.getBoundingClientRect();
          return q.width > 0 && q.top > t.top && q.top < t.bottom + 140 && q.left > t.left;
        })
        .sort((a, b) => b.getBoundingClientRect().left - a.getBoundingClientRect().left)[0];
    })()`,
  };
}

/// Put the app into a locale the way the app itself would: write the key
/// i18next's LanguageDetector reads, then reload so every component re-renders.
/// Runs in `setup`, off camera — the tutorial opens already in Spanish rather
/// than filming someone changing a language setting.
export async function setLocale(cdp, locale, url) {
  await cdp.eval(`localStorage.setItem("i18nextLng", ${JSON.stringify(locale)})`);
  await cdp.nav(url);
  await cdp.waitFor(
    `localStorage.getItem("i18nextLng") === ${JSON.stringify(locale)}`);
}
