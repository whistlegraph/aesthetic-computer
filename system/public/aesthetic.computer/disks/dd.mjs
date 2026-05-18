// dd, 26.05.18 — @dreamdealer's vibe-coding piece.
//
// Thin wrapper over the shared lib/pp-client.mjs (same pattern laer-klokken.mjs
// uses with chat.mjs). Identical behavior + the same sub-gated /api/pp/* bridge
// as pp.mjs — only the identity below differs. Access requires the user's
// Auth0 sub in PP_ALLOWED_SUBS on the help bridge (handle irrelevant).

import { makePpClient } from "../lib/pp-client.mjs";

// Nocturnal neon-gold skin — midnight indigo, amber/teal/rose accents.
const DD = {
  slug: "dd",
  name: "dd",
  title: "DD",
  desc: "Vibe-code AC pieces with claude — published under your own handle.",
  greeting: (h) =>
    `dreams to order, ${h || "@you"} — describe a piece and i'll deal it (built + published).`,
  theme: {
    background:        [12, 16, 28],
    chromeBg:          [18, 24, 40],
    lines:             [90, 120, 170, 96],
    scrollbar:         [255, 200, 90],
    messageText:       [236, 240, 248],
    messageBox:        [220, 228, 240],
    log:               [120, 230, 220],
    logHover:          [255, 220, 120],
    handle:            [255, 180, 90],
    handleHover:       [255, 240, 160],
    url:               [130, 210, 255],
    urlHover:          [255, 220, 120],
    prompt:            [200, 180, 255],
    promptContent:     [130, 210, 255],
    promptHover:       [255, 220, 120],
    promptContentHover:[255, 220, 120],
    painting:          [255, 170, 210],
    paintingHover:     [255, 240, 160],
    kidlisp:           [180, 255, 200],
    kidlispHover:      [255, 240, 160],
    timestamp:         [110, 130, 170],
    timestampHover:    [255, 220, 120],
    heart:             [255, 150, 190],
  },
};

export const { boot, paint, act, sim, leave, meta } = makePpClient(DD);
