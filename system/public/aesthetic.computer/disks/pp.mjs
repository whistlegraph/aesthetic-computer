// pp, 26.05.18 — @prutti's vibe-coding piece.
//
// Thin wrapper over the shared lib/pp-client.mjs — the same way laer-klokken.mjs
// wraps chat.mjs. All behavior lives in the shared module; only this identity
// (label, title, theme) is local. Talks to the sub-gated /api/pp/* bridge;
// access requires the user's Auth0 sub in PP_ALLOWED_SUBS (handle irrelevant).

import { makePpClient } from "../lib/pp-client.mjs";

// Violet / lime maker skin — distinct from aa's cool slate.
const PP = {
  slug: "pp",
  name: "pp",
  title: "PP",
  desc: "Vibe-code AC pieces with claude — published under your own handle.",
  theme: {
    background:        [18, 12, 24],
    chromeBg:          [28, 18, 36],
    lines:             [120, 90, 160, 96],
    scrollbar:         [180, 240, 120],
    messageText:       [240, 236, 248],
    messageBox:        [228, 220, 240],
    log:               [200, 255, 150],
    logHover:          [255, 240, 120],
    handle:            [210, 160, 255],
    handleHover:       [255, 240, 120],
    url:               [180, 255, 200],
    urlHover:          [255, 240, 120],
    prompt:            [220, 200, 255],
    promptContent:     [180, 255, 200],
    promptHover:       [255, 240, 120],
    promptContentHover:[255, 240, 120],
    painting:          [230, 200, 255],
    paintingHover:     [255, 240, 120],
    kidlisp:           [255, 170, 230],
    kidlispHover:      [255, 240, 120],
    timestamp:         [140, 110, 170],
    timestampHover:    [255, 240, 120],
    heart:             [255, 200, 240],
  },
};

export const { boot, paint, act, sim, leave, meta } = makePpClient(PP);
