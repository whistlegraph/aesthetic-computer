// Shared standalone-piece adapter for No Paint proposal contracts.

import { makeProposal, seededRandom } from "./nopaint-proposals.mjs";

export function createNoPaintBrushPiece(nopaintProposal, metadata) {
  let assets = new Map();
  let score = null;
  let frame = 0;
  let stroke = 0;
  let wasPainting = false;

  function boot({ hud, net, needsPaint }) {
    score = null;
    frame = 0;
    stroke = 0;
    wasPainting = false;
    assets = new Map();
    hud?.label?.(nopaintProposal.label);
    for (const path of nopaintProposal.assets || []) {
      const loading = net?.preload?.(path);
      loading?.then((loaded) => {
        assets.set(path, loaded?.img || loaded);
        needsPaint?.();
      })
        .catch(() => {});
    }
  }

  function sim({ needsPaint, system }) {
    if (!system?.nopaint?.is?.("painting")) return;
    frame += 1;
    needsPaint();
  }

  function paint($) {
    const painting = $.system?.nopaint?.is?.("painting");
    if (!painting) {
      wasPainting = false;
      return false;
    }
    const buffer = $.system.nopaint.buffer;
    if (!wasPainting || !score) {
      stroke += 1;
      frame = 0;
      const brush = $.system.nopaint.brush || { x: 0, y: 0 };
      const random = seededRandom(`${nopaintProposal.slug}:${stroke}:${brush.x}:${brush.y}`);
      const base = makeProposal(random, buffer.width, buffer.height);
      score = nopaintProposal.generate({
        random,
        width: buffer.width,
        height: buffer.height,
        base: Object.freeze({ ...base, kind: nopaintProposal.slug }),
      });
    }
    wasPainting = true;
    $.page(buffer).wipe(255, 255, 255, 0);
    nopaintProposal.render({ ...$, nopaintAssets: assets }, score, frame);
    $.page($.screen);
    return true;
  }

  function bake({ page, paste, screen, system }) {
    page(system.painting);
    paste(system.nopaint.buffer);
    page(system.nopaint.buffer).wipe(255, 255, 255, 0);
    page(screen);
    score = null;
    wasPainting = false;
  }

  function meta() {
    return {
      title: metadata.title,
      desc: metadata.desc,
      controls: "drag to propose the brush; release to bake it",
    };
  }

  return Object.freeze({ boot, sim, paint, bake, meta });
}
