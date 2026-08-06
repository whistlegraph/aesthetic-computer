#!/usr/bin/env node
// Plan a neat, non-overlapping layout from the current semantic frame.
// Dry-run is deliberate: a later filmed gesture pass applies the returned drags.

import { FUSER_INTELLIGENCE, readFuserFrame } from "../app-intelligence/fuser.mjs";
import { planFuserRectPack, validateFuserLayoutPlan } from "../lib/fuser-layout.mjs";
import { withSession } from "../lib/cdp.mjs";
import { translator } from "../lib/i18n.mjs";

const args = process.argv.slice(2);
const value = (flag, fallback) => {
  const index = args.indexOf(flag);
  return index < 0 ? fallback : args[index + 1];
};
if (args.includes("--help") || args.includes("-h")) {
  console.log("usage: node bin/fuser-pack.mjs [--locale en] [--match fuser.studio]");
  process.exit(0);
}
const locale = value("--locale", "en");
const result = await withSession(value("--match", FUSER_INTELLIGENCE.hostMatch), async (cdp) => {
  const frame = await readFuserFrame(cdp, { locale, t:translator(locale) });
  const plan = planFuserRectPack(frame.editor.nodes, frame.editor.canvas.rect);
  return { frame:{ capturedAt:frame.capturedAt, selection:frame.editor.selection, nodeCount:frame.editor.nodes.length }, plan, validation:validateFuserLayoutPlan(plan) };
});
console.log(JSON.stringify(result, null, 2));
if (!result.validation.pass) process.exitCode = 3;
