// Regenerate lib/nopaint-stamp-sprites.mjs from the surviving Construct export.
//
//   node toolchain/nopaint/recover-stamp.mjs
//
// The manifest is committed so pieces can import it without a build step; this
// script is how it stays honest. system/tests/nopaint-stamp-sprites.test.mjs
// re-derives the same numbers from data.json and asserts they still match.

import { readFile, writeFile } from "node:fs/promises";

const art = new URL("../../system/public/nopaint.art/", import.meta.url);
const out = new URL("../../system/public/aesthetic.computer/lib/nopaint-stamp-sprites.mjs", import.meta.url);
const project = JSON.parse(await readFile(new URL("data.json", art), "utf8")).project;

const stamp = project[3].find(([name]) => name === "Stamp");
const sheet = project[6].find(([name]) => name === "Stamp");
// Event-sheet locals are [1, name, type, initialValue, …].
const locals = Object.fromEntries(sheet[1]
  .filter((node) => node[0] === 1 && typeof node[1] === "string")
  .map((node) => [node[1], node[3]]));

const sheets = [...new Set(stamp[7].flatMap((a) => a[7].map((f) => f[0])))].sort();

// Origins are irregular ratios, so keep the shortest decimal that reparses to
// the identical double rather than inventing a pixel denominator.
const short = (value) => {
  const text = String(value).replace(/^0\./, ".");
  if (Number(text) !== value) throw new Error(`origin ${value} does not round-trip`);
  return text;
};

let frames = 0;
const rows = stamp[7].map((animation) => {
  const [name, fps, loop] = animation;
  // The manifest derives `loop` from the name, so hold Construct to that rule.
  if (loop !== name.includes("-a-")) throw new Error(`${name} breaks the loop-name rule`);
  const cells = animation[7].map((frame) => {
    const [path, , x, y, w, h, , ox, oy] = frame;
    frames += 1;
    const row = [sheets.indexOf(path), x, y, w, h];
    if (oy !== .5) row.push(short(ox), short(oy));
    else if (ox !== .5) row.push(short(ox));
    return `[${row.join(",")}]`;
  });
  return `  ${JSON.stringify(name)}: [${fps}, [${cells.join(",")}]],`;
});

const source = `// Stamp's Construct sprite manifest, recovered whole from nopaint.art/data.json
// by toolchain/nopaint/recover-stamp.mjs. Every animation name, frame rate,
// crop rectangle, and origin below is exact; nothing here is reconstructed.
//
// Rows are [sheet, x, y, w, h, originX = .5, originY = .5], where \`sheet\`
// indexes STAMP_SHEETS. ${frames} frames across ${stamp[7].length} animations.

const frozen = (value) => Object.freeze(value);

export const STAMP_SHEETS = frozen([
${sheets.map((path) => `  "/nopaint.art/${path}",`).join("\n")}
]);

// The Stamp event sheet's own locals. Each handle's count is how many looping
// "@handle-a-N" animations sit beside its still collection.
export const STAMP_USERS = frozen(${JSON.stringify(locals.users)}.split("|"));
export const STAMP_ANIMATION_COUNTS = frozen(
  ${JSON.stringify(locals.usersAnimationCount)}.split("|").map(Number));

const rows = {
${rows.join("\n")}
};

export const stampAnimations = frozen(Object.fromEntries(
  Object.entries(rows).map(([name, [fps, cells]]) => [name, frozen({
    fps,
    loop: name.includes("-a-"),
    frames: frozen(cells.map(([sheet, x, y, w, h, ox = .5, oy = .5]) =>
      frozen({ sheet: STAMP_SHEETS[sheet], x, y, w, h, ox, oy }))),
  })])));

// A handle's still collection plus its looping animations, in sheet order.
export function stampAnimationsFor(user) {
  const still = \`@\${user}\`;
  const count = STAMP_ANIMATION_COUNTS[STAMP_USERS.indexOf(user)] || 0;
  return frozen({
    still: stampAnimations[still],
    loops: frozen(Array.from({ length: count }, (_, index) =>
      stampAnimations[\`\${still}-a-\${index + 1}\`])),
  });
}
`;

await writeFile(out, source);
console.log(`${stamp[7].length} animations, ${frames} frames → ${out.pathname}`);
