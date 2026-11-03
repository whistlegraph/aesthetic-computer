// Object, 2026.11.02
// `object:8en` opens a KidLisp object by its $code on the turntable in
// lib/object/viewer.mjs. Opening `$8en` directly does the same and keeps the
// URL: disk.mjs hands any KidLisp that draws in 3D to the same viewer.

import { objectPiece } from "../lib/object/viewer.mjs";

let piece, status = "loading";

async function boot(api) {
  // object:8en arrives as colon; object~8en as params.
  const code = (api.colon?.[0] || api.params?.[0] || "").replace(/^\$/, "");
  if (!/^[A-Za-z0-9]{2,16}$/.test(code)) return void (status = "usage: object:$code");
  try {
    const { source } = await (await fetch(`/api/store-kidlisp?code=${code}`)).json();
    piece = objectPiece(source, `$${code}`);
    piece.boot(api);
  } catch (e) {
    status = `could not load $${code}`;
  }
}

function paint(api) {
  if (piece) return piece.paint(api);
  api.wipe(20, 23, 32).ink(138, 143, 158).write(status, { x: 6, y: api.screen.height - 16 });
}

function act(api) {
  piece?.act(api);
}

function sim(api) {
  piece?.sim(api);
}

function leave(api) {
  piece?.leave(api);
}

function meta() {
  return { title: "Object", desc: "A turntable for KidLisp objects: object:$code." };
}

export { boot, paint, act, sim, leave, meta };
