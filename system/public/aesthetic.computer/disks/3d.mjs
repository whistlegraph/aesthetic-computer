// 3d, 2024.2.15.16.28.02.162
// The most basic 3d environment.

/* #region 🏁 TODO 
  - [x] Resize doesn't adjust camera...
  - [] Abstract this into some kind of FPS system?
#endregion */

import { CamDoll } from "../lib/cam-doll.mjs";

let doll, cube;

function boot({ Camera, Dolly, CUBEL, QUAD, Form }) {
  doll = new CamDoll(Camera, Dolly, {
    fov: 80,
    z: 0,
    y: 0,
    sensitivity: 0.002,
  });

  cube = new Form(
    CUBEL, // or try `QUAD`
    // { color: [0, 255, 0, 255] },
    { pos: [0, 0.5, -4], rot: [0, 0, 0], scale: 1 },
  );
}

// 🎨 Paint
function paint({ wipe }) {
  wipe("gray").ink("darkgrey").line().ink("red").form(cube, doll.cam);
}

// 🎪 Act
function act({ event: e }) {
  doll?.act(e);
}

// 🧮 Sim
function sim() {
  doll?.sim();
}

export { boot, paint, act, sim };
