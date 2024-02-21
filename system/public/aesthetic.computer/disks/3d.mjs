// 3d, 2024.2.15.16.28.02.162
// The most basic 3d environment.

/* #region 🏁 TODO 
  - [🟠] Optimize API.
    - [x] 3D Graphics should use the `ink` color by default if a color isn't
        specified.
  - [x] Set up a simple 3d environment demo.
#endregion */

import { CamDoll } from "../lib/cam-doll.mjs";

let doll, cube;

function boot({ Camera, Dolly, CUBEL, Form }) {
  doll = new CamDoll(Camera, Dolly, {
    fov: 80,
    z: 0,
    y: 0,
    sensitivity: 0.002,
  });

  cube = new Form(
    // QUAD,
    CUBEL,
    // { color: [0, 255, 0, 255] },
    { pos: [0, 0.5, -4], rot: [0, 0, 0], scale: 1 },
  );
}

// 🎨 Paint
function paint({ wipe, form }) {
  wipe("gray").ink("darkgrey").line().ink();
  form(cube, doll.cam, { cpu: true });
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
