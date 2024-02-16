// 3d, 2024.2.15.16.28.02.162
// The most basic 3d environment.

/* #region 🏁 TODO 
  - [-] Optimize API.
  - [x] Set up a simple 3d environment demo.
#endregion */

import { CamDoll } from "../lib/cam-doll.mjs";

let doll, square;

// 🎨 Paint
function paint({ wipe, Camera, Dolly, Form, QUAD, CUBEL, form }) {
  const fov = 80;

  doll ||= new CamDoll(Camera, Dolly, {
    fov,
    z: 0,
    y: 0,
    sensitivity: 0.002,
  });

  square ||= new Form(
    QUAD,
    // CUBEL,
    { color: [255, 0, 0, 255] },
    { pos: [0, 0.5, -4], rot: [0, 0, 0], scale: 1 },
  );

  wipe("gray").ink().line();
  form(square, doll.cam, { cpu: true });
}

// 🎪 Act
function act({ event: e }) {
  doll?.act(e);
}

// 🧮 Sim
function sim() {
  doll?.sim();
}

export { paint, act, sim };