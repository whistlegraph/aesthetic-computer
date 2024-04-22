// fps, 2024.2.15.16.28.02.162
// The most basic first person environment.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [o] Make sure pointer lock movement is the right numbers.
  - [] Rewrite this module in the lisp as an initial production piece.
  + Done
  - [x] Get Pointer Lock working.
#endregion */

let cube, penLocked = false;

function boot({ Form, CUBEL, penLock }) {
  penLock();
  cube = new Form(
    CUBEL, // or try `QUAD`
    { pos: [0, 0.5, -4], rot: [0, 0, 0], scale: 1 },
  );
}

function paint({ wipe }) {
  wipe("gray").ink("red").form(cube);
}

function act({ event: e, penLock }) {
  if (e.is("pen:locked")) penLocked = true;
  if (e.is("pen:unlocked")) penLocked = false;
  if (!penLocked && e.is("touch")) penLock();
}

export const system = "fps";
export { boot, paint, act };
