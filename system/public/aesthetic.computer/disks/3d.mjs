// 3d, 2024.2.15.16.28.02.162
// The most basic 3d environment.

/* #region 🏁 TODO 
#endregion */

let cube;

function boot({ CUBEL, Form }) {
  cube = new Form(
    CUBEL, // or try `QUAD`
    { pos: [0, 0.5, -4], rot: [0, 0, 0], scale: 1 },
  );
}

function paint({ wipe }) {
  wipe("gray").ink("darkgrey").line().ink("red").form(cube);
}

export const system = "fps";
export { boot, paint };
