// Login Pattern, 23.03.09.16.52
// This piece is used as a backdrop for the auth0 login screen.

/* #region 📓 TODO 
#endregion */

// 🥾
function boot({ hud, fps, cursor }) {
  cursor("none");
  hud.label(); // Clear any label.
}

// 🎨
function paint({ noise16Aesthetic, screen }) {
  noise16Aesthetic().ink(0, 100).box(0, 0, screen.width, screen.height);
}

export { boot, paint };
