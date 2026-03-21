// Login Pattern, 23.03.09.16.52
// This piece is used as a backdrop for the auth0 login screen.

/* #region 📓 TODO 
#endregion */

// 🥾
function boot({ hud, fps, cursor }) {
  cursor("none"); // No blue cursor.
  hud.label(); // Clear any corner label.
}

// 🎨
function paint({ noise16Aesthetic, screen: { width, height } }) {
  noise16Aesthetic().ink(0, 100).box(0, 0, width, height);
}

export { boot, paint };
