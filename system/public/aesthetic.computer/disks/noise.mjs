// Noise, 2023.9.01.00.09.16.589
// Display noise on every pixel.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

// 🎨 Paint
function paint({ noise16DIGITPAIN, display, screen, resolution }) {
  resolution(display.width / 4, display.height / 4, 0);
  noise16DIGITPAIN();
}

// 📰 Meta
function meta() {
  return {
    title: "Noise",
    desc: "Display noise on every pixel.",
  };
}

export { paint, meta };
