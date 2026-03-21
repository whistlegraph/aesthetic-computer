// Noise, 2023.9.01.00.09.16.589
// Display noise on every pixel.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

// 🎨 Paint
function paint({
  noise16DIGITPAIN,
  noiseTinted,
  display,
  screen,
  resolution,
  params,
  hud,
  ink,
  net: { loadFailureText },
}) {
  if (params[0] === "code") {
    noiseTinted(hud.currentStatusColor(), 0.15, 0.1);
    if (loadFailureText) {
      ink("white").write(
        loadFailureText,
        { x: 6, y: 6 },
        [64, 128],
        screen.width - 6,
      );
    }
  } else {
    resolution(display.width / 4, display.height / 4, 0);
    noise16DIGITPAIN();
  }
}

// 📰 Meta
function meta() {
  return {
    title: "Noise",
    desc: "Display noise on every pixel.",
  };
}

export { paint, meta };
