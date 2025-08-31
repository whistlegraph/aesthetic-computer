// kidlisp-in-js.mjs, 24.08.31.17.35
// Simple split-screen KidLisp showcase

function paint({ kidlisp, wipe, screen }) {
  wipe("black");

  // Split screen in half
  const halfWidth = Math.floor(screen.width / 2);
  const fullHeight = screen.height;

  // Left half: Rainbow lines pattern
  kidlisp(0, 0, halfWidth, fullHeight, "(wipe black)");

  // Right half: Animated color cycling with timer
  kidlisp(
    halfWidth,
    0,
    halfWidth,
    fullHeight,
    "(0.5s... (wipe red) (wipe blue)) (ink rainbow) (line 0 0 100 100)",
  );
}

export { paint };
