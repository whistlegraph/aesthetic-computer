// kidlisp-in-js.mjs, 24.08.31.17.35
// Simple split-screen KidLisp showcase

function paint({ kidlisp, wipe, screen }) {
  wipe("black");

  // Split screen in half
  const hw = Math.floor(screen.width / 2);
  const h = screen.height;

  // Left half: Blue backdrop with white lines
  kidlisp(0, 0, hw, h, "(once (wipe blue))");

  // Right half: Animated color cycling with timer
  kidlisp(
    hw,
    0,
    hw,
    h,
    "(0.5s... (wipe red) (wipe blue)) (ink rainbow) (line 0 0 100 100) (spin 1)",
  );
}

export { paint };
