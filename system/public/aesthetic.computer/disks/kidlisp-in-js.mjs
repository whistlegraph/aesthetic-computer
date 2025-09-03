// kidlisp-in-js.mjs, 24.08.31.17.35
// Simple split-screen KidLisp showcase with $code shorthand support

function paint({ kidlisp, wipe, screen, paintCount, clock, num }) {
  wipe("gray");
  const hw = Math.floor(screen.width / 2);
  const h = screen.height;
  kidlisp(0, 0, hw, h, "(wipe green) (ink blue) (box 20 20 60 60)");
  kidlisp(hw, 0, hw, h, "$air"); // Right half: Test $code shorthand with animation support
}