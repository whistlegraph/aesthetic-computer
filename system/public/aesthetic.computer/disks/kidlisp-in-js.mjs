// kidlisp-in-js.mjs, 24.08.31.17.35
// Simple split-screen KidLisp showcase with $code shorthand support

function paint({ kidlisp, wipe, screen }) {
  wipe("gray");
  const hw = Math.floor(screen.width / 2);
  const h = screen.height;
  kidlisp(0, 0, hw, h, "(wipe red) (ink blue) (repeat 10 line) (blur 2) (zoom 2)"); // Left half: Blue backdrop using inline KidLisp
  kidlisp(hw, 0, hw, h, "(green) (ink) (repeat 30 line) (blur 1)"); // Left half: Blue backdrop using inline KidLisp
  // kidlisp(hw, 0, hw, h, "$bop"); // Right half: Test $code shorthand 
}
