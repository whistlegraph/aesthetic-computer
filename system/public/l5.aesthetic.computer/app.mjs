import { createTryPage } from "../aesthetic.computer/lib/try/shared-page.mjs";

const AC_ORIGIN = "https://aesthetic.computer";

const EXAMPLES = [
  {
    id: "pulse",
    title: "Pulse Circle",
    code: `function setup()\n  size(256, 256)\nend\n\nfunction draw()\n  background(12, 12, 18)\n  local r = 40 + math.sin(frameCount * 0.05) * 20\n  fill(255, 120, 80)\n  circle(width / 2, height / 2, r * 2)\nend`,
  },
  {
    id: "mouse",
    title: "Mouse Dots",
    code: `function setup()\n  background(255)\nend\n\nfunction draw()\n  if mouseIsPressed then\n    fill(20, 20, 20)\n    circle(mouseX, mouseY, 10)\n  end\nend`,
  },
  {
    id: "grid",
    title: "Grid Drift",
    code: `function setup()\n  size(256, 256)\nend\n\nfunction draw()\n  background(245)\n  stroke(0)\n  noFill()\n  for y = 16, height - 16, 16 do\n    for x = 16, width - 16, 16 do\n      local wobble = math.sin((x + y + frameCount) * 0.04) * 3\n      circle(x + wobble, y, 6)\n    end\n  end\nend`,
  },
  {
    id: "hello",
    title: "L5 Hello",
    code: `function setup()\n  size(256, 256)\n  noStroke()\nend\n\nfunction draw()\n  background(16, 16, 24)\n\n  local pulse = 48 + math.sin(frameCount * 0.05) * 18\n  fill(255, 190, 0)\n  circle(width / 2, height / 2, pulse * 2)\n\n  fill(255, 255, 255)\n  text("L5 -> AC", 16, 22)\n\n  if mouseIsPressed then\n    fill(255, 255, 255)\n    circle(mouseX, mouseY, 10)\n  end\nend`,
  },
  {
    id: "l5-shapes",
    title: "Shape Primitives (L5)",
    sourceUrl: "https://l5lua.org/examples/shapes-and-color-shape-primitives/",
    code: `function setup()\n  size(720, 400)\nend\n\nfunction draw()\n  background(220)\n\n  square(20, 20, 100)\n  rect(100, 40, 200, 100)\n\n  ellipse(540, 100, 300, 100)\n  circle(560, 100, 100)\n\n  line(20, 200, 200, 350)\n  triangle(250, 350, 350, 200, 450, 350)\n  quad(500, 250, 550, 200, 700, 300, 650, 350)\n\n  noLoop()\nend`,
  },
  {
    id: "l5-doodle",
    title: "Doodle Draw (L5)",
    sourceUrl: "https://l5lua.org/examples/doodle-draw/",
    code: `function setup()\n  size(800, 600)\n  background(20, 24, 60)\n  strokeWeight(5)\nend\n\nfunction mouseDragged()\n  line(mouseX, mouseY, pmouseX, pmouseY)\nend\n\nfunction mousePressed()\n  stroke(random(255), random(255), random(255))\nend`,
  },
  {
    id: "l5-10print",
    title: "10 Print (L5)",
    sourceUrl: "https://l5lua.org/examples/10print/",
    code: `local block = 20\n\nfunction setup()\n  size(600, 600)\n  frameRate(1)\nend\n\nfunction draw()\n  background(255)\n  block = math.floor(random(10, 140))\n  if block < 8 then block = 8 end\n\n  for y = 1, height, block do\n    for x = 1, width, block do\n      if random() < 0.5 then\n        line(x, y, x + block, y + block)\n      else\n        line(x + block, y, x, y + block)\n      end\n    end\n  end\nend`,
  },
  {
    id: "l5-events",
    title: "Animation Events (L5)",
    sourceUrl: "https://l5lua.org/examples/animation-and-variables-animation-with-events/",
    code: `local x = 25\n\nfunction setup()\n  size(720, 400)\n  textSize(20)\n  noLoop()\nend\n\nfunction draw()\n  background(0)\n  fill((x / 3) % 255, 90, 90)\n  circle(x, height / 2, 50)\n\n  x = x + 5\n  if x > width + 25 then\n    x = -25\n  end\nend\n\nfunction mousePressed()\n  if isLooping() then\n    noLoop()\n  else\n    loop()\n  end\nend\n\nfunction keyPressed()\n  redraw()\nend`,
  },
  {
    id: "l5-conditions",
    title: "Conditions (L5)",
    sourceUrl: "https://l5lua.org/examples/animation-and-variables-conditions/",
    code: `local x = 200\nlocal y = 200\nlocal vx = 2\nlocal vy = 3\nlocal radius = 25\nlocal hue = 0\n\nfunction setup()\n  size(400, 400)\n  background(255)\n\n  noStroke()\n  fill(128)\n  rect(0, 0, 100, height)\n  rect(300, 0, 100, height)\n\n  strokeWeight(4)\nend\n\nfunction draw()\n  stroke(hue, 80, 200)\n\n  if x >= 100 and x <= 300 then\n    fill(0)\n  else\n    fill(255)\n  end\n\n  circle(x, y, radius * 2)\n\n  if mouseIsPressed then\n    x = x + vx\n    y = y + vy\n    hue = (hue + 1) % 255\n  end\n\n  if x < radius or x > width - radius then\n    vx = -vx\n  end\n\n  if y < radius or y > height - radius then\n    vy = -vy\n  end\nend`,
  },
];

const L5_DOCS = {
  setup: { sig: "function setup()", desc: "Called once when the sketch starts. Use for initialization, size(), and initial state.", cat: "Lifecycle" },
  draw: { sig: "function draw()", desc: "Called every frame. Put your rendering and animation logic here.", cat: "Lifecycle" },
  keyPressed: { sig: "function keyPressed()", desc: "Called once when a key is pressed. Use `key` and `keyCode` globals to read which key.", cat: "Lifecycle" },
  keyReleased: { sig: "function keyReleased()", desc: "Called once when a key is released.", cat: "Lifecycle" },
  mousePressed: { sig: "function mousePressed()", desc: "Called once when the mouse button or touch is pressed.", cat: "Lifecycle" },
  mouseReleased: { sig: "function mouseReleased()", desc: "Called once when the mouse button or touch is released.", cat: "Lifecycle" },
  mouseMoved: { sig: "function mouseMoved()", desc: "Called when the mouse moves without a button pressed.", cat: "Lifecycle" },
  mouseDragged: { sig: "function mouseDragged()", desc: "Called when the mouse moves with a button pressed.", cat: "Lifecycle" },
  size: { sig: "size(w, h?)", desc: "Set the sketch resolution. Call in setup(). Height defaults to width if omitted.", cat: "Canvas" },
  background: { sig: "background(r, g?, b?, a?)", desc: "Clear the canvas with a color. Single arg = grayscale. Three args = RGB.", cat: "Graphics" },
  clear: { sig: "clear()", desc: "Clear the canvas to transparent black.", cat: "Graphics" },
  fill: { sig: "fill(r, g?, b?, a?)", desc: "Set the fill color for subsequent shapes. Single arg = grayscale.", cat: "Graphics" },
  noFill: { sig: "noFill()", desc: "Disable fill for subsequent shapes (outline only).", cat: "Graphics" },
  stroke: { sig: "stroke(r, g?, b?, a?)", desc: "Set the stroke (outline) color for subsequent shapes.", cat: "Graphics" },
  noStroke: { sig: "noStroke()", desc: "Disable stroke for subsequent shapes (fill only).", cat: "Graphics" },
  strokeWeight: { sig: "strokeWeight(weight)", desc: "Set the thickness of lines and shape outlines in pixels.", cat: "Graphics" },
  point: { sig: "point(x, y)", desc: "Draw a single pixel at (x, y).", cat: "Graphics" },
  line: { sig: "line(x1, y1, x2, y2)", desc: "Draw a line between two points.", cat: "Graphics" },
  rect: { sig: "rect(x, y, w, h)", desc: "Draw a rectangle. (x, y) is the top-left corner.", cat: "Graphics" },
  square: { sig: "square(x, y, size)", desc: "Draw a square. Shorthand for rect(x, y, size, size).", cat: "Graphics" },
  circle: { sig: "circle(x, y, diameter)", desc: "Draw a circle centered at (x, y) with the given diameter.", cat: "Graphics" },
  ellipse: { sig: "ellipse(x, y, w, h)", desc: "Draw an ellipse centered at (x, y).", cat: "Graphics" },
  triangle: { sig: "triangle(x1, y1, x2, y2, x3, y3)", desc: "Draw a triangle between three points.", cat: "Graphics" },
  quad: { sig: "quad(x1, y1, x2, y2, x3, y3, x4, y4)", desc: "Draw a quadrilateral between four points.", cat: "Graphics" },
  text: { sig: "text(value, x, y)", desc: "Draw text at (x, y). Uses the current fill color.", cat: "Text" },
  textSize: { sig: "textSize(size)", desc: "Set the text size scale multiplier.", cat: "Text" },
  textWidth: { sig: "textWidth(value) -> number", desc: "Measure the pixel width of a string at the current text size.", cat: "Text" },
  frameRate: { sig: "frameRate(fps)", desc: "Request a target frame rate (frames per second).", cat: "Loop Control" },
  noLoop: { sig: "noLoop()", desc: "Stop the draw loop. The sketch freezes on the last frame.", cat: "Loop Control" },
  loop: { sig: "loop()", desc: "Resume the draw loop after noLoop().", cat: "Loop Control" },
  isLooping: { sig: "isLooping() -> boolean", desc: "Returns true if the draw loop is running.", cat: "Loop Control" },
  redraw: { sig: "redraw()", desc: "Request a single draw() call. Useful when looping is off.", cat: "Loop Control" },
  mouseX: { sig: "mouseX", desc: "Current mouse/touch X coordinate.", cat: "Input" },
  mouseY: { sig: "mouseY", desc: "Current mouse/touch Y coordinate.", cat: "Input" },
  pmouseX: { sig: "pmouseX", desc: "Previous frame mouse X coordinate.", cat: "Input" },
  pmouseY: { sig: "pmouseY", desc: "Previous frame mouse Y coordinate.", cat: "Input" },
  movedX: { sig: "movedX", desc: "Mouse X movement delta since last frame.", cat: "Input" },
  movedY: { sig: "movedY", desc: "Mouse Y movement delta since last frame.", cat: "Input" },
  mouseIsPressed: { sig: "mouseIsPressed", desc: "True while any mouse button or touch is held down.", cat: "Input" },
  key: { sig: "key", desc: "The value of the last key pressed (string).", cat: "Input" },
  keyCode: { sig: "keyCode", desc: "The numeric code of the last key pressed.", cat: "Input" },
  keyIsPressed: { sig: "keyIsPressed", desc: "True while any key is held down.", cat: "Input" },
  frameCount: { sig: "frameCount", desc: "Number of frames drawn since the sketch started.", cat: "Screen" },
  width: { sig: "width", desc: "Current sketch width in pixels.", cat: "Screen" },
  height: { sig: "height", desc: "Current sketch height in pixels.", cat: "Screen" },
  deltaTime: { sig: "deltaTime", desc: "Milliseconds elapsed since the last frame.", cat: "Screen" },
  focused: { sig: "focused", desc: "Always true in the AC runtime.", cat: "Screen" },
  random: { sig: "random(max?) or random(min, max)", desc: "Generate a random number. No args = 0..1. One arg = 0..max. Two args = min..max.", cat: "Math" },
  map: { sig: "map(value, inMin, inMax, outMin, outMax)", desc: "Remap a number from one range to another.", cat: "Math" },
  dist: { sig: "dist(x1, y1, x2, y2) -> number", desc: "Euclidean distance between two points.", cat: "Math" },
  lerp: { sig: "lerp(start, stop, amount) -> number", desc: "Linear interpolation between two values. amount is 0..1.", cat: "Math" },
  constrain: { sig: "constrain(value, min, max) -> number", desc: "Clamp a value to the range [min, max].", cat: "Math" },
  radians: { sig: "radians(degrees) -> number", desc: "Convert degrees to radians.", cat: "Math" },
  degrees: { sig: "degrees(radians) -> number", desc: "Convert radians to degrees.", cat: "Math" },
  millis: { sig: "millis() -> number", desc: "Milliseconds elapsed since the sketch started.", cat: "Math" },
  PI: { sig: "PI = 3.14159...", desc: "The ratio of a circle's circumference to its diameter.", cat: "Constants" },
  HALF_PI: { sig: "HALF_PI = PI / 2", desc: "Half of PI (90 degrees in radians).", cat: "Constants" },
  QUARTER_PI: { sig: "QUARTER_PI = PI / 4", desc: "Quarter of PI (45 degrees in radians).", cat: "Constants" },
  TWO_PI: { sig: "TWO_PI = PI * 2", desc: "Two times PI (360 degrees in radians).", cat: "Constants" },
  TAU: { sig: "TAU = PI * 2", desc: "Alias for TWO_PI.", cat: "Constants" },
  LEFT: { sig: "LEFT", desc: "Alignment constant for left-aligned text.", cat: "Constants" },
  RIGHT: { sig: "RIGHT", desc: "Alignment constant for right-aligned text.", cat: "Constants" },
  CENTER: { sig: "CENTER", desc: "Alignment constant for centered text.", cat: "Constants" },
  CORNER: { sig: "CORNER", desc: "Shape mode: (x, y) is the top-left corner.", cat: "Constants" },
  CLOSE: { sig: "CLOSE", desc: "Used with beginShape/endShape to close the shape.", cat: "Constants" },
  RGB: { sig: "RGB", desc: "Color mode constant for red/green/blue.", cat: "Constants" },
  HSB: { sig: "HSB", desc: "Color mode constant for hue/saturation/brightness.", cat: "Constants" },
  HSL: { sig: "HSL", desc: "Color mode constant for hue/saturation/lightness.", cat: "Constants" },
  print: { sig: "print(...args)", desc: "Log values to the browser console.", cat: "Debug" },
  println: { sig: "println(...args)", desc: "Log values to the browser console (alias for print).", cat: "Debug" },
};

await createTryPage({
  acOrigin: AC_ORIGIN,
  mountId: "try-page-root",
  page: {
    title: "Try L5 on AC",
    description: "L5 (Processing-style Lua) live playground and API explorer for Aesthetic Computer.",
  },
  brand: {
    heading: "Try L5 on AC",
    logoUrl: "https://aesthetic.computer/l5.aesthetic.computer/l5-logo-blob.png",
    logoAlt: "L5 logo",
    heroLink: {
      href: "https://l5lua.org/",
      label: "l5lua.org",
      title: "Official L5 docs by l5lua.org",
    },
    footerLinks: [
      { href: "https://l5lua.org/", label: "L5 by the L5 community" },
      { href: "https://aesthetic.computer", label: "Aesthetic Computer" },
    ],
  },
  runtime: {
    framePath: "/l5-hello.lua",
    openPath: "/l5-hello.lua",
    readyTypes: ["ready", "kidlisp-ready"],
    buildReloadMessage: (code) => ({
      type: "l5-reload",
      code,
      ext: "lua",
      liveName: "l5-live",
    }),
  },
  source: {
    defaultUrl: "https://l5lua.org/examples/",
    labelForExample: (example) => (example.sourceUrl ? "L5 Source" : "Source"),
    urlForExample: (example, fallback) => example.sourceUrl || fallback,
  },
  languageLabel: "Lua",
  helperText: "Live reload on edit. Cmd/Ctrl+Enter to force run. Zoom: Cmd/Ctrl +/-/0 in editor.",
  defaultDocMessage: "Hover over a function to see its docs.",
  fontFacesCss: `
    @font-face {
      font-family: "YWFTProcessing-Regular";
      src: url("https://aesthetic.computer/type/webfonts/ywft-processing-regular.woff2") format("woff2");
      font-weight: normal;
      font-style: normal;
      font-display: swap;
    }

    :root {
      --try-font-heading: "YWFTProcessing-Regular", "Berkeley Mono Variable", monospace;
    }
  `,
  themeVars: {
    light: {
      "--try-bg": "#ffffff",
      "--try-ink": "#111111",
      "--try-ink-soft": "#2b2b2b",
      "--try-line": "#111111",
      "--try-panel": "rgba(255, 255, 255, 0.95)",
      "--try-control-bg": "#ffffff",
      "--try-head-bg": "#ffee57",
      "--try-code-bg": "#000000",
      "--try-code-ink": "#ffd700",
      "--try-accent": "#ffd700",
      "--try-accent-soft": "#ffee57",
      "--try-dot-a": "rgba(255, 215, 0, 0.32)",
      "--try-dot-b": "rgba(255, 215, 0, 0.16)",
      "--try-ok": "#127a3f",
      "--try-danger": "#8f1f1f",
    },
    dark: {
      "--try-bg": "#0a0a0a",
      "--try-ink": "#f4f4f4",
      "--try-ink-soft": "#c8c8c8",
      "--try-line": "#f6d648",
      "--try-panel": "rgba(14, 14, 14, 0.92)",
      "--try-control-bg": "#121212",
      "--try-head-bg": "#2a250a",
      "--try-code-bg": "#020202",
      "--try-code-ink": "#f6d648",
      "--try-accent": "#f6d648",
      "--try-accent-soft": "#685b1b",
      "--try-dot-a": "rgba(246, 214, 72, 0.18)",
      "--try-dot-b": "rgba(246, 214, 72, 0.09)",
      "--try-ok": "#62d08e",
      "--try-danger": "#ff9f9f",
    },
  },
  monaco: {
    language: "lua",
    fontFamily: "Berkeley Mono Variable, Menlo, monospace",
    fontSizeDefault: 13,
    fontSizeMin: 8,
    fontSizeMax: 32,
    liveDebounceMs: 400,
    liveReloadPort: 8889,
  },
  examples: EXAMPLES,
  docs: L5_DOCS,
  onLogoError: (img) => {
    img.src = "https://aesthetic.computer/l5.aesthetic.computer/l5-logo-blob.png";
  },
});
