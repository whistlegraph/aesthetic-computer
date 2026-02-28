import { createTryPage } from "../aesthetic.computer/lib/try/shared-page.mjs";
import { compileProcessingSourceForRuntime } from "../aesthetic.computer/lib/try/processing-transpile.mjs";

const AC_ORIGIN = "https://aesthetic.computer";

const EXAMPLES = [
  {
    id: "pulse",
    title: "Pulse Circle",
    code: `void setup() {
  size(256, 256);
}

void draw() {
  background(12, 12, 18);
  float r = 40 + sin(frameCount * 0.05) * 20;
  fill(255, 120, 80);
  circle(width / 2, height / 2, r * 2);
}`,
  },
  {
    id: "mouse",
    title: "Mouse Dots",
    code: `void setup() {
  background(255);
}

void draw() {
  if (mouseIsPressed) {
    fill(20, 20, 20);
    circle(mouseX, mouseY, 10);
  }
}`,
  },
  {
    id: "grid",
    title: "Grid Drift",
    code: `void setup() {
  size(256, 256);
}

void draw() {
  background(245);
  stroke(0);
  noFill();

  for (int y = 16; y <= height - 16; y += 16) {
    for (int x = 16; x <= width - 16; x += 16) {
      float wobble = sin((x + y + frameCount) * 0.04) * 3;
      circle(x + wobble, y, 6);
    }
  }
}`,
  },
  {
    id: "events",
    title: "Event Hooks",
    code: `float x = 24;

void setup() {
  size(320, 180);
}

void draw() {
  background(15, 15, 20);
  fill(245, 220, 72);
  circle(x, height / 2, 26);

  x += 2;
  if (x > width + 24) {
    x = -24;
  }
}

void mousePressed() {
  x = mouseX;
}

void keyPressed() {
  x = 24;
}`,
  },
  {
    id: "l5-parity",
    title: "Processing Shape Primitives",
    sourceUrl: "https://processing.org/examples/shapeprimitives.html",
    code: `void setup() {
  size(360, 220);
}

void draw() {
  background(220);
  square(18, 18, 60);
  rect(96, 26, 110, 58);
  ellipse(268, 70, 120, 62);
  line(24, 145, 155, 198);
  triangle(180, 198, 228, 138, 282, 198);
  quad(296, 154, 320, 128, 352, 160, 336, 198);
  noLoop();
}`,
  },
];

const PROCESSING_DOCS = {
  void: { sig: "void setup() { ... }", desc: "Use Java-style callback declarations. The transpiler converts callbacks into Lua functions.", cat: "Syntax" },
  int: { sig: "int n = 12;", desc: "Typed declarations are converted to local Lua variables.", cat: "Syntax" },
  float: { sig: "float n = 12.5;", desc: "Typed declarations are converted to local Lua variables.", cat: "Syntax" },
  boolean: { sig: "boolean on = true;", desc: "Boolean declarations are converted to local Lua variables.", cat: "Syntax" },
  setup: { sig: "void setup()", desc: "Called once when the sketch starts.", cat: "Lifecycle" },
  draw: { sig: "void draw()", desc: "Called every frame. Put animation and rendering logic here.", cat: "Lifecycle" },
  keyPressed: { sig: "void keyPressed()", desc: "Runs when a key is pressed.", cat: "Lifecycle" },
  keyReleased: { sig: "void keyReleased()", desc: "Runs when a key is released.", cat: "Lifecycle" },
  mousePressed: { sig: "void mousePressed()", desc: "Runs when the mouse or touch begins.", cat: "Lifecycle" },
  mouseReleased: { sig: "void mouseReleased()", desc: "Runs when the mouse or touch ends.", cat: "Lifecycle" },
  mouseDragged: { sig: "void mouseDragged()", desc: "Runs while pointer is down and moving.", cat: "Lifecycle" },
  mouseMoved: { sig: "void mouseMoved()", desc: "Runs when pointer moves without pressing.", cat: "Lifecycle" },
  size: { sig: "size(width, height)", desc: "Set the sketch resolution.", cat: "Canvas" },
  background: { sig: "background(r, g, b, a?)", desc: "Clear the canvas with a color.", cat: "Graphics" },
  fill: { sig: "fill(r, g, b, a?)", desc: "Set fill color for shapes and text.", cat: "Graphics" },
  noFill: { sig: "noFill()", desc: "Disable shape fill.", cat: "Graphics" },
  stroke: { sig: "stroke(r, g, b, a?)", desc: "Set stroke color.", cat: "Graphics" },
  noStroke: { sig: "noStroke()", desc: "Disable stroke rendering.", cat: "Graphics" },
  strokeWeight: { sig: "strokeWeight(px)", desc: "Set line thickness in pixels.", cat: "Graphics" },
  point: { sig: "point(x, y)", desc: "Draw one pixel.", cat: "Graphics" },
  line: { sig: "line(x1, y1, x2, y2)", desc: "Draw a line.", cat: "Graphics" },
  rect: { sig: "rect(x, y, w, h)", desc: "Draw a rectangle.", cat: "Graphics" },
  square: { sig: "square(x, y, size)", desc: "Draw a square.", cat: "Graphics" },
  circle: { sig: "circle(x, y, diameter)", desc: "Draw a circle.", cat: "Graphics" },
  ellipse: { sig: "ellipse(x, y, w, h)", desc: "Draw an ellipse.", cat: "Graphics" },
  triangle: { sig: "triangle(x1, y1, x2, y2, x3, y3)", desc: "Draw a triangle.", cat: "Graphics" },
  quad: { sig: "quad(x1, y1, x2, y2, x3, y3, x4, y4)", desc: "Draw a quadrilateral.", cat: "Graphics" },
  text: { sig: "text(message, x, y)", desc: "Render text.", cat: "Text" },
  textSize: { sig: "textSize(size)", desc: "Set text scale.", cat: "Text" },
  frameRate: { sig: "frameRate(fps)", desc: "Set target frame rate.", cat: "Loop" },
  noLoop: { sig: "noLoop()", desc: "Stop the draw loop.", cat: "Loop" },
  loop: { sig: "loop()", desc: "Resume the draw loop.", cat: "Loop" },
  redraw: { sig: "redraw()", desc: "Request a single draw() pass.", cat: "Loop" },
  frameCount: { sig: "frameCount", desc: "Number of rendered frames.", cat: "Input + State" },
  width: { sig: "width", desc: "Current sketch width in pixels.", cat: "Input + State" },
  height: { sig: "height", desc: "Current sketch height in pixels.", cat: "Input + State" },
  mouseX: { sig: "mouseX", desc: "Current pointer X coordinate.", cat: "Input + State" },
  mouseY: { sig: "mouseY", desc: "Current pointer Y coordinate.", cat: "Input + State" },
  mouseIsPressed: { sig: "mouseIsPressed", desc: "True while pointer is pressed.", cat: "Input + State" },
  key: { sig: "key", desc: "Latest key value.", cat: "Input + State" },
  keyCode: { sig: "keyCode", desc: "Latest key code.", cat: "Input + State" },
  random: { sig: "random(max) or random(min, max)", desc: "Random number helper.", cat: "Math" },
  map: { sig: "map(value, inMin, inMax, outMin, outMax)", desc: "Map one range to another.", cat: "Math" },
  dist: { sig: "dist(x1, y1, x2, y2)", desc: "Distance between two points.", cat: "Math" },
  constrain: { sig: "constrain(value, min, max)", desc: "Clamp a value to a range.", cat: "Math" },
  sin: { sig: "sin(angle)", desc: "Sine helper. Transpiles to math.sin().", cat: "Math" },
  cos: { sig: "cos(angle)", desc: "Cosine helper. Transpiles to math.cos().", cat: "Math" },
  PI: { sig: "PI", desc: "Pi constant.", cat: "Constants" },
  TWO_PI: { sig: "TWO_PI", desc: "2 * PI.", cat: "Constants" },
};

await createTryPage({
  acOrigin: AC_ORIGIN,
  mountId: "try-page-root",
  page: {
    title: "Try Processing on AC",
    description: "Processing-style Java playground for Aesthetic Computer, transpiled to the L5 Lua runtime.",
  },
  brand: {
    heading: "Try Processing on AC",
    logoUrl: "./processing-logo.svg",
    logoAlt: "Processing-inspired logo",
    heroLink: {
      href: "https://processing.org/reference/",
      label: "processing.org/reference",
      title: "Processing language reference",
    },
    footerLinks: [
      { href: "https://processing.org/", label: "Processing" },
      { href: "https://aesthetic.computer", label: "Aesthetic Computer" },
    ],
  },
  runtime: {
    framePath: "/l5-hello.lua",
    openPath: "/l5-hello.lua",
    readyTypes: ["ready", "kidlisp-ready"],
    buildReloadMessage: (source) => ({
      type: "l5-reload",
      code: compileProcessingSourceForRuntime(source),
      ext: "lua",
      liveName: "processing-live",
    }),
  },
  source: {
    defaultUrl: "https://processing.org/examples/",
    labelForExample: (example) => (example.sourceUrl ? "Processing Source" : "Source"),
    urlForExample: (example, fallback) => example.sourceUrl || fallback,
  },
  languageLabel: "Processing v0",
  helperText: "Writes Processing-style Java and transpiles to Lua for the AC L5 runtime. Cmd/Ctrl+Enter to force run.",
  defaultDocMessage: "Hover over a Processing symbol to see v0 docs and runtime notes.",
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
      "--try-bg": "#f5fbff",
      "--try-ink": "#072136",
      "--try-ink-soft": "#29455c",
      "--try-line": "#0f6f9d",
      "--try-panel": "rgba(255, 255, 255, 0.94)",
      "--try-control-bg": "#ffffff",
      "--try-head-bg": "#d8f3ff",
      "--try-code-bg": "#04121a",
      "--try-code-ink": "#9be7ff",
      "--try-accent": "#3ec6f0",
      "--try-accent-soft": "#d8f3ff",
      "--try-dot-a": "rgba(62, 198, 240, 0.17)",
      "--try-dot-b": "rgba(0, 208, 132, 0.13)",
      "--try-ok": "#0f7a4a",
      "--try-danger": "#9b1f3b",
    },
    dark: {
      "--try-bg": "#04131e",
      "--try-ink": "#dbf6ff",
      "--try-ink-soft": "#9bc2d4",
      "--try-line": "#56d3ff",
      "--try-panel": "rgba(6, 20, 31, 0.9)",
      "--try-control-bg": "#061a2a",
      "--try-head-bg": "#0b3143",
      "--try-code-bg": "#010a12",
      "--try-code-ink": "#9be7ff",
      "--try-accent": "#23c5ff",
      "--try-accent-soft": "#0f4d66",
      "--try-dot-a": "rgba(35, 197, 255, 0.16)",
      "--try-dot-b": "rgba(0, 208, 132, 0.08)",
      "--try-ok": "#6ce9b3",
      "--try-danger": "#ffa7bc",
    },
  },
  monaco: {
    language: "java",
    fontFamily: "Berkeley Mono Variable, Menlo, monospace",
    fontSizeDefault: 13,
    fontSizeMin: 8,
    fontSizeMax: 32,
    liveDebounceMs: 450,
    liveReloadPort: 8889,
  },
  examples: EXAMPLES,
  docs: PROCESSING_DOCS,
});
