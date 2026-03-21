import { createTryPage } from "../aesthetic.computer/lib/try/shared-page.mjs";
import { compileProcessingSourceForRuntime } from "../aesthetic.computer/lib/try/processing-transpile.mjs";

const AC_ORIGIN = "https://aesthetic.computer";

const EXAMPLES = [
  {
    id: "linear-motion",
    title: "Linear Motion",
    sourceUrl: "https://processing.org/examples/linear.html",
    code: `/**
 * Linear Motion.
 *
 * Changing a variable to create a moving line.
 * When the line moves off the edge of the window,
 * the variable is set to 0, which places the line
 * back at the bottom of the screen.
 */

float a;

void setup() {
  size(640, 360);
  stroke(255);
  a = height / 2;
}

void draw() {
  background(51);
  line(0, a, width, a);
  a = a - 0.5;
  if (a < 0) {
    a = height;
  }
}`,
  },
  {
    id: "sine",
    title: "Sine",
    sourceUrl: "https://processing.org/examples/sine.html",
    code: `/**
 * Sine.
 *
 * Smoothly scaling size with the sin() function.
 */

float diameter;
float angle = 0;

void setup() {
  size(640, 360);
  diameter = height - 10;
  noStroke();
  fill(255, 204, 0);
}

void draw() {
  background(0);

  float d1 = 10 + (sin(angle) * diameter / 2) + diameter / 2;
  float d2 = 10 + (sin(angle + PI / 2) * diameter / 2) + diameter / 2;
  float d3 = 10 + (sin(angle + PI) * diameter / 2) + diameter / 2;

  ellipse(0, height / 2, d1, d1);
  ellipse(width / 2, height / 2, d2, d2);
  ellipse(width, height / 2, d3, d3);

  angle += 0.02;
}`,
  },
  {
    id: "distance-2d",
    title: "Distance 2D",
    sourceUrl: "https://processing.org/examples/distance2d.html",
    code: `/**
 * Distance 2D.
 *
 * Move the mouse across the image to obscure and reveal the matrix.
 * Measures the distance from the mouse to each square and sets the
 * size proportionally.
 */

float max_distance;

void setup() {
  size(640, 360);
  noStroke();
  max_distance = dist(0, 0, width, height);
}

void draw() {
  background(0);

  for (int i = 0; i <= width; i += 20) {
    for (int j = 0; j <= height; j += 20) {
      float size = dist(mouseX, mouseY, i, j);
      size = size / max_distance * 66;
      ellipse(i, j, size, size);
    }
  }
}`,
  },
  {
    id: "conditionals-1",
    title: "Conditionals 1",
    sourceUrl: "https://processing.org/examples/conditionals1.html",
    code: `/**
 * Conditionals 1.
 *
 * Conditions are like questions.
 * They allow a program to decide to take one action if
 * the answer to a question is "true" or to do another action
 * if the answer to the question is "false."
 */

void setup() {
  size(640, 360);
}

void draw() {
  background(0);

  for (int i = 10; i < width; i += 10) {
    if ((i % 20) == 0) {
      stroke(255);
      line(i, 80, i, height / 2);
    } else {
      stroke(153);
      line(i, 20, i, 180);
    }
  }

  noLoop();
}`,
  },
  {
    id: "random",
    title: "Random",
    sourceUrl: "https://processing.org/examples/random.html",
    code: `/**
 * Random.
 *
 * Random numbers create the basis of this image.
 * Each time the program is loaded the result is different.
 */

void setup() {
  size(640, 360);
  background(0);
  strokeWeight(20);
  frameRate(2);
}

void draw() {
  for (int i = 0; i < width; i++) {
    float r = random(255);
    stroke(r);
    line(i, 0, i, height);
  }
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
  createCanvas: {
    sig: "createCanvas(width, height)",
    desc: "p5.js alias for size(width, height) in Processing v0 mode.",
    cat: "Canvas",
  },
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
  helperText:
    "Canonical Processing examples from processing.org, transpiled to Lua for the AC L5 runtime. p5-style function/createCanvas syntax also works. Cmd/Ctrl+Enter runs, Cmd/Ctrl+S copies a share link.",
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
