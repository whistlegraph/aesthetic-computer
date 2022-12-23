// Plot, 2021.12.05.13.27
// A tool for editing pixel-perfect vector art / glyphs, and icons.

// TODO for July, 2022 version...
// 1. Fix `aesthetic.computer/plot`
// 2. Allow plot to have a custom grid size both from prompt and in url.
// 3. Labels for save and load buttons.
// 4. Make the mouse cursor appear.
// 5. Make the layout flexible.
//    - Center the grid.
// 6. Make an animation preview. 
// 7. Add exporter for drawings in common format.
//    - Vector option (.SVG)
//    - Raster option (.PNG)
//    - Animation (.GIF or .APNG or .WEBP)
// TODO next version...
// 1. Add freehand / drag mode for making larger drawings?
// 2. Add anchor point to be used for the initial pan and also rotation.

const { min, floor } = Math;

import { Typeface } from "../lib/type.mjs";
import { font1 } from "../disks/common/fonts.mjs";

let g; // Our virtual drawing guide.
let save; // A button to save drawings.
let open; // ..and to open them.
let opening = false; // Disables open button if in the process of uploading.

// For tracking and storing each line as its drawn.
let startMark = false;
let currentLine;
const points = [],
  commands = [];

// TODO: Can eventually be shifted around somehow with the javascript console or
// a mouse and then reprinted for pasting back in. 2021.12.10.23.02
const colors = {
  background: [0, 30, 0],
  grid: [0, 70, 0],
  gridOutline: [255, 255, 0, 32],
  lines: [0, 120, 220, 150],
  innerLine: [128, 128, 0, 200],
  inlinePreview: [128, 128, 0, 64],
  activeSquareInline: [255, 128],
  activeSquareCenter: [0, 255, 0],
  ghostSquare: [100, 50],
  save: [255, 0, 0, 80],
  open: [0, 0, 255, 80],
};

const plots = {}; // Stored preloaded drawings.

let width = 13; // Starting size.
let height = 13;

// TODO: Add query params to plot starting size.

let scale = 5;

const abc123Baseline = 8;
const typography = false; // Enabled or disables the baseline.

let typeface;

// 🥾 Boot (Runs once before first paint and sim)
function boot({
  resize,
  cursor,
  geo: { Grid },
  ui: { Button },
  net: { host, preload },
  query,
  screen,
  needsPaint,
}) {
  typeface = new Typeface(preload, font1);
  // cursor("tiny"); // TODO: Why doesn't cursor tiny work here? 22.11.01.16.59
  resize(96, 96); // It might have to do with this resize call?

  // Read some basic query parameters for configuring the resolution.
  //const params = new URLSearchParams(query);
  //width = params.get("width") || width;
  //height = params.get("height") || height;
  //scale = params.get("scale") || scale;

  width = query?.[0] || width;
  height = query?.[1] || height;

  const btnW = 15;
  const gap = 8;

  const gridWidth = width * scale;
  const gridHeight = height * scale;

  const gridX = (screen.width / 2) - gridWidth / 2; 
  const gridY = (screen.height / 2) - gridHeight / 2; 

  g = new Grid(gridX, gridY, width, height, scale);
  open = new Button(gap, screen.height - gap - 2, btnW, 6);
  save = new Button(screen.width - btnW - gap, screen.height - gap - 2, btnW, 6);
  needsPaint();
  // preload("drawings/default.json").then(decode); // Preload drawing.
  // Preload save button icon.
  preload("aesthetic.computer/disks/drawings/save_open_icon.json").then(
    (r) => {
      plots.icon = r;
      needsPaint();
    }
  );
}

// 🎨 Paint (Runs once per display refresh rate)
function paint({
  pen,
  pan,
  unpan,
  grid,
  line,
  painting,
  wipe,
  ink,
  point,
  screen,
  num: { randIntRange }
}) {
  // A. 🌟 Grid
  // Clear the background and draw a grid with an outline.
  wipe(colors.background)
    .ink(colors.grid)
    .grid(g)
    .ink(colors.gridOutline)
    .box(g.scaled, "outline");

  // Add text on the top...
  ink(
    randIntRange(200, 250),
    randIntRange(200, 250),
    randIntRange(200, 250),
    150
  ).printLine("Amalia Plot :)", typeface.glyphs, 2, 2, 6, 1, 0);

  // Render all added lines by generating a bitmap and projecting it on a grid.
  if (commands.length > 0) {
    grid(
      g,
      painting(g.box.w, g.box.h, (p) => {
        p.ink(colors.lines);
        commands.forEach((c) => {
          switch (c.length) {
            case 2:
              p.point(...c);
              break;
            case 4:
              p.line(...c);
              break;
          }
        });
      })
    );
  }

  // Outline the active square and highlight its center point.
  const sq = g.under(pen || {x: 0, y: 0}, (sq) => {
    ink(colors.activeSquareInline).box(sq, "inline");
    g.centers.forEach((p) =>
      ink(colors.activeSquareCenter).point(sq.x + p.x, sq.y + p.y)
    );
  });

  // Draw thin line for all previously added lines.
  pan(g.centerOffset);
  ink(colors.innerLine);
  commands.forEach((c) => {
    switch (c.length) {
      case 2:
        point(...g.get(c[0], c[1]));
        break;
      case 4:
        line(...g.get(c[0], c[1]), ...g.get(c[2], c[3]));
        break;
    }
  });

  if (startMark) {
    // Inline preview between grid squares.
    ink(colors.inlinePreview)
      .line(points[0].x, points[0].y, sq.x, sq.y)
      .unpan();
    // Extended, virtual grid square if we are outside the grid.
    if (!sq.in) ink(colors.ghostSquare).box(sq, "inline");
  } else unpan();

  // Render typographic guides.
  if (typography) {
    const y = g.scaled.y + abc123Baseline * g.scale;
    ink(255, 200, 200, 20).line(0, y, screen.width, y);
  }

  // B. 🌟 Open Button
  ink(colors.open).box(open.box, open.down ? "in" : "out"); // Border
  ink(colors.open).draw(plots.icon, open.box.x + 13, open.box.y + 6, 3, 180); // Icon

  // C. 🌟 Save Button
  ink(colors.save).box(save.box, save.down ? "in" : "out"); // Border
  ink(colors.save).draw(plots.icon, save.box.x + 1, save.box.y, 3); // Icon

  return false;
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, download, upload, num: { timestamp }, needsPaint }) {

  // Undo a step!
  if (e.is("keyboard:down:u")) {
    commands.pop();
  }

  // Add first point if we touch in the grid.
  if (e.is("touch")) {
    g.under(e, (sq) => {
      points.push(sq);
      startMark = true;
    });
  }

  // if (e.is("draw")) {}

  // Add 2nd point to complete the line if we lifted up in a different square.
  if (e.is("lift") && startMark) {
    startMark = false;
    g.under(e, (sq) => {
      if (sq.gx === points[0].gx && sq.gy === points[0].gy) {
        // If we only touched one point then add a point into the commands list.
        commands.push([points[0].gx, points[0].gy]);
      } else {
        // If we made a line by dragging between two points then add a line.
        points.push(sq);
        commands.push([points[0].gx, points[0].gy, points[1].gx, points[1].gy]);
      }
      points.length = 0;
    });
  }

  // Relay event info to the save button.
  save.act(e, () => download(...encode(timestamp())));

  if (!opening) {
    open.act(e, () => {
      upload(".json")
        .then((data) => {
          decode(JSON.parse(data));
          needsPaint();
          opening = false;
        })
        .catch((err) => {
          console.error("JSON load error:", err);
        });
      opening = true;
    });
  }

  needsPaint();
}

// 📚 Library (Useful functions used throughout the program)

// Encode all drawing data (lines) into a single file format.
function encode(filename) {
  // Use JSON to build an AST. 2021.12.11.00.02
  filename += ".json";

  // Create a simple JSON format that is indented by 2 characters.
  const data = JSON.stringify(
    {
      resolution: [g.box.w, g.box.h],
      date: new Date().toISOString(),
      commands: commands.map((args, i) => {
        let name;
        switch (args.length) {
          case 2:
            name = "point";
            break;
          case 4:
            name = "line";
            break;
        }
        return { name, args };
      }),
    },
    null,
    2
  );

  return [ filename, data ];

  // *Future Plans*
  // TODO: Use a custom file format instead of JSON? 2021.12.11.19.02

  // What should the syntax or programmability of this format be?
  /* Maybe something like?

  16x16
  C 255 0 0
  L 1 2 11 11
  L 10 2 1 13
  */

  // Or I could use turtle graphics?

  // TODO: Save this somehow on the network? 2021.12.11.17.01
  // - To clipboard? (Get general clipboard access working.)
  // - Directly on-chain?
  // - On my own server via or Pinata / DO Spaces using web3.eth as auth?

  // Fetch example:
  // This would have to be modified to fit both production and development
  // environments. 2021.12.11.19.07
  /*
  (async () => {
    const rawResponse = await fetch("https://aesthetic.computer/post", {
      method: "POST",
      headers: {
        Accept: "application/json",
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ a: 1, b: "Textual content" }),
    });
    const content = await rawResponse.json();

    console.log(content);
  })();
  */
}

// Read preparsed json data to step through the commands and fill in "lines".
function decode(drawing) {
  commands.length = 0; // Reset the drawing's line data.

  // Repopulate it with the loaded drawing.
  drawing.commands.forEach(({ name, args }) => {
    if (name === "line") commands.push(args);
    else if (name === "point") commands.push(args);
  });
}

export { boot, paint, act };
