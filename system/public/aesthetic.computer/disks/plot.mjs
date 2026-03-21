// Plot, 2021.12.05.13.27
// A tool for editing pixel-perfect vector art / glyphs, and icons.

/* #region 🏁 TODO 
  + This Version
  - [💛] Reflow layout based on grid size and resolution.
  - [] Make a dark on light color scheme. Replace "colors".
    - [] Scheme should be neutral and tintable.
    - [] And on frame resize.
    - [] Add zooming and panning to the grid.
  - [] New "Save" and "Load" buttons.
  - [] Add anchor point to the format.
  - [] Make a "move" mode vs. draw mode.
  - [] Add typography / font mode when typing.
    - [] Live preview of characters?
  + Done
  - [x] Fix the "tiny" cursor.
  - [x] Custom grid size in params.
  + Next Version
   - [] Add freehand / drag mode for making larger drawings?
   - [] Add exporter for drawings in common format.
     - [] Vector option (.SVG)
     - [] Raster option (.PNG)
     - [] Animation (.GIF or .APNG or .WEBP)
     - [] Make an animation preview.
     - [] Use a custom file format instead of JSON? 2021.12.11.19.02
      - Could be something like...
        16x16
        C 255 0 0
        L 1 2 11 11
        L 10 2 1 13
#endregion */

import { Typeface } from "../lib/type.mjs";
import { font1 } from "../disks/common/fonts.mjs";
const { floor } = Math;

// Dark on light.
const colors = {
  background: 127,
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

// Light on dark. (green vibe)
// const colors = {
//   background: [0, 30, 0],
//   grid: [0, 70, 0],
//   gridOutline: [255, 255, 0, 32],
//   lines: [0, 120, 220, 150],
//   innerLine: [128, 128, 0, 200],
//   inlinePreview: [128, 128, 0, 64],
//   activeSquareInline: [255, 128],
//   activeSquareCenter: [0, 255, 0],
//   ghostSquare: [100, 50],
//   save: [255, 0, 0, 80],
//   open: [0, 0, 255, 80],
// };

let g, // Our virtual drawing guide.
  save, // A button to save drawings.
  open, // ..and to open them.
  opening = false; // Disables open button if in the process of uploading.

// For tracking and storing each line as its drawn.
let startMark = false;
const points = [],
  commands = [];

const plots = {}; // Stored preloaded drawings.
let width = 3,
  height = 3; // Starting size.
let scale = 1; //5;
const abc123Baseline = 8;
const typography = false; // Enabled or disables the baseline.
let typeface;

// 🥾 Boot (Runs once before first paint and sim)
function boot({
  cursor,
  geo: { Grid },
  ui: { Button },
  net: { preload },
  screen,
  hud: { label },
  params,
  needsPaint,
}) {
  typeface = new Typeface(font1);
  typeface.load(preload);
  cursor("precise");

  if (params.length > 0) {
    // Get the resolution from the params or use a default.
    const split = params[0].split("x");
    width = parseInt(split[0]) || width;
    height = parseInt(split[1]) || height;
  } else label("plot 3x3");

  // Grid

  // TODO: Determine scale based on grid size and screen width / height.

  const gridAspect = width / height;
  const screenAspect = screen.width / screen.height;
  console.log("gridAspect", gridAspect, "screenAspect", screenAspect);

  let longSide;

  // Screen Landscape
  if (screenAspect > 1) {
    if (gridAspect > 1) {
      longSide = [screen.width, width];
    } else {
      longSide = [screen.height, height];
    }
  } else {
    // Portrait
    if (gridAspect > 1) {
      console.log("Portrait, landscape");
      longSide = [screen.height, height];
    } else {
      longSide = [screen.width, width];
    }
  }
  const margin = longSide[0] / 6;
  scale = floor((longSide[0] - margin) / longSide[1]);

  const gridWidth = width * scale;
  const gridHeight = height * scale;
  const gridX = screen.width / 2 - gridWidth / 2;
  const gridY = screen.height / 2 - gridHeight / 2;

  g = new Grid(gridX, gridY, width, height, scale);

  // Buttons
  const btnW = 15;
  const gap = 8;
  open = new Button(gap, screen.height - gap - 2, btnW, 6);
  save = new Button(
    screen.width - btnW - gap,
    screen.height - gap - 2,
    btnW,
    6
  );

  // preload("drawings/default.json").then(decode); // Preloada default drawing.

  // Preload save button icon.
  preload("aesthetic.computer/disks/drawings/save_open_icon.json").then((r) => {
    plots.icon = r;
    needsPaint();
  });
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
}) {
  // A. 🌟 Grid
  // Clear the background and draw a grid with an outline.
  wipe(colors.background)
    .ink(colors.grid)
    .grid(g)
    .ink(colors.gridOutline)
    .box(g.scaled, "outline");

  // Draw a box on the "center" of the grid.
  ink(255, 0, 0, 64).box(...g.middle(), g.scale);

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
  const sq = g.under(pen || { x: 0, y: 0 }, (sq) => {
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
function act({ event: e, download, sideload, num: { timestamp }, needsPaint }) {
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
      sideload(".json")
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

// Drawing Format

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

  return [filename, data];
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
