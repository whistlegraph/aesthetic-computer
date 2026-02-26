// 📚 Docs, 24.01.29.21.54
// Return up to date data for the aesthetic.computer pieces api.

/* #region 🏁 TODO 
  - [-] Check ida's descriptions.
  - [] Handle flashing screen / subtle cosmetics.
  - [] Wire it up to the VS code extension so it fills in the prompt / opens it.
  - [] Wire this endpoint up to a prompt auto-complete as well. 
  - [] Pals should bump upwards on hover and animate scroll to top on tap.
  - [] And be faded / only fade up when scrolled past 0.
  + Done
  - [x] Fill out an initial range of identifiers.
    - [x] Can always hide unnecessary / incomplete ones later.
  - [x] Build a nice scrollable index page.
#endregion */

import { respond } from "../../backend/http.mjs";
import { defaultTemplateStringProcessor as html } from "../../public/aesthetic.computer/lib/helpers.mjs";
const dev = process.env.CONTEXT === "dev";
const { keys } = Object;

// GET A user's `sub` id from either their handle or email address.
export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  const AC_ORIGIN = "https://aesthetic.computer";
  const LEARN_KIDLISP_ORIGIN = "https://learn.kidlisp.com";

  // TODO: Factor this out.
  const boxBody = `
    <code>box()</code> <em>A random box</em></li><br>
    <code>box(x, y, size)</code></code> <em>Square from top left corner</em><br>
    <mark><code>box(x, y, w, h)</code> <em>Rectangle from top left corner</em></mark><br>
    <code>box(x, y, size, mode)</code> <em>Square with <code>mode</code></em><br>
    <code>box(x, y, w, h, mode)</code> <em>Rectangle with <code>mode</code></em><br>
    <br>
    <hr>
    <code>mode</code>
    <br>
    <code>center</code> &nbsp;- paints a box from the center<br>
    <code>outline</code> - paints the outline of a box<br>
    <code>inline</code> &nbsp;- the opposite of outline<br>
    <em>(thicken with <code>:</code> like <code>outline:4</code>)</em>
    <br>
    combine modes with <code>*</code> like <code>outline*center</code> or <code>inline:3*center</code> 
  `.trim();

  const l5StatusBadge = (status) => {
    if (status === "done") return `<span class="status-badge status-done">done</span>`;
    if (status === "in-progress") return `<span class="status-badge status-in-progress">in progress</span>`;
    return `<span class="status-badge status-planned">planned</span>`;
  };

  const l5ChecklistItems = [
    {
      area: "Docs checklist + /l5 try page",
      status: "done",
      notes: "This docs section and /l5 landing page exist.",
    },
    {
      area: "Lua source detection (.lua) in loader",
      status: "done",
      notes: "disk + parse support .lua, including .mjs -> .lua -> .lisp fallback.",
    },
    {
      area: "Lua runtime adapter (Wasmoon)",
      status: "done",
      notes: "Wasmoon runtime is vendored and wired through lib/l5.mjs.",
    },
    {
      area: "L5 lifecycle bridge (setup/draw/events)",
      status: "in-progress",
      notes: "setup/draw + key/mouse callbacks are mapped; advanced callbacks remain.",
    },
    {
      area: "Core graphics API parity",
      status: "in-progress",
      notes: "background/fill/stroke/line/rect/circle/ellipse/text/triangle/quad mapped.",
    },
    {
      area: "Input globals (mouse/key/frame)",
      status: "in-progress",
      notes: "mouse/key/frame globals are injected each frame; parity is still incomplete.",
    },
    {
      area: "Publish .lua pieces",
      status: "done",
      notes: "Upload + media tracking now accept lua extension.",
    },
    {
      area: "Trust/restricted API posture for Lua",
      status: "done",
      notes: "Lua pieces use trustLevel=l5 and run through restricted API policy.",
    },
  ];

  const l5ChecklistBody = `
    <p>
      This board tracks what is actually implemented, not intended parity.
      Update statuses as work lands.
    </p>
    <table>
      <thead>
        <tr>
          <th>Area</th>
          <th>Status</th>
          <th>Notes</th>
        </tr>
      </thead>
      <tbody>
        ${l5ChecklistItems
          .map(
            (item) => `
              <tr>
                <td>${item.area}</td>
                <td>${l5StatusBadge(item.status)}</td>
                <td>${item.notes}</td>
              </tr>
            `,
          )
          .join("")}
      </tbody>
    </table>
    <p><strong>Status date:</strong> 2026-02-26</p>
  `.trim();

  const l5LifecycleBody = `
    <table>
      <thead>
        <tr>
          <th>L5 callback</th>
          <th>AC bridge</th>
          <th>Status</th>
        </tr>
      </thead>
      <tbody>
        <tr><td><code>setup()</code></td><td><code>boot($)</code></td><td>${l5StatusBadge("done")}</td></tr>
        <tr><td><code>draw()</code></td><td><code>paint($)</code></td><td>${l5StatusBadge("done")}</td></tr>
        <tr><td><code>keyPressed()</code></td><td><code>act($)</code> keyboard events</td><td>${l5StatusBadge("done")}</td></tr>
        <tr><td><code>mousePressed()</code></td><td><code>act($)</code> pen/touch events</td><td>${l5StatusBadge("done")}</td></tr>
      </tbody>
    </table>
  `.trim();

  const l5GraphicsBody = `
    <table>
      <thead>
        <tr>
          <th>L5 API</th>
          <th>AC target</th>
          <th>Status</th>
        </tr>
      </thead>
      <tbody>
        <tr><td><code>background()</code></td><td><code>$.wipe()</code></td><td>${l5StatusBadge("done")}</td></tr>
        <tr><td><code>fill()</code>/<code>stroke()</code></td><td>state + <code>$.ink()</code></td><td>${l5StatusBadge("done")}</td></tr>
        <tr><td><code>line()</code>/<code>point()</code></td><td><code>$.line()</code>/<code>$.plot()</code></td><td>${l5StatusBadge("done")}</td></tr>
        <tr><td><code>rect()</code>/<code>circle()</code>/<code>ellipse()</code></td><td><code>$.box()</code>/<code>$.circle()</code>/<code>$.oval()</code></td><td>${l5StatusBadge("done")}</td></tr>
        <tr><td><code>beginShape()</code>…</td><td><code>$.shape()</code>/<code>$.poly()</code></td><td>${l5StatusBadge("planned")}</td></tr>
      </tbody>
    </table>
  `.trim();

  const l5InputBody = `
    <table>
      <thead>
        <tr>
          <th>L5 global</th>
          <th>Source in AC</th>
          <th>Status</th>
        </tr>
      </thead>
      <tbody>
        <tr><td><code>mouseX</code>/<code>mouseY</code></td><td><code>$.pen.x</code>/<code>$.pen.y</code></td><td>${l5StatusBadge("done")}</td></tr>
        <tr><td><code>mouseIsPressed</code></td><td><code>$.pen.drawing</code></td><td>${l5StatusBadge("done")}</td></tr>
        <tr><td><code>width</code>/<code>height</code></td><td><code>$.screen.width</code>/<code>$.screen.height</code></td><td>${l5StatusBadge("done")}</td></tr>
        <tr><td><code>frameCount</code></td><td>runtime counter</td><td>${l5StatusBadge("done")}</td></tr>
      </tbody>
    </table>
  `.trim();

  const l5UnsupportedBody = `
    <ul>
      <li><code>rotate()</code> and <code>scale()</code> require matrix transform support.</li>
      <li><code>bezier()</code> and curve families are not mapped in v1 scope.</li>
      <li>File/video APIs and full Processing IO are out of scope for initial launch.</li>
      <li>Do not claim full L5 parity until checklist items move to <em>done</em>.</li>
    </ul>
  `.trim();

  const l5ExamplesBody = `
    <p>Use these as starter snippets. Runtime wiring exists; API coverage is still partial.</p>
    <div class="doc-examples">
      <article class="doc-example">
        <h3>Pulse Circle</h3>
        <pre><code class="language-lua">function setup()
  size(256, 256)
end

function draw()
  background(12, 12, 18)
  local r = 40 + math.sin(frameCount * 0.05) * 20
  fill(255, 120, 80)
  circle(width / 2, height / 2, r * 2)
end</code></pre>
      </article>
      <article class="doc-example">
        <h3>Mouse Dots</h3>
        <pre><code class="language-lua">function setup()
  background(255)
end

function draw()
  if mouseIsPressed then
    fill(30, 30, 30)
    circle(mouseX, mouseY, 10)
  end
end</code></pre>
      </article>
    </div>
    <p>
      <a href="${AC_ORIGIN}/l5">Open the L5 try page</a> ·
      <a href="${AC_ORIGIN}/prompt">Open prompt</a>
    </p>
  `.trim();

  const mjsOverviewBody = `
    <p>
      This lane documents the JavaScript piece API for <code>.mjs</code> pieces on AC.
      It is the runtime-facing reference for functions used in <code>boot/paint/act/sim/beat</code>.
    </p>
    <p>
      Start with <a href="${AC_ORIGIN}/docs/structure:paint">paint()</a>,
      then browse <a href="${AC_ORIGIN}/docs/graphics:line">graphics</a>,
      <a href="${AC_ORIGIN}/docs/interaction:pen">interaction</a>,
      and <a href="${AC_ORIGIN}/docs/system:reload">system</a>.
    </p>
  `.trim();

  const kidlispOverviewBody = `
    <p>
      KidLisp docs are part of the unified platform docs program and currently use
      <a href="${LEARN_KIDLISP_ORIGIN}" target="_blank" rel="noopener">learn.kidlisp.com</a>
      as the canonical public reference.
    </p>
    <p>
      Use this section for cross-links between AC platform APIs and KidLisp language APIs.
      Long-term source convergence is tracked in <code>/plans/docs-js-lua-overhaul-hitlist.md</code>.
    </p>
    <p>
      <a href="${LEARN_KIDLISP_ORIGIN}/?tab=reference" target="_blank" rel="noopener">Open full KidLisp reference</a> ·
      <a href="${LEARN_KIDLISP_ORIGIN}/?tab=functions" target="_blank" rel="noopener">Open popularity/function view</a>
    </p>
  `.trim();

  const kidlispCoreBody = `
    <table>
      <thead>
        <tr>
          <th>Family</th>
          <th>Examples</th>
          <th>Canonical source</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>Drawing</td>
          <td><code>wipe</code>, <code>ink</code>, <code>line</code>, <code>box</code>, <code>circle</code></td>
          <td><a href="${LEARN_KIDLISP_ORIGIN}/?tab=reference" target="_blank" rel="noopener">learn.kidlisp.com reference tab</a></td>
        </tr>
        <tr>
          <td>Transform</td>
          <td><code>scroll</code>, <code>zoom</code>, <code>spin</code>, <code>blur</code>, <code>bake</code></td>
          <td><a href="${LEARN_KIDLISP_ORIGIN}/?id=scroll" target="_blank" rel="noopener">Identifier detail pages</a></td>
        </tr>
        <tr>
          <td>Control + Math</td>
          <td><code>def</code>, <code>later</code>, <code>once</code>, <code>repeat</code>, <code>random</code></td>
          <td><a href="${LEARN_KIDLISP_ORIGIN}/?id=def" target="_blank" rel="noopener">Learn identifiers</a></td>
        </tr>
      </tbody>
    </table>
  `.trim();

  const docs = {
    // Commands for programming inside of pieces.
    api: {
      // 🏛️ Generic
      structure: {
        // 🧩 Top Level Piece Functions / meta-level imports.
        boot: {
          label: "🥾 Boot",
          sig: "boot($)",
          desc: "Runs once when a piece starts. Use it to initialize state and configure systems.",
          params: [{ name: "$", type: "api", required: true, desc: "Full AC runtime API object for the piece." }],
          returns: "void",
          done: true,
        },
        paint: {
          label: "🎨 Paint",
          sig: "paint($)",
          desc: "Runs on each render frame. Draw graphics and update visual state here.",
          params: [{ name: "$", type: "api", required: true, desc: "Frame API with graphics/input/system helpers." }],
          returns: "void | boolean",
          done: true,
        },
        act: {
          label: "🎪 Act",
          sig: "act($)",
          desc: "Runs for input and system events (keyboard, pointer, signals, lifecycle notifications).",
          params: [{ name: "$", type: "api", required: true, desc: "Event API. Current event is available at $.event." }],
          returns: "void",
          done: true,
        },
        sim: {
          label: "🧮 Sim",
          sig: "sim($)",
          desc: "Fixed-step simulation hook for logic updates independent from render timing.",
          params: [{ name: "$", type: "api", required: true, desc: "Simulation API for deterministic state updates." }],
          returns: "void",
          done: true,
        },
        beat: {
          label: "🥁 Beat",
          sig: "beat($)",
          desc: "Runs on system metronome ticks for rhythm-synced behavior.",
          params: [{ name: "$", type: "api", required: true, desc: "Beat API including sound timing helpers." }],
          returns: "void",
          done: true,
        },
        leave: {
          label: "👋 Leave",
          sig: "leave($)",
          desc: "Execute code right before the piece is unloaded.",
          params: [{ name: "$", type: "api", required: true, desc: "API snapshot available during teardown." }],
          returns: "void",
          done: true,
        },
        meta: {
          label: " 📰 Meta",
          sig: "meta()",
          desc: "Optional static metadata declaration for the piece (name, author, capabilities).",
          returns: "object",
          done: true,
        },
        preview: {
          label: "🖼️ Preview",
          sig: "preview($)",
          desc: "Render a custom preview thumbnail for galleries and listings.",
          params: [{ name: "$", type: "api", required: true, desc: "Graphics API for drawing thumbnail output." }],
          returns: "void",
          done: true,
        },
        icon: {
          label: "🪷 Icon",
          sig: "icon($)",
          desc: "Render a small icon/fav icon representation for the piece.",
          params: [{ name: "$", type: "api", required: true, desc: "Graphics API for icon rendering." }],
          returns: "void",
          done: true,
        },
        brush: {
          label: "🖌️ Brush",
          sig: "brush($)",
          desc: "For implementing brushes in the `nopaint` system.",
          params: [{ name: "$", type: "api", required: true, desc: "Nopaint brush API and event context." }],
          returns: "void",
          done: true,
        },
        filter: {
          label: "🥤 Filter",
          sig: "filter($)",
          desc: "For implementing filters in the `nopaint` system.",
          params: [{ name: "$", type: "api", required: true, desc: "Nopaint filter API and source image data." }],
          returns: "void",
          done: true,
        },
        curtain: {
          label: "curtain",
          sig: "curtain($)",
          desc: "Top-layer render hook for world-mode pieces.",
          params: [{ name: "$", type: "api", required: true, desc: "World render API for overlay/cutaway effects." }],
          returns: "void",
          done: true,
        },
        background: {
          label: "🏔️ background",
          sig: "background($)",
          desc: "World-system backdrop hook rendered behind foreground actors.",
          params: [{ name: "$", type: "api", required: true, desc: "World render API for backdrop painting." }],
          returns: "void",
          done: true,
        },
        api: {
          sig: "api",
          desc: "References all built-in functionality for a top-level function.",
          returns: "object",
          done: true,
        },
        DEBUG: {
          sig: "DEBUG",
          desc: "A global constant that determines if `AC` is in debug mode.",
          returns: "boolean",
          done: true,
        },
      },
      // 🖱️ Interaction
      interaction: {
        pen: {
          sig: "pen: { x, y, ... }",
          desc: "Current primary pointer state (mouse or touch), including coordinates and press state.",
          returns: "object",
          examples: ["prompt~line", "prompt~plot"],
          done: true,
        },
        pens: {
          sig: "pens(n)",
          desc: "Read active pointer inputs. Without args returns all pens, with index returns one pointer.",
          params: [{ name: "n", type: "number", required: false, desc: "Pointer index for a single pen." }],
          returns: "array | object",
          done: true,
        },
        pen3d: {
          sig: "pen3d",
          desc: "Current XR/controller pointer state when running in immersive input contexts.",
          returns: "object | null",
          done: true,
        },
        event: {
          sig: "event",
          desc: "The current input/system event being processed in the active callback.",
          returns: "object",
          done: true,
        },
      },
      // 🖌️ Graphics
      graphics: {
        "abstract.bresenham": {
          sig: "bresenham(x0, y0, x1, y1)",
          desc: "Returns an array of integer points that make up an aliased line from (x0,y0) to (x1,y1). This function is abstract and does not render anything.",
          done: true,
        },
        line: {
          sig: "line(x0, y0, x1, y1) or line({x0, y0, x1, y1}) or line(p1, p2)",
          desc: "Draw a 1-pixel wide line. Can take 4 coordinates, an object with coordinates, or two points.",
          params: [
            { name: "x0, y0, x1, y1", type: "number", required: false, desc: "Line start and end coordinates." },
            { name: "p1, p2", type: "point", required: false, desc: "Alternative point-object form." },
          ],
          returns: "void",
          examples: ["line", "line:2", "line:5"],
          example: { type: "piece", entry: "line", height: 288 },
          done: true,
        },
        point: {
          sig: "point(...args) or point({x, y})",
          desc: "Plot a single pixel within the panned coordinate space. Takes x,y coordinates or a point object.",
          params: [
            { name: "x, y", type: "number", required: false, desc: "Pixel coordinates." },
            { name: "{x, y}", type: "object", required: false, desc: "Point-object form." },
          ],
          returns: "void",
          examples: ["plot", "plot 48 48", "plot 128 128"],
          example: { type: "piece", entry: "plot", height: 288 },
          done: true,
        },
        box: {
          sig: "box(x, y, w, h, mode)",
          desc: "Draw a box with optional modes: 'fill' (default), 'outline', 'inline'. Add '*center' to draw from center. Use ':N' for thickness.",
          body: boxBody,
          params: [
            { name: "x, y", type: "number", required: true, desc: "Top-left or center position (depending on mode)." },
            { name: "w, h", type: "number", required: true, desc: "Box width and height." },
            { name: "mode", type: "string", required: false, desc: "fill/outline/inline with optional center and thickness modifiers." },
          ],
          returns: "void",
          examples: ["box", "box:outline", "box:center"],
          example: { type: "piece", entry: "box", height: 288 },
          done: true,
        },
        wipe: {
          sig: "wipe(color)",
          desc: "Clear the screen with a solid color. Color can be a single number (0-255 for grayscale) or an array [r,g,b,a].",
          params: [
            { name: "color", type: "number | string | array", required: false, desc: "Fill color. Defaults to black when omitted." },
          ],
          returns: "void",
          examples: ["wipe", "wipe:red", "wipe:white"],
          example: { type: "piece", entry: "wipe", height: 288 },
          done: true,
        },
        ink: {
          sig: "ink(color)",
          desc: "Set the current drawing color. Color can be a single number (0-255 for grayscale) or an array [r,g,b,a].",
          params: [
            { name: "color", type: "number | string | array", required: true, desc: "Next draw color for primitives and text." },
          ],
          returns: "paintApi",
          examples: ["prompt~ink", "prompt~ink:red", "prompt~ink:black"],
          done: true,
        },

        circle: {
          sig: "circle(x, y, radius)",
          desc: "Draw a filled circle centered at (x,y) with the specified radius using the current ink color.",
          params: [
            { name: "x, y", type: "number", required: true, desc: "Circle center coordinates." },
            { name: "radius", type: "number", required: true, desc: "Circle radius in pixels." },
          ],
          returns: "void",
          examples: ["prompt~circle", "prompt~circle:16", "prompt~circle:outline"],
          done: true,
        },
        layer: {
          sig: "layer(index | options)",
          desc: "Work with layered painting buffers for compositing and advanced drawing workflows.",
          returns: "object",
          done: true,
        },
        painting: {
          sig: "painting(width, height, callback) or painting(width, height)",
          desc: "Create an offscreen painting buffer, optionally drawing into it via callback.",
          params: [
            { name: "width, height", type: "number", required: true, desc: "Buffer dimensions." },
            { name: "callback", type: "function", required: false, desc: "Draw callback receiving a paint API." },
          ],
          returns: "painting",
          done: true,
        },
        inkrn: {
          sig: "inkrn()",
          desc: "Read current ink color state as an RGBA array.",
          returns: "[r, g, b, a]",
          done: true,
        },
        pagern: {
          sig: "pagern()",
          desc: "Read the current active page/buffer reference.",
          returns: "painting",
          done: true,
        },
        notice: {
          sig: "notice(msg, color, opts)",
          desc: "Show a transient runtime notice/HUD message.",
          params: [
            { name: "msg", type: "string", required: true, desc: "Notice text." },
            { name: "color", type: "array", required: false, desc: "Foreground/background color pair." },
            { name: "opts", type: "object", required: false, desc: "Duration/behavior options." },
          ],
          returns: "void",
          done: true,
        },
        blend: {
          sig: "blend(mode)",
          desc: "Set blend mode behavior for subsequent draw operations.",
          returns: "void",
          done: true,
        },
        page: {
          sig: "page(buffer)",
          desc: "Switch the active drawing target buffer.",
          params: [{ name: "buffer", type: "painting", required: true, desc: "Target buffer/page." }],
          returns: "void",
          done: true,
        },
        edit: {
          sig: "edit(callback)",
          desc: "Mutate the current pixel buffer with a callback.",
          params: [{ name: "callback", type: "function", required: true, desc: "Pixel mutation callback." }],
          returns: "void",
          done: true,
        },
        copy: {
          sig: "copy(x, y, w, h)",
          desc: "Copy pixel data from the current buffer region.",
          returns: "painting",
          done: true,
        },
        paste: {
          sig: "paste(painting, x, y)",
          desc: "Paste a painting at the given position, anchored from the top left.",
          params: [
            { name: "painting", type: "painting | image", required: true, desc: "Source bitmap, painting id, or URL." },
            { name: "x, y", type: "number", required: false, desc: "Target top-left position. Defaults to 0,0." },
            { name: "scale", type: "number", required: false, desc: "Optional scale factor." },
          ],
          returns: "void",
          examples: ["paste", "paste:camera", "paste:under"],
          example: { type: "piece", entry: "paste", height: 288 },
          done: true,
        },
        stamp: {
          sig: "stamp(painting, x, y, scale)",
          desc: "Paste a painting centered at (x,y). Useful for sprites and markers.",
          params: [
            { name: "painting", type: "painting | image", required: true, desc: "Source bitmap, painting id, or URL." },
            { name: "x, y", type: "number", required: false, desc: "Center position." },
            { name: "scale", type: "number", required: false, desc: "Optional scale factor." },
          ],
          returns: "void",
          examples: ["stamp", "stamp:camera", "stamp:under"],
          example: { type: "piece", entry: "stamp", height: 288 },
          done: true,
        },
        pixel: {
          sig: "pixel(x, y) -> [r, g, b, a]",
          desc: "Read a single pixel color from the current active painting buffer.",
          params: [
            { name: "x, y", type: "number", required: true, desc: "Pixel coordinates." },
          ],
          returns: "[r, g, b, a]",
          examples: ["prompt~pixel"],
          done: true,
        },
        plot: {
          sig: "plot(x, y) or plot({x, y})",
          desc: "Draw one pixel at the given position using current ink color.",
          params: [
            { name: "x, y", type: "number", required: false, desc: "Pixel coordinates." },
            { name: "{x, y}", type: "object", required: false, desc: "Point-object form." },
          ],
          returns: "void",
          examples: ["plot", "plot 32 32", "plot 200 120"],
          example: { type: "piece", entry: "plot", height: 288 },
          done: true,
        },
        flood: {
          sig: "flood(x, y)",
          desc: "Flood-fill adjacent matching pixels at (x,y) using current ink color.",
          params: [
            { name: "x, y", type: "number", required: true, desc: "Seed position for the fill operation." },
          ],
          returns: "void",
          examples: ["prompt~flood", "prompt~flood:blue"],
          done: true,
        },
        lineAngle: {
          sig: "lineAngle(x, y, distance, angle)",
          desc: "Draw a line from origin using polar coordinates.",
          returns: "void",
          done: true,
        },
        pline: {
          sig: "pline(points)",
          desc: "Draw a connected polyline from a point list.",
          returns: "void",
          done: true,
        },
        pppline: {
          sig: "pppline(points)",
          desc: "Draw a pixel-perfect polyline with aliased-style stepping.",
          returns: "void",
          done: true,
        },
        oval: {
          sig: "oval(x, y, w, h, mode)",
          desc: "Draw an ellipse bounded by width and height dimensions.",
          params: [
            { name: "x, y", type: "number", required: true, desc: "Top-left or center position depending on mode." },
            { name: "w, h", type: "number", required: true, desc: "Ellipse width and height." },
            { name: "mode", type: "string", required: false, desc: "fill/outline variants." },
          ],
          returns: "void",
          examples: ["oval", "oval:outline", "oval:center"],
          example: { type: "piece", entry: "oval", height: 288 },
          done: true,
        },
        poly: {
          sig: "poly(x0, y0, x1, y1, ...)",
          desc: "Draw a polygon from point pairs in sequence.",
          params: [
            { name: "points", type: "number[]", required: true, desc: "Alternating x/y coordinate list." },
          ],
          returns: "void",
          examples: ["prompt~poly", "prompt~poly:outline"],
          done: true,
        },
        shape: {
          sig: "shape(points, mode)",
          desc: "Draw a higher-level shape from point arrays/objects with optional mode controls.",
          params: [
            { name: "points", type: "array", required: true, desc: "Point list or packed coordinate data." },
            { name: "mode", type: "string", required: false, desc: "fill/outline behavior." },
          ],
          returns: "void",
          examples: ["shape", "shape:outline"],
          example: { type: "piece", entry: "shape", height: 288 },
          done: true,
        },
        grid: {
          sig: "grid(x, y, w, h, scale)",
          desc: "Create a uniform grid helper for layout and sampling.",
          returns: "Grid",
          done: true,
        },
        draw: {
          sig: "draw(shapeOrCommand, ...args)",
          desc: "Execute a generic draw command helper.",
          returns: "void",
          done: true,
        },
        printLine: {
          sig: "printLine(text, x, y, opts)",
          desc: "Render a single line of text using low-level type metrics.",
          returns: "void",
          done: true,
        },
        form: {
          sig: "form(options)",
          desc: "Create/manage a higher-level geometric form object for 3D or batched drawing.",
          returns: "Form",
          done: true,
        },
        pan: {
          sig: "pan(x, y)",
          desc: "Set or offset the current 2D camera pan.",
          returns: "void",
          done: true,
        },
        unpan: {
          sig: "unpan()",
          desc: "Reset active pan transform.",
          returns: "void",
          done: true,
        },
        savepan: {
          sig: "savepan()",
          desc: "Store current pan transform state.",
          returns: "void",
          done: true,
        },
        loadpan: {
          sig: "loadpan()",
          desc: "Restore previously stored pan transform state.",
          returns: "void",
          done: true,
        },
        skip: {
          sig: "skip(n)",
          desc: "Skip/pad drawing steps in helper-driven sequences.",
          returns: "void",
          done: true,
        },
        glaze: {
          sig: "glaze({ on: bool })",
          desc: "Enable a fullscreen shader `glaze` effect.",
          returns: "void",
          done: true,
        },
        paintCount: {
          sig: "paintCount",
          desc: "The number of `paint` frames that have passed.",
          returns: "bigint",
          done: true,
        },
        screen: {
          sig: "screen",
          desc: "Current screen buffer object with width/height/pixels.",
          returns: "painting",
          done: true,
        },
        display: {
          sig: "display",
          desc: "A reference to the current display information.",
          returns: "object",
          done: true,
        },
        fps: {
          sig: "fps(value)",
          desc: "Set target frame rate for draw loops.",
          returns: "void",
          done: true,
        },
        resolution: {
          sig: "resolution(width, height = width, gap = 8)",
          desc: "Adjust display resolution and optional gap/pixel spacing.",
          params: [
            { name: "width", type: "number", required: true, desc: "Target render width." },
            { name: "height", type: "number", required: false, desc: "Target render height (defaults to width)." },
            { name: "gap", type: "number", required: false, desc: "Display gap spacing between pixels." },
          ],
          returns: "void",
          examples: ["prompt~resolution:128", "prompt~resolution:64"],
          done: true,
        },
        video: {
          sig: "video(mode, options)",
          desc: "Access camera/video capture and tracking modes.",
          returns: "object | void",
          done: true,
        },
        rec: {
          sig: "rec",
          desc: "Recorder subsystem for capturing frames/media output.",
          returns: "Recorder",
          done: true,
        },
        needsPaint: {
          sig: "needsPaint()",
          desc: "Mark the renderer as dirty so a frame will be painted.",
          returns: "void",
          done: true,
        },
        noise16: {
          sig: "noise16(opts)",
          desc: "Apply 16-color noise texture overlay.",
          returns: "void",
          done: true,
        },
        noise16DIGITPAIN: {
          sig: "noise16DIGITPAIN(opts)",
          desc: "Apply DIGITPAIN-flavored 16-color noise texture.",
          returns: "void",
          done: true,
        },
        noise16Aesthetic: {
          sig: "noise16Aesthetic(opts)",
          desc: "Apply Aesthetic-themed 16-color noise texture.",
          returns: "void",
          done: true,
        },
        noise16Sotce: {
          sig: "noise16Sotce(opts)",
          desc: "Apply SOTCE-themed 16-color noise texture.",
          returns: "void",
          done: true,
        },
        noiseTinted: {
          sig: "noiseTinted(opts)",
          desc: "Apply tinted procedural noise to the active buffer.",
          returns: "void",
          done: true,
        },
        write: {
          sig: "write(text, pos, b, bounds, wordWrap)",
          desc: "Render text into the current painting using current ink, font, and optional bounds.",
          params: [
            { name: "text", type: "string", required: true, desc: "Text content to draw." },
            { name: "pos", type: "object | number", required: false, desc: "Position or anchor object." },
            { name: "bounds", type: "object", required: false, desc: "Optional clipping/wrapping bounds." },
          ],
          returns: "painting | metrics",
          examples: ["prompt~write", "prompt~word"],
          done: true,
        },
        "text.capitalize": {
          sig: "capitalize(text)",
          desc: "Capitalize words in a string for display labels/headings.",
          returns: "string",
          done: true,
        },
        "text.box": {
          sig: "box(text, pos, bounds, scale, wordWrap, fontName)",
          desc: "Measure and layout text block metrics without drawing.",
          returns: "object",
          done: true,
        },
        clonePixels: {
          sig: "clonePixels(buffer)",
          desc: "Return a cloned pixel buffer for safe mutation.",
          returns: "Uint8ClampedArray",
          done: true,
        },
        colorsMatch: {
          sig: "colorsMatch(color1, color2)",
          desc: "Checks if two colors `[r, g, b, a]` are the same.",
          returns: "boolean",
          done: true,
        },
        color: {
          sig: "color(?)",
          desc: "Return a color `[r, g, b, a]` from a variety of inputs.",
          returns: "[r, g, b, a]",
          done: true,
        },
        resize: {
          sig: "resize(bitmap, width, height)",
          desc: "Get a fresh resized bitmap with nearest neighbor scaling.",
          returns: "painting",
          done: true,
        },
        Camera: {
          sig: "Camera",
          desc: "3D camera model/type used by form and world rendering helpers.",
          done: true,
        },
        Form: {
          sig: "Form",
          desc: "3D/mesh form primitive type used in advanced rendering.",
          done: true,
        },
        Dolly: {
          sig: "Dolly",
          desc: "Camera dolly helper type for 3D transforms and motion.",
          done: true,
        },
        TRI: {
          sig: "TRI",
          desc: "Triangle primitive constant for form pipelines.",
          done: true,
        },
        QUAD: {
          sig: "QUAD",
          desc: "Quad primitive constant for form pipelines.",
          done: true,
        },
        LINE: {
          sig: "LINE",
          desc: "Line primitive constant for form pipelines.",
          done: true,
        },
        CUBEL: {
          sig: "CUBEL",
          desc: "Cuboid/cube primitive constant for form pipelines.",
          done: true,
        },
        ORIGIN: {
          sig: "ORIGIN",
          desc: "Origin reference constant for transform helpers.",
          done: true,
        },
        "ui.Button": {
          sig: "new Button(box)",
          desc: "An interactive button model with a text label.",
          returns: "Button",
          done: true,
        },
        "ui.TextButton": {
          sig: "new TextButton(text, pos)",
          desc: "An interactive button model with a text label.",
          returns: "TextButton",
          done: true,
        },
        "ui.TextInput": {
          sig: "new TextInput($, text, processCommand, options = { palette, font, wrap })",
          desc: "An interactive text prompt object.",
          returns: "TextInput",
          done: true,
        },
        "content.add": {
          sig: "add(content)",
          desc: "Make a request to add content to the DOM.",
          returns: "string",
          done: true,
        },
        "dom.html": {
          sig: "html(src)",
          desc: "Add `html` content to the DOM.",
          returns: "void",
          done: true,
        },
        "dom.css": {
          sig: "css(src)",
          desc: "Add `css` content to the DOM.",
          returns: "void",
          done: true,
        },
        "dom.javascript": {
          sig: "javascript(src)",
          desc: "Add `javascript` content to the DOM.",
          returns: "void",
          done: true,
        },
        "dom.clear": {
          sig: "clear()",
          desc: "Clear (remove) all DOM content.",
          returns: "void",
          done: true,
        },
        typeface: {
          sig: "typeface",
          desc: "A reference to the default system typeface.",
          returns: "object",
          done: true,
        },
        cursor: {
          sig: "cursor(code)",
          desc: "Set the system mouse cursor to a different graphic.",
          returns: "void",
          done: true,
        },
      },
      sound: {
        "sound.time": {
          sig: "sound.time",
          desc: "Current audio engine time in seconds.",
          returns: "number",
          done: true,
        },
        "sound.bpm": {
          sig: "sound.bpm(newBPM?)",
          desc: "Get or set the current BPM used for beat-based durations.",
          params: [{ name: "newBPM", type: "number", required: false, desc: "Optional BPM override." }],
          returns: "number",
          done: true,
        },
        "sound.freq": {
          sig: "sound.freq(input)",
          desc: "Resolve note names or numeric input to frequency in Hz.",
          params: [{ name: "input", type: "string | number", required: true, desc: "Examples: 440, C4, 4C#, A3." }],
          returns: "number | null",
          done: true,
        },
        "sound.microphone": {
          sig: "sound.microphone",
          desc: "Live microphone object (connect/poll/record + analysis fields).",
          returns: "object",
          done: true,
        },
        "sound.speaker": {
          sig: "sound.speaker",
          desc: "Live speaker output analysis object (amplitude/waveform/frequency data).",
          returns: "object",
          done: true,
        },
        "sound.play": {
          sig: "sound.play(sfx, options, callbacks)",
          desc: "Play a registered sample/sfx by id and return a live handle.",
          params: [
            { name: "sfx", type: "string", required: true, desc: "Sample id/path to play." },
            { name: "options", type: "object", required: false, desc: "Playback options (volume, pan, loop, speed, etc)." },
            { name: "callbacks", type: "object", required: false, desc: "Lifecycle callbacks (for example kill handlers)." },
          ],
          returns: "object",
          done: true,
        },
        "sound.synth": {
          sig: "sound.synth({ tone, type, duration, beats, attack, decay, volume, pan, generator })",
          desc: "Play a synthesized voice and return a handle for kill/progress/update.",
          params: [{ name: "options", type: "object", required: false, desc: "Synth options object with oscillator and envelope fields." }],
          returns: "object",
          done: true,
        },
        "sound.bubble": {
          sig: "sound.bubble({ radius, rise, volume, pan })",
          desc: "Spawn a bubble-style synthesized sound voice.",
          params: [{ name: "options", type: "object", required: false, desc: "Bubble synth options." }],
          returns: "object",
          done: true,
        },
        "sound.kill": {
          sig: "sound.kill(id, fade?)",
          desc: "Stop an active synth/sample by id, with optional fade time.",
          params: [
            { name: "id", type: "number | bigint | string", required: true, desc: "Active sound identifier." },
            { name: "fade", type: "number", required: false, desc: "Optional fade-out duration." },
          ],
          returns: "void",
          done: true,
        },
      },
      network: {
        "net.signup": {
          sig: "signup()",
          desc: "Redirect a user to the signup screen.",
          returns: "void",
          done: true,
        },
        "net.login": {
          sig: "login()",
          desc: "Redirect a user to the login screen.",
          returns: "void",
          done: true,
        },
        "net.logout": {
          sig: "logout()",
          desc: "Log a user out and redirect them to the `prompt`.",
          returns: "void",
          done: true,
        },
        "net.pieces": {
          sig: "pieces",
          desc: "The system path to all built-in piece code.",
          returns: "string",
          done: true,
        },
        "net.parse": {
          sig: "parse(slug)",
          desc: "Parse a textual piece slug.",
          params: [{ name: "slug", type: "string", required: true, desc: "Piece slug or prompt-style path." }],
          returns: "object",
          done: true,
        },
        "net.userRequest": {
          sig: "userRequest(method, endpoint, body)",
          desc: "Make an authorized request for a logged in user.",
          params: [
            { name: "method", type: "string", required: true, desc: "HTTP method (GET, POST, etc)." },
            { name: "endpoint", type: "string", required: true, desc: "Relative API endpoint." },
            { name: "body", type: "object", required: false, desc: "JSON payload for write requests." },
          ],
          returns: "Promise<object>",
          done: true,
        },
        "net.udp": {
          sig: "udp(receive)",
          desc: "Loosely connect the UDP receiver.",
          params: [{ name: "receive", type: "function", required: true, desc: "Callback for incoming UDP-style messages." }],
          returns: "void",
          done: true,
        },
        "net.lan": {
          sig: "lan",
          desc: "A reference to the local area network IP if it is available.",
          returns: "string | null",
          done: true,
        },
        "net.iframe": {
          sig: "iframe",
          desc: "Whether or not the system is running hosted within an `iframe`.",
          returns: "boolean",
          done: true,
        },
        back: {
          sig: "back()",
          desc: "Go back to the previous piece or prompt if there is no history.",
          returns: "void",
          done: true,
        },
        alias: {
          sig: "alias(name, colon, params)",
          desc: "Jump to a piece without changing the corner label or url, and ignoring the history stack.",
          params: [
            { name: "name", type: "string", required: true, desc: "Piece slug to load." },
            { name: "colon", type: "array", required: false, desc: "Colon params to pass through." },
            { name: "params", type: "array", required: false, desc: "Space params to pass through." },
          ],
          returns: "void",
          done: true,
        },
        load: {
          sig: "async load(parsed, fromHistory, alias, devReload, loadedCallback)",
          desc: "Load a piece after parsing a slug, with various options.",
          params: [
            { name: "parsed", type: "object", required: true, desc: "Parsed slug payload." },
            { name: "fromHistory", type: "boolean", required: false, desc: "Treat this load as history navigation." },
            { name: "alias", type: "boolean", required: false, desc: "Skip URL/label rewrite when true." },
            { name: "devReload", type: "boolean", required: false, desc: "Set dev-reload mode." },
            { name: "loadedCallback", type: "function", required: false, desc: "Callback after load success." },
          ],
          returns: "Promise<void>",
          done: true,
        },
        slug: {
          sig: "slug",
          desc: "The full piece address containing its name, colon parameters, and space separated parameters.",
          returns: "object",
          done: true,
        },
        piece: {
          sig: "piece",
          desc: "The name of the running piece.",
          returns: "string",
          done: true,
        },
        query: {
          sig: "query",
          desc: "An object containing the system's URL query parameters.",
          returns: "object",
          done: true,
        },
        params: {
          sig: "params",
          desc: "Array of space-delimited piece parameters from the current slug.",
          returns: "array",
          done: true,
        },
        colon: {
          sig: "colon",
          desc: "Array of colon parameters for the active piece slug.",
          returns: "array",
          done: true,
        },
        preload: {
          sig: "async preload(path, parseJSON = true, progressReport, options)",
          desc: "Preload a media asset from the network.",
          params: [
            { name: "path", type: "string", required: true, desc: "Asset URL or path." },
            { name: "parseJSON", type: "boolean", required: false, desc: "Auto-parse JSON responses." },
            { name: "progressReport", type: "function", required: false, desc: "Progress callback." },
            { name: "options", type: "object", required: false, desc: "Fetch options overrides." },
          ],
          returns: "Promise<any>",
          done: true,
        },
        download: {
          sig: "download(filename, data, modifiers)",
          desc: "Download a file.",
          params: [
            { name: "filename", type: "string", required: true, desc: "Output filename." },
            { name: "data", type: "Blob | string | object", required: true, desc: "Download payload." },
            { name: "modifiers", type: "object", required: false, desc: "Optional mime/options." },
          ],
          returns: "void",
          done: true,
        },
        dark: {
          sig: "dark",
          desc: "If the system is in dark mode.",
          returns: "boolean",
          done: true,
        },
        jump: {
          sig: "jump(to)",
          desc: "Navigate to a piece/url or cached code id.",
          params: [{ name: "to", type: "string", required: true, desc: "Target piece slug, URL, or code id." }],
          returns: "void",
          done: true,
        },
        leaving: {
          sig: "leaving()",
          desc: "Returns true if a piece is leaving / a `jump` is in process.",
          returns: "boolean",
          done: true,
        },
        broadcast: {
          sig: "broadcast(msg)",
          desc: "Send a message to other open `aesthetic.computer` tabs.",
          params: [{ name: "msg", type: "any", required: true, desc: "Broadcast payload." }],
          returns: "void",
          done: true,
        },
        "net.socket": {
          sig: "socket(receive)",
          desc: "Hook into the piece's socket server with a receive callback.",
          params: [{ name: "receive", type: "function", required: true, desc: "Callback for socket events/messages." }],
          returns: "object | void",
          done: true,
        },
        "net.devReload": {
          sig: "devReload",
          desc: "A flag that determines if the piece code was just reloaded in development.",
          returns: "boolean",
          done: true,
        },
        "net.web": {
          sig: "web(url, jumpOut)",
          desc: "Jump the browser to a new url.",
          params: [
            { name: "url", type: "string", required: true, desc: "Destination URL." },
            { name: "jumpOut", type: "boolean", required: false, desc: "Open externally when true." },
          ],
          returns: "void",
          done: true,
        },
        "net.host": {
          sig: "host",
          desc: "The current network host.",
          returns: "string",
          done: true,
        },
        "net.rewrite": {
          sig: "rewrite(path, historical = false)",
          desc: "Rewrite a new URL / parameter path without affecting the history.",
          params: [
            { name: "path", type: "string", required: true, desc: "New path/query to write." },
            { name: "historical", type: "boolean", required: false, desc: "Whether to push history." },
          ],
          returns: "void",
          done: true,
        },
        "net.refresh": {
          sig: "refresh()",
          desc: "Refresh the page / restart `aesthetic.computer`.",
          returns: "void",
          done: true,
        },
        "net.waitForPreload": {
          sig: "waitForPreload()",
          desc: "Tell the system to wait until preloading is finished before painting.",
          returns: "void",
          done: true,
        },
        "net.preloaded": {
          sig: "preloaded()",
          desc: "Tell the system that all preloading is done.",
          returns: "void",
          done: true,
        },
      },
      number: {
        simCount: {
          sig: "simCount",
          desc: "The number of simulation frames passed.",
          returns: "bigint",
          done: true,
        },
        seconds: {
          sig: "seconds(s)",
          desc: "Convert seconds to `sim` frames.",
          params: [{ name: "s", type: "number", required: true, desc: "Seconds value." }],
          returns: "number",
          done: true,
        },
        "num.add": {
          sig: "add(...numbers) | add(numbers[])",
          desc: "Add all numeric inputs and return the total.",
          returns: "number",
          done: true,
        },
        "num.wrap": {
          sig: "wrap(n, to)",
          desc: "Wrap a number into the range 0..to (exclusive upper bound).",
          returns: "number",
          done: true,
        },
        "num.even": {
          sig: "even(n)",
          desc: "Return true when n is evenly divisible by 2.",
          returns: "boolean",
          done: true,
        },
        "num.odd": {
          sig: "odd(n)",
          desc: "Return true when n is odd.",
          returns: "boolean",
          done: true,
        },
        "num.clamp": {
          sig: "clamp(value, low, high)",
          desc: "Clamp a value between low and high.",
          returns: "number",
          done: true,
        },
        "num.rand": {
          sig: "rand()",
          desc: "Return a random float in the range 0..1.",
          returns: "number",
          done: true,
        },
        "num.randInt": {
          sig: "randInt(n)",
          desc: "Gets a random integer.",
          done: true,
        },
        "num.randInd": {
          sig: "randInd(arr)",
          desc: "Generates a random index from an array.",
          done: true,
        },
        "num.randIntArr": {
          sig: "randIntArr(n, count)",
          desc: "Generates an array of random integers from 0-n (inclusive)",
          done: true,
        },
        "num.randIntRange": {
          sig: "randIntRange(low, high)",
          desc: "Generates an integer from low-high (inclusive)",
          done: true,
        },
        "num.rangedInts": {
          sig: "rangedInts(ints)",
          desc: "Converts an array of strings formatted like 1-100 into an array of random integer ranges. Useful for color ranges.",
          done: true,
        },
        "num.multiply": {
          sig: "multiply(operands, n)",
          desc: "Multiplies one or more [] operands by n and returns a Number or Array.",
          done: true,
        },
        "num.dist": {
          sig: "dist(x1, y1, x2, y2)",
          desc: "Compute the distance between two 2D points.",
          done: true,
        },
        "num.dist3d": {
          sig: "dist3d(p1, p2)",
          desc: "Compute the distance between two 3D points as [x, y, z].",
          done: true,
        },
        "num.perlin": {
          sig: "perlin(x, y)",
          desc: "Compute a 2D perlin noise value.",
          done: true,
        },
        "num.radians": {
          sig: "radians(deg)",
          desc: "Convert degrees to radians.",
          done: true,
        },
        "num.degrees": {
          sig: "degrees(rad)",
          desc: "Convert radians to degrees.",
          done: true,
        },
        "num.lerp": {
          sig: "lerp(a, b, amount)",
          desc: "Slides a number between a and b by a normalized amount.",
          done: true,
        },
        "num.map": {
          sig: "map(num, inMin, inMax, outMin, outMax)",
          desc: "Maps a number within a range to a new range.",
          done: true,
        },
        "num.arrMax": {
          sig: "arrMax(arr)",
          desc: "Return the maximum number in an array.",
          done: true,
        },
        "num.arrCompress": {
          sig: "arrCompress(arr, n)",
          desc: "Return a new array with every nth index missing.",
          done: true,
        },
        "num.Track": {
          sig: "new Track(values, result)",
          desc: "Lerp a value using a stepping function, with optional quantization.",
          done: true,
        },
        "num.p2.of": {
          sig: "of(x, y)",
          desc: "Turns two values into an {x, y} point.",
          done: true,
        },
        "num.p2.len": {
          sig: "len(pA)",
          desc: "Gets the length of the point as a vector.",
          done: true,
        },
        "num.p2.norm": {
          sig: "norm(p)",
          desc: "Normalizes a vector to have a length of 1.",
          done: true,
        },
        "num.p2.eq": {
          sig: "eq(p1, p2)",
          desc: "Checks for the equality of two points.",
          done: true,
        },
        "num.p2.inc": {
          sig: "inc(pout, pin)",
          desc: "Mutably adds P->in to P->out.",
          done: true,
        },
        "num.p2.scl": {
          sig: "scl(pout, pin)",
          desc: "Mutably scales P->out by P->in.",
          done: true,
        },
        "num.p2.add": {
          sig: "add(pA, pB)",
          desc: "Immutably adds pA + pB.",
          done: true,
        },
        "num.p2.sub": {
          sig: "sub(pA, pB)",
          desc: "Immutably subtracts pA - pB.",
          done: true,
        },
        "num.p2.rot": {
          sig: "rot(p, angle)",
          desc: "Immutably rotates p by angle in radians.",
          done: true,
        },
        "num.p2.mul": {
          sig: "mul(pA, pB)",
          desc: "Immutably multiplies pA * pB.",
          done: true,
        },
        "num.p2.div": {
          sig: "div(pA, pB)",
          desc: "Immutably divides pA / pB. Expands pA to an {x, y} if it is a single number.",
          done: true,
        },
        "num.p2.mid": {
          sig: "mid(pA, pB)",
          desc: "Calculates the midpoint between two points.",
          done: true,
        },
        "num.p2.dist": {
          sig: "dist(pA, pB)",
          desc: "Calculates the distance between two points.",
          done: true,
        },
        "num.p2.angle": {
          sig: "angle(pA, pB)",
          desc: "Calculates the angle between two points.",
          done: true,
        },
        "num.p2.dot": {
          sig: "dot(pA, pB)",
          desc: "Calculates the dot product of two points.",
          done: true,
        },
        "num.p2.floor": {
          sig: "floor(p)",
          desc: "Applies the floor function to both x and y coordinates of a point.",
          done: true,
        },
        "num.midp": {
          sig: "midp(a, b)",
          desc: "Find the midpoint between two [x, y] coordinates.",
          done: true,
        },
        "num.number": {
          sig: "number(maybeNumber)",
          desc: "Determine if the value is a number or not.",
          done: true,
        },
        "num.intersects": {
          sig: "intersects(line1, line2)",
          desc: "Compute whether two lines intersect. A line is: `{x0, y0, x1, y1}`",
          done: true,
        },
        "num.signedCeil": {
          sig: "signedCeil(n)",
          desc: "Ceil a number away from 0.",
          done: true,
        },
        "num.signedFloor": {
          sig: "signedFloor(val)",
          desc: "Floor a number towards 0.",
          done: true,
        },
        "num.vec2": {
          sig: "vec2.?",
          desc: "All the `vec2` functions from the `glMatrix` library.",
          done: true,
        },
        "num.vec3": {
          sig: "vec3.?",
          desc: "All the `vec3` functions from the `glMatrix` library.",
          done: true,
        },
        "num.vec4": {
          sig: "vec4.?",
          desc: "All the `vec4` functions from the `glMatrix` library.",
          done: true,
        },
        "num.mat3": {
          sig: "mat3.?",
          desc: "All the `mat3` functions from the `glMatrix` library.",
          done: true,
        },
        "num.mat4": {
          sig: "mat4.?",
          desc: "All the `mat4` functions from the `glMatrix` library.",
          done: true,
        },
        "num.quat": {
          sig: "quat.?",
          desc: "All the `quat` (quaternion) functions from the `glMatrix` library.",
          done: true,
        },
        "num.parseColor": {
          sig: "parseColor(params)",
          desc: "Parses a color from piece params.",
          done: true,
        },
        "num.findColor": {
          sig: "findColor(rgb)",
          desc: "Find a color inside of `cssColors` by value",
          done: true,
        },
        "num.saturate": {
          sig: "saturate(rgb, amount = 1)",
          desc: "Saturate a color by `amount`.",
          done: true,
        },
        "num.desaturate": {
          sig: "desaturate(rgb, amount = 1)",
          desc: "Desaturate a color by `amount`",
          done: true,
        },
        "num.shiftRGB": {
          sig: 'shiftRGB(a, b, step, mode = "lerp", range = 255)',
          desc: "Lerp two RGBA arrays, skipping alpha and rounding the output.",
          done: true,
        },
        "num.rgbToHexStr": {
          sig: "rgbToHexStr(r, g, b, prefix = \"\")",
          desc: "Convert separate RGB values to a hex string.",
          done: true,
        },
        "num.hexToRgb": {
          sig: "hexToRgb(h)",
          desc: "Takes a string/number hex value and outputs an [r, g, b] array.",
          done: true,
        },
        "num.blend": {
          sig: "blend(dst, src, alphaIn = 1)",
          desc: "Alpha blends two colors, mutating and returning `dst`.",
          done: true,
        },
        "num.rgbToHsl": {
          sig: "rgbToHsl(r, g, b)",
          desc: "Convert rgb to hsl (360, 100, 100).",
          done: true,
        },
        "num.hslToRgb": {
          sig: "hslToRgb(h, s, l)",
          desc: "Convert hsl (360, 100, 100) to rgb.",
          done: true,
        },
        "num.rainbow": {
          sig: "rainbow()",
          desc: "Return a cycled color from the `rainbow` template.",
          done: true,
        },
        delay: {
          sig: "delay(fun, time)",
          desc: "Delay a function by `time` number of sim steps.",
          done: true,
        },
        blink: {
          sig: "blink(time, fun)",
          desc: "A looped `delay`.",
          done: true,
        },
        "geo.Box": {
          sig: "new Box()",
          desc: "A dynamic box with helpful methods.",
          done: true,
        },
        "geo.DirtyBox": {
          sig: "new DirtyBox()",
          desc: "A box model implementing dirty rectangle optimization.",
          done: true,
        },
        "geo.Grid": {
          sig: "new Grid(x, y, w, h, s = 1)",
          desc: "A 2 dimensional uniform grid, using a box as the frame (with scaling).",
          done: true,
        },
        "geo.Circle": {
          sig: "new Circle(x, y, radius = 8)",
          desc: "A generic circle model.",
          done: true,
        },
        "geo.linePointsFromAngle": {
          sig: "linePointsFromAngle(x1, y1, dist, degrees)",
          desc: "Project outwards from an origin point at dist, and degrees to get the full line.",
          done: true,
        },
        "geo.pointFrom": {
          sig: "pointFrom(x, y, angle, dist)",
          desc: "Project outwards from a point at an `angle` and `dist` and get the resulting point.",
          done: true,
        },
        "geo.Race": {
          sig: "new Race(opts = { quantized: true })",
          desc: "Follows a point over time.",
          done: true,
        },
        "geo.Quantizer": {
          sig: "new Quantizer(opts)",
          desc: "A simple model for lazy following of a 3D point.",
          done: true,
        },
      },
      help: {
        choose: {
          sig: "choose(a, b, ...)",
          desc: "Randomly return one of the arguments.",
          returns: "any",
          done: true,
        },
        flip: {
          sig: "flip()",
          desc: "Flip a coin, returning true or false.",
          returns: "boolean",
          done: true,
        },
        repeat: {
          sig: "repeat(n, fn)",
          desc: "Run a function `n` times, passing in `i` on each iteration and returning an array of the results (like map).",
          params: [
            { name: "n", type: "number", required: true, desc: "Iteration count (floored)." },
            { name: "fn", type: "function", required: true, desc: "Callback receiving index i." },
          ],
          returns: "array",
          done: true,
        },
        every: {
          sig: "every(obj, value)",
          desc: "Set every property of an object to a certain value.",
          params: [
            { name: "obj", type: "object", required: true, desc: "Target object to mutate." },
            { name: "value", type: "any", required: true, desc: "Value assigned to every key." },
          ],
          returns: "void",
          done: true,
        },
        any: {
          sig: "any(objOrArray)",
          desc: "Returns a random value from an object, or array.",
          returns: "any",
          done: true,
        },
        anyIndex: {
          sig: "anyIndex(array)",
          desc: "Returns a random index value from an array.",
          returns: "number",
          done: true,
        },
        anyKey: {
          sig: "anyKey(obj)",
          desc: "Returns a random key from an object.",
          returns: "string",
          done: true,
        },
        each: {
          sig: "each(obj, fun)",
          desc: "Run a function on every value in an object.",
          params: [
            { name: "obj", type: "object", required: true, desc: "Object to iterate." },
            { name: "fun", type: "function", required: true, desc: "Callback receiving (value, key)." },
          ],
          returns: "void",
          done: true,
        },
        shuffleInPlace: {
          sig: "shuffleInPlace(array)",
          desc: "Shuffles an array, mutating it.",
          returns: "array",
          done: true,
        },
        "gizmo.Hourglass": {
          sig: "new Hourglass(max, { completed, flipped, every, autoFlip = false }, startingTicks = 0)",
          desc: "A repeatable timer with callbacks.",
          returns: "Hourglass",
          done: true,
        },
        "gizmo.EllipsisTicker": {
          sig: "new EllipsisTicker()",
          desc: "An animated `...` string for showing processing indicators.",
          returns: "EllipsisTicker",
          done: true,
        },
      },
      system: {
        signal: {
          sig: "signal(content)",
          desc: "Send a message through the `signal` system, good for communicating with added DOM content.",
          params: [{ name: "content", type: "any", required: true, desc: "Signal payload." }],
          returns: "void",
          done: true,
        },
        sideload: {
          sig: "sideload(type)",
          desc: "Open a file chooser to load a file.",
          params: [{ name: "type", type: "string", required: false, desc: "Optional file type filter." }],
          returns: "Promise<File | null>",
          done: true,
        },
        user: {
          sig: "user",
          desc: "A reference to the currently logged in user.",
          returns: "object | null",
          done: true,
        },
        vscode: {
          sig: "vscode",
          desc: "A flag that's true while running the VS Code extension.",
          returns: "boolean",
          done: true,
        },
        meta: {
          sig: "meta(data)",
          desc: "Add meta to the common api so the data can be overridden as needed.",
          params: [{ name: "data", type: "object", required: true, desc: "Meta fields to merge into runtime state." }],
          returns: "void",
          done: true,
        },
        reload: {
          sig: "reload({ piece, name, source, codeChannel })",
          desc: "Reload / start a piece in various ways. Used especially in live development.",
          params: [{ name: "options", type: "object", required: true, desc: "Reload options including piece/name/source/codeChannel." }],
          returns: "void",
          done: true,
        },
        pieceCount: {
          sig: "pieceCount",
          desc: "Keeps track of how many pieces have been run so far in a session.",
          returns: "number",
          done: true,
        },
        store: {
          sig: "store",
          desc: "An object for keeping data in across piece jumps.",
          returns: "object",
          done: true,
        },
        "store.persist": {
          sig: 'store.persist(key, method = "local")',
          desc: "Save a storage key with associated data in the user's browser.",
          params: [
            { name: "key", type: "string", required: true, desc: "Store key to persist." },
            { name: "method", type: "string", required: false, desc: "Persistence backend (for example `local` or `local:db`)." },
          ],
          returns: "Promise<void>",
          done: true,
        },
        "store.retrieve": {
          sig: 'store.retrieve(key, method = "local")',
          desc: "Load a storage key with associated data.",
          params: [
            { name: "key", type: "string", required: true, desc: "Store key to read." },
            { name: "method", type: "string", required: false, desc: "Persistence backend." },
          ],
          returns: "Promise<any>",
          done: true,
        },
        "store.delete": {
          sig: 'store.delete(key, method = "local")',
          desc: "Remove a storage key and any saved data.",
          params: [
            { name: "key", type: "string", required: true, desc: "Store key to remove." },
            { name: "method", type: "string", required: false, desc: "Persistence backend." },
          ],
          returns: "Promise<boolean>",
          done: true,
        },
        debug: {
          sig: "debug",
          desc: "Reports whether the system is in debug / development mode.",
          returns: "boolean",
          done: true,
        },
        canShare: {
          sig: "canShare",
          desc: "Whether the current environment supports the Web Share API.",
          returns: "boolean",
          done: true,
        },
        handle: {
          sig: "handle()",
          desc: "Returns the user's handle, if one exists.",
          returns: "string | null",
          done: true,
        },
        ticket: {
          sig: "ticket(name)",
          desc: "Open a ticketed paywall by its name.",
          params: [{ name: "name", type: "string", required: true, desc: "Ticket/paywall identifier." }],
          returns: "void",
          done: true,
        },
        mint: {
          sig: "mint(picture, progress, params)",
          desc: "Mint a picture on an external service.",
          params: [
            { name: "picture", type: "object", required: true, desc: "Painting/pixel payload." },
            { name: "progress", type: "function", required: false, desc: "Progress callback." },
            { name: "params", type: "object", required: false, desc: "Mint metadata/options." },
          ],
          returns: "Promise<any>",
          done: true,
        },
        print: {
          sig: "print(picture, quantity, progress)",
          desc: "Print the `pixels` that get passed in via an external service. Stickers only right now.",
          params: [
            { name: "picture", type: "object", required: true, desc: "Painting/pixel payload." },
            { name: "quantity", type: "number", required: false, desc: "Requested print quantity." },
            { name: "progress", type: "function", required: false, desc: "Progress callback." },
          ],
          returns: "Promise<any>",
          done: true,
        },
        zip: {
          sig: "zip(content, progress)",
          desc: "Create a zip file of the content. Auto-encodes paintings.",
          params: [
            { name: "content", type: "object | array", required: true, desc: "Files/content to include." },
            { name: "progress", type: "function", required: false, desc: "Progress callback." },
          ],
          returns: "Promise<Blob>",
          done: true,
        },
        "motion.start": {
          sig: "start()",
          desc: "Start tracking device motion.",
          returns: "Promise<void> | void",
          done: true,
        },
        "motion.stop": {
          sig: "stop()",
          desc: "Stop tracking device motion.",
          returns: "void",
          done: true,
        },
        "motion.current": {
          sig: "current",
          desc: "Populated with the device motion data upon `motion.start()`.",
          returns: "object | null",
          done: true,
        },
        speak: {
          sig: "speak(utterance, voice, mode, opts)",
          desc: "Speak an `utterance` aloud.",
          params: [
            { name: "utterance", type: "string", required: true, desc: "Text to speak." },
            { name: "voice", type: "string", required: false, desc: "Voice id/preset." },
            { name: "mode", type: "string", required: false, desc: "Speech backend mode." },
            { name: "opts", type: "object", required: false, desc: "Optional speech options." },
          ],
          returns: "Promise<void> | void",
          done: true,
        },
        act: {
          sig: "act(event, data)",
          desc: "Broadcast an `act` event through the system.",
          params: [
            { name: "event", type: "string", required: true, desc: "Event name." },
            { name: "data", type: "any", required: false, desc: "Optional payload." },
          ],
          returns: "void",
          done: true,
        },
        "get.painting().by()": {
          sig: "get.painting(code, opts).by(handle, opts)",
          desc: "Retrieve a painting from network storage.",
          returns: "Promise<object>",
          done: true,
        },
        upload: {
          sig: "async upload(filename, data, progress, bucket)",
          desc: "Upload a media file to network storage.",
          params: [
            { name: "filename", type: "string", required: true, desc: "Destination filename." },
            { name: "data", type: "Blob | Uint8Array | string", required: true, desc: "File payload." },
            { name: "progress", type: "function", required: false, desc: "Progress callback." },
            { name: "bucket", type: "string", required: false, desc: "Target storage bucket." },
          ],
          returns: "Promise<object>",
          done: true,
        },
        "code.channel": {
          sig: "channel(chan)",
          desc: "Set the current code channel for live development.",
          params: [{ name: "chan", type: "string", required: true, desc: "Channel name/id." }],
          returns: "void",
          done: true,
        },
        encode: {
          sig: "async encode(file)",
          desc: "File should be { type, data } where type is `png`, `webp`, or `jpg`, etc.",
          params: [{ name: "file", type: "object", required: true, desc: "Encoder payload {type, data, ...}." }],
          returns: "Promise<Blob | Uint8Array>",
          done: true,
        },
        file: {
          sig: "async file()",
          desc: "Request a file from the user.",
          returns: "Promise<File | null>",
          done: true,
        },
        authorize: {
          sig: "async authorize()",
          desc: "Authorize a user.",
          returns: "Promise<void>",
          done: true,
        },
        "hand.mediapipe": {
          sig: "mediapipe",
          desc: "A reference to the mediapipe hand tracking data. Enable through `video`.",
          returns: "object | null",
          done: true,
        },
        "hud.label": {
          sig: "label(text, color, offset)",
          desc: "Override the piece corner label.",
          params: [
            { name: "text", type: "string", required: true, desc: "Label text." },
            { name: "color", type: "string | array", required: false, desc: "Optional label color." },
            { name: "offset", type: "number", required: false, desc: "Optional offset/priority." },
          ],
          returns: "void",
          done: true,
        },
        "hud.currentStatusColor": {
          sig: "currentStatusColor()",
          desc: "Get the current connection status label color.",
          returns: "string | array",
          done: true,
        },
        "hud.currentLabel": {
          sig: "currentLabel()",
          desc: "Get the current label content and button.",
          returns: "object",
          done: true,
        },
        "hud.labelBack": {
          sig: "labelBack()",
          desc: "Jump to the `prompt` with the current label applied.",
          returns: "void",
          done: true,
        },
        send: {
          sig: "send({type, content})",
          desc: "Send a message to the bios.",
          params: [{ name: "message", type: "object", required: true, desc: "Message payload with `type` and optional `content`." }],
          returns: "void",
          done: true,
        },
        platform: {
          sig: "platform",
          desc: "Get the current host platform.",
          returns: "string",
          done: true,
        },
        history: {
          sig: "history",
          desc: "An array of previously visited pieces in a session.",
          returns: "array",
          done: true,
        },
        "bgm.set": {
          sig: "set(trackNumber, volume)",
          desc: "Start a background music track, persisting across jumps.",
          params: [
            { name: "trackNumber", type: "number | string", required: true, desc: "Track id/index." },
            { name: "volume", type: "number", required: false, desc: "Playback gain." },
          ],
          returns: "void",
          done: true,
        },
        "bgm.stop": {
          sig: "stop()",
          desc: "Stop a background music track.",
          returns: "void",
          done: true,
        },
        "bgm.data": {
          sig: "data",
          desc: "Gets live analysis data from the current background track.",
          returns: "object | null",
          done: true,
        },
        "system.world": {
          sig: "system.world",
          desc: "A reference to the world system state if a piece is using it.",
          returns: "object | null",
          done: true,
        },
        "system.nopaint": {
          sig: "system.nopaint",
          desc: "A reference to the `nopaint` system state that all brushes use.",
          returns: "object",
          done: true,
        },
        flatten: {
          sig: "flatten()",
          desc: "Paint (bake) all graphics commands immediately.",
          returns: "void",
          done: true,
        },
        connect: {
          sig: "connect()",
          desc: "Connect with external wallet software.",
          returns: "Promise<void> | void",
          done: true,
        },
        wiggle: {
          sig: "wiggle(n, level, speed)",
          desc: "Oscillate a value over time using a sine wave.",
          params: [
            { name: "n", type: "number", required: true, desc: "Base value." },
            { name: "level", type: "number", required: false, desc: "Amplitude." },
            { name: "speed", type: "number", required: false, desc: "Oscillation speed." },
          ],
          returns: "number",
          done: true,
        },
        dark: {
          sig: "dark",
          desc: "Gets whether the system is in dark mode.",
          returns: "boolean",
          done: true,
        },
        darkMode: {
          sig: "darkMode(enabled)",
          desc: "Toggle dark mode on or off with a boolean.",
          params: [{ name: "enabled", type: "boolean", required: true, desc: "Target dark mode state." }],
          returns: "void",
          done: true,
        },
        gpuReady: {
          sig: "gpuReady",
          desc: "Whether the system GPU is ready for rendering.",
          returns: "boolean",
          done: true,
        },
        "gpu.message": {
          sig: "message(content)",
          desc: "Send a message to the GPU driver.",
          params: [{ name: "content", type: "object", required: true, desc: "Driver command payload." }],
          returns: "void",
          done: true,
        },
      },
      mjs: {
        overview: {
          sig: "MJS / AC piece API overview",
          desc: "Entry point for JavaScript piece API docs.",
          body: mjsOverviewBody,
          done: true,
        },
      },
      kidlisp: {
        overview: {
          sig: "KidLisp API overview",
          desc: "How KidLisp docs connect into the unified AC docs system.",
          body: kidlispOverviewBody,
          done: true,
        },
        core: {
          sig: "KidLisp core map",
          desc: "Core families and canonical source links for KidLisp APIs.",
          body: kidlispCoreBody,
          done: true,
        },
      },
      l5: {
        overview: {
          sig: "L5 (Lua) on Aesthetic Computer",
          desc: "Processing-style Lua compatibility notes and rollout status.",
          body: `
            <p>
              This is the implementation board for L5 support in AC.
              Keep this page aligned with the actual runtime state.
            </p>
            <p>
              <a href="${AC_ORIGIN}/docs/l5:checklist">Open checklist</a> ·
              <a href="${AC_ORIGIN}/docs/l5:examples">Open examples</a> ·
              <a href="${AC_ORIGIN}/l5">Open /l5 try page</a>
            </p>
          `.trim(),
          done: true,
        },
        checklist: {
          sig: "L5 compatibility checklist (v0)",
          desc: "Single source of truth for what is implemented right now.",
          body: l5ChecklistBody,
          done: true,
        },
        lifecycle: {
          sig: "L5 lifecycle bridge",
          desc: "How setup/draw/events map onto AC piece lifecycle hooks.",
          body: l5LifecycleBody,
          done: "in-progress",
        },
        graphics: {
          sig: "L5 graphics mapping",
          desc: "Core drawing calls and their AC equivalents.",
          body: l5GraphicsBody,
          done: "in-progress",
        },
        input: {
          sig: "L5 input globals",
          desc: "Frame-updated globals expected by Processing-style sketches.",
          body: l5InputBody,
          done: "in-progress",
        },
        unsupported: {
          sig: "Known gaps / out of scope (v1)",
          desc: "Features explicitly not shipped yet.",
          body: l5UnsupportedBody,
          done: true,
        },
        examples: {
          sig: "L5 example snippets",
          desc: "Starter Lua examples for the upcoming runtime.",
          body: l5ExamplesBody,
          done: true,
        },
        size: {
          sig: "size(width, height?)",
          desc: "Set sketch resolution by forwarding to AC `resolution()`.",
          params: [
            { name: "width", type: "number", required: true, desc: "Target width." },
            { name: "height", type: "number", required: false, desc: "Target height (defaults to width)." },
          ],
          returns: "void",
          done: true,
        },
        background: {
          sig: "background(r, g, b, a?)",
          desc: "Clear frame with a solid color.",
          returns: "void",
          done: true,
        },
        clear: {
          sig: "clear()",
          desc: "Clear frame to transparent black.",
          returns: "void",
          done: true,
        },
        fill: {
          sig: "fill(r, g, b, a?)",
          desc: "Set fill color for subsequent shape and text draws.",
          returns: "void",
          done: true,
        },
        noFill: {
          sig: "noFill()",
          desc: "Disable shape fill rendering.",
          returns: "void",
          done: true,
        },
        stroke: {
          sig: "stroke(r, g, b, a?)",
          desc: "Set stroke color for line/outline rendering.",
          returns: "void",
          done: true,
        },
        noStroke: {
          sig: "noStroke()",
          desc: "Disable stroke rendering.",
          returns: "void",
          done: true,
        },
        strokeWeight: {
          sig: "strokeWeight(weight)",
          desc: "Set line/outline thickness.",
          params: [{ name: "weight", type: "number", required: true, desc: "Stroke width in pixels." }],
          returns: "void",
          done: true,
        },
        point: {
          sig: "point(x, y)",
          desc: "Plot a single point using stroke color if stroke is enabled.",
          returns: "void",
          done: true,
        },
        line: {
          sig: "line(x1, y1, x2, y2)",
          desc: "Draw a line using stroke color.",
          returns: "void",
          done: true,
        },
        rect: {
          sig: "rect(x, y, width, height)",
          desc: "Draw a rectangle with active fill/stroke state.",
          returns: "void",
          done: true,
        },
        square: {
          sig: "square(x, y, size)",
          desc: "Draw a square with active fill/stroke state.",
          returns: "void",
          done: true,
        },
        circle: {
          sig: "circle(x, y, diameter)",
          desc: "Draw a circle with active fill/stroke state.",
          returns: "void",
          done: true,
        },
        ellipse: {
          sig: "ellipse(x, y, width, height)",
          desc: "Draw an ellipse with active fill/stroke state.",
          returns: "void",
          done: true,
        },
        triangle: {
          sig: "triangle(x1, y1, x2, y2, x3, y3)",
          desc: "Draw a triangle with active fill/stroke state.",
          returns: "void",
          done: true,
        },
        quad: {
          sig: "quad(x1, y1, x2, y2, x3, y3, x4, y4)",
          desc: "Draw a quadrilateral with active fill/stroke state.",
          returns: "void",
          done: true,
        },
        text: {
          sig: "text(value, x, y)",
          desc: "Draw text at coordinates using fill color.",
          returns: "void",
          done: true,
        },
        textSize: {
          sig: "textSize(size)",
          desc: "Set text size scale for subsequent `text()` draws.",
          params: [{ name: "size", type: "number", required: true, desc: "Text size value." }],
          returns: "void",
          done: true,
        },
        textWidth: {
          sig: "textWidth(value)",
          desc: "Measure text width in pixels.",
          returns: "number",
          done: true,
        },
        frameRate: {
          sig: "frameRate(fps)",
          desc: "Request a target frame rate for draw calls.",
          params: [{ name: "fps", type: "number", required: true, desc: "Target frames per second." }],
          returns: "void",
          done: true,
        },
        noLoop: {
          sig: "noLoop()",
          desc: "Stop continuous draw execution.",
          returns: "void",
          done: true,
        },
        loop: {
          sig: "loop()",
          desc: "Resume continuous draw execution.",
          returns: "void",
          done: true,
        },
        isLooping: {
          sig: "isLooping()",
          desc: "Get whether draw loop is active.",
          returns: "boolean",
          done: true,
        },
        redraw: {
          sig: "redraw()",
          desc: "Request a one-off draw when loop is disabled.",
          returns: "void",
          done: true,
        },
        random: {
          sig: "random(max?) or random(min, max)",
          desc: "Generate a random number with optional range.",
          returns: "number",
          done: true,
        },
        map: {
          sig: "map(value, inMin, inMax, outMin, outMax)",
          desc: "Remap a value from one range to another.",
          returns: "number",
          done: true,
        },
        dist: {
          sig: "dist(x1, y1, x2, y2)",
          desc: "Calculate Euclidean distance between two points.",
          returns: "number",
          done: true,
        },
        lerp: {
          sig: "lerp(start, stop, amount)",
          desc: "Linear interpolate between two values.",
          returns: "number",
          done: true,
        },
        radians: {
          sig: "radians(degrees)",
          desc: "Convert degrees to radians.",
          returns: "number",
          done: true,
        },
        degrees: {
          sig: "degrees(radians)",
          desc: "Convert radians to degrees.",
          returns: "number",
          done: true,
        },
        constrain: {
          sig: "constrain(value, min, max)",
          desc: "Clamp a value into a minimum/maximum range.",
          returns: "number",
          done: true,
        },
        millis: {
          sig: "millis()",
          desc: "Get elapsed runtime milliseconds.",
          returns: "number",
          done: true,
        },
        frameCount: {
          sig: "frameCount",
          desc: "Number of draw frames processed so far.",
          returns: "number",
          done: true,
        },
        width: {
          sig: "width",
          desc: "Current sketch width in pixels.",
          returns: "number",
          done: true,
        },
        height: {
          sig: "height",
          desc: "Current sketch height in pixels.",
          returns: "number",
          done: true,
        },
        mouseX: {
          sig: "mouseX",
          desc: "Current pointer X coordinate.",
          returns: "number",
          done: true,
        },
        mouseY: {
          sig: "mouseY",
          desc: "Current pointer Y coordinate.",
          returns: "number",
          done: true,
        },
        pmouseX: {
          sig: "pmouseX",
          desc: "Previous frame pointer X coordinate.",
          returns: "number",
          done: true,
        },
        pmouseY: {
          sig: "pmouseY",
          desc: "Previous frame pointer Y coordinate.",
          returns: "number",
          done: true,
        },
        mouseIsPressed: {
          sig: "mouseIsPressed",
          desc: "Whether pointer is currently pressed.",
          returns: "boolean",
          done: true,
        },
        key: {
          sig: "key",
          desc: "Last key value from keyboard events.",
          returns: "string",
          done: true,
        },
        keyCode: {
          sig: "keyCode",
          desc: "Last key code value from keyboard events.",
          returns: "number",
          done: true,
        },
        keyIsPressed: {
          sig: "keyIsPressed",
          desc: "Whether a key is currently pressed.",
          returns: "boolean",
          done: true,
        },
      },
    },
    // 😱 Commands for entering into the prompt.
    prompts: {
      // 📦 Pack / Export
      pack: {
        sig: "pack <piece>",
        desc: "Download a piece as a self-contained HTML file.",
        params: [
          { name: "piece", type: "string", required: true, desc: "Piece name or $code" }
        ],
        done: true,
      },
      bundle: {
        sig: "bundle <piece>",
        desc: "Download a piece as a self-contained HTML file.",
        params: [
          { name: "piece", type: "string", required: true, desc: "Piece name or $code" }
        ],
        done: true,
      },
      m4d: {
        sig: "m4d <piece>",
        desc: "Download a piece as an offline Max for Live device (.amxd).",
        params: [
          { name: "piece", type: "string", required: true, desc: "Piece name or $code" }
        ],
        done: true,
      },
      "4d": {
        sig: "4d <piece>",
        desc: "Download a piece as an offline Max for Live device (.amxd).",
        params: [
          { name: "piece", type: "string", required: true, desc: "Piece name or $code" }
        ],
        done: true,
      },
      m4do: {
        sig: "m4do <piece>",
        desc: "Download a piece as an online Max for Live device (.amxd) that streams from aesthetic.computer.",
        params: [
          { name: "piece", type: "string", required: true, desc: "Piece name" }
        ],
        done: true,
      },
      "4do": {
        sig: "4do <piece>",
        desc: "Download a piece as an online Max for Live device (.amxd) that streams from aesthetic.computer.",
        params: [
          { name: "piece", type: "string", required: true, desc: "Piece name" }
        ],
        done: true,
      },
      // 🔷 Tezos Wallet
      tezos: {
        sig: "tezos <action> [network]",
        desc: "Manage Tezos wallet.",
        params: [
          { name: "action", type: "enum", values: ["connect", "disconnect", "status"], required: true },
          { name: "network", type: "enum", values: ["ghostnet", "mainnet"], required: false, default: "ghostnet" }
        ],
        done: true,
      },
      // 🎨 KidLisp Keeping
      keep: {
        sig: "keep $code",
        desc: "Keep $code in your wallet.",
        params: [
          { name: "code", type: "string", prefix: "$", required: true, desc: "KidLisp piece code" }
        ],
        done: true,
      },
      tape: {
        sig: "tape [duration] [flags]",
        desc: "Record your screen.",
        params: [
          { name: "duration", type: "number", required: false, default: 5, desc: "Seconds (add 'f' for frames)" },
          { name: "flags", type: "flags", values: ["mic", "nomic", "baktok"], required: false }
        ],
        done: true,
      },
      "tape:add": {
        sig: "tape:add",
        desc: "Add time to your tape.",
        done: false,
        hidden: true,
      },
      "tape:tt": {
        sig: "tape:tt",
        desc: "",
        done: false,
        hidden: true,
      },
      "tape:nomic": {
        sig: "tape:nomic",
        desc: "",
        done: false,
        hidden: true,
      },
      "tape:mic": {
        sig: "tape:mic",
        desc: "",
        done: false,
        hidden: true,
      },
      tapem: {
        sig: "tapem",
        desc: "",
        done: false,
        hidden: true,
      },
      "tape:cut": {
        sig: "tape:cut",
        desc: "",
        done: false,
        hidden: true,
      },
      cut: {
        sig: "cut",
        desc: "Stop a tape.",
        done: false,
      },
      me: {
        sig: "me",
        desc: "Go to your profile.",
        done: false,
      },
      scream: {
        sig: "scream <message>",
        desc: "Scream at all users.",
        params: [
          { name: "message", type: "string", required: true, desc: "Your scream text" }
        ],
        done: true,
      },
      nonotifs: {
        sig: "nonotifs",
        desc: "Turn off notifications.",
        done: false,
      },
      notifs: {
        sig: "notifs",
        desc: "Turn on notifications.",
        done: false,
      },
      news: {
        sig: "news",
        desc: "Aesthetic.computer news.",
        done: true,
      },
      nela: {
        sig: "nela",
        desc: "Open NELA Computer Club.",
        done: true,
      },
      selfie: {
        sig: "selfie",
        desc: "Open the front camera.",
        done: false,
      },
      cam: {
        sig: "cam",
        desc: "Take a picture.",
        done: false,
      },
      camu: {
        sig: "camu",
        desc: "",
        done: false,
        hidden: true,
      },
      sparkle: {
        sig: "sparkle",
        desc: "Paint with Maya's really fun brush.",
        done: false,
      },
      "painting:start": {
        sig: "painting:start",
        desc: "",
        done: false,
        hidden: true,
      },
      print: {
        sig: "print",
        desc: "Make a sticker.",
        done: false,
      },
      mint: {
        sig: "mint",
        desc: "Mint your painting.",
        done: false,
        //TODO: can this open in new tab?
      },
      "painting:done": {
        sig: "painting:done",
        desc: "",
        done: false,
        hidden: true,
      },
      "yes!": {
        sig: "yes!",
        desc: "Finish your painting.",
        done: false,
      },
      done: {
        sig: "done",
        desc: "Finish your painting.",
        done: false,
      },
      flower: {
        sig: "flower",
        desc: "He loves me.",
        done: false,
        hidden: true,
      },
      petal: {
        sig: "petal",
        desc: "He loves me not.",
        done: false,
        hidden: true,
      },
      bro: {
        sig: "bro",
        desc: "Stay out of his room.",
        done: false,
      },
      sis: {
        sig: "sis",
        desc: "Don't steal her makeup.",
        done: false,
      },
      gf: {
        sig: "gf",
        desc: "Caring confidant.",
        done: false,
      },
      bf: {
        sig: "bf",
        desc: "He might care.",
        done: false,
      },
      bb: {
        sig: "bb",
        desc: "AC fundraiser.",
        done: false,
      },
      p: {
        sig: "p",
        desc: "View your current painting's steps.",
        done: false,
      },
      pain: {
        sig: "pain",
        desc: "View your current painting's steps.",
        done: false,
      },
      load: {
        sig: "load",
        desc: "",
        done: false,
        hidden: true,
      },
      "mood:nuke": {
        sig: "mood:nuke",
        desc: "",
        done: false,
        hidden: true,
      },
      "mood:denuke": {
        sig: "mood:denuke",
        desc: "",
        done: false,
        hidden: true,
      },
      mood: {
        sig: "mood [emoji]",
        desc: "Set your mood.",
        params: [
          { name: "emoji", type: "string", required: false, desc: "Emoji or text mood" }
        ],
        done: true,
      },
      channel: {
        sig: "channel [name]",
        desc: "View or set a piece code channel.",
        params: [
          { name: "name", type: "string", required: false, desc: "Channel name to join" }
        ],
        done: true,
      },
      "code-channel": {
        sig: "code-channel",
        desc: "",
        done: false,
        hidden: true,
      },
      run: {
        sig: "run",
        desc: "",
        done: false,
        hidden: true,
      },
      docs: {
        sig: "docs",
        desc: "Aesthetic Computer Documentation.",
        done: false,
      },
      l5docs: {
        sig: "l5docs",
        desc: "Open the L5 compatibility docs checklist.",
        done: true,
      },
      l5: {
        sig: "l5",
        desc: "Open the L5 try page.",
        done: true,
      },
      l5learn: {
        sig: "l5learn",
        desc: "Open the L5 try page.",
        done: true,
      },
      code: {
        sig: "code [name]",
        desc: "Write a piece.",
        params: [
          { name: "name", type: "string", required: false, desc: "Piece name (creates new)" }
        ],
        done: true,
      },
      edit: {
        sig: "edit <piece>",
        desc: "Edit a piece.",
        params: [
          { name: "piece", type: "string", required: true, desc: "Piece name to edit" }
        ],
        done: true,
      },
      source: {
        sig: "source [piece]",
        desc: "Download piece code.",
        params: [
          { name: "piece", type: "string", required: false, desc: "Piece name (or current)" }
        ],
        done: true,
      },
      email: {
        sig: "email <address>",
        desc: "Update your email.",
        params: [
          { name: "address", type: "email", required: true, desc: "New email address" }
        ],
        done: true,
        hidden: false,
      },
      "admin:migrate-": {
        sig: "admin:migrate-",
        desc: "",
        done: false,
        hidden: true,
      },
      handle: {
        sig: "handle <name>",
        desc: "Set your user handle.",
        params: [
          { name: "name", type: "string", required: true, desc: "New handle (alphanumeric)" }
        ],
        done: true,
      },
      handles: {
        sig: "handles",
        desc: "Browse all user handles.",
        done: true,
      },
      ul: {
        sig: "ul",
        desc: "Upload your painting.",
        done: false,
      },
      upload: {
        sig: "upload",
        desc: "Upload your painting.",
        done: false,
      },
      flip: {
        sig: "flip",
        desc: "Flip painting vertically.",
        done: false,
      },
      flop: {
        sig: "flop",
        desc: "Flop painting horizontally.",
        done: false,
      },
      right: {
        sig: "right",
        desc: "Rotate painting right.",
        done: false,
      },
      left: {
        sig: "left",
        desc: "Rotate painting left.",
        done: false,
      },
      resize: {
        sig: "resize <w> [h]",
        desc: "Resize by x and y pixel #s.",
        params: [
          { name: "w", type: "number", required: true, desc: "Width in pixels" },
          { name: "h", type: "number", required: false, desc: "Height (defaults to w)" }
        ],
        done: true,
      },
      res: {
        sig: "res <w> [h]",
        desc: "Resize by x and y pixel #s.",
        params: [
          { name: "w", type: "number", required: true, desc: "Width in pixels" },
          { name: "h", type: "number", required: false, desc: "Height (defaults to w)" }
        ],
        done: true,
      },
      dl: {
        sig: "dl [scale]",
        desc: "Download your painting.",
        params: [
          { name: "scale", type: "number", required: false, default: 1, desc: "Scale multiplier" }
        ],
        done: true,
      },
      download: {
        sig: "download [scale]",
        desc: "Download your painting.",
        params: [
          { name: "scale", type: "number", required: false, default: 1, desc: "Scale multiplier" }
        ],
        done: true,
      },
      gutter: {
        sig: "gutter",
        desc: "",
        done: false,
        hidden: true,
      },
      login: {
        sig: "login",
        desc: "Log in.",
        done: false,
      },
      hi: {
        sig: "hi",
        desc: "Log in.",
        done: false,
      },
      signup: {
        sig: "signup",
        desc: "Sign up.",
        done: false,
      },
      imnew: {
        sig: "imnew",
        desc: "Sign up.",
        done: false,
      },
      logout: {
        sig: "logout",
        desc: "Log out.",
        done: false,
      },
      bye: {
        sig: "bye",
        desc: "Leave a bot / character or log out.",
        done: false,
      },
      no: {
        sig: "no",
        desc: "Undo painting step.",
        done: false,
      },
      yes: {
        sig: "yes",
        desc: "Redo painting step.",
        done: false,
      },
      nopan: {
        sig: "nopan",
        desc: "Center your painting.",
        done: false,
      },
      new: {
        sig: "new",
        desc: "Start a new painting.",
        done: false,
      },
      "painting:reset": {
        sig: "painting:reset",
        desc: "",
        done: false,
        hidden: true,
      },
      publish: {
        sig: "publish",
        desc: "Publish your last-run piece.",
        done: false,
      },
      "no!": {
        sig: "no!",
        desc: "Abandon your painting.",
        done: false,
      },
      "3ine:reset": {
        sig: "3ine:reset",
        desc: "",
        done: false,
        hidden: true,
      },
      dark: {
        sig: "dark",
        desc: "Enable dark system theme.",
        done: false,
      },
      light: {
        sig: "light",
        desc: "Enable light system theme.",
        done: false,
      },
      serious: {
        sig: "serious",
        desc: "Toggle minimal black & white prompt.",
        done: false,
      },
      stop: {
        sig: "stop",
        desc: "Stop a running merry pipeline.",
        done: false,
      },
      mug: {
        sig: "mug [code] [color]",
        desc: "Preview & order a mug with a painting.",
        params: [
          { name: "code", type: "string", required: false, desc: "Painting code" },
          { name: "color", type: "string", required: false, desc: "Mug color (white, black, blue, pink, orange)" },
        ],
        done: true,
      },
      merry: {
        sig: "merry [duration-]piece ...",
        desc: "Run pieces in sequence.",
        params: [
          { name: "pieces", type: "string", required: true, desc: "Pieces to chain, optionally with duration prefix" },
        ],
        done: true,
      },
      merryo: {
        sig: "merryo [duration-]piece ...",
        desc: "Run pieces in a loop.",
        params: [
          { name: "pieces", type: "string", required: true, desc: "Pieces to chain and loop" },
        ],
        done: true,
      },
      mo: {
        sig: "mo[.duration] piece ...",
        desc: "Shorthand for merryo (looping merry).",
        done: true,
      },
      desktop: {
        sig: "desktop",
        desc: "Download the desktop app.",
        done: false,
      },
      chatgpt: {
        sig: "chatgpt",
        desc: "Open ChatGPT.",
        done: false,
        hidden: true,
      },
      nws: {
        sig: "nws",
        desc: "Open Aesthetic News.",
        done: false,
        hidden: true,
      },
      product: {
        sig: "product [key]",
        desc: "Switch or view active shop product.",
        done: false,
        hidden: true,
      },
      2022: {
        sig: "2022",
        desc: "",
        done: false,
        hidden: true,
      },
      connect: {
        sig: "connect",
        desc: "",
        done: false,
        hidden: true,
      },
      "bgm stop": {
        sig: "bgm stop",
        desc: "",
        done: false,
        hidden: true,
      },
      "+": {
        sig: "+",
        desc: "Make new window.",
        done: false,
        //TODO: can this open in a new tab?
      },
      google: {
        sig: "google <query>",
        desc: "Search google.",
        params: [
          { name: "query", type: "string", required: true, desc: "Search query" }
        ],
        done: true,
      },
      github: {
        sig: "github",
        desc: "View AC source code.",
        done: false,
        //TODO: can this open in a new tab?
      },
      gmail: {
        sig: "gmail",
        desc: "Go to gmail.",
        done: false,
      },
      gh: {
        sig: "gh",
        desc: "View AC source code.",
        done: false,
      },
      ucla: {
        sig: "ucla-syllabus",
        desc: "UCLA DESMA 28 - Syllabus",
        done: false,
      },
      "ucla-1": {
        sig: "ucla-1",
        desc: "UCLA DESMA 28 - Piece 1",
        done: false,
      },
      "ucla-2": {
        sig: "ucla-2",
        desc: "UCLA DESMA 28 - Piece 2",
        done: false,
      },
      "ucla-3": {
        sig: "ucla-3",
        desc: "UCLA DESMA 28 - Piece 3",
        done: false,
      },
      "ucla-4": {
        sig: "ucla-4",
        desc: "UCLA DESMA 28 - Piece 4",
        done: false,
      },
      "ucla-4-box": {
        sig: "ucla-4-box",
        desc: "UCLA DESMA 28 - Piece 4 (Box)",
        done: false,
      },
      "ucla-5": {
        sig: "ucla-5",
        desc: "UCLA DESMA 28 - Piece 5",
        done: false,
      },
      "ucla-6": {
        sig: "ucla-6",
        desc: "UCLA DESMA 28 - Piece 6",
        done: false,
      },
      "ucla-7": {
        sig: "ucla-7",
        desc: "UCLA DESMA 28 - Piece 7",
        done: false,
      },
      "ucla-7-dial": {
        sig: "ucla-7-dial",
        desc: "UCLA DESMA 28 - Piece 7 (Dial)",
        done: false,
      },
      "ucla-7-jump": {
        sig: "ucla-7-jump",
        desc: "UCLA DESMA 28 - Piece 7 (Jump)",
        done: false,
      },
      app: {
        sig: "app",
        desc: "Get AC in the app store.",
        done: false,
      },
      ios: {
        sig: "ios",
        desc: "Get AC in the app store.",
        done: false,
      },
      pp: {
        sig: "pp",
        desc: "Read the privacy policy.",
        done: false,
      },
      direct: {
        sig: "direct",
        desc: "Aesthetic Inc. corporate updates.",
        done: false,
      },
      support: {
        sig: "support",
        desc: "Go to AC support page.",
        done: false,
      },
      browserstack: {
        sig: "browserstack",
        desc: "Go to AC browserstack.",
        done: false,
        hidden: true,
      },
      bs: {
        sig: "bs",
        desc: "Go to AC browser stack.",
        done: false,
        hidden: true,
      },
      gpt: {
        sig: "gpt",
        desc: "",
        done: false,
        hidden: true,
      },
      help: {
        sig: "help",
        desc: "Join a help channel.",
        done: false,
      },
      shillball: {
        sig: "shillball",
        desc: "",
        done: false,
        hidden: true,
      },
      sb: {
        sig: "sb",
        desc: "",
        done: false,
        hidden: true,
      },
      prod: {
        sig: "prod",
        desc: "",
        done: false,
        hidden: true,
      },
      local: {
        sig: "local",
        desc: "",
        done: false,
        hidden: true,
      },
      of: {
        sig: "of",
        desc: "View ordfish paintings.",
        done: false,
        //TODO: fix swimming count, stuck on 56
      },
    },
    // 🧩 Pieces that can be entered into the prompt.
    pieces: {
      404: {
        sig: "404",
        desc: "",
        done: false,
        hidden: true,
      },
      about: {
        sig: "about",
        desc: "",
        done: false,
        hidden: true,
      },
      aframe: {
        sig: "aframe",
        desc: "",
        done: false,
        hidden: true,
      },
      "a*": {
        sig: "a*",
        desc: "A* pathfinding animation.",
        done: true,
      },
      "alex-row": {
        sig: "alex-row",
        desc: "",
        done: false,
        hidden: true,
      },
      alphapoet: {
        sig: "alphapoet",
        desc: "Generate poems.",
        done: false,
      },
      angel: {
        sig: "angel",
        desc: "Say a prayer.",
        done: false,
      },
      api: {
        sig: "api",
        desc: "",
        done: false,
        hidden: true,
      },
      baktok: {
        sig: "baktok",
        desc: "Learn to talk backwards.",
        done: false,
      },
      balls: {
        sig: "balls",
        desc: "",
        done: false,
        hidden: true,
      },
      "basic-line-pointer": {
        sig: "basic-line-pointer",
        desc: "Drag mouse to point a line.",
        done: false,
      },
      bgm: {
        sig: "bgm",
        desc: "Background music visualizer.",
        done: false,
        hidden: false,
      },
      bits: {
        sig: "bits",
        desc: "",
        done: false,
        hidden: false,
      },
      blank: {
        sig: "blank",
        desc: "",
        done: false,
        hidden: true,
      },
      "blank-vello": {
        sig: "blank-vello",
        desc: "GPU test: Vello WASM renderer (purple lines)",
        done: true,
      },
      "blank-webgl2": {
        sig: "blank-webgl2",
        desc: "GPU test: WebGL2 renderer (cyan lines)",
        done: true,
      },
      "blank-canvas2d": {
        sig: "blank-canvas2d",
        desc: "GPU test: Canvas2D fallback (green lines)",
        done: true,
      },
      "blank-thorvg": {
        sig: "blank-thorvg",
        desc: "GPU test: ThorVG WASM stub (orange lines)",
        done: true,
      },
      "blank-blend2d": {
        sig: "blank-blend2d",
        desc: "GPU test: Blend2D WASM stub (magenta lines)",
        done: true,
      },
      bleep: {
        sig: "bleep",
        desc: "Play notes on a grid. Try adding a #.",
        done: false,
      },
      blur: {
        sig: "blur",
        desc: "Blur pixels.",
        done: false,
      },
      box: {
        sig: "box[:color]",
        desc: "Draw rectangles with brush gestures.",
        colon: [
          { name: "color", values: ["red", "green", "blue", "yellow", "white", "black", "orange", "purple", "pink", "cyan"] },
        ],
        done: true,
      },
      "booted-by": {
        sig: "booted-by",
        desc: "Special thanks to early patrons.",
        done: false,
      },
      // botce: {
      //   sig: "botce",
      //   desc: "Get spiritual advice.",
      //   done: false,
      // },
      boxes: {
        sig: "boxes",
        desc: "",
        done: false,
        hidden: true,
      },
      boyfriend: {
        sig: "boyfriend",
        desc: "He might care.",
        done: false,
      },
      "brick-breaker": {
        sig: "brick-breaker",
        desc: "",
        done: false,
        hidden: true,
      },
      brother: {
        sig: "brother",
        desc: "Stay out of his room.",
        done: false,
      },
      brush: {
        sig: "brush",
        desc: "",
        done: false,
        hidden: true,
        //TODO: add new templates repository
      },
      bubble: {
        sig: "bubble",
        desc: "Make bubble boing. Sound on.",
        done: false,
      },
      butterflies: {
        sig: "butterflies",
        desc: "A 1-bit bitmap reader instrument.",
        done: false,
      },
      camera: {
        sig: "camera[:mode]",
        desc: "Take a picture.",
        colon: [
          { name: "mode", type: "enum", values: ["under", "u"], required: false, desc: "Put camera under drawing" }
        ],
        examples: ["camera", "camera:under"],
        done: true,
      },
      chat: {
        sig: "chat",
        desc: "Chat with handles.",
        done: false,
        hidden: false,
      },
      chord: {
        sig: "chord",
        desc: "",
        done: false,
        hidden: false,
      },
      colors: {
        sig: "colors",
        desc: "An index of usable colors on AC.",
        done: false,
        hidden: false,
      },
      colplay: {
        sig: "colplay",
        desc: "Turn colors into notes.",
        done: false,
      },
      common: {
        sig: "common",
        desc: "",
        done: false,
        hidden: true,
      },
      clock: {
        sig: "clock[:divisor] [melody] [sync]",
        desc: "Musical clock with melody, waveforms, Hz shifts, and parallel tracks.",
        colon: [
          { name: "divisor", type: "number", required: false, default: 1, desc: "Time divisor (0.5 = faster, 2 = slower)" }
        ],
        params: [
          { name: "melody", type: "string", required: false, desc: "Notes like cdefg, {square}cde, (ceg) (dfa)" },
          { name: "sync", type: "enum", values: ["sync"], required: false, desc: "UTC sync mode" }
        ],
        examples: ["clock cdefg", "clock:0.5 {square}cdefgab", "clock (ceg) (dfa)", "clock ^cdefg"],
        done: true
      },
      commits: {
        sig: "commits",
        desc: "Browse the live commit history.",
        done: true,
      },
      crop: {
        sig: "crop",
        desc: "Crop your painting.",
        done: false,
      },
      dad: {
        sig: "dad",
        desc: "A dad-icated and punny guy.",
        done: false,
      },
      debug: {
        sig: "debug",
        desc: "",
        done: false,
        hidden: true,
      },
      deck: {
        sig: "deck",
        desc: "A little slide deck!",
        done: false,
        hidden: false,
      },
      decode: {
        sig: "decode",
        desc: "Reveal an encoded message. See encode.",
        done: false,
      },
      "delete-erase-and-forget-me": {
        sig: "delete-erase-and-forget-me",
        desc: "Delete your account.",
        done: false,
      },
      // 🔗 External Links
      github: {
        sig: "github",
        desc: "Open the AC GitHub repo.",
        done: true,
      },
      gh: {
        sig: "gh",
        desc: "Open the AC GitHub repo.",
        done: true,
      },
      gmail: {
        sig: "gmail",
        desc: "Open Gmail.",
        done: true,
      },
      agc: {
        sig: "agc",
        desc: "Open ACG at MIT Media Lab.",
        done: true,
      },
      "ucla-syllabus": {
        sig: "ucla-syllabus",
        desc: "Open the UCLA syllabus.",
        done: true,
      },
      demo: {
        sig: "demo",
        desc: "Watch a demo of AC.",
        done: false,
        hidden: false,
      },
      description: {
        sig: "description",
        desc: "",
        done: false,
        hidden: true,
      },
      digitpain0: {
        sig: "digitpain0",
        desc: "",
        done: false,
        hidden: true,
      },
      digitpain1: {
        sig: "digitpain1",
        desc: "",
        done: false,
        hidden: true,
      },
      digitpain2: {
        sig: "digitpain2",
        desc: "",
        done: false,
        hidden: true,
      },
      digitpain3: {
        sig: "digitpain3",
        desc: "",
        done: false,
        hidden: true,
      },
      docgen: {
        sig: "docgen",
        desc: "",
        done: false,
        hidden: true,
      },
      dolls: {
        sig: "dolls",
        desc: "",
        done: false,
        hidden: true,
      },
      doodle: {
        sig: "doodle",
        desc: "",
        done: false,
        hidden: true,
      },
      download: {
        sig: "download",
        desc: "Download your painting.",
        done: false,
      },
      drawings: {
        sig: "drawings",
        desc: "",
        done: false,
        hidden: true,
      },
      dync: {
        sig: "dync",
        desc: "",
        done: false,
        hidden: true,
      },
      encode: {
        sig: "encode",
        desc: "Encrypt a secret message.",
        done: false,
      },
      ff: {
        sig: "ff",
        desc: "View the Freaky Flowers collection.",
        done: false,
      },
      field: {
        sig: "field",
        desc: "Play in the field with others.",
        done: false,
      },
      fill: {
        sig: "fill",
        desc: "Fill with solid color.",
        done: false,
      },
      fly: {
        sig: "fly",
        desc: "",
        done: false,
        hidden: true,
      },
      "freaky-flowers": {
        sig: "freaky-flowers",
        desc: "View the Freaky Flowers collection.",
        done: false,
      },
      gargoyle: {
        sig: "gargoyle",
        desc: "A steadfast guardian.",
        done: false,
      },
      girlfriend: {
        sig: "girlfriend",
        desc: "Caring confidant.",
        done: false,
      },
      give: {
        sig: "give",
        desc: "Support aesthetic.computer.",
        done: true,
      },
      gostop: {
        sig: "gostop",
        desc: "Stop and go.",
        done: false,
      },
      handprint: {
        sig: "handprint",
        desc: "Track your hand.",
        done: false,
      },
      handtime: {
        sig: "handtime",
        desc: "Draw with a pinch.",
        done: false,
      },
      "hell_-world": {
        sig: "hell_-world",
        desc: "View the hell_ world collection.",
        done: false,
        //TODO: doesn't seem to work?
      },
      hha: {
        sig: "hha",
        desc: "Happy Hands Assembler",
        done: false,
        hidden: false,
      },
      horizon: {
        sig: "horizon",
        desc: "Walk on the horizon.",
        done: false,
      },
      husband: {
        sig: "husband",
        desc: "Absent-minded but well-meaning.",
        done: false,
      },
      hw: {
        sig: "hw",
        desc: "View the hell_ world paintings.",
        done: false,
      },
      icon: {
        sig: "icon",
        desc: "",
        done: false,
        hidden: true,
      },
      images: {
        sig: "images",
        desc: "",
        done: false,
        hidden: true,
      },
      imessage: {
        sig: "imessage",
        desc: "",
        done: false,
        hidden: true,
      },
      i: {
        sig: "i",
        desc: "",
        done: false,
        hidden: true,
      },
      kid: {
        sig: "kid",
        desc: "Maybe a unicorn.",
        done: false,
      },
      lang: {
        sig: "lang",
        desc: "",
        done: false,
        hidden: true,
      },
      learn: {
        sig: "learn",
        desc: "",
        done: false,
        hidden: true,
      },
      'laer-klokken': {
        sig: "laer-klokken",
        desc: "Learn the 'clock'!",
        done: false
      },
      "legacy-prompt": {
        sig: "legacy-prompt",
        desc: "",
        done: false,
        hidden: true,
      },
      liar: {
        sig: "liar",
        desc: "Incredibly honest and trustworthy.",
        done: false,
      },
      '3-kidlisp-tests': {
        sig: "3-kidlisp-tests",
        desc: "Tests of a new language.",
        done: false,
      },
      'fia-birthday': {
        sig: "fia-birthday",
        desc: "Come to Fía's birthday!",
        done: false,
      },
      "kaos-pad-template": {
        sig: "kaos-pad-template",
        desc: "A simple multi-touch XY pad template.",
        done: false,
      },
      line: {
        sig: "line[:thickness]",
        desc: "Draw lines with your finger.",
        colon: [
          { name: "thickness", type: "number", required: false, default: 1, desc: "Line width in pixels" }
        ],
        examples: ["line", "line:2", "line:5"],
        done: true,
      },
      list: {
        sig: "list",
        desc: "View all commands.",
        done: false,
      },
      "lmn-flower": {
        sig: "lmn-flower",
        desc: "",
        done: false,
        hidden: true,
      },
      "lmn-petal": {
        sig: "lmn-petal",
        desc: "",
        done: false,
        hidden: true,
      },
      "login-pattern": {
        sig: "login-pattern",
        desc: "",
        done: false,
        hidden: true,
      },
      "login-wait": {
        sig: "login-wait",
        desc: "",
        done: false,
        hidden: true,
      },
      m2w2: {
        sig: "m2w2",
        desc: "Music 2 Whistlegraph 2.",
        done: false,
      },
      melody: {
        sig: "melody",
        desc: "Plays a sequence.",
        done: false,
      },
      metronome: {
        sig: "metronome",
        desc: "Keep time.",
        done: false,
      },
      microphone: {
        sig: "microphone",
        desc: "",
        done: false,
        hidden: true,
        //TODO: try to fix
      },
      mom: {
        sig: "mom",
        desc: "Why does she love this way?",
        done: false,
      },
      mood: {
        sig: "mood",
        desc: "Set your mood.",
        done: false,
      },
      moods: {
        sig: "moods",
        desc: "Read all the moods.",
        done: false,
      },
      multipen: {
        sig: "multipen",
        desc: "",
        done: false,
        hidden: true,
      },
      nail: {
        sig: "nail",
        desc: "",
        done: false,
        hidden: true,
      },
      noise: {
        sig: "noise",
        desc: "Some nice noise.",
        done: false,
      },
      news: {
        sig: "news",
        desc: "Community news and links.",
        done: true,
      },
      nopaint: {
        sig: "nopaint",
        desc: "",
        done: false,
        hidden: true,
        //TODO: shouldnt this go to nopaint site? nopaint.art
      },
      notepat: {
        sig: "notepat[:wave][:octave] [melody...]",
        desc: "A melodic keyboard instrument.",
        // Colon params: command:opt1:opt2 → colon[0], colon[1]
        colon: [
          { name: "wave", type: "enum", values: ["sine", "square", "triangle", "sawtooth", "noise"], required: false, default: "sine" },
          { name: "octave", type: "number", values: [1, 2, 3, 4, 5, 6, 7, 8, 9], required: false, default: 4 }
        ],
        // Space params: command arg1 arg2 → params[0], params[1]
        params: [
          { name: "melody", type: "string", required: false, desc: "Melody in note:word format (e.g. C:twin- C:-kle)" }
        ],
        examples: ["notepat", "notepat:square", "notepat:sine:5", "notepat twinkle"],
        done: true,
      },
      stample: {
        sig: "stample",
        desc: "A sampling instrument.",
        done: false,
      },
      old: {
        sig: "old",
        desc: "",
        done: false,
        hidden: true,
      },
      oldpull: {
        sig: "oldpull",
        desc: "",
        done: false,
        hidden: true,
      },
      oldwand: {
        sig: "oldwand",
        desc: "",
        done: false,
        hidden: true,
      },
      ordfish: {
        sig: "ordfish",
        desc: "View the Ordfish painting collection.",
        done: false,
      },
      ordsy: {
        sig: "ordsy",
        desc: "",
        done: false,
        hidden: true,
      },
      oval: {
        sig: "oval",
        desc: "Draw an oval.",
        done: false,
      },
      painting: {
        sig: "painting",
        desc: "View your current painting's steps.",
        done: false,
      },
      paint: {
        sig: "paint",
        desc: "Generate marks by adding instructions.",
        done: false,
      },
      paste: {
        sig: "paste",
        desc: "Paste an image from your library.",
        done: false,
      },
      perf: {
        sig: "perf",
        desc: "",
        done: false,
        hidden: true,
      },
      phand: {
        sig: "phand",
        desc: "",
        done: false,
        hidden: true,
      },
      pip: {
        sig: "pip",
        desc: "",
        done: false,
        hidden: true,
      },
      play: {
        sig: "play",
        desc: "",
        done: false,
        hidden: true,
      },
      pline: {
        sig: "pline",
        desc: "",
        done: false,
        hidden: true,
      },
      plot: {
        sig: "plot",
        desc: "Plot vector graphics.",
        done: false,
      },
      pond: {
        sig: "pond",
        desc: "Draw ripples with others.",
        done: false,
      },
      profile: {
        sig: "profile",
        desc: "Go to your profile or enter another user's.",
        done: false,
      },
      prompt: {
        sig: "prompt",
        desc: "Go to the prompt.",
        done: false,
      },
      prutti: {
        sig: "prutti",
        desc: "Genius old man rants.",
        done: false,
      },
      ptt: {
        sig: "ptt",
        desc: "",
        done: false,
        hidden: true,
      },
      pull: {
        sig: "pull",
        desc: "",
        done: false,
        hidden: true,
      },
      rain: {
        sig: "rain",
        desc: "A nice rain animation.",
        done: false,
      },
      rattle: {
        sig: "rattle",
        desc: "",
        done: false,
      },
      rect: {
        sig: "rect",
        desc: "Draw a rectangle.",
        done: false,
      },
      "run&gun": {
        sig: "run&gun",
        desc: "",
        done: false,
        hidden: true,
      },
      sage: {
        sig: "sage",
        desc: "Paths move across the screen.",
        done: false,
      },
      sb: {
        sig: "sb",
        desc: "",
        done: false,
        hidden: true,
      },
      "scawy-snake": {
        sig: "scawy-snake",
        desc: "The classic game of snake.",
        done: false,
        //TODO: need replay button
      },
      seashells: {
        sig: "seashells",
        desc: "A multi-touch bytebeat instrument.",
        done: false,
      },
      screenshots: {
        sig: "screenshots",
        desc: "",
        done: false,
      },
      screentest: {
        sig: "screentest",
        desc: "",
        done: false,
        hidden: true,
      },
      selfie: {
        sig: "selfie",
        desc: "Open the front camera.",
        done: false,
      },
      sfx: {
        sig: "sfx",
        desc: "",
        done: false,
        hidden: true,
      },
      shape: {
        sig: "shape",
        desc: "Draw a freehand polygon.",
        done: false,
      },
      shop: {
        sig: "shop",
        desc: "Order artwork and services from @jeffrey.",
        done: true,
      },
      share: {
        sig: "share",
        desc: "Generate a QR code to share.",
        done: false,
      },
      signature: {
        sig: "signature",
        desc: "",
        done: false,
        hidden: true,
      },
      sign: {
        sig: "sign",
        desc: "",
        done: false,
        hidden: true,
      },
      sing: {
        sig: "sing",
        desc: "",
        done: false,
        hidden: true,
      },
      sister: {
        sig: "sister",
        desc: "Don't steal her makeup.",
        done: false,
      },
      slip: {
        sig: "slip",
        desc: "A two octave slide instrument.",
        done: false,
      },
      smear: {
        sig: "smear",
        desc: "Move pixels around.",
        done: false,
      },
      sno: {
        sig: "sno",
        desc: "Walk around and fall asleep.",
        done: false,
      },
      song: {
        sig: "song",
        desc: "Learn a song.",
        done: false,
        //TODO: adjust song , default octave thing
      },
      "sotce-net": {
        sig: "sotce-net",
        desc: "diaries (work in progress)",
        done: false,
        hidden: true,
      },
      // "sparkle-brush": {
      //   sig: "sparkle-brush",
      //   desc: "Paint with Maya's really fun brush.",
      //   done: false,
      // },
      sparkle: {
        sig: "sparkle",
        desc: "Paint with @maya's really fun brush.",
        done: false,
      },
      spline: {
        sig: "spline",
        desc: "A springy wave.",
        done: false,
      },
      spray: {
        sig: "spray",
        desc: "",
        done: false,
        hidden: true,
      },
      sprinkles: {
        sig: "sprinkles",
        desc: "Watch the pretty sprinkles.",
        done: false,
      },
      sprite: {
        sig: "sprite",
        desc: "",
        done: false,
        hidden: true,
      },
      squaresong: {
        sig: "squaresong",
        desc: "Mmmm squaresong.",
        done: false,
      },
      stage: {
        sig: "stage",
        desc: "",
        done: false,
        hidden: true,
      },
      // "stage-setup.html": {
      //   sig: "stage-setup.html",
      //   desc: "",
      //   done: false,
      //   hidden: true,
      // },
      staka: {
        sig: "staka",
        desc: "",
        done: false,
        hidden: true,
      },
      starfield: {
        sig: "starfield",
        desc: "A celestial experience.",
        done: false,
      },
      test: {
        sig: "test",
        desc: "",
        done: false,
        hidden: true,
      },
      textfence: {
        sig: "textfence",
        desc: "A tiny play by @jeffrey and @georgica.",
        done: false,
      },
      tone: {
        sig: "tone[:wave] [frequency]",
        desc: "Listen to a tone.",
        colon: [
          { name: "wave", type: "enum", values: ["sine", "triangle", "square", "sawtooth", "cycle"], required: false, default: "sine" }
        ],
        params: [
          { name: "frequency", type: "number", required: false, desc: "Tone frequency in Hz (50-4000)" }
        ],
        examples: ["tone", "tone 440", "tone:square 880", "tone:cycle"],
        done: true,
      },
      toss: {
        sig: "toss[:wave][:tempo]",
        desc: "Play microtonal oscillators.",
        colon: [
          { name: "wave", type: "enum", values: ["sine", "square", "triangle", "sawtooth"], required: false, default: "sine" },
          { name: "tempo", type: "number", values: [60, 80, 100, 120, 140, 160], required: false, default: 120 }
        ],
        done: true,
      },
      tracker: {
        sig: "tracker",
        desc: "A simple music tracker.",
        done: false,
      },
      udp: {
        sig: "udp",
        desc: "",
        done: false,
        hidden: true,
      },
      uke: {
        sig: "uke",
        desc: "Standard note meter.",
        done: false,
      },
      valbear: {
        sig: "valbear",
        desc: "Make a V-Day card.",
        done: false,
      },
      vary: {
        sig: "vary",
        desc: "",
        done: false,
        hidden: true,
      },
      video: {
        sig: "video",
        desc: "",
        done: false,
        hidden: true,
      },
      wand: {
        sig: "wand",
        desc: "Sculpt in XR.",
        done: false,
        hidden: false,
      },
      wallet: {
        sig: "wallet",
        desc: "View your Tezos wallet.",
        done: true,
      },
      wave: {
        sig: "wave",
        desc: "Wave using your hand.",
        done: false,
        hidden: false,
      },
      wg: {
        sig: "wg",
        desc: "Watch and learn whistlegraphs.",
        done: false,
      },
      wgr: {
        sig: "wgr",
        desc: "Whistlegraph recorder.",
        done: false,
        hidden: false,
      },
      whistlegraph: {
        sig: "whistlegraph",
        desc: "Whistlegraph index.",
        done: false,
        hidden: false,
      },
      whistle: {
        sig: "whistle",
        desc: "Convert whistles to sine waves.",
        done: false,
      },
      wife: {
        sig: "wife",
        desc: "Get ready for chores.",
        done: false,
      },
      wipe: {
        sig: "wipe",
        desc: "",
        done: false,
        hidden: false,
        //TODO: doesn't seem to clear the painting
      },
      word: {
        sig: "word",
        desc: "Add text to your painting.",
        done: false,
      },
      zoom: {
        sig: "zoom",
        desc: "",
        done: false,
        hidden: true,
      },
    },
  };

  const page = html` <html>
    <head>
      <link
        rel="icon"
        href="https://${event.headers["host"]}/icon/128x128/prompt.png"
        type="image/png"
      />
      <meta charset="utf-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <title>$name · Aesthetic Computer</title>
      <style>
        ::-webkit-scrollbar {
          display: none;
        }
        html {
          cursor:
            url("/aesthetic.computer/cursors/precise.svg") 12 12,
            auto;
        }
        body {
          margin: 0;
          font-size: 22px;
          font-family: monospace;
          -webkit-text-size-adjust: none;
        }
        body.doc {
          overflow-x: hidden;
        }
        h1 a, h1 a:visited {
          color: inherit;
          cursor: inherit;
          text-decoration: none;
        }
        .links a[data-done=false] {
          /* text-decoration: line-through; */
        }
        h1 {
          font-weight: normal;
          font-size: 22px;
          margin: 0;
          position: fixed;
        }
        h1:before {
          content: "";
          height: 2.5em;
          width: 100vw;
          position: absolute;
          z-index: -1;
          top: -16px;
          left: -16px;
        }
        h1[data-done=false]:after {
          content: "wip";
          color: yellow;
          background: maroon;
          position: absolute;
          right: -32px;
          top: 0px;
          display: block;
          font-size: 50%;
          padding: 0px 3px 2px 3px;
          border-radius: 2px;
        }
        h2 {
          font-weight: normal;
          font-size: 18px;
        }
        iframe {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          border: none;
          z-index: -1;
          opacity: 0.5;
        }
        section {
          margin: 12px 16px 12px 16px;
        }
        #pals {
          position: fixed;
          bottom: 5px;
          right: 16px;
          user-select: none;
        }
        p {
          margin-top: 0;
        }
        #command-list, #docs-welcome {
          margin-top: 1.5em;
        }
        #docs-welcome {
          padding: 0;
        }
        #title {
          display: inline-block;
        }
        #title.block:after {
          content: "_";
          position: absolute;
          top: 6px;
          right: -16px;
          line-height: 20px;
          color: rgb(205, 92, 155);
          background-color: rgb(205, 92, 155);
        }
        a.prompt,
        a.prompt:visited {
          color: white;
          text-decoration: none;
          cursor: inherit;
        }
        a.prompt:hover {
          color: rgb(205, 92, 155);
        }
        small {
          opacity: 0.25;
          font-size: 100%;
        }
        .code-doc {
          padding-top: 2.5em;
          font-size: 65%;
          padding-left: 1px;
        }
        .doc-body {
          margin-top: 1em;
        }
        .doc-body table {
          border-collapse: collapse;
          width: 100%;
          max-width: 960px;
          margin-top: 0.75em;
          margin-bottom: 0.75em;
        }
        .doc-body th,
        .doc-body td {
          text-align: left;
          border: 1px solid rgba(255, 255, 255, 0.2);
          padding: 0.4em 0.5em;
          vertical-align: top;
        }
        .doc-body ul {
          padding-left: 1.2em;
          margin-top: 0.6em;
          margin-bottom: 0.6em;
        }
        .doc-body h3 {
          margin: 0.8em 0 0.4em;
          font-size: 1em;
          font-weight: normal;
        }
        .status-badge {
          display: inline-block;
          padding: 0.1em 0.45em;
          border-radius: 0.35em;
          font-size: 0.9em;
          text-transform: lowercase;
          border: 1px solid currentColor;
          white-space: nowrap;
        }
        .status-done {
          color: #4ade80;
        }
        .status-in-progress {
          color: #fbbf24;
        }
        .status-planned {
          color: #94a3b8;
        }
        .doc-examples {
          display: grid;
          gap: 0.75em;
        }
        .doc-example {
          border: 1px solid rgba(255, 255, 255, 0.2);
          padding: 0.45em 0.6em;
          border-radius: 0.35em;
        }
        .code-doc-welcome {
          padding-top: 2.75em;
          font-size: 65%;
          padding-bottom: 3em;
        }
        .lane-grid {
          display: grid;
          gap: 14px;
          grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
        }
        .lane-card {
          border: 1px solid rgba(255, 255, 255, 0.22);
          border-radius: 8px;
          padding: 10px 11px 11px 11px;
        }
        .lane-card.lane-mjs {
          border-color: rgba(124, 212, 255, 0.45);
          background: linear-gradient(180deg, rgba(26, 60, 86, 0.2), rgba(26, 60, 86, 0.05));
        }
        .lane-card.lane-l5 {
          border-color: rgba(245, 213, 66, 0.5);
          background: linear-gradient(180deg, rgba(90, 75, 0, 0.22), rgba(90, 75, 0, 0.08));
        }
        .lane-card.lane-kidlisp {
          border-color: rgba(102, 230, 187, 0.45);
          background: linear-gradient(180deg, rgba(16, 74, 56, 0.2), rgba(16, 74, 56, 0.06));
        }
        .lane-head {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 8px;
          margin-bottom: 0.45em;
        }
        .lane-title {
          font-size: 1.1em;
        }
        .lane-mjs .lane-title {
          color: #9bddff;
        }
        .lane-l5 .lane-title {
          color: #ffe37a;
        }
        .lane-kidlisp .lane-title {
          color: #9ff2d4;
        }
        .lane-count {
          font-size: 0.92em;
          opacity: 0.75;
          white-space: nowrap;
        }
        .lane-subtitle {
          font-size: 0.95em;
          opacity: 0.9;
          margin-bottom: 0.65em;
        }
        .lane-section {
          margin-top: 0.75em;
        }
        .lane-section h3 {
          margin: 0 0 0.25em 0;
          font-size: 0.95em;
          font-weight: normal;
          opacity: 0.85;
        }
        .doc-meta {
          margin-top: 0.3em;
          font-size: 0.92em;
          opacity: 0.8;
          display: flex;
          flex-wrap: wrap;
          gap: 8px;
        }
        .doc-status {
          text-transform: lowercase;
        }
        .doc-grid {
          display: grid;
          gap: 11px;
        }
        .doc-block {
          border: 1px solid rgba(255, 255, 255, 0.2);
          border-radius: 8px;
          padding: 0.55em 0.7em;
        }
        .doc-block h2 {
          margin: 0 0 0.4em;
          font-size: 1em;
          text-transform: uppercase;
          letter-spacing: 0.04em;
          opacity: 0.85;
        }
        .doc-preview {
          border: 1px solid rgba(255, 255, 255, 0.2);
          border-radius: 8px;
          overflow: hidden;
        }
        .doc-preview iframe {
          position: static !important;
          z-index: auto !important;
          opacity: 1 !important;
          width: 100% !important;
          height: 100% !important;
          min-height: 280px;
          border: 0;
          display: block;
        }
        .doc-preview-head {
          padding: 0.45em 0.65em;
          font-size: 0.9em;
          border-bottom: 1px solid rgba(255, 255, 255, 0.2);
        }
        .doc-preview-body {
          min-height: 280px;
        }
        .doc-preview-links {
          display: flex;
          flex-wrap: wrap;
          gap: 7px;
          margin-top: 0.65em;
        }
        .doc-preview-links a {
          text-decoration: none;
          padding: 3px 6px;
          border: 1px solid rgba(255, 255, 255, 0.26);
          border-radius: 6px;
        }
        .doc-preview-links button {
          text-decoration: none;
          padding: 3px 6px;
          border: 1px solid rgba(255, 255, 255, 0.26);
          border-radius: 6px;
          background: transparent;
          color: inherit;
          font: inherit;
          cursor: pointer;
        }
        pre {
          margin-top: 1em;
          margin-bottom: 1em;
        }
        pre code.hljs {
          padding: 0.2em 0em;
          position: relative;
          overflow-x: visible;
        }
        pre code.hljs:after {
          content: "";
          height: 100%;
          top: 0;
          right: -16px;
          width: 16;
          background-color: #f3f3f3;
          position: absolute;
        }
        pre code.hljs:before {
          content: "";
          height: 100%;
          top: 0;
          left: -16px;
          width: 16px;
          background-color: #f3f3f3;
          position: absolute;
        }
        .links a {
          text-decoration: none;
          /* border: 1px solid; */
          padding: 4px;
        }
        @media (prefers-color-scheme: dark) {
          body {
            background-color: rgb(64, 56, 74);
            color: rgba(255, 255, 255, 0.85);
          }
          h1 a:hover {
            color: rgb(205, 92, 155);
          }
          h1:before {
            background-image: linear-gradient(to bottom, rgba(64, 56, 74, 0.75) 80%, transparent);
          }
          .hljs-title.function_ {
            color: rgb(225, 105, 175);
          }
          .hljs {
            color: white;
          }
          pre code.hljs,
          pre code.hljs:after,
          pre code.hljs:before {
            background: rgb(25, 0, 25);
          }
          .links a {
            color: rgb(205, 92, 155);
          }
          .links a.top-level {
            color: rgb(92, 205, 155);
          }
        }
        @media (prefers-color-scheme: light) {
          body {
            background-color: rgba(244, 235, 250);
          }
          .doc-body th,
          .doc-body td,
          .doc-example,
          .lane-card,
          .doc-block,
          .doc-preview,
          .doc-preview-head,
          .doc-preview-links a,
          .doc-preview-links button {
            border-color: rgba(0, 0, 0, 0.2);
          }
          .lane-card.lane-mjs {
            background: linear-gradient(180deg, rgba(206, 237, 255, 0.8), rgba(238, 248, 255, 0.7));
            border-color: rgba(49, 133, 173, 0.55);
          }
          .lane-card.lane-l5 {
            background: linear-gradient(180deg, rgba(255, 247, 185, 0.85), rgba(255, 252, 220, 0.7));
            border-color: rgba(179, 149, 37, 0.55);
          }
          .lane-card.lane-kidlisp {
            background: linear-gradient(180deg, rgba(204, 245, 230, 0.8), rgba(233, 252, 245, 0.7));
            border-color: rgba(41, 141, 102, 0.5);
          }
          .lane-mjs .lane-title {
            color: rgb(20, 90, 128);
          }
          .lane-l5 .lane-title {
            color: rgb(130, 102, 0);
          }
          .lane-kidlisp .lane-title {
            color: rgb(24, 112, 76);
          }
          a.prompt, a.prompt:visited {
            color: rgb(64, 56, 74);
          }
          h1 a:hover, a.prompt:hover {
            color: rgb(205, 92, 155);
          }
          h1:before {
            background-image: linear-gradient(to bottom, rgba(244, 235, 250, 0.75) 80%, transparent);
          }
          .hljs-title.function_ {
            color: rgb(205, 92, 155);
          }
          .links a {
            color: rgb(180, 72, 135);
          }
          .links a.top-level {
            color: green;
          }
        }
        .nolink {
          user-select: none;
          pointer-events: none;
        }
      </style>
      <link
        rel="stylesheet"
        href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.1/styles/default.min.css"
      />
      <script nonce="$nonce" src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.1/highlight.min.js"></script>
      <script nonce="$nonce">
        hljs.highlightAll();
      </script>
    </head>
    <body$bodyclass>
      <section>$content</section>
      <img
        id="pals"
        width="64"
        src="https://${event.headers["host"]}/purple-pals.svg"
      />
      <script nonce="$nonce">
        const titleLink = document.querySelector("#title a");
        if (window.self !== window.top && titleLink.innerText === "docs") {
          title.classList.add("nolink");
        }
        const docPreviewFrame = document.getElementById("doc-preview-frame");
        const docPreviewBase = docPreviewFrame?.dataset?.src || docPreviewFrame?.src || "";
        function withFreshTimestamp(url) {
          if (!url) return "";
          try {
            const next = new URL(url, window.location.origin);
            next.searchParams.set("t", Date.now());
            return next.toString();
          } catch (_err) {
            const joiner = url.includes("?") ? "&" : "?";
            return url + joiner + "t=" + Date.now();
          }
        }
        window.runDocPreview = function runDocPreview() {
          if (!docPreviewFrame) return;
          docPreviewFrame.src = withFreshTimestamp(docPreviewFrame.src || docPreviewBase);
        };
        window.resetDocPreview = function resetDocPreview() {
          if (!docPreviewFrame || !docPreviewBase) return;
          docPreviewFrame.src = withFreshTimestamp(docPreviewBase);
        };
        // 🌠 Live editing (while developing aesthetic locally)
        if (${dev}) {
          var socket = new WebSocket("ws://localhost:8889");

          socket.onopen = function (event) {
            console.log("🟢 Live editing enabled.");
          };

          socket.onmessage = function (event) {
            const msg = JSON.parse(event.data);
            console.log("🟡 Message from server:", msg);
            if (msg.type === "reload") document.location.reload();
          };

          socket.onerror = function (event) {
            console.error("🔴 Error observed:", event);
          };

          socket.onclose = function (event) {
            console.log("🔴 Live editing disabled.");
          };
        }
      </script>
    </body>
  </html>`.trim();

  const content = `
    <h1 data-done="$done" id="title"><a href="/docs">$name</a></h1>
    <div class="code-doc">
      <pre><code class="language-$lang">$sig</code></pre>
      <p>$desc</p>
      <div class="doc-body">$body</div>
    </div>
  `.trim();

  function escapeHTML(value) {
    return String(value || "")
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll('"', "&quot;")
      .replaceAll("'", "&#39;");
  }

  function normalizeStatus(done) {
    if (done === true || done === "done") return "done";
    if (done === "in-progress") return "in-progress";
    return "planned";
  }

  function statusBadge(done) {
    const status = normalizeStatus(done);
    return `<span class="status-badge status-${status}">${status.replace("-", " ")}</span>`;
  }

  function familyFromCategory(category) {
    if (category === "l5") return "L5 / Lua API";
    if (category === "kidlisp") return "KidLisp / Language API";
    if (category === "prompts") return "Prompt Commands";
    if (category === "pieces") return "Pieces";
    if (category === "mjs") return "MJS / AC Piece API";
    return "MJS / AC Piece API";
  }

  function parseSigName(sig, fallback = "") {
    const source = String(sig || "").trim();
    if (!source) return fallback;
    const match = source.match(/^([A-Za-z0-9_.$]+)\s*\(/);
    if (match?.[1]) return match[1];
    return fallback || source;
  }

  function previewUrlForDoc(doc, category, word) {
    const flags = `nogap=true&nolabel=true&noauth=true&popout=true&t=${Date.now()}`;
    if (doc?.example?.url) return doc.example.url;
    if (doc?.example?.type === "piece" && doc.example.entry) {
      return `${AC_ORIGIN}/${encodeURIComponent(doc.example.entry)}?${flags}`;
    }
    if (category === "l5") {
      return `${AC_ORIGIN}/l5-hello.lua?${flags}`;
    }
    if (category === "kidlisp") {
      return `${AC_ORIGIN}/kidlisp?${flags}`;
    }
    if (Array.isArray(doc?.examples) && doc.examples.length) {
      const first = String(doc.examples[0]).replace(/^prompt~/, "");
      return `${AC_ORIGIN}/prompt~${encodeURIComponent(first)}?${flags}`;
    }
    const guess = parseSigName(doc?.sig, word);
    return `${AC_ORIGIN}/prompt~${encodeURIComponent(guess)}?${flags}`;
  }

  function renderParamsSection(doc) {
    if (!Array.isArray(doc?.params) || doc.params.length === 0) {
      return `<p>Parameters are not fully documented yet.</p>`;
    }
    return `
      <table>
        <thead>
          <tr>
            <th>Name</th>
            <th>Type</th>
            <th>Required</th>
            <th>Description</th>
          </tr>
        </thead>
        <tbody>
          ${doc.params
            .map((param) => {
              const required = param.required ? "yes" : "no";
              const type = Array.isArray(param.values)
                ? `${param.type || "enum"}: ${param.values.join(", ")}`
                : (param.type || "");
              return `
                <tr>
                  <td><code>${escapeHTML(param.name || "")}</code></td>
                  <td>${escapeHTML(type)}</td>
                  <td>${required}</td>
                  <td>${escapeHTML(param.desc || "")}</td>
                </tr>
              `;
            })
            .join("")}
        </tbody>
      </table>
    `;
  }

  function renderExamplesSection(doc, category, word) {
    const examples = Array.isArray(doc?.examples) ? doc.examples : [];
    if (!examples.length) {
      return `
        <p>No explicit examples yet.</p>
        <p><a href="${AC_ORIGIN}/prompt">Open prompt to experiment live</a>.</p>
      `;
    }

    return `
      <ul>
        ${examples
          .map((example) => {
            const clean = String(example).replace(/^prompt~/, "");
            return `<li><a href="${AC_ORIGIN}/prompt~${encodeURIComponent(clean)}">${escapeHTML(clean)}</a></li>`;
          })
          .join("")}
      </ul>
      <p><a href="${AC_ORIGIN}/prompt">Open prompt</a> · <a href="${AC_ORIGIN}/docs/${category}:${word}">Permalink</a></p>
    `;
  }

  function renderDocContent(category, word, doc) {
    const lang = doc?.lang || (category === "l5" ? "lua" : category === "kidlisp" ? "lisp" : "javascript");
    const previewSrc = previewUrlForDoc(doc, category, word);
    const family = familyFromCategory(category);
    const status = normalizeStatus(doc?.done);
    const returns = doc?.returns || "Not specified.";
    const notes = doc?.notes || "";
    const desc = doc?.desc || "Description pending.";
    const sig = doc?.sig || `${word}(...)`;

    return `
      <h1 data-done="${escapeHTML(doc?.done)}" id="title"><a href="/docs">${escapeHTML(word)}</a></h1>
      <div class="code-doc">
        <div class="doc-meta">
          <span><strong>${escapeHTML(family)}</strong></span>
          <span>/</span>
          <span><code>${escapeHTML(category)}</code></span>
          <span>/</span>
          <span class="doc-status">${statusBadge(status)}</span>
        </div>
        <pre><code class="language-${escapeHTML(lang)}">${escapeHTML(sig)}</code></pre>
        <p>${escapeHTML(desc)}</p>
        <div class="doc-grid">
          <div class="doc-block doc-body">
            <h2>Parameters</h2>
            ${renderParamsSection(doc)}
          </div>
          <div class="doc-block doc-body">
            <h2>Returns</h2>
            <p><code>${escapeHTML(returns)}</code></p>
          </div>
          <div class="doc-block doc-body">
            <h2>Examples</h2>
            ${renderExamplesSection(doc, category, word)}
          </div>
          <div class="doc-block doc-body">
            <h2>Runtime Notes</h2>
            ${notes ? `<p>${escapeHTML(notes)}</p>` : `<p>No additional runtime notes yet.</p>`}
          </div>
          <div class="doc-block doc-body">
            <h2>Details</h2>
            ${doc?.body || `<p>No additional details yet.</p>`}
          </div>
          <div class="doc-block">
            <h2>Live Preview</h2>
            <div class="doc-preview">
              <div class="doc-preview-head">Embedded AC preview</div>
              <div class="doc-preview-body">
                <iframe
                  id="doc-preview-frame"
                  data-src="${previewSrc}"
                  src="${previewSrc}"
                  title="${escapeHTML(word)} preview"
                  loading="lazy"
                  allow="autoplay; clipboard-write"
                ></iframe>
              </div>
            </div>
            <div class="doc-preview-links">
              <button type="button" onclick="runDocPreview()">Run</button>
              <button type="button" onclick="resetDocPreview()">Reset</button>
              <a href="${previewSrc}" target="_blank" rel="noopener">Open preview</a>
              <a href="${AC_ORIGIN}/prompt" target="_blank" rel="noopener">Open prompt</a>
              <a href="${AC_ORIGIN}/docs/${category}:${word}" target="_blank" rel="noopener">Open doc</a>
            </div>
          </div>
        </div>
      </div>
    `;
  }

  const commands = { ...docs.prompts, ...docs.pieces };
  let commandList = "";
  // ➿ Loop through all commands and generate HTML.
  keys(commands)
    .sort()
    .forEach((c) => {
      commandList += `
        <a class="prompt" href="https://${event.headers["host"]}/prompt~${c}" onclick="send('docs:${c}')">${c}</a>
        <small>${commands[c].desc}</small><br>
      `;
    });

  // Generate doc links for a given category.
  function genLinks(category) {
    return keys(docs.api[category] || [])
      .map(
        (k) =>
          `<a href="/docs/${category}:${k}" data-done="${docs.api[category][k].done}">${k}</a>`,
      )
      .join(" ");
  }

  function laneCounts(categories) {
    let total = 0;
    let done = 0;
    let inProgress = 0;
    categories.forEach((category) => {
      keys(docs.api[category] || {}).forEach((word) => {
        total++;
        const status = normalizeStatus(docs.api[category][word]?.done);
        if (status === "done") done++;
        if (status === "in-progress") inProgress++;
      });
    });
    const planned = total - done - inProgress;
    return { total, done, inProgress, planned };
  }

  function laneCountLabel(categories) {
    const count = laneCounts(categories);
    return `${count.done} done · ${count.inProgress} in progress · ${count.planned} planned`;
  }

  const indexContent = html`
    <h1 id="title">
      <a
        href="https://${event.headers["host"]}"
        onclick="if (window.opener) window.close();"
        >docs</a
      >
    </h1>
    <div class="code-doc-welcome">
      <div class="lane-grid">
        <article class="lane-card lane-mjs">
          <div class="lane-head">
            <div class="lane-title">MJS / AC Piece API</div>
            <div class="lane-count">${laneCountLabel(["mjs", "structure", "graphics", "interaction", "sound", "number", "network", "help", "system"])}</div>
          </div>
          <div class="lane-subtitle">JavaScript runtime API for AC piece authors (<code>.mjs</code>).</div>
          <div class="links">
            <a data-done="${docs.api.mjs.overview.done}" class="top-level" href="/docs/mjs:overview">overview</a>
            <a data-done="${docs.api.structure.boot.done}" class="top-level" href="/docs/structure:boot">boot</a>
            <a data-done="${docs.api.structure.paint.done}" class="top-level" href="/docs/structure:paint">paint</a>
            <a data-done="${docs.api.structure.act.done}" class="top-level" href="/docs/structure:act">act</a>
            <a data-done="${docs.api.structure.sim.done}" class="top-level" href="/docs/structure:sim">sim</a>
            <a data-done="${docs.api.structure.beat.done}" class="top-level" href="/docs/structure:beat">beat</a>
          </div>
          <div class="lane-section">
            <h3>Graphics</h3>
            <span class="links">${genLinks("graphics")}</span>
          </div>
          <div class="lane-section">
            <h3>Interaction</h3>
            <span class="links">${genLinks("interaction")}</span>
          </div>
          <div class="lane-section">
            <h3>Sound + Number + Network</h3>
            <span class="links">${genLinks("sound")} ${genLinks("number")} ${genLinks("network")}</span>
          </div>
          <div class="lane-section">
            <h3>Help + System</h3>
            <span class="links">${genLinks("help")} ${genLinks("system")}</span>
          </div>
        </article>

        <article class="lane-card lane-l5">
          <div class="lane-head">
            <div class="lane-title">L5 / Lua API</div>
            <div class="lane-count">${laneCountLabel(["l5"])}</div>
          </div>
          <div class="lane-subtitle">Processing-style Lua compatibility layer on AC.</div>
          <div class="links">
            <a data-done="${docs.api.l5.overview.done}" class="top-level" href="/docs/l5:overview">overview</a>
            ${genLinks("l5")}
          </div>
          <div class="lane-section">
            <h3>Runtime Surfaces</h3>
            <span class="links">
              <a href="${AC_ORIGIN}/l5">/l5 playground</a>
              <a href="${AC_ORIGIN}/prompt">prompt</a>
            </span>
          </div>
        </article>

        <article class="lane-card lane-kidlisp">
          <div class="lane-head">
            <div class="lane-title">KidLisp / Language API</div>
            <div class="lane-count">${laneCountLabel(["kidlisp"])}</div>
          </div>
          <div class="lane-subtitle">Canonical language reference is maintained on learn.kidlisp.com.</div>
          <div class="links">
            <a data-done="${docs.api.kidlisp.overview.done}" class="top-level" href="/docs/kidlisp:overview">overview</a>
            ${genLinks("kidlisp")}
          </div>
          <div class="lane-section">
            <h3>Canonical Source</h3>
            <span class="links">
              <a href="${LEARN_KIDLISP_ORIGIN}/?tab=reference" target="_blank" rel="noopener">reference</a>
              <a href="${LEARN_KIDLISP_ORIGIN}/?tab=functions" target="_blank" rel="noopener">function popularity</a>
              <a href="${LEARN_KIDLISP_ORIGIN}/?id=wipe" target="_blank" rel="noopener">identifier pages</a>
            </span>
          </div>
        </article>
      </div>
    </div>
  `.trim();

  const commandsContent = html`
    <h1 id="title">
      <a
        href="https://${event.headers["host"]}"
        onclick="if (window.opener) window.close();"
        >$name</a
      >
    </h1>
    <p id="command-list" style="white-space: nowrap; display: inline-block;">
      ${commandList}
    </p>
    <script>
      function send(message) {
        if (window.opener) {
          window.opener.postMessage(message, "*");
          window.close();
        }
      }
      const title = document.querySelector("#title");
      const originalHTML = title.innerHTML;
      document.querySelectorAll(".prompt").forEach((prompt) => {
        prompt.addEventListener("pointerenter", (e) => {
          title.innerHTML = prompt.innerHTML;
          title.classList.add("block");
          prompt.addEventListener(
            "pointerleave",
            (e) => {
              title.innerHTML = originalHTML;
              title.classList.remove("block");
            },
            { once: true },
          );
        });
      });
    </script>
  `.trim();

  docs.template = page
    .replace("$bodyclass", " class='doc'")
    .replace("$content", content);

  const splitPath = event.path.split("/");

  if (
    splitPath.length === 4 ||
    (splitPath.length === 3 && splitPath[1] !== "api")
  ) {
    let mergedDocs;
    keys(docs.api).forEach((key) => {
      mergedDocs = {
        ...mergedDocs,
        ...docs.api[key],
      };
    });

    mergedDocs = { ...mergedDocs, ...docs.pieces, ...docs.prompts };

    // TODO:
    // Use a "colon" here for parsing and building pages from. api, pieces,
    // and "prompts".

    // 🔦 What should have overlap and what should not?

    // /docs/structure:paint
    // /docs/pieces:line
    // /docs/graphics:line

    const [category, word] = splitPath.pop().split(":");

    let doc;
    if (category !== "pieces" && category !== "prompts") {
      doc = docs.api[category]?.[word];
    } else if (category) {
      doc = docs[category]?.[word];
    } else {
      return respond(404, "Not found. :(", {
        "Content-Type": "text/html; charset=UTF-8",
      });
    }

    if (!doc) {
      return respond(404, "Not found. :(", {
        "Content-Type": "text/html; charset=UTF-8",
      });
    }

    return respond(
      200,
      page
        .replace("$bodyclass", " class='doc'")
        .replace("$content", renderDocContent(category, word, doc))
        .replaceAll("$name", word),
      {
        "Content-Type": "text/html; charset=UTF-8",
        "Cross-Origin-Embedder-Policy": "require-corp",
        "Cross-Origin-Opener-Policy": "same-origin-allow-popups",
        "Cross-Origin-Resource-Policy": "cross-origin",
      },
    );
  }

  // Return json or html.
  if (splitPath.pop().endsWith(".json")) {
    return respond(200, docs);
  } else {
    return respond(
      200,
      page
        .replace("$bodyclass", "")
        .replaceAll("$content", indexContent)
        .replaceAll("$name", "docs"),
      {
        "Content-Type": "text/html; charset=UTF-8",
        "Cross-Origin-Embedder-Policy": "require-corp",
        "Cross-Origin-Opener-Policy": "same-origin-allow-popups",
        "Cross-Origin-Resource-Policy": "cross-origin",
      },
    );
  }
}
