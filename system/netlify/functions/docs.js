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

  const docs = {
    // Commands for programming inside of pieces.
    api: {
      // 🏛️ Generic
      structure: {
        // 🧩 Top Level Piece Functions / meta-level imports.
        boot: {
          label: "🥾 Boot",
          sig: "boot({ ... })",
          desc: "Runs once when a piece starts.",
          done: false,
        },
        paint: {
          label: "🎨 Paint",
          sig: "paint({ ... })",
          desc: "Repeatedly draw to the screen at the hardware refresh rate.",
          done: false,
        },
        act: {
          label: "🎪 Act",
          sig: "act({ ... })",
          desc: "Respond to user and system input.",
          done: false,
        },
        sim: {
          label: "🧮 Sim",
          sig: "sim({ ... })",
          desc: "For calculations occurring once per logic frame. (120fps)",
          done: false,
        },
        beat: {
          label: "🥁 Beat",
          sig: "beat({ ... })",
          desc: "Runs once per system metronome tick, for rhythmic coordination.",
          done: false,
        },
        leave: {
          label: "👋 Leave",
          sig: "leave({ ... })",
          desc: "Execute code right before the piece is unloaded.",
          done: false,
        },
        meta: {
          label: " 📰 Meta",
          sig: "meta({ ... })",
          desc: "Runs once when a piece starts.",
          done: false,
        },
        preview: {
          label: "🖼️ Preview",
          sig: "preview({ ... })",
          desc: "Paint a custom thumbnail image.",
          done: false,
        },
        icon: {
          label: "🪷 Icon",
          sig: "icon({ ... })",
          desc: "Paint a piece icon, AKA `favicon`",
          done: false,
        },
        brush: {
          label: "🖌️ Brush",
          sig: "brush({ ... })",
          desc: "For implementing brushes in the `nopaint` system.",
          done: false,
        },
        filter: {
          label: "🥤 Filter",
          sig: "filter({ ... })",
          desc: "For implementing filters in the `nopaint` system.",
          done: false,
        },
        curtain: {
          label: "curtain",
          sig: "curtain({ ... })",
          desc: "A top layer paint for pieces in the `world` system",
          done: false,
        },
        background: {
          label: "🏔️ background",
          sig: "background({ ... })",
          desc: "A backdrop for pieces in the `world` system",
          done: false,
        },
        api: {
          sig: "api",
          desc: "References all built-in functionality for a top-level function.",
          done: false,
        },
        DEBUG: {
          sig: "DEBUG",
          desc: "A global constant that determines if `AC` is in debug mode.",
          done: false,
        },
      },
      // 🖱️ Interaction
      interaction: {
        pen: {
          sig: "pen: { x, y, ... }",
          desc: "Contains active mouse + touch pointer data.",
          done: false,
        },
        pens: {
          sig: "pens(n)",
          desc: "Get all active pen pointers or a specific one, for multi-touch.",
          done: false,
        },
        pen3d: {
          sig: "pen3d",
          desc: "A reference to the active XR pen pointer.",
          done: false,
        },
        event: {
          sig: "event",
          desc: "A reference to the currently running `event`.",
          done: false,
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
          done: true,
        },
        point: {
          sig: "point(...args) or point({x, y})",
          desc: "Plot a single pixel within the panned coordinate space. Takes x,y coordinates or a point object.",
          done: true,
        },
        box: {
          sig: "box(x, y, w, h, mode)",
          desc: "Draw a box with optional modes: 'fill' (default), 'outline', 'inline'. Add '*center' to draw from center. Use ':N' for thickness.",
          body: boxBody,
          done: true,
        },
        wipe: {
          sig: "wipe(color)",
          desc: "Clear the screen with a solid color. Color can be a single number (0-255 for grayscale) or an array [r,g,b,a].",
          done: true,
        },
        ink: {
          sig: "ink(color)",
          desc: "Set the current drawing color. Color can be a single number (0-255 for grayscale) or an array [r,g,b,a].",
          done: true,
        },

        circle: {
          sig: "circle(x, y, radius)",
          desc: "Draw a filled circle centered at (x,y) with the specified radius using the current ink color.",
          done: true,
        },
        layer: {
          sig: "",
          desc: "",
          done: false,
        },
        painting: {
          sig: "",
          desc: "",
          done: false,
        },
        inkrn: {
          sig: "",
          desc: "",
          done: false,
        },
        pagern: {
          sig: "",
          desc: "",
          done: false,
        },
        notice: {
          sig: "notice(msg, color, opts)",
          desc: "",
          done: false,
        },
        blend: {
          sig: "",
          desc: "",
          done: false,
        },
        page: {
          sig: "",
          desc: "",
          done: false,
        },
        edit: {
          sig: "",
          desc: "",
          done: false,
        },
        copy: {
          sig: "",
          desc: "",
          done: false,
        },
        paste: {
          sig: "paste(painting, x, y)",
          desc: "Paste a painting at the given position, anchored from the top left.",
          done: false,
        },
        stamp: {
          sig: "",
          desc: "Similar to paste but always centered.",
          done: false,
        },
        pixel: {
          sig: "",
          desc: "",
          done: false,
        },
        plot: {
          sig: "",
          desc: "",
          done: false,
        },
        flood: {
          sig: "",
          desc: "",
          done: false,
        },
        lineAngle: {
          sig: "",
          desc: "",
          done: false,
        },
        pline: {
          sig: "",
          desc: "",
          done: false,
        },
        pppline: {
          sig: "",
          desc: "",
          done: false,
        },
        oval: {
          sig: "",
          desc: "",
          done: false,
        },
        poly: {
          sig: "",
          desc: "",
          done: false,
        },
        shape: {
          sig: "",
          desc: "",
          done: false,
        },
        grid: {
          sig: "",
          desc: "",
          done: false,
        },
        draw: {
          sig: "",
          desc: "",
          done: false,
        },
        printLine: {
          sig: "",
          desc: "",
          done: false,
        },
        form: {
          sig: "",
          desc: "",
          done: false,
        },
        pan: {
          sig: "",
          desc: "",
          done: false,
        },
        unpan: {
          sig: "",
          desc: "",
          done: false,
        },
        savepan: {
          sig: "",
          desc: "",
          done: false,
        },
        loadpan: {
          sig: "",
          desc: "",
          done: false,
        },
        skip: {
          sig: "",
          desc: "",
          done: false,
        },
        glaze: {
          sig: "glaze({ on: bool})",
          desc: "Enable a fullscreen shader `glaze` effect.",
          done: false,
        },
        paintCount: {
          sig: "paintCount",
          desc: "The number of `paint` frames that have passed.",
          done: false,
        },
        screen: {
          sig: "",
          desc: "",
          done: false,
        },
        display: {
          sig: "display",
          desc: "A reference to the current display information.",
          done: false,
        },
        fps: {
          sig: "",
          desc: "",
          done: false,
        },
        resolution: {
          sig: "resolution(width, height = width, gap = 8)",
          desc: "Adjust the display resolution.",
          done: false,
        },
        video: {
          sig: "",
          desc: "",
          done: false,
        },
        rec: {
          sig: "",
          desc: "",
          done: false,
        },
        needsPaint: {
          sig: "needsPaint()",
          desc: "",
          done: false,
        },
        noise16: {
          sig: "",
          desc: "",
          done: false,
        },
        noise16DIGITPAIN: {
          sig: "",
          desc: "",
          done: false,
        },
        noise16Aesthetic: {
          sig: "",
          desc: "",
          done: false,
        },
        noise16Sotce: {
          sig: "",
          desc: "",
          done: false,
        },
        noiseTinted: {
          sig: "",
          desc: "",
          done: false,
        },
        write: {
          sig: "write(text, pos, b, bounds, wordWrap)",
          desc: "Returns a clones pixel buffer (painting)",
          done: false,
        },
        "text.capitalize": {
          sig: "",
          desc: "",
          done: false,
        },
        "text.box": {
          sig: "",
          desc: "",
          done: false,
        },
        clonePixels: {
          sig: "clonePixels(buffer)",
          desc: "Returns a clones pixel buffer (painting)",
          done: false,
        },
        colorsMatch: {
          sig: "colorsMatch(color1, color2)",
          desc: "Checks if two colors `[r, g, b, a]` are the same.",
          done: false,
        },
        color: {
          sig: "color(?)",
          desc: "Return a color `[r, g, b, a]` from a variety of inputs.",
          done: false,
        },
        resize: {
          sig: "resize(bitmap, width, height)",
          desc: "Get a fresh resized bitmap with nearest neighbor scaling.",
          done: false,
        },
        Camera: {
          sig: "N/A",
          desc: "N/A",
          done: false,
        },
        Form: {
          sig: "N/A",
          desc: "N/A",
          done: false,
        },
        Dolly: {
          sig: "N/A",
          desc: "N/A",
          done: false,
        },
        TRI: {
          sig: "N/A",
          desc: "N/A",
          done: false,
        },
        QUAD: {
          sig: "N/A",
          desc: "N/A",
          done: false,
        },
        LINE: {
          sig: "N/A",
          desc: "N/A",
          done: false,
        },
        CUBEL: {
          sig: "N/A",
          desc: "N/A",
          done: false,
        },
        ORIGIN: {
          sig: "N/A",
          desc: "N/A",
          done: false,
        },
        "ui.Button": {
          sig: "new Button(box)",
          desc: "An interactive button model with a text label.",
          done: false,
        },
        "ui.TextButton": {
          sig: "new TextButton(text, pos)",
          desc: "An interactive button model with a text label.",
          done: false,
        },
        "ui.TextInput": {
          sig: "new TextInput($, text, processCommand, options = { palette, font, wrap })",
          desc: "An interactive text prompt object.",
          done: false,
        },
        "content.add": {
          sig: "add(content)",
          desc: "Make a request to add content to the DOM.",
          done: false,
        },
        "dom.html": {
          sig: "html(src)",
          desc: "Add `html` content to the DOM.",
          done: false,
        },
        "dom.css": {
          sig: "css(src)",
          desc: "Add `css` content to the DOM.",
          done: false,
        },
        "dom.javascript": {
          sig: "javascript(src)",
          desc: "Add `javascript` content to the DOM.",
          done: false,
        },
        "dom.clear": {
          sig: "clear()",
          desc: "Clear (remove) all DOM content.",
          done: false,
        },
        typeface: {
          sig: "typeface",
          desc: "A reference to the default system typeface.",
          done: false,
        },
        cursor: {
          sig: "cursor(code)",
          desc: "Set the system mouse cursor to a different graphic.",
          done: false,
        },
      },
      sound: {
        "sound.time": {
          sig: "",
          desc: "",
          done: false,
        },
        "sound.bpm": {
          sig: "",
          desc: "",
          done: false,
        },
        "sound.freq": {
          sig: "",
          desc: "",
          done: false,
        },
        "sound.microphone": {
          sig: "",
          desc: "",
          done: false,
        },
        "sound.speaker": {
          sig: "",
          desc: "",
          done: false,
        },
        "sound.play": {
          sig: "",
          desc: "",
          done: false,
        },
        "sound.synth": {
          sig: "",
          desc: "",
          done: false,
        },
        "sound.bubble": {
          sig: "",
          desc: "",
          done: false,
        },
        "sound.kill": {
          sig: "",
          desc: "",
          done: false,
        },
      },
      network: {
        "net.signup": {
          sig: "signup()",
          desc: "Redirect a user to the signup screen.",
          done: false,
        },
        "net.login": {
          sig: "login()",
          desc: "Redirect a user to the login screen.",
          done: false,
        },
        "net.logout": {
          sig: "logout()",
          desc: "Log a user out and redirect them to the `prompt`.",
          done: false,
        },
        "net.pieces": {
          sig: "pieces",
          desc: "The system path to all built-in piece code.",
          done: false,
        },
        "net.parse": {
          sig: "parse(slug)",
          desc: "Parse a textual piece slug.",
          done: false,
        },
        "net.userRequest": {
          sig: "userRequest(method, endpoint, body)",
          desc: "Make an authorized request for a logged in user.",
          done: false,
        },
        "net.udp": {
          sig: "udp(receive)",
          desc: "Loosely connect the UDP receiver.",
          done: false,
        },
        "net.lan": {
          sig: "lan",
          desc: "A reference to the local area network IP if it is available.",
          done: false,
        },
        "net.iframe": {
          sig: "iframe",
          desc: "Whether or not the system is running hosted within an `iframe`.",
          done: false,
        },
        back: {
          sig: "back()",
          desc: "Go back to the previous piece or prompt if there is no history.",
          done: false,
        },
        alias: {
          sig: "alias(name, colon, params)",
          desc: "Jump to a piece without changing the corner label or url, and ignoring the history stack.",
          done: false,
        },
        load: {
          sig: "async load(parsed, fromHistory, alias, devReload, loadedCallback)",
          desc: "Load a piece after parsing a slug, with various options.",
          done: false,
        },
        slug: {
          sig: "slug",
          desc: "The full piece address containing its name, colon parameters, and space separated parameters.",
          done: false,
        },
        piece: {
          sig: "piece",
          desc: "The name of the running piece.",
          done: false,
        },
        query: {
          sig: "query",
          desc: "An object containing the system's URL query parameters.",
          done: false,
        },
        params: {
          sig: "params",
          desc: "",
          done: false,
        },
        colon: {
          sig: "colon",
          desc: "An array of colon paramaters for a piece.",
          done: false,
        },
        preload: {
          sig: "async preload(path, parseJSON = true, progressReport, options)",
          desc: "Preload a media asset from the network.",
          done: false,
        },
        download: {
          sig: "download(filename, data, modifiers)",
          desc: "Download a file.",
          done: false,
        },
        dark: {
          sig: "dark",
          desc: "If the system is in dark mode.",
          done: false,
        },
        jump: {
          sig: "jump(to)",
          desc: "Gets a random integer.",
          done: false,
        },
        leaving: {
          sig: "leaving()",
          desc: "Returns true if a piece is leaving / a `jump` is in process.",
          done: false,
        },
        broadcast: {
          sig: "broadcast(msg)",
          desc: "Send a message to other open `aesthetic.computer` tabs.",
          done: false,
        },
        "net.socket": {
          sig: "socket(receive)",
          desc: "Hook into the piece's socket server with a receive callback.",
          done: false,
        },
        "net.devReload": {
          sig: "devReload",
          desc: "A flag that determines if the piece code was just reloaded in development.",
          done: false,
        },
        "net.web": {
          sig: "web(url, jumpOut)",
          desc: "Jump the browser to a new url.",
          done: false,
        },
        "net.host": {
          sig: "host",
          desc: "The current hetwork host.",
          done: false,
        },
        "net.rewrite": {
          sig: "rewrite(path, historical = false)",
          desc: "Rewrite a new URL / parameter path without affecting the history.",
          done: false,
        },
        "net.refresh": {
          sig: "refresh()",
          desc: "Refresh the page / restart `aesthetic.computer`.",
          done: false,
        },
        "net.waitForPreload": {
          sig: "waitForPreload()",
          desc: "Tell the system to wait until preloading is finished before painting.",
          done: false,
        },
        "net.preloaded": {
          sig: "preloaded()",
          desc: "Tell the system that all preloading is done.",
          done: false,
        },
      },
      number: {
        simCount: {
          sig: "simCount",
          desc: "The number of simulation frames passed.",
          done: false,
        },
        seconds: {
          sig: "seconds(s)",
          desc: "Convert seconds to `sim` frames.",
          done: false,
        },
        "num.add": {
          sig: "",
          desc: "",
          done: false,
        },
        "num.wrap": {
          sig: "",
          desc: "",
          done: false,
        },
        "num.even": {
          sig: "",
          desc: "",
          done: false,
        },
        "num.odd": {
          sig: "",
          desc: "",
          done: false,
        },
        "num.clamp": {
          sig: "",
          desc: "",
          done: false,
        },
        "num.rand": {
          sig: "",
          desc: "",
          done: false,
        },
        "num.randInt": {
          sig: "randInt(n)",
          desc: "Gets a random integer.",
          done: false,
        },
        "num.randInd": {
          sig: "randInd(arr)",
          desc: "Generates a random index from an array.",
          done: false,
        },
        "num.randIntArr": {
          sig: "randIntArr(n, count)",
          desc: "Generates an array of random integers from 0-n (inclusive)",
          done: false,
        },
        "num.randIntRange": {
          sig: "randIntRange(low, high)",
          desc: "Generates an integer from low-high (inclusive)",
          done: false,
        },
        "num.rangedInts": {
          sig: "rangedInts(ints)",
          desc: "Converts an array of strings formatted like 1-100 into an array of random integer ranges. Useful for color ranges.",
          done: false,
        },
        "num.multiply": {
          sig: "multiply(operands, n)",
          desc: "Multiplies one or more [] operands by n and returns a Number or Array.",
          done: false,
        },
        "num.dist": {
          sig: "dist(x1, y1, x2, y2)",
          desc: "Compute the distance between two 2D points.",
          done: false,
        },
        "num.dist3d": {
          sig: "dist3d(p1, p2)",
          desc: "Compute the distance between two 3D points as [x, y z]",
          done: false,
        },
        "num.perlin": {
          sig: "perlin(x, y)",
          desc: "Compute a 2D perlin noise value.",
          done: false,
        },
        "num.radians": {
          sig: "radians(deg)",
          desc: "Convert degrees to radians.",
          done: false,
        },
        "num.degrees": {
          sig: "degrees(rad)",
          desc: "Convert radians to degrees.",
          done: false,
        },
        "num.lerp": {
          sig: "lerp(a, b, amount)",
          desc: "Slides a number between a and b by a normalized amount.",
          done: false,
        },
        "num.map": {
          sig: "map(num, inMin, inMax, outMin, outMax)",
          desc: "Maps a number within a range to a new range.",
          done: false,
        },
        "num.arrMax": {
          sig: "arrMax(arr)",
          desc: "Return the maximum number in an array.",
          done: false,
        },
        "num.arrCompress": {
          sig: "arrCompress(arr, n)",
          desc: "Return a new array with every nth index missing.",
          done: false,
        },
        "num.Track": {
          sig: "new Track(values, result)",
          desc: "Lerp a value using a stepping function, with optional quantization.",
          done: false,
        },
        "num.p2.of": {
          sig: "of(x, y)",
          desc: "Turns two values into an {x, y} point.",
          done: false,
        },
        "num.p2.len": {
          sig: "len(pA)",
          desc: "Gets the length of the point as a vector.",
          done: false,
        },
        "num.p2.norm": {
          sig: "norm(p)",
          desc: "Normalizes a vector to have a length of 1.",
          done: false,
        },
        "num.p2.eq": {
          sig: "eq(p1, p2)",
          desc: "Checks for the equality of two points.",
          done: false,
        },
        "num.p2.inc": {
          sig: "inc(pout, pin)",
          desc: "Mutably adds P->in to P->out.",
          done: false,
        },
        "num.p2.scl": {
          sig: "scl(pout, pin)",
          desc: "Mutably scales P->out by P->in.",
          done: false,
        },
        "num.p2.add": {
          sig: "add(pA, pB)",
          desc: "Immutably adds pA + pB.",
          done: false,
        },
        "num.p2.sub": {
          sig: "sub(pA, pB)",
          desc: "Immutably subtracts pA - pB.",
          done: false,
        },
        "num.p2.rot": {
          sig: "rot(p, angle)",
          desc: "Immutably rotates p by angle in radians.",
          done: false,
        },
        "num.p2.mul": {
          sig: "mul(pA, pB)",
          desc: "Immutably multiplies pA * pB.",
          done: false,
        },
        "num.p2.div": {
          sig: "div(pA, pB)",
          desc: "Immutably divides pA / pB. Expands pA to an {x, y} if it is a single number.",
          done: false,
        },
        "num.p2.mid": {
          sig: "mid(pA, pB)",
          desc: "Calculates the midpoint between two points.",
          done: false,
        },
        "num.p2.dist": {
          sig: "dist(pA, pB)",
          desc: "Calculates the distance between two points.",
          done: false,
        },
        "num.p2.angle": {
          sig: "angle(pA, pB)",
          desc: "Calculates the angle between two points.",
          done: false,
        },
        "num.p2.dot": {
          sig: "dot(pA, pB)",
          desc: "Calculates the dot product of two points.",
          done: false,
        },
        "num.p2.floor": {
          sig: "floor(p)",
          desc: "Applies the floor function to both x and y coordinates of a point.",
          done: false,
        },
        "num.midp": {
          sig: "midp(a, b)",
          desc: "Find the midpoint between two [x, y] coordinates.",
          done: false,
        },
        "num.number": {
          sig: "number(maybeNumber)",
          desc: "Determine if the value is a number or not.",
          done: false,
        },
        "num.intersects": {
          sig: "intersects(line1, line2)",
          desc: "Compute whether two lines intersect. A line is: `{x0, y0, x1, y1}`",
          done: false,
        },
        "num.signedCeil": {
          sig: "signedCeil(n)",
          desc: "Ceil a number away from 0.",
          done: false,
        },
        "num.signedFloor": {
          sig: "signedFloor(val)",
          desc: "Floor a number towards 0.",
          done: false,
        },
        "num.vec2": {
          sig: "vec2.?",
          desc: "All the `vec2` functions from the `glMatrix` library.",
          done: false,
        },
        "num.vec3": {
          sig: "vec3.?",
          desc: "All the `vec3` functions form the `glMatrix` library.",
          done: false,
        },
        "num.vec4": {
          sig: "vec4.?",
          desc: "All the `vec4` functions from the `glMatrix` library.",
          done: false,
        },
        "num.mat3": {
          sig: "mat3.?",
          desc: "All the `mat3` functions from the `glMatrix` library.",
          done: false,
        },
        "num.mat4": {
          sig: "mat4.?",
          desc: "All the `mat4` functions from the `glMatrix` library.",
          done: false,
        },
        "num.quat": {
          sig: "quat.?",
          desc: "All the `quat` (quaternion) functions from the `glMatrix` library.",
          done: false,
        },
        "num.parseColor": {
          sig: "parseColor(params)",
          desc: "Parses a color from piece params.",
          done: false,
        },
        "num.findColor": {
          sig: "findColor(rgb)",
          desc: "Find a color inside of `cssColors` by value",
          done: false,
        },
        "num.saturate": {
          sig: "saturate(rgb, amount = 1)",
          desc: "Saturate a color by `amount`.",
          done: false,
        },
        "num.desaturate": {
          sig: "desaturate(rgb, amount = 1)",
          desc: "Desaturate a color by `amount`",
          done: false,
        },
        "num.shiftRGB": {
          sig: 'shiftRGB(a, b, step, mode = "lerp", range = 255)',
          desc: "Lerp two RGBA arrays, skipping alpha and rounding the output.",
          done: false,
        },
        "num.rgbToHexStr": {
          sig: "rgbToHex(r, g, b)",
          desc: "Convert separate rgb values to a single integer.",
          done: false,
        },
        "num.hexToRgb": {
          sig: "hexToRgb(h)",
          desc: "Takes either a string hex or a number hex and outputs and [RGB] array.",
          done: false,
        },
        "num.blend": {
          sig: "blend(dst, src, alphaIn = 1)",
          desc: "Alpha blends two colors, mutating and returning `dst`.",
          done: false,
        },
        "num.rgbToHsl": {
          sig: "rgbToHsl(r, g, b)",
          desc: "Convert rgb to hsl (360, 100, 100).",
          done: false,
        },
        "num.hslToRgb": {
          sig: "hslToRgb(h, s, l)",
          desc: "Convert hsl (360, 100, 100) to rgb.",
          done: false,
        },
        "num.rainbow": {
          sig: "rainbow()",
          desc: "Return a cycled color from the `rainbow` template.",
          done: false,
        },
        delay: {
          sig: "delay(fun, time)",
          desc: "Delay a function by `time` number of sim steps.",
          done: false,
        },
        blink: {
          sig: "blink(time, fun)",
          desc: "A looped `delay`.",
          done: false,
        },
        "geo.Box": {
          sig: "new Box()",
          desc: "A dynamic box with helpful methods.",
          done: false,
        },
        "geo.DirtyBox": {
          sig: "new DirtyBox()",
          desc: "A box model implementing dirty rectangle optimization.",
          done: false,
        },
        "geo.Grid": {
          sig: "new Grid(x, y, w, h, s = 1)",
          desc: "A 2 dimensional uniform grid, using a box as the frame (with scaling).",
          done: false,
        },
        "geo.Circle": {
          sig: "new Circle(x, y, radius = 8)",
          desc: "A generic circle model.",
          done: false,
        },
        "geo.linePointsFromAngle": {
          sig: "linePointsFromAngle(x1, y1, dist, degrees)",
          desc: "Project outwards from an origin point at dist, and degrees to get the full line.",
          done: false,
        },
        "geo.pointFrom": {
          sig: "pointFrom(x, y, angle, dist)",
          desc: "Project outwards from a point at an `angle` and `dist` and get the resulting point.",
          done: false,
        },
        "geo.Race": {
          sig: "new Race(opts = { quantized: true })",
          desc: "Follows a point over time.",
          done: false,
        },
        "geo.Quantizer": {
          sig: "new Quantizer(opts)",
          desc: "A simple model for lazy following of a 3D point.",
          done: false,
        },
      },
      help: {
        choose: {
          sig: "choose(a, b, ...)",
          desc: "Randomly return one of the arguments.",
          done: false,
        },
        flip: {
          sig: "flip()",
          desc: "Flip a coin, returning true or false.",
          done: false,
        },
        repeat: {
          sig: "repeat(n, fn)",
          desc: "Run a function `n` times, passing in `i` on each iteration and returning an array of the results (like map).",
          done: false,
        },
        every: {
          sig: "every(obj, value)",
          desc: "Set every property of an object to a certain value.",
          done: false,
        },
        any: {
          sig: "any(objOrArray)",
          desc: "Returns a random value from an object, or array.",
          done: false,
        },
        anyIndex: {
          sig: "anyIndex(array)",
          desc: "Returns a random index value from an array.",
          done: false,
        },
        anyKey: {
          sig: "anyKey(obj)",
          desc: "Returns a random key from an object.",
          done: false,
        },
        each: {
          sig: "each(obj, fun)",
          desc: "Run a function on every value in an object.",
          done: false,
        },
        shuffleInPlace: {
          sig: "shuffleInPlace(array)",
          desc: "Shuffles an array, mutating it.",
          done: false,
        },
        "gizmo.Hourglass": {
          sig: "new Hourglass(max, { completed, flipped, every, autoFlip = false }, startingTicks = 0)",
          desc: "A repeatable timer with callbacks.",
          done: false,
        },
        "gizmo.EllipsisTicker": {
          sig: "new EllipsisTicker()",
          desc: "An animated `...` string for showing processing indicators.",
          done: false,
        },
      },
      system: {
        signal: {
          sig: "signal(content)",
          desc: "Send a message through the `signal` system, good for communicating with added DOM content.",
          done: false,
        },
        sideload: {
          sig: "sideload(type)",
          desc: "Open a file chooser to load a file.",
          done: false,
        },
        user: {
          sig: "user",
          desc: "A reference to the currently logged in user.",
          done: false,
        },
        vscode: {
          sig: "vscode",
          desc: "A flag that's true while running the VS Code extension.",
          done: false,
        },
        meta: {
          sig: "meta(data)",
          desc: "Add meta to the common api so the data can be overridden as needed.",
          done: false,
        },
        reload: {
          sig: "reload({ piece, name, source, codeChannel })",
          desc: "Reload / start a piece in various ways. Used especially in live development.",
          done: false,
        },
        pieceCount: {
          sig: "pieceCount",
          desc: "Keeps track of how many pieces have been run so far in a session.",
          done: false,
        },
        store: {
          sig: "store",
          desc: "An object for keeping data in across piece jumps.",
          done: false,
        },
        "store.persist": {
          sig: 'store.persist(key, method = "local")',
          desc: "Save a storage key with associated data in the user's browser.",
          done: false,
        },
        "store.retrieve": {
          sig: 'store.retrieve(key, method = "local")',
          desc: "Load a storage key with associated data.",
          done: false,
        },
        "store.delete": {
          sig: 'store.persist(key, method = "local")',
          desc: "Remove a storage key and any saved data.",
          done: false,
        },
        debug: {
          sig: "debug",
          desc: "Reports whether the system is in debug / development mode.",
          done: false,
        },
        canShare: {
          sig: "canShare",
          desc: "",
          done: false,
        },
        handle: {
          sig: "handle()",
          desc: "Returns the user's handle, if one exists.",
          done: false,
        },
        ticket: {
          sig: "ticket(name)",
          desc: "Open a ticketed paywall by its name.",
          done: false,
        },
        mint: {
          sig: "mint(picture, progress, params)",
          desc: "Mint a picture on an external service.",
          done: false,
        },
        print: {
          sig: "print(picture, quantity, progress)",
          desc: "Print the `pixels` that get passed in via an external service. Stickers only right now.",
          done: false,
        },
        zip: {
          sig: "zip(content, progress)",
          desc: "Create a zip file of the content. Auto-encodes paintings.",
          done: false,
        },
        "motion.start": {
          sig: "start()",
          desc: "Start tracking device motion.",
          done: false,
        },
        "motion.stop": {
          sig: "stop()",
          desc: "Stop tracking device motion.",
          done: false,
        },
        "motion.current": {
          sig: "current",
          desc: "Populated with the device motion data upon `motion.start()`.",
          done: false,
        },
        speak: {
          sig: "speak(utterance, voice, mode, opts)",
          desc: "Speak an `utterance` aloud.",
          done: false,
        },
        act: {
          sig: "act(event, data)",
          desc: "Broadcast an `act` event through the system.",
          done: false,
        },
        "get.painting().by()": {
          sig: "get.painting(code, opts).by(handle, opts)",
          desc: "Retrieve a painting from network storage.",
          done: false,
        },
        upload: {
          sig: "async upload(filename, data, progress, bucket)",
          desc: "Upload a media file to network storage.",
          done: false,
        },
        "code.channel": {
          sig: "channel(chan)",
          desc: "Set the current code chnnel for live development.",
          done: false,
        },
        encode: {
          sig: "async encode(file)",
          desc: "File should be { type, data } where type is `png`, `webp`, or `jpg`, etc.",
          done: false,
        },
        file: {
          sig: "async file()",
          desc: "Request a file from the user.",
          done: false,
        },
        authorize: {
          sig: "async authorize()",
          desc: "Authorize a user.",
          done: false,
        },
        "hand.mediapipe": {
          sig: "mediapipe",
          desc: "A reference to the mediapipe hand tracking data. Enable through `video`.",
          done: false,
        },
        "hud.label": {
          sig: "label(text, color, offset)",
          desc: "Override the piece corner label.",
          done: false,
        },
        "hud.currentStatusColor": {
          sig: "currentStatusColor()",
          desc: "Get the current connection status label color.",
          done: false,
        },
        "hud.currentLabel": {
          sig: "currentLabel()",
          desc: "Get the current label content and button.",
          done: false,
        },
        "hud.labelBack": {
          sig: "labelBack()",
          desc: "Jump to the `prompt` with the current label applied.",
          done: false,
        },
        send: {
          sig: "send({type, content})",
          desc: "Send a message to the bios.",
          done: false,
        },
        platform: {
          sig: "platform",
          desc: "Get the current host platform.",
          done: false,
        },
        history: {
          sig: "history",
          desc: "An array of previously visited pieces in a session.",
          done: false,
        },
        "bgm.set": {
          sig: "set(trackNumber, volume)",
          desc: "Start a background music track, persisting across jumps.",
          done: false,
        },
        "bgm.stop": {
          sig: "stop()",
          desc: "Stop a background music track.",
          done: false,
        },
        "bgm.data": {
          sig: "data",
          desc: "Gets live analysis data from the current background track.",
          done: false,
        },
        "system.world": {
          sig: "system.world",
          desc: "A reference to the world system state if a piece is using it.",
          done: false,
        },
        "system.nopaint": {
          sig: "system.nopaint",
          desc: "A refrence to the `nopaint` system state that all brushes use.",
          done: false,
        },
        flatten: {
          sig: "flatten()",
          desc: "Paint (bake) all graphics commands immediately.",
          done: false,
        },
        connect: {
          sig: "connect()",
          desc: "Connect with external wallet software.",
          done: false,
        },
        wiggle: {
          sig: "wiggle(n, level, speed)",
          desc: "Oscillate a value over time using a sine wave.",
          done: false,
        },
        dark: {
          sig: "dark",
          desc: "Gets whether the system is in dark mode.",
          done: false,
        },
        darkMode: {
          sig: "darkMode(enabled)",
          desc: "Toggle dark mode on or off with a boolean.",
          done: false,
        },
        gpuReady: {
          sig: "gpuReady",
          desc: "Whether the system GPU is ready for rendering.",
          done: false,
        },
        "gpu.message": {
          sig: "message(content)",
          desc: "Send a message to the GPU driver.",
          done: false,
        },
      },
    },
    // 😱 Commands for entering into the prompt.
    prompts: {
      tape: {
        sig: "tape",
        desc: "Record your screen.",
        done: false,
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
        sig: "scream",
        desc: "Scream at all users.",
        done: false,
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
      "@maya/sparkle": {
        sig: "@maya/sparkle",
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
        sig: "mood",
        desc: "Set your mood.",
        done: false,
      },
      channel: {
        sig: "channel",
        desc: "View or set a piece code channel.",
        done: false,
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
      code: {
        sig: "code",
        desc: "Write a piece.",
        done: false,
      },
      edit: {
        sig: "edit",
        desc: "Edit a piece.",
        done: false,
      },
      source: {
        sig: "source",
        desc: "Download piece code.",
        done: false,
      },
      email: {
        sig: "email",
        desc: "Update your email.",
        done: false,
        hidden: false,
      },
      "admin:migrate-": {
        sig: "admin:migrate-",
        desc: "",
        done: false,
        hidden: true,
      },
      handle: {
        sig: "handle",
        desc: "Set your user handle.",
        done: false,
      },
      handles: {
        sig: "handles",
        desc: "A directory of user handles.",
        done: false,
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
        sig: "resize",
        desc: "Resize by x and y pixel #s.",
        done: false,
      },
      res: {
        sig: "res",
        desc: "Resize by x and y pixel #s.",
        done: false,
      },
      dl: {
        sig: "dl",
        desc: "Download your painting.",
        done: false,
      },
      download: {
        sig: "download",
        desc: "Download your painting.",
        done: false,
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
        sig: "google",
        desc: "Search google.",
        done: false,
        //TODO: can this open in a new tab?
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
      camera: {
        sig: "camera",
        desc: "Take a picture.",
        done: false,
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
        sig: "clock",
        desc: "Every computer needs a clock.",
        done: false
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
      line: {
        sig: "line",
        desc: "Draw a 1px line.",
        done: false,
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
      nopaint: {
        sig: "nopaint",
        desc: "",
        done: false,
        hidden: true,
        //TODO: shouldnt this go to nopaint site? nopaint.art
      },
      notepat: {
        sig: "notepat",
        desc: "A melodic keyboard instrument.",
        done: false,
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
        sig: "tone",
        desc: "Listen to a tone.",
        done: false,
      },
      toss: {
        sig: "toss",
        desc: "Play microtonal oscillators.",
        done: false,
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
        .code-doc-welcome {
          padding-top: 2.75em;
          font-size: 65%;
          padding-bottom: 3em;
        }
        pre {
          margin-top: 1em;
          margin-bottom: 1em;
        }
        pre code.hljs.language-javascript {
          padding: 0.2em 0em;
          position: relative;
          overflow-x: visible;
        }
        pre code.hljs.language-javascript:after {
          content: "";
          height: 100%;
          top: 0;
          right: -16px;
          width: 16;
          background-color: #f3f3f3;
          position: absolute;
        }
        pre code.hljs.language-javascript:before {
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
          .language-javascript.hljs {
            color: white;
          }
          pre code.hljs.language-javascript,
          pre code.hljs.language-javascript:after,
          pre code.hljs.language-javascript:before {
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
      <pre><code class="language-javascript">$sig</code></pre>
      <p>$desc</p>
    </div>
  `.trim();

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

  const indexContent = html`
    <h1 id="title">
      <a
        href="https://${event.headers["host"]}"
        onclick="if (window.opener) window.close();"
        >docs</a
      >
    </h1>
    <div class="code-doc-welcome">
      <h2>Graphics</h2>
      <span class="links">
        <a
          data-done="${docs.api.structure.paint.done}"
          class="top-level"
          href="/docs/structure:paint"
          >paint</a
        >
        ${genLinks("graphics")}
      </span>
      <h2>Number</h2>
      <span class="links">
        <a
          data-done="${docs.api.structure.sim.done}"
          class="top-level"
          href="/docs/structure:sim"
          >sim</a
        >
        ${genLinks("number")}
      </span>
      <h2>Sound</h2>
      <span class="links">
        <a
          data-done="${docs.api.structure.beat.done}"
          class="top-level"
          href="/docs/structure:beat"
          >beat</a
        >
        ${genLinks("sound")}
        <h2>Interaction</h2>
        <span class="links">
          <a
            data-done="${docs.api.structure.act.done}"
            class="top-level"
            href="/docs/structure:act"
            >act</a
          >
          ${genLinks("interaction")}
        </span>
        <h2>Network</h2>
        <span class="links">
          <a
            data-done="${docs.api.structure.meta.done}"
            class="top-level"
            href="/docs/structure:meta"
            >meta</a
          >
          <a
            data-done="${docs.api.structure.icon.done}"
            class="top-level"
            href="/docs/structure:icon"
            >icon</a
          >
          <a
            data-done="${docs.api.structure.preview.done}"
            class="top-level"
            href="/docs/structure:preview"
            >preview</a
          >
          ${genLinks("network")}
        </span>
      </span>
      <h2>Help</h2>
      <span class="links">${genLinks("help")}</span>
      <h2>System</h2>
      <span class="links">
        <a
          data-done="${docs.api.structure.boot.done}"
          class="top-level"
          href="/docs/structure:boot"
          >boot</a
        >
        ${genLinks("system")}
      </span>
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
      doc = docs.api[category][word];
    } else if (category) {
      doc = docs[category][word];
    } else {
      return respond(404, "Not found. :(", {
        "Content-Type": "text/html; charset=UTF-8",
      });
    }

    return respond(
      200,
      page
        .replace("$bodyclass", " class='doc'")
        .replace("$content", content)
        .replaceAll("$name", word)
        .replaceAll("$sig", doc?.sig)
        .replaceAll("$desc", doc?.desc)
        .replaceAll("$done", doc?.done),
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
