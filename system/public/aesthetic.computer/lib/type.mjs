// Type, 22.12.10.14.06
// Abstraction for typography and text input.

/* #region 🏁 todo
 + Later
 - [] Make history on message input optional?
 - [] Gracefully allow for multiple instances of TextInput in a single piece? 
 - [] Add tab auto-completion feature that can be side-loaded with contextual
      data based on where the text module is used.
 + Done
 - [x] Add different colors to "print" / storing the ink color / writing
      a backdrop somehow... maybe using layer?
#endregion */

import { font1 } from "../disks/common/fonts.mjs";

const { floor } = Math;
const { keys, entries } = Object;
const undef = undefined;

// Preloads and holds the glyphs for a system typeface.
class Typeface {
  data;
  name;
  glyphs = {};
  //loaded = false;

  constructor(data = font1, name = "font-1") {
    this.data = data;
    this.name = name;
  }

  async load($preload) {
    // 1. Ignore any keys with a "glyph" prefix because these are settings.
    const glyphsToLoad = entries(this.data).filter(
      ([g, loc]) => !g.startsWith("glyph")
    );
    const promises = glyphsToLoad.map(([glyph, location], i) => {
      // 2. Load all other keys / glyphs over the network.
      return $preload(
        `aesthetic.computer/disks/drawings/${this.name}/${location}.json`
      ).then((res) => {
        this.glyphs[glyph] = res;
      });
    });

    // Wait for all the promises to resolve before returning
    await Promise.all(promises);
    return this;
  }

  // TODO: Add ability to center text on its line.

  print(
    $,
    pos = { x: undef, y: undef, size: 1, thickness: 1, rotation: 0 },
    lineNumber,
    text,
    bg = null
  ) {
    // TODO: Pass printLine params through / make a state machine.
    const font = this.glyphs;
    const lineHeightGap = 2;
    const size = pos.size || 1;
    const blockHeight = (this.data.glyphHeight || 9) * size + lineHeightGap;
    const blockWidth = 6;
    const thickness = pos.thickness || 1;
    const rotation = pos.rotation || 0;

    // Set x, y position and override if centering is specified.
    let x = pos.x || 0,
      y = (pos.y || 0) + lineNumber * blockHeight;

    pos.center = pos.center || "";

    if (pos.center.includes("x")) {
      const hw = (text.length * blockWidth) / 2;
      x = pos.x === undef ? $.screen.width / 2 - hw : x - hw;
    }
    if (pos.center.includes("y")) {
      const hh = blockHeight / 2;
      y = pos.y === undef ? $.screen.height / 2 - hh : y - hh;
    }

    const rn = $.inkrn(); // Remember the current ink color.

    // Background
    if (bg !== null) {
      $.ink(bg).box(x, y, blockWidth * size * text.length, blockHeight);
    }

    $.ink(rn).printLine(
      text,
      font,
      x,
      y,
      blockWidth,
      size,
      0,
      thickness,
      rotation
    ); // Text
  }
}

// An interactive text prompt object.
class TextInput {
  text; // text content

  blink; // block cursor blink timer
  showBlink = false;
  cursor = "blink";

  canType = false;

  #autolock = true;
  lock = false;

  typeface;
  pal; // color palette
  wrap = "char"; // auto-wrap setting

  processCommand; // text processing callback
  historyDepth = 0;

  // Add support for loading from preloaded system typeface.
  constructor(
    $,
    text = "",
    processCommand,
    options = { palette: undefined, font: font1, autolock: true, wrap: "char" }
  ) {
    // Load typeface, preventing double loading of the system default.
    if ($.typeface?.data !== options.font) {
      this.typeface = new Typeface(options.font); // Load custom typeface.
      this.typeface.load($.net.preload);
    } else {
      this.typeface = $.typeface; // Set to system typeface.
    }

    this.#autolock = options.autolock;

    this.text = text;
    this.wrap = options.wrap;
    this.startingInput = this.text;
    this.pal = options.palette || {
      fg: 255,
      bg: 0,
      block: 255,
      line: 255,
    };
    this.processCommand = processCommand;
    $.send({ type: "text-input-enabled" });
  }

  paint($) {
    const prompt = new Prompt(6, 6, floor($.screen.width / 6) - 2);

    // Wrap and render the text.
    if (this.wrap === "char") {
      // Print `text` to the prompt one "char" at time if it exists in the font.
      for (const char of this.text) {
        const pic = this.typeface.glyphs[char];
        if (pic) $.ink(this.pal.fg).draw(pic, prompt.pos, prompt.scale);
        if (pic || char === " ") prompt.forward(); // Move cursor on a match.
      }
    } else if (this.wrap === "word") {
      const words = this.text.split(" ");

      words.forEach((word, i) => {
        // Look ahead at word lenth.
        const wordLen = word.length;
        if (prompt.cursor.x + wordLen >= prompt.colWidth) prompt.newLine();

        for (const char of word) {
          const pic = this.typeface.glyphs[char];
          if (pic) {
            $.ink(this.pal.fg).draw(pic, prompt.pos, prompt.scale);
            prompt.forward();
          }
        }

        if (i < words.length - 1) prompt.forward(); // Move forward a space.
      });
    }

    if (this.canType) {
      $.ink(this.pal.line).line(
        prompt.gutter,
        0,
        prompt.gutter,
        $.screen.height
      ); // Ruler
      $.ink(127).box(0, 0, $.screen.width, $.screen.height, "inline"); // Focus

      if (this.lock) {
        // Show a spinner if the prompt is "locked".
        const center = $.geo.Box.from(prompt.pos).center;
        const distance = 2; // You can adjust this value as per your needs

        const topL = [center.x - distance, center.y - distance];
        const topR = [center.x + distance, center.y - distance];
        const bottomL = [center.x - distance, center.y + distance];
        const bottomR = [center.x + distance, center.y + distance];
        const middleL = [center.x - distance, center.y];
        const middleR = [center.x + distance, center.y];

        // const topL = [r.x, r.y];
        // const topR = [r.x + r.w, r.y];
        // const bottomL = [r.x, r.y + r.h];
        // const bottomR = [r.x + r.w, r.y + r.h];
        // const middleL = [r.x, r.y + r.h / 2];
        // const middleR = [r.x + r.w, r.y + r.h / 2];

        $.ink(this.pal.block);
        if ($.paintCount % 60 < 20) {
          $.line(...topR, ...bottomL);
        } else if ($.paintCount % 60 < 40) {
          $.line(...middleL, ...middleR);
        } else {
          $.line(...topL, ...bottomR);
        }
      } else {
        if (this.cursor === "blink" && this.showBlink)
          $.ink(this.pal.block).box(prompt.pos); // Draw blinking cursor.
        if (this.cursor === "stop") {
          $.ink(255, 0, 0).box(prompt.pos.x + 1, prompt.pos.y + 3, 3);
        }
      }
    }

    // Return false if we have loaded every glyph.
    // (Can be wired up to the return value of the parent's `paint`)
    // TODO: This causes some extra paints on startup.
    return !(
      keys(this.typeface.glyphs).length === keys(this.typeface.data).length
    );
  }

  // Clear the TextInput object and flip the cursor to ON.
  blank(cursor) {
    if (cursor) this.cursor = cursor;
    this.text = "";
    this.blink?.flip(true);
  }

  // Simulate anything necessary.
  sim({ seconds, needsPaint, gizmo: { Hourglass } }) {
    this.blink =
      this.blink ||
      new Hourglass(seconds(0.75), {
        flipped: (showBlinkOverride) => {
          if (showBlinkOverride !== undefined)
            this.showBlink = showBlinkOverride;
          else this.showBlink = !this.showBlink;
          needsPaint();
        },
        autoFlip: true,
      });

    if (this.lock) needsPaint();
    if (this.canType) this.blink.step();
  }

  // Handle user input.
  async act($) {
    const { event: e, slug, store, needsPaint } = $;

    // ✂️ Paste from user clipboard.
    if (e.is("pasted:text") && this.lock === false) {
      this.text += e.text;
      this.blink?.flip(true);
    }

    // ⌨️ Add text via the keyboard.
    if (e.is("keyboard:down") && this.lock === false) {
      if (this.canType === false) {
        this.canType = true;
        this.text = "";
      }

      if (e.key.length === 1 && e.ctrl === false && e.key !== "`") {
        if (this.text === "" && e.key === " ") {
          this.blink?.flip(true);
          return; // Skip openeing spaces.
        }
        this.text += e.key.replace(/[“”]/g, '"').replace(/[‘’]/g, "'"); // Printable keys with subbed punctuation.
      } else {
        // Other keys.
        if (e.key === "Backspace") this.text = this.text.slice(0, -1);
        const key = `${slug}:history`;

        // Send a command or message.
        if (e.key === "Enter") {
          // Make a history stack if one doesn't exist already.
          store[key] = store[key] || [];

          // Push input to a history stack, avoiding repeats.
          if (store[key][0] !== this.text) store[key].unshift(this.text);

          // console.log("📚 Stored prompt history:", store[key]);
          store.persist(key); // Persist the history stack across tabs.

          // 🍎 Process commands for a given context, passing the text input.
          if (this.#autolock) this.lock = true;
          await this.processCommand?.(this.text);
          if (this.#autolock) this.lock = false;
          // this.text = "";
        }

        if (e.key === "Escape") this.text = "";

        // Move backwards through history stack.
        if (e.key === "ArrowUp") {
          const history = (await store.retrieve(key)) || [""];
          this.text = history[this.historyDepth];
          this.historyDepth = (this.historyDepth + 1) % history.length;
        }

        // ... and forwards.
        if (e.key === "ArrowDown") {
          const history = (await store.retrieve(key)) || [""];
          this.text = history[this.historyDepth];
          this.historyDepth -= 1;
          if (this.historyDepth < 0) this.historyDepth = history.length - 1;
        }
      }
      this.blink?.flip(true);
    }

    // Handle activation / focusing of the input
    // (including os-level software keyboard overlays)
    if (e.is("typing-input-ready")) {
      this.canType = true;
      this.text = "";
      this.blink?.flip(true);
    }

    if (e.is("keyboard:close")) {
      this.canType = false;
      needsPaint();
    }

    if (e.is("defocus")) {
      this.canType = false;
      this.text = this.startingInput;
      needsPaint();
    }
  }
}

// Manages the scale / wrapping of text and the interaction of a cursor.
// (Just for rendering of `Text`)
class Prompt {
  top = 0;
  left = 0;

  scale = 1;
  blockWidth = 6;
  blockHeight = 10;
  letterWidth = this.blockWidth * this.scale;
  letterHeight = this.blockHeight * this.scale;

  colWidth = 48; // Maximum character width of each line before wrapping.

  cursor = { x: 0, y: 0 };
  gutter; // A y-position at the end of the colWidth.

  constructor(top = 0, left = 0, colWidth = 48) {
    this.top = top;
    this.left = left;
    this.colWidth = colWidth;
    this.gutter = this.colWidth * this.blockWidth;
  }

  get pos() {
    const x = this.top + this.cursor.x * this.letterWidth;
    const y = this.left + this.cursor.y * this.letterHeight;
    return { x, y, w: this.letterWidth, h: this.letterHeight };
  }

  forward() {
    this.cursor.x = (this.cursor.x + 1) % (this.colWidth - 1);
    if (this.cursor.x === 0) this.cursor.y += 1;
  }

  newLine() {
    this.cursor.y += 1;
    this.cursor.x = 0;
  }
}

export { Typeface, TextInput, Prompt };
