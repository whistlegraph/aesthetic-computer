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
import { repeat } from "../lib/help.mjs";

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

    if (Array.isArray(pos)) {
      pos = { x: pos[0], y: pos[1] };
    }

    // Randomize pos.x and pos.y if undefined.
    if (pos.center === undefined) {
      if (pos.x === undefined) pos.x = $.num.randInt($.screen.width);
      if (pos.y === undefined) pos.y = $.num.randInt($.screen.height);
    }

    // Set x, y position and override if centering is specified.
    let x = pos.x || 0,
      y = (pos.y || 0) + lineNumber * blockHeight;

    pos.center = pos.center || "";

    if (pos.center.includes("x")) {
      const hw = (text.length * blockWidth * size) / 2;
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
  showBlink = true;
  cursor = "blink";
  go;

  canType = false;

  #autolock = true;
  lock = false;

  #prompt;

  typeface;
  pal; // color palette
  wrap = "char"; // auto-wrap setting

  processCommand; // text processing callback
  historyDepth = 0;

  inputStarted = false; // Flipped when the TextInput is first activated.
  //                       (To clear any starting text.)
  #movedCursor; // Shift the cursor off the end of the prompt by dragging or
  //               using the arrow keys.
  #moveThreshold = 6; // Drag threshold.
  #moveDeltaX = 0;

  runnable = false; // Whether a command can be tried.
  lastText; // Store the last text reply.
  didReset; // Callback for blank reset.

  key;

  // Add support for loading from preloaded system typeface.
  constructor(
    $,
    text = "",
    processCommand,
    options = {
      palette: undefined,
      font: font1,
      autolock: true,
      wrap: "char",
      didReset,
    }
  ) {
    this.key = `${$.slug}:history`; // This is "per-piece" and should
    //                                be per TextInput object...23.05.23.12.50

    // Load typeface, preventing double loading of the system default.
    if ($.typeface?.data !== options.font) {
      this.typeface = new Typeface(options.font); // Load custom typeface.
      this.typeface.load($.net.preload);
    } else {
      this.typeface = $.typeface; // Set to system typeface.
    }

    this.#autolock = options.autolock;
    this.didReset = options.didReset;

    this.#prompt = new Prompt(6, 6, floor($.screen.width / 6) - 2);

    this.#movedCursor = null; // Used when the pointer moves the cursor.

    this.text = text;
    this.lastText = text;
    this.wrap = options.wrap || "char";
    this.startingInput = this.text;
    this.pal = options.palette || {
      fg: 255,
      bg: 0,
      block: 255,
      blockHi: 0,
      line: 255,
    };

    const {
      ui: { TextButton: TB },
    } = $;
    this.go = new TB("Enter");

    if (this.text.length === 0) {
      this.go.btn.disabled = true;
    }

    this.processCommand = processCommand;
    $.send({ type: "keyboard:enabled" });
  }

  // Paint the TextInput, with an optional `frame` for placement.
  // TODO: Provide a full frame along with an x, y position..
  paint($, clear = false, frame = $.screen) {
    if (!clear && this.pal.bg !== undefined) $.ink(this.pal.bg).box(frame); // Paint bg.

    const prompt = this.#prompt;
    prompt.cursor = { x: 0, y: 0 };

    // Wrap and render the text.
    if (this.wrap === "char") {
      // Print `text` to the prompt one "char" at time if it exists in the font.
      for (const char of this.text) {
        if (char.charCodeAt(0) === 10) {
          prompt.newLine();
        } else {
          const pic = this.typeface.glyphs[char];
          if (pic) $.ink(this.pal.fg).draw(pic, prompt.pos, prompt.scale);
          if (pic || char === " ") prompt.forward(); // Move cursor on a match.
        }
      }
    } else if (this.wrap === "word") {
      const words = this.text.split(" ");

      words.forEach((word, i) => {
        // Look ahead at word lenth.
        const wordLen = word.length;
        if (prompt.cursor.x + wordLen >= prompt.colWidth) prompt.newLine();
        [...word].forEach((char, index) => {
          // Detect new line character.
          if (char.charCodeAt(0) === 10) {
            prompt.newLine();
          } else {
            const pic = this.typeface.glyphs[char];
            if (pic) {
              $.ink(this.pal.fg).draw(pic, prompt.pos, prompt.scale);
              prompt.forward();
            }
          }
        });

        if (i < words.length - 1 && prompt.cursor.x !== 0) prompt.forward(); // Move forward a space.
      });
    }

    // TODO: Now offset the cursor if it's not at the end.
    if (this.#movedCursor) {
      prompt.cursor = this.#movedCursor;
    }

    if (this.canType) {
      $.ink(this.pal.line).line(
        prompt.gutter,
        0,
        prompt.gutter,
        $.screen.height
      ); // Ruler
      $.ink(127).box(0, 0, $.screen.width, $.screen.height, "inline"); // Focus
    }

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

      $.ink(this.pal.block);
      if ($.paintCount % 60 < 20) {
        $.line(...topR, ...bottomL);
      } else if ($.paintCount % 60 < 40) {
        $.line(...middleL, ...middleR);
      } else {
        $.line(...topL, ...bottomR);
      }
    } else {
      if (this.cursor === "blink" && this.showBlink && this.canType) {
        $.ink(this.pal.block).box(prompt.pos); // Draw blinking cursor.

        // 🧨
        // TODO: Prompt index is different from text index due to
        //       text wrapping, so I need to store an offset.

        // Prompt needs to store an text index -> prompt index map..
        // Or at every prompt index there needs to be a character index
        // related to text, or null.

        // And if it's null then we skip it when dragging left or right.

        const index = this.#prompt.index;
        const char = this.text[index];

        const pic = this.typeface.glyphs[char];
        if (pic) $.ink(this.pal.blockHi).draw(pic, prompt.pos);
      }
    }

    if (this.cursor === "stop" && !this.canType) {
      $.ink(255, 0, 0).box(prompt.pos.x + 1, prompt.pos.y + 3, 3);
    }

    // Prompt Button
    if (!this.go.btn.disabled) {
      this.go.reposition({ right: 6, bottom: 6, screen: frame });
      // if (this.go.txt === "Enter") {
      // this.go.paint({ ink: $.ink }, [
      //   [0, 100, 0],
      //   [0, 255, 0, 150],
      //   [0, 200, 0],
      //   [0, 50, 0, 0],
      // ]);
      // } else
      this.go.paint({ ink: $.ink });
    }

    // Return false if we have loaded every glyph.
    // (Can be wired up to the return value of the parent's `paint`)
    // TODO: This causes some extra paints on startup.
    return !(
      keys(this.typeface.glyphs).length === keys(this.typeface.data).length
    );
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

  showButton(txt) {
    this.go.btn.disabled = false;
    this.go.txt = txt || "Enter";
  }

  // Forget the original finished message.
  forget() {
    this.lastText = "";
  }

  // Run a command.
  async #execute(store) {
    // Make a history stack if one doesn't exist already.
    store[this.key] = store[this.key] || [];
    // Push input to a history stack, avoiding repeats.
    if (store[this.key][0] !== this.text) store[this.key].unshift(this.text);
    // console.log("📚 Stored prompt history:", store[key]);
    store.persist(this.key); // Persist the history stack across tabs.

    // 🍎 Process commands for a given context, passing the text input.
    if (this.#autolock) this.lock = true;
    await this.processCommand?.(this.text);
    if (this.#autolock) this.lock = false;
  }

  // Clear the TextInput object and flip the cursor to ON.
  blank(cursor) {
    if (cursor) this.cursor = cursor;
    this.text = "";
    this.#movedCursor = null;
    this.blink?.flip(true);
  }

  // Handle user input.
  async act($) {
    const { event: e, store, needsPaint } = $;

    // Reflow the prompt on frame resize.
    if (e.is("reframed")) {
      this.#prompt.resize(floor($.screen.width / 6) - 2);
      needsPaint();
    }

    // ✂️ Paste from user clipboard.
    if (e.is("pasted:text") && this.lock === false) {
      const index = this.#prompt.index;
      const paste = e.text;
      this.text = this.text.slice(0, index) + paste + this.text.slice(index);
      if (this.#movedCursor)
        this.#prompt.forward(this.#movedCursor, paste.length);
      this.blink?.flip(true);
    }

    // ⌨️ Add text via the keyboard.
    if (e.is("keyboard:down") && this.lock === false) {
      if (this.canType === false) {
        this.canType = true;
        this.text = "";
        this.#movedCursor = null;
        this.inputStarted = true;
        this.#prompt.cursor = { x: 0, y: 0 };
      }

      if (e.key.length === 1 && e.ctrl === false && e.key !== "`") {
        if (this.text === "" && e.key === " ") {
          this.blink?.flip(true);
          return; // Skip opening spaces.
        }

        // Printable keys with subbed punctuation.
        let insert = e.key.replace(/[“”]/g, '"').replace(/[‘’]/g, "'");
        // Insert text at the calculated index
        const index = this.#prompt.index;
        // Add spaces if the cursor has scrolled beyond the text.
        if (this.#movedCursor) this.#prompt.forward(this.#movedCursor);
        this.text = this.text.slice(0, index) + insert + this.text.slice(index);
      } else {
        // Other keys.
        if (e.key === "Delete") {
          // Delete the character under the cursor.
          const index = this.#prompt.index;
          // console.log("Index:", index, "Length:", this.text.length);

          this.text = this.text.slice(0, index) + this.text.slice(index + 1);
        } else if (e.key === "Backspace") {
          if (this.text.length === 1) {
            this.text = "";
            this.#movedCursor = null;
          } else {
            if (this.#movedCursor) {
              const index = this.#prompt.index;
              if (index === 0 && this.text.length > 0) {
              } else {
                this.text =
                  this.text.slice(0, index - 1) + this.text.slice(index);
              }
              this.#prompt.backward(this.#movedCursor);
            } else {
              this.text = this.text.slice(0, -1);
            }
          }
        }

        if (e.key === "Enter" && this.runnable) await this.#execute(store); // Send a command.

        if (e.key === "Escape") {
          this.#movedCursor = null;
          this.text = "";
        }

        // Move backwards through history stack.
        if (e.key === "ArrowUp") {
          const history = (await store.retrieve(this.key)) || [""];
          this.text = history[this.historyDepth];
          this.#movedCursor = null;
          this.historyDepth = (this.historyDepth + 1) % history.length;
        }

        // ... and forwards.
        if (e.key === "ArrowDown") {
          const history = (await store.retrieve(this.key)) || [""];
          this.text = history[this.historyDepth];
          this.#movedCursor = null;
          this.historyDepth -= 1;
          if (this.historyDepth < 0) this.historyDepth = history.length - 1;
        }

        // Move cursor forward.
        if (e.key === "ArrowRight") {
          if (this.#prompt.index < this.text.length) {
            if (!this.#movedCursor) this.#movedCursor = this.#prompt.cursor;
            this.#prompt.forward(this.#movedCursor);
            if (this.#prompt.index === this.text.length)
              this.#movedCursor = null;
          }
        }

        // Move cursor backward.
        if (e.key === "ArrowLeft") {
          if (!this.#movedCursor) this.#movedCursor = this.#prompt.cursor;
          this.#prompt.backward(this.#movedCursor);
        }
      }

      if (e.key !== "Enter" && this.text.length > 0) {
        this.go.btn.disabled = false;
        this.go.txt = "Enter";
        this.runnable = true;
      } else {
        this.go.btn.disabled = true;
        this.runnable = false;
      }

      this.blink?.flip(true);
    }

    // Handle activation / focusing of the input
    // (including os-level software keyboard overlays)
    // if (e.is("keyboard:open") && this.inputStarted) this.canType = true;
    // if (e.is("keyboard:open")) {}

    // if (e.is("keyboard:close")) {
    //  console.log("keyboard close...");
    //  $.send({ type: `keyboard:${!this.lock ? "unlock" : "lock"}` });
    // }

    // if (e.is("focus")) {}
    // if (e.is("defocus")) {}

    if (e.is("touch") && !this.lock && !this.inputStarted && !this.canType) {
      $.send({ type: "keyboard:lock" });
    }

    if (!this.lock) {
      this.go.btn.act(e, {
        down: () => {
          $.send({ type: "keyboard:unlock" });
        },
        push: async () => {
          if (this.runnable) {
            await this.#execute(store);
            this.go.btn.disabled = true;
          } else {
            this.lastText = this.text;
            this.text = "";
            this.go.btn.disabled = true;
            this.canType = true;
            this.cursor = "blink";
            this.blink?.flip(true);
            needsPaint();
            this.inputStarted = true;
            $.send({ type: "keyboard:unlock" });
          }
        },
        cancel: () => {
          $.send({ type: "keyboard:lock" });
        },
        rollover: (btn) => {
          if (btn) $.send({ type: "keyboard:unlock" });
        },
        rollout: () => {
          $.send({ type: "keyboard:lock" });
        },
      });
    }

    if (e.is("touch") && e.device === "mouse" && !this.lock) {
      this.blink?.flip(true);
    }

    if (e.is("lift") && !this.lock) {
      this.moveDeltaX = 0;
      $.send({ type: "keyboard:unlock" });
    }

    if (e.is("draw") && !this.lock && this.canType && !this.go.btn.down) {
      $.send({ type: "keyboard:lock" });

      if (
        (this.#moveDeltaX > 0 && e.delta.x < 0) ||
        (this.#moveDeltaX < 0 && e.delta.x > 0)
      ) {
        this.#moveDeltaX = 0; // Reset delta on every directional change.
      }

      this.#moveDeltaX += e.delta.x; // Add up the deltas.

      while (this.#moveDeltaX <= -this.#moveThreshold) {
        this.#moveDeltaX += this.#moveThreshold;
        if (!this.#movedCursor) this.#movedCursor = this.#prompt.cursor;
        this.#prompt.backward(this.#movedCursor);
      }

      while (
        this.#moveDeltaX >= this.#moveThreshold &&
        this.#prompt.index < this.text.length
      ) {
        this.#moveDeltaX -= this.#moveThreshold;
        if (!this.#movedCursor) this.#movedCursor = this.#prompt.cursor;
        this.#prompt.forward(this.#movedCursor);
        if (this.#prompt.index === this.text.length) this.#movedCursor = null;
      }

      this.blink?.flip(true);
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

  resize(newColWidth) {
    this.colWidth = newColWidth;
    // TODO: Reflow the prompt cursor here?
  }

  get index() {
    const x = this.cursor.x;
    const y = this.cursor.y;
    const cols = this.colWidth;
    const lineBreaks = y; // Number of line breaks before the current row
    return y * (cols + 1) + x - lineBreaks;
  }

  // Caluclate the screen x, y position of the top left of the cursor.
  get pos() {
    const x = this.top + this.cursor.x * this.letterWidth;
    const y = this.left + this.cursor.y * this.letterHeight;
    return { x, y, w: this.letterWidth, h: this.letterHeight };
  }

  // Move the cursor forward, optionally input an override cursor.
  forward(cursor = this.cursor, amount = 1) {
    repeat(amount, () => {
      cursor.x = (cursor.x + 1) % (this.colWidth - 1);
      if (cursor.x === 0) cursor.y += 1;
    });
  }

  // Move cursor backward, with optional override cursor.
  backward(cursor = this.cursor) {
    if (cursor.x === 0) {
      if (cursor.y > 0) {
        cursor.y -= 1;
        cursor.x = this.colWidth - 2;
      }
    } else {
      cursor.x -= 1;
    }
  }

  // Create a cursor line break.
  newLine() {
    this.cursor.y += 1;
    this.cursor.x = 0;
  }
}

export { Typeface, TextInput, Prompt };
