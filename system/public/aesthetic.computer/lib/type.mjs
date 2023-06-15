// Type, 22.12.10.14.06
// Abstraction for typography and text input.

/* #region 🏁 todo
 + Next Version of `TextInput` (before recording)
 - [-] Add multi-select / shift+select to replace or modify whole regions. 
 + Later
 - [] Add tab auto-completion feature that can be side-loaded with contextual
      data based on where the text module is used.
 - [] Make history on message input optional?
 - [] Gracefully allow for multiple instances of TextInput in a single piece? 
 + Done
 - [x] Add support for spaces to be inserted before the
      first character.
 - [x] Add support for creating line breaks.
 - [x] Word break edge cases.
   (Insertion + Deletion)
   - [x] Backspacing characters at the start character of the broken
         word should remove all extraneous space before the next word...
   - [x] Adding spaces on the start character of a broken word *should*
         increase the number of spaces by default...
   - [x] A full length word appearing on a line after a break
         will jump the line wrong and the cursor wrong.
   - [x] Creating new lines (SHIFT+ENTER) at the start of broken words should
         create an extra new line.
 - [x] Enter after a reply does not clear the cursor posiiton Enter after a reply does not clear the cursor position.
 - [x] Don't Backspace when cursor is on first character. 
 - [x] Test line break printing again.
  - [x] Word wrapping.
  - [x] Character wrapping.
 - [x] Get character wrapping working.
 - [x] Can't move cursor to the right when under a single character text.
 - [x] Disallow opening spaces.
 - [x] Rewrite paste to work. 
 - [x] Test movable cursor again.
 - [x] Add debug flag for drawing of spaces.
 - [x️‍] Scrubbing does not respect word wrapping.
 - [x] Infinite loop while adding spaces before the first character of the
       first line.
 - [x] Adding space between two words / causing a break from inside will
       shove the cursor to the top left.
 - [x] Backspacing the cursor on the first character of any line
       doesn't work.
 - [x] The cursor does not jump accordingly when inserting a character
       inside a word that will break it to the next line.
 - [x‍] Re-calculate gutter on resize.
 - [x] Add a gutter command to change the prompt gutter.
 - [x] Moving cursor to the right does not respect word breaks
 - [x] Draw glyphs under the moved cursor.
 - [x] "`" hotkeying back should start with the cursor non-visible.
 - [x] Receiving a bot reply should update the spinner. 
 - [x] Pressing return should reset the cursor and just work...
 - [😃] Rewrite Prompt with index maps to finish word wrapping support.
 - [x] Upcycling commands should reset the cursor position.
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

  // Return only the character index from the data.
  get glyphData() {
    const glyphsOnly = { ...this.data };
    // TODO: Remove other "glyph" prefixes here if they ever exist. 23.06.07.01.10
    delete glyphsOnly.glyphHeight;
    return glyphsOnly;
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
  #text; // text content

  #renderSpaces = false; // Whether to render invisible space characters. " "
  //                        For debugging purposes.

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
  scheme;

  processCommand; // text processing callback
  historyDepth = 0;

  inputStarted = false; // Flipped when the TextInput is first activated.
  //                       (To clear any starting text.)
  //               using the arrow keys.
  #moveThreshold = 6; // Drag threshold.
  #moveDeltaX = 0;

  runnable = false; // Whether a command can be tried.
  didReset; // Callback for blank reset.

  key;

  editableCallback; // A function that can run when the TextInput is made
  //                   editable.

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
      editable,
    }
  ) {
    this.key = `${$.slug}:history`; // This is "per-piece" and should
    //                                be per TextInput object...23.05.23.12.50

    this.editableCallback = options.editable;

    // Load typeface, preventing double loading of the system default.
    if ($.typeface?.data !== options.font) {
      this.typeface = new Typeface(options.font); // Load custom typeface.
      this.typeface.load($.net.preload);
    } else {
      this.typeface = $.typeface; // Set to system typeface.
    }

    this.#autolock = options.autolock;
    this.didReset = options.didReset;

    this.#prompt = new Prompt(
      6,
      6,
      options.wrap, // "char" or "word"
      floor($.screen.width / 6) - 2 // colWidth
    );

    this.text = text;

    this.startingInput = this.text;
    this.scheme = options.scheme || {
      dark: {
        fg: 255,
        bg: 0,
        block: 255,
        blockHi: 0,
        line: 255,
      },
      light: {
        fg: 0,
        bg: 255,
        block: 0,
        blockHi: 255,
        line: 0,
      },
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

  // Adjust the gutter width for text wrapping.
  set gutter(n) {
    this.#prompt.colWidth = n;
    this.#prompt.gutter = this.#prompt.colWidth * this.#prompt.blockWidth;
  }

  // Snap cursor to the end of text.
  snap() {
    this.#prompt.snapTo(this.text);
  }

  // Run a command
  async run(store) {
    this.#prompt.snapTo(this.text);
    await this.#execute(store); // Send a command.
  }

  // Set the text and reflow it.
  set text(str) {
    this.#text = str;
    this.flow();
  }

  // Reflow the input text.
  flow() {
    this.#prompt.mapTo(this.#text); // Rebuild the text map index.
  }

  // Return the text contents of the input.
  get text() {
    return this.#text;
  }


  // Paint the TextInput, with an optional `frame` for placement.
  // TODO: Provide a full frame along with an x, y position..
  paint($, clear = false, frame = $.screen) {
    this.pal = this.scheme[$.dark ? "dark" : "light"] || this.scheme;

    if (!clear && this.pal.bg !== undefined) $.ink(this.pal.bg).box(frame); // Paint bg.
    const ti = this;
    const prompt = this.#prompt;

    function paintBlockLetter(char, pos) {
      if (char.charCodeAt(0) === 10 && ti.#renderSpaces) {
        $.ink([255, 0, 0, 127]).box(pos.x, pos.y, 4);
      } else if (char !== " " && char.charCodeAt(0) !== 10) {
        const pic = ti.typeface.glyphs[char] || ti.typeface.glyphs["?"];
        $.ink(ti.pal.fg).draw(pic, pos, prompt.scale);
      } else if (ti.#renderSpaces) {
        $.ink([0, 255, 0, 127]).box(pos.x, pos.y, 3);
      }
    }

    // 🗺️ Render the text from the maps! (Can go both ways...)

    // A. Draw all text from displayToTextMap.
    Object.keys(prompt.cursorToTextMap).forEach((key) => {
      const [x, y] = key.split(":").map((c) => parseInt(c));
      const char = this.text[prompt.cursorToTextMap[key]];
      paintBlockLetter(char, prompt.pos({ x, y }));
    });

    // Or...
    // B. Draw all text from textToDisplayMap
    // prompt.textToCursorMap.forEach((pos, i) => {
    //   const char = this.text[i];
    //   paintBlockLetter(char, prompt.pos(pos));
    // });

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
      const center = $.geo.Box.from(prompt.pos()).center;
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
        $.ink(this.pal.block).box(prompt.pos()); // Draw blinking cursor.
        const char = this.text[this.#prompt.textPos()];
        const pic = this.typeface.glyphs[char];
        if (pic) $.ink(this.pal.blockHi).draw(pic, prompt.pos());
      }
    }

    if (this.cursor === "stop" && !this.canType) {
      const pos = prompt.pos();
      $.ink(255, 0, 0).box(pos.x + 1, pos.y + 3, 3);
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
      keys(this.typeface.glyphs).length === keys(this.typeface.glyphData).length
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

  // Run a command.
  async #execute(store) {
    // Make a history stack if one doesn't exist already.
    store[this.key] = store[this.key] || [];
    // Push input to a history stack, avoiding repeats.
    if (store[this.key][0] !== this.text) store[this.key].unshift(this.text);
    // console.log("📚 Stored prompt history:", store[key]);
    store.persist(this.key); // Persist the history stack across tabs.

    // 🍎 Process commands for a given context, passing the text input.
    if (this.#autolock) this.lock = true; // TODO: This might be redundant now. 23.06.07.23.32
    await this.processCommand?.(this.text);
    if (this.#autolock) this.lock = false;
  }

  // Clear the TextInput object and flip the cursor to ON.
  blank(cursor) {
    console.log("BLANK");
    if (cursor) this.cursor = cursor;
    this.text = "";
    this.#prompt.cursor = { x: 0, y: 0 };
    this.blink.flip(true);
  }

  // Set the UI state to be that of a completed reply.
  replied() {
    this.lock = false;
    this.runnable = false;
    this.inputStarted = false;
    this.canType = false;
    this.showButton("Enter");
  }

  // Handle user input.
  async act($) {
    const { event: e, store, needsPaint } = $;

    // Reflow the prompt on frame resize.
    if (e.is("reframed")) {
      this.#prompt.resize(floor($.screen.width / 6) - 2);
      this.flow();
      needsPaint();
    }

    // ✂️ Paste from user clipboard.
    if (e.is("pasted:text") && this.lock === false && this.canType) {
      const paste = e.text;
      const index = this.#prompt.textPos();

      // Just add the text to the end.
      if (index === undefined) {
        this.text += paste;
        this.#prompt.snapTo(this.text);
      } else {
        // Or inside.
        this.text = this.text.slice(0, index) + paste + this.text.slice(index);
        const newCursor = this.#prompt.textToCursorMap[index + paste.length];
        this.#prompt.cursor = { ...newCursor };
      }

      this.blink.flip(true);
    }

    // ⌨️ Add text via the keyboard.
    if (e.is("keyboard:down") && this.lock === false) {
      if (this.canType === false) {
        this.canType = true;
        this.text = "";
        this.inputStarted = true;
        this.editableCallback?.(this);
        this.editableCallback?.(this);
        this.#prompt.cursor = { x: 0, y: 0 };
      }

      if (e.key.length === 1 && e.ctrl === false && e.key !== "`") {
        // if (this.text === "" && e.key === " ") {
        //   this.blink.flip(true);
        //   return; // Skip opening spaces.
        // }

        // Printable keys with subbed punctuation.
        let insert = e.key.replace(/[“”]/g, '"').replace(/[‘’]/g, "'");

        let index = this.#prompt.textPos(); // TODO: This needs to present doubled up
        //                                           characters in the right order.

        const char = this.text[index];
        const newLine = char?.charCodeAt(0) === 10;

        const underCursor = index !== undefined;

        // If the cursor is in the corner but comes before any text.
        if (
          index === undefined &&
          this.#prompt.cursor.x === 0 &&
          this.#prompt.cursor.y === 0
        ) {
          index = 0;
          this.text = insert + this.text;
          this.#prompt.forward(this.#prompt.cursor, insert.length);
          this.blink.flip(true);
          this.showBlink = true;
          return;
        }

        if (newLine && underCursor) {
          // Double up the characters so that this new character exists
          // at the position of the current cursor.
          this.text =
            this.text.slice(0, index + 1) + insert + this.text.slice(index + 1);
          this.#prompt.forward();
          this.blink.flip(true);
          this.showBlink = true;
          return;
        }

        // Move backwards until we reach a character
        while (index === undefined) {
          index = this.#prompt.textPos(
            this.#prompt.backward({ ...this.#prompt.cursor })
          );
        }

        const sliceIndex = underCursor ? index : index + 1;

        // If we are on the first char of a wrapped word, and inserting spaces,
        // then add enough spaces through to the end of the line.
        if (this.#prompt.wrapped(sliceIndex) && insert === " ") {
          const lastCursor = this.#prompt.textToCursorMap[sliceIndex - 1];
          const thisCursor = this.#prompt.cursor;

          let spaces = 0;
          spaces = this.#prompt.colWidth - lastCursor.x + thisCursor.x;
          insert = " ".repeat(spaces - 1);
        }

        this.text =
          this.text.slice(0, sliceIndex) + insert + this.text.slice(sliceIndex);

        if (!underCursor || index === 0) {
          // Append at end of line.
          let skipForward = false;

          const newIndex = this.#prompt.textPos();
          const mapped = this.#prompt.textToCursorMap[newIndex];
          if (mapped) {
            this.#prompt.cursor = { ...mapped };
          } else {
            skipForward = true;
          }
          if (newIndex <= index && index > 0) {
            // We broke a line so jump ahead the difference.
            this.#prompt.forward(this.#prompt.cursor, index - newIndex + 2);
          } else if (!skipForward) this.#prompt.forward(); // Move forward a space.
        } else {
          let newCursor =
            this.#prompt.textToCursorMap[sliceIndex + insert.length];
          // Check for the skipped new line character.
          if (!newCursor)
            newCursor =
              this.#prompt.textToCursorMap[sliceIndex + insert.length + 1];
          if (newCursor) this.#prompt.cursor = { ...newCursor };
        }
      } else {
        // Other keys.
        if (e.key === "Delete") {
          // Delete the character under the cursor.
          const index = this.#prompt.textPos();
          if (index !== undefined) {
            if (
              !(
                this.text[index]?.charCodeAt(0) === 10 &&
                index === this.text.length - 1
              )
            ) {
              // quit
              this.text =
                this.text.slice(0, index) + this.text.slice(index + 1) || "";
            }
          } else {
            const back = this.#prompt.backward({ ...this.#prompt.cursor });
            const backIndex = this.#prompt.textPos(back);
            if (this.text.length - 1 !== backIndex) {
              // Do nothing, we are at the end.
              // If we are on the first character or if there is
              // text before the cursor.
              const bi = backIndex === undefined ? 0 : backIndex + 1;
              this.text =
                this.text.slice(0, bi) + this.text.slice(bi + 1) || "";
            }
          }
        } else if (e.key === "Backspace") {
          const prompt = this.#prompt;

          // Detect if the cursor is on a `\n` new line character.
          // It could potentially be on "two characters"...
          const currentTextIndex = prompt.textPos();
          const onNewline = this.text[currentTextIndex]?.charCodeAt(0) === 10;

          if (onNewline) {
            this.text =
              this.text.slice(0, currentTextIndex) +
              this.text.slice(currentTextIndex + 1);
            if (this.text.length === currentTextIndex) {
              prompt.snapTo(this.text);
            } else {
              prompt.crawlBackward();
              if (prompt.posHasVisibleCharacter()) prompt.forward();
            }
          } else {
            // Otherwise continue with a normal backspace action.

            // Move an invisible cursor back and retrieve the text index for it.
            const back = prompt.backward({ ...prompt.cursor });
            const key = `${back.x}:${back.y}`;

            const cursorTextIndex = prompt.cursorToTextMap[key];
            const hasNewLine =
              prompt.cursorToTextMap[key + ":\\n"] !== undefined;

            const currentPosition = prompt.textPos();
            if (prompt.wrapped(currentPosition)) {
              // Delete backwards from text position through any
              // spaces until the last visible character.

              let movablePosition = prompt.textPos();
              let char = this.text[movablePosition - 1];
              let len = 0;
              while (char === " ") {
                movablePosition -= 1;
                char = this.text[movablePosition - 1];
                len += 1;
              }

              // Remove `len` characters from the text.
              if (len > 0) {
                this.text =
                  this.text.slice(0, currentPosition - len) +
                  this.text.slice(currentPosition);
                prompt.cursor = {
                  ...prompt.textToCursorMap[currentPosition - len],
                };
              }

              this.blink.flip(true);
              this.showBlink = true;
              return;
            }

            if (currentTextIndex === 0) return; // Don't delete if on first character.

            // 🎁 Exception for moving backwards at the start of a word-wrapped line.
            if (cursorTextIndex === undefined && currentTextIndex > 0) {
              this.text =
                this.text.slice(0, currentTextIndex - 1) +
                this.text.slice(currentTextIndex);
              prompt.cursor = {
                ...prompt.textToCursorMap[currentTextIndex - 1],
              };
            }

            if (cursorTextIndex >= 0) {
              this.text =
                this.text.slice(0, cursorTextIndex) +
                this.text.slice(cursorTextIndex + 1);

              let cursor = prompt.textToCursorMap[cursorTextIndex - 1];

              if (!cursor) {
                if (prompt.posHasVisibleCharacter()) {
                  cursor = prompt.textToCursorMap[cursorTextIndex];
                } else {
                  cursor = { x: 0, y: 0 };
                }
              }

              if (cursor) {
                prompt.cursor = { ...cursor };
                if (cursorTextIndex > 0 && !hasNewLine) {
                  if (!prompt.wrapped(cursorTextIndex)) prompt.forward();
                }
              } else {
                prompt.crawlBackward();
              }
            }
          }
        }

        if (e.key === "Escape") {
          this.text = "";
          this.#prompt.cursor = { x: 0, y: 0 };
        }

        // Move backwards through history stack.
        if (e.key === "ArrowUp") {
          const history = (await store.retrieve(this.key)) || [""];
          this.text = history[this.historyDepth];
          this.#prompt.snapTo(this.text);
          this.historyDepth = (this.historyDepth + 1) % history.length;
        }

        // ... and forwards.
        if (e.key === "ArrowDown") {
          const history = (await store.retrieve(this.key)) || [""];
          this.text = history[this.historyDepth];
          this.#prompt.snapTo(this.text);
          this.historyDepth -= 1;
          if (this.historyDepth < 0) this.historyDepth = history.length - 1;
        }

        // Move cursor forward.
        if (e.key === "ArrowRight") this.#prompt.crawlForward();

        // Move cursor backward.
        if (e.key === "ArrowLeft") this.#prompt.crawlBackward();
      }

      if (e.key !== "Enter") {
        if (this.text.length > 0) {
          this.go.btn.disabled = false;
          this.go.txt = "Enter";
          this.runnable = true;
        } else {
          this.go.btn.disabled = true;
          this.runnable = false;
        }
      }

      if (e.key === "Enter") {
        if (e.shift) {
          // ✏️ Make a new line while editing.
          const pos = this.#prompt.textPos();
          const char = this.text[pos];
          if (
            pos === undefined ||
            (char?.charCodeAt(0) === 10 && pos === this.text.length - 1)
          ) {
            this.text += `\n`;
            this.#prompt.newLine();
            this.#prompt.snapTo(this.text);
          } else {
            const hasVis = this.#prompt.posHasVisibleCharacter();

            // Check to see if the cursor is at the start of a word breaked word
            // and if it is, then add another line.
            let insert = "\n";
            let wrapped = false;
            if (this.#prompt.wrapped(pos)) {
              wrapped = true;
              insert += "\n"; // Add an extra line if on a wrapped word.
            }
            this.text = this.text.slice(0, pos) + insert + this.text.slice(pos);

            if (hasVis && !wrapped) {
              this.#prompt.cursor = { ...this.#prompt.textToCursorMap[pos] };
            } else {
              this.#prompt.cursor.y += 1;
            }
          }
        } else if (this.runnable) {
          // 💻 Execute a command!
          await this.run(store);
        }
      }

      this.blink.flip(true);
      this.showBlink = true;
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
          needsPaint();
        },
        push: async () => {
          if (this.runnable) {
            await this.run(store);
          } else {
            this.go.btn.disabled = true;
            this.canType = true;
            this.blank("blink");
            this.inputStarted = true;
            this.editableCallback?.(this);
            needsPaint();
            $.send({ type: "keyboard:unlock" });
          }
        },
        cancel: () => {
          $.send({ type: "keyboard:lock" });
          needsPaint();
        },
        rollover: (btn) => {
          if (btn) $.send({ type: "keyboard:unlock" });
          needsPaint();
        },
        rollout: () => {
          $.send({ type: "keyboard:lock" });
          needsPaint();
        },
      });
    }

    if (e.is("touch") && e.device === "mouse" && !this.lock) {
      this.blink.flip(true);
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
        this.#prompt.crawlBackward();
      }

      while (this.#moveDeltaX >= this.#moveThreshold) {
        this.#moveDeltaX -= this.#moveThreshold;
        this.#prompt.crawlForward();
      }

      this.blink.flip(true);
    }
  }
}

// Manages the scale / wrapping of text and the interaction of a cursor.
// (Just for rendering of `Text`)
class Prompt {
  top = 0;
  left = 0;

  wrap = "char"; // auto-wrap setting, could also be "word".
  scale = 1;
  blockWidth = 6;
  blockHeight = 10;
  letterWidth = this.blockWidth * this.scale;
  letterHeight = this.blockHeight * this.scale;

  colWidth = 48; // Maximum character width of each line before wrapping.

  cursor = { x: 0, y: 0 };
  gutter; // A y-position at the end of the colWidth.

  lineBreaks = []; // Legacy?

  cursorToTextMap = {}; // Keep track of text data in relationship to whitespace.
  textToCursorMap = [];
  wrappedWordIndices = []; // Keep track of word wrapped indices after
  //                           each mapping.

  #mappedTo = ""; // Text that has been mapped.

  constructor(top = 0, left = 0, wrap, colWidth = 48) {
    this.top = top;
    this.left = left;
    this.wrap = wrap;
    this.colWidth = colWidth;
    this.gutter = this.colWidth * this.blockWidth;
  }

  // Snap the cursor to the end of a text.
  snapTo(text) {
    if (text[text.length - 1]) {
      this.cursor = { ...this.textToCursorMap[text.length - 1] };
      if (text[text.length - 1].charCodeAt(0) !== 10) this.forward(); // Move ahead one space after the end.
    } else {
      this.cursor = { x: 0, y: 0 };
    }
  }

  // Generate text map for rendering and UI operations.
  mapTo(text) {
    // Begin the cursor / text-wrapping crawl.
    this.#mappedTo = text;
    this.cursorToTextMap = {};
    this.textToCursorMap = [];
    this.wrappedWordIndices = [];
    const cursor = { x: 0, y: 0 };

    // Wrap and map the text either by character or word.
    // (Word wrapping is complex and skips text indices for invisible
    //  characters and in some edge cases with line breaks)
    if (this.wrap === "char") {
      let textIndex = 0;
      let brokeLine = false;

      for (let c = 0; c < text.length; c += 1) {
        const char = text[c];
        const newLine = char.charCodeAt(0) === 10;

        if (c === 0) {
          if (newLine) {
            this.newLine(cursor);
            brokeLine = true;
          }
          this.#updateMaps(text, textIndex, cursor); // Update cursor<->text indexing.
          continue;
        }

        if (newLine) {
          this.newLine(cursor);
          brokeLine = true;
        } else {
          !brokeLine ? this.forward(cursor) : (brokeLine = false);
        }
        textIndex += 1;
        this.#updateMaps(text, textIndex, cursor); // Update cursor<->text indexing.
      }
    } else if (this.wrap === "word") {
      let textIndex = 0;
      let brokeLine = false;
      let wordStart = false;
      let wordCount = 0;

      for (let c = 0; c < text.length; c += 1) {
        const char = text[c];
        const newLine = char.charCodeAt(0) === 10;

        // First character...
        if (c === 0) {
          if (newLine) {
            this.newLine(cursor);
            brokeLine = true;
          }
          this.#updateMaps(text, textIndex, cursor);
          if (!newLine && char !== " ") {
            wordStart = true;
            wordCount += 1;
          }
          continue;
        }

        if (!newLine && char !== " ") {
          if (!wordStart) {
            wordStart = true;
            wordCount += 1;

            let len = 0;
            for (let i = c; i < text.length; i += 1) {
              const char = text[i];
              if (char !== " " && char.charCodeAt(0) !== 10) {
                len += 1;
              } else {
                break;
              }
            }

            if (cursor.x + len >= this.colWidth - 1) {
              if (!this.posHasNewLine(cursor)) {
                this.newLine(cursor);
                brokeLine = true;
                this.wrappedWordIndices.push(c);
              }
            }
          }
        } else {
          wordStart = false;
        }

        if (newLine) {
          this.newLine(cursor);
          brokeLine = true;
        } else {
          !brokeLine ? this.forward(cursor) : (brokeLine = false);
        }
        textIndex += 1;
        this.#updateMaps(text, textIndex, cursor);
      }
    }
  }

  // Lookup to check if the word at the beginning of this index was
  // word-wrapped.
  wrapped(index) {
    if (this.wrap !== "word") return false; // Make sure word wrap is on.
    return this.wrappedWordIndices.includes(index);
  }

  #updateMaps(text, textIndex, cursor = this.cursor) {
    const char = text[textIndex];
    const newLine = char.charCodeAt(0) === 10;
    this.textToCursorMap[textIndex] = { ...cursor };
    let key = `${cursor.x}:${cursor.y}`;
    if (newLine) key = key + ":\\n";
    this.cursorToTextMap[key] = textIndex;
  }

  resize(newColWidth) {
    this.colWidth = newColWidth;
  }

  // Get the current text index given a cursor position.
  textPos(cursor = this.cursor) {
    if (this.textToCursorMap.length === 0) {
      return 0;
    } else {
      // First check for any preceeding new line characters...
      const key = `${cursor.x}:${cursor.y}`;
      let pos = this.cursorToTextMap[key];
      if (pos === undefined) pos = this.cursorToTextMap[key + ":\\n"];
      return pos;
    }
  }

  // Determine whether a cursor has a visible character in the map.
  posHasVisibleCharacter(cursor = this.cursor) {
    return this.cursorToTextMap[`${cursor.x}:${cursor.y}`] !== undefined;
  }

  // Determine whether there is an invisible new line character
  // under the cursor in the map.
  posHasNewLine(cursor = this.cursor) {
    return this.cursorToTextMap[`${cursor.x}:${cursor.y}:\\n`] !== undefined;
  }

  // Flatten the coordinates of the cursor to return a linear value.
  // (Does not necessarily match text, due to line breaks, etc.)
  get index() {
    const x = this.cursor.x;
    const y = this.cursor.y;
    const cols = this.colWidth;
    const lineBreaks = y; // Number of line breaks before the current row
    return y * (cols + 1) + x - lineBreaks;
  }

  // Caluclate the screen x, y position of the top left of the cursor.
  // (Also include the width and height of the block.)
  pos(cursor = this.cursor) {
    const x = this.top + cursor.x * this.letterWidth;
    const y = this.left + cursor.y * this.letterHeight;
    return { x, y, w: this.letterWidth, h: this.letterHeight };
  }

  // Move the cursor forward, optionally input an override cursor.
  forward(cursor = this.cursor, amount = 1) {
    repeat(amount, () => {
      cursor.x = (cursor.x + 1) % (this.colWidth - 1);
      if (cursor.x === 0) cursor.y += 1;
    });
    return cursor;
  }

  // Move the cursor forward only by the mapped text.
  crawlForward() {
    if (this.#mappedTo.length === 0) return;

    const back = this.backward({ ...this.cursor });
    const backIndex = this.textPos(back);
    const startIndex = this.textPos();

    if (
      backIndex === this.#mappedTo.length ||
      (startIndex === this.#mappedTo.length - 1 &&
        !this.posHasVisibleCharacter())
    ) {
      return; // We are at the end.
    }

    if (backIndex !== this.#mappedTo.length - 1) {
      // Check to see if the next character is a new line and
      // only move forward on the x if it is.
      if (
        this.#mappedTo[startIndex + 1]?.charCodeAt(0) === 10 &&
        this.posHasVisibleCharacter()
      ) {
        this.cursor.x += 1;
      } else {
        // Otherwise move forward and jump through potential word wrapping.
        this.forward();
        // Skip any undefined / wrapped sections.
        if (startIndex !== this.#mappedTo.length - 1) {
          while (this.textPos() === undefined) {
            this.forward();
          }
        }
      }
    } else if (startIndex === 0) this.forward();
  }

  // Move the cursor backward only by the mapped text.
  crawlBackward() {
    const back = this.backward({ ...this.cursor });
    let backIndex = this.textPos(back);
    const currentIndex = this.textPos();

    if (backIndex === undefined) {
      // Check for new line character.
      if (
        this.posHasNewLine() &&
        currentIndex <= 1 &&
        this.#mappedTo[currentIndex].charCodeAt(0) === 10
      ) {
        this.cursor.y -= 1;
        return;
      } else {
        if (this.posHasNewLine()) {
          const backupAmount = this.posHasVisibleCharacter() ? 2 : 1;
          this.cursor = {
            ...this.textToCursorMap[currentIndex - backupAmount],
          };
          if (this.posHasVisibleCharacter()) this.forward();
          return;
        } else {
          // Otherwise back up as needed.
          while (backIndex === undefined) {
            this.backward();
            backIndex = this.textPos(this.backward(back));
          }
        }
      }
    }
    this.backward();
  }

  // Move cursor backward, with optional override cursor.
  backward(cursor = this.cursor) {
    if (cursor.x === 0) {
      if (cursor.y > 0) {
        cursor.x = this.colWidth - 2;
        cursor.y -= 1;
      }
    } else {
      cursor.x -= 1;
    }
    return cursor;
  }

  // Create and track a cursor line break.
  newLine(cursor = this.cursor) {
    cursor.y += 1;
    cursor.x = 0;
  }
}

export { Typeface, TextInput, Prompt };
