// Type, 22.12.10.14.06
// Abstraction for typography and text input.

/* #region 🏁 TODO
  + Future
  - [] Gracefully allow for multiple instances of TextInput in a single piece? 
endregion */

import { font1 } from "../disks/common/fonts.mjs";
import { repeat } from "../lib/help.mjs";

const { floor, min } = Math;
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

  get blockWidth() {
    return this.glyphs.a.resolution[0];
  }

  async load($preload) {
    // 1. Ignore any keys with a "glyph" prefix because these are settings.
    const glyphsToLoad = entries(this.data).filter(
      ([g, loc]) => !g.startsWith("glyph"),
    );
    const promises = glyphsToLoad.map(([glyph, location], i) => {
      // 2. Load all other keys / glyphs over the network.
      return $preload(
        `aesthetic.computer/disks/drawings/${this.name}/${location}.json`,
      )
        .then((res) => {
          this.glyphs[glyph] = res;
        })
        .catch((err) => {
          console.error("Couldn't load typeface:", err);
        });
    });

    // Wait for all the promises to resolve before returning
    await Promise.all(promises);
    return this;
  }

  // 📓 tf.print
  print(
    $,
    pos = { x: undef, y: undef, size: 1, thickness: 1, rotation: 0 },
    lineNumber,
    text,
    bg = null,
  ) {
    // TODO: Pass printLine params through / make a state machine.
    const font = this.glyphs;
    const lineHeightGap = 2;
    const size = pos.size || 1;
    const blockHeight = (this.data.glyphHeight || 9) * size + lineHeightGap;
    const blockWidth = 6;
    const thickness = pos.thickness || 1;
    const rotation = pos.rotation || 0;
    const fullWidth = blockWidth * size * text.length;

    if (Array.isArray(pos)) {
      pos = { x: pos[0], y: pos[1] };
    }

    const width = $.screen.width; // $.system?.world?.size
    // ? $.system.world.size.width
    // : $.screen.width;
    const height = $.screen.height; // $.system?.world?.size
    // ? $.system.world.size.height
    // : $.screen.height;

    const w = text.length * blockWidth * size;

    // Randomize pos.x and pos.y if undefined.
    if (pos.center === undefined) {
      if (pos.right !== undefined) {
        pos.x = width - w - pos.right;
      } else if (pos.left !== undefined) {
        pos.x = pos.left;
      } else if (pos.x === undefined) {
        pos.x = $.num.randIntRange(-fullWidth / 2, width + fullWidth / 2);
      }

      if (pos.bottom !== undefined) {
        pos.y = height - blockHeight - pos.bottom;
      } else if (pos.top !== undefined) {
        pos.y = pos.top;
      } else if (pos.y === undefined) {
        pos.y = $.num.randIntRange(-blockHeight / 2, height + blockHeight / 2);
      }
    }

    // Set x, y position and override if centering is specified.
    let x = pos.x || 0,
      y = pos.y || 0;

    pos.center = pos.center || "";


    if (pos.center.includes("x")) {
      const hw = w / 2;
      x = pos.x === undef ? width / 2 - hw : x - hw;
    }

    if (pos.center.includes("y")) {
      const hh = Math.floor(blockHeight / 2);
      y = pos.y === undef ? height / 2 - hh : y - hh;
    }

    y += lineNumber * blockHeight;

    // Only print the line if it will be visible on screen now...
    // Deprecated because it's incompatible with pan.
    // TODO: Eventually this could be added but it
    //       needs to take into account the current panTranslation in graph.
    // 📔 Or culling should happen further down the line?

    // if (
    //   y < -blockHeight * size ||
    //   y > $.screen.height ||
    //   x > $.screen.width ||
    //   x < -w
    // ) {
    //   // Offscreen render.
    //   return;
    // }

    const rn = $.inkrn(); // Remember the current ink color.

    // Background
    if (bg !== null) $.ink(bg).box(x, y, fullWidth, blockHeight);

    $.ink(rn).printLine(
      text,
      font,
      x,
      y,
      blockWidth,
      size,
      0,
      thickness,
      rotation,
    ); // Text
  }
}

// An interactive text prompt object.
class TextInput {
  $; // a reference to the api.
  #text; // text content
  #lastPrintedText = ""; // a place to cache a previous reply.
  #lastUserText = ""; // cache the user's in-progress edited text.
  submittedText = ""; // cache the user's submitted text.

  mute = false; // Whether to prevent sounds from playing.

  shifting = false; // Whether we are emoving the cursor or not.

  #renderSpaces = false; // Whether to render invisible space characters. " "
  //                        For debugging purposes.

  blink; // block cursor blink timer
  showBlink = true;
  cursor = "blink";

  // Buttons
  enter; // A button for replying or inputting text.
  copy; // A button for copying to the clipboard, that shows up conditionally.
  paste; // Similar to copy.

  canType = false;

  //#autolock = true;
  #lock = false;
  #lockTimeout;
  #showSpinner = false;

  #prompt;

  hideGutter = false;
  #gutterMax;
  #activatingPress = false;

  typeface;
  pal; // color palette
  scheme;

  #processCommand; // text processing callback
  // #processingCommand = false;
  historyDepth = -1;
  #prehistory;

  //inputStarted = false; // Flipped when the TextInput is first activated.
  //                       (To clear any starting text.)
  #moveThreshold = 6; // Drag threshold.
  #moveDeltaX = 0;

  runnable = false; // Whether a command can be tried.
  didReset; // Callback for blank reset.

  key;

  copiedCallback; // When the "Copy" button is pressed, for designing wrappers.

  #copyPasteTimeout; // UI Timer for clipboard copy response.
  #copyPasteScheme; // An override for the Copy button's color.

  #coatedCopy; // Stores a version of the current text output that could be
  //              decorated. (With a URL, for example.)

  activate; // Hook to `activate` inside of act.
  activated; // Optional callback for when the the text input becomes
  //            activated via pushing the Enter button or typing a key.
  activatedOnce = false;
  backdropTouchOff = false; // Determines whether to activate the input
  //                           after tapping the backdrop.
  commandSentOnce = false; // 🏴

  closeOnEmptyEnter = false;

  // Add support for loading from preloaded system typeface.
  constructor(
    $,
    text = "",
    processCommand,
    options = {
      palette: undefined,
      font: font1,
      //autolock: true,
      wrap: "char",
    },
  ) {
    this.key = `${$.slug}:history`; // This is "per-piece" and should
    //                                 be per TextInput object...23.05.23.12.50

    this.$ = $;

    this.closeOnEmptyEnter = options.closeOnEmptyEnter || false;
    this.hideGutter = options.hideGutter || false;
    // ^ Close keyboard on empty entry.

    this.copiedCallback = options.copied;

    // Load typeface, preventing double loading of the system default.
    if (!options.font) options.font = font1; // Use preloaded font as needed.
    if ($.typeface?.data !== options.font) {
      this.typeface = new Typeface(options.font); // Load custom typeface.
      this.typeface.load($.net.preload);
    } else {
      this.typeface = $.typeface; // Set to system typeface.
    }

    this.activated = options.activated;
    //this.#autolock = options.autolock;
    this.didReset = options.didReset;

    const blockWidth = 6;
    this.#gutterMax = options.gutterMax || 48;

    this.#prompt = new Prompt(
      blockWidth,
      blockWidth,
      options.wrap || "char", // "char" or "word"
      $.store["gutter:lock"] ||
        Math.min(this.#gutterMax, floor($.screen.width / blockWidth) - 2),
      options.lineSpacing,
    );

    this.print(text); // Set initial text.

    this.startingInput = this.text;
    this.scheme = options.scheme || {
      dark: {
        text: 255,
        background: 0,
        block: 255,
        highlight: 0,
        guideline: 255,
      },
      light: {
        text: 0,
        background: 255,
        block: 0,
        highlight: 255,
        guideline: 0,
      },
    };

    const {
      ui: { TextButton: TB },
    } = $;

    this.enter = new TB(this.scheme.buttons?.enter || "Enter");
    this.copy = new TB(this.scheme.buttons?.copy.label || "Copy");
    this.paste = new TB(this.scheme.buttons?.paste?.label || "Paste");
    this.copy.btn.disabled = true; // Copy is disabled by default,
    this.paste.btn.disabled = true; // as is Paste.

    if (this.text.length === 0) {
      this.enter.btn.disabled = true;
    }

    this.#processCommand = processCommand;
    $.send({ type: "keyboard:enabled" });
  }

  // Stretches the gutter to be the screen width minus two slots.
  fullGutter($) {
    this.gutter = Math.min(
      this.#gutterMax,
      floor($.screen.width / this.#prompt.blockWidth) - 2,
    );
  }

  set lock(bool) {
    this.#lock = bool;
    if (bool) {
      this.#lockTimeout = setTimeout(() => {
        this.#showSpinner = true;
      }, 100);
    } else {
      clearTimeout(this.#lockTimeout);
      this.#showSpinner = false;
    }
  }

  get lock() {
    return this.#lock;
  }

  // Adjust the gutter width for text wrapping.
  set gutter(n) {
    this.#prompt.colWidth = n;
    this.#prompt.gutter = this.#prompt.colWidth * this.#prompt.blockWidth;
  }

  // Alias for the setter above, returned in columns.
  get columns() {
    return this.#prompt.colWidth;
  }

  // Reset the user text after a message is complete.
  clearUserText() {
    this.#lastUserText = "";
  }

  addUserText(txt) {
    this.#lastUserText = txt;
  }

  // Snap cursor to the end of text.
  snap() {
    this.#prompt.snapTo(this.text);
    this.$.send({
      type: "keyboard:cursor",
      content: { cursor: this.#text.length },
    });
  }

  // Run a command
  async run(store) {
    this.snap();
    this.submittedText = "";
    await this.#execute(store); // Send a command.
  }

  // Set the text and reflow it.
  set text(str) {
    if (str === this.#text) {
      // console.warn("Redundant text");
      return;
    }
    this.#text = str;
    this.flow();
    // console.log("📝 Setting text to:", str);
  }

  // Return the prompt.
  get prompt() {
    return this.#prompt;
  }

  print(text) {
    this.text = text;
    this.bakePrintedText();
  }

  bakePrintedText() {
    this.#lastPrintedText = this.text;
  }

  latentFirstPrint(text) {
    if (!this.activatedOnce) this.text = text;
  }

  // Reflow the input text.
  flow() {
    this.#prompt.mapTo(this.text); // Rebuild the text map index.
  }

  // Return the text contents of the input.
  get text() {
    return this.#text;
  }

  #coatCopy(text) {
    return this.copiedCallback?.(text) || text;
  }

  // Paint the TextInput, with an optional `frame` for placement.
  paint(
    $,
    clear = false,
    frame = { x: 0, y: 0, width: $.screen.width, height: $.screen.height },
  ) {
    this.pal =
      this.scheme[$.dark ? "dark" : "light"] ||
      this.scheme["dark"] ||
      this.scheme;

    if (!clear && this.pal.background !== undefined)
      $.ink(this.pal.background).box(frame); // Paint bg.
    const ti = this;
    const prompt = this.#prompt;

    function paintBlockLetter(char, pos, alt = false) {
      if (char.charCodeAt(0) === 10 && ti.#renderSpaces) {
        $.ink([255, 0, 0, 127]).box(pos.x, pos.y, 4);
      } else if (char !== " " && char.charCodeAt(0) !== 10) {
        const pic = ti.typeface.glyphs[char] || ti.typeface.glyphs["?"];
        $.ink(!alt ? ti.pal.text : ti.pal.prompt || ti.pal.text).draw(
          pic,
          pos,
          prompt.scale,
        ); // Draw each character.
      } else if (ti.#renderSpaces) {
        $.ink([0, 255, 0, 127]).box(pos.x, pos.y, 3);
      }
    }

    // 🗺️ Render the text from the maps! (Can go both ways...)
    if (frame.x || frame.y) $.pan(frame.x, frame.y);

    if (!this.#lock && this.selection && this.canType) {
      for (let i = this.selection[0]; i < this.selection[1]; i += 1) {
        const c = prompt.textToCursorMap[i];
        const p = prompt.pos(c, true);
        $.ink(this.pal.selection || [255, 255, 0, 64]).box(p);
      }
    }

    // A. Draw all text from displayToTextMap.
    let submittedIndex = 0;
    Object.keys(prompt.cursorToTextMap).forEach((key) => {
      const [x, y] = key.split(":").map((c) => parseInt(c));
      const char = this.text[prompt.cursorToTextMap[key]];
      let fromSubmitted = false;
      if (!this.canType && submittedIndex < this.submittedText.length) {
        if (char === this.submittedText[submittedIndex]) fromSubmitted = true;
        submittedIndex += 1;
      }

      // console.log(this.submittedText, this.text)
      // if (this.canType || !this.commandSentOnce || (this.lock && this.text === this.submittedText))
      //   fromSubmitted = true;
      paintBlockLetter(char, prompt.pos({ x, y }), fromSubmitted);
    });

    // Or...
    // B. Draw all text from textToDisplayMap
    //    TODO: Include the submitted text index. 23.07.28.15.47
    // prompt.textToCursorMap.forEach((pos, i) => {
    //   const char = this.text[i];
    //   paintBlockLetter(char, prompt.pos(pos));
    // });

    if (this.canType) {
      if (!this.hideGutter) {
        $.ink(this.pal.guideline).line(
          prompt.gutter,
          0,
          prompt.gutter,
          frame.height - 1,
        ); // Ruler
      }
      $.ink($.dark ? 127 : "teal").box(
        0,
        0,
        frame.width,
        frame.height,
        "inline",
      ); // Focus
    }

    if (this.#lock) {
      if (this.#showSpinner) {
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
      }
    } else {
      if (this.cursor === "blink" && this.showBlink && this.canType) {
        $.ink(this.pal.block).box(prompt.pos(undefined, true)); // Draw blinking cursor.
        const char = this.text[this.#prompt.textPos()];
        const pic = this.typeface.glyphs[char];
        if (pic)
          $.ink(this.pal.highlight).draw(pic, prompt.pos(undefined, true));
      }
    }

    if (this.cursor === "stop" && !this.canType) {
      const pos = prompt.pos();
      $.ink(255, 0, 0).box(pos.x + 1, pos.y + 3, 3);
    }

    // Build custom color schemes for the `ui.TextButton`s if they were defined.
    let btnScheme, btnHvrScheme;
    const pal = this.pal;
    if (pal.btn && pal.btnTxt)
      btnScheme = [pal.btn, pal.btnTxt, pal.btnTxt, pal.btn];
    if (pal.btnHvr && pal.btnHvrTxt)
      btnHvrScheme = [pal.btnHvr, pal.btnHvrTxt, pal.btnHvrTxt, pal.btnHvr];

    // Enter Button
    if (!this.enter.btn.disabled) {
      // Outline the whole screen.
      if (this.#activatingPress) {
        const color =
          pal.focusOutline ||
          (Array.isArray(pal.text)
            ? [...pal.text.slice(0, 3), 128]
            : [255, 0, 200, 64]);

        // Highlight outline.
        $.ink(color).box(0, 0, frame.width, frame.height, "inline");
      }
    }

    if (frame.x || frame.y) $.unpan();

    if (!this.enter.btn.disabled) {
      this.enter.reposition({ right: 6, bottom: 6, screen: frame });
      $.layer(2);
      this.enter.paint($, btnScheme, btnHvrScheme);
      $.layer(1);
    }

    // Copy Button
    if (!this.copy.btn.disabled) {
      this.copy.reposition({ left: 6, bottom: 6, screen: frame });
      this.copy.btn.publishToDom($, "copy", this.#coatedCopy);
      $.layer(2);
      this.copy.paint(
        { ink: $.ink },
        this.#copyPasteScheme || btnScheme,
        btnHvrScheme,
      );
      $.layer(1);
    }

    // Paste Button
    if (!this.paste.btn.disabled) {
      this.paste.reposition({ left: 6, bottom: 6, screen: frame });
      this.paste.btn.publishToDom($, "paste");
      $.layer(2);
      this.paste.paint(
        { ink: $.ink },
        this.#copyPasteScheme || btnScheme,
        btnHvrScheme,
      );
      $.layer(1);
    }

    // Return false if we have loaded every glyph.
    // (Can be wired up to the return value of the parent's `paint`)
    return !(
      keys(this.typeface.glyphs).length === keys(this.typeface.glyphData).length
    );
  }

  // Simulate anything necessary.
  sim({ seconds, needsPaint, gizmo: { Hourglass } }) {
    this.blink =
      this.blink ||
      new Hourglass(seconds(0.75), {
        flipped: (count, showBlinkOverride) => {
          if (showBlinkOverride !== undefined)
            this.showBlink = showBlinkOverride;
          else this.showBlink = !this.showBlink;
          needsPaint();
        },
        autoFlip: true,
      });

    if (this.#lock) needsPaint();
    if (this.canType) this.blink.step();
  }

  showButton($, { nocopy, nopaste } = { nocopy: false, nopaste: false }) {
    this.enter.btn.disabled = false;
    if (!nocopy && this.text.length > 0) {
      this.#coatedCopy = this.#coatCopy(this.text); // Wrap text to be copied.
      this.copy.btn.disabled = false;
      this.paste.btn.disabled = true; // Disable paste button.
      this.paste.btn.removeFromDom($, "paste");
    } else if (nopaste) {
      // Force turning off the paste button.
      this.paste.btn.disabled = true; // Disable paste button.
      this.paste.btn.removeFromDom($, "paste");
    } else {
      this.paste.btn.disabled = false; // Enable paste button.
    }
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
    await this.#processCommand?.(this.text);
    this.commandSentOnce = true;
  }

  // Clear the TextInput object and flip the cursor to ON.
  blank(cursor) {
    if (cursor) this.cursor = cursor;
    this.text = "";
    this.#prompt.cursor = { x: 0, y: 0 };
    this.blink?.flip(true);
    this.$.send({ type: "keyboard:text:replace", content: { text: "" } });
  }

  // Set the UI state to be that of a completed reply.
  replied($) {
    this.runnable = false;
    //this.inputStarted = false;
    this.canType = false;
    this.clearUserText();
    this.showButton($);
  }

  #buildCopyPasteScheme() {
    // Use default or set custom scheme for inactive button reply.
    let scheme = [64, 127, 127, 64];
    if (this.pal.btnReply && this.pal.btnReplyTxt)
      scheme = [
        this.pal.btnReply,
        this.pal.btnReplyTxt,
        this.pal.btnReplyTxt,
        this.pal.btnReply,
      ];
    return scheme;
  }

  // Handle user input.
  async act($) {
    const { debug, event: e, store, needsPaint, sound } = $;

    // Reflow the prompt on frame resize.
    if (e.is("reframed")) {
      if (!$.store["gutter:lock"]) this.fullGutter($);
      this.flow();
      needsPaint();
    }

    // ⌨️ Add text via the keyboard.
    if (e.is("keyboard:down") && this.#lock === false && !this.enter.btn.down) {
      // 🔡 Inserting an individual character.
      if (e.key.length === 1 && e.ctrl === false && e.key !== "`") {
        // if (this.text === "" && e.key === " ") {
        //   this.blink.flip(true);
        //   return; // Skip opening spaces.
        // }

        // Printable keys with subbed punctuation.
        let insert = e.key.replace(/[“”]/g, '"').replace(/[‘’]/g, "'");
        let index = this.#prompt.textPos();

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
            this.#prompt.backward({ ...this.#prompt.cursor }),
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
        if (e.key === "Backspace") {
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
              this.snap();
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
          this.activate(this);
          $.send({ type: "keyboard:open" });
          this.text = "";
          $.send({ type: "keyboard:text:replace", content: { text: "" } });
          this.#prompt.cursor = { x: 0, y: 0 };
        }

        // Move backwards through history stack.
        if (e.key === "ArrowUp") {
          // TODO: Check to see if this is the first history traversal,
          //       and store the current text if it is...
          const history = (await store.retrieve(this.key)) || [""];
          if (this.#prehistory === undefined) this.#prehistory = this.text;

          this.historyDepth += 1;
          if (this.historyDepth === history.length) {
            this.historyDepth = -1;
          }

          if (this.historyDepth === -1) {
            this.text = this.#prehistory;
          } else {
            this.text = history[this.historyDepth] || "";
          }

          this.snap();

          $.send({
            type: "keyboard:text:replace",
            content: { text: this.text },
          });
          this.selection = null;
        }

        // ... and forwards.
        if (e.key === "ArrowDown") {
          const history = (await store.retrieve(this.key)) || [""];
          if (this.#prehistory === undefined) this.#prehistory = this.text;

          this.historyDepth -= 1;
          if (this.historyDepth < -1) this.historyDepth = history.length - 1;

          if (this.historyDepth === -1) {
            this.text = this.#prehistory;
          } else {
            this.text = history[this.historyDepth] || "";
          }

          this.snap();
          $.send({
            type: "keyboard:text:replace",
            content: { text: this.text },
          });
          this.selection = null;
        }
      }

      if (e.key !== "Enter" && e.key !== "`") {
        if (this.text.length > 0) {
          this.enter.btn.disabled = false;
          this.runnable = true;
        } else {
          this.enter.btn.disabled = true;
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
            this.snap();
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
          $.send({
            type: "keyboard:text:replace",
            content: { text: this.text /*cursor: this.#prompt.textPos()*/ },
          });
        } else if (this.runnable) {
          if (this.text.trim().length > 0) {
            // 💻 Execute a command!
            if (!this.mute) {
              sound.synth({
                type: "sine",
                tone: 850,
                attack: 0.1,
                decay: 0.96,
                volume: 0.65,
                duration: 0.005,
              });
            }
            await this.run(store);
          }
        } else if (!this.canType) {
          activate(this);
        }
      }

      this.blink.flip(true);
      this.showBlink = true;
    }

    // Handle activation / focusing of the input
    // (including os-level software keyboard overlays)
    if (e.is("keyboard:open") && !this.#lock) activate(this);
    if (e.is("keyboard:close") && !this.#lock) {
      deactivate(this);
    }

    if (
      e.is("touch") &&
      !this.#lock &&
      //!this.inputStarted &&
      !this.canType &&
      !this.backdropTouchOff &&
      (this.copy.btn.disabled === true || !this.copy.btn.box.contains(e)) &&
      (this.paste.btn.disabled === true || !this.paste.btn.box.contains(e))
    ) {
      this.#activatingPress = true;
      $.send({ type: "keyboard:unlock" });
      if (!this.mute) {
        sound.synth({
          type: "sine",
          tone: 400,
          attack: 0.1,
          decay: 0.96,
          volume: 0.5,
          duration: 0.01,
        });
      }
    }

    // Begin the prompt input mode / leave the splash.
    function activate(ti) {
      // console.log("😃 Activating TextInput...");
      ti.activatedOnce = true;

      if (ti.canType) {
        console.log("✔️✍️ TextInput already activated.");
        // (This redundancy check is because this behavior is tied to
        // keyboard open and close events.)
        return;
      }

      ti.activated?.($, true);
      ti.#activatingPress = false;
      // ti.enter.btn.down = false;
      if (ti.text.length > 0) {
        ti.copy.btn.disabled = true;
        ti.copy.btn.removeFromDom($, "copy");
      }
      ti.canType = true;

      if (ti.#lastUserText.length > 0) {
        ti.text = ti.#lastUserText;
        ti.runnable = true;
        ti.paste.btn.disabled = false;
      } else {
        if (ti.#lastPrintedText) ti.blank("blink");

        // ti.enter.btn.disabled = true;
        ti.paste.btn.disabled = false;
      }
      //ti.inputStarted = true;
      $.act("text-input:editable");

      if (!ti.mute) {
        sound.synth({
          type: "sine",
          tone: 600,
          attack: 0.1,
          decay: 0.96,
          volume: 0.5,
          duration: 0.005,
        });
      }
    }

    this.activate = activate;

    // Leave the prompt input mode.
    function deactivate(ti) {
      // console.log("🙁 Deactivating TextInput...");

      if (ti.canType === false) {
        // Assume we are already deactivated.
        if (debug) console.log("❌✍️ TextInput already deactivated.");
        // (This redundancy check is because this behavior is tied to
        // keyboard open and close events.)
        return;
      }

      ti.activated?.($, false);

      ti.enter.btn.disabled = false;
      ti.paste.btn.disabled = false;
      ti.canType = false;
      ti.runnable = false;
      ti.#lastUserText = ti.text;
      ti.backdropTouchOff = false;

      ti.text = ti.#lastPrintedText || ti.text;

      if (ti.#lastPrintedText.length > 0 && ti.commandSentOnce) {
        ti.copy.btn.disabled = false;
        ti.#coatedCopy = ti.#coatCopy(ti.text); // Wrap text to be copied.

        ti.paste.btn.disabled = true; // Disable paste button.
        ti.paste.btn.removeFromDom($, "paste");
      }

      $.act("text-input:uneditable");
      needsPaint();

      if (!ti.mute) {
        sound.synth({
          type: "sine",
          tone: 250,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.001,
        });
      }

      ti.mute = false; // Always unmute on deactivation,
      //                  for `field`. 23.12.02.00.43
    }

    if (
      e.is("touch") &&
      ((this.enter.btn.disabled === false && this.enter.btn.box.contains(e)) ||
        (this.copy.btn.disabled === false && this.copy.btn.box.contains(e)) ||
        (this.paste.btn.disabled === false && this.paste.btn.box.contains(e)))
    ) {
      this.backdropTouchOff = true;
    }

    if (e.is("lift")) this.backdropTouchOff = false;

    // UI Button Actions
    if (!this.#lock) {
      // Enter Button...
      if (
        e.is("draw") &&
        this.enter.btn.disabled === false &&
        this.enter.btn.box.contains(e) &&
        !this.enter.btn.down
      ) {
        $.send({ type: "keyboard:lock" });
      }

      // Copy Button...
      if (
        (e.is("draw") || e.is("touch")) &&
        this.copy.btn.disabled === false &&
        this.copy.btn.box.contains(e) // &&
      ) {
        $.send({ type: "keyboard:lock" });
      }

      // Paste button...
      if (
        (e.is("draw") || e.is("touch")) &&
        this.paste.btn.disabled === false &&
        this.paste.btn.box.contains(e) // &&
      ) {
        $.send({ type: "keyboard:lock" });
      }

      // 🔲 Enter
      this.enter.btn.act(e, {
        down: () => {
          needsPaint();
        },
        push: async () => {
          if (this.runnable) {
            if (this.text.trim().length > 0) {
              await this.run(store);
            } else if (this.closeOnEmptyEnter) {
              $.send({ type: "keyboard:close" });
            }
          } else {
            activate(this);
            if (this.runnable && this.text.trim().length > 0) {
              await this.run(store);
            } else if (this.closeOnEmptyEnter) {
              $.send({ type: "keyboard:close" });
            }
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

      // 🔲 Copy
      this.copy.btn.act(e, {
        down: () => {
          needsPaint();
          if (!this.mute) {
            sound.synth({
              type: "sine",
              tone: 600,
              attack: 0.1,
              decay: 0.99,
              volume: 0.75,
              duration: 0.001,
            });
          }
        },
        push: () => {
          if (!this.mute) {
            sound.synth({
              type: "sine",
              tone: 800,
              attack: 0.1,
              decay: 0.99,
              volume: 0.75,
              duration: 0.005,
            });
          }
          needsPaint();
        },
        cancel: () => needsPaint(),
      });

      // 🔲 Paste
      this.paste.btn.act(e, {
        down: () => {
          needsPaint();
          if (!this.mute) {
            sound.synth({
              type: "sine",
              tone: 600,
              attack: 0.1,
              decay: 0.99,
              volume: 0.75,
              duration: 0.001,
            });
          }
        },
        push: () => {
          if (!this.mute) {
            sound.synth({
              type: "sine",
              tone: 800,
              attack: 0.1,
              decay: 0.99,
              volume: 0.75,
              duration: 0.005,
            });
          }
          needsPaint();
        },
        cancel: () => needsPaint(),
      });
    }

    // ✂️ Copy to a user's clipboard.
    if (e.name?.startsWith("clipboard:copy")) {
      const copied = e.is("clipboard:copy:copied");
      if (debug) {
        copied
          ? console.log("📋 Copy: Copied 🙃")
          : console.warn("📋 Copy: Failed ⚠️");
      }

      this.copy.txt = copied
        ? this.scheme.buttons?.copy?.copied || "Copied"
        : this.scheme.buttons?.copy?.failed || "Failed";
      this.#copyPasteScheme = this.#buildCopyPasteScheme(); // Greyed out.

      needsPaint();
      clearTimeout(this.#copyPasteTimeout);
      this.#copyPasteTimeout = setTimeout(() => {
        this.copy.btn.disabled = false;
        this.copy.txt = this.scheme.buttons?.copy?.label || "Copy";
        this.#copyPasteScheme = undefined;
        needsPaint();
      }, 500);
    }

    // 🗞️ Paste UI signal.
    if (e.name?.startsWith("clipboard:paste")) {
      let label;
      if (e.is("clipboard:paste:pasted")) {
        if (debug) console.log("📋 Paste: Pasted 🙃");
        label = this.scheme.buttons?.paste?.pasted || "Pasted";
      } else if (e.is("clipboard:paste:pasted:empty")) {
        if (debug) console.warn("📋 Paste: Empty 👐️");
        label = this.scheme.buttons?.paste?.empty || "Empty";
      } else {
        if (debug) console.warn("📋 Paste: Failed ⚠️");
        label = this.scheme.buttons?.paste?.failed || "Failed";
      }

      this.paste.txt = label;
      this.#copyPasteScheme = this.#buildCopyPasteScheme(); // Greyed out.
      needsPaint();
      clearTimeout(this.#copyPasteTimeout);
      this.#copyPasteTimeout = setTimeout(() => {
        this.paste.btn.disabled = false;
        this.paste.txt = this.scheme.buttons?.paste?.label || "Paste";
        this.#copyPasteScheme = undefined;
        needsPaint();
      }, 500);
    }

    // This should only be possible when the text box is locked, unless
    // it's the first activation.
    if (
      e.is("prompt:text:replace") &&
      (!this.activatedOnce || this.#lock === false)
    ) {
      this.text = e.text;
      this.#lastUserText = e.text;
      this.#prompt.snapTo(this.text.slice(0, e.cursor));
      // this.runnable = true;
      this.blink.flip(true);
      this.selection = null;

      if (this.text.length > 0) {
        this.enter.btn.disabled = false;
        this.runnable = true;
      } else {
        this.enter.btn.disabled = true;
        this.runnable = false;
      }

      if (this.#prehistory !== undefined) this.#prehistory = this.text;
    }

    if (e.is("prompt:text:cursor") && this.#lock === false) {
      if (e.cursor === this.text.length) {
        // this.snap();
        this.#prompt.snapTo(this.text);
      } else {
        this.#prompt.cursor = { ...this.#prompt.textToCursorMap[e.cursor] };
      }

      if (e.start !== undefined && e.end !== undefined) {
        this.selection = [e.start, e.end];
      } else {
        this.selection = null;
      }

      this.blink.flip(true);
    }

    if (e.is("touch") && !this.#lock) this.blink.flip(true);

    if (e.is("lift") && !this.#lock) {
      if (this.shifting) {
        this.moveDeltaX = 0;
        this.shifting = false;
      }
      $.send({ type: "keyboard:unlock" });
    }

    if (
      e.is("draw") &&
      !this.#lock &&
      this.canType &&
      !this.enter.btn.down &&
      !this.paste.btn.down
    ) {
      if (!this.shifting) {
        $.send({ type: "keyboard:lock" });

        this.shifting = true;
        this.backdropTouchOff = true;
      }

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
        this.selection = null;
        $.send({ type: "keyboard:cursor", content: -1 });
      }

      while (this.#moveDeltaX >= this.#moveThreshold) {
        this.#moveDeltaX -= this.#moveThreshold;
        this.#prompt.crawlForward();
        this.selection = null;
        if (this.prompt.textPos() === undefined) {
          $.act("textinput:shift-right:empty"); // Send a signal when we shift
          // to the right past the edge of the text, for autocomplete
          // implementations in pieces that use `TextInput`.
        }
        $.send({ type: "keyboard:cursor", content: 1 });
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

  colWidth = 48; // Maximum character width of each line before wrapping.

  cursor = { x: 0, y: 0 };
  gutter; // A y-position at the end of the colWidth.

  lineBreaks = []; // Legacy?

  cursorToTextMap = {}; // Keep track of text data in relationship to whitespace.
  textToCursorMap = [];
  wrappedWordIndices = []; // Keep track of word wrapped indices after
  //                           each mapping.

  #mappedTo = ""; // Text that has been mapped.

  constructor(top = 0, left = 0, wrap, colWidth = 48, lineSpacing = 0) {
    this.letterWidth = this.blockWidth * this.scale;
    this.letterHeight = this.blockHeight * this.scale + lineSpacing;
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

      for (let c = 0; c < text?.length; c += 1) {
        const char = text[c];
        let newLine = char.charCodeAt(0) === 10;

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

        //if (!newLine && char !== " ") {
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

        // Create a line break if a line will begin with a space and we're
        // not on a space.
        if (
          char === " " &&
          cursor.x + 1 === this.colWidth - 1 &&
          text[textIndex] !== " "
        ) {
          newLine = true;
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
  pos(cursor = this.cursor, bh = false) {
    const x = this.top + cursor.x * this.letterWidth;
    const y = this.left + cursor.y * this.letterHeight;
    return {
      x,
      y,
      w: this.letterWidth,
      h: bh ? this.blockHeight : this.letterHeight,
    };
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
