// Type, 22.12.10.14.06
// Abstraction for typography and text input.

/* #region 🏁 TODO
  + Future
  - [] Gracefully allow for multiple instances of TextInput in a single piece? 
endregion */

import * as fonts from "../disks/common/fonts.mjs";
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

  constructor(name = "font_1") {
    this.name = name;
    this.data = fonts[name] || fonts.font_1;
  }

  // Return only the character index from the data.
  get glyphData() {
    const glyphsOnly = { ...this.data };
    // TODO: Remove other "glyph" prefixes here if they ever exist. 23.06.07.01.10
    delete glyphsOnly.glyphHeight;
    delete glyphsOnly.glyphWidth;
    return glyphsOnly;
  }

  get blockWidth() {
    return this.data.glyphWidth;
  }

  get blockHeight() {
    return this.data.glyphHeight;
  }

  async load($preload, needsPaintCallback) {
    // TODO: Add support for on-demand character loading here using this api that
    //       gets the json for the glyphs: https://localhost:8888/api/bdf-glyph?char=h
    if (this.name === "font_1") {
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
      }); // Wait for all the promises to resolve before returning
      await Promise.all(promises);
    } else if (this.name === "unifont") {
      // 🗺️ UNIFONT Homepage: https://unifoundry.com/unifont.html
      // Add a basic placeholder glyph for "?" character

      this.glyphs["?"] = {
        resolution: [6, 9],
        pixels: [
          [0, 1, 1, 1, 0],
          [1, 0, 0, 0, 1],
          [0, 0, 0, 1, 0],
          [0, 0, 1, 0, 0],
          [0, 1, 0, 0, 0],
          [0, 1, 0, 0, 0],
          [0, 0, 0, 0, 0],
          [0, 1, 0, 0, 0],
          [0, 0, 0, 0, 0],
        ],
      };

      // Add a better fallback for emoji - a simple smiley
      this.glyphs["☺"] = {
        resolution: [6, 9],
        pixels: [
          [0, 1, 1, 1, 1, 0],
          [1, 0, 0, 0, 0, 1],
          [1, 0, 1, 0, 1, 0],
          [1, 0, 0, 0, 0, 1],
          [1, 0, 1, 1, 0, 1],
          [1, 0, 0, 0, 0, 1],
          [0, 1, 1, 1, 1, 0],
          [0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0],
        ],
      };

      // Create a set to track glyphs currently being loaded to avoid duplicate requests
      const loadingGlyphs = new Set();

      // Create a set to track failed glyphs to avoid repeated requests
      const failedGlyphs = new Set();

      // Wrap the glyphs object with a Proxy for automatic loading
      this.glyphs = new Proxy(this.glyphs, {
        get: (target, char) => {
          // if (char === "glyphWidth") console.log("Target:", target, "Char:", char);

          // If glyph exists, return it immediately
          if (target[char]) {
            return target[char];
          }

          // If glyph has failed to load before, don't try again
          if (failedGlyphs.has(char)) {
            return this.getEmojiFallback(char, target);
          }

          // If glyph is currently being loaded, return placeholder
          if (loadingGlyphs.has(char)) {
            return this.getEmojiFallback(char, target);
          }

          // If it's a special character we shouldn't load, return fallback
          if (char === "?" || typeof char !== "string" || char.length === 0) {
            return target["?"] || null;
          }

          // Start loading the glyph asynchronously
          loadingGlyphs.add(char);

          // Convert character to Unicode code point(s) for API request
          // This handles emoji and multi-byte characters properly
          const codePoints = [];

          // Handle the case where we might have invalid/corrupted characters
          try {
            // Use Array.from to properly iterate over Unicode code points
            // This handles surrogate pairs correctly
            const characters = Array.from(char);

            for (const singleChar of characters) {
              const codePoint = singleChar.codePointAt(0);

              if (codePoint !== undefined) {
                // Check for lone surrogates (U+D800-U+DFFF) which are invalid
                if (codePoint >= 0xd800 && codePoint <= 0xdfff) {
                  console.warn(
                    `Invalid lone surrogate detected: U+${codePoint.toString(16).toUpperCase()} in char "${char}"`,
                  );
                  // For debugging, let's also try to reconstruct what this should be
                  if (char.length >= 2) {
                    console.warn(
                      `Original char length: ${char.length}, char codes:`,
                      Array.from(char).map(
                        (c) =>
                          `U+${c.codePointAt(0).toString(16).toUpperCase()}`,
                      ),
                    );
                  }
                  // Use replacement character instead
                  codePoints.push("FFFD");
                  continue;
                }

                // Use proper hex formatting for all code points
                const hexValue = codePoint
                  .toString(16)
                  .toUpperCase()
                  .padStart(codePoint > 0xffff ? 5 : 4, "0");
                codePoints.push(hexValue);
              }
            }
          } catch (error) {
            console.warn(`Error processing character "${char}":`, error);
            // Fallback to replacement character
            codePoints.push("FFFD");
          }

          // If no valid code points were found, use replacement character
          if (codePoints.length === 0) {
            codePoints.push("FFFD");
          }

          // Join multiple code points with underscores for complex characters
          const codePointStr = codePoints.join("_");

          // Make API call to load the glyph using code points
          fetch(`/api/bdf-glyph?char=${codePointStr}`)
            .then((response) => {
              if (!response.ok) {
                if (response.status === 404) {
                  console.info(
                    `Glyph "${char}" (${codePointStr}) not available in unifont`,
                  );
                } else {
                  console.warn(
                    `Failed to load glyph "${char}" (${codePointStr}): HTTP ${response.status}`,
                  );
                }
                throw new Error(`Failed to load glyph: ${response.status}`);
              }
              return response.json();
            })
            .then((glyphData) => {
              // Store the loaded glyph
              target[char] = glyphData;
              loadingGlyphs.delete(char);

              // Trigger a repaint to show the newly loaded glyph
              if (
                needsPaintCallback &&
                typeof needsPaintCallback === "function"
              ) {
                needsPaintCallback();
              }
            })
            .catch((err) => {
              // Mark this glyph as failed to avoid future requests
              failedGlyphs.add(char);
              loadingGlyphs.delete(char);

              // Don't log as error for 404s, just info
              if (!err.message.includes("404")) {
                console.warn(
                  `Failed to load glyph "${char}" (${codePointStr}):`,
                  err,
                );
              }
            });

          // Return appropriate fallback immediately while loading
          return this.getEmojiFallback(char, target);
        },
      });
    }
    return this;
  }

  // Helper method to get appropriate fallback for different character types
  getEmojiFallback(char, target) {
    if (!char || char.length === 0) {
      return target["?"] || null;
    }

    const codePoint = char.codePointAt(0);

    // Check if it's an emoji (rough heuristic)
    if (codePoint >= 0x1f600 && codePoint <= 0x1f64f) {
      // Emoticons block - use simple smiley fallback
      return target["☺"] || target["?"] || null;
    } else if (codePoint >= 0x1f300 && codePoint <= 0x1f5ff) {
      // Miscellaneous Symbols and Pictographs - use question mark
      return target["?"] || null;
    } else if (codePoint >= 0x1f680 && codePoint <= 0x1f6ff) {
      // Transport and Map Symbols - use question mark
      return target["?"] || null;
    } else if (codePoint >= 0x2600 && codePoint <= 0x26ff) {
      // Miscellaneous Symbols - use question mark
      return target["?"] || null;
    } else {
      // For other missing characters, use standard fallback
      return target["?"] || null;
    }
  }
  // 📓 tf.print
  print(
    $,
    pos = { x: undef, y: undef, size: 1, thickness: 1, rotation: 0 },
    lineNumber,
    text,
    bg = null,
    charColors = null,
  ) {
    // TODO: Pass printLine params through / make a state machine.
    const font = this.glyphs;
    const size = pos.size || 1;
    const blockMargin = 1;
    const blockHeight = ((this.blockHeight || 10) + blockMargin) * size;
    const blockWidth = this.data.glyphWidth; // * size;
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
    //   return;    // }

    const rn = $.inkrn(); // Remember the current ink color.

    // Background
    if (bg !== null) $.ink(bg).box(x, y, fullWidth, blockHeight);

    // if (text === "POW") console.log("POW PRINT 😫", x, y, width, height);    // Check if we have per-character colors
    if (charColors && charColors.length > 0) {
      // Render each character with its own color
      let currentX = x;
      for (let i = 0; i < text.length; i++) {
        const char = text[i];
        const charColor = charColors[i];

        // Set color for this character
        if (charColor) {
          if (Array.isArray(charColor)) {
            $.ink(...charColor);
          } else {
            $.ink(charColor);
          }
        } else {
          $.ink(...rn); // Use original color if no specific color
        }

        // Render single character
        $.printLine(
          char,
          font,
          currentX,
          y,
          blockWidth,
          size,
          0,
          thickness,
          rotation,
        );

        // Move to next character position
        currentX += blockWidth * size;
      }
    } else {
      // Original single-color rendering
      $.ink(...rn).printLine(
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
  #edgeCancelled = false;
  #manuallyDeactivated = false;
  #manualDeactivationTime = 0;
  #manuallyActivated = false;
  #manualActivationTime = 0;


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
  #recentlyShifting = false; // Track if we just finished character sliding

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
      font: "font_1" /*"unifont"*/, // fonts.font_1,
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

    this.copiedCallback = options.copied; // Load typeface, preventing double loading of the system default.
    if (!options.font) options.font = "font_1"; // Use preloaded font as needed.

    // Flag to track if we need to repaint due to async glyph loading
    this._needsRepaint = false;

    if ($.typeface?.data !== options.font) {
      this.typeface = new Typeface(options.font); // Load custom typeface.
      this.#moveThreshold = this.typeface.blockWidth;
      // Pass needsPaint callback for async glyph loading
      this.typeface.load($.net.preload, () => {
        this._needsRepaint = true;
      });
    } else {
      this.typeface = $.typeface; // Set to system typeface.
    }

    this.activated = options.activated;
    //this.#autolock = options.autolock;
    this.didReset = options.didReset;

    const blockWidth = this.typeface.blockWidth;
    this.#gutterMax = options.gutterMax || 48;

    this.#prompt = new Prompt(
      6, // blockWidth,
      6, // blockWidth,
      options.wrap || "char", // "char" or "word"
      $.store["gutter:lock"] ||
        Math.min(this.#gutterMax, floor($.screen.width / blockWidth) - 2),
      options.lineSpacing,
      this.typeface,
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
    this.enter.stickyScrubbing = true; // Prevent drag-between-button behavior
    this.enter.btn.stickyScrubbing = true; // Also set on the actual button object
    this.enter.btn.noEdgeDetection = true; // Prevent cancellation from edge detection
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
      floor($.screen.width / this.#prompt.letterWidth) - 2,
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

  get recentlyShifting() {
    return this.#recentlyShifting;
  }

  // Adjust the gutter width for text wrapping.
  set gutter(n) {
    this.#prompt.colWidth = n;
    this.#prompt.gutter = this.#prompt.colWidth * this.#prompt.letterWidth;
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
        //

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
        // Use green cursor in kidlisp mode, otherwise use normal color
        const cursorColor = $.system?.prompt?.kidlispMode
          ? $.dark
            ? [100, 255, 100]
            : [0, 150, 0]
          : this.pal.block;
        $.ink(cursorColor).box(prompt.pos(undefined, true)); // Draw blinking cursor.
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

    // Check if we need to repaint due to async glyph loading
    if (this._needsRepaint) {
      this._needsRepaint = false;
      needsPaint();
    }
  }

  // Helper method to ensure blink is initialized before use
  #ensureBlink() {
    if (!this.blink) {
      // Initialize blink with a basic Hourglass if sim hasn't run yet
      const Hourglass = this.$.gizmo?.Hourglass;
      if (Hourglass) {
        this.blink = new Hourglass(45, { // 0.75 seconds at 60fps
          flipped: (count, showBlinkOverride) => {
            if (showBlinkOverride !== undefined)
              this.showBlink = showBlinkOverride;
            else this.showBlink = !this.showBlink;
            this.$.needsPaint?.();
          },
          autoFlip: true,
        });
      }
    }
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
    // Push input to a history stack, avoiding repeats and prompt-prefixed navigation.
    if (store[this.key][0] !== this.text && !this.text.startsWith("prompt~")) {
      store[this.key].unshift(this.text);
    }
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

    // Handle UI interaction cancellation when pointer leaves window
    if (e.is("ui:cancel-interactions")) {
      // Mark that edge cancellation happened and cancel the activation state
      this.#edgeCancelled = true;
      this.#activatingPress = false; // Cancel the activation state immediately

      // Reset any backdrop touch state to ensure fresh interactions
      this.backdropTouchOff = false;

      // Handle all buttons to ensure proper cancellation
      this.enter.btn.act(e);
      this.copy.btn.act(e);
      this.paste.btn.act(e);
      needsPaint(); // Repaint to remove the activation outline
      return;
    }

    // Reflow the prompt on frame resize.
    if (e.is("reframed")) {
      if (!$.store["gutter:lock"]) this.fullGutter($);
      this.flow();
      needsPaint();
    }

    // ⌨️ Add text via the keyboard.
    if (e.is("keyboard:down") && this.#lock === false && !this.enter.btn.down) {
      // Reset edge cancellation when user actively starts typing
      if (e.key.length === 1 && e.ctrl === false && e.key !== "`") {
        this.#edgeCancelled = false;
      }
      
      // 🔡 Inserting an individual character.
      if (e.key.length === 1 && e.ctrl === false && e.key !== "`") {
        // Auto-activate TextInput when user starts typing if not already active
        if (!this.canType && !this.#edgeCancelled) {
          this.#manuallyActivated = true;
          this.#manualActivationTime = Date.now();
          activate(this);
        }
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
          this.#ensureBlink();
          this.blink?.flip(true);
          this.showBlink = true;
          return;
        }

        if (newLine && underCursor) {
          // Double up the characters so that this new character exists
          // at the position of the current cursor.
          this.text =
            this.text.slice(0, index + 1) + insert + this.text.slice(index + 1);
          this.#prompt.forward();
          this.#ensureBlink();
          this.blink?.flip(true);
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

              this.#ensureBlink();
              this.blink?.flip(true);
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
          // console.log("💻 Activation:", this.activate, typeof this);
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
        // Only manage Enter button disabled state when TextInput is active
        if (this.canType) {
          if (this.text.length > 0) {
            this.enter.btn.disabled = false;
            this.runnable = true;
          } else {
            this.enter.btn.disabled = true;
            this.runnable = false;
          }
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
            // Deactivate directly after sending message via keyboard Enter
            deactivate(this);
          }
        } else if (!this.canType && !this.#edgeCancelled) {
          activate(this);
        }
      }

      this.#ensureBlink();
      this.blink?.flip(true);
      this.showBlink = true;
    }

    // Handle activation / focusing of the input
    // (including os-level software keyboard overlays)
    if (e.is("keyboard:open") && !this.#lock && !this.#edgeCancelled) {
      // Only activate via keyboard:open if we haven't manually deactivated recently
      // and there's no active touch interaction
      const timeSinceManualDeactivation = Date.now() - this.#manualDeactivationTime;
      const timeSinceManualActivation = Date.now() - this.#manualActivationTime;
      if (!this.#manuallyDeactivated || this.#activatingPress || timeSinceManualDeactivation > 100) {
        activate(this);
      }
    }
    if (e.is("keyboard:close") && !this.#lock) {
      // Don't process if already deactivated
      if (!this.canType) {
        return;
      }
      
      // Only deactivate via keyboard:close if we haven't recently manually activated via touch
      const timeSinceManualDeactivation = Date.now() - this.#manualDeactivationTime;
      const timeSinceManualActivation = Date.now() - this.#manualActivationTime;
      
      // Don't close if Enter button just activated the TextInput
      if (this._enterButtonActivation) {
        return;
      }
      
      // Increase the threshold for manual activation to 500ms to account for command execution time
      if ((!this.#manuallyDeactivated || timeSinceManualDeactivation > 100) && (!this.#manuallyActivated || timeSinceManualActivation > 500)) {
        deactivate(this);
      }
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
      this.#edgeCancelled = false; // Reset edge cancellation on new touch
      // Note: Don't reset #manuallyDeactivated here - let the time-based logic handle it
      $.send({ type: "keyboard:unlock" });
      if (!this.mute) {
        sound.synth({
          type: "sine",
          tone: 300,
          attack: 0.1,
          decay: 0.96,
          volume: 0.5,
          duration: 0.01,
        });
      }
    }

    // Begin the prompt input mode / leave the splash.
    function activate(ti) {
      ti.activatedOnce = true;

      if (ti.canType) {
        // (This redundancy check is because this behavior is tied to
        // keyboard open and close events.)
        return;
      }

      // Note: Don't reset #manuallyDeactivated here - let the time-based logic handle it

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
        if (ti.#lastPrintedText) {
          ti.blank("blink");
        }

        // ti.enter.btn.disabled = true;
        ti.runnable = false; // Explicitly set runnable to false when no text
        ti.paste.btn.disabled = false;
      }
      
      //ti.inputStarted = true;
      $.act("text-input:editable");

      // Ensure keyboard is unlocked and opened for mobile devices
      $.send({ type: "keyboard:unlock" });
      $.send({ type: "keyboard:open" }); // Necessary for desktop and mobile.
      


      if (!ti.mute) {
        sound.synth({
          type: "sine",
          tone: 300,
          attack: 0.1,
          decay: 0.96,
          volume: 0.5,
          duration: 0.01,
        });
      }
    }

    this.activate = activate;

    // Leave the prompt input mode.
    function deactivate(ti) {
      if (ti.canType === false) {
        // Assume we are already deactivated.
        // (This redundancy check is because this behavior is tied to
        // keyboard open and close events.)
        return;
      }

      ti.activated?.($, false);

      ti.enter.btn.disabled = false; // Always enable Enter button when deactivated so user can reactivate
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

    if (e.is("lift")) {
      // Handle shifting reset first, before other lift logic
      if (this.shifting) {
        this.#moveDeltaX = 0;
        this.shifting = false;
        this.#recentlyShifting = true; // Track that we just finished character sliding
        
        // Reset the recently shifting flag after a short delay
        setTimeout(() => {
          this.#recentlyShifting = false;
        }, 50);
      }
      
      // Always unlock keyboard on lift (unless locked)
      if (!this.#lock) {
        $.send({ type: "keyboard:unlock" });
      }
      
      // Store the current backdropTouchOff state before resetting it
      const shouldPreventActivation = this.backdropTouchOff;
      this.backdropTouchOff = false;
      
      // Process activation for inactive TextInput
      if (!this.canType) {
        // Only process lift if we had an activating press (prevents orphaned lifts)
        if (this.#activatingPress) {
          // Check if we should activate BEFORE setting activatingPress to false
          // But only if edge cancellation didn't happen AND backdrop touch shouldn't be prevented
          if (!this.#edgeCancelled && !shouldPreventActivation) {
            this.#manuallyActivated = true; // Set flag to block unwanted keyboard events
            this.#manualActivationTime = Date.now();
            activate(this);
          } 

          this.#activatingPress = false;
        }
        // Don't reset #edgeCancelled here - let it persist to prevent keyboard events from activating
      } else {
        // Handle deactivation for active TextInput
        // Don't deactivate if lift is over Enter button and button is down (push is about to occur)
        const isOverEnterButton = (this.enter.btn.disabled === false && this.enter.btn.box.contains(e));
        const enterButtonIsDown = this.enter.btn.down;
        
        // Check if lift is over any interactive element
        const isOverInteractive = (
          (this.copy.btn.disabled === false && this.copy.btn.box.contains(e)) ||
          (this.paste.btn.disabled === false && this.paste.btn.box.contains(e)) ||
          isOverEnterButton
        );
        
        // Don't deactivate if:
        // 1. Over interactive elements OR if over active enter button
        // 2. Just finished character sliding (recentlyShifting)
        if (!isOverInteractive || (isOverEnterButton && enterButtonIsDown)) {
          if (!isOverInteractive && !this.#recentlyShifting) {
            this.#manuallyDeactivated = true;
            this.#manualDeactivationTime = Date.now();
            deactivate(this);
          }
        }
      }
    }

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
          $.send({ type: "keyboard:unlock" }); // Unlock keyboard for mobile
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
          needsPaint();
        },
        scrub: () => {
          // Silent scrubbing
        },
        push: async () => {
          // Prevent race conditions by checking if we're already processing
          if (this.#lock) {
            return;
          }
          
          // If the TextInput is not active but has text that should be sent
          if (!this.canType && this.text.trim().length > 0) {
            this.#manuallyActivated = true;
            this.#manualActivationTime = Date.now();
            activate(this);
            // Run command immediately after activation
            if (this.runnable && this.text.trim().length > 0) {
              await this.run(store);
              deactivate(this);
            }
          } else if (this.runnable && this.text.trim().length > 0) {
            this.#manuallyActivated = true;
            this.#manualActivationTime = Date.now();
            await this.run(store);
            // Deactivate directly after sending message
            this._enterHandledMessage = true; // Flag to prevent redundant keyboard:close
            deactivate(this);
            // Clear the flag after a short delay
            setTimeout(() => {
              this._enterHandledMessage = false;
            }, 200);
          } else if (this.runnable && this.text.trim().length === 0 && this.closeOnEmptyEnter) {
            deactivate(this);
          } else {
            // Reset edge cancellation for Enter button activation
            this.#edgeCancelled = false;
            
            this.#manuallyActivated = true;
            this.#manualActivationTime = Date.now();
            activate(this);
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
          ? "📋 Copy: Copied 🙃"
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
        if (debug) "📋 Paste: Pasted 🙃";
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
      this.#ensureBlink();
      this.blink?.flip(true);
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

      this.#ensureBlink();
      this.blink?.flip(true);
    }

    if (e.is("touch") && !this.#lock) {
      this.#ensureBlink();
      this.blink?.flip(true);
      this.#recentlyShifting = false; // Reset the recently shifting flag on new touch
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
        
        // Play character movement click sound
        if (!this.mute) {
          sound.synth({
            type: "sine",
            tone: 800,
            attack: 0.01,
            decay: 0.95,
            volume: 0.25,
            duration: 0.008,
          });
        }
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
        
        // Play character movement click sound
        if (!this.mute) {
          sound.synth({
            type: "sine",
            tone: 800,
            attack: 0.01,
            decay: 0.95,
            volume: 0.25,
            duration: 0.008,
          });
        }
      }

      this.#ensureBlink();
      this.blink?.flip(true);
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

  letterWidth; // Taken from the typeface's block sizing.
  letterHeight;

  colWidth = 48; // Maximum character width of each line before wrapping.

  cursor = { x: 0, y: 0 };
  gutter; // A y-position at the end of the colWidth.

  lineBreaks = []; // Legacy?

  cursorToTextMap = {}; // Keep track of text data in relationship to whitespace.
  textToCursorMap = [];
  wrappedWordIndices = []; // Keep track of word wrapped indices after
  //                           each mapping.

  #mappedTo = ""; // Text that has been mapped.

  constructor(
    top = 0,
    left = 0,
    wrap,
    colWidth = 48,
    lineSpacing = 0,
    typeface,
  ) {
    this.letterWidth = typeface.blockWidth * this.scale;
    this.letterHeight = typeface.blockHeight * this.scale + lineSpacing;
    this.top = top;
    this.left = left;
    this.wrap = wrap;
    this.colWidth = colWidth;
    this.gutter = this.colWidth * this.letterWidth;
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
    const cursor = { x: 0, y: 0 }; // Wrap and map the text either by character or word.
    // (Word wrapping is complex and skips text indices for invisible
    //  characters and in some edge cases with line breaks)
    if (this.wrap === "char") {
      let textIndex = 0;
      let brokeLine = false;

      // Use Array.from to properly handle Unicode code points
      const characters = Array.from(text);

      for (let c = 0; c < characters.length; c += 1) {
        const char = characters[c];
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

      // Use Array.from to properly handle Unicode code points
      const characters = Array.from(text);

      for (let c = 0; c < characters.length; c += 1) {
        const char = characters[c];
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
            for (let i = c; i < characters.length; i += 1) {
              const char = characters[i];
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
        } // Create a line break if a line will begin with a space and we're
        // not on a space.
        if (
          char === " " &&
          cursor.x + 1 === this.colWidth - 1 &&
          characters[textIndex] !== " "
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
      h: this.letterHeight,
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
