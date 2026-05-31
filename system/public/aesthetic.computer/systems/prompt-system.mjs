// Prompt (System)
// This module contains all of the prompt system functionality.
// Prompts are language in<->out services.

import { Conversation } from "../lib/ask.mjs";

let conversation,
  input,
  abort,
  messageComplete = true,
  processing = false,
  thinking = false; // 💭 True while awaiting the first token of an LLM reply.

let cancel;

// 💭 Flip the "thinking" state and mirror it onto the Disk API so pieces (e.g.
// prompt.mjs) can paint a thinking character while a reply is being composed.
function setThinking($, value) {
  thinking = value;
  if ($?.system?.prompt) $.system.prompt.thinking = value;
  $?.needsPaint?.(); // Surface (or clear) the thinking character promptly.
}

// 🎵 A bright little rising arpeggio played when the helper starts thinking.
function playThinkingMelody($) {
  const notes = [440, 554, 659]; // A4 · C#5 · E5
  notes.forEach((tone, i) => {
    setTimeout(() => {
      $.sound?.synth?.({
        type: "sine",
        tone,
        attack: 0.01,
        decay: 0.92,
        volume: 0.18,
        duration: 0.08,
      });
    }, i * 90);
  });
}

export async function prompt_boot(
  $,
  { prompt, program, hint, forgetful, memory, gutterMax, lineSpacing },
  reply,
  halt,
  scheme,
  wrap,
  copied,
  activated,
) {
  messageComplete = true;
  processing = false;

  conversation = new Conversation($.store, $.slug, forgetful, memory);
  const messages = []; //await conversation.retrieve();

  if (messages.length > 0) {
    prompt = messages[messages.length - 1].text;
  } else {
    prompt = prompt?.replaceAll("@", $.handle() || "pal");
  }

  input = new $.ui.TextInput(
    $,
    prompt,
    async (text) => {
      // Shortcuts for exiting back to the prompt if we are not in it.
      const exits = ["q", "quit", "leave", "exit", "forget", "bye"];
      if (exits.indexOf(text) !== -1 && $.slug !== "prompt") {
        await conversation.forget();
        input.blank();
        if ($.slug.indexOf("botce") > -1) {
          return $.net.refresh();
        } else {
          return $.jump("prompt");
        }
      }

      input.lock = true;
      $.send({ type: "keyboard:lock" });
      input.enter.btn.disabled = true;

      // Disable any paste button.
      input.paste.btn.disabled = true;
      input.paste.btn.removeFromDom($, "paste");

      input.canType = false;

      // Prevent the TextInput from being deactivated during command execution
      input._preventDeactivation = true;

      let halted = await halt?.($, text);

      if (!$.leaving()) {
        input.lock = false;
        $.send({ type: "keyboard:unlock" });
      }

      if (halted) {
        messageComplete = true;
        if (halted.left) {
          return; // Ditch if we already loaded a piece.
        }

        if ($.leaving()) {
          // input.lock = true;
          //$.send({ type: "keyboard:unlock" });
          return; // Keep the screen emptied out if we are leaving.
        }

        // Assume we set custom replied state via `TextInput -> replied`.
        // (Immediate bot-style reply)
        if (halted.replied) {
          input.lock = false;
          $.send({ type: "keyboard:unlock" });
          input.runnable = false;
          input.bakePrintedText();
          input.showButton($);
          $.needsPaint();
          $.send({ type: "keyboard:close" });
          return;
        }

        // Otherwise set the reply state now.
        reply?.(input.text);
        input.bakePrintedText();
        input.runnable = false;
        input.showButton($);
        $.needsPaint();
        input.canType = true;
        
        // Add a small delay before clearing the preventDeactivation flag to let any pending events settle
        setTimeout(() => {
          input._preventDeactivation = false;
        }, 200);
        
        //input.inputStarted = true;
        return; // No more processing necessary.
      }

      processing = input.lock = true;

      $.send({ type: "keyboard:close" });
      $.send({ type: "keyboard:lock" });

      let firstAnd = true; // Clear the text on first reply.
      input.submittedText = ""; // Clear any previously submitted text cache.

      // Cancel a request (via `act`)
      cancel = function () {
        setThinking($, false);
        abort?.(); // Prevent fail from running here...
      };

      // 💭 Start thinking — a character + melody play until the first token.
      setThinking($, true);
      playThinkingMelody($);

      abort = conversation.ask(
        { prompt: text, program, hint },
        function and(msg) {
          setThinking($, false); // First token arrived — done thinking, now speaking.
          // Replace curly single and double quotes with straight quotes.
          msg = msg.replace(/[\u2018\u2019\u201C\u201D]/g, (match) => {
            if (match === "\u2018" || match === "\u2019") {
              // Replace single curly quotes
              return "'";
            } else {
              // Replace double curly quotes
              return '"';
            }
          });

          msg = msg.replace(/—/g, "-"); // Replace em dash with hyphen.

          if (firstAnd) input.submittedText = input.text;
          input.text = firstAnd ? input.text + "\n\n" + msg : input.text + msg;
          input.snap();
          firstAnd = false;
        },
        function done() {
          setThinking($, false);
          messageComplete = true;
          processing = input.lock = false;
          $.send({ type: "keyboard:unlock" });
          reply?.(input.text, input);
          input.bakePrintedText();
          input.clearUserText();
          $.send({ type: "keyboard:text:replace", content: { text: "" } });
          // input.submittedText = "";
          input.runnable = false;
          input.showButton($);
          $.needsPaint();
          $.sound.synth({
            type: "sine",
            tone: 500,
            attack: 0.1,
            decay: 0.96,
            volume: 0.65,
            duration: 0.05,
          });
        },
        function fail() {
          setThinking($, false);
          input.activate(input);
          input.text = "";
          input.snap();
          $.notice("NETWORK FAILURE", ["yellow", "red"]);
          $.send({ type: "keyboard:text:replace", content: { text: "" } });
          $.needsPaint();
          reply?.(input.text);
          input.submittedText = "";
          processing = input.lock = false;
          $.send({ type: "keyboard:unlock" });
          $.send({ type: "keyboard:open" }); // Will not auto-open on iOS? 23.12.02.13.01
          // input.bakePrintedText();
          input.runnable = false;
          //if (input.text.length > 0) {
          // messageComplete = true;
          // input.showButton($);
          //}
        },
      );
    },
    {
      // autolock: false,
      wrap,
      scheme,
      copied,
      activated,
      didReset: () => {
        messageComplete = true;
      },
      gutterMax,
      lineSpacing,
    },
  );

  $.needsPaint();
  $.system.prompt = { input, convo: conversation, thinking: false }; // Set the input on the Disk API.
}

export function prompt_sim($) {
  input?.sim($);
}

export function prompt_paint($) {
  return input?.paint($);
}

export function prompt_leave() {
  abort?.();
}

export function prompt_act($) {
  const { event: e, slug } = $;

  // Cancel any existing request on tap.
  if (
    !messageComplete &&
    processing &&
    e.is("keyboard:down")
    //(e.is("keyboard:down:escape") ||
    //  (e.is("keyboard:down:`") && slug === "prompt"))
  ) {
    cancel();
  }

  let inputHandled = false;

  // Whitelist events for tracking the TextInput.
  // Note: Any event typed added to `TextInput` -> `act` must be
  //       whitelisted here. 💬
  if (
    e.is("ui:cancel-interactions") ||
    e.is("move") ||
    e.is("draw") ||
    e.is("touch") ||
    e.is("lift") ||
    e.is("focus") ||
    e.is("reframed") ||
    e.is("defocus") ||
    e.is("keyboard:open") ||
    e.is("keyboard:close") ||
    // e.is("pasted:text") ||
    e.is("prompt:text:replace") ||
    e.is("prompt:text:select") ||
    e.is("prompt:text:cursor") ||
    e.of?.("clipboard")
  ) {
    input?.act($);
    inputHandled = true;
  }

  if (!messageComplete && !processing && !inputHandled) input?.act($);

  if (messageComplete && e.is("keyboard:down")) {
    if (!inputHandled) input?.act($); // Capture any printable keystrokes.
    messageComplete = false;
  }
}
