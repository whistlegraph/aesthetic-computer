// Prompt (System)
// This module contains all of the prompt system functionality.
// Prompts are language in<->out services.

import { TextInput } from "../lib/type.mjs";
import { Conversation } from "../lib/ask.mjs";

let conversation,
  input,
  abort,
  messageComplete = true,
  abortMessage = "NETWORK FAILURE",
  processing = false;

let cancel;

export async function prompt_boot(
  $,
  { prompt, program, hint, forgetful },
  reply,
  halt,
  scheme,
  wrap,
  copied,
  activated,
) {
  messageComplete = true;
  processing = false;

  conversation = new Conversation($.store, $.slug, forgetful);
  const messages = []; //await conversation.retrieve();

  if (messages.length > 0) prompt = messages[messages.length - 1].text;

  input = new TextInput(
    $,
    prompt,
    async (text) => {
      // Shortcuts for exiting back to the prompt if we are not in it.
      const exits = ["q", "quit", "leave", "exit", "forget", "bye"];
      if (exits.indexOf(text) !== -1 && $.slug !== "prompt") {
        await conversation.forget();
        input.blank();
        return $.jump("prompt");
      }

      input.lock = true;
      $.send({ type: "keyboard:lock" });
      input.enter.btn.disabled = true;

      // Disable any paste button.
      input.paste.btn.disabled = true;
      input.paste.btn.removeFromDom($, "paste");

      input.inputStarted = false;
      input.canType = false;

      const halted = await halt?.($, text);
      if (!$.leaving()) {
        input.lock = false;
        $.send({ type: "keyboard:unlock" });
      }

      if (halted) {
        messageComplete = true;
        if (halted.left) return; // Ditch if we already loaded a piece.

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

        if ($.leaving()) {
          input.lock = false;
          $.send({ type: "keyboard:unlock" });
          return; // Keep the screen emptied out if we are leaving.
        }

        // Otherwise set the reply state now.
        reply?.(input.text);
        input.bakePrintedText();
        input.runnable = false;
        input.showButton($);
        $.needsPaint();
        input.canType = true;
        input.inputStarted = true;
        return; // No more processing necessary.
      }

      processing = input.lock = true;
      abortMessage = "NETWORK FAILURE";

      $.send({ type: "keyboard:close" });
      $.send({ type: "keyboard:lock" });

      let firstAnd = true; // Clear the text on first reply.
      input.submittedText = ""; // Clear any previously submitted text cache.

      // Cancel a request (via `act`)
      cancel = function () {
        abortMessage = "";
        abort?.(); // Prevent fail from running here...
        input.activate(input);
        $.send({
          type: "keyboard:text:replace",
          content: { text: abortMessage },
        });
      };

      abort = conversation.ask(
        { prompt: text, program, hint },
        function and(msg) {
          if (firstAnd) input.submittedText = input.text;
          input.text = firstAnd ? input.text + "\n\n" + msg : input.text + msg;
          input.snap();
          firstAnd = false;
        },
        function done() {
          messageComplete = true;
          processing = input.lock = false;
          $.send({ type: "keyboard:unlock" });
          reply?.(input.text, input);
          input.bakePrintedText();
          input.clearUserText();
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
          input.text = abortMessage;
          input.snap();
          // just use notice here
          // $.send({ type: "keyboard:text:replace", content: { text: "" } });
          $.needsPaint();
          reply?.(input.text);
          input.submittedText = "";
          processing = input.lock = false;
          $.send({ type: "keyboard:unlock" });
          input.bakePrintedText();
          input.runnable = false;
          if (input.text.length > 0) {
            messageComplete = true;
            input.showButton($);
          }
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
    },
  );

  $.needsPaint();
  $.system.prompt = { input, convo: conversation }; // Set the input on the Disk API.
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
