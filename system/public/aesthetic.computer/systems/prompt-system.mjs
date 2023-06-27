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

export async function prompt_boot(
  $,
  { prompt, program, hint, forgetful },
  reply,
  halt,
  scheme,
  wrap,
  editable,
  copied,
  activated
) {
  messageComplete = true;
  processing = false;

  conversation = new Conversation($.store, $.slug, forgetful);
  const messages = await conversation.retrieve();

  if (messages.length > 0) prompt = messages[messages.length - 1].text;

  input = new TextInput(
    $,
    prompt,
    async (text) => {
      // Shortcuts for exiting back to the prompt if we are not in it.
      const exits = ["q", "quit", "leave", "exit", "forget"];
      if (exits.indexOf(text) !== -1) {
        await conversation.forget();
        input.blank();
        if ($.slug !== "prompt") {
          return $.jump("prompt");
        } else return;
      }

      input.lock = true;
      input.enter.btn.disabled = true;

      // Disable any paste button.
      input.paste.btn.disabled = true;
      input.paste.btn.removeFromDom($, "paste");

      input.inputStarted = false;
      input.canType = false;

      const halted = await halt?.($, text);
      if (!$.jumping()) input.lock = false;
      if (halted) {
        messageComplete = true;
        reply?.(input.text);
        input.bakePrintedText();
        $.needsPaint();
        return; // No more processing necessary.
      }

      processing = input.lock = true;
      abortMessage = "NETWORK FAILURE";

      $.send({ type: "keyboard:close" });
      $.send({ type: "keyboard:lock" });

      let firstAnd = true; // Clear the text on first reply.

      abort = conversation.ask(
        { prompt: text, program, hint },
        function and(msg) {
          input.text = firstAnd ? msg : input.text + msg;
          input.snap();
          firstAnd = false;
        },
        function done() {
          messageComplete = true;
          processing = input.lock = false;
          reply?.(input.text);
          input.bakePrintedText();
          input.clearUserText();
          input.runnable = false;
          input.showButton($);
          $.needsPaint();
        },
        function fail() {
          input.text = abortMessage;
          $.needsPaint();
          reply?.(input.text);
          //input.#lastPrintedText = input.text;
          processing = input.lock = false;
          input.bakePrintedText();
          input.runnable = false;
          if (input.text.length > 0) {
            messageComplete = true;
            input.showButton($);
          }
        }
      );
    },
    {
      autolock: false,
      wrap,
      scheme,
      editable,
      copied,
      activated,
      didReset: () => {
        messageComplete = true;
      },
    }
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
    (e.is("keyboard:down:escape") ||
      (e.is("keyboard:down:`") && slug === "prompt"))
  ) {
    abort?.();
    abortMessage = "";
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
    e.is("pasted:text") ||
    e.is("defocus") ||
    e.is("keyboard:open") ||
    e.is("keyboard:close") ||
    e.is("clipboard:copy:copied") ||
    e.is("clipboard:copy:failed") ||
    e.is("clipboard:paste:pasted") ||
    e.is("clipboard:paste:failed")
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
