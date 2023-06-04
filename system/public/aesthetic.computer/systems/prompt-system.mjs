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
  wrap
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
        input.forget();
        if ($.slug !== "prompt") {
          return $.jump("prompt");
        } else return;
      }

      input.lock = true;
      input.go.btn.disabled = true;
      const halted = await halt?.($, text);
      if (!$.jumping()) { input.lock = false; }
      if (halted) return; // No more processing necessary.

      processing = input.lock = true;
      abortMessage = "NETWORK FAILURE";

      $.send({ type: "keyboard:close" });
      $.send({ type: "keyboard:lock" });

      input.inputStarted = false;
      input.canType = false;
      let firstAnd = true; // Clear the text on first reply.

      abort = conversation.ask(
        { prompt: text, program, hint },
        function and(msg) {
          input.text = firstAnd ? msg : input.text + msg;
          firstAnd = false;
        },
        function done() {
          messageComplete = true;
          processing = input.lock = false;
          reply?.(input.text);
          input.showButton("Prompt");
        },
        function fail() {
          input.text = abortMessage;
          processing = input.lock = false;
          input.canType = true;
          if (input.text.length === 0) {
          } else {
            input.cursor = "stop";
            messageComplete = true;
            input.showButton("Back");
          }
        }
      );
    },
    {
      autolock: false,
      wrap,
      palette: scheme?.[$.dark ? "dark" : "light"],
      didReset: () => {
        messageComplete = true;
      },
    }
  );

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
    e.is("defocus") ||
    e.is("typing-input-ready") ||
    e.is("typing-input-unready")
  ) {
    input?.act($);
    inputHandled = true;
  }

  if (!messageComplete && !processing && !inputHandled) input?.act($);

  if (messageComplete && e.is("keyboard:down")) {
    input.blank("blink"); // Clear input and switch back to blink cursor.
    if (!inputHandled) input?.act($); // Capture any printable keystrokes.
    messageComplete = false;
  }
}
