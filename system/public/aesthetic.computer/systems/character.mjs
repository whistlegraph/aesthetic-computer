// Character
// This module contains all of the  character system functionality.
// Characters are language in<->out
// services.

import { TextInput } from "../lib/type.mjs";
import { ask } from "../lib/ask.mjs";

let input,
  controller,
  messageComplete = false,
  processing = false;

export function character_boot($, { prompt, program, hint }, reply) {
  input = new TextInput(
    $,
    prompt,
    async (text) => {
      const exits = ["q", "quit", "leave", "exit", "bye", "no"];
      if (exits.indexOf(text) !== -1) {
        $.jump("prompt");
        return;
      }

      input.blank();
      processing = input.lock = true;
      controller = ask(
        { prompt: text, program, hint },
        function and(msg) {
          input.text += msg;
        },
        function done() {
          input.cursor = "stop";
          messageComplete = true;
          processing = input.lock = false;
          reply(input.text);
        },
        function fail() {
          input.text = "NETWORK FAILURE";
          input.cursor = "stop";
          messageComplete = true;
          processing = input.lock = false;
        }
      );
    },
    { autolock: false, wrap: "word" }
  );
}

export function character_sim($) {
  input?.sim($);
}

export function character_paint($) {
  $.wipe(0);
  return input?.paint($);
}

export function character_leave() {
  controller?.abort();
}

export function character_act($) {
  const { event: e } = $;

  if (!messageComplete && processing) {
    if (e.is("keyboard:down:escape")) {
      console.log(controller);
      controller?.abort();
    }
  }

  if (!messageComplete && !processing) input?.act($);
  if (messageComplete && (e.is("keyboard:down") || e.is("touch"))) {
    input.blank("blink"); // Clear input and switch back to blink cursor.
    input?.act($); // Capture any printable keystrokes.
    messageComplete = false;
  }
}
