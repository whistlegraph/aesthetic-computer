// Prompt (System)
// This module contains all of the prompt system functionality.
// Prompts are language in<->out services.

import { TextInput } from "../lib/type.mjs";
import { ask } from "../lib/ask.mjs";

let input,
  controller,
  messageComplete = false,
  processing = false;

export function prompt_boot(
  $,
  { prompt, program, hint },
  reply,
  halt,
  scheme,
  wrap,
) {
  input = new TextInput(
    $,
    prompt,
    async (text) => {
      const exits = ["q", "quit", "leave", "exit", "bye", "no", "end"];
      if (exits.indexOf(text) !== -1) return $.jump("prompt");

      input.lock = true;
      const halted = await halt?.($, text);
      input.lock = false;
      if (halted) return; // No more processing necessary.

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
          reply?.(input.text);
        },
        function fail() {
          input.text = "NETWORK FAILURE";
          input.cursor = "stop";
          messageComplete = true;
          processing = input.lock = false;
        }
      );
    },
    {
      autolock: false,
      wrap,
      palette: scheme?.[$.dark ? "dark" : "light"],
    }
  );
  $.system.prompt = { input }; // Set the input on the Disk API.
}

export function prompt_sim($) {
  input?.sim($);
}

export function prompt_paint($) {
  return input?.paint($);
}

export function prompt_leave() {
  controller?.abort();
}

export function prompt_act($) {
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
