// Prompt (System)
// This module contains all of the prompt system functionality.
// Prompts are language in<->out services.

import { TextInput } from "../lib/type.mjs";
import { ask } from "../lib/ask.mjs";

let input,
  abort,
  messageComplete = false,
  abortMessage = "NETWORK FAILURE",
  processing = false;

export function prompt_boot(
  $,
  { prompt, program, hint },
  reply,
  halt,
  scheme,
  wrap
) {
  input = new TextInput(
    $,
    prompt,
    async (text) => {
      const exits = ["q", "quit", "leave", "exit", "bye", "no"];
      if (exits.indexOf(text) !== -1) return $.jump("prompt");

      input.lock = true;
      const halted = await halt?.($, text);
      input.lock = false;
      if (halted) return; // No more processing necessary.

      input.blank();
      processing = input.lock = true;
      abortMessage = "NETWORK FAILURE";

      abort = ask(
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
          input.text = abortMessage;

          if (input.text.length === 0) {
          } else {
            input.cursor = "stop";
          }

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
  abort?.();
}

export function prompt_act($) {
  const { event: e } = $;

  // Cancel any existing request on tap.
  if (
    !messageComplete &&
    processing &&
    (e.is("keyboard:down:escape") || e.is("touch"))
  ) {
    abort?.();
    abortMessage = "";
  }

  if (!messageComplete && !processing) input?.act($);

  if (messageComplete && (e.is("keyboard:down") || e.is("touch"))) {
    input.blank("blink"); // Clear input and switch back to blink cursor.
    input?.act($); // Capture any printable keystrokes.
    messageComplete = false;
  }
}
