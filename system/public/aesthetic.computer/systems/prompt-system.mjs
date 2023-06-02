// Prompt (System)
// This module contains all of the prompt system functionality.
// Prompts are language in<->out services.

import { TextInput } from "../lib/type.mjs";
import { Conversation } from "../lib/ask.mjs";

let conversation,
  input,
  abort,
  messageComplete = false,
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
      const halted = await halt?.($, text);
      input.lock = false;
      if (halted) return; // No more processing necessary.

      input.blank();
      processing = input.lock = true;
      abortMessage = "NETWORK FAILURE";

      abort = conversation.ask(
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
      e.is("touch") ||
      (e.is("keyboard:down:`") && slug === "prompt"))
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
