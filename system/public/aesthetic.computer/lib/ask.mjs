import { DEBUG } from "../disks/common/debug.mjs";

let controller;

// TODO:
// ❤️‍🔥 Controller needs to instantiate outside / why can't `ask` return
//    controller or why can't the "leave" be managed?

// Query a LLM
// `options` can be a string prompt or an object { prompt, program }
// where `program` has a `before` and `after` string.
export function ask(options, and, done, fail) {
  let prompt,
    program = { before: "", after: "" },
    hint;

  if (typeof options === "string") {
    prompt = options;
  } else {
    ({ prompt, program, hint } = options);
  }

  program.before = program.before?.trim(); // Sanitize prompt program.
  program.after = program.after?.trim();

  controller?.abort(); // Prevent multiple asks / cancel existing ones.
  controller = new AbortController();
  const signal = controller.signal;

  const host = DEBUG
    ? "https://localhost:9000"
    : "https://ai.aesthetic.computer";

  const responsePromise = fetch(`${host}/api/ask`, {
    method: "POST",
    signal,
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ prompt, program, hint }),
  });

  let timeout;

  const timeoutPromise = new Promise((resolve, reject) => {
    timeout = setTimeout(() => {
      controller.abort();
      reject(new Error(`Reply timed out after 10 seconds!`));
    }, 10 * 1000);
  });

  Promise.race([responsePromise, timeoutPromise])
    .then((response) => {
      clearTimeout(timeout);

      if (!response.ok) throw new Error(`Failed to reply: ${response.status}`);

      const readableStream = response.body;
      const decoder = new TextDecoder();

      const reader = readableStream.getReader();

      // Detect chunks of JSON as they stream in.
      function read() {
        reader.read().then(({ done: complete, value }) => {
          if (complete) {
            // if (DEBUG) console.log("❗ Response complete.");
            controller = null;
            done?.();
          } else {
            const got = decoder.decode(value, { stream: true }); // Chunk->text.
            and?.(got);
            read(); // keep reading
          }
        });
      }

      read();
    })
    .catch((error) => {
      console.error("Failed to ask:", error);
      fail?.();
    });

  return controller;
}

/*
export async function ask(options, and, done, fail) {
  let prompt,
    program = { before: "", after: "" },
    hint;

  if (typeof options === "string") {
    prompt = options;
  } else {
    ({ prompt, program, hint } = options);
  }

  program.before = program.before?.trim(); // Sanitize prompt program.
  program.after = program.after?.trim();

  controller?.abort(); // Prevent multiple asks / cancel existing ones.
  controller = new AbortController();
  const signal = controller.signal;

  try {
    const host = DEBUG
      ? "https://localhost:9000"
      : "https://ai.aesthetic.computer";

    const responsePromise = fetch(`${host}/api/ask`, {
      method: "POST",
      signal,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ prompt, program, hint }),
    });

    let timeout;

    const timeoutPromise = new Promise((resolve, reject) => {
      timeout = setTimeout(() => {
        controller.abort();
        reject(new Error(`Reply timed out after 10 seconds!`));
      }, 10 * 1000);
    });

    const response = await Promise.race([responsePromise, timeoutPromise]);
    clearTimeout(timeout);

    if (!response.ok) throw new Error(`Failed to reply: ${response.status}`);

    const readableStream = response.body;
    const decoder = new TextDecoder();

    const reader = readableStream.getReader();

    // Detect chunks of JSON as they stream in.
    while (true) {
      const { done: complete, value } = await reader.read();

      if (complete) {
        // if (DEBUG) console.log("❗ Response complete.");
        controller = null;
        done?.();
        break;
      }

      const got = decoder.decode(value, { stream: true }); // Chunk to text.
      and?.(got);
    }
  } catch (error) {
    console.error("Failed to ask:", error);
    fail?.();
  }

  return controller;
}
*/
