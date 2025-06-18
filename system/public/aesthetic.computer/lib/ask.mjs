import { DEBUG } from "../disks/common/debug.mjs";

export class Conversation {
  messages = [];
  forgetful = false;
  memory;
  controller;

  // from the `disk` api
  store;
  key;

  constructor(store, slug, forgetful = false, memory = Infinity) {
    this.store = store;
    this.key = slug + ":conversation";
    this.forgetful = forgetful;
    this.memory = memory;
  }

  // Retrieve messages from the store.
  // This can probably be deprecated... 23.07.24.17.17
  async retrieve() {
    if (!this.forgetful) {
      this.messages =
        this.store[this.key] ||
        (await this.store.retrieve(this.key, "local:db")) ||
        [];
      return this.messages.slice();
    } else {
      return this.messages.slice();
    }
  }

  async forget() {
    await this.store.delete(this.key, "local:db");
    delete this.store[this.key];
    this.messages = [];
  }

  ask(options, and, done, fail) {
    let prompt,
      program = { before: "", after: "" },
      hint;

    if (typeof options === "string") {
      prompt = options;
    } else {
      ({ prompt, program, hint } = options);
    }

    let messageLength = this.messages.length;

    if (messageLength > this.memory) this.messages.length = messageLength = 0;

    if (messageLength === 0) {
      program.before = program.before?.trim(); // Sanitize prompt program.
      program.after = program.after?.trim();
      if (program.before)
        this.messages.push({ by: "system", text: program.before });
      this.messages.push({ by: "user", text: prompt });
      if (program.after)
        this.messages.push({ by: "system", text: program.after });
    } else {
      this.messages.push({ by: "user", text: prompt });
    }

    // if (DEBUG) console.log("🗨️ Sending a conversation:", this.messages);

    this.controller?.abort(); // Prevent multiple asks / cancel existing ones.
    this.controller = new AbortController();
    const signal = this.controller.signal;

    const host = ``; // DEBUG
    // ? `` // Just use current host, via `netlify.toml`.
    // : "https://ai.aesthetic.computer";

    const responsePromise = fetch(`${host}/api/ask`, {
      method: "POST",
      signal,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ messages: this.messages, hint }),
    });

    if (this.forgetful) this.messages.length = 0;

    let timeout;

    const timeoutPromise = new Promise((resolve, reject) => {
      timeout = setTimeout(() => {
        this.controller?.abort();
        reject(new Error(`Reply timed out after 10 seconds!`));
      }, 10 * 1000);
    });

    let streamedReply = "";

    const convo = this;

    function reportFailure(error) {
      // console.error("Failed to ask:", error);
      // Clear the messages that were added,
      convo.messages = convo.messages.slice(0, messageLength);
      fail?.(); // and fail out.
    }

    Promise.race([responsePromise, timeoutPromise])
      .then((response) => {
        clearTimeout(timeout);

        if (!response.ok)
          throw new Error(`Failed to reply: ${response.status}`);

        const readableStream = response.body;
        const decoder = new TextDecoder();

        const reader = readableStream.getReader();

        // Detect chunks of JSON as they stream in.
        function read() {
          reader
            .read()
            .then(async ({ done: complete, value }) => {
              if (complete) {
                // if (DEBUG) console.log("❗ Response complete.");
                convo.controller = null;
                done?.();
                // Add last message to the queue.
                if (!convo.forgetful)
                  convo.messages.push({ by: "system", text: streamedReply });
                if (convo.store && convo.key) {
                  // convo.store[convo.key] = convo.messages; // Add messages to store.
                  // await convo.store.persist(convo.key, "local:db"); // Persist messages.
                }
              } else {
                const got = decoder.decode(value, { stream: true }); // Chunk->text.
                streamedReply += got;
                and?.(got);
                read(); // keep reading
              }
            })
            .catch(reportFailure);
        }
        read();
      })
      .catch(reportFailure);
    return () => {
      this.controller?.abort();
    };
  }
}
