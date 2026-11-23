import { DEBUG } from "../disks/common/debug.mjs";

// A Conversation asks Aesel's hosted inference (/api/easel-inference), the
// route every model request on AC takes: the signed-in handle pays in
// braincells, on its account's default model. (It asked /api/ask before,
// which picked models from a caller-supplied hint.) Conversation is
// constructed in several places with only a store and a slug, so the token
// arrives here once from disk.mjs rather than through every caller.
let tokenProvider = null;

export function setAskTokenProvider(provider) {
  tokenProvider = provider;
}

const SITE = "https://aesthetic.computer";
const DEFAULT_MODEL = "anthropic/claude-sonnet-5.5";
let defaultModel = null; // the account's curated default, read once a page

async function accountModel(headers, signal) {
  if (!defaultModel) {
    defaultModel = fetch(`${SITE}/api/easel-credits`, { headers, signal })
      .then((response) => (response.ok ? response.json() : {}))
      .then((body) => body.curation?.default || DEFAULT_MODEL)
      .catch(() => { defaultModel = null; return DEFAULT_MODEL; });
  }
  return defaultModel;
}

// The conversation as the old endpoint read it: every system line (the
// program before and after, and earlier replies, which are kept as system
// lines) folded into one system prompt; what was said, as the turns.
function request(messages, hint, model) {
  const system = messages.filter((m) => m.by === "system").map((m) => m.text).join("\n\n");
  const turns = messages.filter((m) => m.by === "user" || m.by === "assistant").map((m) => ({ role: m.by, content: m.text }));
  const max_tokens = hint?.startsWith("code") ? 4096 : hint?.startsWith("character") ? 256 : 1024;
  return { model, max_tokens, ...(system ? { system } : {}), messages: turns };
}

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


    // Kept as one promise rather than an await so the timeout race below still
    // starts counting from here. Hosted inference needs a handle: with no
    // token the ask fails, and the piece shows its own fallback.
    // Capture before token lookup yields: forgetful conversations clear the
    // live array below, and later asks can also mutate it while auth is pending.
    const sent = this.messages.slice();
    const responsePromise = (async () => {
      let token = null;
      try { token = await tokenProvider?.(); } catch (err) {
        if (DEBUG) console.warn("🔑 ask: no token —", err);
      }
      if (!token) throw new Error("Sign in to ask: hosted inference needs a handle.");
      const headers = { Authorization: `Bearer ${token}` };
      const model = await accountModel(headers, signal);
      return fetch(`${SITE}/api/easel-inference`, {
        method: "POST",
        signal,
        headers: { ...headers, "Content-Type": "application/json", "x-ac-client": "ask" },
        body: JSON.stringify(request(sent, hint, model)),
      });
    })();

    if (this.forgetful) this.messages.length = 0;

    let timeout;

    const timeoutPromise = new Promise((resolve, reject) => {
      timeout = setTimeout(() => {
        this.controller?.abort();
        reject(new Error(`Reply timed out after 30 seconds!`));
      }, 30 * 1000);
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
        let tail = "";

        // Server-sent events in the Anthropic shape: the reply's text arrives
        // as deltas, handed on as they come.
        function text(chunk) {
          tail += chunk;
          const lines = tail.split("\n");
          tail = lines.pop();
          let got = "";
          for (const line of lines) {
            if (!line.startsWith("data:")) continue;
            let event;
            try { event = JSON.parse(line.slice(5)); } catch { continue; }
            if (event.type === "content_block_delta" && event.delta?.type === "text_delta") got += event.delta.text;
            if (event.type === "error") throw new Error(event.error?.message || "inference failed");
          }
          return got;
        }

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
                const got = text(decoder.decode(value, { stream: true }));
                if (got) {
                  streamedReply += got;
                  and?.(got);
                }
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
