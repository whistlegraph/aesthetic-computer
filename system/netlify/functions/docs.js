// 📚 Docs, 24.01.29.21.54
// Return up to date data for the aesthetic.computer pieces api.

/* #region 🏁 TODO 
  - [🟡] Build a nice scrollable index page.
  - [] Finish docs pages.
  - [] Handle flashing screen / subtle cosmetics.
  - [] Rename `docs` to `help` ?
  - [] Wire it up to the VS code extension so it fills in the prompt / opens it.
  - [] Wire this endpoint up to a prompt auto-complete as well. 
  - [] Pals should bump upwards on hover and animate scroll to top on tap.
  - [] And be faded / only appear when scrolled past 0.
#endregion */

import { respond } from "../../backend/http.mjs";
import { defaultTemplateStringProcessor as html } from "../../public/aesthetic.computer/lib/helpers.mjs";
const dev = process.env.CONTEXT === "dev";

// GET A user's `sub` id from either their handle or email address.
export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  // TODO: Factor this out.
  const boxBody = `
    <code>box()</code> <em>A random box</em></li><br>
    <code>box(x, y, size)</code></code> <em>Square from top left corner</em><br>
    <mark><code>box(x, y, w, h)</code> <em>Rectangle from top left corner</em></mark><br>
    <code>box(x, y, size, mode)</code> <em>Square with <code>mode</code></em><br>
    <code>box(x, y, w, h, mode)</code> <em>Rectangle with <code>mode</code></em><br>
    <br>
    <hr>
    <code>mode</code>
    <br>
    <code>center</code> &nbsp;- paints a box from the center<br>
    <code>outline</code> - paints the outline of a box<br>
    <code>inline</code> &nbsp;- the opposite of outline<br>
    <em>(thicken with <code>:</code> like <code>outline:4</code>)</em>
    <br>
    combine modes with <code>*</code> like <code>outline*center</code> or <code>inline:3*center</code> 
  `.trim();

  const docs = {
    top: {
      // 🧩 Top Level Piece Functions
      boot: {
        label: "🥾 Boot",
        sig: "boot({ ... })",
        desc: "Runs once when a piece starts.",
      },
      paint: {
        label: "🎨 Paint",
        sig: "paint({ ... })",
        desc: "Repeatedly draw to the screen at the hardware refresh rate.",
      },
      act: {
        label: "🎪 Act",
        sig: "act({ ... })",
        desc: "Respond to user and system input.",
      },
      sim: {
        label: "🧮 Sim",
        sig: "sim({ ... })",
        desc: "For calculations occurring once per logic frame. (120fps)",
      },
      beat: {
        label: "🥁 Beat",
        sig: "beat({ ... })",
        desc: "Runs once per system metronome tick, for rhythmic coordination.",
      },
      leave: {
        label: "👋 Leave",
        sig: "leave({ ... })",
        desc: "Execute code right before the piece is unloaded.",
      },
      meta: {
        label: " 📰 Meta",
        sig: "meta({ ... })",
        desc: "Runs once when a piece starts.",
      },
      preview: {
        label: "🖼️ Preview",
        sig: "preview({ ... })",
        desc: "Paint a custom thumbnail image.",
      },
      icon: {
        label: "🪷 Icon",
        sig: "icon({ ... })",
        desc: "Paint a piece icon, AKA `favicon`",
      },
    },
    // Commands for programming inside of pieces.
    api: {
      // Generic
      api: {
        sig: "api",
        desc: "Contains all built-in functionality for a piece.",
      },
      // Input
      pen: {
        sig: "pen: { x, y, ... }",
        desc: "Contains active mouse + touch pointer data.",
      },
      // Graphics
      wipe: {
        sig: "wipe(color)",
        desc: "Paint all pixels the same `color`.",
      },
      ink: {
        sig: "ink(color)",
        desc: "Select a `color` for painting with.",
      },
      line: {
        sig: "line(x1, y1, x2, y2)",
        desc: "Paint straight a 1px line from two points.",
      },
      box: {
        sig: "box(x, y, w, h)",
        desc: "Paint a box of a given size.",
        body: boxBody,
      },
    },
    // Pieces that can be entered into the prompt.
    pieces: {
      // Brushes
      line: {
        sig: "line:thickness color",
        desc: "Paint freehand lines in any `thickness` or `color`",
      },
      rect: {
        sig: "rect color",
        desc: "Paint rectangles in any `color`",
      },
      smear: {
        sig: "smear size",
        desc: "Smear pixels on your painting.",
      },
      crop: {
        sig: "crop",
        desc: "Crop your painting.",
      },
      // Characters
      // ...
    },
    // Commands for entering into the prompt.
    prompts: {
      no: {
        sig: "no",
        desc: "Undo the last step of the system painting.",
      },
      yes: {
        sig: "yes",
        desc: "Redo the last step of the system painting.",
      },
    },
  };

  const page = html` <html>
    <head>
      <link
        rel="icon"
        href="https://${event.headers["host"]}/icon/128x128/prompt.png"
        type="image/svg"
      />
      <meta charset="utf-8" />
      <title>$name · Aesthetic Computer</title>
      <style>
        ::-webkit-scrollbar {
          display: none;
        }
        html {
          cursor:
            url("/aesthetic.computer/cursors/precise.svg") 12 12,
            auto;
        }
        body {
          margin: 0;
          font-size: 22px;
        }
        body,
        p {
          font-family: monospace;
          padding-right: 16px;
        }
        h1 a,
        h1 a:visited {
          color: inherit;
          text-decoration: none;
          cursor: inherit;
        }
        h1 {
          font-weight: normal;
          font-size: 22px;
          margin: 0;
          position: fixed;
        }
        iframe {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          border: none;
          z-index: -1;
          opacity: 0.5;
        }
        section {
          margin: 12px 16px 12px 16px;
        }
        #pals {
          position: fixed;
          bottom: 5px;
          right: 16px;
        }
        p {
          margin-top: 0;
        }
        #title {
          position: relative;
          display: inline-block;
        }
        #title.block:after {
          content: "_";
          position: absolute;
          top: 6px;
          right: -16px;
          line-height: 20px;
          color: rgb(205, 92, 155);
          background-color: rgb(205, 92, 155);
        }
        a.prompt,
        a.prompt:visited {
          color: white;
          text-decoration: none;
          cursor: inherit;
        }
        a.prompt:hover {
          color: rgb(205, 92, 155);
        }
        small {
          opacity: 0.25;
          font-size: 100%;
        }
        @media (prefers-color-scheme: dark) {
          body {
            background-color: rgb(64, 56, 74);
            color: rgba(255, 255, 255, 0.85);
          }
          h1 a:hover {
            color: rgb(205, 92, 155);
          }
        }
        @media (prefers-color-scheme: light) {
          body {
            background-color: rgba(255, 255, 255, 0.85);
            color: rgb(64, 56, 74);
          }
          h1 a:hover {
            color: rgb(205, 92, 155);
          }
        }
        /* body.light {
          background-color: rgba(255, 255, 255, 0.25);
        }
        body.dark {
          background-color: rgba(0, 0, 0, 0.25);
        }
        body.high-contrast {
          background-color: none;
        } */
        /* body.light #pals {
          filter: brightness(0%);
        }
        body.dark #pals {
          filter: brightness(200%);
        }
        body.high-contrast #pals {
          filter: saturate(0%);
        } */
      </style>
      <link
        rel="stylesheet"
        href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.1/styles/default.min.css"
      />
      <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.1/highlight.min.js"></script>
      <script>
        hljs.highlightAll();
      </script>
    </head>
    <body>
      <section>$content</section>
      <img
        id="pals"
        width="64"
        src="https://${event.headers["host"]}/purple-pals.svg"
      />
      <!--<iframe src="https://${event.headers[
        "host"
      ]}/prompt~docs"></iframe>-->
      <script>
        window.addEventListener("message", (event) => {
          const message = event.data; // The JSON data our extension sent
          switch (message.theme) {
            case "light":
              document.body.classList.add("light");
              document.body.classList.remove("dark", "high-contrast");
              break;
            case "dark":
              document.body.classList.add("dark");
              document.body.classList.remove("light", "high-contrast");
              break;
            case "high-contrast":
              document.body.classList.add("high-contrast");
              document.body.classList.remove("dark", "light");
              break;
          }
        });

        // 🌠 Live editing (while developing aesthetic locally)
        if (${dev}) {
          var socket = new WebSocket("ws://localhost:8889");

          socket.onopen = function (event) {
            console.log("🟢 Live editing enabled.");
          };

          socket.onmessage = function (event) {
            const msg = JSON.parse(event.data);
            console.log("🟡 Message from server:", msg);

            // Refresh the current page if needed.
            if (msg.type === "reload") {
              document.location.reload();
              /*
              fetch(document.location)
                .then((response) => {
                  if (!response.ok)
                    throw new Error("Network error:", response.statusCode);
                  return response.text();
                })
                .then((html) => (document.documentEleme
                .catch((error) => console.error("🔴 Error reloading:", error));
              */
            }
          };

          socket.onerror = function (event) {
            console.error("🔴 Error observed:", event);
          };

          socket.onclose = function (event) {
            console.log("🔴 Live editing disabled.");
          };
        }
      </script>
    </body>
  </html>`.trim();

  const content = `
    <h1>$name</h1>
    <p>
      <code class="language-javascript">$sig</code>
      <br>
      $desc
    </p>
  `;

  const commands = { ...docs.prompts, ...docs.pieces };
  let commandList = "";
  // ➿ Loop through all commands and generate HTML.
  Object.keys(commands)
    .sort()
    .forEach((c) => {
      commandList += `
        <a class="prompt" href="https://${event.headers["host"]}/prompt~${c}" onclick="send('docs:${c}')">${c}</a>
        <small>${commands[c].desc}</small><br>
      `;
    });

  const indexContent = html`
    <h1 id="title">
      <a
        href="https://${event.headers["host"]}"
        onclick="if (window.opener) window.close();"
        >$name</a
      >
    </h1>
    <p style="white-space: nowrap; display: inline-block;">${commandList}</p>
    <script>
      function send(message) {
        if (window.opener) {
          window.opener.postMessage(message, "*");
          window.close();
        }
      }
      const title = document.querySelector("#title");
      const originalHTML = title.innerHTML;
      document.querySelectorAll(".prompt").forEach((prompt) => {
        prompt.addEventListener("pointerenter", (e) => {
          title.innerHTML = prompt.innerHTML;
          title.classList.add("block");
          prompt.addEventListener(
            "pointerleave",
            (e) => {
              title.innerHTML = originalHTML;
              title.classList.remove("block");
            },
            { once: true },
          );
        });
      });
    </script>
  `;

  docs.template = page;

  const splitPath = event.path.split("/");

  if (splitPath.length === 4) {
    const word = splitPath.pop();
    const doc =
      docs.api[word] ||
      docs.top[word] ||
      docs.pieces[word] ||
      docs.prompts[word];

    return respond(
      200,
      page
        .replaceAll("$content", content)
        .replaceAll("$name", word)
        .replaceAll("$sig", doc?.sig)
        .replaceAll("$desc", doc?.desc),
      { "Content-Type": "text/html; charset=UTF-8" },
    );
  }

  // Return json or html.
  if (splitPath.pop().endsWith(".json")) {
    return respond(200, docs);
  } else {
    return respond(
      200,
      page.replaceAll("$content", indexContent).replaceAll("$name", "docs"),
      { "Content-Type": "text/html; charset=UTF-8" },
    );
  }
}
