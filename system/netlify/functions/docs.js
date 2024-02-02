// 📚 Docs, 24.01.29.21.54
// Return up to date data for the aesthetic.computer pieces api.

/* #region 🏁 TODO 
  - [] Fill out an initial range of identifiers.
    - [] Can always hide unnecessary / incomplete ones later.
  - [] Handle flashing screen / subtle cosmetics.
  - [] Wire it up to the VS code extension so it fills in the prompt / opens it.
  - [] Wire this endpoint up to a prompt auto-complete as well. 
  - [] Pals should bump upwards on hover and animate scroll to top on tap.
  - [] And be faded / only fade up when scrolled past 0.
  + Done
  - [x] Build a nice scrollable index page.
#endregion */

import { respond } from "../../backend/http.mjs";
import { defaultTemplateStringProcessor as html } from "../../public/aesthetic.computer/lib/helpers.mjs";
const dev = process.env.CONTEXT === "dev";
const { keys } = Object;

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
    // Commands for programming inside of pieces.
    api: {
      // 🏛️ Generic
      structure: {
        // 🧩 Top Level Piece Functions
        boot: {
          label: "🥾 Boot",
          sig: "boot({ ... })",
          desc: "Runs once when a piece starts.",
          done: false,
        },
        paint: {
          label: "🎨 Paint",
          sig: "paint({ ... })",
          desc: "Repeatedly draw to the screen at the hardware refresh rate.",
          done: false,
        },
        act: {
          label: "🎪 Act",
          sig: "act({ ... })",
          desc: "Respond to user and system input.",
          done: false,
        },
        sim: {
          label: "🧮 Sim",
          sig: "sim({ ... })",
          desc: "For calculations occurring once per logic frame. (120fps)",
          done: false,
        },
        beat: {
          label: "🥁 Beat",
          sig: "beat({ ... })",
          desc: "Runs once per system metronome tick, for rhythmic coordination.",
          done: false,
        },
        leave: {
          label: "👋 Leave",
          sig: "leave({ ... })",
          desc: "Execute code right before the piece is unloaded.",
          done: false,
        },
        meta: {
          label: " 📰 Meta",
          sig: "meta({ ... })",
          desc: "Runs once when a piece starts.",
          done: false,
        },
        preview: {
          label: "🖼️ Preview",
          sig: "preview({ ... })",
          desc: "Paint a custom thumbnail image.",
          done: false,
        },
        icon: {
          label: "🪷 Icon",
          sig: "icon({ ... })",
          desc: "Paint a piece icon, AKA `favicon`",
          done: false,
        },
        api: {
          sig: "api",
          desc: "References all built-in functionality for a top-level function.",
          done: false,
        },
      },
      // 🖱️ Interaction
      interaction: {
        pen: {
          sig: "pen: { x, y, ... }",
          desc: "Contains active mouse + touch pointer data.",
          done: false,
        },
      },
      // 🖌️ Graphics
      graphics: {
        wipe: {
          sig: "wipe(color)",
          desc: "Paint all pixels the same `color`.",
          done: false,
        },
        ink: {
          sig: "ink(color)",
          desc: "Select a `color` for painting with.",
          done: false,
        },
        line: {
          sig: "line(x1, y1, x2, y2)",
          desc: "Paint straight a 1px line from two points.",
          done: false,
        },
        box: {
          sig: "box(x, y, w, h)",
          desc: "Paint a box of a given size.",
          body: boxBody,
          done: false,
        },
      },
      number: {
        randInt: {
          sig: "randInt(n)",
          desc: "Gets a random integer.",
          done: false,
        },
      },
      help: {
        choose: {
          sig: "choose(a, b, ...)",
          desc: "Randomly return one of the arguments.",
          done: false,
        },
      },
    },
    // 🧩 Pieces that can be entered into the prompt.
    pieces: {
      // Brushes
      line: {
        sig: "line:thickness color",
        desc: "Paint freehand lines in any `thickness` or `color`",
        done: false,
      },
      rect: {
        sig: "rect color",
        desc: "Paint rectangles in any `color`",
        done: false,
      },
      smear: {
        sig: "smear size",
        desc: "Smear pixels on your painting.",
        done: false,
      },
      crop: {
        sig: "crop",
        desc: "Crop your painting.",
        done: false,
      },
      // Characters
      // ...
    },
    // 😱 Commands for entering into the prompt.
    prompts: {
      no: {
        sig: "no",
        desc: "Undo the last step of the system painting.",
        done: false,
      },
      yes: {
        sig: "yes",
        desc: "Redo the last step of the system painting.",
        done: false,
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
          font-family: monospace;
        }
        body.doc {
          overflow-x: hidden;
        }
        h1 a, h1 a:visited {
          color: inherit;
          cursor: inherit;
          text-decoration: none;
        }
        .links a[data-done=false] {
          text-decoration: line-through; 
        }
        h1 {
          font-weight: normal;
          font-size: 22px;
          margin: 0;
          position: fixed;
        }
        h1:after {
          content: "";
          height: 2.5em;
          width: 100vw;
          position: absolute;
          z-index: -1;
          top: -16px;
          left: -16px;
        }
        h1[data-done=false]:after {
          content: "wip";
          color: yellow;
          background: maroon;
          position: absolute;
          right: -32px;
          top: 0px;
          display: block;
          font-size: 50%;
          padding: 0px 3px 2px 3px;
          border-radius: 2px;
        }
        h2 {
          font-weight: normal;
          font-size: 18px;
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
          user-select: none;
        }
        p {
          margin-top: 0;
        }
        #command-list, #docs-welcome {
          margin-top: 1.5em;
        }
        #docs-welcome {
          padding: 0;
        }
        #title {
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
        .code-doc {
          padding-top: 2.5em;
          font-size: 65%;
          padding-left: 1px;
        }
        .code-doc-welcome {
          padding-top: 2.75em;
          font-size: 65%;
          padding-bottom: 3em;
        }
        pre {
          margin-top: 1em;
          margin-bottom: 1em;
        }
        pre code.hljs.language-javascript {
          padding: 0.2em 0em;
          position: relative;
          overflow-x: visible;
        }
        pre code.hljs.language-javascript:after {
          content: "";
          height: 100%;
          top: 0;
          right: -16px;
          width: 16;
          background-color: #f3f3f3;
          position: absolute;
        }
        pre code.hljs.language-javascript:before {
          content: "";
          height: 100%;
          top: 0;
          left: -16px;
          width: 16px;
          background-color: #f3f3f3;
          position: absolute;
        }
        .links a {
          text-decoration: none;
          /* border: 1px solid; */
          padding: 4px;
        }
        @media (prefers-color-scheme: dark) {
          body {
            background-color: rgb(64, 56, 74);
            color: rgba(255, 255, 255, 0.85);
          }
          h1 a:hover {
            color: rgb(205, 92, 155);
          }
          h1:after {
            background-image: linear-gradient(to bottom, rgba(64, 56, 74, 0.75) 80%, transparent);
          }
          .hljs-title.function_ {
            color: rgb(225, 105, 175);
          }
          .language-javascript.hljs {
            color: white;
          }
          pre code.hljs.language-javascript,
          pre code.hljs.language-javascript:after,
          pre code.hljs.language-javascript:before {
            background: rgb(25, 0, 25);
          }
          .links a {
            color: rgb(205, 92, 155);
          }
          .links a.top-level {
            color: rgb(92, 205, 155);
          }
        }
        @media (prefers-color-scheme: light) {
          body {
            background-color: rgba(244, 235, 250);
          }
          a.prompt, a.prompt:visited {
            color: rgb(64, 56, 74);
          }
          h1 a:hover, a.prompt:hover {
            color: rgb(205, 92, 155);
          }
          h1:after {
            background-image: linear-gradient(to bottom, rgba(244, 235, 250, 0.75) 80%, transparent);
          }
          .hljs-title.function_ {
            color: rgb(205, 92, 155);
          }
          .links a {
            color: rgb(180, 72, 135);
          }
          .links a.top-level {
            color: green;
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
    <body$bodyclass>
      <section>$content</section>
      <img
        id="pals"
        width="64"
        src="https://${event.headers["host"]}/purple-pals.svg"
      />
      <!--<iframe src="https://${event.headers["host"]}/prompt~docs"></iframe>-->
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
    <h1 data-done="$done" id="title"><a href="/docs">$name</a></h1>
    <div class="code-doc">
      <pre><code class="language-javascript">$sig</code></pre>
      <p>$desc</p>
    </div>
  `.trim();

  const commands = { ...docs.prompts, ...docs.pieces };
  let commandList = "";
  // ➿ Loop through all commands and generate HTML.
  keys(commands)
    .sort()
    .forEach((c) => {
      commandList += `
        <a class="prompt" href="https://${event.headers["host"]}/prompt~${c}" onclick="send('docs:${c}')">${c}</a>
        <small>${commands[c].desc}</small><br>
      `;
    });

  // Generate doc links for a given category.
  function genLinks(category) {
    return keys(docs.api[category] || [])
      .map(
        (k) =>
          `<a href="/docs/${category}:${k}" data-done="${docs.api[category][k].done}">${k}</a>`,
      )
      .join(" ");
  }

  const indexContent = html`
    <h1 id="title">
      <a
        href="https://${event.headers["host"]}"
        onclick="if (window.opener) window.close();"
        >docs</a
      >
    </h1>
    <div class="code-doc-welcome">
      <h2>Graphics</h2>
      <span class="links">
        <a
          data-done="${docs.api.structure.paint.done}"
          class="top-level"
          href="/docs/structure:paint"
          >paint</a
        >
        ${genLinks("graphics")}
      </span>
      <h2>Number</h2>
      <span class="links">
        <a
          data-done="${docs.api.structure.sim.done}"
          class="top-level"
          href="/docs/structure:sim"
          >sim</a
        >
        ${genLinks("number")}
      </span>
      <h2>Sound</h2>
      <span class="links">
        <a
          data-done="${docs.api.structure.beat.done}"
          class="top-level"
          href="/docs/structure:beat"
          >beat</a
        >
        ${genLinks("sound")}
        <h2>Interaction</h2>
        <span class="links">
          <a
            data-done="${docs.api.structure.act.done}"
            class="top-level"
            href="/docs/structure:act"
            >act</a
          >
          ${genLinks("interaction")}
        </span>
        <h2>Network</h2>
        <span class="links">
          <a
            data-done="${docs.api.structure.meta.done}"
            class="top-level"
            href="/docs/structure:meta"
            >meta</a
          >
          <a
            data-done="${docs.api.structure.icon.done}"
            class="top-level"
            href="/docs/structure:icon"
            >icon</a
          >
          <a
            data-done="${docs.api.structure.preview.done}"
            class="top-level"
            href="/docs/structure:preview"
            >preview</a
          >
          ${genLinks("network")}
        </span>
      </span>
      <h2>Help</h2>
      <span class="links">${genLinks("help")}</span>
    </div>
  `.trim();

  const commandsContent = html`
    <h1 id="title">
      <a
        href="https://${event.headers["host"]}"
        onclick="if (window.opener) window.close();"
        >$name</a
      >
    </h1>
    <p id="command-list" style="white-space: nowrap; display: inline-block;">
      ${commandList}
    </p>
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
  `.trim();

  docs.template = page
    .replace("$bodyclass", " class='doc'")
    .replace("$content", content);

  const splitPath = event.path.split("/");

  if (
    splitPath.length === 4 ||
    (splitPath.length === 3 && splitPath[1] !== "api")
  ) {
    let mergedDocs;
    keys(docs.api).forEach((key) => {
      mergedDocs = {
        ...mergedDocs,
        ...docs.api[key],
      };
    });

    mergedDocs = { ...mergedDocs, ...docs.pieces, ...docs.prompts };

    // TODO:
    // Use a "colon" here for parsing and building pages from. api, pieces,
    // and "prompts".

    // 🔦 What should have overlap and what should not?

    // /docs/structure:paint
    // /docs/pieces:line
    // /docs/graphics:line

    const [category, word] = splitPath.pop().split(":");

    let doc;
    if (category !== "pieces" && category !== "prompts") {
      doc = docs.api[category][word];
    } else if (category) {
      doc = docs[category][word];
    } else {
      return respond(404, "Not found. :(", {
        "Content-Type": "text/html; charset=UTF-8",
      });
    }

    return respond(
      200,
      page
        .replace("$bodyclass", " class='doc'")
        .replace("$content", content)
        .replaceAll("$name", word)
        .replaceAll("$sig", doc?.sig)
        .replaceAll("$desc", doc?.desc)
        .replaceAll("$done", doc?.done),
      { "Content-Type": "text/html; charset=UTF-8" },
    );
  }

  // Return json or html.
  if (splitPath.pop().endsWith(".json")) {
    return respond(200, docs);
  } else {
    return respond(
      200,
      page
        .replace("$bodyclass", "")
        .replaceAll("$content", indexContent)
        .replaceAll("$name", "docs"),
      { "Content-Type": "text/html; charset=UTF-8" },
    );
  }
}
