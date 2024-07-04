// Redirect Proxy, 24.01.28.00.16
// For authorizing clients via the VSCode Extension both on desktop and on the web.
// Transcribed from: https://github.com/estruyf/vscode-redirect/blob/main/pages/index.tsx

import { defaultTemplateStringProcessor as html } from "../../public/aesthetic.computer/lib/helpers.mjs";

async function fun(event, context) {
  const { queryStringParameters } = event;
  const state = queryStringParameters.state;
  const tenant = event.path.indexOf("sotce") > -1 ? "sotce" : "aesthetic";

  let url = "";

  if (state) {
    const decodedUrl = new URL(decodeURIComponent(state));
    Object.keys(queryStringParameters).forEach((key) => {
      if (key !== "state") {
        decodedUrl.searchParams.set(key, queryStringParameters[key]);
      }
    });

    if (
      decodedUrl.href &&
      (decodedUrl.href.startsWith("vscode://") ||
        decodedUrl.href.startsWith("vscode-insiders://") ||
        decodedUrl.href.includes(".github.dev") ||
        decodedUrl.href.includes(".gitpod.io") ||
        decodedUrl.href.startsWith("https://vscode.dev"))
    ) {
      url = decodedUrl.href;
    }
  }

  const body = html`
    <!doctype html>
    <html lang="en">
      <head>
        <meta charset="UTF-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <title>
          Logging in... ·
          ${tenant === "aesthetic" ? "Aesthetic Computer" : "Sotce"}
        </title>
        <style>
          body {
            background: ${tenant === "aesthetic" ? "rgb(50, 30, 60)" : "white"};
            color: ${tenant === "aesthetic" ? "rgb(220, 30, 100)" : "black"};
            font-family: ${tenant === "aesthetic" ? "monospace" : "arial"};
            display: flex;
            width: 100vw;
            height: 100vh;
          }
          h1 {
            font-family: sans-serif;
            font-weight: normal;
          }
          code {
            font-size: 120%;
            color: pink;
            opacity: 0.75;
          }
          a {
            color: ${tenant === "aesthetic" ? "white" : "black"};
          }
          #wrapper {
            margin: auto;
          }
          mark {
            background-color: maroon;
            color: pink;
          }
        </style>
      </head>
      <body>
        <div id="wrapper">
          <h1>Redirecting to <code>Visual Studio Code!</code></h1>
          <p>
            ${url
              ? `In case you are not redirected, <a href="${url}" title="Open Visual Studio Code">click here</a>.`
              : "<mark>No redirect URL provided</mark>"}
          </p>
        </div>
        <script>
          // Redirection logic here if needed
          if ("${url}") {
            window.location.href = "${url}";
          }
        </script>
      </body>
    </html>
  `;

  return { statusCode: 200, headers: { "Content-Type": "text/html" }, body };

  // return url
  //   ? { statusCode: 302, headers: { Location: url } }
  //   : { statusCode: 200, body: "No redirect URL provided" };
}

export const handler = fun;
