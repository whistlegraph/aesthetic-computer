// Serves HTML from a template for every landing route on aesthetic.computer.

import { builder } from "@netlify/functions";
// import { readFile } from "fs/promises";
import https from "https";
import { URLSearchParams } from "url";

import { parse, metadata } from "../../public/aesthetic.computer/lib/parse.mjs";

async function fun(event, context) {
  const dev = process.env.CONTEXT === "dev";
  if (dev) console.log("Node version:", process.version);

  // TODO: Return a 500 or 404 for everything that does not exist...
  //       - [] Like for example if the below import fails...
  if (event.path === "/favicon.ico") return { statusCode: 500 };

  let slug = event.path.slice(1) || "prompt";

  const parsed = parse(slug, { hostname: event.headers["host"] });

  debugger;

  if (process.env.CONTEXT === "dev") console.log(slug, parsed);

  // Remote host.
  // TODO: Node currently doesn't support dynamic imports from http/s - 22.07.19.05.25
  //       - Implementation below.
  /*
  let importPath;
  if (slug.startsWith('@')) {
    importPath = `https://${parsed.host}/${parsed.path}.mjs`;
  } else {
    importPath = `../../public/${parsed.path}.mjs`;
  }
  // TODO: Check to see if the path is on this server.
  const { desc } = await import(importPath);
  */

  let meta;

  const redirect = {
    statusCode: 302,
    headers: {
      "Content-Type": "text/html",
      Location: "/" + new URLSearchParams(event.queryStringParameters),
    },
    body: '<a href="https://aesthetic.computer">https://aesthetic.computer</a>',
  };

  // Externally hosted pieces always start with @.
  try {
    if (slug.startsWith("@")) {
      const externalPiece = await getPage(
        `https://${parsed.host}/${parsed.path}.mjs`
      );
      console.log(externalPiece);
      if (externalPiece?.code !== 200) return redirect;
    } else {
      // Locally hosted piece.
      try {
        // Just whitelist freaky-flowers for now 22.11.28.13.36.
        // Also whitelist wg 22.12.25.20.28

        if (
          !parsed.text.startsWith("requestProvider.js.map") &&
          (parsed.text.startsWith("ff") ||
            parsed.text.startsWith("freaky-flowers") ||
            parsed.text.startsWith("wg") ||
            parsed.text.startsWith("prompt") ||
            parsed.text.startsWith("valbear") ||
            parsed.text === "")
        ) {
          const m = await import(`../../public/${parsed.path}.mjs`);
          meta = m.meta?.(parsed); // Parse any special piece metadata if it exists.
        }
      } catch (e) {
        console.log(e);
      }
    }
  } catch {
    // If either module doesn't load, then we KNOW we won't be able to load
    // the piece, so we can fallback to the main route.
    return redirect;
  }

  // *** Server Metadata Fields***
  const { title, desc, ogImage, twitterImage } = metadata(
    event.headers["host"],
    slug,
    meta
  );

  const assetURI = dev ? "/assets/" : "https://assets.aesthetic.computer/";

  const html = `
    <!DOCTYPE html>
    <html>
      <head>
        <meta charset="utf-8">
        <title>${title}</title>
        <link rel="icon" href="${assetURI}images/favicon.png" type="image/png">
        <link rel="stylesheet" crossorigin="anonymous" href="/type/webfonts/berkeley-mono-variable.css" />
        <link rel="stylesheet" crossorigin="anonymous" href="/type/webfonts/ywft-processing-regular.css" />
        <link rel="stylesheet" crossorigin="anonymous" href="/aesthetic.computer/style.css" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />
        <meta name="og:title" content="${title}" />
        <meta name="og:description" content="${desc}" />
        <meta name="og:image" content="${ogImage}" />
        <meta name="twitter:card" content="summary_large_image" />
        <meta name="twitter:title" content="${title}" />
        <meta name="twitter:site" content="aesthetic.computer" />
        <meta name="twitter:image" content="${twitterImage}"/>
        <script crossorigin="anonymous" src="https://cdn.auth0.com/js/auth0-spa-js/2.0/auth0-spa-js.production.js"></script>
        <script crossorigin="anonymous" src="/aesthetic.computer/boot.mjs" type="module" defer></script>
      </head>
      <body class="native-cursor">
      <script>
        if (window.self !== window.top) document.body.classList.add("embed");
      </script>
      </body>
    </html>
  `;
  return {
    statusCode: 200,
    headers: {
      "Content-Type": "text/html",
    },
    body: html,
    ttl: 60,
  };
}

async function getPage(url) {
  return new Promise((resolve) => {
    let data = "";
    https
      .get(url, (res) => {
        res.on("data", (chunk) => {
          data += chunk;
        });
        res.on("end", () => {
          resolve({ data, code: res.statusCode });
        });
      })
      .on("error", (e) => {
        console.log("Error:", e);
        resolve(); // TODO: Should I error here, rather than resolve?
      });
  });
}

export const handler = builder(fun);
