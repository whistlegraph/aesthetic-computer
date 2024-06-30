// Serves HTML from a template for every landing route on aesthetic.computer.

import https from "https";
import path from "path";
import { promises as fs } from "fs";
import { URLSearchParams } from "url";
import { encode } from "he";
import * as num from "../../public/aesthetic.computer/lib/num.mjs";
import {
  parse,
  metadata,
  updateCode,
} from "../../public/aesthetic.computer/lib/parse.mjs";
import { respond } from "../../backend/http.mjs";
import { defaultTemplateStringProcessor as html } from "../../public/aesthetic.computer/lib/helpers.mjs";
import { networkInterfaces } from "os";
const dev = process.env.CONTEXT === "dev";

async function fun(event, context) {
  // TODO: Return a 500 or 404 for everything that does not exist...
  //       - [] Like for example if the below import fails...

  if (
    event.path === "/favicon.ico" ||
    event.path === "/requestProvider.js.map"
  ) {
    return { statusCode: 500 };
  }

  console.log("📁", event.path);
  console.log("😃", __dirname, __filename);

  let slug = event.path.slice(1) || "prompt";

  console.log("Path:", event.path, "Host:", event.headers["host"]);

  // Some domains will rewrite the initial slug.
  if (event.headers["host"] === "botce.ac") {
    slug = "botce";
  } else if (
    event.headers["host"] === "m2w2.whistlegraph.com" &&
    event.path.length <= 1
  ) {
    slug = "wg~m2w2";
  }

  const parsed = parse(slug, { hostname: event.headers["host"] });

  // Get local IP.
  let lanHost;
  if (dev) {
    const ifaces = networkInterfaces();
    let ipAddress;

    // Iterate over network interfaces to find the 1st non-internal IPv4 address
    Object.keys(ifaces).forEach((ifname) => {
      ifaces[ifname].forEach((iface) => {
        if (iface.family === "IPv4" && !iface.internal) {
          ipAddress = iface.address;
          return;
        }
      });
    });
    lanHost = `"https://${ipAddress}:8888"`; // Quoted for use in `body`.
  }

  let meta;

  const redirect = {
    statusCode: 302,
    headers: {
      "Content-Type": "text/html",
      Location: "/" + new URLSearchParams(event.queryStringParameters),
    },
    body: '<a href="https://aesthetic.computer">https://aesthetic.computer</a>',
  };

  let statusCode = 200; // Might change if a piece can't load.

  // Load and pre-process a piece's source code, then run it's `meta` function.
  let sourceCode, module;

  try {
    // Externally hosted pieces always start with @.
    if (slug.startsWith("@") && slug.indexOf("/") !== -1) {
      const externalPiece = await getPage(
        `https://${parsed.host}/${parsed.path}.mjs`,
      );
      sourceCode = externalPiece.data;
      if (externalPiece?.code !== 200) return redirect;
    } else {
      // Locally hosted piece.
      try {
        let path = parsed.path.replace("aesthetic.computer/disks/", "");
        if (path.startsWith("@")) path = "profile";
        try {

          // ❤️‍🔥 
          // TODO: Try to print the file system heirarchy / find these files...

          sourceCode = await fs.readFile(
            `${dev ? "./" : "/var/task/system/"}public/aesthetic.computer/disks/${path}.mjs`,
            "utf8",
          );
        } catch (err) {
          console.error("📃 Error reading or importing source code:", err);
          statusCode = 404;
          respond(statusCode, `Content not found: ${path}`);
          // throw err;
        }
      } catch (e) {
        console.log("🔴 Piece load failure...");
        const anonUrl = `https://art.aesthetic.computer/${
          parsed.path.split("/").pop().mjs
        }`;
        console.log("📥 Attempting to load piece from anon:", anonUrl);
        const externalPiece = await getPage(anonUrl);
        sourceCode = externalPiece.data;
        if (externalPiece?.code !== 200) statusCode = 404;
      }
    }

    if (sourceCode) {
      // TODO: 🟣 How can I try to read the module's meta function here...
      //       does it need to pass through a proxy or something?

      // TODO: 🧡 This should also work for kidlisp pieces somehow...
      // console.log("Updated source:", sourceCode, "UPDATED SOURCE^");
      const originalCode = sourceCode;

      const currentDirectory = dev ? process.cwd() : "/var/task/system";

      // Log the current working directory
      console.log("🚗 Current Directory:", currentDirectory);

      sourceCode = updateCode(
        sourceCode,
        dev ? "localhost:8888" : event.headers["host"],
        dev,
        (event.headers["x-forwarded-proto"] || "https") + ":", //,
        true,
        currentDirectory,
      );

      const tempPath = path.join("/tmp", `${slug}.mjs`);

      try {
        await fs.writeFile(tempPath, sourceCode);
        // console.log(sourceCode);
        module = await import(`file://${tempPath}`);
        // console.log("Module:", module);
      } catch (err) {
        console.log("⚠️ Import error:", err, tempPath);
      } finally {
        await fs.unlink(tempPath);
      }

      meta =
        module?.meta?.({ ...parsed, num }) ||
        (() => {
          // Parse the source for a potential title and description.
          let title = "",
            desc = "";
          const lines = originalCode.split("\n");
          if (lines[0].startsWith("//")) {
            title = lines[0].split(",")[0].slice(3).trim();
          }
          if (lines[1].startsWith("//")) desc = lines[1].slice(3).trim();
          return { title, desc };
        })(); // Parse any special piece metadata if it exists.

      console.log("📰 Metadata:", meta, "Path:", parsed.text);
    }
  } catch {
    // If either module doesn't load, then we can fallback to the main route.
    return redirect;
  }

  const { title, desc, ogImage, icon, twitterImage } = metadata(
    event.headers["host"],
    slug,
    meta,
  );

  const body = html`
    <!doctype html>
    <html>
      <head>
        <meta charset="utf-8" />
        <title>${title}</title>
        <link rel="icon" href="${icon}" type="image/png" />
        <meta
          name="viewport"
          content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
        />
        <meta name="description" content="${encode(desc)}" />
        <meta name="og:title" content="${encode(title)}" />
        <meta name="og:description" content="${encode(desc)}" />
        <meta name="og:image" content="${ogImage}" />
        <meta name="twitter:card" content="summary_large_image" />
        <meta name="twitter:title" content="${encode(title)}" />
        <meta name="twitter:site" content="aesthetic.computer" />
        <meta name="twitter:image" content="${twitterImage}" />
        ${dev
          ? ""
          : `<!-- <script crossorigin="anonymous" src="https://js.sentry-cdn.com/ef4704c0df6a410e972bca14d69e1898.min.js"></script> -->`}
        <script
          crossorigin="anonymous"
          src="/aesthetic.computer/boot.mjs"
          type="module"
          defer
        ></script>
        <!-- Google tag (gtag.js) -->
        <script
          async
          src="https://www.googletagmanager.com/gtag/js?id=G-B4TLVYKXVF"
        ></script>
        <script>
          window.dataLayer = window.dataLayer || [];
          function gtag() {
            dataLayer.push(arguments);
          }
          gtag("js", new Date());
          gtag("config", "G-B4TLVYKXVF");
        </script>
        <link
          rel="stylesheet"
          crossorigin="anonymous"
          href="/aesthetic.computer/style.css"
        />
      </head>
      <body class="native-cursor" ${lanHost ? " data-lan-host=" + lanHost : ""}>
        <script>
          if (window.self !== window.top) document.body.classList.add("embed");
        </script>
      </body>
    </html>
  `;
  return {
    statusCode,
    headers: {
      "Content-Type": "text/html",
      // "Cross-Origin-Embedder-Policy": "require-corp",
      "Cross-Origin-Opener-Policy": "same-origin-allow-popups",
      "Cross-Origin-Resource-Policy": "cross-origin",
    },
    body,
    ttl: 60,
  };
}

async function getPage(url) {
  return new Promise((resolve, reject) => {
    let data = "";
    const options = dev
      ? { agent: new https.Agent({ rejectUnauthorized: false }) }
      : {};
    https
      .get(url, options, (res) => {
        res.on("data", (chunk) => {
          data += chunk;
        });
        res.on("end", () => {
          resolve({ data, code: res.statusCode });
        });
      })
      .on("error", (e) => {
        console.log("Error:", e);
        reject(e);
      });
  });
}

export const handler = fun;
