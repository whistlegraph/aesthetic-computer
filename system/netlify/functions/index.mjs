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
  inferTitleDesc,
  updateCode,
} from "../../public/aesthetic.computer/lib/parse.mjs";
import { respond } from "../../backend/http.mjs";
import { defaultTemplateStringProcessor as html } from "../../public/aesthetic.computer/lib/helpers.mjs";
import { networkInterfaces } from "os";
const dev = process.env.CONTEXT === "dev";

async function fun(event, context) {
  // TODO: Return a 500 or 404 for everything that does not exist...
  //       - [] Like for example if the below import fails...
  console.log("📁 index ➡️", event.path);

  if (
    event.path === "/favicon.ico" ||
    event.path === "/requestProvider.js.map"
  ) {
    return { statusCode: 500 };
  }

  if (event.headers["host"] === "sotce.local:8888") {
    return respond(
      302,
      '<a href="https://localhost:8888/sotce-net">https://localhost:8888/sotce-net</a>',
      {
        "Content-Type": "text/html",
        Location: "https://localhost:8888/sotce-net",
      },
    );
  }
  // console.log("😃", __dirname, __filename);

  let slug = event.path.slice(1) || "prompt";

  // Prevent loading of .json, font, or other non-code files as Lisp/JS pieces
  const forbiddenExtensions = [".json", ".ttf", ".otf", ".woff", ".woff2", ".eot", ".svg", ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".ico"];
  for (const ext of forbiddenExtensions) {
    if (slug.endsWith(ext)) {
      console.log(`⛔ Blocked attempt to load forbidden file type: ${slug}`);
      return respond(404, `File type not allowed: ${ext}`);
    }
  }

  // Safely decode URL-encoded characters in the slug
  try {
    slug = decodeURIComponent(slug);
  } catch (error) {
    console.log("⚠️ Failed to decode URL slug:", slug, "Error:", error.message);
    // If decoding fails, fall back to simple Unicode character decoding
    slug = slug
      .replace(/%C2%A7/g, "\n") // UTF-8 encoded § to newline
      .replace(/%C2%A4/g, "%") // UTF-8 encoded ¤ to percent
      .replace(/%C2%A8/g, ";") // UTF-8 encoded ¨ to semicolon
      .replace(/§/g, "\n") // Direct § to newline (in case not URL-encoded)
      .replace(/¤/g, "%") // Direct ¤ to percent (in case not URL-encoded)
      .replace(/¨/g, ";") // Direct ¨ to semicolon (in case not URL-encoded)
      // Standard URL decoding
      .replace(/%28/g, "(")
      .replace(/%29/g, ")")
      .replace(/%20/g, " ");
  }

  // console.log("Path:", event.path, "Host:", event.headers["host"]);

  // Some domains will rewrite the initial slug.
  if (event.headers["host"] === "botce.ac") {
    slug = "botce";
  } else if (event.headers["host"] === "wipppps.world" || event.headers["host"] === "www.wipppps.world") {
    slug = "wipppps";
  } else if (event.headers["host"] === "sundarakarma.com" || event.headers["host"] === "www.sundarakarma.com") {
    slug = "sundarakarma.com";
  } else if (
    event.headers["host"] === "m2w2.whistlegraph.com" &&
    event.path.length <= 1
  ) {
    slug = "wg~m2w2";
  }

  // Handle kidlisp:code URL pattern and convert to $code format
  const originalPath = event.path.slice(1) || "prompt"; // Store original for redirect check
  if (slug.startsWith("kidlisp:") && slug.length > 8) {
    const code = slug.slice(8); // Remove "kidlisp:" prefix
    const newSlug = `$${code}`; // Convert to $code format
    console.log(`🔄 Converting kidlisp:${code} to $${code} and redirecting`);
    
    // Redirect to the $code format to update the URL bar
    return respond(
      302,
      `<a href="/${newSlug}">Redirecting to /${newSlug}</a>`,
      {
        "Content-Type": "text/html",
        Location: `/${newSlug}`,
      },
    );
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

  // Load and pre-process a piece's source code, then run it's `meta` function.
  let statusCode = 200,
    sourceCode,
    language = "javascript", // Might switch to 'lisp' if necessary.
    module,
    fromHandle = false;

  try {
    // Externally hosted pieces always start with @.
    if (slug.startsWith("@") && slug.indexOf("/") !== -1) {
      const baseUrl = `https://${event.headers["host"]}/${parsed.path}`;

      console.log("🧔🧩 Loading handled piece:", `${baseUrl}.mjs`);
      try {
        let handledPiece = await getPage(`${baseUrl}.mjs`);

        // Try to load the Lisp source if the .mjs file is not found
        if (handledPiece?.code !== 200) {
          console.log("🧔🧩 .mjs not found, trying .lisp:", `${baseUrl}.lisp`);
          handledPiece = await getPage(`${baseUrl}.lisp`);
          language = "lisp";
        }

        if (handledPiece?.code !== 200) {
          statusCode = 404;
          // return respond(statusCode, `Content not found: ${path}`);
        } else {
          sourceCode = handledPiece.data;
          fromHandle = true;
        }
      } catch (err) {
        console.log("Failed to load handled piece:", err);
      }

      // const url = `https://${event.headers["host"]}/${parsed.path}.mjs`;
      // console.log("🧔🧩 Loading handled piece:", url);
      // try {
      //   const handledPiece = await getPage(url);

      //   // TODO: This should also be able to handle lisp source.

      //   if (handledPiece?.code !== 200) {
      //     statusCode = 404;
      //     respond(statusCode, `Content not found: ${path}`);
      //   }
      //   sourceCode = handledPiece.data;
      //   fromHandle = true;
      // } catch (err) {
      //   console.log("Failed to load handled piece:", err);
      // }
    } else {
      // Locally hosted piece.
      try {
        let path = parsed.path.replace("aesthetic.computer/disks/", "");
        if (path.startsWith("@")) path = "profile";

        // Handle special kidlisp path case
        if (path === "(...)" || path === "(...)") {
          // This is inline kidlisp code, not a file to load
          console.log("🤖 Detected inline kidlisp, skipping file load");
          sourceCode = null; // No source code to load
        } else {
          try {
            const basePath = `${dev ? "./" : "/var/task/system/"}public/aesthetic.computer/disks/${path}`;
            try {
              sourceCode = await fs.readFile(`${basePath}.mjs`, "utf8");
            } catch (errJavaScript) {
              try {
                sourceCode = await fs.readFile(`${basePath}.lisp`, "utf8");
                language = "lisp";
              } catch (errLisp) {
                console.error(
                  "📃 Error reading or importing source code (both .mjs and .lisp failed):",
                  errJavaScript,
                  errLisp,
                );
                statusCode = 404;
                // return respond(statusCode, `Content not found: ${path}`);
              }
            }
          } catch (err) {
            console.error("📃 Error:", err);
            statusCode = 404;
            // return respond(statusCode, `Content not found: ${path}`);
            // throw err;
          }
        }
      } catch (e) {
        console.log("🔴 Piece load failure...");
        const anonUrl = `https://art.aesthetic.computer/${
          parsed.path.split("/").pop() + ".mjs"
        }`;
        console.log("📥 Attempting to load piece from anon:", anonUrl);
        const externalPiece = await getPage(anonUrl);
        sourceCode = externalPiece.data;
        if (externalPiece?.code !== 200) statusCode = 404;
      }
    }

    // TODO: ❤️‍🔥 How will this work for handled pieces?

    if (sourceCode) {
      const originalCode = sourceCode;
      let currentDirectory = process.cwd();
      if (!dev) currentDirectory += "/system";
      // console.log("🚗 Current Directory:", currentDirectory);

      sourceCode = updateCode(
        sourceCode,
        dev ? "localhost:8888" : event.headers["host"],
        dev,
        (event.headers["x-forwarded-proto"] || "https") + ":", //,
        fromHandle ? false : true,
        fromHandle ? undefined : currentDirectory,
      );

      // const tempPath = path.join("/tmp", `${slug.replaceAll("/", "-")}.mjs`);

      // try {
      //   await fs.writeFile(tempPath, sourceCode);
      //   if (language === "javascript")
      //     module = await import(`file://${tempPath}`);
      // } catch (err) {
      //   console.log("⚠️ Import error:", err, tempPath);
      // } finally {
      //   await fs.unlink(tempPath);
      // }

      const tempPath = path.join("/tmp", `${slug.replaceAll("/", "-")}.mjs`);

      try {
        console.log(
          "📖 Writing to:",
          tempPath,
          "Source length:",
          sourceCode?.length,
        );
        await fs.writeFile(tempPath, sourceCode);
        if (language === "javascript")
          module = await import(`file://${tempPath}`);
        // TODO: This fails in development sometimes, still not sure why...
      } catch (err) {
        console.error("⚠️ Import error:", err);
        try {
          await fs.access(tempPath);
          const contents = await fs.readFile(tempPath, "utf8");
          // console.error("🪵 Temp file contents:\n", contents);
        } catch (accessErr) {
          console.error("❌ Temp file does not exist:", tempPath);
        }
      } finally {
        try {
          await fs.unlink(tempPath);
          // console.log("🧹 Cleaned up temp file.");
        } catch (e) {
          // console.warn("⚠️ Failed to delete temp file:", e);
        }
      }
      console.log("🧊 Module:", module?.meta, tempPath);
      
      // Get initial metadata from module or infer it
      meta = module?.meta?.({ ...parsed, num }) || inferTitleDesc(originalCode);
      
      // Override title with first line comment if it's a Lisp file and no meta.title exists
      if (language === "lisp" && originalCode && (!meta?.title || meta.title === parsed.text)) {
        const firstLine = originalCode.split('\n')[0]?.trim();
        if (firstLine && firstLine.startsWith(';')) {
          const title = firstLine.substring(1).trim(); // Remove semicolon and trim whitespace
          if (title) {
            meta = { ...meta, title, standaloneTitle: true };
          }
        }
      }
      console.log("📰 Metadata:", meta, "Path:", parsed.text);
    } else if (parsed.source) {
      // Handle inline kidlisp code that doesn't need file loading
      console.log("🤖 Using inline kidlisp source for metadata");
      
      // Get initial metadata from inference
      meta = inferTitleDesc(parsed.source);
      
      // Override title with first line comment if it exists and current title is default
      const firstLine = parsed.source.split('\n')[0]?.trim();
      if (firstLine && firstLine.startsWith(';') && (!meta?.title || meta.title === parsed.text)) {
        const title = firstLine.substring(1).trim(); // Remove semicolon and trim whitespace
        if (title) {
          meta = { ...meta, title, standaloneTitle: true };
        }
      }
      
      console.log(
        "📰 Kidlisp Metadata:",
        meta,
        "Source:",
        parsed.source?.substring(0, 100) + "...",
      );
    }
  } catch (err) {
    // If either module doesn't load, then we can fallback to the main route.
    console.log("🔴 Error loading module:", err, sourceCode);
    return redirect;
  }

  const { title, desc, ogImage, icon, twitterImage, manifest } = metadata(
    event.headers["host"],
    slug,
    meta,
  );

  // TODO: Not sure if 'location' is correct here, but I wan tto skip rendering the link rel icon and og:image if the icon or preview parameter is present
  //      in the request url qury params...
  const qsp = event.queryStringParameters || {};
  const previewOrIcon = "icon" in qsp || "preview" in qsp;

  const body = html`
    <!doctype html>
    <html>
      <head>
        <meta charset="utf-8" />
        <title>${title}</title>
        ${!previewOrIcon
          ? html`<link rel="icon" href="${icon}" type="image/png" />`
          : ""}
        ${!previewOrIcon
          ? html`<link rel="apple-touch-icon" href="${icon}" />`
          : ""}
        <link rel="manifest" href="${manifest}" />
        <meta
          name="viewport"
          content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
        />
        <meta name="description" content="${encode(desc)}" />
        <meta name="og:title" content="${encode(title)}" />
        <meta name="og:description" content="${encode(desc)}" />
        ${!previewOrIcon
          ? html`<meta name="og:image" content="${ogImage}" />`
          : ""}
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
        <div id="console" class="hidden">booting...</div>
        <script>
          if (window.self !== window.top) document.body.classList.add("embed");
          // Apply nogap class immediately if parameter is present to prevent flash
          const params = new URLSearchParams(location.search);
          if (params.has("nogap") || location.search.includes("nogap")) {
            document.body.classList.add("nogap");
          }
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
