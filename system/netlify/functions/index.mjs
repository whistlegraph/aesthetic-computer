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
import { handleFromPermahandle } from "../../backend/authorization.mjs";
import { defaultTemplateStringProcessor as html } from "../../public/aesthetic.computer/lib/helpers.mjs";
import { networkInterfaces } from "os";
const dev = process.env.CONTEXT === "dev" || process.env.NETLIFY_DEV === "true";

// Fire-and-forget piece hit tracking (don't await, don't block page load)
async function trackPieceHit(piece, type) {
  try {
    const baseUrl = dev ? "https://localhost:8888" : "https://aesthetic.computer";
    const { got } = await import("got");
    await got.post(`${baseUrl}/api/piece-hit`, {
      json: { piece, type },
      // Don't pass auth header - piece hits are anonymous for now
      // This avoids Auth0 calls on every page load
      https: { rejectUnauthorized: false },
      timeout: { request: 5000 },
    });
  } catch (e) {
    // Silent fail - don't let tracking break page loads
    if (dev) console.log("📊 Hit tracking failed:", e.message);
  }
}

async function fun(event, context) {
  const _startTime = Date.now();
  
  // 🐛 DEBUG: Log the actual path resolution for troubleshooting
  console.log(`🔍 DEBUG index.mjs: path=${event.path}, dev=${dev}, __dirname=${typeof __dirname !== 'undefined' ? __dirname : 'N/A'}`);
  
  try {
    // TODO: Return a 500 or 404 for everything that does not exist...
    //       - [] Like for example if the below import fails...

    if (
      event.path === "/favicon.ico" ||
      event.path === "/requestProvider.js.map"
    ) {
      return { statusCode: 500 };
    }

  // Serve system .mjs files directly as static assets
  // These get caught by the catch-all redirect but shouldn't go through piece loading
  if (event.path.endsWith(".mjs") && !event.path.startsWith("/disks/")) {
    try {
      // On Netlify, with base="system", files are at /var/task/ (not /var/task/system/)
      const baseDir = dev ? process.cwd() : "/var/task";
      const filePath = path.join(baseDir, "public/aesthetic.computer", event.path.slice(1));
      const content = await fs.readFile(filePath, "utf8");
      return {
        statusCode: 200,
        headers: { 
          "Content-Type": "application/javascript",
          "Cache-Control": "public, max-age=60"
        },
        body: content,
      };
    } catch (err) {
      console.log(`⚡ System .mjs file not found: ${event.path}, tried: ${path.join(dev ? process.cwd() : "/var/task", "public/aesthetic.computer", event.path.slice(1))}`);
      return { statusCode: 404, body: `File not found: ${event.path}` };
    }
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

  // Serve specific kidlisp.com pages before the catch-all
  // /kidlisp.com/device* → device.html (FF1 optimized display)
  // /device.kidlisp.com/* → device.html (local dev path for device.kidlisp.com)
  // /top.kidlisp.com/* → device.html (local dev path for top.kidlisp.com - auto-loads top100)
  // /kidlisp.com/pj* → pj.html (PJ mode)
  if (event.path.startsWith("/kidlisp.com/device") || 
      event.path.startsWith("/device.kidlisp.com") ||
      event.path.startsWith("/top.kidlisp.com")) {
    try {
      const htmlContent = await fs.readFile(
        path.join(process.cwd(), "public/kidlisp.com/device.html"),
        "utf8"
      );
      return {
        statusCode: 200,
        headers: { "Content-Type": "text/html" },
        body: htmlContent,
      };
    } catch (err) {
      console.error("❌ Error serving kidlisp.com/device:", err);
      return respond(500, "Error loading kidlisp.com device mode");
    }
  }
  
  if (event.path.startsWith("/kidlisp.com/pj")) {
    try {
      const htmlContent = await fs.readFile(
        path.join(process.cwd(), "public/kidlisp.com/pj.html"),
        "utf8"
      );
      return {
        statusCode: 200,
        headers: { "Content-Type": "text/html" },
        body: htmlContent,
      };
    } catch (err) {
      console.error("❌ Error serving kidlisp.com/pj:", err);
      return respond(500, "Error loading kidlisp.com PJ mode");
    }
  }

  // Serve kidlisp.com/index.html for all /kidlisp.com/* paths (SPA routing)
  // This handles paths like /kidlisp.com, /kidlisp.com/, /kidlisp.com/$abc
  if (event.path.startsWith("/kidlisp.com")) {
    try {
      const htmlContent = await fs.readFile(
        path.join(process.cwd(), "public/kidlisp.com/index.html"),
        "utf8"
      );
      return {
        statusCode: 200,
        headers: { "Content-Type": "text/html" },
        body: htmlContent,
      };
    } catch (err) {
      console.error("❌ Error serving kidlisp.com:", err);
      return respond(500, "Error loading kidlisp.com");
    }
  }

  // Serve jas.life/index.html for /jas.life path
  if (event.path === "/jas.life" || event.path.startsWith("/jas.life/")) {
    try {
      const htmlContent = await fs.readFile(
        path.join(process.cwd(), "public/jas.life/index.html"),
        "utf8"
      );
      return {
        statusCode: 200,
        headers: { "Content-Type": "text/html" },
        body: htmlContent,
      };
    } catch (err) {
      console.error("❌ Error serving jas.life:", err);
      return respond(500, "Error loading jas.life");
    }
  }

  // Serve GIF files directly as static assets (for mockup previews, etc.)
  if (event.path.endsWith(".gif")) {
    try {
      const baseDir = dev ? process.cwd() : "/var/task";
      const gifPath = path.join(baseDir, "public/aesthetic.computer", event.path.slice(1));
      const gifBuffer = await fs.readFile(gifPath);
      return {
        statusCode: 200,
        headers: { 
          "Content-Type": "image/gif",
          "Cache-Control": "public, max-age=60"
        },
        body: gifBuffer.toString("base64"),
        isBase64Encoded: true,
      };
    } catch (err) {
      console.error("❌ Error serving GIF:", event.path, err.message);
      return respond(404, "GIF not found");
    }
  }

  // Serve WebP files directly as static assets (for animated mockup previews with transparency)
  if (event.path.endsWith(".webp")) {
    try {
      const baseDir = dev ? process.cwd() : "/var/task";
      const webpPath = path.join(baseDir, "public/aesthetic.computer", event.path.slice(1));
      const webpBuffer = await fs.readFile(webpPath);
      return {
        statusCode: 200,
        headers: { 
          "Content-Type": "image/webp",
          "Cache-Control": "public, max-age=60"
        },
        body: webpBuffer.toString("base64"),
        isBase64Encoded: true,
      };
    } catch (err) {
      console.error("❌ Error serving WebP:", event.path, err.message);
      return respond(404, "WebP not found");
    }
  }

  // console.log("😃", __dirname, __filename);

  let slug = event.path.slice(1) || "prompt";

  // Solo mode: trailing `|` is syntactic sugar for ?solo
  // e.g., /notepat| → /notepat?solo (302 redirect)
  if (slug.endsWith("|")) {
    const cleanSlug = slug.slice(0, -1);
    const existingParams = event.queryStringParameters || {};
    const paramStr = Object.entries({ ...existingParams, solo: "true" })
      .map(([k, v]) => v === "true" ? k : `${k}=${v}`)
      .join("&");
    return {
      statusCode: 302,
      headers: { Location: `/${cleanSlug}?${paramStr}` },
    };
  }

  // Handle direct requests to /disks/ paths (static asset requests)
  if (slug.startsWith("disks/")) {
    // For direct disk file requests, strip the "disks/" prefix
    // so "/disks/prompt.mjs" becomes "prompt.mjs"
    slug = slug.substring(6); // Remove "disks/"
    
    // Also strip the file extension if present, since the netlify function expects
    // the piece name without extension
    if (slug.endsWith(".mjs")) {
      slug = slug.slice(0, -4); // Remove ".mjs"
    } else if (slug.endsWith(".lisp")) {
      slug = slug.slice(0, -5); // Remove ".lisp"
    }
  }

  // Prevent loading of .json, font, or other non-code files as Lisp/JS pieces
  // Note: .gif is allowed for mockup previews served from /aesthetic.computer/
  const forbiddenExtensions = [".json", ".ttf", ".otf", ".woff", ".woff2", ".eot", ".svg", ".png", ".jpg", ".jpeg", ".bmp", ".ico"];
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
    console.log("⚠️ Failed to decode URL slug (likely contains literal % or # symbols):", slug);
  // If decoding fails, leave slug as-is for legacy % symbols (e.g., old tape %code links)
    // But still handle common escape sequences manually
    slug = slug
      .replace(/%C2%A7/g, "\n") // UTF-8 encoded § to newline
      .replace(/%C2%A4/g, "%") // UTF-8 encoded ¤ to percent
      .replace(/%C2%A8/g, ";") // UTF-8 encoded ¨ to semicolon
      .replace(/%23/g, "#") // URL-encoded hash to #
      .replace(/§/g, "\n") // Direct § to newline (in case not URL-encoded)
      .replace(/¤/g, "%") // Direct ¤ to percent (in case not URL-encoded)
      .replace(/¨/g, ";") // Direct ¨ to semicolon (in case not URL-encoded)
      // Standard URL decoding (safe ones only)
      .replace(/%28/g, "(")
      .replace(/%29/g, ")")
      .replace(/%20/g, " ");
  // Note: Legacy % symbols (like in "tape %JyK") are left as-is
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
  } else if (
    (event.headers["host"] === "notepat.com" || event.headers["host"] === "www.notepat.com") &&
    event.path.length <= 1
  ) {
    slug = "notepat";
  }

  // Handle kidlisp:code URL pattern and convert to $code format
  const originalPath = event.path.slice(1) || "prompt"; // Store original for redirect check
  if (slug.startsWith("kidlisp:") && slug.length > 8) {
    const code = slug.slice(8); // Remove "kidlisp:" prefix
    const newSlug = `$${code}`; // Convert to $code format
    console.log(`[kidlisp] Converting kidlisp:${code} to $${code} and redirecting`);
    
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

  // Handle clock:code URL pattern and convert to *code format
  if (slug.startsWith("clock:") && slug.length > 6) {
    const code = slug.slice(6); // Remove "clock:" prefix
    const newSlug = `*${code}`; // Convert to *code format
    console.log(`[clock] Converting clock:${code} to *${code} and redirecting`);
    
    return respond(
      302,
      `<a href="/${newSlug}">Redirecting to /${newSlug}</a>`,
      {
        "Content-Type": "text/html",
        Location: `/${newSlug}`,
      },
    );
  }

  // 🎄 Handle mo.XX and merryo.XX uniform timing shorthand URLs
  // e.g., /mo.1:a:b:c → /mo~.1:a:b:c (piece=mo, timing=.1, pieces=a,b,c)
  // e.g., /merryo.05:tone:clock → /merryo~.05:tone:clock
  const moMatch = slug.match(/^(mo|merryo)\.(\d+(?:\.\d+)?)([:~].+)?$/);
  if (moMatch) {
    const [, prefix, timing, rest] = moMatch;
    // Route to the mo piece with timing as first colon param
    const newSlug = `mo~.${timing}${rest || ""}`;
    console.log(`[merry] Converting ${slug} to ${newSlug}`);
    return respond(
      302,
      `<a href="/${newSlug}">Redirecting to /${newSlug}</a>`,
      {
        "Content-Type": "text/html",
        Location: `/${encodeURIComponent(newSlug)}`,
      },
    );
  }

  // Handle *xxx clock shortcode - serve page directly, let client fetch melody
  // The client-side disk.mjs normalizes *code to clock piece and preserves the URL
  if (slug.startsWith("*") && slug.length > 1 && !slug.includes("~")) {
    const code = slug.slice(1); // Remove * prefix
    console.log(`[clock] Serving clock shortcode: *${code} (client will fetch melody)`);
    // Fall through to normal page rendering - client handles the rest
  }

  // Handle permahandle URLs (e.g., /ac25namuc → /@jeffrey)
  // Permahandles are 9 chars starting with "ac"
  if (slug.length === 9 && slug.startsWith("ac") && /^ac[0-9]{2}[a-z]{5}$/.test(slug)) {
    console.log(`[permahandle] Checking if ${slug} is a valid permahandle...`);
    try {
      const result = await handleFromPermahandle(slug);
      if (result?.handle) {
        console.log(`[permahandle] Found: ${slug} → @${result.handle}`);
        // Redirect to the user's profile
        return respond(
          302,
          `<a href="/@${result.handle}">Redirecting to /@${result.handle}</a>`,
          {
            "Content-Type": "text/html",
            Location: `/@${result.handle}`,
          },
        );
      }
    } catch (err) {
      console.log(`[permahandle] Lookup failed for ${slug}:`, err.message);
    }
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
        // Strip the "aesthetic.computer/disks/" prefix from parsed.path
        // Handle cases where it might be duplicated
        let path = parsed.path;
        
        // Remove all occurrences of "aesthetic.computer/disks/" 
        while (path.startsWith("aesthetic.computer/disks/")) {
          path = path.substring("aesthetic.computer/disks/".length);
        }
        
        if (path.startsWith("@")) path = "profile";

        // Skip API paths - these are handled by separate functions
        if (path.startsWith("api/")) {
          console.log("[index] Skipping API path:", path);
          sourceCode = null;
        // Handle special kidlisp path case
        } else if (path === "(...)" || path === "(...)") {
          // This is inline kidlisp code, not a file to load
          console.log("[kidlisp] Detected inline kidlisp, skipping file load");
          sourceCode = null; // No source code to load
        // Handle $code nanoid pieces - these load source from MongoDB client-side
        } else if (path.startsWith("$") && path.length >= 4 && /^\$[a-zA-Z0-9]+$/.test(path)) {
          console.log("[kidlisp] Detected $code piece, skipping file load:", path);
          sourceCode = null; // Source loaded client-side from MongoDB
          statusCode = 200; // Ensure we return 200 for valid $code pieces
        // Handle *code clock pieces - these load source from MongoDB client-side and route to clock.mjs
        } else if (path.startsWith("*") && path.length >= 4 && /^\*[a-zA-Z0-9]+$/.test(path)) {
          console.log("[clock] Detected *code piece, skipping file load:", path);
          sourceCode = null; // Source loaded client-side from MongoDB
          statusCode = 200; // Ensure we return 200 for valid *code pieces
        } else {
          try {
            const basePath = `${dev ? "./" : "/var/task/"}public/aesthetic.computer/disks/${path}`;
            console.log(`🔍 DEBUG: Trying to load disk from basePath=${basePath}`);
            try {
              sourceCode = await fs.readFile(`${basePath}.mjs`, "utf8");
              console.log(`🔍 DEBUG: Successfully loaded ${basePath}.mjs`);
            } catch (errJavaScript) {
              console.log(`🔍 DEBUG: Failed to load .mjs: ${errJavaScript.message}`);
              try {
                sourceCode = await fs.readFile(`${basePath}.lisp`, "utf8");
                console.log(`🔍 DEBUG: Successfully loaded ${basePath}.lisp`);
                language = "lisp";
              } catch (errLisp) {
                console.error(
                  "📃 Error reading or importing source code (both .mjs and .lisp failed):",
                  errJavaScript,
                  errLisp,
                );
                console.log(`🔍 DEBUG: Both .mjs and .lisp failed for ${basePath}`);
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
    } else if (parsed.source) {
      // Handle inline kidlisp code that doesn't need file loading
      console.log("[kidlisp] Using inline kidlisp source for metadata");
      
      // Reset status to 200 for inline kidlisp pieces
      statusCode = 200;
      
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
    }
  } catch (err) {
    // If either module doesn't load, then we can fallback to the main route.
    console.log("🔴 Error loading module:", err, sourceCode);
    return redirect;
  }

  const { title, desc, ogImage, icon, iconWebp, twitterImage, manifest } = metadata(
    event.headers["host"],
    slug,
    meta,
    "https:", // Server-side defaults to HTTPS
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
        ${!previewOrIcon && iconWebp
          ? html`<link rel="icon" href="${iconWebp}" type="image/webp" />`
          : ""}
        ${!previewOrIcon
          ? html`<link rel="icon" href="${icon}" type="image/png" />`
          : ""}
        ${!previewOrIcon
          ? html`<link rel="apple-touch-icon" href="${icon}" />`
          : ""}
        <link rel="manifest" href="${manifest}" />
        <meta name="theme-color" content="#000000" />
        <meta name="mobile-web-app-capable" content="yes" />
        <meta name="apple-mobile-web-app-capable" content="yes" />
        <meta name="apple-mobile-web-app-status-bar-style" content="black" />
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
        <script>
          // 🎹 DAW Sync for Max for Live - must be defined before any modules load
          // These functions queue messages until bios.mjs connects via window.acDawConnect()
          (function() {
            var dawQueue = [];
            var dawSend = null;
            var dawBpm = 60;
            var dawPlaying = null; // null so first update always fires
            var dawSampleRateReceived = false; // Track if we've received sample rate from M4L
            var dawWaitingForSampleRate = false; // Track if transport started before sample rate arrived
            
            function send(msg) {
              if (dawSend) {
                dawSend(msg);
              } else {
                dawQueue.push(msg);
              }
            }
            
            window.acDawTempo = function(bpm) {
              var newBpm = Math.round(bpm);
              if (newBpm !== dawBpm && newBpm > 0) {
                console.log("🎹 DAW tempo: " + newBpm + " BPM");
                dawBpm = newBpm;
                send({ type: "daw:tempo", content: { bpm: newBpm } });
              }
            };
            
            window.acDawTransport = function(playing) {
              var isPlaying = playing === 1;
              if (isPlaying !== dawPlaying) {
                console.log("🎹 DAW transport: " + (isPlaying ? "playing" : "stopped"));
                dawPlaying = isPlaying;
                send({ type: "daw:transport", content: { playing: isPlaying } });
                
                // 🎹 When DAW starts playing, activate audio by simulating a click
                // This resumes the AudioContext which is suspended by default in jweb~
                // BUT: Wait for sample rate to arrive first so AudioContext uses correct rate
                if (isPlaying) {
                  if (dawSampleRateReceived) {
                    console.log("🎹 DAW play: Activating audio (sample rate already received)...");
                    // Try dispatching to canvas first (where bios listens), then document
                    var canvas = document.querySelector('canvas');
                    var target = canvas || document.body;
                    console.log("🎹 Dispatching pointer event to:", target.tagName);
                    
                    // Dispatch both pointerdown and click events
                    var pointerEvent = new PointerEvent('pointerdown', {
                      bubbles: true,
                      cancelable: true,
                      view: window,
                      clientX: 100,
                      clientY: 100,
                      pointerId: 1,
                      pointerType: 'mouse',
                      isPrimary: true,
                      button: 0,
                      buttons: 1
                    });
                    target.dispatchEvent(pointerEvent);
                    
                    // Also dispatch a regular click as backup
                    var clickEvent = new MouseEvent('click', {
                      bubbles: true,
                      cancelable: true,
                      view: window,
                      clientX: 100,
                      clientY: 100,
                      button: 0,
                      buttons: 1
                    });
                    target.dispatchEvent(clickEvent);
                  } else {
                    console.log("🎹 DAW play: Waiting for sample rate before activating audio...");
                    dawWaitingForSampleRate = true;
                  }
                }
              }
            };
            
            window.acDawPhase = function(phase) {
              send({ type: "daw:phase", content: { time: phase } });
            };
            
            window.acDawSamplerate = function(sr) {
              console.log("🎹 DAW sample rate received:", sr);
              dawSampleRateReceived = true;
              send({ type: "daw:samplerate", content: { samplerate: sr } });
              
              // If we were waiting for sample rate to activate audio, do it now
              if (dawWaitingForSampleRate) {
                console.log("🎹 Sample rate received, now activating audio...");
                dawWaitingForSampleRate = false;
                var canvas = document.querySelector('canvas');
                var target = canvas || document.body;
                var pointerEvent = new PointerEvent('pointerdown', {
                  bubbles: true,
                  cancelable: true,
                  view: window,
                  clientX: 100,
                  clientY: 100,
                  pointerId: 1,
                  pointerType: 'mouse',
                  isPrimary: true,
                  button: 0,
                  buttons: 1
                });
                target.dispatchEvent(pointerEvent);
              }
            };
            
            // 🎸 Pedal peak data receiver (for audio effect visualization)
            window.acPedalPeak = function(peak) {
              send({ type: "pedal:peak", content: { peak: peak } });
            };
            
            // 🎸 Pedal envelope data receiver (L/R peaks and RMS)
            window.acPedalEnvelope = function(peakL, peakR, rmsL, rmsR) {
              send({ type: "pedal:envelope", content: { peakL: peakL, peakR: peakR, rmsL: rmsL, rmsR: rmsR } });
            };
            
            // Called by bios.mjs to connect the message queue
            window.acDawConnect = function(sendFunc) {
              console.log("🎹 acDawConnect called, sendFunc type:", typeof sendFunc);
              dawSend = sendFunc;
              // Flush queued messages
              console.log("🎹 Flushing", dawQueue.length, "queued messages");
              for (var i = 0; i < dawQueue.length; i++) {
                dawSend(dawQueue[i]);
              }
              dawQueue = [];
              console.log("🎹 Max for Live detected - DAW sync connected");
            };
            
            // Signal to M4L that we're ready
            if (window.max) {
              console.log("🎹 Sent ready signal to Max");
              window.max.outlet("ready");
            }
          })();
        </script>
        <script>
          (function () {
            try {
              var rawHash = window.location && window.location.hash
                ? window.location.hash.slice(1)
                : "";
              var hash = rawHash ? rawHash.trim() : "";
              if (!hash) return;

              if (!/^[A-Za-z0-9]{3,12}$/.test(hash)) return;

              var cacheKey = "ac-painting-meta-" + hash;
              var cached = null;
              var storageAvailable = typeof sessionStorage !== "undefined";
              if (storageAvailable) {
                try {
                  cached = sessionStorage.getItem(cacheKey);
                } catch (_) {
                  storageAvailable = false;
                }
              }
              function normalizeMeta(raw) {
                try {
                  if (!raw) return null;
                  var code = (raw.code || raw.hash || hash || "").toString().replace(/^#/, "");
                  var slug = raw.slug || raw.slugCode || "";
                  if (!code || !slug) return null;
                  var handle = (raw.handle || "anon").replace(/^@+/, "");
                  var hasHandle = handle && handle !== "anon";
                  var displayHandle = hasHandle ? "@" + handle : "Anonymous";
                  var title = "#" + code + " • Aesthetic Computer";
                  var desc =
                    "Painting #" + code + (hasHandle ? " by " + displayHandle : "") +
                    " on aesthetic.computer";
                  var imageSlug = (hasHandle ? handle + "/painting/" + slug : slug) + ".png";
                  var image = "/api/pixel/2048:conform/" + encodeURI(imageSlug);
                  return {
                    code: code,
                    handle: handle,
                    slug: slug,
                    title: title,
                    desc: desc,
                    description: desc,
                    image: image,
                  };
                } catch (formatErr) {
                  console.warn("⚠️ Failed to normalize painting meta", formatErr);
                  return null;
                }
              }

              if (cached) {
                try {
                  var parsed = JSON.parse(cached);
                  var normalized = normalizeMeta(parsed);
                  if (normalized && normalized.code) {
                    applyMeta(normalized);
                    return;
                  }
                } catch (_) {}
              }

              fetch("/api/painting-code?code=" + encodeURIComponent(hash), {
                credentials: "omit",
              })
                .then(function (res) {
                  if (!res.ok) throw new Error("Painting lookup failed");
                  return res.json();
                })
                .then(function (data) {
                  if (!data || !data.slug) return;
                  var metaPayload = normalizeMeta({
                    code: data.code || hash,
                    handle: data.handle,
                    slug: data.slug,
                  });
                  if (!metaPayload) return;
                  if (storageAvailable) {
                    try {
                      sessionStorage.setItem(cacheKey, JSON.stringify(metaPayload));
                    } catch (_) {}
                  }
                  applyMeta(metaPayload);
                })
                .catch(function (err) {
                  console.warn("⚠️ Unable to hydrate painting metadata", err);
                });

              function applyMeta(payload) {
                try {
                  if (!payload) return;
                  var normalizedPayload = normalizeMeta(payload) || payload;
                  var desc = normalizedPayload.desc || normalizedPayload.description;
                  document.title = normalizedPayload.title;
                  setMeta('meta[name="description"]', "content", desc);
                  setMeta('meta[name="og:title"]', "content", normalizedPayload.title);
                  setMeta('meta[name="og:description"]', "content", desc);
                  setMeta('meta[name="og:image"]', "content", normalizedPayload.image);
                  setMeta('meta[name="twitter:title"]', "content", normalizedPayload.title);
                  setMeta('meta[name="twitter:description"]', "content", desc);
                  setMeta('meta[name="twitter:image"]', "content", normalizedPayload.image);
                  if (typeof window !== "undefined") {
                    window.acSTARTING_PAINTING_META = normalizedPayload;
                  }
                } catch (metaErr) {
                  console.warn("⚠️ Failed applying painting meta", metaErr);
                }
              }

              function setMeta(selector, attr, value) {
                var el = document.querySelector(selector);
                if (!el || !value) return;
                el.setAttribute(attr, value);
              }
            } catch (outerErr) {
              console.warn("⚠️ Painting meta bootstrap error", outerErr);
            }
          })();
        </script>
        <script>
          (function () {
            var rawHash = window.location && window.location.hash
              ? window.location.hash.slice(1)
              : "";
            var hash = rawHash ? rawHash.trim() : "";
            if (!hash) return;

            var lowered = hash.toLowerCase();
            var looksLikePaintingCode = /^(?:[A-Za-z0-9]{3,12})$/.test(hash) &&
              lowered !== "debug" &&
              lowered !== "nodebug";

            if (!looksLikePaintingCode) return;

            if (typeof window.acSTARTING_PIECE === "undefined") {
              window.acSTARTING_PIECE = "painting";
            }

            window.acSTARTING_HASH = hash;
          })();
        </script>
        <!-- 🚀 Eager WebSocket connection - starts BEFORE boot.mjs loads -->
        <script>
          (function() {
            // Start WebSocket connection immediately for faster module loading
            // boot.mjs will pick up this connection via window.acEarlyWS
            try {
              var isLocal = location.hostname === 'localhost' || location.hostname === '127.0.0.1';
              var wsUrl = isLocal ? 'wss://localhost:8889' : 'wss://session-server.aesthetic.computer';
              var ws = new WebSocket(wsUrl);
              var connected = false;
              var connectPromise = new Promise(function(resolve) {
                var timeout = setTimeout(function() {
                  if (!connected && ws.readyState !== 1) {
                    ws.close();
                    resolve(false);
                  }
                }, 800); // 800ms timeout
                ws.onopen = function() {
                  connected = true;
                  clearTimeout(timeout);
                  resolve(true);
                };
                ws.onerror = function() {
                  clearTimeout(timeout);
                  resolve(false);
                };
              });
              window.acEarlyWS = { ws: ws, connected: connectPromise, isConnected: function() { return connected; } };
            } catch(e) {
              window.acEarlyWS = null;
            }
          })();
        </script>
        <script
          crossorigin="anonymous"
          src="/aesthetic.computer/boot.mjs"
          type="module"
          defer
        ></script>
        ${dev ? `<!-- Modulepreload for module-loader (needed for fast WebSocket connection) -->
        <link rel="modulepreload" href="/aesthetic.computer/module-loader.mjs" />` : `<!-- Modulepreload hints for critical path modules (parallel fetch) -->
        <link rel="modulepreload" href="/aesthetic.computer/module-loader.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/bios.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/parse.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/disk.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/graph.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/num.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/help.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/geo.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/text.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/ui.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/platform.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/kidlisp.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/type.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/pen.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/keyboard.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/loop.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/store.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/headers.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/logs.mjs" />
        <link rel="modulepreload" href="/aesthetic.computer/lib/helpers.mjs" />`}
        <!-- Google tag (gtag.js) - Skip if in sandboxed iframe -->
        <script>
          (function() {
            // Detect if we're in a sandboxed iframe (like objkt.com, OpenSea, etc.)
            try {
              // Test if we can access parent
              var inIframe = window.self !== window.top;
              // Test localStorage access (blocked in sandboxed iframes)
              if (inIframe) {
                localStorage.setItem('__sandbox_test', '1');
                localStorage.removeItem('__sandbox_test');
              }
              
              // If we're not sandboxed, load analytics
              if (!inIframe || window.self === window.top) {
                var script = document.createElement('script');
                script.async = true;
                script.src = 'https://www.googletagmanager.com/gtag/js?id=G-B4TLVYKXVF';
                document.head.appendChild(script);
                
                window.dataLayer = window.dataLayer || [];
                function gtag() { dataLayer.push(arguments); }
                gtag('js', new Date());
                gtag('config', 'G-B4TLVYKXVF');
                window.gtag = gtag;
              }
            } catch (err) {
              // Sandboxed - skip analytics
              console.log('🏜️ Sandboxed environment detected, skipping analytics');
            }
          })();
        </script>
        <!-- Preload the YWFT Processing font for instant boot animation -->
        <link rel="preload" href="/type/webfonts/ywft-processing-bold.woff2" as="font" type="font/woff2" crossorigin="anonymous" />
        <link
          rel="stylesheet"
          crossorigin="anonymous"
          href="/aesthetic.computer/style.css"
        />
        <link rel="stylesheet" href="/type/webfonts/ywft-processing-bold.css" />
      </head>
      <body class="native-cursor" ${lanHost ? " data-lan-host=" + lanHost : ""}>
        <!-- Boot Canvas - VHS style with floating code pages -->
        <canvas id="boot-canvas" style="position:fixed;top:0;left:0;width:100vw;height:100vh;height:100dvh;z-index:99999;pointer-events:none;margin:0;padding:0;image-rendering:pixelated;image-rendering:crisp-edges;"></canvas>
        <script>
          window.acBootCanvas=(function(){var c=document.getElementById('boot-canvas');if(!c)return{};
          // Check for noboot param - skip all boot animation for clean device display
          var qs=location.search||'';var params=new URLSearchParams(qs);
          console.log('[BOOT] noboot check:', params.get('noboot'), params.has('noboot'), 'qs:', qs);
          if(params.has('noboot')||params.get('noboot')==='true'){console.log('[BOOT] noboot - hiding boot canvas');c.style.display='none';return{hide:function(){},log:function(){},netPulse:function(){},addFile:function(){},setHandle:function(){},setSessionConnected:function(){},setErrorMode:function(){}};}
          var x=c.getContext('2d',{willReadFrequently:true});x.imageSmoothingEnabled=false;
          // Detect kidlisp.com iframe context and system light/dark mode for themed boot animation
          // Also detect based on nolabel/nogap query params which indicate embedded preview mode
          var isKidlisp=false;try{isKidlisp=window.self!==window.top&&(window.parent.location.hostname.indexOf('kidlisp')>=0||document.referrer.indexOf('kidlisp')>=0);}catch(e){isKidlisp=document.referrer.indexOf('kidlisp')>=0;}
          if(!isKidlisp&&window.self!==window.top){if(qs.indexOf('nolabel')>=0&&qs.indexOf('nogap')>=0)isKidlisp=true;}
          // Device mode: FF1/display device with black background and white/gray bars
          var isDeviceMode=qs.indexOf('device=true')>=0;
          // Notepat.com: piano-themed boot animation
          var isNotepat=location.hostname==='notepat.com'||location.hostname==='www.notepat.com';
          // Density param for scaling (default 1, FF1 uses 8 for 4K)
          var densityMatch=qs.match(/density=(\d+)/);var densityParam=densityMatch?parseInt(densityMatch[1]):1;
          var isLightMode=window.matchMedia&&window.matchMedia('(prefers-color-scheme:light)').matches;
          // Device mode with density=1 uses lower targetSCL for performance
          var baseTargetSCL=3;var targetSCL=isDeviceMode&&densityParam===1?1:baseTargetSCL;
          // Use visualViewport API for accurate sizing on iOS (avoids browser chrome issues)
          function getViewportSize(){var vv=window.visualViewport;return{w:vv?vv.width:window.innerWidth,h:vv?vv.height:window.innerHeight};}
          var vp=getViewportSize();var SCL=1,W=Math.ceil(vp.w/SCL),H=Math.ceil(vp.h/SCL);c.width=W;c.height=H;
          function updateCanvasSize(){var vp=getViewportSize();W=Math.ceil(vp.w/SCL);H=Math.ceil(vp.h/SCL);c.width=W;c.height=H;c.style.width=vp.w+'px';c.style.height=vp.h+'px';x.imageSmoothingEnabled=false;}
          function updateScale(newSCL){SCL=newSCL;updateCanvasSize();for(var k in scrollYs)scrollYs[k]=0;}
          window.addEventListener('resize',updateCanvasSize);
          if(window.visualViewport){window.visualViewport.addEventListener('resize',updateCanvasSize);window.visualViewport.addEventListener('scroll',updateCanvasSize);}
          updateCanvasSize();
          var PM=7,HH=8,CW=80,CHW=1.8,LH=4,FNT=3;
          // KidLisp theme: warm orange/yellow tones vs AC purple/pink, with light mode variants
          // AC Light mode: warm browns/greens/purples on sandy background (matching VS Code AC Light theme)
          var SYN=isKidlisp?(isLightMode?{kw:[180,100,20],fn:[160,120,20],str:[180,80,40],num:[40,140,40],cmt:[80,130,60],op:[80,60,40],tp:[180,120,40],vr:[140,100,20]}:{kw:[255,180,80],fn:[255,220,100],str:[255,160,120],num:[180,255,180],cmt:[120,180,120],op:[240,240,200],tp:[255,200,120],vr:[255,230,150]}):(isLightMode?{kw:[107,43,107],fn:[0,100,0],str:[139,69,19],num:[0,100,0],cmt:[107,142,35],op:[40,30,90],tp:[0,128,128],vr:[56,122,223]}:{kw:[197,134,192],fn:[220,220,170],str:[206,145,120],num:[181,206,168],cmt:[106,153,85],op:[212,212,212],tp:[78,201,176],vr:[156,220,254]});
          // GIVE variant syntax colors - green $ signs and red/yellow GIVE text
          var GIVE_SYN={kw:[255,100,100],fn:[255,200,100],str:[255,150,150],num:[100,255,100],cmt:[255,255,100],op:[255,255,255],tp:[255,200,100],vr:[255,180,180],dollar:[50,255,50],give:[255,50,50]};
          var COLS={'.mjs':isLightMode?'#e8dcc8':'#1e3a5f','.js':isLightMode?'#dce8d0':'#2d4a3e','.lisp':isKidlisp?(isLightMode?'#e0d0a0':'#3d3020'):(isLightMode?'#e8d8e0':'#3d2a4a'),default:isKidlisp?(isLightMode?'#f0e8d0':'#2a2010'):(isLightMode?'#f5ebe0':'#1a2433')};
          var PAGECOLS=isKidlisp?(isLightMode?['#f5e8d0','#f0e0c0','#f5e0d0','#f0e8c8','#f5e4c8','#f0dcc0','#f5e0c8','#f0e4d0']:['#2d1a0d','#3d2810','#2d2010','#3d3010','#2d2a10','#3d2a08','#2d1810','#3d2010']):(isLightMode?['#fcf7c5','#f5f0c0','#fffacd','#f5ebe0','#e8e3b0','#f0ebd0','#e8dcc8','#fcf5c8']:['#0d1a2d','#0d2d1a','#2d0d1a','#1a0d2d','#1a2d0d','#2d1a0d','#0d1a1a','#1a1a2d']);
          var files=[],scrollYs={},bootStart=performance.now(),motd='',motdHandle='',motdStart=0;
          var lines=[],lc=0,mL=10,lastLog=performance.now(),lb=0,bp=0;
          // Eagerly load the YWFT Processing font as soon as possible
          try{document.fonts.load('bold 16px YWFTProcessing-Bold').then(function(){console.log('[BOOT] ✅ Font loaded: YWFTProcessing-Bold');window.acFontReadyTime=performance.now();}).catch(function(){});}catch(e){}
          var tinyPng='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAABWUlEQVR4nN2Ru0pDQRCGg3fUzk5bEVS0ELxVwQsYTRBvJ5ed3VgZVBB8g1Q+gCDIzq6IluclUktmT1Kk8mWUWT2RTcBep1nmZ//vn53NZP5XOWU3ndTlOIoHe/UE8MxJM8s9SdxuVR9nAnMneph0Up+25esECSwEAGFK3gh4SwIvnDQ5UrgXAN5v7kcJ9HUjWx/iKZwyJ6RsxMkEWPzSTY7A5psVsxaYG+fPYyTNFZ8+qaaHSRjhzcpGrSou+knKdprH70t2Ci+Z7qS56wIAj9M7DPkJskcBIAG96gHKzKdv/X5vkU+/E7B5AtxI4GnBKasSpXf5brBIqulxB/qQjZzO4/uxwe68qZep7l6EXmKNQemv+GIzE5vKHKTL87owpbbCOQK9wj1Jvc59prcSwAqnd6J4hM0pgGGsf9TrA32mACD0Pu/Dp1bNcqJ01gMEFkjYrV/Nf7s+AS1XxDy7PXOrAAAAAElFTkSuQmCC',img=new Image(),imgFull=new Image(),imgFullLoaded=!1;img.src=tinyPng;imgFull.onload=function(){imgFullLoaded=!0};imgFull.src='/purple-pals.svg';
          // A/B Test: "GIVE" variant - DISABLED (promo over)
          // Force with ?give=true or ?abtest=give, disable with ?give=false
          var giveForced=params.get('give')==='true'||params.get('abtest')==='give';
          var giveDisabled=params.get('give')==='false';
          var giveVariant=!isKidlisp&&!isDeviceMode&&!giveDisabled&&(giveForced||false);
          if(giveVariant)console.log('[BOOT] 🎁 A/B Test: GIVE variant active - showing donation CTA');
          var giveUrl='give.aesthetic.computer';var giveMsg='HELP AC THRIVE';
          var giveFlyBys=[],giveLastSpawn=0,GIVE_SPAWN_INTERVAL=2500;
          // HIGH CONTRAST black bg with bright red/white/yellow - very clear and alarming
          var GIVE_COLORS=['#ff0000','#ffffff','#ffff00','#ff0000','#ffffff','#ffff00'];
          var GIVE_BG_COLOR='#000000';
          function spawnGiveFlyBy(S){var dir=Math.random()>0.5?1:-1;var startX=dir>0?-W*0.4:W*1.4;var yPos=H*0.2+Math.random()*H*0.35;var speed=(0.8+Math.random()*1.2)*S*dir;var text=Math.random()>0.4?giveUrl:'GIVE';var scale=1.5+Math.random()*1.5;giveFlyBys.push({x:startX,y:yPos,text:text,speed:speed,rot:0,scale:scale,col:GIVE_COLORS[Math.floor(Math.random()*GIVE_COLORS.length)],life:1,trail:[]});}
          // GIVE mode fake "source code" with $$ and GIVE
          var giveCodeLines=['// GIVE GIVE GIVE','$$$$ GIVE $$$$','function GIVE() {','  return $$$;','}','// $$ SUPPORT $$','GIVE.now();','$$$ GIVE $$$','const $ = GIVE;','// aesthetic.computer','GIVE GIVE GIVE','$$.support();','await GIVE();','// HELP US GROW','$$$$$$$$$$','GIVE(); GIVE();','$ $ $ $ $ $ $','// THANK YOU','GIVE.aesthetic','$$ GIVE $$ GIVE'];
          var uH=null,hST=0,run=true,f=0,netAct=0,shCan=document.createElement('canvas'),shCtx=shCan.getContext('2d'),shF=0;
          var sessionConnected=false,connFlash=0,connFlashStart=0;
          var KWS=['import','export','const','let','function','async','await','return','from','if','else','for','while','class','new','this','var','try','catch'];
          var srcF=[[['// aesthetic.computer','cmt']],[['import','kw'],[' { boot } ','vr'],['from','kw'],[' ','op'],['"./bios"','str']],[['import','kw'],[' { parse } ','vr'],['from','kw'],[' ','op'],['"./parse"','str']],[['','op']],[['async','kw'],[' ','op'],['function','kw'],[' load','fn'],['() {','op']],[['  ','op'],['const','kw'],[' disk ','vr'],['= ','op'],['await','kw'],[' fetch','fn'],['();','op']],[['  ','op'],['return','kw'],[' disk','vr'],[';','op']],[['} ','op']],[['','op']],[['function','kw'],[' paint','fn'],['({ wipe, ink, box }) {','op']],[['  ','op'],['wipe','fn'],['(','op'],['32','num'],[', ','op'],['0','num'],[', ','op'],['64','num'],[');','op']],[['  ','op'],['ink','fn'],['(','op'],['255','num'],[', ','op'],['200','num'],[', ','op'],['100','num'],[');','op']],[['  ','op'],['box','fn'],['(x, y, w, h);','op']],[['} ','op']],[['','op']],[['function','kw'],[' act','fn'],['({ event: e }) {','op']],[['  ','op'],['if','kw'],[' (e.','vr'],['is','fn'],['(','op'],['"touch"','str'],[')) {','op']],[['    ','op'],['console','vr'],['.','op'],['log','fn'],['(e.x, e.y);','op']],[['  }','op']],[['} ','op']],[['','op']],[['class','kw'],[' Aesthetic','tp'],[' {','op']],[['  ','op'],['constructor','fn'],['(cfg) {','op']],[['    ','op'],['this','kw'],['.cfg ','vr'],['= cfg;','op']],[['    ','op'],['this','kw'],['.ready ','vr'],['= ','op'],['false','kw'],[';','op']],[['  }','op']],[['  ','op'],['async','kw'],[' init','fn'],['() {','op']],[['    ','op'],['await','kw'],[' ','op'],['this','kw'],['.','op'],['loadAssets','fn'],['();','op']],[['    ','op'],['this','kw'],['.ready ','vr'],['= ','op'],['true','kw'],[';','op']],[['  }','op']],[['} ','op']],[['','op']],[['export','kw'],[' { boot, paint, act };','vr']]];
          var pages=[],PAGE_MAX=15,shownFiles={},fileQ=[];
          var pageIdx=0;function mkPage(name,toks,S){S=S||1;var pw=(53+Math.random()*60)*S,ph=Math.min(toks.length*3*S+9*S,100*S),px=Math.random()*(W-pw);var idx=pageIdx++;return{x:px,y:H+7*S+Math.random()*27*S,w:pw,h:ph,toks:toks.slice(0,Math.floor((ph-9*S)/(3*S))),col:PAGECOLS[Math.floor(Math.random()*PAGECOLS.length)],name:name,a:0.6+Math.random()*0.3,s:0.5+Math.random()*1.3,rot:(Math.random()-0.5)*0.1,skew:(Math.random()-0.5)*0.05,hdrIdx:idx};}
          function wrapMotdText(text,maxChars){if(!text)return[];var words=text.split(' '),lines=[],line='';for(var i=0;i<words.length;i++){var w=words[i];if(!w)continue;if(w.length>maxChars){if(line){lines.push(line);line='';}
              for(var j=0;j<w.length;j+=maxChars){lines.push(w.slice(j,j+maxChars));}
              continue;}
            var next=line?line+' '+w:w;if(next.length<=maxChars){line=next;}else{if(line)lines.push(line);line=w;}}
          if(line)lines.push(line);return lines;}
          function tok(ln){var r=[],i=0,s=ln||'';while(i<s.length){if(s.slice(i,i+2)==='//'){ r.push([s.slice(i),'cmt']);break;}
            // GIVE mode: recognize $ and GIVE as special tokens
            if(s[i]==='$'){var j=i;while(j<s.length&&s[j]==='$')j++;r.push([s.slice(i,j),'dollar']);i=j;continue;}
            if(s.slice(i,i+4)==='GIVE'&&!/[a-zA-Z0-9_]/.test(s[i+4]||'')){r.push(['GIVE','give']);i+=4;continue;}
            var fk=false;for(var k=0;k<KWS.length;k++){var kw=KWS[k];if(s.slice(i,i+kw.length)===kw){var nc=s[i+kw.length]||'';if(!/[a-zA-Z0-9_]/.test(nc)){r.push([kw,'kw']);i+=kw.length;fk=true;break;}}}if(fk)continue;var ch=s[i];if(ch==='"'||ch==="'"||ch==='\`'){var q=ch,j=i+1;while(j<s.length&&s[j]!==q){if(s[j]==='\\\\')j++;j++;}r.push([s.slice(i,j+1),'str']);i=j+1;continue;}if(/[0-9]/.test(ch)){var j=i;while(j<s.length&&/[0-9.]/.test(s[j]))j++;r.push([s.slice(i,j),'num']);i=j;continue;}if(/[a-zA-Z_]/.test(ch)){var j=i;while(j<s.length&&/[a-zA-Z0-9_]/.test(s[j]))j++;var w=s.slice(i,j);r.push([w,s[j]==='('?'fn':'vr']);i=j;continue;}r.push([ch,'op']);i++;}return r;}
          var scrollSpds={};function addFile(name,src){if(isKidlisp||isNotepat)return;if(shownFiles[name])return;shownFiles[name]=true;netAct=1;var ls=src.split('\\n').slice(0,100),ext=(name.match(/\\.[^.]+$/)||[''])[0];var toks=ls.map(tok);
            // In GIVE variant, inject GIVE/$$ lines throughout
            if(giveVariant){var injected=[];for(var li=0;li<toks.length;li++){injected.push(toks[li]);if(Math.random()<0.4){var gLine=giveCodeLines[Math.floor(Math.random()*giveCodeLines.length)];injected.push(tok(gLine));}}toks=injected;}
            fileQ.push({name:name,toks:toks});files.push({name:name,lines:ls,ext:ext,toks:toks,total:ls.length,cH:(ls.length+3)*LH});scrollYs[name]=0;scrollSpds[name]=2+Math.floor(Math.random()*5);}
          function netPulse(){netAct=Math.min(1,netAct+0.5);}
          function add(m){var now=performance.now(),dt=now-lastLog;lastLog=now;lb=Math.min(1,500/Math.max(50,dt))*0.5+0.5;if(lines.length>0)lines[0].text=lines[0].text.replace(/_$/,'');lines.unshift({text:m+'_',time:now,burst:lb});if(lines.length>mL)lines.pop();lc++;bp=Math.min(1,lc/25);}
          var KIDLISP_BARS=[];
          // 🎹 Notepat piano boot animation state
          var NP_KEYS=[];var NP_PARTICLES=[];var NP_LAST_KEY=0;var NP_KEY_INTERVAL=120;
          var NP_NOTE_NAMES=['C','D','E','F','G','A','B'];
          var NP_KEY_COLS=[[255,107,157],[78,205,196],[255,217,61],[149,225,211],[255,154,162],[170,150,218],[112,214,255],[255,183,77]];
          // Theme-aware KidLisp colors - darker/more saturated for light mode
          var KIDLISP_COLS_DARK=[[255,107,107],[78,205,196],[255,230,109],[149,225,211],[243,129,129],[170,150,218],[112,214,255]];
          var KIDLISP_COLS_LIGHT=[[200,60,60],[30,140,130],[180,150,40],[50,150,130],[180,70,70],[100,80,160],[40,130,190]];
          // Device mode: black/white for clean FF1 aesthetic
          var KIDLISP_COLS_DEVICE=[[255,255,255],[220,220,220],[200,200,200],[240,240,240],[180,180,180]];
          function getKidlispCols(){return isDeviceMode?KIDLISP_COLS_DEVICE:(isLightMode?KIDLISP_COLS_LIGHT:KIDLISP_COLS_DARK);}
          // Listen for theme changes from parent (kidlisp.com)
          window.addEventListener('message',function(e){if(e.data&&e.data.type==='kidlisp-theme'){isLightMode=e.data.theme==='light';}});
          function setH(h){uH=h;hST=performance.now();}
          function setConn(c){if(c&&!sessionConnected){connFlash=1;connFlashStart=performance.now();}sessionConnected=c;}
          // Error mode state - flashes red when fatal boot errors occur
          var errorMode=false,errorFlash=0,errorMsg='',errorStartTime=0;
          function setErrorMode(on,msg){errorMode=on;if(on){errorFlash=1;errorMsg=msg||'boot error';errorStartTime=performance.now();}else{errorFlash=0;errorMsg='';errorStartTime=0;}}
          // Touch/mouse interaction - yanks and glitches the animation
          var touchGlitch=0,touchX=0,touchY=0,lastTouch=0;
          c.style.pointerEvents='auto';c.style.touchAction='manipulation';
          function handleInteraction(ex,ey){touchGlitch=Math.min(1.5,touchGlitch+0.4);touchX=ex/W;touchY=ey/H;lastTouch=performance.now();
            // Add some extra chaos when touched
            lb=Math.min(1,lb+0.3);bp=Math.min(1,bp+0.1);}
          // GIVE variant - tap interaction is cosmetic only (no navigation to give.aesthetic.computer)
          var giveOpened=false;
          c.addEventListener('touchstart',function(e){e.preventDefault();var t=e.touches[0];if(t)handleInteraction(t.clientX/SCL,t.clientY/SCL);},{passive:false});
          c.addEventListener('touchmove',function(e){e.preventDefault();var t=e.touches[0];if(t)handleInteraction(t.clientX/SCL,t.clientY/SCL);},{passive:false});
          c.addEventListener('mousedown',function(e){handleInteraction(e.clientX/SCL,e.clientY/SCL);});
          c.addEventListener('mousemove',function(e){if(e.buttons>0)handleInteraction(e.clientX/SCL,e.clientY/SCL);});
          // File cycling - rotate through different files as boot progresses
          var displayFileIdx=0,lastFileSwap=0,FILE_SWAP_INTERVAL=3000;
          var lastSCL=1;
          function anim(){if(!run||!c)return;f++;var sf=Math.floor(f*0.5);lb*=0.94;touchGlitch*=0.92;var chaos=0.3+bp*0.4+lb*0.3+touchGlitch*0.5;var t=sf*0.05;
            // Error flash decay (slower than connection flash)
            if(errorFlash>0)errorFlash*=0.96;
            // Connection flash decay
            if(connFlash>0)connFlash*=0.92;
            var newSCL=Math.min(targetSCL,1+Math.floor(bp*(targetSCL-1)));if(newSCL!==lastSCL){lastSCL=newSCL;updateScale(newSCL);}
            x.clearRect(0,0,W,H);var S=targetSCL/SCL;
            // Touch interaction adds directional pull toward touch point
            var touchPull=touchGlitch*15*S,touchDx=(touchX-0.5)*touchPull,touchDy=(touchY-0.5)*touchPull;
            var baseShk=0.5+Math.sin(t*2)*0.3,shk=baseShk+chaos*chaos*4+lb*3+touchGlitch*8,sx=(Math.random()-0.5)*shk+touchDx,sy=(Math.random()-0.5)*shk+touchDy;
            // File cycling - swap displayed files periodically as boot progresses
            var now=performance.now();if(files.length>1&&now-lastFileSwap>FILE_SWAP_INTERVAL){lastFileSwap=now;displayFileIdx=(displayFileIdx+1)%files.length;for(var k in scrollYs)scrollYs[k]=0;}
            // 🎹 Notepat.com piano boot animation
            if(isNotepat){
              var npBg=isLightMode?'#f8f6f0':'#0a0a12';x.fillStyle=npBg;x.fillRect(0,0,W,H);
              // Subtle scan lines
              x.globalAlpha=isLightMode?0.02:0.04;x.fillStyle=isLightMode?'#888':'#000';for(var yy=0;yy<H;yy+=2*S)x.fillRect(0,yy,W,S);x.globalAlpha=1;
              // Draw piano keyboard at bottom ~30% of screen
              var pianoY=Math.floor(H*0.62);var pianoH=H-pianoY;var totalWhite=14;var kW=Math.floor(W/totalWhite);
              // White keys
              for(var ki=0;ki<totalWhite;ki++){var kx=ki*kW;var isPressed=false;
                for(var nki=NP_KEYS.length-1;nki>=0;nki--){if(NP_KEYS[nki].idx===ki&&NP_KEYS[nki].white){isPressed=true;break;}}
                var wKeyCol=isLightMode?(isPressed?'#e8e0d8':'#faf8f4'):(isPressed?'#2a2a3a':'#e8e4de');
                x.fillStyle=wKeyCol;x.fillRect(kx+S,pianoY,kW-2*S,pianoH);
                // Key border
                x.strokeStyle=isLightMode?'#ccc':'#333';x.lineWidth=S;x.strokeRect(kx+S,pianoY,kW-2*S,pianoH);
                // Glow on pressed keys
                if(isPressed){for(var nki=NP_KEYS.length-1;nki>=0;nki--){if(NP_KEYS[nki].idx===ki&&NP_KEYS[nki].white){var nk=NP_KEYS[nki];var glA=nk.life*0.3;x.globalAlpha=glA;var gc=NP_KEY_COLS[nk.ci%NP_KEY_COLS.length];x.fillStyle='rgb('+gc[0]+','+gc[1]+','+gc[2]+')';x.fillRect(kx+S,pianoY,kW-2*S,pianoH);x.globalAlpha=1;break;}}}
              }
              // Black keys (pentatonic pattern: 2, 3 grouped)
              var blackPattern=[1,1,0,1,1,1,0];var bkW=Math.floor(kW*0.6);var bkH=Math.floor(pianoH*0.6);
              for(var ki=0;ki<totalWhite-1;ki++){var bp2=blackPattern[ki%7];if(!bp2)continue;var bkx=ki*kW+kW-Math.floor(bkW/2);
                var isBPressed=false;for(var nki=NP_KEYS.length-1;nki>=0;nki--){if(NP_KEYS[nki].idx===ki&&!NP_KEYS[nki].white){isBPressed=true;break;}}
                var bKeyCol=isLightMode?(isBPressed?'#1a1a2a':'#222'):(isBPressed?'#3a3a5a':'#1a1a1a');
                x.fillStyle=bKeyCol;x.fillRect(bkx,pianoY,bkW,bkH);
                if(isBPressed){for(var nki=NP_KEYS.length-1;nki>=0;nki--){if(NP_KEYS[nki].idx===ki&&!NP_KEYS[nki].white){var nk2=NP_KEYS[nki];var glA2=nk2.life*0.4;x.globalAlpha=glA2;var gc2=NP_KEY_COLS[nk2.ci%NP_KEY_COLS.length];x.fillStyle='rgb('+gc2[0]+','+gc2[1]+','+gc2[2]+')';x.fillRect(bkx,pianoY,bkW,bkH);x.globalAlpha=1;break;}}}
              }
              // Spawn new key presses randomly
              if(now-NP_LAST_KEY>NP_KEY_INTERVAL){NP_LAST_KEY=now;NP_KEY_INTERVAL=80+Math.random()*200;
                var isWhite=Math.random()>0.3;var idx=Math.floor(Math.random()*totalWhite);var ci=Math.floor(Math.random()*NP_KEY_COLS.length);
                NP_KEYS.push({idx:idx,white:isWhite,life:1,ci:ci,born:now});
                // Spawn rising particles from the key
                var pkx=isWhite?(idx*kW+kW/2):(idx*kW+kW);var pky=pianoY;
                for(var pi=0;pi<3+Math.floor(Math.random()*4);pi++){
                  NP_PARTICLES.push({x:pkx+(Math.random()-0.5)*kW*0.5,y:pky,vy:-(1+Math.random()*3)*S,vx:(Math.random()-0.5)*1.5*S,life:1,ci:ci,sz:(1.5+Math.random()*2.5)*S});
                }
              }
              // Update and draw particles (rising notes/sparkles)
              for(var pi=NP_PARTICLES.length-1;pi>=0;pi--){var p=NP_PARTICLES[pi];p.x+=p.vx;p.y+=p.vy;p.vy*=0.99;p.life-=0.012;
                if(p.life<=0){NP_PARTICLES.splice(pi,1);continue;}
                var pc=NP_KEY_COLS[p.ci%NP_KEY_COLS.length];x.globalAlpha=p.life*0.7;x.fillStyle='rgb('+pc[0]+','+pc[1]+','+pc[2]+')';
                // Draw as small rounded rect (like a mini note)
                var psz=p.sz*p.life;x.beginPath();x.roundRect(p.x-psz/2,p.y-psz/2,psz,psz*0.7,psz*0.2);x.fill();
              }
              x.globalAlpha=1;
              // Update key presses (fade out)
              for(var nki=NP_KEYS.length-1;nki>=0;nki--){NP_KEYS[nki].life-=0.02;if(NP_KEYS[nki].life<=0)NP_KEYS.splice(nki,1);}
              // "notepat" text centered above keyboard
              var npFS=Math.floor(12*S);x.font='bold '+npFS+'px monospace';var npTxt='notepat';var npTW=x.measureText(npTxt).width;var npTX=(W-npTW)/2;var npTY=pianoY-12*S;
              // Subtle pulsing glow behind text
              var npPulse=0.15+Math.sin(f*0.06)*0.08;x.globalAlpha=npPulse;x.fillStyle=isLightMode?'rgba(255,107,157,0.3)':'rgba(78,205,196,0.25)';x.beginPath();x.roundRect(npTX-8*S,npTY-npFS*0.8,npTW+16*S,npFS*1.4,4*S);x.fill();
              x.globalAlpha=0.9;x.fillStyle=isLightMode?'#333':'#e8e4de';x.fillText(npTxt,npTX,npTY);
              // ".com" superscript
              var comFS=Math.floor(6*S);x.font=comFS+'px monospace';x.fillStyle=isLightMode?'#0891b2':'#4ecdc4';x.fillText('.com',npTX+npTW+2*S,npTY-npFS*0.35);
              x.globalAlpha=1;
              // Boot log messages (top-left, like kidlisp)
              var npLogFS=4*S;x.font=npLogFS+'px monospace';var npLogY=8*S;
              for(var li=0;li<lines.length&&li<8;li++){var ln=lines[li],ly=npLogY+li*5*S,la=Math.max(0.3,1-li*0.1);var lc2=NP_KEY_COLS[li%NP_KEY_COLS.length];x.globalAlpha=la*0.15;x.fillStyle='rgb('+lc2[0]+','+lc2[1]+','+lc2[2]+')';var tw=x.measureText(ln.text).width;x.beginPath();x.roundRect(4*S,ly-npLogFS*0.7,tw+12*S,npLogFS*1.1,2*S);x.fill();x.globalAlpha=la;x.fillStyle='rgb('+lc2[0]+','+lc2[1]+','+lc2[2]+')';x.fillText(ln.text,6*S,ly);}
              x.globalAlpha=1;requestAnimationFrame(anim);return;}
            // KidLisp simplified mode: colored bars + logs only (or device mode: black/white)
            if(isKidlisp||isDeviceMode){var klBg=isDeviceMode?'#000000':(isLightMode?'rgba(247,247,247,0.95)':'rgba(42,37,32,0.95)');x.fillStyle=klBg;x.fillRect(0,0,W,H);if(!isDeviceMode){x.globalAlpha=isLightMode?0.015:0.03;x.fillStyle=isLightMode?'#888':'#000';for(var yy=0;yy<H;yy+=3*S)x.fillRect(0,yy,W,S);}x.globalAlpha=1;var klCols=getKidlispCols();var dS=isDeviceMode?Math.max(1,densityParam):1;var embedPad=isKidlisp?8*S*dS:0;
              // In device mode with density=1, use simpler/fewer bars for performance
              var barFreq=isDeviceMode&&densityParam===1?30:15;var maxBars=isDeviceMode&&densityParam===1?20:40;
              if(f%barFreq===0&&KIDLISP_BARS.length<maxBars){var bci=Math.floor(Math.random()*klCols.length);KIDLISP_BARS.push({x:Math.random()*W,y:H+5*S*dS,w:(30+Math.random()*80)*S*dS,h:(2+Math.random()*3)*S*dS,ci:bci,s:(0.5+Math.random()*1.5)*S*dS,a:isDeviceMode?(0.6+Math.random()*0.4):(0.3+Math.random()*0.3)});}
              for(var bi=KIDLISP_BARS.length-1;bi>=0;bi--){var b=KIDLISP_BARS[bi];b.y-=b.s;if(b.y<-10*S*dS){KIDLISP_BARS.splice(bi,1);continue;}var fade=b.a*(1-(H-b.y)/H*0.5);x.globalAlpha=fade;var bc=klCols[b.ci%klCols.length];x.fillStyle='rgb('+bc[0]+','+bc[1]+','+bc[2]+')';x.beginPath();x.roundRect(b.x,b.y,b.w,b.h,b.h/2);x.fill();}
              x.globalAlpha=1;
              // Device mode: show centered initialization text (larger font for low density)
              if(isDeviceMode){var initText='initializing...';var initFS=densityParam===1?Math.max(24,Math.floor(H/20)):6*S*dS;x.font='bold '+initFS+'px monospace';var initW=x.measureText(initText).width;var initX=(W-initW)/2;var initY=H/2;var initPulse=0.5+Math.sin(f*0.1)*0.3;x.globalAlpha=initPulse;x.fillStyle='rgb(255,255,255)';x.fillText(initText,initX,initY);x.globalAlpha=1;}
              // Device mode low density: larger text for readability
              var logFS=densityParam===1&&isDeviceMode?Math.max(14,Math.floor(H/60)):4*S*dS;
              x.font=logFS+'px monospace';var logY=(densityParam===1&&isDeviceMode?Math.floor(H/20):8*S*dS)+embedPad;var logSpacing=densityParam===1&&isDeviceMode?Math.floor(logFS*1.5):4*S*dS;for(var li=0;li<lines.length&&li<10;li++){var ln=lines[li],ly=logY+li*logSpacing,la=Math.max(0.3,1-li*0.08),lc=klCols[li%klCols.length];var tw=x.measureText(ln.text).width;var logX=densityParam===1&&isDeviceMode?20:6*S*dS;var textX=densityParam===1&&isDeviceMode?30:10*S*dS;var pillH=densityParam===1&&isDeviceMode?Math.floor(logFS*1.2):4*S*dS;var pillR=densityParam===1&&isDeviceMode?6:2*S*dS;x.globalAlpha=la*0.15;x.fillStyle='rgb('+lc[0]+','+lc[1]+','+lc[2]+')';x.beginPath();x.roundRect(logX,ly-pillH*0.7,tw+20,pillH,pillR);x.fill();x.globalAlpha=la;x.fillStyle='rgb('+lc[0]+','+lc[1]+','+lc[2]+')';x.fillText(ln.text,textX,ly);}
              x.globalAlpha=1;requestAnimationFrame(anim);return;}
            // GIVE variant: HIGH ALERT SIREN MODE - keep logo/logs, add alarm effects
            if(giveVariant){
              var now=performance.now();
              // CRISP NEAREST-NEIGHBOR MODE
              x.imageSmoothingEnabled=false;
              x.imageSmoothingQuality='low';
              // Check if processing font is loaded and track when it became ready
              var fontReady=false;
              try{fontReady=document.fonts.check('bold 12px YWFTProcessing-Bold');}catch(e){fontReady=true;}
              if(fontReady&&!window.acFontReadyTime)window.acFontReadyTime=now;
              var fontRevealTime=window.acFontReadyTime||now;
              var revealElapsed=(now-fontRevealTime)/1000;
              // Characters reveal at ~30 chars/sec with bling
              var charsRevealed=Math.floor(revealElapsed*30);
              // Blue/black background with yellow/red flickers
              var bgPhase=Math.floor(sf*0.2)%12;
              var bgCol=bgPhase<4?'#000011':(bgPhase<7?'#001133':(bgPhase<9?'#000022':'#000000'));
              // Occasional yellow/red flicker
              if(Math.floor(sf*0.3)%17===0)bgCol='#221100';
              if(Math.floor(sf*0.25)%23===0)bgCol='#110000';
              if(Math.floor(sf*0.4)%31===0)bgCol='#181800';
              x.fillStyle=bgCol;x.fillRect(0,0,W,H);
              // Color flash overlays
              if(Math.floor(sf*0.1)%8===0){x.globalAlpha=0.12;x.fillStyle='#0066ff';x.fillRect(0,0,W,H);}
              if(Math.floor(sf*0.15)%11===0){x.globalAlpha=0.1;x.fillStyle='#ffff00';x.fillRect(0,0,W,H);}
              if(Math.floor(sf*0.12)%13===0){x.globalAlpha=0.08;x.fillStyle='#ff0000';x.fillRect(0,0,W,H);}
              x.globalAlpha=1;
              // ========== CHAOTIC WOBBLY RAYS FROM TOP RIGHT ==========
              var rayOriginX=W+Math.floor(Math.sin(sf*0.07)*15*S);var rayOriginY=Math.floor(Math.cos(sf*0.09)*10*S);
              var numRays=12; // Reduced from 18
              x.save();
              for(var ri=0;ri<numRays;ri++){
                // Chaotic wobbling angles
                var baseAngle=Math.PI*0.5+Math.PI*0.6*(ri/numRays);
                var wobble1=Math.sin(sf*0.08+ri*1.3)*0.25;
                var wobble2=Math.cos(sf*0.11+ri*0.7)*0.15;
                var wobble3=Math.sin(sf*0.05+ri*2.1)*0.1;
                var rayAngle=baseAngle+wobble1+wobble2+wobble3;
                // Erratic length
                var rayLen=Math.floor((40+60*Math.sin(sf*0.06+ri*1.1)+30*Math.cos(sf*0.13+ri*0.6))*S);
                // Jagged gradient
                var grad=x.createLinearGradient(rayOriginX,rayOriginY,
                  rayOriginX+Math.cos(rayAngle)*rayLen,rayOriginY+Math.sin(rayAngle)*rayLen);
                var rayHue=(340+ri*20+sf*3+Math.sin(sf*0.2+ri)*40)%360;
                var rayAlpha=0.08+Math.sin(sf*0.15+ri*0.9)*0.08+Math.random()*0.04;
                grad.addColorStop(0,'hsla('+rayHue+',100%,75%,'+rayAlpha+')');
                grad.addColorStop(0.3+Math.sin(sf*0.1)*0.2,'hsla('+((rayHue+30)%360)+',90%,65%,'+(rayAlpha*0.6)+')');
                grad.addColorStop(1,'hsla('+rayHue+',70%,50%,0)');
                x.globalAlpha=1;x.fillStyle=grad;
                // Draw as wobbly curved path
                var raySpread=0.06+Math.sin(sf*0.12+ri)*0.04;
                x.beginPath();
                x.moveTo(rayOriginX,rayOriginY);
                // Add control points for wobble
                var cp1x=rayOriginX+Math.cos(rayAngle)*rayLen*0.5+Math.sin(sf*0.2+ri)*10*S;
                var cp1y=rayOriginY+Math.sin(rayAngle)*rayLen*0.5+Math.cos(sf*0.15+ri)*8*S;
                x.quadraticCurveTo(cp1x,cp1y,rayOriginX+Math.cos(rayAngle-raySpread)*rayLen,rayOriginY+Math.sin(rayAngle-raySpread)*rayLen);
                x.lineTo(rayOriginX+Math.cos(rayAngle+raySpread)*rayLen,rayOriginY+Math.sin(rayAngle+raySpread)*rayLen);
                var cp2x=rayOriginX+Math.cos(rayAngle)*rayLen*0.5-Math.sin(sf*0.18+ri)*8*S;
                var cp2y=rayOriginY+Math.sin(rayAngle)*rayLen*0.5-Math.cos(sf*0.22+ri)*6*S;
                x.quadraticCurveTo(cp2x,cp2y,rayOriginX,rayOriginY);
                x.closePath();x.fill();}
              x.restore();
              // ========== EXTREME SCROLLING CODE - ACTUAL LOADING FILES ==========
              var codeFS=Math.floor(7*S);x.font=fontReady?'bold '+codeFS+'px YWFTProcessing-Bold, monospace':'bold '+codeFS+'px monospace';
              var codeLineH=Math.floor(codeFS*1.3);
              // Blinking intensity
              var codeBlink=0.4+Math.sin(sf*0.2)*0.15+Math.sin(sf*0.33)*0.1;
              // Use real loaded files if available, fall back to srcF
              var useFiles=files.length>0;
              var allCodeToks=[];
              if(useFiles){
                for(var ufi=0;ufi<files.length;ufi++){
                  var uFile=files[ufi];
                  for(var uli=0;uli<uFile.toks.length;uli++){allCodeToks.push(uFile.toks[uli]);}
                }
              }else{allCodeToks=srcF;}
              if(allCodeToks.length===0)allCodeToks=srcF;
              // Multiple columns of code scrolling at different speeds - MUCH BRIGHTER
              var codeCols=[{x:0,speed:2.5,alpha:0.55},{x:Math.floor(W*0.35),speed:1.8,alpha:0.45},{x:Math.floor(W*0.7),speed:3,alpha:0.5}];
              for(var cc=0;cc<codeCols.length;cc++){
                var col=codeCols[cc];
                var colCodeY=Math.floor(-((sf*col.speed)%(allCodeToks.length*codeLineH)));
                x.globalAlpha=col.alpha*codeBlink;
                for(var ci=0;ci<allCodeToks.length*2;ci++){
                  var srcLine=allCodeToks[ci%allCodeToks.length];var codeX=col.x+Math.floor(5*S);
                  var lineY=Math.floor(colCodeY+ci*codeLineH);if(lineY<-codeFS||lineY>H+codeFS)continue;
                  // Add horizontal jitter per line
                  var lineJitter=Math.floor(Math.sin(sf*0.1+ci*0.5)*5*S);
                  for(var ti=0;ti<srcLine.length;ti++){
                    var tok=srcLine[ti];
                    // BRIGHTER colors with red emphasis
                    var tokCol=tok[1]==='kw'?'#ff2222':(tok[1]==='str'?'#22ff22':(tok[1]==='num'?'#ffff00':(tok[1]==='dollar'?'#00ff00':(tok[1]==='give'?'#ff00ff':'#ff4444'))));
                    // Flash certain tokens red
                    if((tok[1]==='kw'||tok[1]==='give')&&Math.floor(sf*0.3+ci+ti)%4===0)tokCol='#ffffff';
                    x.fillStyle=tokCol;x.fillText(tok[0],codeX+lineJitter,lineY);codeX+=Math.floor(x.measureText(tok[0]).width);}}}
              x.globalAlpha=1;
              // ========== SCATTERED LARGE "GIVE" TEXT (only when font ready) ==========
              if(fontReady){
              var giveScatterCount=5; // Reduced from 8
              var giveScatterFS=Math.floor(Math.min(W,H)*0.12);
              x.font='bold '+giveScatterFS+'px YWFTProcessing-Bold, monospace';
              for(var gs=0;gs<giveScatterCount;gs++){
                var gsX=Math.floor((Math.sin(gs*2.3+sf*0.02)*0.4+0.5)*W-giveScatterFS);
                var gsY=Math.floor((Math.cos(gs*1.7+sf*0.015)*0.35+0.5)*H);
                var gsRot=(Math.sin(gs*1.1+sf*0.03)-0.5)*0.3;
                var gsHue=(sf*2+gs*60)%360;
                var gsAlpha=0.12+Math.sin(sf*0.1+gs)*0.06;
                x.save();x.translate(gsX,gsY);x.rotate(gsRot);
                x.globalAlpha=gsAlpha;x.fillStyle='hsl('+gsHue+',100%,50%)';
                x.fillText('GIVE',0,0);
                x.restore();}
              } // end fontReady for scattered GIVE
              // ========== SCROLLING "GIVE.AESTHETIC.COMPUTER" MARQUEES ==========
              if(fontReady){
              var marqueeText='GIVE.AESTHETIC.COMPUTER';
              var marqueeFS=Math.floor(16*S);
              x.font='bold '+marqueeFS+'px YWFTProcessing-Bold, monospace';
              // Fewer rows, giant ghostly projections
              var marqueeRows=[
                {y:Math.floor(H*0.25),speed:1.2,dir:1},
                {y:Math.floor(H*0.6),speed:0.9,dir:-1},
                {y:Math.floor(H*0.9),speed:1.5,dir:1}
              ];
              for(var mr=0;mr<marqueeRows.length;mr++){
                var row=marqueeRows[mr];
                var baseX=((sf*row.speed*row.dir*S)%(W*2));
                if(row.dir>0)baseX=-W+baseX;else baseX=W-baseX;
                // Draw each character with color variation
                var charX=baseX;
                for(var mci=0;mci<marqueeText.length;mci++){
                  var mch=marqueeText[mci];
                  var mchW=x.measureText(mch).width;
                  // Color varies per character
                  var mHue=(mci*25+sf*2+mr*60)%360;
                  var mCol='hsl('+mHue+',100%,70%)';
                  x.globalAlpha=0.12+Math.sin(sf*0.02+mci*0.3+mr)*0.05;
                  x.fillStyle=mCol;
                  x.fillText(mch,charX,row.y);
                  charX+=mchW;
                }
                // Second copy for seamless
                charX=baseX+(row.dir>0?W*1.5:-W*1.5);
                for(var mci=0;mci<marqueeText.length;mci++){
                  var mch=marqueeText[mci];
                  var mchW=x.measureText(mch).width;
                  var mHue=(mci*25+sf*2+mr*60)%360;
                  var mCol='hsl('+mHue+',100%,70%)';
                  x.globalAlpha=0.12+Math.sin(sf*0.02+mci*0.3+mr)*0.05;
                  x.fillStyle=mCol;
                  x.fillText(mch,charX,row.y);
                  charX+=mchW;
                }}
              } // end fontReady for marquee
              x.globalAlpha=1;
              // ========== PALS LOGO TOP LEFT - CRISP PIXEL VERSION ==========
              var lS=Math.floor(28*S),lX=Math.floor(5*S),lY=Math.floor(5*S);
              // Pulsing glow rings (pixel snapped)
              for(var gr=3;gr>=0;gr--){var glowSize=Math.floor(lS+(gr*8+Math.sin(sf*0.2+gr)*4)*S);
                x.globalAlpha=0.15-gr*0.03;x.fillStyle=['#ff0000','#ffff00','#ff00ff','#00ffff'][gr%4];
                x.fillRect(Math.floor(lX-(glowSize-lS)/2),Math.floor(lY-(glowSize-lS)/2),glowSize,glowSize);}
              // Chromatic split trails - CRISP
              x.imageSmoothingEnabled=false;var logoImg=imgFullLoaded?imgFull:img;
              var logoShake=Math.floor(Math.sin(sf*0.3)*3*S);
              // Ghostly chromatic trails behind
              x.globalAlpha=0.25;x.filter='hue-rotate(-60deg) saturate(2)';
              x.drawImage(logoImg,lX-Math.floor(5*S)+logoShake,lY-Math.floor(3*S),lS,lS);
              x.filter='hue-rotate(60deg) saturate(2)';
              x.drawImage(logoImg,lX+Math.floor(5*S)-logoShake,lY+Math.floor(3*S),lS,lS);
              x.filter='hue-rotate(180deg) saturate(1.5)';x.globalAlpha=0.2;
              x.drawImage(logoImg,lX+Math.floor(Math.sin(sf*0.15)*3*S),lY+Math.floor(Math.cos(sf*0.12)*3*S),lS,lS);
              // Clean solid logo on top - SHARP AND CLEAR
              x.filter='none';
              x.imageSmoothingEnabled=false;
              x.globalAlpha=1;
              x.drawImage(logoImg,lX,lY,lS,lS);
              // ========== BRIGHT BOOT LOGS TOP LEFT ==========
              var tX=Math.floor(lX+lS+4*S),tYbase=Math.floor(lY+S);
              x.font='bold '+Math.floor(4*S)+'px monospace';
              // "aesthetic.computer" title with alarm colors
              var titlePulse=Math.floor(sf*0.2)%2===0;
              x.globalAlpha=0.9;x.fillStyle=titlePulse?'#ff6666':'#ffffff';
              x.fillText('Aesthetic.Computer',tX,tYbase);
              // Boot timer
              var sec=(now-bootStart)/1000;var secT=sec.toFixed(2)+'s';
              x.fillStyle='#ffff00';x.fillText(secT,tX,Math.floor(tYbase+5*S));
              // UTC time
              var d=new Date(),utcT=d.getUTCHours().toString().padStart(2,'0')+':'+d.getUTCMinutes().toString().padStart(2,'0')+':'+d.getUTCSeconds().toString().padStart(2,'0')+' UTC';
              x.font=Math.floor(3*S)+'px monospace';x.fillStyle='#ff8888';x.fillText(utcT,tX,Math.floor(tYbase+10*S));
              // Boot log lines - BRIGHT (with GIVE mode joke injection)
              var logStartY=Math.floor(tYbase+15*S);x.font=Math.floor(4*S)+'px monospace';
              // Dynamic counters based on boot time - randomized jumpy values
              var bootSec=(now-bootStart)/1000;
              var moneyHundreds=300+Math.floor(Math.sin(sf*0.3)*50+Math.cos(sf*0.7)*30);
              var moneyThousands=Math.floor(Math.abs(Math.sin(sf*0.5)*500+Math.cos(sf*0.2)*400+Math.sin(sf*1.3)*99));
              var moneyCount=Math.abs(moneyHundreds)*1000+moneyThousands;
              var moneyStr='$'+moneyCount.toLocaleString();
              var ageYears=33+Math.abs(Math.floor(Math.sin(sf*0.2)*2+Math.cos(sf*0.5)*2))%5;
              var ageDays=Math.abs(Math.floor(Math.sin(sf*0.4)*180+Math.cos(sf*0.8)*180))%365;
              var ageHours=Math.abs(Math.floor(Math.sin(sf*0.6)*12+Math.cos(sf*1.1)*12))%24;
              var ageMins=Math.abs(Math.floor(Math.sin(sf*0.9)*30+Math.cos(sf*1.4)*30))%60;
              var ageStr=ageYears+'y '+ageDays+'d '+ageHours+'h '+ageMins+'m';
              var giveJokes=[{text:'SPENDING '+moneyStr+'_'},{text:"USING @jeffrey's LIFE: "+ageStr+' OLD_'}];
              // Flying number particles from money display
              if(!window.moneyParticles)window.moneyParticles=[];
              var mp=window.moneyParticles;
              // Spawn new particles every few frames (reduced rate)
              if(sf%5===0&&mp.length<30){
                var digits=['$','0','1','5','9'];
                mp.push({x:tX+Math.random()*80*S,y:logStartY,vx:(Math.random()-0.5)*6*S,vy:-Math.random()*4*S-2*S,char:digits[Math.floor(Math.random()*digits.length)],life:1,rot:(Math.random()-0.5)*0.5,rotV:(Math.random()-0.5)*0.15,size:3+Math.random()*3});}
              // Update and draw particles
              x.font='bold '+Math.floor(4*S)+'px monospace';
              for(var mpi=mp.length-1;mpi>=0;mpi--){
                var p=mp[mpi];p.x+=p.vx;p.y+=p.vy;p.vy+=0.15*S;p.rot+=p.rotV;p.life-=0.02;
                if(p.life<=0||p.y>H){mp.splice(mpi,1);continue;}
                x.save();x.translate(p.x,p.y);x.rotate(p.rot);
                x.font='bold '+Math.floor(p.size*S)+'px monospace';
                var pHue=(sf*5+mpi*30)%360;x.globalAlpha=p.life*0.8;
                x.fillStyle='hsl('+pHue+',100%,70%)';x.fillText(p.char,0,0);
                x.restore();}
              // Always show joke logs at top, then real logs
              var allLogs=giveJokes.concat(lines);
              for(var li=0;li<allLogs.length&&li<8;li++){
                var logY=Math.floor(logStartY+li*4*S);if(logY>H*0.5)break;
                var logAlpha=Math.max(0.5,1-li*0.06);
                var logCol=['#ff0000','#ffff00','#ff8888','#88ffff','#ff88ff','#88ff88'][li%6];
                x.globalAlpha=logAlpha;x.fillStyle=logCol;
                x.fillText(allLogs[li].text,tX,logY);}
              x.globalAlpha=1;
              // ========== CURRENCY SYMBOLS FLYING UP (CRISP) ==========
              if(fontReady){
              var currencies=['GIVE $5','GIVE $10','GIVE $25','GIVE $50','GIVE $100','GIVE €20','GIVE £15','GIVE 0.01 ETH','GIVE 5 TEZ','GIVE $1','GIVE ₿0.001','GIVE $500'];
              var currCount=18; // Reduced from 35
              // Currency text reveals after headline+button (chars 37+)
              var currRevealed=Math.max(0,charsRevealed-37);
              for(var ci=0;ci<Math.min(currCount,Math.floor(currRevealed/2));ci++){
                var cxBase=Math.floor(W*0.2+(Math.sin(ci*1.3)*0.3+0.3)*W*0.6+Math.floor(Math.sin(sf*0.03+ci)*30*S));
                var cyBase=H+80*S;var cySpeed=(1.0+((ci*7)%10)*0.2)*S;
                var cyPos=Math.floor(cyBase-((sf*cySpeed+ci*60)%(H+150*S)));
                var cFS=Math.floor(Math.max(4,(5+Math.sin(ci*0.7)*4))*S);
                x.font='bold '+cFS+'px YWFTProcessing-Bold, monospace';
                // Draw whole string (faster than per-character)
                var currText=currencies[ci%currencies.length];
                var cHue=(ci*50+sf*3)%360;
                x.globalAlpha=0.7+Math.sin(sf*0.25+ci)*0.25;
                x.fillStyle='hsl('+cHue+',100%,70%)';
                x.fillText(currText,cxBase,cyPos);}
              } // end fontReady for currency
              // ========== SEQUENTIAL WORD HIGHLIGHT HEADLINE ==========
              if(fontReady){
              var hlWords=["IT'S","TIME","TO","GROW","INTO","SOMETHING","NEW"];
              // Word highlight cycles every ~0.8 seconds per word
              var wordCycleTime=800; // ms per word
              var cyclePos=Math.floor(now/wordCycleTime)%hlWords.length;
              // Always show "IT'S TIME TO GROW" first, then "INTO SOMETHING NEW"
              var showSet=(cyclePos<4)?0:1;
              // Landscape mode (W > H): single line, Portrait: split lines
              var isLandscape=W>H;
              var linesToShow;
              if(isLandscape){
                linesToShow=showSet===0?["IT'S TIME TO GROW"]:["INTO SOMETHING NEW"];
              }else{
                linesToShow=showSet===0?["IT'S TIME","TO GROW"]:["INTO","SOMETHING","NEW"];
              }
              // Which word in the current set should be highlighted?
              var highlightWordIdx=showSet===0?cyclePos:(cyclePos-4);
              // Map to actual words in linesToShow (line 0 has 2 words, line 1 has 1-2 words)
              var wordsInSet=showSet===0?["IT'S","TIME","TO","GROW"]:["INTO","SOMETHING","NEW"];
              var highlightWord=wordsInSet[highlightWordIdx];
              var longestLine=0;for(var lli=0;lli<linesToShow.length;lli++){if(linesToShow[lli].length>longestLine)longestLine=linesToShow[lli].length;}
              // Landscape: use more width, Portrait: constrain by height
              var giveFS=isLandscape?Math.floor(Math.min(H*0.18, W/(longestLine*0.58))):Math.floor(Math.min(H*0.14, W/(longestLine*0.65)));
              var lineH=giveFS*1.1;
              var totalH=linesToShow.length*lineH;
              var startY=Math.floor(H*0.60)-totalH/2;
              // Strong highlight colors that cycle
              var hlColors=[
                [255,255,0],   // yellow
                [0,255,255],   // cyan
                [255,0,255],   // magenta
                [0,255,0],     // green
                [255,128,0],   // orange
                [128,255,255], // light cyan
                [255,128,255]  // pink
              ];
              var activeColor=hlColors[cyclePos%hlColors.length];
              for(var lineIdx=0;lineIdx<linesToShow.length;lineIdx++){
                var lineText=linesToShow[lineIdx];
                var lineWords=lineText.split(' ');
                x.font='bold '+giveFS+'px YWFTProcessing-Bold, monospace';
                var lineW=x.measureText(lineText).width;
                var lineX=Math.floor((W-lineW)/2);
                var lineY=Math.floor(startY+lineIdx*lineH);
                var hlX=lineX;
                var wordStart=0;
                for(var wi=0;wi<lineWords.length;wi++){
                  var word=lineWords[wi];
                  var isHighlighted=(word===highlightWord);
                  var wordEnd=wordStart+word.length;
                  // Draw each character
                  for(var ci=0;ci<word.length;ci++){
                    var hlCh=word[ci];
                    var hlJX=Math.floor((Math.random()-0.5)*2*S);
                    var hlJY=Math.floor((Math.random()-0.5)*2*S);
                    var hlChW=x.measureText(hlCh).width;
                    if(isHighlighted){
                      // HIGHLIGHTED WORD - strong color with subtle glow
                      var pulse=0.7+Math.sin(now*0.015)*0.3;
                      // Single glow layer (was 3)
                      x.globalAlpha=0.4*pulse;
                      x.fillStyle='rgb('+activeColor[0]+','+activeColor[1]+','+activeColor[2]+')';
                      x.fillText(hlCh,hlX-Math.floor(3*S)+hlJX,lineY+hlJY);
                      x.fillText(hlCh,hlX+Math.floor(3*S)+hlJX,lineY+hlJY);
                      // Main highlighted char
                      x.globalAlpha=1;
                      x.fillStyle='rgb('+activeColor[0]+','+activeColor[1]+','+activeColor[2]+')';
                      x.fillText(hlCh,hlX+hlJX,lineY+hlJY);
                    }else{
                      // Non-highlighted - dimmer with subtle color shift
                      x.globalAlpha=0.4;x.fillStyle='#000000';
                      x.fillText(hlCh,hlX+Math.floor(2*S)+hlJX,lineY+Math.floor(2*S)+hlJY);
                      x.globalAlpha=0.7;
                      var dimR=180+Math.floor(Math.sin(sf*0.02+ci*0.2)*40);
                      x.fillStyle='rgb('+dimR+','+Math.floor(dimR*0.6)+','+Math.floor(dimR*0.4)+')';
                      x.fillText(hlCh,hlX+hlJX,lineY+hlJY);
                    }
                    hlX+=hlChW;
                  }
                  // Add space between words
                  if(wi<lineWords.length-1){
                    var spaceW=x.measureText(' ').width;
                    hlX+=spaceW;
                  }
                  wordStart=wordEnd+1;
                }
              }
              } // end fontReady for headline
              // (messages now flicker as main text above)
              // ========== TEXT-SHAPED GLOW BUTTON ==========
              if(fontReady){
              // Landscape: wider button, larger font. Portrait: constrained for vertical.
              var aspectRatio=W/H;
              var extremeLandscape=aspectRatio>2; // Ultra-wide (21:9 or wider)
              var btnMaxW=isLandscape?Math.min(W*0.95, extremeLandscape?900*S:600*S):Math.min(W*0.9, 420*S);
              var btnW=Math.floor(btnMaxW);var btnH=Math.floor(H*0.14);var btnX=Math.floor((W-btnW)/2);var btnY=Math.floor(H-btnH-12*S);
              // "ENTER 'give' ON PROMPT" - TEXT-SHAPED GLOW + letter by letter
              var btnFS=extremeLandscape?Math.floor(Math.min(btnH*0.7, btnW*0.06)):isLandscape?Math.floor(Math.min(btnH*0.55, btnW*0.05)):Math.floor(Math.min(btnH*0.45, btnW*0.085));
              x.font='bold '+btnFS+'px YWFTProcessing-Bold, monospace';
              var btnT="ENTER 'give' ON PROMPT";
              var totalW=0;for(var ti=0;ti<btnT.length;ti++)totalW+=x.measureText(btnT[ti]).width;
              var startX=Math.floor((W-totalW)/2);
              var baseY=Math.floor(btnY+btnH*0.62);
              // Button chars revealed after headline
              var btnCharsRevealed=Math.max(0,charsRevealed-15);
              var btnRevealed=Math.min(btnT.length,btnCharsRevealed);
              // ===== TEXT-SHAPED GLOW LAYERS =====
              for(var glLayer=6;glLayer>=1;glLayer--){
                var glowSpread=Math.floor(glLayer*3*S);
                var glowHue=(f*3+glLayer*40)%360;
                x.globalAlpha=0.15-glLayer*0.02;
                x.font='bold '+(btnFS+glowSpread)+'px YWFTProcessing-Bold, monospace';
                var glowTotalW=0;for(var gti=0;gti<btnRevealed;gti++)glowTotalW+=x.measureText(btnT[gti]).width;
                var glowStartX=Math.floor((W-glowTotalW)/2);
                var glowCurX=glowStartX;
                for(var gli=0;gli<btnRevealed;gli++){
                  var glCh=btnT[gli];
                  var glChW=x.measureText(glCh).width;
                  var glJitterX=Math.floor(Math.sin(f*0.15+gli*0.8)*2*S);
                  var glJitterY=Math.floor(Math.sin(f*0.12+gli*1.2)*3*S);
                  x.fillStyle='hsl('+glowHue+',100%,60%)';
                  x.fillText(glCh,glowCurX+glJitterX,baseY+glJitterY);
                  glowCurX+=Math.floor(glChW);}}
              // Reset font for main text
              x.font='bold '+btnFS+'px YWFTProcessing-Bold, monospace';
              var curX=startX;
              // 'give' is at index 7-10 in "ENTER 'give' ON PROMPT"
              var giveStart=7,giveEnd=10;
              for(var li=0;li<btnRevealed;li++){
                var ch=btnT[li];
                var chW=x.measureText(ch).width;
                // Bling flash on newly revealed
                var btnAge=btnCharsRevealed-li;
                var btnBling=btnAge<3?1:0;
                // Per-letter jitter and wave
                var letterJitterX=Math.floor(Math.sin(f*0.15+li*0.8)*2*S+(Math.random()-0.5)*S);
                var letterJitterY=Math.floor(Math.sin(f*0.12+li*1.2)*3*S+Math.cos(f*0.09+li)*2*S);
                var letterHue=(f*4+li*25)%360;
                // Check if this is part of 'give'
                var isGive=li>=giveStart&&li<=giveEnd;
                if(btnBling){
                  x.globalAlpha=0.9;x.fillStyle='#ffffff';
                  x.fillText(ch,curX+letterJitterX,baseY+letterJitterY);}
                // Shadow
                x.globalAlpha=0.5;x.fillStyle='#000000';
                x.fillText(ch,curX+letterJitterX+Math.floor(2*S),baseY+letterJitterY+Math.floor(2*S));
                // Chromatic split
                x.globalAlpha=0.4;x.fillStyle=isGive?'#00ff00':'#ff0000';
                x.fillText(ch,curX+letterJitterX-Math.floor(S),baseY+letterJitterY);
                x.fillStyle=isGive?'#80ff80':'#00ffff';
                x.fillText(ch,curX+letterJitterX+Math.floor(S),baseY+letterJitterY);
                // Main letter - 'give' is flashy lime green, others rainbow
                if(isGive){
                  var giveFlash=0.7+Math.sin(f*0.3+li)*0.3;
                  var giveG=Math.floor(200+Math.sin(f*0.4+li)*55);
                  x.globalAlpha=1;x.fillStyle='rgb('+Math.floor(80+Math.sin(f*0.5)*40)+','+giveG+','+Math.floor(50+Math.sin(f*0.6)*30)+')';
                }else{
                  x.globalAlpha=1;x.fillStyle='hsl('+letterHue+',100%,85%)';
                }
                x.fillText(ch,curX+letterJitterX,baseY+letterJitterY);
                curX+=Math.floor(chW);}
              } // end fontReady for button
              // ========== HEAVY COPY-PASTE GLITCHING ==========
              // Occasional screen flash
              if(Math.floor(f*0.08)%12===0){x.globalAlpha=0.1;x.fillStyle='#ffffff';x.fillRect(0,0,W,H);}
              try{
                // Multiple copy-paste glitches per frame
                var numGlitches=1+Math.floor(Math.random()*3);
                for(var cpg=0;cpg<numGlitches;cpg++){
                  if(Math.random()<0.5){
                    // Horizontal slice copy-paste
                    var cpY=Math.floor(Math.random()*H*0.9);
                    var cpH=Math.floor(5+Math.random()*40)*S;
                    var cpShiftX=Math.floor((Math.random()-0.5)*40*S);
                    var cpShiftY=Math.floor((Math.random()-0.5)*60*S);
                    if(cpY+cpH<H&&cpY>0&&cpY+cpShiftY>0&&cpY+cpShiftY+cpH<H){
                      var cpData=x.getImageData(0,cpY,W,cpH);
                      x.putImageData(cpData,cpShiftX,cpY+cpShiftY);}}
                  if(Math.random()<0.4){
                    // Vertical slice copy-paste
                    var cpX=Math.floor(Math.random()*W*0.9);
                    var cpW=Math.floor(10+Math.random()*50)*S;
                    var cpVShiftX=Math.floor((Math.random()-0.5)*30*S);
                    var cpVShiftY=Math.floor((Math.random()-0.5)*20*S);
                    if(cpX+cpW<W&&cpX>0&&cpX+cpVShiftX>0&&cpX+cpVShiftX+cpW<W){
                      var cpVData=x.getImageData(cpX,0,cpW,H);
                      x.putImageData(cpVData,cpX+cpVShiftX,cpVShiftY);}}
                  if(Math.random()<0.3){
                    // Block copy-paste
                    var blkX=Math.floor(Math.random()*W*0.7);
                    var blkY=Math.floor(Math.random()*H*0.7);
                    var blkW=Math.floor(30+Math.random()*80)*S;
                    var blkH=Math.floor(20+Math.random()*60)*S;
                    var blkDX=Math.floor((Math.random()-0.5)*100*S);
                    var blkDY=Math.floor((Math.random()-0.5)*80*S);
                    if(blkX+blkW<W&&blkY+blkH<H&&blkX+blkDX>0&&blkY+blkDY>0&&blkX+blkDX+blkW<W&&blkY+blkDY+blkH<H){
                      var blkData=x.getImageData(blkX,blkY,blkW,blkH);
                      x.putImageData(blkData,blkX+blkDX,blkY+blkDY);}}}
              }catch(e){}
              x.globalAlpha=1;requestAnimationFrame(anim);return;
            }
            // Normal boot rendering (non-GIVE mode)
            // Light mode: warm sandy/tan/cream tones (matching kidlisp.com & AC light theme); Dark mode: deep moody colors
            var BGCOLS=isLightMode?['#fcf7c5','#f5f0c0','#fffacd','#f5ebe0','#e8e3b0','#f0ebd0','#fcf5c8','#f5ecd0']:['#2d2020','#202d24','#20202d','#2d2820','#28202d','#202d2d','#2d2028','#242d20'];
            var sHH=HH*S;
            var NAMECOLS=isKidlisp?['#ffb06b','#ffc86b','#ffdf6b','#ffa86b','#ffe06b','#ff986b','#ffd06b','#ffaa6b']:(isLightMode?['#8b4513','#006400','#281e5a','#806000','#6b238e','#008080','#a0522d','#2e8b57']:['#ff6b6b','#6bff8b','#6b8bff','#ffcf6b','#cf6bff','#6bffff','#ff6bcf','#8bff6b']);
            var HDRCOLS=isKidlisp?['#3d2818','#3d3018','#3d2810','#3d3410','#3d2a18','#3d2c10','#3d2610','#3d3218']:(isLightMode?['#e8dcc8','#dce8d0','#d8d0e8','#e8e0c8','#e0d0e8','#d0e0e0','#e8d0d8','#d0e8d0']:['#3d2828','#283d2c','#28283d','#3d3428','#34283d','#283d3d','#3d2834','#2c3d28']);
            if(files.length>0){var nF=files.length,maxCols=Math.min(nF,4),colW=Math.floor(W/maxCols),pH=H;
            var hScale=Math.min(1,colW/(W/1.5)),baseFnt=Math.max(2*S,Math.floor(4*S/maxCols*2)),baseLH=Math.max(3*S,Math.floor(baseFnt*1.3));
            // Cycle through files - each column shows a different file from the rotating offset
            for(var fc=0;fc<maxCols;fc++){var fileIdx=(displayFileIdx+fc)%nF;var fi=files[fileIdx],pX=fc*colW,pY=0,bgC=BGCOLS[(displayFileIdx+fc)%BGCOLS.length],nameC=NAMECOLS[(displayFileIdx+fc)%NAMECOLS.length],hdrC=HDRCOLS[(displayFileIdx+fc)%HDRCOLS.length];
            x.fillStyle=bgC;x.globalAlpha=0.9;x.fillRect(pX,pY,colW,pH);x.globalAlpha=1;x.fillStyle=hdrC;x.fillRect(pX,pH-sHH,colW,sHH);
            x.font='bold '+Math.max(2*S,baseFnt)+'px monospace';x.fillStyle=nameC;x.fillText(fi.name,pX+2*S,pH-sHH+5*S);
            x.save();x.beginPath();x.rect(pX,pY,colW,pH-sHH);x.clip();x.translate(pX,0);x.scale(hScale,1);x.translate(-pX/hScale,0);var sY=scrollYs[fi.name]||0,cY=pY+3*S-sY,lnW=Math.floor(baseFnt*2.5);x.font=baseFnt+'px monospace';
            for(var i=0;i<fi.toks.length;i++){var y=cY+i*baseLH;if(y<pY-baseLH||y>pH-sHH+baseLH)continue;x.fillStyle=isLightMode?'#8b7355':'#4a6a8a';x.globalAlpha=0.4;x.fillText(String(i+1).padStart(2,' '),pX/hScale+1*S,y);x.globalAlpha=1;var tks=fi.toks[i],xOff=pX/hScale+lnW;for(var j=0;j<tks.length;j++){var rgb=SYN[tks[j][1]]||SYN.op;x.fillStyle='rgb('+rgb[0]+','+rgb[1]+','+rgb[2]+')';x.fillText(tks[j][0],xOff,y);xOff+=x.measureText(tks[j][0]).width;}}
            x.restore();var spd=scrollSpds[fi.name]||3;if(f%spd===0)scrollYs[fi.name]=(sY+baseLH);if(scrollYs[fi.name]>fi.toks.length*baseLH-pH+sHH+20*S)scrollYs[fi.name]=0;}}
            else{if(fileQ.length>0&&f%15===0){var fq=fileQ.shift();pages.push(mkPage(fq.name,fq.toks,S));}
            for(var i=0;i<pages.length;i++)pages[i].y-=pages[i].s*S;
            pages=pages.filter(function(p){return p.y>-p.h-50*S;});
            x.font=(2*S)+'px monospace';
            for(var i=0;i<pages.length;i++){var p=pages[i];var jx=(Math.random()-0.5)*S,jy=(Math.random()-0.5)*0.7*S;
              x.save();x.translate(p.x+p.w/2+jx,p.y+p.h/2+jy);x.rotate(p.rot||0);x.transform(1,p.skew||0,0,1,0,0);x.translate(-p.w/2,-p.h/2);
              if(Math.random()>0.7){x.globalAlpha=p.a*0.3;x.fillStyle=p.col;x.fillRect(-S+(Math.random()-0.5)*3*S,-S+(Math.random()-0.5)*3*S,p.w+2*S,p.h+2*S);}
              x.globalAlpha=p.a;x.fillStyle=p.col;x.fillRect(0,0,p.w,p.h);
              if(Math.random()>0.88){x.fillStyle=isLightMode?'#000':'#fff';x.globalAlpha=isLightMode?0.1:0.2;x.fillRect(0,Math.random()*p.h,p.w,S);}
              var pHdrCols=isLightMode?['#e8dcc8','#dce8d0','#d8d0e0','#e8e0c8','#e0d0d8','#d0e0e0','#e8d8c8','#d8e0d0']:['#3d2828','#283d2c','#28283d','#3d3428','#34283d','#283d3d','#3d2834','#2c3d28'];x.fillStyle=pHdrCols[p.hdrIdx%pHdrCols.length];x.fillRect(0,0,p.w,6*S);
              x.font='bold '+(3*S)+'px monospace';x.fillStyle=isLightMode?'#806000':'#6a9aca';x.globalAlpha=p.a;x.fillText(p.name,2*S,4*S);
              x.font=(2*S)+'px monospace';x.globalAlpha=p.a*0.85;
              for(var li=0;li<p.toks.length;li++){var ly=9*S+li*3*S;if(ly>p.h-2*S)break;
                x.fillStyle=isLightMode?'#8b7355':'#3a5a7a';x.fillText(String(li+1).padStart(3,' '),S,ly);
                var tks=p.toks[li],xOff=7*S;for(var j=0;j<tks.length;j++){var tx=tks[j][0];if(xOff+tx.length*1.4*S>p.w-S)break;var rgb=SYN[tks[j][1]]||SYN.op;x.fillStyle='rgb('+rgb[0]+','+rgb[1]+','+rgb[2]+')';x.fillText(tx,xOff,ly);xOff+=x.measureText(tx).width;}}x.restore();}}
            if(shCtx&&(shF++%2===0)){try{var sW=Math.max(1,Math.floor(W*0.5)),sH=Math.max(1,Math.floor(H*0.5));if(shCan.width!==sW)shCan.width=sW;if(shCan.height!==sH)shCan.height=sH;shCtx.clearRect(0,0,sW,sH);shCtx.filter='contrast(1.15) saturate(1.05)';shCtx.drawImage(c,0,0,sW,sH);shCtx.filter='none';x.save();x.globalAlpha=0.25;x.globalCompositeOperation='overlay';x.imageSmoothingEnabled=true;x.drawImage(shCan,0,0,W,H);x.restore();}catch(e){}}
            x.globalAlpha=isLightMode?0.02+chaos*0.04:0.04+chaos*0.08;x.fillStyle=isLightMode?'#fff':'#000';for(var yy=0;yy<H;yy+=3*S)x.fillRect(0,yy,W,S);
            if(Math.random()<0.15+chaos*0.5){var sc=2+(chaos*6)|0;for(var gi=0;gi<sc;gi++){var gy=Math.random()*H|0,gh=(S+Math.random()*3*S)|0,gs=((Math.random()-0.5)*(5+chaos*12)*S)|0;if(gy+gh<H&&gy>0)try{var sl=x.getImageData(0,gy,W,gh);x.putImageData(sl,gs,gy)}catch(e){}}}
            // Vertical scan splitting - VHS tracking distortion
            if(Math.random()<0.12+chaos*0.4){var vsc=1+(chaos*4)|0;for(var vi=0;vi<vsc;vi++){var vx=Math.random()*W|0,vw=(2*S+Math.random()*6*S)|0,vs=((Math.random()-0.5)*(4+chaos*10)*S)|0;if(vx+vw<W&&vx>0)try{var vsl=x.getImageData(vx,0,vw,H);x.putImageData(vsl,vx,vs)}catch(e){}}}
            // Occasional full-width horizontal tear/split
            if(Math.random()<0.05+chaos*0.15){var tearY=(H*0.3+Math.random()*H*0.4)|0,tearH=(H-tearY)|0,tearShift=((Math.random()-0.5)*(8+chaos*16)*S)|0;try{var tearSlice=x.getImageData(0,tearY,W,tearH);x.putImageData(tearSlice,tearShift,tearY)}catch(e){}}
            x.globalAlpha=isLightMode?0.03+chaos*0.06:0.05+chaos*0.1;for(var ni=0;ni<(15+chaos*35|0);ni++){x.fillStyle=isKidlisp?(isLightMode?'rgb('+(Math.random()*120|0)+','+(Math.random()*80|0)+','+(Math.random()*30|0)+')':'rgb('+(Math.random()*180|0)+','+(Math.random()*120|0)+','+(Math.random()*50|0)+')'):(isLightMode?'rgb('+(Math.random()*80|0)+','+(Math.random()*40|0)+','+(Math.random()*120|0)+')':'rgb('+(Math.random()*100|0)+','+(Math.random()*50|0)+','+(Math.random()*180|0)+')');x.fillRect(Math.random()*W|0,Math.random()*H|0,S,S);}
            x.globalAlpha=isLightMode?0.02+chaos*0.04:0.03+chaos*0.08*(0.5+Math.random()*0.5);x.fillStyle=isKidlisp?(isLightMode?'rgb('+(200+chaos*20)+','+(160+chaos*15)+','+(80+chaos*10)+')':'rgb('+(140+chaos*30)+','+(90+chaos*20)+','+(20+chaos*10)+')'):(isLightMode?'rgb('+(150+chaos*20)+','+(100+chaos*10)+','+(180+chaos*20)+')':'rgb('+(90+chaos*30)+','+(20+chaos*10)+','+(140+chaos*40)+')');x.fillRect(0,0,W,H);
            x.globalAlpha=1;var lS=21*S,lX=5*S,lY=5*S,tX=lX+lS+4*S;
            var pnkR=isKidlisp?(isLightMode?(180+Math.sin(t*1.5)*20+chaos*15|0):(100+Math.sin(t*1.5)*30+chaos*20|0)):(isLightMode?(120+Math.sin(t*1.5)*20+chaos*15|0):(60+Math.sin(t*1.5)*30+chaos*20|0)),pnkG=isKidlisp?(isLightMode?(120+Math.sin(t*2.1)*15+chaos*10|0):(60+Math.sin(t*2.1)*20+chaos*15|0)):(isLightMode?(80+Math.sin(t*2.1)*15+chaos*10|0):(20+Math.sin(t*2.1)*15+chaos*10|0)),pnkB=isKidlisp?(isLightMode?(60+Math.sin(t*1.8)*10+chaos*8|0):(20+Math.sin(t*1.8)*10+chaos*10|0)):(isLightMode?(160+Math.sin(t*1.8)*30+chaos*20|0):(80+Math.sin(t*1.8)*40+chaos*30|0));
            x.globalAlpha=0.5+chaos*0.15;x.fillStyle='rgb('+pnkR+','+pnkG+','+pnkB+')';x.fillRect(lX-2*S,lY-2*S,lS+4*S,lS+4*S);x.globalAlpha=1;
            x.imageSmoothingEnabled=imgFullLoaded;var logoImg=imgFullLoaded?imgFull:img;var llx=lX+sx*0.4*S,lly=lY+sy*0.4*S;
            var trCt=2+(chaos*3)|0;for(var tr=trCt;tr>=0;tr--){var trA=tr===0?1:(0.15+chaos*0.15)*(1-tr/trCt);var trO=tr*(2+chaos*3)*S;var trS=shk*(0.5+tr*0.3)*S;x.globalAlpha=trA;if(tr>0){var hue=tr*20*(Math.sin(t*2)>0?1:-1);x.filter='hue-rotate('+hue+'deg) saturate('+(1.3+chaos)+')';}x.drawImage(logoImg,llx+(Math.sin(t*3+tr)*0.5)*trS-trO*0.2,lly+(Math.cos(t*2+tr)*0.5)*trS+trO*0.15,lS,lS);x.filter='none';}
            x.globalAlpha=0.2+chaos*0.25;x.globalCompositeOperation='multiply';x.fillStyle='rgb('+(200+chaos*30|0)+','+(100+chaos*40|0)+','+(220+chaos*20|0)+')';x.fillRect(llx-2*S,lly-2*S,lS+4*S,lS+4*S);x.globalCompositeOperation='source-over';x.globalAlpha=1;
            var sec=(performance.now()-bootStart)/1000;var tss=baseShk+chaos*0.7;var tYbase=lY+S;
            x.font='bold '+(4*S)+'px monospace';var aber=(0.3+chaos*0.5)*S;var dotBounce=(Math.sin(t*4)*1+Math.sin(t*7)*0.5)*S;netAct*=0.92;
            x.globalAlpha=0.35;x.fillStyle=isKidlisp?(isLightMode?'rgb(180,100,20)':'rgb(255,180,80)'):(isLightMode?'rgb(180,80,120)':'rgb(255,120,180)');x.fillText('Aesthetic',tX-aber*0.5+sx*0.2*S,tYbase+sy*0.2*S);x.fillStyle=isKidlisp?(isLightMode?'rgb(180,140,40)':'rgb(255,220,120)'):(isLightMode?'rgb(80,180,140)':'rgb(120,255,200)');x.fillText('Aesthetic',tX+aber*0.3+sx*0.2*S,tYbase-aber*0.2+sy*0.2*S);x.globalAlpha=1;x.fillStyle=isKidlisp?(isLightMode?'rgb(120,60,0)':'rgb(255,240,200)'):(isLightMode?'rgb(100,60,120)':'rgb(240,200,255)');x.fillText('Aesthetic',tX+sx*0.2*S,tYbase+sy*0.2*S);
            var dotX=tX+x.measureText('Aesthetic').width+1*S;var dotBlink=Math.sin(t*8)>0;var dotCol=dotBlink?'rgb(0,255,0)':'rgb(255,0,0)';x.globalAlpha=0.4;x.fillStyle=dotBlink?'rgb(0,180,0)':'rgb(180,0,0)';x.fillText('.',dotX-aber*0.3+sx*0.2*S,tYbase+dotBounce+sy*0.2*S);x.fillStyle=dotBlink?'rgb(0,180,0)':'rgb(180,0,0)';x.fillText('.',dotX+aber*0.2+sx*0.2*S,tYbase+dotBounce-aber*0.15+sy*0.2*S);x.globalAlpha=1;x.fillStyle=dotCol;x.fillText('.',dotX+sx*0.2*S,tYbase+dotBounce+sy*0.2*S);
            var compX=dotX+x.measureText('.').width+1*S;x.globalAlpha=0.35;x.fillStyle=isKidlisp?(isLightMode?'rgb(180,100,20)':'rgb(255,180,80)'):(isLightMode?'rgb(180,80,120)':'rgb(255,120,180)');x.fillText('Computer',compX-aber*0.5+sx*0.2*S,tYbase+sy*0.2*S);x.fillStyle=isKidlisp?(isLightMode?'rgb(180,140,40)':'rgb(255,220,120)'):(isLightMode?'rgb(80,180,140)':'rgb(120,255,200)');x.fillText('Computer',compX+aber*0.3+sx*0.2*S,tYbase-aber*0.2+sy*0.2*S);x.globalAlpha=1;x.fillStyle=isKidlisp?(isLightMode?'rgb(120,60,0)':'rgb(255,240,200)'):(isLightMode?'rgb(100,60,120)':'rgb(240,200,255)');x.fillText('Computer',compX+sx*0.2*S,tYbase+sy*0.2*S);
            // WebSocket connection indicator
            var wsX=compX+x.measureText('Computer').width+4*S,wsT=sessionConnected?'📡':'⏳',wsPulse=sessionConnected?(0.7+Math.sin(t*2)*0.3):(0.4+Math.sin(t*6)*0.3);x.font=(3*S)+'px monospace';x.globalAlpha=wsPulse;x.fillStyle=sessionConnected?'rgb(80,255,160)':'rgb(255,120,80)';x.fillText(wsT,wsX+sx*0.2*S,tYbase+sy*0.2*S);
            var secT=sec.toFixed(2)+'s';x.font='bold '+(4*S)+'px monospace';var secX=tX;x.globalAlpha=0.4;x.fillStyle=isKidlisp?(isLightMode?'rgb(180,100,20)':'rgb(255,180,80)'):(isLightMode?'rgb(180,80,120)':'rgb(255,120,180)');x.fillText(secT,secX-aber+sx*0.15*S,tYbase+5*S+sy*0.15*S);x.fillStyle=isKidlisp?(isLightMode?'rgb(180,140,40)':'rgb(255,220,120)'):(isLightMode?'rgb(80,180,140)':'rgb(120,255,200)');x.fillText(secT,secX+aber*0.5+sx*0.15*S,tYbase+5*S-aber*0.3+sy*0.15*S);x.globalAlpha=1;x.fillStyle=isKidlisp?(isLightMode?'rgb(140,80,20)':'rgb(255,220,180)'):(isLightMode?'rgb(100,80,120)':'rgb(220,180,240)');x.fillText(secT,secX+sx*0.15*S+(Math.sin(t*5)*tss*0.2)*S,tYbase+5*S+sy*0.15*S+(Math.cos(t*4)*tss*0.2)*S);
            var d=new Date(),utcT=d.getUTCHours().toString().padStart(2,'0')+':'+d.getUTCMinutes().toString().padStart(2,'0')+':'+d.getUTCSeconds().toString().padStart(2,'0')+' UTC';x.font=(3*S)+'px monospace';x.globalAlpha=0.35;x.fillStyle=isKidlisp?(isLightMode?'rgb(180,120,60)':'rgb(255,200,140)'):(isLightMode?'rgb(180,100,140)':'rgb(255,140,200)');x.fillText(utcT,tX-aber*0.6+sx*0.15*S,tYbase+10*S+sy*0.15*S);x.fillStyle=isKidlisp?(isLightMode?'rgb(180,150,80)':'rgb(255,230,180)'):(isLightMode?'rgb(100,180,160)':'rgb(140,255,220)');x.fillText(utcT,tX+aber*0.3+sx*0.15*S,tYbase+10*S-aber*0.2+sy*0.15*S);x.globalAlpha=0.9;x.fillStyle=isKidlisp?(isLightMode?'rgb(120,80,40)':'rgb(255,220,180)'):(isLightMode?'rgb(120,100,140)':'rgb(200,170,230)');x.fillText(utcT,tX+sx*0.15*S,tYbase+10*S+sy*0.15*S);x.globalAlpha=1;
            if(uH){var hAge=(performance.now()-hST)/1000,hFade=Math.min(1,hAge*2),hPulse=0.8+0.2*Math.sin(hAge*3);x.save();x.font='bold '+(5*S)+'px monospace';var hY=tYbase+16*S,hX=tX;x.globalAlpha=hFade*hPulse*0.3;x.fillStyle=isLightMode?'#7030a0':'#a855f7';x.fillText(uH,hX+sx*0.15*S,hY+sy*0.15*S);var ha=(0.4+chaos*0.5)*S;x.globalAlpha=hFade*hPulse*0.25;x.fillStyle=isLightMode?'#c04060':'#ff6b9d';x.fillText(uH,hX-ha+sx*0.15*S,hY+sy*0.15*S);x.fillStyle=isLightMode?'#40a080':'#6bffb8';x.fillText(uH,hX+ha*0.5+sx*0.15*S,hY-ha*0.3+sy*0.15*S);x.globalAlpha=hFade*hPulse;x.fillStyle=isLightMode?'rgb(100,60,140)':'rgb(240,210,255)';x.fillText(uH,hX+sx*0.15*S,hY+sy*0.15*S);x.restore();}
            var tSY=tYbase+15*S;x.font=(4*S)+'px monospace';var now=performance.now();
            for(var i=0;i<lines.length;i++){var y=tSY+i*4*S;if(y>H-3*S)break;var al=Math.max(0.4,1-i*0.08),la=(now-lines[i].time)/1000,lineBurst=Math.max(0,lines[i].burst-la*2),dt=lines[i].text;var isActive=i===0&&dt.indexOf('_')>-1;if(isActive){var fastBlink=Math.sin(t*12)>0;dt=fastBlink?dt:dt.replace(/_$/,' ');}else if(i===0&&f%30<15)dt=dt.replace(/_$/,' ');var lsh=(baseShk*0.4+lineBurst*2+chaos)*S;var lxx=tX+sx*0.5*S+(Math.sin(t*3+i)*lsh*0.3),lyy=y+sy*0.5*S+(Math.cos(t*2+i)*lsh*0.3);var laber=(0.8+chaos*2+lineBurst)*S;if(isActive){x.globalAlpha=0.5;x.fillStyle=isLightMode?'rgb(180,140,40)':'rgb(255,200,80)';x.fillText(dt,lxx-laber*0.8,lyy);x.fillStyle=isLightMode?'rgb(140,180,80)':'rgb(200,255,120)';x.fillText(dt,lxx+laber*0.4,lyy-laber*0.2);x.globalAlpha=1;x.fillStyle=isLightMode?'rgb(120,100,40)':'rgb(255,255,150)';x.fillText(dt,lxx,lyy);}else{if(laber>S){x.globalAlpha=al*0.4;x.fillStyle=isLightMode?'rgb(180,80,120)':'rgb(255,120,180)';x.fillText(dt,lxx-laber,lyy);x.fillStyle=isLightMode?'rgb(80,180,140)':'rgb(120,255,200)';x.fillText(dt,lxx+laber*0.5,lyy-laber*0.3);}x.globalAlpha=al;x.fillStyle=isLightMode?'rgb('+(100+i*4)+','+(80+i*3)+','+(120+i*2)+')':'rgb('+(220-i*6)+','+(180-i*8)+','+(240-i*4)+')';x.fillText(dt,lxx,lyy);}}
            // VHS Rainbow Circus MOTD Slogan - wrapped, centered, animated
            if(motd){x.save();var mAge=(performance.now()-motdStart)/1000;var maxW=W*0.9;var roughChars=Math.max(6,Math.floor(maxW/(7*S)));var linesMotd=wrapMotdText(motd,roughChars);var longest=0;for(var li=0;li<linesMotd.length;li++){if(linesMotd[li].length>longest)longest=linesMotd[li].length;}
              var baseFS=Math.min(16*S,Math.max(6*S,maxW/Math.max(8,longest*1.15)));var lineH=baseFS*1.2;var maxH=H*0.55;if(linesMotd.length*lineH>maxH){baseFS=Math.max(5*S,maxH/(linesMotd.length*1.2));lineH=baseFS*1.2;}
              var breathScale=1+Math.sin(f*0.04)*0.08+Math.sin(f*0.07)*0.05;var kernWave=Math.sin(f*0.03)*0.3;var rainbowCols=['#ff0040','#ff8000','#ffff00','#00ff80','#00ffff','#0080ff','#8000ff','#ff00ff'];var charsShown=Math.min(motd.length,Math.floor(mAge*12));var shownSoFar=0;var startY=H/2-(linesMotd.length*lineH)/2+sy*0.3*S;
              for(var li=0;li<linesMotd.length;li++){var line=linesMotd[li];var lineChars=Math.max(0,Math.min(line.length,charsShown-shownSoFar));shownSoFar+=line.length;if(lineChars<=0)continue;x.font='bold '+baseFS+'px YWFTProcessing-Bold, monospace';var totalW=0;for(var mi=0;mi<lineChars;mi++){var chS=1+Math.sin((mi+f*0.08+li*2)*0.5)*0.15;totalW+=x.measureText(line[mi]).width*chS+(1+Math.sin((mi+f*0.05)*0.7)*kernWave)*3*S;}
                var mX=(W-totalW*breathScale)/2+sx*0.3*S;var mY=startY+li*lineH;for(var ri=0;ri<lineChars;ri++){var ch=line[ri],ci=Math.floor((ri+f*0.2+li*2)%rainbowCols.length),charAge=mAge-(shownSoFar-line.length+ri)/12,entryScale=Math.min(1,charAge*4),charPulse=1+Math.sin((ri+f*0.08+li)*0.5)*0.15,charScale=entryScale*charPulse*breathScale,charKern=(1+Math.sin((ri+f*0.05)*0.7)*kernWave)*3*S;x.save();var charY=mY+Math.sin((ri+f+li)*0.25)*4*S-(1-entryScale)*20*S;x.translate(mX,charY);x.scale(charScale,charScale);x.font='bold '+baseFS+'px YWFTProcessing-Bold, monospace';var cw=x.measureText(ch).width;x.globalAlpha=0.35*entryScale;x.fillStyle=rainbowCols[(ci+2)%rainbowCols.length];x.fillText(ch,-2*S/charScale,0);x.fillStyle=rainbowCols[(ci+4)%rainbowCols.length];x.fillText(ch,S/charScale,-S/charScale);x.globalAlpha=(0.85+Math.sin(f*0.1+ri+li)*0.15)*entryScale;x.fillStyle=rainbowCols[ci];x.fillText(ch,0,0);x.restore();mX+=cw*charScale+charKern;}}
              if(motdHandle){var label='mood of the day from '+motdHandle;var labelFS=Math.max(3*S,baseFS*0.45);x.font='bold '+labelFS+'px monospace';var labelW=x.measureText(label).width;var labelX=(W-labelW)/2+sx*0.2*S;var labelY=Math.min(H-3*S,startY+linesMotd.length*lineH+4*S)+sy*0.2*S;x.globalAlpha=0.6+Math.sin(f*0.05)*0.15;x.fillStyle=isLightMode?'rgb(120,80,140)':'rgb(220,180,240)';x.fillText(label,labelX,labelY);x.globalAlpha=1;}
              x.restore();}
            // Connection status VHS tint
            if(!sessionConnected){x.globalCompositeOperation='screen';x.globalAlpha=0.15+Math.sin(t*3)*0.06;x.fillStyle='rgb('+(255+Math.sin(t*7)*20|0)+','+(60+Math.cos(t*5)*30|0)+','+(100+Math.sin(t*9)*40|0)+')';x.fillRect(0,0,W,H);x.globalCompositeOperation='source-over';}
            if(connFlash>0.01){x.globalCompositeOperation='screen';x.globalAlpha=connFlash*0.6;x.fillStyle='rgb(80,255,180)';x.fillRect(0,0,W,H);x.globalCompositeOperation='source-over';}
            // Error mode - SHUTDOWN/CRASH effect with giant X then blackout
            if(errorMode||errorFlash>0.01){
              var errElapsed=errorStartTime?(performance.now()-errorStartTime)/1000:0;
              // Phase 1 (0-0.5s): Giant red X appears with intense flash
              // Phase 2 (0.5-1.0s): X stays, screen starts fading to black  
              // Phase 3 (1.0-1.5s): Full blackout before refresh
              if(errorMode&&errElapsed>1.0){
                // Phase 3: Black out completely
                x.globalCompositeOperation='source-over';x.globalAlpha=1;x.fillStyle='rgb(0,0,0)';x.fillRect(0,0,W,H);
              }else if(errorMode&&errElapsed>0.5){
                // Phase 2: X with fading to black
                var blackFade=(errElapsed-0.5)/0.5;
                x.globalCompositeOperation='source-over';x.globalAlpha=blackFade*0.9;x.fillStyle='rgb(0,0,0)';x.fillRect(0,0,W,H);
                // Draw X on top
                x.globalAlpha=1-blackFade*0.7;
                var xSize=Math.min(W,H)*0.7,xThick=Math.max(8*S,xSize*0.12),cx=W/2,cy=H/2;
                x.strokeStyle='rgb(255,0,0)';x.lineWidth=xThick;x.lineCap='round';
                x.beginPath();x.moveTo(cx-xSize/2,cy-xSize/2);x.lineTo(cx+xSize/2,cy+xSize/2);x.stroke();
                x.beginPath();x.moveTo(cx+xSize/2,cy-xSize/2);x.lineTo(cx-xSize/2,cy+xSize/2);x.stroke();
              }else{
                // Phase 1: Intense red flash + X appears
                x.globalCompositeOperation='screen';var errA=errorMode?0.4+Math.sin(t*8)*0.2:errorFlash*0.5;x.globalAlpha=errA;x.fillStyle='rgb(255,'+(40+Math.sin(t*12)*30|0)+','+(60+Math.cos(t*9)*40|0)+')';x.fillRect(0,0,W,H);
                // Glitch lines
                if(errorMode&&Math.random()<0.3){var gy=Math.random()*H|0,gh=(S*3+Math.random()*S*8)|0;x.globalAlpha=0.6;x.fillStyle='rgb(255,0,0)';x.fillRect(0,gy,W,gh);}
                x.globalCompositeOperation='source-over';
                // Draw giant X - grows in over 0.3s
                if(errorMode){
                  var xGrow=Math.min(1,errElapsed/0.3);
                  var xSize=Math.min(W,H)*0.7*xGrow,xThick=Math.max(8*S,xSize*0.12),cx=W/2,cy=H/2;
                  var xShake=8*S*(1-errElapsed*2);
                  cx+=Math.sin(t*20)*xShake;cy+=Math.cos(t*17)*xShake;
                  x.globalAlpha=0.9+Math.sin(t*15)*0.1;
                  x.strokeStyle='rgb(255,'+(20+Math.sin(t*25)*20|0)+',0)';x.lineWidth=xThick;x.lineCap='round';
                  x.shadowColor='rgb(255,0,0)';x.shadowBlur=20*S;
                  x.beginPath();x.moveTo(cx-xSize/2,cy-xSize/2);x.lineTo(cx+xSize/2,cy+xSize/2);x.stroke();
                  x.beginPath();x.moveTo(cx+xSize/2,cy-xSize/2);x.lineTo(cx-xSize/2,cy+xSize/2);x.stroke();
                  x.shadowBlur=0;
                }
              }
              x.globalCompositeOperation='source-over';x.globalAlpha=1;}
            // Touch interaction visual feedback - ripple effect
            if(touchGlitch>0.05){var rippleR=Math.max(1,(1-touchGlitch)*100*S+10*S);x.globalAlpha=touchGlitch*0.3;x.strokeStyle=isLightMode?'rgb(100,60,140)':'rgb(200,150,255)';x.lineWidth=2*S;x.beginPath();x.arc(touchX*W,touchY*H,rippleR,0,Math.PI*2);x.stroke();x.globalAlpha=1;}
            x.globalAlpha=1;requestAnimationFrame(anim);}anim();
          var obj={log:add,hide:function(){run=false;c.remove();},setHandle:setH,addFile:addFile,netPulse:netPulse,setSessionConnected:setConn,setErrorMode:setErrorMode};Object.defineProperty(obj,'motd',{get:function(){return motd;},set:function(v){motd=v;motdStart=performance.now();}});Object.defineProperty(obj,'motdHandle',{get:function(){return motdHandle;},set:function(v){motdHandle=v||'';}});return obj;})();
          window.acBOOT_LOG_CANVAS=function(m){if(window.acBootCanvas&&window.acBootCanvas.log)window.acBootCanvas.log(m);};
          window.acBOOT_ADD_FILE=function(n,s){if(window.acBootCanvas&&window.acBootCanvas.addFile)window.acBootCanvas.addFile(n,s);};
          window.acBOOT_NET_PULSE=function(){if(window.acBootCanvas&&window.acBootCanvas.netPulse)window.acBootCanvas.netPulse();};
          // Fetch MOTD for boot screen
          (async function(){try{var r=await fetch('/api/mood/moods-of-the-day');if(r.ok){var d=await r.json();if(d&&d.mood){window.acBootCanvas.motd=d.mood;if(d.handle)window.acBootCanvas.motdHandle=d.handle;}}}catch(e){}})();
          // Fetch boot files to display - more variety for cycling display
          (async function(){
            var paths=['boot.mjs','bios.mjs','lib/parse.mjs','lib/graph.mjs','lib/num.mjs','lib/disk.mjs','lib/geo.mjs','lib/text.mjs','lib/pen.mjs','lib/help.mjs'];
            // Shuffle paths for variety on each boot
            for(var i=paths.length-1;i>0;i--){var j=Math.floor(Math.random()*(i+1));var tmp=paths[i];paths[i]=paths[j];paths[j]=tmp;}
            for(var i=0;i<paths.length;i++){try{window.acBOOT_NET_PULSE();var r=await fetch('/aesthetic.computer/'+paths[i]);window.acBOOT_NET_PULSE();if(r.ok){var t=await r.text();window.acBOOT_ADD_FILE(paths[i],t);}}catch(e){}}
          })();
        </script>
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
  // 🌸 Cute compact log
  const _ms = Date.now() - _startTime;
  const _path = event.path === "/" ? "🏠" : event.path.slice(0, 20);
  console.log(`✨ ${_path} ${meta?.title || "~"} ${_ms}ms`);

  // 📊 Track piece hit (fire-and-forget, don't block response)
  // Skip hit tracking in dev mode
  if (!dev && statusCode === 200 && parsed?.text && !previewOrIcon) {
    const pieceType = parsed.path?.startsWith("@") ? "user" : "system";
    trackPieceHit(parsed.text, pieceType).catch(() => {});
  }

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
  } catch (error) {
    console.error("❌ Error in index.mjs handler:", error);
    console.error("   Path:", event.path);
    console.error("   Stack:", error.stack);
    
    return {
      statusCode: 500,
      headers: { "Content-Type": "text/plain" },
      body: `Server Error: ${error.message}\n\nPath: ${event.path}\n\nPlease check the server logs.`,
    };
  }
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
