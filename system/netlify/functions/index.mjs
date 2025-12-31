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
async function trackPieceHit(piece, type, headers) {
  try {
    const baseUrl = dev ? "https://localhost:8888" : "https://aesthetic.computer";
    const { got } = await import("got");
    await got.post(`${baseUrl}/api/piece-hit`, {
      json: { piece, type },
      headers: {
        Authorization: headers.authorization || headers.Authorization || "",
      },
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
  try {
    // TODO: Return a 500 or 404 for everything that does not exist...
    //       - [] Like for example if the below import fails...

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

  // Serve GIF files directly as static assets (for mockup previews, etc.)
  if (event.path.endsWith(".gif")) {
    try {
      const gifPath = path.join(process.cwd(), "public/aesthetic.computer", event.path.slice(1));
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
      const webpPath = path.join(process.cwd(), "public/aesthetic.computer", event.path.slice(1));
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

  const { title, desc, ogImage, icon, twitterImage, manifest } = metadata(
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
                console.log("🎹 HTML send() forwarding to bios:", msg.type);
                dawSend(msg);
              } else {
                console.log("🎹 HTML send() queueing (no bios yet):", msg.type);
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
        <script
          crossorigin="anonymous"
          src="/aesthetic.computer/boot.mjs"
          type="module"
          defer
        ></script>
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
        <link
          rel="stylesheet"
          crossorigin="anonymous"
          href="/aesthetic.computer/style.css"
        />
      </head>
      <body class="native-cursor" ${lanHost ? " data-lan-host=" + lanHost : ""}>
        <!-- Boot Canvas - syntax highlighted, variable sizes, flush left -->
        <canvas id="boot-canvas" style="position:fixed;top:0;left:0;width:100vw;height:100vh;z-index:99999;pointer-events:none;margin:0;padding:0;"></canvas>
        <script>
          window.acBootCanvas=(function(){var c=document.getElementById('boot-canvas');if(!c)return{};var x=c.getContext('2d'),W=window.innerWidth,H=window.innerHeight;c.width=W;c.height=H;window.addEventListener('resize',function(){W=window.innerWidth;H=window.innerHeight;c.width=W;c.height=H});var lS=64,lX=8,lY=8,tX=lX+lS+14,tSY=18,lH=14,mL=18;var tinyPng='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAABWUlEQVR4nN2Ru0pDQRCGg3fUzk5bEVS0ELxVwQsYTRBvJ5ed3VgZVBB8g1Q+gCDIzq6IluclUktmT1Kk8mWUWT2RTcBep1nmZ//vn53NZP5XOWU3ndTlOIoHe/UE8MxJM8s9SdxuVR9nAnMneph0Up+25esECSwEAGFK3gh4SwIvnDQ5UrgXAN5v7kcJ9HUjWx/iKZwyJ6RsxMkEWPzSTY7A5psVsxaYG+fPYyTNFZ8+qaaHSRjhzcpGrSou+knKdprH70t2Ci+Z7qS56wIAj9M7DPkJskcBIAG96gHKzKdv/X5vkU+/E7B5AtxI4GnBKasSpXf5brBIqulxB/qQjZzO4/uxwe68qZep7l6EXmKNQemv+GIzE5vKHKTL87owpbbCOQK9wj1Jvc59prcSwAqnd6J4hM0pgGGsf9TrA32mACD0Pu/Dp1bNcqJ01gMEFkjYrV/Nf7s+AS1XxDy7PXOrAAAAAElFTkSuQmCC',img=new Image(),imgFull=new Image(),imgFullLoaded=!1;img.src=tinyPng;imgFull.onload=function(){imgFullLoaded=!0};imgFull.src='/purple-pals.svg';var uH=null,hST=0;var SYN={kw:[197,134,192],fn:[220,220,170],str:[206,145,120],num:[181,206,168],cmt:[106,153,85],op:[212,212,212],tp:[78,201,176],vr:[156,220,254]};var srcF=[[['// aesthetic.computer boot sequence','cmt']],[['','op']],[['import','kw'],[' { disk, bios } ','vr'],['from','kw'],[' ','op'],['"./lib"','str'],[';','op']],[['import','kw'],[' { paint, act } ','vr'],['from','kw'],[' ','op'],['"./piece"','str'],[';','op']],[['','op']],[['const','kw'],[' canvas ','vr'],['=','op'],[' document.','vr'],['getElementById','fn'],['(','op'],['"screen"','str'],[');','op']],[['const','kw'],[' ctx ','vr'],['=','op'],[' canvas.','vr'],['getContext','fn'],['(','op'],['"2d"','str'],[');','op']],[['','op']],[['async','kw'],[' function','kw'],[' boot','fn'],['() {','op']],[['  ','op'],['await','kw'],[' bios.','vr'],['init','fn'],['();','op']],[['  ','op'],['const','kw'],[' user ','vr'],['=','op'],[' await','kw'],[' auth','fn'],['();','op']],[['  ','op'],['console.','vr'],['log','fn'],['(','op'],['"🚀 booting..."','str'],[');','op']],[['  ','op'],['return','kw'],[' { user, disk };','vr']],[['}','op']],[['','op']],[['function','kw'],[' render','fn'],['(t) {','op']],[['  ','op'],['wipe','fn'],['(','op'],['32','num'],[', ','op'],['0','num'],[', ','op'],['64','num'],[');','op']],[['  ','op'],['ink','fn'],['(','op'],['255','num'],[', ','op'],['200','num'],[', ','op'],['100','num'],[');','op']],[['  ','op'],['box','fn'],['(x, y, w, h);','op']],[['  ','op'],['requestAnimationFrame','fn'],['(render);','op']],[['}','op']],[['','op']],[['class','kw'],[' Aesthetic','tp'],[' {','op']],[['  ','op'],['constructor','fn'],['(config) {','op']],[['    ','op'],['this','kw'],['.config ','vr'],['=','op'],[' config;','op']],[['    ','op'],['this','kw'],['.ready ','vr'],['=','op'],[' false','kw'],[';','op']],[['  }','op']],[['','op']],[['  ','op'],['async','kw'],[' init','fn'],['() {','op']],[['    ','op'],['await','kw'],[' this','kw'],['.','op'],['loadAssets','fn'],['();','op']],[['    ','op'],['this','kw'],['.ready ','vr'],['=','op'],[' true','kw'],[';','op']],[['  }','op']],[['}','op']],[['','op']],[['// event handlers','cmt']],[['function','kw'],[' act','fn'],['({ event }) {','op']],[['  ','op'],['if','kw'],[' (event.','vr'],['is','fn'],['(','op'],['"touch"','str'],[')) {','op']],[['    ','op'],['const','kw'],[' { x, y } ','vr'],['=','op'],[' event;','op']],[['    ','op'],['ink','fn'],['(','op'],['"red"','str'],[').','op'],['circle','fn'],['(x, y, ','op'],['10','num'],[');','op']],[['  }','op']],[['}','op']],[['','op']],[['export','kw'],[' { boot, render, act };','vr']]];var codeLines=[],srcI=0,CODE_MAX=35,CODE_SPD=2.2;for(var i=0;i<25;i++){var idx=i%srcF.length;codeLines.push({toks:srcF[idx],y:H-i*18-Math.random()*5,a:0.25+Math.random()*0.35,s:CODE_SPD+Math.random()*0.8,sz:11})}srcI=25%srcF.length;var lines=[],lc=0,bp=0,cv=true,ct=0,lastLog=performance.now(),lb=0,bootStart=performance.now();var f=0,run=true;function add(m){var now=performance.now(),dt=now-lastLog;lastLog=now;var spd=Math.min(1,500/Math.max(50,dt));lb=0.5+spd*0.5;if(lines.length>0)lines[0].text=lines[0].text.replace(/_$/,'');lines.unshift({text:m+'_',time:now,burst:lb});if(lines.length>mL)lines.pop();lc++;bp=Math.min(1,lc/15)}function setH(h){uH=h;hST=performance.now()}function anim(){if(!run||!document.getElementById('boot-canvas'))return;var t=f*0.05;f++;ct++;if(ct%15===0)cv=!cv;lb*=0.9;var chaos=bp+lb*0.5,cL=bp;var baseShk=0.5+Math.sin(t*2)*0.3,shk=baseShk+chaos*chaos*5+lb*4,sx=(Math.random()-0.5)*shk,sy=(Math.random()-0.5)*shk;if(f%3===0){codeLines.push({toks:srcF[srcI],y:H+5,a:0.3+Math.random()*0.3,s:CODE_SPD+Math.random()*0.5+chaos*0.5,sz:11});srcI=(srcI+1)%srcF.length;if(codeLines.length>CODE_MAX)codeLines.shift()}for(var i=0;i<codeLines.length;i++)codeLines[i].y-=codeLines[i].s;codeLines=codeLines.filter(function(c){return c.y>-20});x.clearRect(0,0,W,H);for(var i=0;i<codeLines.length;i++){var ln=codeLines[i];if(ln.y<0||ln.y>H)continue;var codeAl=ln.a*(0.4+chaos*0.5);x.font=ln.sz+'px "Courier New",monospace';x.globalAlpha=codeAl;var cShk=0.5+chaos*2,bX=10+(Math.random()-0.5)*cShk,yy=ln.y+(Math.random()-0.5)*cShk,xOff=0;for(var j=0;j<ln.toks.length;j++){var tok=ln.toks[j],txt=tok[0],ck=tok[1],rgb=SYN[ck]||SYN.op;x.fillStyle='rgb('+rgb[0]+','+rgb[1]+','+rgb[2]+')';x.fillText(txt,bX+xOff,yy);xOff+=x.measureText(txt).width}}x.globalAlpha=1;var ls=baseShk*0.8+chaos*chaos*3;x.imageSmoothingEnabled=imgFullLoaded;var logoImg=imgFullLoaded?imgFull:img;var llx=lX+sx*0.5,lly=lY+sy*0.5;var trailCt=3+(chaos*4)|0;for(var tr=trailCt;tr>=0;tr--){var trA=tr===0?1:(0.15+chaos*0.2)*(1-tr/trailCt);var trO=tr*(2+chaos*4);var trS=ls*(1+tr*0.5);x.globalAlpha=trA;if(tr>0){var hue=tr*15*(Math.random()>0.5?1:-1);x.filter='hue-rotate('+hue+'deg) saturate('+(1.5+chaos)+')'}x.drawImage(logoImg,llx+(Math.random()-0.5)*trS-trO*0.3,lly+(Math.random()-0.5)*trS+trO*0.2,lS,lS);x.filter='none'}x.globalAlpha=0.3+chaos*0.4;x.globalCompositeOperation='multiply';x.fillStyle='rgb('+(180+chaos*40|0)+','+(80+chaos*30|0)+','+(200+chaos*30|0)+')';x.fillRect(llx-2,lly-2,lS+4,lS+4);x.globalCompositeOperation='source-over';x.globalAlpha=1;x.imageSmoothingEnabled=!0;var ms=performance.now()-bootStart;x.font='bold 10px "Courier New",monospace';var ts=String(ms|0).padStart(5,'0')+'ms';x.globalAlpha=0.6+chaos*0.4;x.fillStyle='rgb('+(80+chaos*120|0)+','+(80+chaos*40|0)+','+(100+chaos*100|0)+')';var tss=baseShk+chaos*2;x.fillText(ts,lX+4+sx*0.5+(Math.random()-0.5)*tss,lY+lS+14+sy*0.5+(Math.random()-0.5)*tss);x.globalAlpha=1;if(uH){var hAge=(performance.now()-hST)/1000,hFade=Math.min(1,hAge*2),hPulse=0.8+0.2*Math.sin(hAge*3);x.save();x.font='bold 18px "Courier New",monospace';var hY=lY+lS+32,hX=lX+4;x.globalAlpha=hFade*hPulse*0.4;x.filter='blur(8px)';x.fillStyle='#a855f7';x.fillText(uH,hX,hY);x.filter='none';var ha=1.5+chaos*2;x.globalAlpha=hFade*hPulse*0.3;x.fillStyle='#ff6b9d';x.fillText(uH,hX-ha,hY);x.fillStyle='#6bffb8';x.fillText(uH,hX+ha*0.5,hY-ha*0.3);x.globalAlpha=hFade*hPulse;x.fillStyle='#e879f9';x.fillText(uH,hX+sx*0.3,hY+sy*0.3);x.restore()}x.font='12px "Courier New",monospace';var now=performance.now();for(var i=0;i<lines.length;i++){var y=tSY+i*lH;if(y>H-10)continue;var age=i*0.08,al=Math.max(0.25,1-age*0.6),la=(now-lines[i].time)/1000,lineBurst=Math.max(0,lines[i].burst-la*2),dt=lines[i].text;if(i===0&&!cv)dt=dt.replace(/_$/,' ');var lsh=baseShk*0.5+lineBurst*3+chaos*1.5,lxx=tX+sx+(Math.random()-0.5)*lsh,lyy=y+sy+(Math.random()-0.5)*lsh;var bR=200-cL*100-i*5,bG=80-cL*55-i*4,bB=140+cL*60-i*2;var aber=1+chaos*3+lineBurst*2;if(aber>1.5){x.globalAlpha=al*0.4;x.fillStyle='rgb(255,50,100)';x.fillText(dt,lxx-aber,lyy);x.fillStyle='rgb(50,255,150)';x.fillText(dt,lxx+aber*0.5,lyy-aber*0.3);x.fillStyle='rgb(100,100,255)';x.fillText(dt,lxx+aber*0.3,lyy+aber*0.5)}x.globalAlpha=al*(0.15+chaos*0.25+lineBurst*0.3);x.filter='blur('+(2+chaos*3)+'px)';x.fillStyle='rgb('+Math.min(255,bR+40)+','+Math.max(0,bG+20)+','+Math.min(255,bB+30)+')';x.fillText(dt,lxx,lyy);x.filter='none';x.globalAlpha=al;x.fillStyle='rgb('+Math.max(0,Math.min(255,bR+lineBurst*25))+','+Math.max(0,bG)+','+Math.min(255,bB+lineBurst*15)+')';x.fillText(dt,lxx,lyy)}x.globalAlpha=1;x.globalAlpha=0.03+chaos*0.12;x.fillStyle='#000';for(var yy=0;yy<H;yy+=3)x.fillRect(0,yy,W,1);if(Math.random()<0.05+chaos*0.5){var sc=1+(chaos*6)|0;for(var i=0;i<sc;i++){var gy=Math.random()*H|0,gh=2+Math.random()*6|0,gs=((Math.random()-0.5)*(8+chaos*25))|0;if(gy+gh<H&&gy>0)try{var sl=x.getImageData(0,gy,W,gh);x.putImageData(sl,gs,gy)}catch(e){}}}var na=10+chaos*70|0;x.globalAlpha=0.03+chaos*0.1;for(var i=0;i<na;i++){x.fillStyle='rgb('+(Math.random()*100|0)+','+(Math.random()*40|0)+','+(Math.random()*200|0)+')';x.fillRect(Math.random()*W|0,Math.random()*H|0,1+Math.random()*2,1)}x.globalAlpha=0.02+chaos*0.12*(0.5+Math.random()*0.5);x.fillStyle='rgb('+(100+cL*20)+','+(25+cL*10)+','+(160+cL*40)+')';x.fillRect(0,0,W,H);var dS=0.3,dA=Math.max(0,(chaos-dS)/(1-dS));if(dA>0.05){var pS=1+(dA*dA*8)|0;if(pS>1){var sW=Math.max(1,(W/pS)|0),sH=Math.max(1,(H/pS)|0);x.imageSmoothingEnabled=false;try{var tc=document.createElement('canvas');tc.width=sW;tc.height=sH;var tx=tc.getContext('2d');tx.drawImage(c,0,0,sW,sH);x.clearRect(0,0,W,H);x.drawImage(tc,0,0,W,H)}catch(e){}x.imageSmoothingEnabled=true}var bA=dA*dA*4;if(bA>0.5){x.filter='blur('+bA+'px)';x.globalAlpha=0.3+dA*0.5;try{x.drawImage(c,0,0)}catch(e){}x.filter='none';x.globalAlpha=1}}x.globalAlpha=1;requestAnimationFrame(anim)}anim();return{log:add,hide:function(){run=false;c.remove()},setHandle:setH}})();window.acBOOT_LOG_CANVAS=function(m){if(window.acBootCanvas&&window.acBootCanvas.log)window.acBootCanvas.log(m)};
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
  if (statusCode === 200 && parsed?.text && !previewOrIcon) {
    const pieceType = parsed.path?.startsWith("@") ? "user" : "system";
    trackPieceHit(parsed.text, pieceType, event.headers).catch(() => {});
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
