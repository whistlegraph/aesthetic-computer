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

        // Handle special kidlisp path case
        if (path === "(...)" || path === "(...)") {
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
          window.acBootCanvas=(function(){var c=document.getElementById('boot-canvas');if(!c)return{};var x=c.getContext('2d'),W=window.innerWidth,H=window.innerHeight;c.width=W;c.height=H;window.addEventListener('resize',function(){W=window.innerWidth;H=window.innerHeight;c.width=W;c.height=H});var lS=64,lX=8,lY=8,tX=lX+lS+14,tSY=18,lH=14,mL=18;var img=new Image(),imgLoaded=!1;img.onload=function(){imgLoaded=!0};img.src='data:image/svg+xml;utf8,%3Csvg%20width%3D%2224%22%20height%3D%2224%22%20viewBox%3D%220%200%2024%2024%22%20fill%3D%22none%22%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%3E%20%3Cpath%20fill-rule%3D%22evenodd%22%20clip-rule%3D%22evenodd%22%20d%3D%22M14.8982%205.10335C15.5333%204.92226%2016.0802%204.97918%2016.6196%205.27392L16.6294%205.27925L16.6386%205.28543C16.8852%205.45034%2017.0637%205.6336%2017.1768%205.8569C17.2893%206.07886%2017.3264%206.31921%2017.3264%206.57986C17.3264%206.8465%2017.3041%207.09444%2017.2269%207.3334C17.2091%207.38846%2017.1886%207.44241%2017.1652%207.4955C17.23%207.47358%2017.2711%207.4571%2017.2859%207.44936C17.3941%207.3926%2017.5907%207.24475%2017.8166%207.06782C17.8604%207.03348%2017.9051%206.99825%2017.9497%206.96304C18.1213%206.82774%2018.2924%206.69283%2018.4123%206.61077C18.6212%206.46789%2018.9896%206.23185%2019.3662%206.01043C19.7366%205.79269%2020.1374%205.57551%2020.4022%205.48361C20.7689%205.35632%2021.2081%205.38009%2021.5334%205.72086C21.7339%205.93084%2021.8023%206.15795%2021.7913%206.36941C21.7808%206.57%2021.7001%206.73725%2021.6399%206.84298L21.6299%206.86056L21.6171%206.87629C21.4157%207.12388%2021.1869%207.28577%2020.956%207.41907C20.851%207.47973%2020.743%207.53584%2020.6386%207.59007L20.6124%207.60369C20.4982%207.66307%2020.3871%207.72144%2020.2759%207.78716C20.0167%207.94039%2019.4561%208.36643%2019.1861%208.59807C18.854%208.88299%2018.5291%209.22697%2018.2969%209.64591C18.2562%209.71926%2018.2357%209.85967%2018.246%2010.0459C18.2558%2010.2216%2018.2902%2010.397%2018.3192%2010.5068C18.3474%2010.6137%2018.369%2010.7073%2018.39%2010.7987C18.4496%2011.0574%2018.5052%2011.2984%2018.696%2011.7739C18.8086%2012.0543%2018.9341%2012.2641%2019.0783%2012.5052C19.1295%2012.5907%2019.183%2012.6802%2019.2391%2012.7781C19.2555%2012.8067%2019.2708%2012.8346%2019.2856%2012.8619C19.3428%2012.9667%2019.3941%2013.0606%2019.4805%2013.1372C19.5653%2013.2123%2019.6973%2013.2788%2019.9584%2013.218L19.9665%2013.2161L19.9748%2013.2147C20.347%2013.1537%2020.6475%2013.0147%2020.955%2012.8725C20.9664%2012.8672%2020.9778%2012.862%2020.9891%2012.8567C21.298%2012.714%2021.636%2012.5602%2022.0258%2012.5602C22.3606%2012.5602%2022.6158%2012.7005%2022.7802%2012.9167C22.9376%2013.1238%2023%2013.384%2023%2013.6229C23%2013.9455%2022.7996%2014.1926%2022.6034%2014.3582C22.4028%2014.5276%2022.1652%2014.6481%2021.9994%2014.718C21.48%2014.9369%2020.4859%2015.2891%2019.7384%2015.3685C19.7042%2015.3721%2019.661%2015.3789%2019.6105%2015.3867C19.1991%2015.4509%2018.3%2015.591%2017.7518%2014.7545C17.6407%2014.585%2017.4006%2014.2594%2017.1498%2013.9515C17.0248%2013.7981%2016.8997%2013.6521%2016.7888%2013.5341C16.6729%2013.4106%2016.5881%2013.3346%2016.5416%2013.305C16.38%2013.2022%2016.262%2013.2217%2016.1713%2013.2721C16.0628%2013.3325%2015.9743%2013.4514%2015.9373%2013.5606C15.8308%2013.8749%2015.691%2014.2874%2015.5776%2014.6879C15.4273%2015.2186%2015.2045%2016.0282%2015.0393%2016.6782C15.0149%2016.7741%2014.9905%2016.8863%2014.963%2017.0127C14.957%2017.0402%2014.9509%2017.0685%2014.9445%2017.0974C14.9098%2017.2562%2014.8705%2017.4305%2014.8234%2017.6033C14.7321%2017.9384%2014.6013%2018.3097%2014.3836%2018.556C14.0202%2018.9669%2013.4846%2019.0727%2013.0429%2018.9548C12.6009%2018.8368%2012.2125%2018.4785%2012.2125%2017.943C12.2125%2017.6301%2012.3162%2017.124%2012.4343%2016.6504C12.5535%2016.1718%2012.6958%2015.6946%2012.7899%2015.416C12.9757%2014.749%2013.2116%2013.8949%2013.3866%2013.1212C13.3979%2013.0613%2013.4099%2012.9976%2013.4226%2012.9309C13.4978%2012.5343%2013.5932%2012.031%2013.6699%2011.5717C13.7149%2011.3024%2013.7529%2011.0514%2013.7766%2010.8478C13.8015%2010.633%2013.8065%2010.5012%2013.7992%2010.4515C13.7844%2010.3497%2013.751%2010.315%2013.729%2010.2987C13.699%2010.2766%2013.6441%2010.2559%2013.5426%2010.2494C13.4417%2010.2429%2013.3238%2010.2517%2013.1852%2010.2649C13.1692%2010.2664%2013.1529%2010.268%2013.1363%2010.2696C13.0172%2010.2811%2012.8839%2010.294%2012.7597%2010.294C12.6223%2010.294%2012.472%2010.2977%2012.3149%2010.3015%2011.9756%2010.3098%2011.605%2010.3188%2011.2661%2010.2933%2011.1149%2010.2819%2010.9352%2010.258%2010.7544%2010.2337L10.7295%2010.2304C10.5535%2010.2067%2010.3742%2010.1825%2010.2029%2010.1659C10.0225%2010.1485%209.86134%2010.1404%209.7317%2010.1484%209.59167%2010.157%209.53289%2010.1823%209.51826%2010.1932%209.40563%2010.2772%209.36293%2010.3344%209.34618%2010.3683%209.33318%'var lines=[],lc=0,bp=0,cv=true,ct=0,lastLog=performance.now(),lb=0,bootStart=performance.now();var SYN={kw:[197,134,192],fn:[220,220,170],str:[206,145,120],num:[181,206,168],cmt:[106,153,85],op:[212,212,212],tp:[78,201,176],vr:[156,220,254]};var codeToks=[[['const','kw'],[' boot ','vr'],['=','op'],[' async ','kw'],['() => {','op']],[['await','kw'],[' loadSystem','fn'],['();','op']],[['import','kw'],[' { disk } ','vr'],['from','kw'],[' "./lib"','str'],[';','op']],[['export','kw'],[' function','kw'],[' paint','fn'],['(api) {','op']],[['wipe','fn'],['(','op'],['32','num'],[', ','op'],['0','num'],[', ','op'],['64','num'],[');','op']],[['ink','fn'],['(','op'],['255','num'],[', ','op'],['200','num'],[', ','op'],['100','num'],[').','op'],['box','fn'],['();','op']],[['if','kw'],[' (event.','vr'],['is','fn'],['(','op'],['"touch"','str'],[')) {','op']],[['const','kw'],[' [x, y] = api.','vr'],['pen','vr'],[';','op']],[['return','kw'],[' { meta, boot, act };','vr']],[['async','kw'],[' function','kw'],[' connect','fn'],['(ws) {','op']],[['socket.','vr'],['send','fn'],['(','op'],['JSON','tp'],['.stringify(d));','op']],[['class','kw'],[' Aesthetic','tp'],[' {','op']],[['constructor','fn'],['(ctx) { ','op'],['this','kw'],['.ctx = ctx; }','op']],[['render','fn'],['() { ','op'],['this','kw'],['.draw(); }','op']],[['} ','op'],['// end module','cmt']],[['const','kw'],[' frame = ','vr'],['requestAnimationFrame','fn'],[';','op']],[['ctx.','vr'],['fillStyle','vr'],[' = ','op'],['rgb','fn'],['(r, g, b);','op']],[['for','kw'],[' (','op'],['let','kw'],[' i = ','vr'],['0','num'],['; i < n; i++) {','op']],[['pixels[i] = ','vr'],['noise16','fn'],['() | ','op'],['0','num'],[';','op']],[['Promise','tp'],['.all(tasks);','op']],[['// async/await','cmt']],[['(() => { ','op'],['/* IIFE */','cmt'],[' })();','op']]];var codeCols=[[],[],[],[]],codeMax=60,codeSpd=2.5,fontSz=[9,10,11,12,13,14];for(var pi=0;pi<4;pi++)for(var pj=0;pj<25;pj++)codeCols[pi].push({toks:codeToks[Math.random()*codeToks.length|0],y:H-pj*28-Math.random()*20,al:0.25+Math.random()*0.45,spd:codeSpd+Math.random()*1.5,sz:fontSz[Math.random()*fontSz.length|0]});var f=0,run=true;function add(m){var now=performance.now(),dt=now-lastLog;lastLog=now;var spd=Math.min(1,500/Math.max(50,dt));lb=0.5+spd*0.5;if(lines.length>0)lines[0].text=lines[0].text.replace(/_$/,'');lines.unshift({text:m+'_',time:now,burst:lb});if(lines.length>mL)lines.pop();lc++;bp=Math.min(1,lc/15)}function anim(){if(!run||!document.getElementById('boot-canvas'))return;var t=f*0.05;f++;ct++;if(ct%15===0)cv=!cv;lb*=0.9;var chaos=bp+lb*0.5,cL=bp;var baseShk=0.5+Math.sin(t*2)*0.3,shk=baseShk+chaos*chaos*5+lb*4,sx=(Math.random()-0.5)*shk,sy=(Math.random()-0.5)*shk;var spawnCt=2+(chaos*2)|0;for(var sp=0;sp<spawnCt;sp++){var col=Math.random()*4|0,toks=codeToks[Math.random()*codeToks.length|0],sz=fontSz[Math.random()*fontSz.length|0];codeCols[col].unshift({toks:toks,y:H+5+Math.random()*15,al:0.25+Math.random()*0.5,spd:codeSpd+Math.random()*1.5,sz:sz});if(codeCols[col].length>codeMax)codeCols[col].pop()}for(var ci=0;ci<codeCols.length;ci++)for(var cj=0;cj<codeCols[ci].length;cj++)codeCols[ci][cj].y-=codeCols[ci][cj].spd+chaos*1.2;codeCols=codeCols.map(function(col){return col.filter(function(c){return c.y>-20})});x.clearRect(0,0,W,H);var codeStartX=6,codeW=W-12,colW=codeW/4;for(var ci=0;ci<codeCols.length;ci++){var baseX=codeStartX+ci*colW;for(var cj=0;cj<codeCols[ci].length;cj++){var cc=codeCols[ci][cj];if(cc.y<5||cc.y>H-5)continue;var codeAl=cc.al*(0.35+chaos*0.4);x.font=cc.sz+'px "Courier New",monospace';var xOff=0;for(var tk=0;tk<cc.toks.length;tk++){var tok=cc.toks[tk],txt=tok[0],ck=tok[1],rgb=SYN[ck]||SYN.op;var cr=Math.min(255,rgb[0]+chaos*20)|0,cg=Math.max(0,rgb[1]-chaos*15)|0,cb=Math.min(255,rgb[2]+chaos*10)|0;x.fillStyle='rgb('+cr+','+cg+','+cb+')';x.globalAlpha=codeAl;var cs=0.3+chaos*2,tx=baseX+xOff+(Math.random()-0.5)*cs,ty=cc.y+(Math.random()-0.5)*cs;x.fillText(txt,tx,ty);xOff+=x.measureText(txt).width}}}x.globalAlpha=1;var ls=baseShk*0.8+chaos*chaos*3;if(imgLoaded||img.complete){x.drawImage(img,lX+sx*0.5+(Math.random()-0.5)*ls,lY+sy*0.5+(Math.random()-0.5)*ls,lS,lS)}else{try{x.save();x.globalAlpha=0.95;x.fillStyle='rgb('+(140+(Math.random()*80|0))+','+(60+(Math.random()*40|0))+','+(150+(Math.random()*80|0))+')';x.beginPath();x.arc(lX+lS/2,lY+lS/2,lS/2,0,6.283185307179586);x.fill();x.restore()}catch(e){}}var ms=performance.now()-bootStart;x.font='bold 10px "Courier New",monospace';var ts=String(ms|0).padStart(5,'0')+'ms';x.globalAlpha=0.6+chaos*0.4;x.fillStyle='rgb('+(80+chaos*120|0)+','+(80+chaos*40|0)+','+(100+chaos*100|0)+')';var tss=baseShk+chaos*2;x.fillText(ts,lX+4+sx*0.5+(Math.random()-0.5)*tss,lY+lS+14+sy*0.5+(Math.random()-0.5)*tss);x.globalAlpha=1;x.font='12px "Courier New",monospace';var now=performance.now();for(var i=0;i<lines.length;i++){var y=tSY+i*lH;if(y>H-10)continue;var age=i*0.08,al=Math.max(0.25,1-age*0.6),la=(now-lines[i].time)/1000,lineBurst=Math.max(0,lines[i].burst-la*2),dt=lines[i].text;if(i===0&&!cv)dt=dt.replace(/_$/,' ');var lsh=baseShk*0.5+lineBurst*3+chaos*1.5,lxx=tX+sx+(Math.random()-0.5)*lsh,lyy=y+sy+(Math.random()-0.5)*lsh;var bR=200-cL*100-i*5,bG=80-cL*55-i*4,bB=140+cL*60-i*2;x.globalAlpha=al*(0.15+chaos*0.25+lineBurst*0.3);x.filter='blur('+(2+chaos*3)+'px)';x.fillStyle='rgb('+Math.min(255,bR+40)+','+Math.max(0,bG+20)+','+Math.min(255,bB+30)+')';x.fillText(dt,lxx,lyy);x.filter='none';x.globalAlpha=al;x.fillStyle='rgb('+Math.max(0,Math.min(255,bR+lineBurst*25))+','+Math.max(0,bG)+','+Math.min(255,bB+lineBurst*15)+')';x.fillText(dt,lxx,lyy)}x.globalAlpha=1;x.globalAlpha=0.03+chaos*0.12;x.fillStyle='#000';for(var yy=0;yy<H;yy+=3)x.fillRect(0,yy,W,1);if(Math.random()<0.05+chaos*0.5){var sc=1+(chaos*6)|0;for(var i=0;i<sc;i++){var gy=Math.random()*H|0,gh=2+Math.random()*6|0,gs=((Math.random()-0.5)*(8+chaos*25))|0;if(gy+gh<H&&gy>0)try{var sl=x.getImageData(0,gy,W,gh);x.putImageData(sl,gs,gy)}catch(e){}}}var na=10+chaos*70|0;x.globalAlpha=0.03+chaos*0.1;for(var i=0;i<na;i++){x.fillStyle='rgb('+(Math.random()*100|0)+','+(Math.random()*40|0)+','+(Math.random()*200|0)+')';x.fillRect(Math.random()*W|0,Math.random()*H|0,1+Math.random()*2,1)}x.globalAlpha=0.02+chaos*0.12*(0.5+Math.random()*0.5);x.fillStyle='rgb('+(100+cL*20)+','+(25+cL*10)+','+(160+cL*40)+')';x.fillRect(0,0,W,H);x.globalAlpha=1;requestAnimationFrame(anim)}anim();return{log:add,hide:function(){run=false;c.remove()}}})();window.acBOOT_LOG_CANVAS=function(m){if(window.acBootCanvas&&window.acBootCanvas.log)window.acBootCanvas.log(m)};
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
