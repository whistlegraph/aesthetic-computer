// Media

export const config = { path: "/media/*" };

export default async function handleRequest(request) {
  const url = new URL(request.url);
  const path = url.pathname.split("/");
  let newUrl;

  if (path[1] === "media") {
    const resourcePath = path.slice(2).join("/");

    // Handle /media/tapes/CODE or /media/paintings/CODE routes
    if (path[2] === "tapes" && path[3]) {
      return await handleTapeCodeRequest(path[3]);
    }
    
    if (path[2] === "paintings" && path[3]) {
      return await handlePaintingCodeRequest(path[3]);
    }

    if (!path[2]?.includes("@") && !path[2]?.match(/^ac[a-z0-9]+$/i)) {
      // No @ prefix and not a user code (acXXXXX format) - treat as direct file path
      const extension = resourcePath.split(".").pop()?.toLowerCase();
      
      // Special handling for .mp4 tape files
      if (extension === "mp4" && resourcePath.match(/^[^/]+\/[^/]+-\d+\.mp4$/)) {
        return await handleTapeMp4Request(resourcePath);
      }
      
      const baseUrl =
        extension === "mjs"
          ? "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com"
          : "https://user.aesthetic.computer";

      newUrl = `${baseUrl}/${resourcePath}`;
      // Properly encode the URL, especially the pipe character in Auth0 user IDs
      const response = await fetch(newUrl.split('/').map((part, i) => i < 3 ? part : encodeURIComponent(part)).join('/'));
      const contentType = response.headers.get("Content-Type");
      const moddedResponse = new Response(response.body, {
        headers: { ...response.headers },
        status: response.status,
        statusText: response.statusText,
      });
      moddedResponse.headers.set("Access-Control-Allow-Origin", "*");
      if (contentType) {
        moddedResponse.headers.set("Content-Type", contentType);
      }
      return moddedResponse;
    } else {
      // Handle both @username and acXXXXX user code formats
      const userIdentifier = path[2];
      const userId = await queryUserID(userIdentifier, request);
      
      if (!userId) {
        return new Response(`User not found: ${userIdentifier}`, { status: 404 });
      }
      
      const newPath = `${userId}/${path.slice(3).join("/")}`;

      if (newPath.split("/").pop().split(".")[1]?.length > 0) {
        if (newPath.split(".").pop() === "mjs") {
          newUrl = `https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/${newPath}`;
        } else {
          newUrl = `https://user.aesthetic.computer/${newPath}`;
        }
        // TODO: How can I ensure that Allow-Origin * can be here?
        // Properly encode the URL, especially the pipe character in Auth0 user IDs
        const response = await fetch(newUrl.split('/').map((part, i) => i < 3 ? part : encodeURIComponent(part)).join('/'));
        // Create a new Response object using the fetched response's body

        const contentType = response.headers.get("Content-Type");

        const moddedResponse = new Response(response.body, {
          // Copy all the fetched response's headers
          headers: { ...response.headers },
          status: response.status,
          statusText: response.statusText,
        });
        // // Set the Access-Control-Allow-Origin header to *
        moddedResponse.headers.set("Access-Control-Allow-Origin", "*");
        moddedResponse.headers.set("Content-Type", contentType);
        return moddedResponse;
        // return fetch(encodeURI(newUrl));
      } else {
        const path = newPath.replace("/media", "");
        newUrl = `/media-collection?for=${path}`;
        return new URL(newUrl, request.url);
      }
    }
  } else {
    return new Response("💾 Not a `media` path.", { status: 500 });
  }
}

async function queryUserID(userIdentifier, request) {
  // Use the same host as the incoming request
  const requestUrl = new URL(request.url);
  const host = `${requestUrl.protocol}//${requestUrl.host}`;
  
  // Determine if it's a user code (acXXXXX) or handle (@username)
  let url;
  if (userIdentifier.match(/^ac[a-z0-9]+$/i)) {
    // User code format - query by code
    url = `${host}/user?code=${encodeURIComponent(userIdentifier)}`;
  } else {
    // Handle format (with or without @)
    url = `${host}/user?from=${encodeURIComponent(userIdentifier)}`;
  }
  
  try {
    const res = await fetch(url);
    if (res.ok) {
      const json = await res.json();
      return json.sub;
    } else {
      console.error(`Error: ${res.status} ${res.statusText}`);
      console.error(
        `Response headers: ${JSON.stringify(
          Array.from(res.headers.entries()),
        )}`,
      );
      return null;
    }
  } catch (error) {
    console.error(`Fetch failed: ${error}`);
    return null;
  }
}

/**
 * Handle tape code requests like /media/tapes/A6LwKTML
 * @param {string} code - Tape code
 * @returns {Response}
 */
async function handleTapeCodeRequest(code) {
  const host = Deno.env.get("CONTEXT") === "dev" 
    ? "https://localhost:8888" 
    : "https://aesthetic.computer";
  
  try {
    // Query tape by code
    const res = await fetch(`${host}/.netlify/functions/get-tape?code=${encodeURIComponent(code)}`);
    
    if (!res.ok) {
      return new Response(`Tape not found: ${code}`, { status: 404 });
    }
    
    const tape = await res.json();
    
    // Redirect to the ZIP file
    const bucket = tape.bucket || "art-aesthetic-computer";
    const key = tape.user ? `${tape.user}/${tape.slug}.zip` : `${tape.slug}.zip`;
    const zipUrl = `https://${bucket}.sfo3.digitaloceanspaces.com/${key}`;
    
    return Response.redirect(zipUrl, 302);
    
  } catch (error) {
    console.error(`Error fetching tape by code:`, error);
    return new Response("Error fetching tape", { status: 500 });
  }
}

/**
 * Handle painting code requests like /media/paintings/ABC123
 * @param {string} code - Painting code
 * @returns {Response}
 */
async function handlePaintingCodeRequest(code) {
  const host = Deno.env.get("CONTEXT") === "dev" 
    ? "https://localhost:8888" 
    : "https://aesthetic.computer";
  
  try {
    // Query painting by code
    const res = await fetch(`${host}/.netlify/functions/get-painting?code=${encodeURIComponent(code)}`);
    
    if (!res.ok) {
      return new Response(`Painting not found: ${code}`, { status: 404 });
    }
    
    const painting = await res.json();
    
    // Redirect to the PNG file
    const bucket = painting.bucket || "user-aesthetic-computer";
    const key = painting.user ? `${painting.user}/${painting.slug}.png` : `${painting.slug}.png`;
    const pngUrl = `https://${bucket}.sfo3.digitaloceanspaces.com/${key}`;
    
    return Response.redirect(pngUrl, 302);
    
  } catch (error) {
    console.error(`Error fetching painting by code:`, error);
    return new Response("Error fetching painting", { status: 500 });
  }
}

/**
 * Handle tape MP4 requests with conversion status checking
 * @param {string} resourcePath - Path like "userId/tape-slug.mp4"
 * @returns {Response}
 */
async function handleTapeMp4Request(resourcePath) {
  // Extract slug from path (remove .mp4 extension)
  const pathParts = resourcePath.split("/");
  const filename = pathParts[pathParts.length - 1];
  const slug = filename.replace(/\.mp4$/, "");
  
  // Query MongoDB for tape status
  const host = Deno.env.get("CONTEXT") === "dev" 
    ? "https://localhost:8888" 
    : "https://aesthetic.computer";
  
  try {
    const res = await fetch(`${host}/.netlify/functions/get-tape-status?slug=${encodeURIComponent(slug)}`);
    
    if (!res.ok) {
      return new Response("Tape not found", { status: 404 });
    }
    
    const tape = await res.json();
    
    // Check MP4 conversion status
    if (tape.mp4Status === "complete" && tape.mp4) {
      // MP4 is ready - redirect to actual file
      return Response.redirect(tape.mp4, 302);
    } else if (tape.mp4Status === "processing") {
      // MP4 is still processing - return JSON status
      const acceptHeader = Deno.env.get("HTTP_ACCEPT") || "";
      
      if (acceptHeader.includes("application/json")) {
        return new Response(JSON.stringify({
          status: "processing",
          message: "MP4 conversion in progress",
          slug: tape.slug,
          code: tape.code,
        }), {
          status: 202,
          headers: {
            "Content-Type": "application/json",
            "Access-Control-Allow-Origin": "*",
          },
        });
      } else {
        // Return HTML for browser requests
        return new Response(`
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Converting Tape...</title>
  <style>
    body {
      font-family: system-ui, -apple-system, sans-serif;
      display: flex;
      align-items: center;
      justify-content: center;
      min-height: 100vh;
      margin: 0;
      background: #0a0a0a;
      color: #fff;
    }
    .container {
      text-align: center;
      padding: 2rem;
    }
    .spinner {
      width: 50px;
      height: 50px;
      border: 4px solid rgba(255,255,255,0.1);
      border-top-color: #fff;
      border-radius: 50%;
      animation: spin 1s linear infinite;
      margin: 0 auto 1rem;
    }
    @keyframes spin {
      to { transform: rotate(360deg); }
    }
    h1 { margin: 0 0 0.5rem; font-size: 1.5rem; }
    p { margin: 0; opacity: 0.7; }
    code { 
      background: rgba(255,255,255,0.1);
      padding: 0.2rem 0.5rem;
      border-radius: 4px;
      font-family: 'Monaco', monospace;
    }
  </style>
  <script>
    // Auto-refresh every 5 seconds
    setTimeout(() => location.reload(), 5000);
  </script>
</head>
<body>
  <div class="container">
    <div class="spinner"></div>
    <h1>🎬 Converting Tape to MP4</h1>
    <p>Tape <code>${tape.code}</code> is being processed...</p>
    <p style="margin-top: 1rem; font-size: 0.9rem;">This page will auto-refresh.</p>
  </div>
</body>
</html>
        `, {
          status: 202,
          headers: {
            "Content-Type": "text/html; charset=utf-8",
            "Access-Control-Allow-Origin": "*",
            "Refresh": "5", // Auto-refresh header as backup
          },
        });
      }
    } else {
      // MP4 not started yet - return pending status
      return new Response(JSON.stringify({
        status: "pending",
        message: "MP4 conversion not started",
        slug: tape.slug,
        code: tape.code,
      }), {
        status: 202,
        headers: {
          "Content-Type": "application/json",
          "Access-Control-Allow-Origin": "*",
        },
      });
    }
  } catch (error) {
    console.error(`Error checking tape status:`, error);
    return new Response("Error checking tape status", { status: 500 });
  }
}
