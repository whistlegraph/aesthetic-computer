// os-image — Personalized FedAC OS image download
// GET /api/os-image
// Authorization: Bearer <token>
//
// Downloads the template .img from DO Spaces, patches config.json
// with the authenticated user's handle/sub/email, and streams it back.
// The template has a 4096-byte padded config.json at a known location.

const RELEASES_BASE =
  "https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com/os";
const TEMPLATE_URL = `${RELEASES_BASE}/native-notepat-latest.img`;
const AUTH0_DOMAIN = "hi.aesthetic.computer";

// Config placeholder is padded to 4096 bytes with spaces.
// We search for it in the FAT32 image and overwrite in-place.
const CONFIG_MARKER = '{"handle":"","piece":"notepat","sub":"","email":""}';
const CONFIG_PAD_SIZE = 4096;

export default async (req) => {
  if (req.method === "OPTIONS") {
    return new Response("ok", {
      headers: {
        "Access-Control-Allow-Origin": "*",
        "Access-Control-Allow-Headers": "Authorization",
        "Access-Control-Allow-Methods": "GET, OPTIONS",
      },
    });
  }

  if (req.method !== "GET") {
    return new Response("GET only", { status: 405 });
  }

  // Authenticate via Auth0 token
  const authHeader = req.headers.get("authorization") || "";
  const token = authHeader.replace(/^Bearer\s+/i, "").trim();
  if (!token) {
    return Response.json(
      { error: "Authorization required. Log in at aesthetic.computer first." },
      { status: 401 },
    );
  }

  let userInfo;
  try {
    const res = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
      headers: { Authorization: `Bearer ${token}` },
    });
    if (!res.ok) throw new Error(`Auth failed: ${res.status}`);
    userInfo = await res.json();
  } catch (err) {
    return Response.json(
      { error: `Authentication failed: ${err.message}` },
      { status: 401 },
    );
  }

  // Look up handle via /user endpoint (try .netlify/functions/ path for internal call)
  let handle = "";
  const email = userInfo.email || "";
  const lookupUrls = [
    `https://aesthetic.computer/.netlify/functions/user?from=${encodeURIComponent(email)}&withHandle=true`,
    `https://aesthetic.computer/user?from=${encodeURIComponent(email)}&withHandle=true`,
  ];
  for (const url of lookupUrls) {
    if (handle) break;
    try {
      const handleRes = await fetch(url, {
        headers: { Accept: "application/json" },
      });
      if (handleRes.ok) {
        const text = await handleRes.text();
        if (text.startsWith("{")) {
          const data = JSON.parse(text);
          handle = data.handle || "";
        }
      }
    } catch (_) {}
  }

  if (!handle) {
    return Response.json(
      {
        error:
          "You need a handle first. Visit aesthetic.computer/handle to claim one.",
      },
      { status: 403 },
    );
  }

  // Build personalized config (padded to CONFIG_PAD_SIZE)
  const config = JSON.stringify({
    handle,
    piece: "notepat",
    sub: userInfo.sub || "",
    email: userInfo.email || "",
  });
  const padded = config + " ".repeat(Math.max(0, CONFIG_PAD_SIZE - config.length));
  const configBytes = new TextEncoder().encode(padded);

  // Download template image
  let imgData;
  try {
    const res = await fetch(TEMPLATE_URL);
    if (!res.ok) throw new Error(`Template download failed: ${res.status}`);
    imgData = new Uint8Array(await res.arrayBuffer());
  } catch (err) {
    return Response.json(
      { error: `Template not available: ${err.message}` },
      { status: 503 },
    );
  }

  // Find the config placeholder in the image and patch it
  const markerBytes = new TextEncoder().encode(CONFIG_MARKER);
  let found = false;
  for (let i = 0; i < imgData.length - markerBytes.length; i++) {
    let match = true;
    for (let j = 0; j < markerBytes.length; j++) {
      if (imgData[i + j] !== markerBytes[j]) {
        match = false;
        break;
      }
    }
    if (match) {
      // Overwrite the 4096-byte config region
      for (let j = 0; j < configBytes.length && i + j < imgData.length; j++) {
        imgData[i + j] = configBytes[j];
      }
      found = true;
      break;
    }
  }

  if (!found) {
    return Response.json(
      { error: "Template image missing config placeholder" },
      { status: 500 },
    );
  }

  // Stream the patched image back
  return new Response(imgData, {
    status: 200,
    headers: {
      "Content-Type": "application/octet-stream",
      "Content-Disposition": `attachment; filename="ac-native-${handle}.img"`,
      "Content-Length": String(imgData.length),
      "Access-Control-Allow-Origin": "*",
    },
  });
};

export const config = { path: "/api/os-image" };
