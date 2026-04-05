// os-image — Proxy to oven for personalized FedAC OS image downloads.
// Oven handles the heavy lifting (template download, config patching, streaming).
// This edge function just forwards the auth header and streams the response.

const OVEN_BASE = "https://oven.aesthetic.computer/os-image";

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

  const authHeader = req.headers.get("authorization") || "";
  if (!authHeader) {
    return Response.json(
      { error: "Authorization required. Log in at aesthetic.computer first." },
      { status: 401 },
    );
  }

  // Proxy to oven
  try {
    const ovenUrl = OVEN_BASE + new URL(req.url).search;
    const ovenRes = await fetch(ovenUrl, {
      headers: { Authorization: authHeader },
    });

    // Pass through oven's response (including errors)
    return new Response(ovenRes.body, {
      status: ovenRes.status,
      headers: {
        "Content-Type": ovenRes.headers.get("content-type") || "application/octet-stream",
        "Content-Disposition": ovenRes.headers.get("content-disposition") || "",
        "Content-Length": ovenRes.headers.get("content-length") || "",
        "X-AC-OS-Requested-Layout":
          ovenRes.headers.get("x-ac-os-requested-layout") || "",
        "X-AC-OS-Layout": ovenRes.headers.get("x-ac-os-layout") || "",
        "X-AC-OS-Fallback": ovenRes.headers.get("x-ac-os-fallback") || "",
        "X-AC-OS-Fallback-Reason":
          ovenRes.headers.get("x-ac-os-fallback-reason") || "",
        "Access-Control-Allow-Origin": "*",
      },
    });
  } catch (err) {
    return Response.json(
      { error: `Oven unavailable: ${err.message}` },
      { status: 502 },
    );
  }
};

export const config = { path: "/api/os-image" };
