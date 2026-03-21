// os-image — Proxy to oven for personalized FedAC OS image downloads.
// Oven handles the heavy lifting (42MB template download, config patching, streaming).
// This edge function just forwards the auth header and streams the response.

const OVEN_URL = "https://oven.aesthetic.computer/os-image";

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
    const ovenRes = await fetch(OVEN_URL, {
      headers: { Authorization: authHeader },
    });

    // Pass through oven's response (including errors)
    return new Response(ovenRes.body, {
      status: ovenRes.status,
      headers: {
        "Content-Type": ovenRes.headers.get("content-type") || "application/octet-stream",
        "Content-Disposition": ovenRes.headers.get("content-disposition") || "",
        "Content-Length": ovenRes.headers.get("content-length") || "",
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
