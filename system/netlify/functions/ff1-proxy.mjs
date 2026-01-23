// FF1 Proxy (Netlify Function)
// Relays FF1 displayPlaylist requests to Feral File cloud relay.
// 
// NOTE: The Feral File relay (artwork-info.feral-file.workers.dev) may be 
// temporarily unavailable. This function provides two modes:
// 1. Cloud relay mode (requires topicID + apiKey from FF1 app)
// 2. Direct mode (requires deviceIp - FF1 must be accessible from server)

const RELAY_BASE = "https://artwork-info.feral-file.workers.dev/api/cast";

function corsHeaders() {
  return {
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Methods": "POST, OPTIONS",
    "Access-Control-Allow-Headers": "Content-Type",
  };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return {
      statusCode: 204,
      headers: corsHeaders(),
      body: "",
    };
  }

  if (event.httpMethod !== "POST") {
    return {
      statusCode: 405,
      headers: corsHeaders(),
      body: JSON.stringify({ success: false, error: "Method not allowed" }),
    };
  }

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch (err) {
    return {
      statusCode: 400,
      headers: corsHeaders(),
      body: JSON.stringify({ success: false, error: "Invalid JSON body" }),
    };
  }

  const { topicID, apiKey, command, request, deviceIp } = body;

  // Mode 1: Direct device connection (if deviceIp provided)
  if (deviceIp) {
    const directUrl = `http://${deviceIp}:1111/api/cast`;
    const payload = {
      command: command || "displayPlaylist",
      request,
    };

    try {
      const response = await fetch(directUrl, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      });

      const responseText = await response.text();
      let responseData;
      try {
        responseData = JSON.parse(responseText);
      } catch {
        responseData = { raw: responseText };
      }

      if (!response.ok) {
        return {
          statusCode: response.status,
          headers: corsHeaders(),
          body: JSON.stringify({
            success: false,
            error: `FF1 direct error: ${response.status}`,
            details: responseData,
          }),
        };
      }

      return {
        statusCode: 200,
        headers: corsHeaders(),
        body: JSON.stringify({ success: true, response: responseData, mode: "direct" }),
      };
    } catch (err) {
      return {
        statusCode: 500,
        headers: corsHeaders(),
        body: JSON.stringify({ 
          success: false, 
          error: `Cannot reach FF1 at ${deviceIp}: ${err.message}`,
          hint: "FF1 must be accessible from the server (same network or tunneled)"
        }),
      };
    }
  }

  // Mode 2: Cloud relay (requires topicID)
  if (!topicID) {
    return {
      statusCode: 400,
      headers: corsHeaders(),
      body: JSON.stringify({
        success: false,
        error: "Missing topicID - get this from your FF1 app settings",
        hint: "Go to FF1 app > Settings > Developer to find your Topic ID",
      }),
    };
  }

  if (!apiKey) {
    return {
      statusCode: 400,
      headers: corsHeaders(),
      body: JSON.stringify({
        success: false,
        error: "Missing apiKey - the cloud relay requires an API key",
        hint: "Get your API key from the FF1 app or contact Feral File support",
      }),
    };
  }

  const relayUrl = `${RELAY_BASE}?topicID=${encodeURIComponent(topicID)}`;

  const payload = {
    command: command || "displayPlaylist",
    request,
  };

  try {
    const response = await fetch(relayUrl, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "API-KEY": apiKey,
      },
      body: JSON.stringify(payload),
    });

    const responseText = await response.text();
    let responseData;
    try {
      responseData = JSON.parse(responseText);
    } catch {
      responseData = { raw: responseText };
    }

    if (!response.ok) {
      // Check for Cloudflare worker errors (relay may be down)
      if (response.status === 404 || responseText.includes("error code: 1042")) {
        return {
          statusCode: 503,
          headers: corsHeaders(),
          body: JSON.stringify({
            success: false,
            error: "FF1 cloud relay appears to be unavailable",
            hint: "The Feral File relay service may be down. Try using direct mode with deviceIp, or use the local tunnel (ac-ff1 tunnel).",
            details: responseData,
          }),
        };
      }

      return {
        statusCode: response.status,
        headers: corsHeaders(),
        body: JSON.stringify({
          success: false,
          error: `FF1 relay error: ${response.status}`,
          details: responseData,
        }),
      };
    }

    return {
      statusCode: 200,
      headers: corsHeaders(),
      body: JSON.stringify({ success: true, response: responseData, mode: "relay" }),
    };
  } catch (err) {
    return {
      statusCode: 500,
      headers: corsHeaders(),
      body: JSON.stringify({ success: false, error: err.message }),
    };
  }
}
