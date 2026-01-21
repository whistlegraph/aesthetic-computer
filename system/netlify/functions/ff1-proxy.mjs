// FF1 Proxy (Netlify Function)
// Relays FF1 displayPlaylist requests to Feral File cloud relay.

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

  const { topicID, apiKey, command, request } = body;

  if (!topicID) {
    return {
      statusCode: 400,
      headers: corsHeaders(),
      body: JSON.stringify({
        success: false,
        error: "Missing topicID - get this from your FF1 app settings",
      }),
    };
  }

  const relayUrl = `${RELAY_BASE}?topicID=${encodeURIComponent(topicID)}`;

  const payload = {
    command: command || "displayPlaylist",
    request,
  };

  try {
    const headers = {
      "Content-Type": "application/json",
    };
    if (apiKey) {
      headers["API-KEY"] = apiKey;
    }

    const response = await fetch(relayUrl, {
      method: "POST",
      headers,
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
          error: `FF1 relay error: ${response.status}`,
          details: responseData,
        }),
      };
    }

    return {
      statusCode: 200,
      headers: corsHeaders(),
      body: JSON.stringify({ success: true, response: responseData }),
    };
  } catch (err) {
    return {
      statusCode: 500,
      headers: corsHeaders(),
      body: JSON.stringify({ success: false, error: err.message }),
    };
  }
}
