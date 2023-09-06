// Logo, 23.05.02.22.09
// Proxy a random logo from one endpoint.

import { respond } from "../../backend/http.mjs";
import { logoUrl } from "../../backend/logo.mjs";

// GET A user's `sub` id from either their handle or email address.
export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  } else {
    const { got } = await import("got");
    const response = await got(logoUrl(), {
      responseType: "buffer",
      https: { rejectUnauthorized: false },
    });

    return {
      statusCode: 200,
      headers: {
        "Content-Type": "image/png", // Adjust this if your image type changes
      },
      body: Buffer.from(response.body, "binary").toString("base64"),
      isBase64Encoded: true,
    };
  }
}
