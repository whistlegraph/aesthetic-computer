/*
import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  const { got } = await import("got");

  const stripeJsUrl = "https://js.stripe.com/v3/";

  try {
    // Fetch the Stripe JS library using got
    const response = await got(stripeJsUrl);

    console.log(response.headers);

    return {
      statusCode: 200,
      headers: {
        "Content-Type": "application/javascript",
        ...response.headers, // Forward the headers from the Stripe response
      },
      body: response.body,
    };
  } catch (error) {
    // Handle any exceptions
    return {
      statusCode: 500,
      body: JSON.stringify({ error: error.message }),
    };
  }
}
*/