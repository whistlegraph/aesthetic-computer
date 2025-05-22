import { authorize } from "../../backend/authorization.mjs";

export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  const user = await authorize(event.headers, "aesthetic");

  return {
    statusCode: 200,
    body: JSON.stringify({ authorized: !!user }),
    headers: { "Content-Type": "application/json" },
  };
}
