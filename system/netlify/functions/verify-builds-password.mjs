// Verify builds password for builds.false.work
import { respond } from "../../backend/http.mjs";

const BUILDS_PASSWORD = process.env.BUILDS_PASSWORD;

export async function handler(event, context) {
  if (event.httpMethod === "OPTIONS") {
    return respond(204, "");
  }

  if (event.httpMethod !== "POST") {
    return respond(405, { success: false });
  }

  try {
    const { password } = JSON.parse(event.body);

    if (!BUILDS_PASSWORD) {
      console.error("BUILDS_PASSWORD env var not set");
      return respond(500, { success: false, error: "Server misconfigured" });
    }

    return respond(200, { success: password === BUILDS_PASSWORD });
  } catch (error) {
    return respond(400, { success: false });
  }
}
