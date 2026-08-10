// oskiewar-country, 26.08.09
// Returns only the request's trusted two-letter country for local flag UI.

import { respond } from "../../backend/http.mjs";
import { countryFromHeaders } from "../../backend/oskiewar-country.mjs";

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "", {
    "Cache-Control": "private, no-store",
  });
  if (event.httpMethod !== "GET") return respond(405,
    { error: "Method not allowed" }, { "Cache-Control": "private, no-store" });
  return respond(200, { country: countryFromHeaders(event.headers) }, {
    "Cache-Control": "private, no-store",
    Vary: "CF-IPCountry, X-Nf-Country",
  });
}

