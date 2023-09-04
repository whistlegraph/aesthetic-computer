// TODO: - [] Update `say` with this function also...

export function corsHeaders(request) {
  const dev = process.env.VERCEL_ENV === "development";
  const production = !dev;

  const allowedOrigins = ["https://aesthetic.computer", "https://botce.ac"];

  // If in production, check the request's origin against allowedOrigins.
  // If there's a match, set that as the Access-Control-Allow-Origin value.
  // If in development, simply use the wildcard.
  let originToSet;
  if (production) {
    if (request.headers && allowedOrigins.includes(request.headers.origin)) {
      originToSet = request.headers.origin;
    } else {
      originToSet = allowedOrigins[0]; // Default to the first one if no match
    }
  } else {
    originToSet = "*";
  }

  return {
    "Access-Control-Allow-Methods": "GET,OPTIONS,PATCH,DELETE,POST,PUT",
    "Access-Control-Allow-Origin": originToSet,
    "Access-Control-Allow-Headers":
      "X-CSRF-Token, X-Requested-With, Accept, Accept-Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version",
  };
}
