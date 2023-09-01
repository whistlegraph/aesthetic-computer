export function corsHeaders(request) {
  let origin;
  if (typeof request.headers.get === "function") {
    origin = request.headers.get("Origin"); // For edge functions..
  } else {
    origin = "https://" + request.headers.host; // For serverless functions.
  }

  const production = origin === "https://aesthetic.computer";
  const allowedOrigin = production ? "https://aesthetic.computer" : "*";

  return {
    "Access-Control-Allow-Methods": "GET,OPTIONS,PATCH,DELETE,POST,PUT",
    "Access-Control-Allow-Origin": allowedOrigin,
    "Access-Control-Allow-Headers": "X-CSRF-Token, X-Requested-With, Accept, Accept-Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version",
  }; // Define CORS headers.
}