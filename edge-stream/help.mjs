export function corsHeaders(request) {
  let origin;
  if (typeof request.headers.get === "function") {
    origin = request.headers.get("Origin"); // For edge functions..
  } else {
    origin = request.headers.host; // For serverless functions.
  }

  const production = origin === "https://aesthetic.computer";
  const allowedOrigin = production ? "https://aesthetic.computer" : "*";

  return {
    "Access-Control-Allow-Origin": allowedOrigin,
    "Access-Control-Allow-Headers": "Content-Type",
  }; // Define CORS headers.
}