export function corsHeaders(request) {
  const origin = request.headers.get("Origin");
  const production = origin === "https://aesthetic.computer";
  const allowedOrigin = production ? "https://aesthetic.computer" : "*";

  return {
    "Access-Control-Allow-Origin": allowedOrigin,
    "Access-Control-Allow-Headers": "Content-Type",
  }; // Define CORS headers.
}
