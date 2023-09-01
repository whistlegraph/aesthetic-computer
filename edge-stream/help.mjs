
export function corsHeaders(request) {
  const dev = process.env.VERCEL_ENV === "development";
  const production = !dev;
  const allowedOrigin = production ? "https://aesthetic.computer" : "*";

  return {
    "Access-Control-Allow-Methods": "GET,OPTIONS,PATCH,DELETE,POST,PUT",
    "Access-Control-Allow-Origin": allowedOrigin,
    "Access-Control-Allow-Headers": "X-CSRF-Token, X-Requested-With, Accept, Accept-Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version",
  }; // Define CORS headers.
}