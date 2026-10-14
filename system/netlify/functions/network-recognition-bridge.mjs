import { RECOGNITION_PARENTS } from "../../public/aesthetic.computer/lib/network-recognition-model.mjs";

export async function handler(event) {
  const headers = {
    "Content-Type": "text/html; charset=utf-8", "Cache-Control": "no-store",
    "Referrer-Policy": "no-referrer", "X-Content-Type-Options": "nosniff",
    "Content-Security-Policy": `default-src 'none'; script-src 'self'; connect-src 'self' https://hi.aesthetic.computer; frame-src https://hi.aesthetic.computer; base-uri 'none'; form-action 'none'; frame-ancestors ${RECOGNITION_PARENTS.join(" ")}`,
  };
  if (event.httpMethod !== "GET") return { statusCode: 405, headers, body: "" };
  return { statusCode: 200, headers, body: `<!doctype html><html lang="en"><meta charset="utf-8"><title>AC session measurement</title><script type="module" src="/aesthetic.computer/lib/network-recognition-bridge.mjs"></script></html>` };
}
