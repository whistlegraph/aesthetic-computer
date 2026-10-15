import { qrcode } from "/aesthetic.computer/dep/@akamfoad/qr/qr.mjs";
const API = "/api/nopaint-signin";
export async function signInRequest(body, { signal, token } = {}) {
  const response = await fetch(API, {
    method: "POST",
    signal,
    cache: "no-store",
    headers: {
      "Content-Type": "application/json",
      ...(token ? { Authorization: `Bearer ${token}` } : {}),
    },
    body: JSON.stringify(body),
  });
  const data = await response.json();
  if (!response.ok)
    throw Object.assign(
      Error(data.error || data.message || "Sign-in could not finish."),
      { status: response.status },
    );
  return data;
}
export async function sendSignInLink(code, email) {
  email = email.trim();
  const parameters = await signInRequest({ action: "email", code, email });
  const response = await fetch(
    "https://hi.aesthetic.computer/passwordless/start",
    {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      signal: AbortSignal.timeout(15000),
      body: JSON.stringify({
        ...parameters,
        connection: "email",
        email,
        send: "link",
      }),
    },
  );
  if (!response.ok)
    throw Error(
      response.status === 429
        ? "Please wait before requesting another link."
        : "Your sign-in link could not be sent. Try signing in with AC.",
    );
}
export function drawSignInQR(canvas, url) {
  const qr = qrcode(url),
    count = qr.getModuleCount(),
    scale = 5,
    margin = 4;
  canvas.width = canvas.height = (count + margin * 2) * scale;
  const ctx = canvas.getContext("2d");
  ctx.fillStyle = "white";
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = "black";
  for (let y = 0; y < count; y++)
    for (let x = 0; x < count; x++)
      if (qr.isDark(y, x))
        ctx.fillRect((x + margin) * scale, (y + margin) * scale, scale, scale);
}
