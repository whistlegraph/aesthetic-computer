// Secure phone-to-browser login helper for fight on TV/console browsers.
// The QR contains only the short public claim code. The private polling
// secret never leaves this browser except as a credential to the API.

const API = "/api/device-pair";

export async function createFightLogin() {
  const response = await fetch(API, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ action: "create", kind: "browser" }),
  });
  if (!response.ok) throw new Error(`login create failed (${response.status})`);
  const pair = await response.json();
  if (!pair.code || !pair.pollSecret) throw new Error("login create returned no credential");
  return {
    code: pair.code,
    pollSecret: pair.pollSecret,
    loginUrl: `${location.origin}/api/device-pair-login?code=${encodeURIComponent(pair.code)}&kind=browser`,
  };
}

export async function pollFightLogin(pair, { signal } = {}) {
  const query = new URLSearchParams({ code: pair.code, secret: pair.pollSecret });
  const response = await fetch(`${API}?${query}`, { cache: "no-store", signal });
  if (response.status === 410) return { status: "expired" };
  if (!response.ok) throw new Error(`login poll failed (${response.status})`);
  return response.json();
}

export function adoptFightSession(session) {
  if (!session?.accessToken || !session?.account) throw new Error("invalid browser session");
  const encoded = btoa(JSON.stringify(session));
  localStorage.setItem("session-aesthetic", encoded);
  location.reload();
}
