// The prompt's authored and Aesel-generated KidLisp use the same save contract.
// Browsing an existing $code does not call this and never claims authorship.
export async function savePromptKidlisp(api, source, { code = null, request = fetch } = {}) {
  const handle = api.handle?.();
  if (!handle || api.store?.["kidlisp:cache-enabled"] === false) return { code, saved: false };
  try {
    const token = await api.authorize();
    if (!token || api.handle?.() !== handle) throw new Error("Sign-in changed");
    const response = await request("/api/store-kidlisp", {
      method: "POST",
      signal: AbortSignal.timeout(10000),
      headers: { "Content-Type": "application/json", Authorization: `Bearer ${token}` },
      body: JSON.stringify({ source, handled: true }),
    });
    const result = await response.json();
    if (!response.ok || result.handled !== true || result.handle !== handle || !/^[a-z0-9]{3,64}$/i.test(result.code || ""))
      throw new Error("Handled save was not confirmed");
    if (api.handle?.() !== handle) return { code, saved: false, changed: true };
    return { code: result.code, saved: true, handle };
  } catch {
    api.notice?.("Could not save under your handle; running without that save.", ["yellow"]);
    return { code, saved: false, failed: true };
  }
}
