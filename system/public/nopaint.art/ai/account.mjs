import { otpSignIn } from "/aesthetic.computer/lib/auth0-otp.mjs";
export const CLIENT_ID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
export class Account {
  constructor(onchange = () => {}) {
    this.onchange = onchange;
    this.value = { connected: false };
    this.models = [];
    this.otp = otpSignIn({ clientId: CLIENT_ID });
    this.auth = null;
  }
  async sdk() {
    if (this.auth) return this.auth;
    if (!globalThis.auth0)
      await new Promise((resolve, reject) => {
        const script = document.createElement("script");
        script.src = "/aesthetic.computer/dep/auth0-spa-js.production.js";
        script.onload = resolve;
        script.onerror = () => reject(Error("Sign-in could not load."));
        document.head.append(script);
      });
    this.auth = new globalThis.auth0.Auth0Client({
      domain: "hi.aesthetic.computer",
      clientId: CLIENT_ID,
      cacheLocation: "localstorage",
      useRefreshTokens: true,
      authorizationParams: { redirect_uri: location.origin },
      httpTimeoutInSeconds: 30,
    });
    return this.auth;
  }
  async boot() {
    const params = new URLSearchParams(location.search);
    if (params.has("code") && params.has("state")) {
      try {
        await (await this.sdk()).handleRedirectCallback();
      } finally {
        history.replaceState(null, "", location.pathname);
      }
    }
    await this.refresh();
  }
  async token() {
    const otp = await this.otp.token();
    if (otp) return otp;
    const auth = await this.sdk();
    if (!(await auth.isAuthenticated())) return null;
    return auth.getTokenSilently();
  }
  async request(path, body, timeout = 20000) {
    const token = await this.token();
    if (!token) throw Object.assign(Error("Sign in with AC."), { status: 401 });
    const response = await fetch(path, {
      method: body ? "POST" : "GET",
      headers: {
        Authorization: `Bearer ${token}`,
        "Content-Type": "application/json",
      },
      ...(body ? { body: JSON.stringify(body) } : {}),
      signal: AbortSignal.timeout(timeout),
      cache: "no-store",
    });
    const result = await response.json();
    if (!response.ok)
      throw Object.assign(
        Error(
          result.error ||
            result.message ||
            `AC request failed (${response.status}).`,
        ),
        { status: response.status },
      );
    return result;
  }
  async refresh() {
    try {
      const response = await fetch("/api/nopaint-inference", {
        cache: "no-store",
        signal: AbortSignal.timeout(15000),
      });
      if (!response.ok) throw Error("AC cloud unavailable.");
      const catalog = await response.json();
      this.models = catalog.models || [];
      this.service = catalog.service;
    } catch (error) {
      this.service = { available: false, detail: error.message };
      this.models = this.models.map((m) => ({ ...m, available: false }));
    }
    try {
      if (!(await this.token())) this.value = { connected: false };
      else
        this.value = {
          ...(await this.request("/api/easel-credits")),
          connected: true,
          stale: false,
        };
    } catch (error) {
      if ([401, 403].includes(error.status))
        this.value = { connected: false, error: error.message };
      else
        this.value = {
          ...this.value,
          stale: true,
          error:
            "AC is temporarily unavailable. Showing the last known balance.",
        };
    }
    this.onchange();
    return this.value;
  }
  async login() {
    await (await this.sdk()).loginWithRedirect();
  }
  async logout() {
    this.otp.forget();
    await (await this.sdk()).logout({ openUrl: false });
    this.value = { connected: false };
    this.onchange();
  }
  async move(input) {
    return this.request("/api/nopaint-inference", input, 200000);
  }
}
