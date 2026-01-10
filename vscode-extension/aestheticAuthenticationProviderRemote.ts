import {
  authentication,
  AuthenticationProvider,
  AuthenticationProviderAuthenticationSessionsChangeEvent,
  AuthenticationSession,
  Disposable,
  env,
  EventEmitter,
  ExtensionContext,
  ProgressLocation,
  Uri,
  UriHandler,
  window as win,
} from "vscode";
import { PromiseAdapter, promiseFromEvent } from "./util";

// Isomorphic use of web `crypto` api across desktop (node) and
// a web worker (vscode.dev).
let icrypto: any;
if (typeof self === "undefined") {
  icrypto = require("crypto").webcrypto;
} else {
  icrypto = crypto;
}

let remoteOutput = win.createOutputChannel("aesthetic");

interface TokenInformation {
  access_token: string;
  refresh_token: string;
}

interface AestheticAuthenticationSession extends AuthenticationSession {
  refreshToken: string;
}

class UriEventHandler extends EventEmitter<Uri> implements UriHandler {
  public handleUri(uri: Uri) {
    this.fire(uri);
  }
}

const uriHandler = new UriEventHandler();
win.registerUriHandler(uriHandler);

export class AestheticAuthenticationProvider
  implements AuthenticationProvider, Disposable
{
  private _sessionChangeEmitter =
    new EventEmitter<AuthenticationProviderAuthenticationSessionsChangeEvent>();
  private _disposable: Disposable;
  private _pendingStates: string[] = [];
  private _codeExchangePromises = new Map<
    string,
    { promise: Promise<TokenInformation>; cancel: EventEmitter<void> }
  >();
  private _codeVerfifiers = new Map<string, string>();
  private _scopes = new Map<string, string[]>();
  private env = {
    AUTH_TYPE: "",
    AUTH_NAME: "",
    CLIENT_ID: "",
    AUTH0_DOMAIN: "",
    REDIRECT_URL: "",
    SESSIONS_SECRET_KEY: "",
  };

  private static looksLikeEmail(value: string | undefined): boolean {
    if (!value) return false;
    // Rough heuristic: `@` + a dot in the domain part.
    const at = value.indexOf("@");
    if (at <= 0) return false;
    const domain = value.slice(at + 1);
    return domain.includes(".") && !value.startsWith("@");
  }

  private async getTenantHandleBySub(sub: string): Promise<string | undefined> {
    if (!sub) return undefined;

    // Fetch handle using the same method as chat.mjs
    const prefix = this.env.AUTH_TYPE === "sotce" ? "sotce-" : "";
    const host = this.env.AUTH_TYPE === "sotce" ? "https://sotce.net" : "https://aesthetic.computer";
    const url = `${host}/handle?for=${prefix}${sub}`;

    try {
      const response = await fetch(url);
      if (!response.ok) return undefined;
      const data = (await response.json()) as { handle?: string };
      return data.handle ? data.handle.trim() : undefined;
    } catch (error) {
      console.error("Failed to fetch handle:", error);
      return undefined;
    }
  }

  private async getTenantHandleByEmail(email: string): Promise<string | undefined> {
    if (!email) return undefined;

    // This is a Netlify function on the public site.
    const host = this.env.AUTH_TYPE === "sotce" ? "https://sotce.net" : "https://aesthetic.computer";
    const url = `${host}/user?from=${encodeURIComponent(email)}&withHandle=true&tenant=${encodeURIComponent(this.env.AUTH_TYPE)}`;

    const response = await fetch(url);
    if (!response.ok) return undefined;
    const data = (await response.json()) as { handle?: unknown };
    return typeof data.handle === "string" && data.handle.trim() ? data.handle.trim() : undefined;
  }

  private async resolveAccountLabel(accessToken: string): Promise<string | undefined> {
    const userinfo = (await this.getUserInfo(accessToken)) as {
      name?: string;
      email?: string;
      nickname?: string;
      handle?: string;
    };

    const directHandle = typeof userinfo.handle === "string" ? userinfo.handle : undefined;
    const email = typeof userinfo.email === "string" ? userinfo.email : undefined;

    const tenantHandle = email
      ? await this.getTenantHandleByEmail(email).catch(() => undefined)
      : undefined;

    const handle = tenantHandle || directHandle;
    if (handle) return handle.startsWith("@") ? handle : `@${handle}`;

    return userinfo.nickname || userinfo.name || userinfo.email;
  }

  constructor(
    private readonly context: ExtensionContext,
    local: boolean,
    tenant: string,
  ) {
    if (tenant === "aesthetic") {
      this.env.AUTH_TYPE = `aesthetic`;
      this.env.AUTH_NAME = `Aesthetic Computer`;
      this.env.CLIENT_ID = `LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt`;
      this.env.AUTH0_DOMAIN = `hi.aesthetic.computer`;
      this.env.REDIRECT_URL = `https://${
        local ? "localhost:8888" : "aesthetic.computer"
      }/redirect-proxy`;
    } else if (tenant === "sotce") {
      this.env.AUTH_TYPE = `sotce`;
      this.env.AUTH_NAME = `Sotce Net`;
      this.env.CLIENT_ID = `3SvAbUDFLIFZCc1lV7e4fAAGKWXwl2B0`;
      this.env.AUTH0_DOMAIN = `hi.sotce.net`;
      this.env.REDIRECT_URL = `https://${
        local ? "localhost:8888" : "sotce.net"
      }/redirect-proxy-sotce`;
    }

    this.env.SESSIONS_SECRET_KEY = `${this.env.AUTH_TYPE}.sessions`;

    // console.log("Authentication provider registering...", AUTH_TYPE, AUTH_NAME);

    this._disposable = Disposable.from(
      authentication.registerAuthenticationProvider(
        this.env.AUTH_TYPE,
        this.env.AUTH_NAME,
        this,
        { supportsMultipleAccounts: false },
      ),
    );
  }

  get onDidChangeSessions() {
    return this._sessionChangeEmitter.event;
  }

  get redirectUri() {
    const publisher = this.context.extension.packageJSON.publisher;
    const name = this.context.extension.packageJSON.name;

    let callbackUrl = `${env.uriScheme}://${publisher}.${name}`;
    return callbackUrl;
  }

  /**
   * Get the existing sessions
   * @param scopes
   * @returns
   */
  public async getSessions(
    scopes?: string[],
  ): Promise<readonly AestheticAuthenticationSession[]> {
    try {
      const allSessions = await this.context.secrets.get(
        this.env.SESSIONS_SECRET_KEY,
      );
      if (!allSessions) {
        return [];
      }

      // Get all required scopes
      const allScopes = this.getScopes(scopes || []) as string[];

      const sessions = JSON.parse(
        allSessions,
      ) as AestheticAuthenticationSession[];
      if (sessions) {
        // Best-effort migration: older sessions often used email as the label.
        // Upgrade to @handle without forcing a logout.
        const changedSessions: AestheticAuthenticationSession[] = [];
        const upgradedSessions = await Promise.all(
          sessions.map(async (session) => {
            const currentLabel = session.account?.label;
            if (!AestheticAuthenticationProvider.looksLikeEmail(currentLabel)) return session;

            try {
              let accessToken = session.accessToken;
              let label: string | undefined;

              try {
                label = await this.resolveAccountLabel(accessToken);
              } catch {
                // If the access token is stale, try to refresh and retry once.
                if (session.refreshToken) {
                  const refreshed = await this.getAccessToken(
                    session.refreshToken,
                    this.env.CLIENT_ID,
                  );
                  if (refreshed.access_token) {
                    accessToken = refreshed.access_token;
                    label = await this.resolveAccountLabel(accessToken);
                  }
                }
              }

              if (!label || label === currentLabel) return session;
              const updated = {
                ...session,
                accessToken,
                account: {
                  ...session.account,
                  label,
                },
              } as AestheticAuthenticationSession;
              changedSessions.push(updated);
              return updated;
            } catch {
              return session;
            }
          }),
        );

        if (changedSessions.length) {
          await this.context.secrets.store(
            this.env.SESSIONS_SECRET_KEY,
            JSON.stringify(upgradedSessions),
          );
          this._sessionChangeEmitter.fire({
            added: [],
            removed: [],
            changed: changedSessions,
          });
        }

        if (allScopes && scopes) {
          const session = upgradedSessions.find((s) =>
            scopes.every((scope) => s.scopes.includes(scope)),
          );
          if (session && session.refreshToken) {
            const refreshToken = session.refreshToken;
            const { access_token } = await this.getAccessToken(
              refreshToken,
              this.env.CLIENT_ID,
            );

            if (access_token) {
              const updatedSession = Object.assign({}, session, {
                accessToken: access_token,
                scopes: scopes,
              });
              return [updatedSession];
            } else {
              this.removeSession(session.id);
            }
          }
        } else {
          return upgradedSessions;
        }
      }
    } catch (e) {
      // Nothing to do
    }

    return [];
  }

  /**
   * Create a new auth session
   * @param scopes
   * @returns
   */
  public async createSession(
    scopes: string[],
  ): Promise<AestheticAuthenticationSession> {
    try {
      const { access_token, refresh_token } = await this.login(scopes);
      if (!access_token) {
        throw new Error(`${this.env.AUTH_NAME} login failure`);
      }

      const userinfo: { name: string; email: string; sub: string; nickname?: string; handle?: string } =
        await this.getUserInfo(access_token);

      const tenantHandle = userinfo.sub
        ? await this.getTenantHandleBySub(userinfo.sub).catch(() => undefined)
        : undefined;
      const handle = tenantHandle || userinfo.handle;
      const label =
        (handle ? (handle.startsWith("@") ? handle : `@${handle}`) : undefined) ||
        userinfo.nickname ||
        userinfo.name ||
        userinfo.email;

      const session: AestheticAuthenticationSession = {
        id: generateRandomString(12),
        accessToken: access_token,
        refreshToken: refresh_token,
        account: {
          label,
          id: userinfo.sub,
        },
        scopes: this.getScopes(scopes),
      };

      await this.context.secrets.store(
        this.env.SESSIONS_SECRET_KEY,
        JSON.stringify([session]),
      );

      this._sessionChangeEmitter.fire({
        added: [session],
        removed: [],
        changed: [],
      });

      return session;
    } catch (e) {
      win.showErrorMessage(`🔴 Log in failed: ${e}`);
      throw e;
    }
  }

  /**
   * Remove an existing session
   * @param sessionId
   */
  public async removeSession(sessionId: string): Promise<void> {
    const allSessions = await this.context.secrets.get(
      this.env.SESSIONS_SECRET_KEY,
    );
    if (allSessions) {
      let sessions = JSON.parse(allSessions) as AuthenticationSession[];
      const sessionIdx = sessions.findIndex((s) => s.id === sessionId);
      const session = sessions[sessionIdx];
      sessions.splice(sessionIdx, 1);

      await this.context.secrets.store(
        this.env.SESSIONS_SECRET_KEY,
        JSON.stringify(sessions),
      );

      if (session) {
        this._sessionChangeEmitter.fire({
          added: [],
          removed: [session],
          changed: [],
        });
      }
    }
  }

  /**
   * Dispose the registered services
   */
  public async dispose() {
    this._disposable.dispose();
  }

  /**
   * Log in to Aesthetic Computer
   */
  private async login(scopes: string[] = []): Promise<TokenInformation> {
    return await win.withProgress<TokenInformation>(
      {
        location: ProgressLocation.Notification,
        title: `🟡 Logging in to ${this.env.AUTH_NAME}...`,
        cancellable: true,
      },
      async (_, token) => {
        const nonceId = generateRandomString(12);

        const scopeString = scopes.join(" ");

        // Retrieve all required scopes
        scopes = this.getScopes(scopes);

        const codeVerifier = generateRandomString(32);
        const codeChallenge = await sha256(codeVerifier);

        let callbackUri = await env.asExternalUri(Uri.parse(this.redirectUri));

        remoteOutput.appendLine(`Callback URI: ${callbackUri.toString(true)}`);

        const callbackQuery = new URLSearchParams(callbackUri.query);
        const stateId = callbackQuery.get("state") || nonceId;

        remoteOutput.appendLine(`State ID: ${stateId}`);
        remoteOutput.appendLine(`Nonce ID: ${nonceId}`);

        callbackQuery.set("state", encodeURIComponent(stateId));
        callbackQuery.set("nonce", encodeURIComponent(nonceId));
        callbackUri = callbackUri.with({
          query: callbackQuery.toString(),
        });

        this._pendingStates.push(stateId);
        this._codeVerfifiers.set(stateId, codeVerifier);
        this._scopes.set(stateId, scopes);

        const searchParams = new URLSearchParams([
          ["response_type", "code"],
          ["client_id", this.env.CLIENT_ID],
          ["redirect_uri", this.env.REDIRECT_URL],
          ["state", encodeURIComponent(callbackUri.toString(true))],
          ["scope", scopes.join(" ")],
          ["prompt", "login"],
          ["code_challenge_method", "S256"],
          ["code_challenge", codeChallenge],
        ]);
        const uri = Uri.parse(
          `https://${
            this.env.AUTH0_DOMAIN
          }/authorize?${searchParams.toString()}`,
        );

        remoteOutput.appendLine(`Login URI: ${uri.toString(true)}`);

        await env.openExternal(uri);

        let codeExchangePromise = this._codeExchangePromises.get(scopeString);
        if (!codeExchangePromise) {
          codeExchangePromise = promiseFromEvent(
            uriHandler.event,
            this.handleUri(scopes),
          );
          this._codeExchangePromises.set(scopeString, codeExchangePromise);
        }

        try {
          return await Promise.race([
            codeExchangePromise.promise,
            new Promise<string>((_, reject) =>
              setTimeout(() => reject("Cancelled"), 60000),
            ),
            promiseFromEvent<any, any>(
              token.onCancellationRequested,
              (_, __, reject) => {
                reject("User Cancelled");
              },
            ).promise,
          ]);
        } finally {
          this._pendingStates = this._pendingStates.filter(
            (n) => n !== stateId,
          );
          codeExchangePromise?.cancel.fire();
          this._codeExchangePromises.delete(scopeString);
          this._codeVerfifiers.delete(stateId);
          this._scopes.delete(stateId);
        }
      },
    );
  }

  /**
   * Handle the redirect to VS Code (after sign in from Aesthetic Computer)
   * @param scopes
   * @returns
   */
  private handleUri: (
    scopes: readonly string[],
  ) => PromiseAdapter<Uri, TokenInformation> =
    (scopes) => async (uri, resolve, reject) => {
      const query = new URLSearchParams(uri.query);
      const code = query.get("code");
      const stateId = query.get("state");

      if (!code) {
        reject(new Error("No code"));
        return;
      }
      if (!stateId) {
        reject(new Error("No state"));
        return;
      }

      const codeVerifier = this._codeVerfifiers.get(stateId);
      if (!codeVerifier) {
        reject(new Error("No code verifier"));
        return;
      }

      // Check if it is a valid auth request started by the extension
      if (!this._pendingStates.some((n) => n === stateId)) {
        reject(new Error("State not found"));
        return;
      }

      const postData = new URLSearchParams({
        grant_type: "authorization_code",
        client_id: this.env.CLIENT_ID,
        code,
        code_verifier: codeVerifier,
        redirect_uri: this.env.REDIRECT_URL,
      }).toString();

      const response = await fetch(
        `https://${this.env.AUTH0_DOMAIN}/oauth/token`,
        {
          method: "POST",
          headers: {
            "Content-Type": "application/x-www-form-urlencoded",
            "Content-Length": postData.length.toString(),
          },
          body: postData,
        },
      );

      const { access_token, refresh_token } = await response.json();

      resolve({
        access_token,
        refresh_token,
      });
    };

  /**
   * Get the user info from Aesthetic Computer
   * @param token
   * @returns
   */
  private async getUserInfo(token: string) {
    const response = await fetch(`https://${this.env.AUTH0_DOMAIN}/userinfo`, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });
    return await response.json();
  }

  /**
   * Get all required scopes
   * @param scopes
   */
  private getScopes(scopes: string[] = []): string[] {
    let modifiedScopes = [...scopes];

    if (!modifiedScopes.includes("offline_access")) {
      modifiedScopes.push("offline_access");
    }
    if (!modifiedScopes.includes("openid")) {
      modifiedScopes.push("openid");
    }
    if (!modifiedScopes.includes("profile")) {
      modifiedScopes.push("profile");
    }
    if (!modifiedScopes.includes("email")) {
      modifiedScopes.push("email");
    }

    return modifiedScopes.sort();
  }

  /**
   * Retrieve a new access token by the refresh token
   * @param refreshToken
   * @param clientId
   * @returns
   */
  private async getAccessToken(
    refreshToken: string,
    clientId: string,
  ): Promise<TokenInformation> {
    const postData = new URLSearchParams({
      grant_type: "refresh_token",
      client_id: clientId,
      refresh_token: refreshToken,
    }).toString();

    const response = await fetch(
      `https://${this.env.AUTH0_DOMAIN}/oauth/token`,
      {
        method: "POST",
        headers: {
          "Content-Type": "application/x-www-form-urlencoded",
          "Content-Length": postData.length.toString(),
        },
        body: postData,
      },
    );

    const { access_token } = await response.json();

    return { access_token, refresh_token: "" };
  }
}

// 📚 Library

function generateRandomString(n: number): string {
  const buffer = new Uint8Array(n);
  icrypto.getRandomValues(buffer);
  return toBase64UrlEncoding(buffer);
}

export function toBase64UrlEncoding(buffer: Uint8Array): string {
  let base64String;

  if (typeof Buffer !== "undefined") {
    // Node.js environment
    base64String = Buffer.from(buffer).toString("base64");
  } else {
    // Browser environment
    const binaryString = Array.from(buffer)
      .map((byte) => {
        return String.fromCharCode(byte);
      })
      .join("");
    base64String = btoa(binaryString);
  }

  return base64String.replace(/\+/g, "-").replace(/\//g, "_").replace(/=/g, "");
}

export async function sha256(buffer: string | Uint8Array): Promise<string> {
  const data =
    typeof buffer === "string" ? new TextEncoder().encode(buffer) : buffer;
  const hashBuffer = await icrypto.subtle.digest("SHA-256", data);
  return toBase64UrlEncoding(new Uint8Array(hashBuffer));
}
