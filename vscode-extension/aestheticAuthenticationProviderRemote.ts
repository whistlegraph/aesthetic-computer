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
  UIKind,
  Uri,
  UriHandler,
  window,
} from "vscode";
import { PromiseAdapter, promiseFromEvent } from "./util";
// import fetch from 'node-fetch';
import * as crypto from "crypto";

export const AUTH_TYPE = `aesthetic`;
const AUTH_NAME = `Aesthetic Computer`;
const CLIENT_ID = `LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt`;
const AUTH0_DOMAIN = `hi.aesthetic.computer`;
const SESSIONS_SECRET_KEY = `${AUTH_TYPE}.sessions`;
let REDIRECT_URL : string;

let remoteOutput = window.createOutputChannel("aesthetic");

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
  private _uriHandler = new UriEventHandler();

  constructor(
    private readonly context: ExtensionContext,
    local: boolean,
  ) {
    REDIRECT_URL = `https://${
      local ? "localhost:8888" : "aesthetic.computer"
    }/redirect-proxy`;

    this._disposable = Disposable.from(
      authentication.registerAuthenticationProvider(
        AUTH_TYPE,
        AUTH_NAME,
        this,
        { supportsMultipleAccounts: false },
      ),
      window.registerUriHandler(this._uriHandler),
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
      const allSessions = await this.context.secrets.get(SESSIONS_SECRET_KEY);
      if (!allSessions) {
        return [];
      }

      // Get all required scopes
      const allScopes = this.getScopes(scopes || []) as string[];

      const sessions = JSON.parse(
        allSessions,
      ) as AestheticAuthenticationSession[];
      if (sessions) {
        if (allScopes && scopes) {
          const session = sessions.find((s) =>
            scopes.every((scope) => s.scopes.includes(scope)),
          );
          if (session && session.refreshToken) {
            const refreshToken = session.refreshToken;
            const { access_token } = await this.getAccessToken(
              refreshToken,
              CLIENT_ID,
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
          return sessions;
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
        throw new Error(`Aesthetic Computer login failure`);
      }

      const userinfo: { name: string; email: string } =
        await this.getUserInfo(access_token);

      const session: AestheticAuthenticationSession = {
        id: dateid(),
        accessToken: access_token,
        refreshToken: refresh_token,
        account: {
          label: userinfo.name,
          id: userinfo.email,
        },
        scopes: this.getScopes(scopes),
      };

      await this.context.secrets.store(
        SESSIONS_SECRET_KEY,
        JSON.stringify([session]),
      );

      this._sessionChangeEmitter.fire({
        added: [session],
        removed: [],
        changed: [],
      });

      return session;
    } catch (e) {
      window.showErrorMessage(`Sign in failed: ${e}`);
      throw e;
    }
  }

  /**
   * Remove an existing session
   * @param sessionId
   */
  public async removeSession(sessionId: string): Promise<void> {
    const allSessions = await this.context.secrets.get(SESSIONS_SECRET_KEY);
    if (allSessions) {
      let sessions = JSON.parse(allSessions) as AuthenticationSession[];
      const sessionIdx = sessions.findIndex((s) => s.id === sessionId);
      const session = sessions[sessionIdx];
      sessions.splice(sessionIdx, 1);

      await this.context.secrets.store(
        SESSIONS_SECRET_KEY,
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
    return await window.withProgress<TokenInformation>(
      {
        location: ProgressLocation.Notification,
        title: "Signing in to Aesthetic Computer...",
        cancellable: true,
      },
      async (_, token) => {
        const nonceId = dateid();

        const scopeString = scopes.join(" ");

        // Retrieve all required scopes
        scopes = this.getScopes(scopes);

        const codeVerifier = toBase64UrlEncoding(crypto.randomBytes(32));
        const codeChallenge = toBase64UrlEncoding(sha256(codeVerifier));

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
          ["client_id", CLIENT_ID],
          ["redirect_uri", REDIRECT_URL],
          ["state", encodeURIComponent(callbackUri.toString(true))],
          ["scope", scopes.join(" ")],
          ["prompt", "login"],
          ["code_challenge_method", "S256"],
          ["code_challenge", codeChallenge],
        ]);
        const uri = Uri.parse(
          `https://${AUTH0_DOMAIN}/authorize?${searchParams.toString()}`,
        );

        remoteOutput.appendLine(`Login URI: ${uri.toString(true)}`);

        await env.openExternal(uri);

        let codeExchangePromise = this._codeExchangePromises.get(scopeString);
        if (!codeExchangePromise) {
          codeExchangePromise = promiseFromEvent(
            this._uriHandler.event,
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
        client_id: CLIENT_ID,
        code,
        code_verifier: codeVerifier,
        redirect_uri: REDIRECT_URL,
      }).toString();

      const response = await fetch(`https://${AUTH0_DOMAIN}/oauth/token`, {
        method: "POST",
        headers: {
          "Content-Type": "application/x-www-form-urlencoded",
          "Content-Length": postData.length.toString(),
        },
        body: postData,
      });

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
    const response = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
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

    const response = await fetch(`https://${AUTH0_DOMAIN}/oauth/token`, {
      method: "POST",
      headers: {
        "Content-Type": "application/x-www-form-urlencoded",
        "Content-Length": postData.length.toString(),
      },
      body: postData,
    });

    const { access_token } = await response.json();

    return { access_token, refresh_token: "" };
  }
}

export function toBase64UrlEncoding(buffer: Buffer) {
  return buffer
    .toString("base64")
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=/g, "");
}

export function sha256(buffer: string | Uint8Array): Buffer {
  return crypto.createHash("sha256").update(buffer).digest();
}

// 📚 Library
function dateid() {
  return new Date().toString();
}
