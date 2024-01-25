const {
  authentication,
  env,
  EventEmitter,
  ProgressLocation,
  Disposable,
  Uri,
  window,
} = require("vscode");

const { v4: uuid } = require("uuid");

const AUTH_TYPE = `aesthetic`;
const AUTH_NAME = `Aesthetic Computer`;
const CLIENT_ID = `LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt`;
const AUTH0_DOMAIN = `hi.aesthetic.computer`;
const SESSIONS_SECRET_KEY = `${AUTH_TYPE}.sessions`;

class UriEventHandler extends EventEmitter {
  handleUri(uri) {
    this.fire(uri);
  }
}

class AestheticAuthenticationProvider {
  constructor(context) {
    this.context = context;
    this._sessionChangeEmitter = new EventEmitter();
    this._disposable = Disposable.from(
      authentication.registerAuthenticationProvider(
        AUTH_TYPE,
        AUTH_NAME,
        this,
        { supportsMultipleAccounts: false },
      ),
      window.registerUriHandler(this._uriHandler),
    );

    this._pendingStates = [];
    this._codeExchangePromises = new Map();
    this._uriHandler = new UriEventHandler();
    console.log("URI Handler:", this._uriHandler);
  }

  get onDidChangeSessions() {
    return this._sessionChangeEmitter.event;
  }

  get redirectUri() {
    const publisher = this.context.extension.packageJSON.publisher;
    const name = this.context.extension.packageJSON.name;
    return `${env.uriScheme}://${publisher}.${name}`;
  }

  async getSessions(scopes) {
    const allSessions = await this.context.secrets.get(SESSIONS_SECRET_KEY);

    if (allSessions) {
      return JSON.parse(allSessions);
    }

    return [];
  }

  async createSession(scopes) {
    try {
      const token = await this.login(scopes);
      if (!token) {
        throw new Error(`Aesthetic login failure`);
      }

      const userinfo = await this.getUserInfo(token);

      const session = {
        id: uuid(),
        accessToken: token,
        account: {
          label: userinfo.name,
          id: userinfo.email,
        },
        scopes: [],
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

  async removeSession(sessionId) {
    const allSessions = await this.context.secrets.get(SESSIONS_SECRET_KEY);
    if (allSessions) {
      let sessions = JSON.parse(allSessions);
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

  async dispose() {
    this._disposable.dispose();
  }

  async login(scopes = []) {
    return await window.withProgress(
      {
        location: ProgressLocation.Notification,
        title: "Signing in to Aesthetic Computer...",
        cancellable: true,
      },
      async (_, token) => {
        const stateId = uuid();

        this._pendingStates.push(stateId);

        if (!scopes.includes("openid")) {
          scopes.push("openid");
        }
        if (!scopes.includes("profile")) {
          scopes.push("profile");
        }
        if (!scopes.includes("email")) {
          scopes.push("email");
        }

        const searchParams = new URLSearchParams([
          ["response_type", "token"],
          ["client_id", CLIENT_ID],
          ["redirect_uri", this.redirectUri],
          ["state", stateId],
          ["scope", scopes.join(" ")],
          ["prompt", "login"],
        ]);
        const uri = Uri.parse(
          `https://${AUTH0_DOMAIN}/authorize?${searchParams.toString()}`,
        );
        await env.openExternal(uri);

        let codeExchangePromise = this._codeExchangePromises.get(
          scopes.join(" "),
        );
        if (!codeExchangePromise) {
          codeExchangePromise = promiseFromEvent(
            this._uriHandler.event,
            this.handleUri(scopes),
          );
          this._codeExchangePromises.set(scopes.join(" "), codeExchangePromise);
          console.log("Code exchange promise:", codeExchangePromise, this._uriHandler.event);
        }

        try {
          return await Promise.race([
            codeExchangePromise.promise,
            new Promise((_, reject) =>
              setTimeout(() => reject("Ran out of time."), 60000),
            ),
            promiseFromEvent(token.onCancellationRequested, (_, __, reject) => {
              reject("Cancelled.");
            }).promise,
          ]);
        } finally {
          this._pendingStates = this._pendingStates.filter(
            (n) => n !== stateId,
          );
          codeExchangePromise.cancel.fire();
          this._codeExchangePromises.delete(scopes.join(" "));
        }
      },
    );
  }

  handleUri = (scopes) => async (uri, resolve, reject) => {
    console.log("Handling URI:", uri);

    const query = new URLSearchParams(uri.fragment);
    const access_token = query.get("access_token");
    const state = query.get("state");

    if (!access_token) {
      reject(new Error("No token"));
      return;
    }
    if (!state) {
      reject(new Error("No state"));
      return;
    }

    if (!this._pendingStates.some((n) => n === state)) {
      reject(new Error("State not found"));
      return;
    }

    resolve(access_token);
  };

  async getUserInfo(token) {
    const response = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });
    return await response.json();
  }
}

function promiseFromEvent(event, adapter = passthrough) {
  let subscription;
  let cancel = new EventEmitter();

  console.log("Event and adapter:", event, adapter)

  return {
    promise: new Promise((resolve, reject) => {
      cancel.event((_) => reject("Cancelled"));
      subscription = event((value) => {
        try {
          Promise.resolve(adapter(value, resolve, reject)).catch(reject);
        } catch (error) {
          reject(error);
        }
      });
    }).then(
      (result) => {
        subscription.dispose();
        return result;
      },
      (error) => {
        subscription.dispose();
        throw error;
      },
    ),
    cancel,
  };
}

module.exports = { AestheticAuthenticationProvider };
