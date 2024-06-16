// Sotce Net, 24.06.13.06.38
// A paid diary network, 'handled' by Aesthetic Computer.

/* #region 🏁 TODO 

  - [🟢] auth0 based auth / registration
    - [] review existing ac auth implementation and necessary
         keys
    - [] Set up login pages / logo etc. 
  - [🟠] Set up email sending service via google apps?
    - [] not logged in route
    - [] logged in route
  - [] stripe paywall
    - [] bring in necessary keys
    - [] logged in but unpaid route
    - [] logged in and paid route
    - [] this should be subscription based
  - [] The handle system would be shared among ac users.
    - [] Perhaps the subs could be 'sotce' prefixed.
  - [] Store whether a user is subscribed with an expiration date.
  - [] 
  + Done
  - [x] Developmnt mode.
  - [x] Live reload function.
  - [x] Set up path->method routing in `sotce-net.js`.
#endregion */

// ♻️ Environment
// const AUTH0_CLIENT_ID = process.env.SOTCE_AUTH0_M2M_CLIENT_ID;
// const AUTH0_SECRET = process.env.SOTCE_AUTH0_M2M_SECRET;
const AUTH0_CLIENT_ID_SPA = "3SvAbUDFLIFZCc1lV7e4fAAGKWXwl2B0";
const AUTH0_DOMAIN = "https://hi.sotce.net";

// 📚 Libraries
// TODO: - [] Auth0 clientside.
//       - [] Auth0 clientside.

const dev = process.env.NETLIFY_DEV;

import { defaultTemplateStringProcessor as html } from "../../public/aesthetic.computer/lib/helpers.mjs";

export const handler = async (event, context) => {
  // console.log("Event:", event);
  // console.log("Context:", context);
  // console.log("Path:", event.path);

  // 🚙 Router
  const method = event.httpMethod.toLowerCase();
  let path = event.path;
  if (path.startsWith("/sotce-net"))
    path = path.replace("/sotce-net", "/").replace("//", "/");

  // 🏠 Home
  if (path === "/" && method === "get") {
    const body = html`
      <html>
        <head>
          <meta charset="utf-8" />
          ${dev ? reloadScript : ""}
          <script
            crossorigin="anonymous"
            src="/aesthetic.computer/dep/auth0-spa-js.production.js"
          ></script>
          <script>
            (async () => {
              const clientId = "${AUTH0_CLIENT_ID_SPA}";
              // const before = performance.now();

              const auth0Client = await window.auth0.createAuth0Client({
                domain: "${AUTH0_DOMAIN}",
                clientId,
                cacheLocation: "localstorage",
                useRefreshTokens: true,
                authorizationParams: { redirect_uri: window.location.origin },
              });

              if (
                location.search.includes("state=") &&
                (location.search.includes("code=") ||
                  location.search.includes("error="))
              ) {
                try {
                  console.log("🔐 Handling auth0 redirect...");
                  await auth0Client.handleRedirectCallback();
                } catch (e) {
                  console.error("🔐", e);
                }
                window.history.replaceState({}, document.title, "/");
              }

              const isAuthenticated = await auth0Client.isAuthenticated();
              console.log("Authenticated?", isAuthenticated);

              if (!isAuthenticated) {
                const opts = { prompt: "login" }; // Never skip the login screen.
                // if (mode === "signup") opts.screen_hint = mode;
                opts.screen_hint = "signup";
                // TODO: Can have an optional signup mode...
                auth0Client.loginWithRedirect({ authorizationParams: opts });
              }
            })();
          </script>
        </head>
        <body>
          <h1>${path} : ${method}</h1>
        </body>
      </html>
    `;

    return {
      body,
      statusCode: 200,
      headers: { "Content-Type": "text/html; charset=utf-8" },
    };
  }
};

const reloadScript = html`
  <script>
    // Live reloading in development.
    let reconnectInterval = false;
    function connect() {
      clearInterval(reconnectInterval);
      let ws;
      try {
        ws = new WebSocket("wss://localhost:8889");
      } catch {
        console.warn("🧦 Connection failed.");
        return;
      }

      ws.onopen = (e) => {
        console.log("🧦 Connected:", e);
      };

      ws.onmessage = (e) => {
        const msg = JSON.parse(e.data);
        if (msg.type === "reload" && msg.content.piece === "*refresh*") {
          console.log("🧦 ♻️ Reloading...");
          location.reload();
        }
      };

      ws.onclose = (e) => {
        console.log("🧦 🔴 Closed:", e);
        reconnectInterval = setInterval(connect, 1000);
      };
    }
    connect();
  </script>
`.trim();
