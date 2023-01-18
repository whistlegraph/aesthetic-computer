// Sotce Blog
// Allows sotce to give Patreon subscribers access to her blog (Tumblr).
// Patreon dev docs: https://www.patreon.com/portal

/* #region todo 📓 
 - [-] Create a session and hook it up to the Tumblr somehow?
+ Done
 - [x] Hook up to blog.sotce.com.
 - [x] Write the logic that detects whether a user is sotce or a subscriber. 
#endregion */

const dev = process.env.NETLIFY_DEV;
// const patreonAccessToken = process.env.SOTCE_PATREON_ACCESS_TOKEN;
// const patreonRefreshToken = process.env.SOTCE_PATREON_REFRESH_TOKEN;

import crypto from "crypto"; // Import crypto library.

async function fun(event, context) {
  const client = process.env.SOTCE_PATREON_CLIENT_ID;
  const secret = process.env.SOTCE_PATREON_CLIENT_SECRET;
  const vanity = "blog.sotce.com"; // "sotce.aesthetic.computer"
  const redirect = dev
    ? "https://localhost:8888/sotce-blog"
    : `https://${vanity}`;

  const storageItem = "hasSotceBlogAccess";
  // Both values above also must be set (space separated) in the
  // Patreon developer account's client redirect URI field.

  // 1. Return an entry page if it's a GET.
  if (event.httpMethod === "GET") {
    // If the code parameter is not present, return the HTML template with the authorization link
    const scope = encodeURIComponent("identity identity.memberships"); // Space separated... identity[email]
    //                                                           See also https://docs.patreon.com/#scopes
    const state = crypto.randomBytes(32).toString("hex"); // For securing the request.
    const loginUrl = `https://www.patreon.com/oauth2/authorize?response_type=code&client_id=${client}&redirect_uri=${redirect}&state=${state}&scope=${scope}`;

    const gateBody = `
      <html>
        <head>
          <script>
            function openPopup() {
              const popup = window.open("${loginUrl}", "Patreon Login", "width=400, height=650");

              // Center the popup in the current window.
              const parentWidth = window.innerWidth;
              const parentHeight = window.innerHeight;
              const parentLeft = window.screenX;
              const parentTop = window.screen.availTop;
              const screenHeight = window.screen.availHeight;
              const left = parentLeft + (parentWidth - 400) / 2;
              const top = parentTop + (screenHeight - 650) / 2;
              popup.moveTo(left, top);

              const hasSotceBlogAccess = localStorage.setItem("${storageItem}", false);
              window.addEventListener("storage", function (event) {
                if (event.key === "${storageItem}" && Boolean(event.newValue) === true) {
                  console.log("Access Granted!");
                  document.body.classList.remove("gate");
                }
              });
            }
          </script>
          <style>
            body.gate { background: white; }
            body:not(.gate) * { display: none; pointer-events: none; user-select: none; }
            body:not(.gate) { background: transparent; }
            button {
              font-family: Helvetica, Arial, sans-serif;
              border: none;
              background: none;
              font-size: 8vmin;
              cursor: pointer;
            }
            button:hover {
              font-size: 9vmin;
            }
            button:active {
              font-size: 8.5vmin;
            }
            body { display: flex; }
            #wrapper { margin: auto; }
          </style>
        </head>
        <body class="gate">
          <div id="wrapper">
            <button onclick="openPopup()">enter</button>
          </div>
        </body>
      </html>
    `;

    const code = event.queryStringParameters.code;

    // A. If there is no code yet, then return a gated button.
    if (!code) {

      // Show the normal gate screen or a closed window if we came from
      // the patreon pop-up and no code was yielded (denied permission).

      return {
        statusCode: 200,
        headers: {
          "Content-Type": "text/html",
        },
        body:
          event.headers.referer !== "https://www.patreon.com/"
            ? gateBody
            : "<script>window.close();</script>",
      };

      // B. If the code parameter is present, exchange it for an access token
    } else if (code) {
      const { got } = await import("got"); // Import the "got" http library.

      let hasBlogAccess = false;

      try {
        // 1. Get the user's Patreon access token.
        const tokenResponse = await got.post(
          "https://www.patreon.com/api/oauth2/token",
          {
            form: {
              code,
              client_id: client,
              client_secret: secret,
              redirect_uri: redirect,
              grant_type: "authorization_code",
            },
          }
        );

        const accessToken = JSON.parse(tokenResponse.body).access_token;

        // 2. Check patronage.
        // Read the docs here to build a query: https://docs.patreon.com/#get-api-oauth2-v2-identity
        // Note: Encode square [] brackets as %5B and %5D
        const identityResponse = await got(
          `https://www.patreon.com/api/oauth2/v2/identity?include=memberships.currently_entitled_tiers&fields%5Buser%5D=email,is_email_verified&fields%5Bmember%5D=currently_entitled_amount_cents,patron_status`,
          {
            headers: {
              Authorization: `Bearer ${accessToken}`,
              Accept: "application/json",
            },
          }
        );

        const id = JSON.parse(identityResponse.body);

        // Check to see if the user is sotce.
        const isSotce =
          id.data.attributes.email === "hellosotce@gmail.com" &&
          id.data.attributes.is_email_verified;

        // Get membership info for this user associated with sotce's campaign.
        const sotceCampaignID = "4483adce-b1bb-41c7-aea4-d0bacd02b0ed";
        const membership = id.included?.filter(
          (v) => v.id === sotceCampaignID
        )[0]?.attributes;

        // Give only sotce's account & $3 > tier accounts access.
        hasBlogAccess =
          isSotce ||
          (membership?.currently_entitled_amount_cents >= 300 &&
            membership?.patron_status === "active_patron");
      } catch (err) {
        // TODO: - [] Show an access denied page?
        //            Is this what happens if the user doesn't allow?
        // TODO: - [] What if they are not logged into patreon?

        return {
          headers: {
            "Content-Type": "application/json",
          },
          statusCode: 500,
          body: JSON.stringify({ hasBlogAccess, error: err }),
        };
      }

      let blogBody;

      if (hasBlogAccess) {
        blogBody = `
          ${event.queryStringParameters.code}
          <script>
            localStorage.setItem("${storageItem}", ${hasBlogAccess});
            window.close();
          </script>
        `; // Just show a totally empty body that sets the storage
        // and closes the window.
      } else {
        // Rejected case... maybe do a redirect here or something...
        blogBody = `<script>window.close();</script>`;
      }

      return {
        statusCode: 200,
        headers: {
          "Content-Type": "text/html",
        },
        body: blogBody,
      };
    }
  } else {
    return {
      statusCode: 405,
      body: "Unauthorized HTTP Method",
    };
  }
}

export const handler = fun;
