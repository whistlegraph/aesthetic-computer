// Sotce Blog, 2023.01.17.21.58
// Allows sotce to give Patreon subscribers access to her blog (Tumblr).
// Patreon dev docs: https://www.patreon.com/portal

/* #region todo 📓 
 - [] Explain to sotce a little more in detail how it works / loopholes etc.
+ Done
 - [x] Make it cute and put it into production.
 - [x] Test on a phone. 
 - [x] What if they are not logged into Patreon?
 - [x] Create a session and hook it up to the Tumblr somehow?
 - [x] Make sure it is not annoying / can take multiple page hits.
  - [x] (Only expire after a certain timestamp)
 - [x] Hook up to blog.sotce.com.
 - [x] Write the logic that detects whether a user is sotce or a subscriber. 
#endregion */

/* region docs 📚
Note: Add the below to a Tumblr theme to activate the gate on that site.
  <iframe src="https://blog.sotce.com" id="sotce-gate"></iframe> 
  <style>
  #sotce-gate {
    border: none;
    position: absolute;
    top: 0;
    left: 0;
    z-index: 100;
    width: 100vw;
    height: 100vh;
  }
  body.curtain header,
  body.curtain footer,
  body.curtain div.content {
    display: none;
  }
  </style>
  <script>
    document.body.classList.add("curtain");

    window.addEventListener('message', function (e) {
      if (e.data === "sotceBlogReveal") {
        localStorage.setItem(
          "hasSotceBlogAccess",
          JSON.stringify({value: true, timestamp: new Date().getTime()})
        );
        document.body.classList.remove("curtain");
      } 
    });

    const currentTime = new Date().getTime();
    const data = JSON.parse(localStorage.getItem("hasSotceBlogAccess"));
    const sotceGate = document.querySelector("#sotce-gate");

    if (data && currentTime - data.timestamp < 24*60*60*1000) {
      // Data is still valid. Remove sotceGate and curtain.
      console.log("Welcome!");
      sotceGate.remove();
      document.body.classList.remove("curtain");
    } else {
      // Data has expired. Keep things closed.
      console.log("Please authorize through Patreon...");
    }
  </script>
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
    // See also https://docs.patreon.com/#scopes
    const state = crypto.randomBytes(32).toString("hex"); // For securing the request.
    const loginUrl = `https://www.patreon.com/oauth2/authorize?response_type=code&client_id=${client}&redirect_uri=${redirect}&state=${state}&scope=${scope}`;

    const gateBody = `
      <!DOCTYPE html>
      <html>
        <head>
          <script>
            function openPopup() {
              // Center the popup in the current window.
              const parentWidth = window.innerWidth;
              const parentHeight = window.innerHeight;
              const parentLeft = window.screenX;
              const parentTop = window.screen.availTop;
              const screenHeight = window.screen.availHeight;
              const left = parentLeft + (parentWidth - 400) / 2;
              const top = parentTop + (screenHeight - 650) / 2;

              const popup = window.open("${loginUrl}", "Patreon Authorization", \`width=400, height=650, left=\${left}, top=\${top}\`);
              const hasSotceBlogAccess = localStorage.setItem("${storageItem}", false);

              window.addEventListener("storage", function (event) {
                if (event.key === "${storageItem}" && Boolean(event.newValue) === true) {
                  window.parent.postMessage("sotceBlogReveal", '*');
                  document.body.classList.remove("gate");
                }
              });
            }
          </script>
          <style>
            body.gate { background: white; }
            body:not(.gate) * { display: none; }
            body:not(.gate) { background: transparent; }
            button {
              font-family: Helvetica, Arial, sans-serif;
              color: black;
              border: none;
              background: none;
              font-size: 8vmin;
              cursor: pointer;
            }
            button:hover {
              transform: scale(1.2);
            }
            button:active {
              transform: scale(1.1);
            }
            body { display: flex; height: 100vh; overflow: hidden; }
            #wrapper { margin: auto; display: flex; flex-direction: column; user-select: none; }
            #enter { padding-bottom: 2vmin; }
            #doorway {
              max-width: 50vmin;
            }
          </style>
        </head>
        <body class="gate">
          <div id="wrapper">
            <button id="enter" onclick="openPopup()">enter</button>
            <img id="doorway" crossorigin="anonymous" src="https://sotce-media.aesthetic.computer/apple.jpg">
          </div>
        </body>
      </html>
    `;

    const closer = (status) => {
      let script;
      if (status === "success") {
        script = `
        <script>
          const hasSotceBlogAccess = localStorage.setItem("${storageItem}", true);
          window.close();
        </script>`;
      } else {
        script = `
        <script>
        window.close();
        </script>`;
      }

      return `
      <html>
        <head>
          <style>
            body {
              font-family: Helvetica, Arial, sans-serif;
              display: flex;
              margin: none;
              height: 100vh;
              overflow: hidden;
            }
            h3 {
              font-weight: normal;
              margin: auto;
            }
            .${status} {
              color: ${status === "success" ? "green" : "red"};
            }
          </style>
        </head>
        <body>
          <h3 class=${status}>you may now close me</h3>
          ${script}
        </body>
      </html>`;
    };

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
            : closer("failure"),
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
        // Should this error be handled better?
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
        blogBody = closer("success"); // Just show a totally empty body that sets the storage
        // and closes the window.
      } else {
        // Rejected case...
        blogBody = `<script> window.close(); </script>`;
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
