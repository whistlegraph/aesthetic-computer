// Sotce Diary
// Allows sotce to give Patreon subscribers access to her diary (Tumblr).

/* #region todo 📓 
 - [] 
+ Done
#endregion */

const dev = process.env.NETLIFY_DEV;
// const patreonAccessToken = process.env.SOTCE_PATREON_ACCESS_TOKEN;
// const patreonRefreshToken = process.env.SOTCE_PATREON_REFRESH_TOKEN;

import crypto from "crypto"; // Import crypto library.

async function fun(event, context) {
  const host = dev ? "localhost" : "sotce.aesthetic.computer";
  const origin = dev
    ? "https://localhost:8888/sotce-diary"
    : "https://sotce.aesthetic.computer";
  const redirect = "https://sotce.aesthetic.computer"; // This also needs to be set on the Patreon developer account.

  // 1. Return an entry page if it's a GET.
  if (event.httpMethod === "GET") {
    return {
      statusCode: 200,
      headers: {
        "Content-Type": "text/html",
      },
      body: `
        <h1>Sotce's Diary</h1>
        <button id="submit-button">Submit</button>
        <script>
          document.getElementById("submit-button").addEventListener("click", async () => {
            // Add your fetch request here
            const response = await fetch('${origin}', {
              method: 'POST',
              body: JSON.stringify({ tier: 'your_tier' }),
              headers: { 'Content-Type': 'application/json' },
            });
            const data = await response.json();
            console.log(data);
          });
        </script>
      `,
    };
  } else if (event.httpMethod !== "POST") {
    // 2. Or deny access if it's not POST.
    return {
      statusCode: 405,
      body: "Unauthorized HTTP Method",
    };
  }

  const client = process.env.SOTCE_PATREON_CLIENT_ID;
  const secret = process.env.SOTCE_PATREON_CLIENT_SECRET;

  // Assume POST
  return {
    statusCode: 307,
    headers: {
      "Access-Control-Allow-Origin": "*",
      Location: `https://www.patreon.com/oauth2/authorize?response_type=code&client_id=${client}&redirect_uri=${redirect}&scope=users&state=${crypto
        .randomBytes(32)
        .toString("hex")}`,
    },
  };

  // 3. Otherwise identify the user.
  // const { got } = await import("got"); // Import the "got" http library.

  // Send the request to the Patreon OAuth2 authorization endpoint

  // const { body } = await got({
  //   method: "GET",
  //   url: "https://www.patreon.com/oauth2/authorize",
  //   searchParams: {
  //     response_type: "code",
  //     client_id: client,
  //     redirect_uri: redirect,
  //     scope: "users",
  //     state: crypto.randomBytes(32).toString("hex"),
  //   },
  //   followRedirect: false,
  // });
  // Extract the code from the redirect URL

  // const url = new URL(body);
  // const code = url.searchParams.get("code");

  // console.log(url, code);

  // Get the access token
  /*
  const { body } = await got.post("https://www.patreon.com/api/oauth2/token", {
    json: true,
    form: {
      grant_type: "authorization_code",
      client_id: client,
      client_secret: secret,
      redirect_uri: redirect,
      code,
    },
  });

  console.log(body);
  */

  /*
  // Extract the access token from the response
  const access_token = body.access_token;
  // Extract the patron_id from the response
  const patron_id = body.refresh_token;
  // Extract the tier from the event
  const { tier } = JSON.parse(event.body);

  // Make a GET request to the Patreon API to retrieve the patron's membership
  const { body: membershipData } = await got(
    `https://www.patreon.com/api/oauth2/v2/members/${patron_id}`,
    {
      headers: {
        Authorization: `Bearer ${access_token}`,
        "Content-Type": "application/json",
      },
      json: true,
    }
  );

  // Check if the user is subscribed to the specified tier
  const isSubscribed = membershipData.included.some(
    (membership) => membership.attributes.tier_name === tier
  );
  */

  const isSubscribed = true;

  // Return the result
  return {
    statusCode: 200,
    body: JSON.stringify({ isSubscribed }),
  };
}

export const handler = fun;
