// Logo, 23.05.02.22.09
// Proxy a random logo from one endpoint.
// If loaded in the browser, return a tappable HTML response to cycle logos.
// Otherwise, proxy the `.png`.

/* #region 🏁 TODO 
  - [] Make it cooler?
#endregion */

import { respond } from "../../backend/http.mjs";
import { logoUrl } from "../../backend/logo.mjs";

export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  const { got } = await import("got");

  const queryParams = new URLSearchParams(event.queryStringParameters);
  const previousLogo = queryParams.get("previousLogo");

  let logo;
  do {
    logo = logoUrl();
  } while (logo === previousLogo); // Make sure we don't select the same logo

  const response = await got(logo, {
    responseType: "buffer",
    https: { rejectUnauthorized: false },
  });

  const base64Logo = Buffer.from(response.body, "binary").toString("base64");

  const userAgent = event.headers["user-agent"] || "";
  const isServer =
    userAgent.includes("curl") ||
    userAgent.includes("wget") ||
    userAgent.includes("python-requests") ||
    userAgent.includes("node-fetch");
  const isPngEndpoint = event.path.split("/").pop() === "logo.png";

  if (isServer || isPngEndpoint) {
    return {
      statusCode: 200,
      headers: {
        "Content-Type": "image/png",
      },
      body: base64Logo,
      isBase64Encoded: true,
    };
  } else {
    const htmlResponse = `
    <html>
        <head>
            <link rel="icon" href="${logo}" type="image/x-icon">
            <style>
                html { height: 100%; }
                body { 
                    display: flex; 
                    justify-content: center; 
                    align-items: center; 
                    height: 100%;
                    background-color: ${randomPurple()};
                    margin: 0;
                    overflow: hidden;
                    transition: filter 0.2s;
                }
                body.blurred {
                    filter: blur(5px);
                }
                img { 
                    object-fit: contain;
                    width: 100vw;
                    height: 100%;
                    cursor: pointer;
                }
            </style>
        </head>
        <body>
            <img crossorigin src="${logo}" onclick="updateQueryString()">
        </body>
        <script>
            const strippedUrl = window.location.origin + window.location.pathname;
            window.history.replaceState({}, document.title, strippedUrl);
            function updateQueryString() {
              document.body.classList.add('blurred');
              const currentLogoId = '${logo}'; 
              const newURL = strippedUrl + '?previousLogo=' + encodeURIComponent(currentLogoId);
              setTimeout(() => { window.location = newURL; }, 200);
            }
        </script>
    </html>
`;

    return {
      statusCode: 200,
      headers: {
        "Content-Type": "text/html",
      },
      body: htmlResponse,
    };
  }
}

const randomPurple = () => {
  const r = Math.floor(Math.random() * 50 + 50); // Values between 200 and 255
  const g = Math.floor(Math.random() * 25); // Values between 0 and 50
  const b = Math.floor(Math.random() * 50 + 50); // Values between 200 and 255
  return `rgb(${r},${g},${b})`;
};
