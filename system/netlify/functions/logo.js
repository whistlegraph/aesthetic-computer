// Logo, 23.05.02.22.09
// Proxy a random logo from one endpoint.

/* #region 🏁 TODO 
  - [] Does the `favicon` actually work with CORS?
#endregion */

import { respond } from "../../backend/http.mjs";
import { logoUrl } from "../../backend/logo.mjs";

export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  const { got } = await import("got");

  const chosenLogo = logoUrl();

  const response = await got(chosenLogo, {
    responseType: "buffer",
    https: { rejectUnauthorized: false },
  });

  const base64Logo = Buffer.from(response.body, "binary").toString("base64");
  const dataUrl = `data:image/png;base64,${base64Logo}`;


  // Check the User-Agent to determine the type of request
  const userAgent = event.headers["user-agent"] || "";
  const isServer =
    userAgent.includes("curl") ||
    userAgent.includes("wget") ||
    userAgent.includes("python-requests") ||
    userAgent.includes("node-fetch");

  if (isServer) {
    // If it's a server tool or library, return the PNG
    return {
      statusCode: 200,
      headers: {
        "Content-Type": "image/png",
      },
      body: base64Logo,
      isBase64Encoded: true,
    };
  } else {
    // If it's not recognized as a server tool, return HTML with the image centered and background purple
    const htmlResponse = `
      <html>
        <head>
          <link rel="icon" href="${dataUrl}" type="image/x-icon">
          <style>
            body { 
              display: flex; 
              justify-content: center; 
              align-items: center; 
              height: 100vh;
              background-color: ${randomPurple()};
              margin: 0;
              overflow: hidden;
            }
            img { 
              object-fit: contain;
              width: 100vw;
              height: 100vh;
              cursor: pointer;
            }
          </style>
        </head>
        <body>
          <img crossorigin src="${dataUrl}" onclick="location.reload()">
        </body>
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
