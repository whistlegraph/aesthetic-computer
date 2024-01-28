// Redirect Proxy, 24.01.28.00.16
// For authorizing clients via the VSCode Extension both on desktop and on the web.
// Transcribed from: https://github.com/estruyf/vscode-redirect/blob/main/pages/index.tsx

async function fun(event, context) {
  const { queryStringParameters } = event;
  const state = queryStringParameters.state;
  let url = "";

  if (state) {
    const decodedUrl = new URL(decodeURIComponent(state));
    Object.keys(queryStringParameters).forEach((key) => {
      if (key !== "state") {
        decodedUrl.searchParams.set(key, queryStringParameters[key]);
      }
    });

    if (
      decodedUrl.href &&
      (decodedUrl.href.startsWith("vscode://") ||
        decodedUrl.href.startsWith("vscode-insiders://") ||
        decodedUrl.href.includes(".github.dev") ||
        decodedUrl.href.includes(".gitpod.io") ||
        decodedUrl.href.startsWith("https://vscode.dev"))
    ) {
      url = decodedUrl.href;
    }
  }

  const html = `
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>aesthetic.computer</title>
        <style>
          body { background: rgb(32, 32, 32); }
        </style>
    </head>
    <body>
        <div>
            <h1>You'll be redirected to Visual Studio Code!</h1>
            <p>${
              url
                ? `In case you are not redirected, <a href="${url}" title="Open Visual Studio Code">click here</a>.`
                : "No redirect URL provided"
            }</p>
        </div>
        <script>
            // Redirection logic here if needed
            if("${url}") {
                window.location.href = "${url}";
            }
        </script>
    </body>
    </html>
  `;

  return {
    statusCode: 200,
    headers: { "Content-Type": "text/html" },
    body: html,
  };

  // return url
  //   ? { statusCode: 302, headers: { Location: url } }
  //   : { statusCode: 200, body: "No redirect URL provided" };
}

export const handler = fun;
