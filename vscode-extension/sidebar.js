(function () {
  const vscode = acquireVsCodeApi();

  // Send session data to iframe
  const iframe = document.getElementById("aesthetic");

  // Handle messages sent from the extension to the webview
  window.addEventListener("message", (event) => {
    const message = event.data; // The json data that the extension sent
    // console.log("📶 Received message:", message);
    switch (message.type) {
      case "vscode-extension:reload": {
        vscode.postMessage({ type: "vscode-extension:reload" });
        break;
      }
      case "setSession": {
        // window.aestheticSession = message.session;
        // sendSessionToIframe(message.session);
        break;
      }
      case "clipboard:copy": {
        // console.log("📎 Copying clipboard message...");
        vscode.postMessage({
          type: "clipboard:copy",
          value: message.value,
        });
        break;
      }
      case "clipboard:copy:confirmation": {
        iframe.contentWindow.postMessage(
          { type: "clipboard:copy:confirmation" },
          "*",
        );
        break;
      }
      case "publish": {
        vscode.postMessage({
          type: "publish",
          url: message.url,
        });
        break;
      }
      case "setCode": {
        vscode.postMessage({ type: "setCode", value: message.value });
        break;
      }
      case "runPiece": {
        vscode.postMessage({ type: "runPiece" });
        break;
      }
      case "openDocs": {
        vscode.postMessage({ type: "openDocs" });
        break;
      }
      case "openSource": {
        vscode.postMessage({
          type: "openSource",
          title: message.title,
          source: message.source,
        });
        break;
      }
      case "login": {
        vscode.postMessage({ type: "login" });
        break;
      }
      case "logout": {
        vscode.postMessage({ type: "logout" });
        break;
      }
    }
  });

  //console.log("Starting refresh logic...");

  //function refreshIframe() {
  //  console.log("♻️ Refreshing iframe due to load failure...");
  //  iframe.src += ""; // iframe.src.split("?")[0] + "?" + new Date().getTime();
  //}

  // Add an event listener to the iframe to set the flag when it loads
  //iframe.addEventListener("load", () => {
  //console.log("♻️ Iframe load event detected. Verifying content...");
  // Fetch the iframe src to make sure it returns 200, otherwise run refreshIframe
  // fetch(iframe.src, { method: "GET" })
  //   .then((response) => {
  //     console.log(response);
  //     if (response.ok) {
  //       console.log("♻️ Content verification successful.");
  //     } else {
  //       console.log("♻️ Content verification failed, refreshing iframe...");
  //       refreshIframe();
  //       // setTimeout(refreshIframe, 3000);
  //     }
  //   })
  //   .catch((error) => {
  //     console.error("♻️ Error during content verification:", error);
  //     // refreshIframe();
  //     refreshIframe();
  //     // setTimeout(refreshIframe, 1000);
  //   });
  //});
})();
