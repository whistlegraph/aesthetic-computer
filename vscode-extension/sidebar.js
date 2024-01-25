(function () {
  const vscode = acquireVsCodeApi();

  // Send session data to iframe
  function sendSessionToIframe() {
    const iframe = document.getElementById("aesthetic");
    console.log("From panel:", iframe, window.aestheticSession)
    if (iframe && window.aestheticSession) {
      iframe.contentWindow.postMessage(
        { type: "setSession", session: window.aestheticSession },
        "*",
      );
    }
  }

  window.addEventListener("load", sendSessionToIframe);

  // Handle messages sent from the extension to the webview
  window.addEventListener("message", (event) => {
    const message = event.data; // The json data that the extension sent
    console.log("📶 Received message:", message);
    switch (message.type) {
      case "publish": {
        vscode.postMessage({
          type: "publish",
          url: message.url,
        });
      }
      case "setCode": {
        vscode.postMessage({ type: "setCode", value: message.value });
      }
      case "runPiece": {
        vscode.postMessage({ type: "runPiece" });
        break;
      }
    }
  });
})();
