(function () {
  const vscode = acquireVsCodeApi();

  // Send session data to iframe
  function sendSessionToIframe(session) {
    const iframe = document.getElementById("aesthetic");
    if (iframe && session) {
      iframe.contentWindow.postMessage({ type: "setSession", session }, "*");
    }
  }

  window.addEventListener("load", () => {
    sendSessionToIframe(window.aestheticSession);
  });

  // Handle messages sent from the extension to the webview
  window.addEventListener("message", (event) => {
    const message = event.data; // The json data that the extension sent
    console.log("📶 Received message:", message);
    switch (message.type) {
      case "setSession": {
        sendSessionToIframe(message.session);
      }
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
