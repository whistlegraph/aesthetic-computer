(function () {
  const vscode = acquireVsCodeApi();

  // Send session data to iframe
  function sendSessionToIframe(session) {
    const iframe = document.getElementById("aesthetic");
    if (iframe && session) {
      iframe.contentWindow.postMessage({ type: "setSession", session }, "*");
    } else if (iframe) {
      console.log("🔴 Clearing session...");
      iframe.contentWindow.postMessage({ type: "clearSession" }, "*");
      window.aestheticSession = null;
    }
  }

  window.addEventListener("load", () => {
    console.log("Loaded window...", window.aestheticSession);
    if (window.aestheticSession) sendSessionToIframe(window.aestheticSession);
  });

  // Handle messages sent from the extension to the webview
  window.addEventListener("message", (event) => {
    const message = event.data; // The json data that the extension sent
    console.log("📶 Received message:", message);
    switch (message.type) {
      case "vscode-extension:reload": {
        vscode.postMessage({ type: "vscode-extension:reload" });
        break;
      }
      case "setSession": {
        window.aestheticSession = message.session;
        sendSessionToIframe(message.session);
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
})();
