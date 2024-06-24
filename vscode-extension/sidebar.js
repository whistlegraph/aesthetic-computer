(function () {
  const vscode = acquireVsCodeApi();

  // Send session data to iframe
  const iframe = document.getElementById("aesthetic");

  iframe.classList.add("visible");

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
        vscode.postMessage({ type: "clipboard:copy", value: message.value });
        break;
      }
      case "url:updated": {
        vscode.postMessage({ type: "url:updated", slug: message.slug });
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
        vscode.postMessage({ type: "login", tenant: message.tenant || "aesthetic" });
        break;
      }
      case "logout": {
        vscode.postMessage({ type: "logout", tenant: message.tenant || "aesthetic" });
        break;
      }
      case "ready": {
        clearTimeout(readyTimeout);
        break;
      }
    }
  });

  // ♻️ Refresh the iframe's src url until it loads successfully.
  let readyTimeout = setTimeout(refresh, 5000);

  function refresh() {
    console.log("🫐 Awaiting...");
    const url = new URL(iframe.src);
    url.searchParams.set("ac-timestamp", new Date().getTime());
    iframe.src = url.href;
    readyTimeout = setTimeout(refresh, 5000);
  }
})();
