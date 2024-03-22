(function () {
  const vscode = acquireVsCodeApi();

  //     let origin;
  //     if (
  //       window.location.href.startsWith("vscode-webview") ||
  //       window.location.href.startsWith("http://")
  //     ) {
  //       origin = "https://aesthetic.computer";
  //     } else {
  //       // not window.top.origin;
  //       // not https://vscode.dev
  //       // not window.parent.origin
  //       // not window.location.origin
  //       // not "*"
  //       origin = "https://aesthetic.computer";
  //     }

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
})();
