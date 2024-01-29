(function () {
  const vscode = acquireVsCodeApi();

  // It seems like no matter what I try I can't communicate logins and logouts
  // to the iframe through vscode, so I might as well copy over the session
  // information to retrieve it in a url for the iframe.

  // Send session data to iframe
  // function sendSessionToIframe(session) {
  //   const iframe = document.getElementById("aesthetic");
  //   console.log("Iframe:", iframe, "Session:", session, iframe && session);
  //   if (iframe && session) {
  //     console.log(
  //       "sending to boot...",
  //       iframe.contentWindow,
  //       iframe.src,
  //       "top",
  //       window.top,
  //       "parent:",
  //       window.parent,
  //     );
  //     // x production has a window.top but no window.parent
  //     // x and so does local...
  //     // so how do i know if i'm in production or not?
  //     console.log("Location origin:", window.location.origin);
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
  //     console.log("Computed origin:", origin);
  //     iframe.contentWindow.postMessage({ type: "setSession", session }, origin);
  //   } else if (iframe) {
  //     console.log("🔴 Clearing session...");
  //     iframe.contentWindow.postMessage({ type: "clearSession" }, origin);
  //     window.aestheticSession = null;
  //   }
  // }

  // window.addEventListener("load", () => {
  // console.log("Loaded webview...", window.aestheticSession);
  // if (window.aestheticSession) sendSessionToIframe(window.aestheticSession);
  // });

  // Handle messages sent from the extension to the webview
  window.addEventListener("message", (event) => {
    const message = event.data; // The json data that the extension sent
    // console.log("📶 Received message:", message);
    // console.log("Message type:", message.type);
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
