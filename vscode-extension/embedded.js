(function () {
  const vscode = acquireVsCodeApi();

  // Send session data to iframe
  const iframe = document.getElementById("aesthetic");

  iframe.classList.add("visible");

  // Handle messages sent from the extension to the webview AND from the iframe
  window.addEventListener("message", (event) => {
    const message = event.data; // The json data that the extension sent
    console.log("📶 Received message:", message, "from:", event.source === iframe.contentWindow ? "iframe" : "extension");
    switch (message.type) {
      case "vscode-extension:reload": {
        vscode.postMessage({ type: "vscode-extension:reload" });
        break;
      }
      case "vscode-extension:defocus": {
        vscode.postMessage({ type: "vscode-extension:defocus" });
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
      // case "aesthetic-panel:open": {
      //   console.log("🪧 Posting panel opener...");
      //   iframe.contentWindow.postMessage({ type: "aesthetic-panel:open" }, "*");
      //   break;
      // }
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
        vscode.postMessage({
          type: "login",
          tenant: message.tenant || "aesthetic",
        });
        break;
      }
      case "logout": {
        vscode.postMessage({
          type: "logout",
          tenant: message.tenant || "aesthetic",
        });
        break;
      }
      case "openExternal": {
        vscode.postMessage({
          type: "openExternal",
          url: message.url,
        });
        break;
      }
      case "ready": {
        console.log("🫐 ✅ Received ready message, clearing timeout");
        clearTimeout(readyTimeout);
        break;
      }
    }
  });

  // Add event listener for when the window is focused.
  window.addEventListener("focus", () => {
    iframe.contentWindow.postMessage({ type: "aesthetic-parent:focused" }, "*");
  });

  // ♻️ Refresh the iframe's src url until it loads successfully.
  let readyTimeout = setTimeout(refresh, 5000);
  let refreshCount = 0;
  const maxRefreshAttempts = 5; // Increased retry attempts

  function refresh() {
    refreshCount++;
    if (refreshCount > maxRefreshAttempts) {
      console.log("🫐 Max refresh attempts reached, assuming ready");
      clearTimeout(readyTimeout);
      return;
    }
    console.log(`🫐 Awaiting... (attempt ${refreshCount}/${maxRefreshAttempts})`);
    
    // Post a message to the iframe to show retry status in boot log
    try {
      iframe.contentWindow?.postMessage({ 
        type: "boot-retry", 
        attempt: refreshCount, 
        maxAttempts: maxRefreshAttempts 
      }, "*");
    } catch (e) {
      // Iframe might not be ready yet
    }
    
    const url = new URL(iframe.src);
    url.searchParams.set("ac-timestamp", new Date().getTime());
    iframe.src = url.href;
    readyTimeout = setTimeout(refresh, 5000);
  }
  
  // Also listen for iframe load event as a fallback
  iframe.addEventListener('load', () => {
    console.log("🫐 Iframe loaded");
    // Give the iframe content a bit of time to initialize and send ready message
    setTimeout(() => {
      if (readyTimeout) {
        console.log("🫐 Clearing timeout after iframe load");
        clearTimeout(readyTimeout);
      }
    }, 2000);
  });
})();
