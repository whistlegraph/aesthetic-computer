// Exercise PNG encoding, upload requests, progress, and completion callbacks.
// Limit interception to these endpoints so worker/module loading is untouched.
export async function mockNoPaintUploads(page, baseURL) {
  const origin = new URL(baseURL).origin;
  const state = { mode: "success", presigns: 0, puts: 0, saves: 0, files: {}, tracked: [] };
  await page.exposeFunction("__acCaptureNoPaintUpload", (ext, base64) => {
    state.files[ext] = Buffer.from(base64, "base64");
  });
  await page.evaluateOnNewDocument(() => {
    const open = XMLHttpRequest.prototype.open;
    const send = XMLHttpRequest.prototype.send;
    XMLHttpRequest.prototype.open = function (method, url, ...args) {
      this.noPaintUploadExt = String(url).match(/\/api\/nopaint-test-upload\.(png|zip)$/)?.[1];
      return open.call(this, method, url, ...args);
    };
    XMLHttpRequest.prototype.send = function (body) {
      if (this.noPaintUploadExt && body instanceof Blob) {
        const ext = this.noPaintUploadExt;
        const reader = new FileReader();
        reader.onload = () => window.__acCaptureNoPaintUpload(ext, reader.result.split(",")[1]);
        reader.readAsDataURL(body);
      }
      return send.call(this, body);
    };
  });
  const holds = new Map();
  state.hold = (stage) => {
    let release;
    const promise = new Promise((resolve) => { release = resolve; });
    holds.set(stage, promise);
    return () => { holds.delete(stage); release(); };
  };
  const client = await page.createCDPSession();
  await client.send("Fetch.enable", { patterns: [
    { urlPattern: `${origin}/presigned-upload-url/*` },
    { urlPattern: `${origin}/api/nopaint-test-upload.*` },
    { urlPattern: `${origin}/api/track-media-stream` },
  ] });
  client.on("Fetch.requestPaused", async ({ requestId, request }) => {
    const url = new URL(request.url);
    let status = 200;
    let contentType = "application/json";
    let body = "";
    if (url.pathname.startsWith("/presigned-upload-url/")) {
      state.presigns++;
      await holds.get("presign");
      status = state.mode === "presign-error" ? 404 : 200;
      const ext = url.pathname.split("/")[2];
      body = JSON.stringify({ uploadURL: `${origin}/api/nopaint-test-upload.${ext}`, slug: `nopaint-test.${ext}` });
    } else if (request.method === "PUT") {
      state.puts++;
      status = state.mode === "storage-error" ? 403 : 200;
    } else if (url.pathname === "/api/track-media-stream") {
      state.saves++;
      state.tracked.push(JSON.parse(request.postData));
      await holds.get("save");
      contentType = "text/event-stream";
      body = `event: complete\ndata: ${JSON.stringify({ code: state.code || (state.saves === 1 ? "test" : `test${state.saves}`) })}\n\n`;
    }
    await client.send("Fetch.fulfillRequest", {
      requestId,
      responseCode: status,
      responseHeaders: [{ name: "Content-Type", value: contentType }],
      body: Buffer.from(body).toString("base64"),
    });
  });
  return state;
}
