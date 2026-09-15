// Exercise PNG encoding, upload requests, progress, and completion callbacks.
// Limit interception to these endpoints so worker/module loading is untouched.
export async function mockNoPaintUploads(page, baseURL) {
  const origin = new URL(baseURL).origin;
  const state = { mode: "success", presigns: 0, puts: 0, saves: 0 };
  const client = await page.createCDPSession();
  await client.send("Fetch.enable", { patterns: [
    { urlPattern: `${origin}/presigned-upload-url/*` },
    { urlPattern: `${origin}/api/nopaint-test-upload.png` },
    { urlPattern: `${origin}/api/track-media-stream` },
  ] });
  client.on("Fetch.requestPaused", async ({ requestId, request }) => {
    const url = new URL(request.url);
    let status = 200;
    let contentType = "application/json";
    let body = "";
    if (url.pathname.startsWith("/presigned-upload-url/")) {
      state.presigns++;
      status = state.mode === "presign-error" ? 404 : 200;
      body = JSON.stringify({ uploadURL: `${origin}/api/nopaint-test-upload.png`, slug: "nopaint-test.png" });
    } else if (request.method === "PUT") {
      state.puts++;
      status = state.mode === "storage-error" ? 403 : 200;
    } else if (url.pathname === "/api/track-media-stream") {
      state.saves++;
      contentType = "text/event-stream";
      body = `event: complete\ndata: ${JSON.stringify({ code: state.saves === 1 ? "test" : `test${state.saves}` })}\n\n`;
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
