// Deprecated: guidelines are now served by news.mjs
export async function handler() {
  return {
    statusCode: 410,
    body: "Gone",
    headers: { "Content-Type": "text/plain" },
  };
}
