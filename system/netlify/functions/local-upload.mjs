// Local dev upload fallback — saves files to disk when S3 credentials are missing.
import { mkdirSync, writeFileSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));

export async function handler(event) {
  // Extract filename from path: /local-upload/CODE.ext
  const filename = event.path.split("/").pop();
  if (!filename) {
    return { statusCode: 400, body: "Missing filename" };
  }

  const dir = join(__dirname, "..", "..", "..", "local-uploads");
  mkdirSync(dir, { recursive: true });
  const filepath = join(dir, filename);

  // event.body is base64-encoded for binary content in Netlify Functions
  const body = event.isBase64Encoded
    ? Buffer.from(event.body, "base64")
    : Buffer.from(event.body || "");

  if (body.length === 0) {
    return { statusCode: 400, body: "Empty body" };
  }

  writeFileSync(filepath, body);
  console.log(`📁 Local upload saved: ${filepath} (${body.length} bytes)`);

  return {
    statusCode: 200,
    body: "OK",
  };
}
