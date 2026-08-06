import { createHash } from "node:crypto";
import { existsSync, readFileSync, statSync } from "node:fs";
import { extname, resolve } from "node:path";

const MAX_MESSAGES_MEDIA_BYTES = 20 * 1024 * 1024;

const MESSAGES_MEDIA_TYPES = new Map([
  [".jpg", { kind: "image", mimeType: "image/jpeg" }],
  [".jpeg", { kind: "image", mimeType: "image/jpeg" }],
  [".png", { kind: "image", mimeType: "image/png" }],
  [".heic", { kind: "image", mimeType: "image/heic" }],
  [".heif", { kind: "image", mimeType: "image/heif" }],
  [".webp", { kind: "image", mimeType: "image/webp" }],
  [".pdf", { kind: "document", mimeType: "application/pdf" }],
]);

function assertPdfHeader(bytes) {
  // ISO 32000 permits the PDF header to appear within the first 1024 bytes.
  if (bytes.subarray(0, 1024).indexOf(Buffer.from("%PDF-")) < 0) {
    throw new Error("PDF attachment does not contain a valid PDF header");
  }
}

export function inspectMessagesMedia(path) {
  const absolute = resolve(String(path || ""));
  if (!existsSync(absolute)) throw new Error(`attachment not found: ${absolute}`);
  const stat = statSync(absolute);
  if (!stat.isFile()) throw new Error(`attachment is not a file: ${absolute}`);

  const extension = extname(absolute).toLowerCase();
  const mediaType = MESSAGES_MEDIA_TYPES.get(extension);
  if (!mediaType) {
    throw new Error(
      `unsupported Messages attachment type "${extension || "(none)"}" — use jpg, png, heic, webp, or pdf`,
    );
  }
  if (stat.size <= 0) throw new Error("attachment is empty");
  if (stat.size > MAX_MESSAGES_MEDIA_BYTES) {
    throw new Error("attachment exceeds the 20 MB guarded send limit");
  }

  const bytes = readFileSync(absolute);
  if (extension === ".pdf") assertPdfHeader(bytes);

  return {
    path: absolute,
    bytes: stat.size,
    sha256: createHash("sha256").update(bytes).digest("hex"),
    extension,
    ...mediaType,
  };
}

export function chooseMessagesMediaTransport(kind, requested, appleService) {
  if (!new Set(["auto", "backend", "ui"]).has(requested)) {
    throw new Error(`unknown media transport "${requested}" (use auto, backend, or ui)`);
  }
  if (kind === "document") {
    if (requested === "ui") {
      throw new Error("PDF attachments require the verified backend media transport");
    }
    if (requested === "auto" && appleService !== "iMessage") {
      throw new Error(
        `PDF attachments over ${appleService} require explicit mediaTransport=backend until that route is verified`,
      );
    }
    return { primary: "backend", allowUiFallback: false };
  }
  if (requested === "ui") return { primary: "ui", allowUiFallback: false };
  if (requested === "backend") return { primary: "backend", allowUiFallback: false };
  if (appleService === "iMessage") return { primary: "backend", allowUiFallback: true };
  return { primary: "ui", allowUiFallback: false };
}
