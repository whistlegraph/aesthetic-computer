// iiif.mjs
// IIIF (International Image Interoperability Framework) surfaces for the
// paintings — the image-native counterpart to the Linked Art records. Getty and
// peer institutions drive their image viewers (Mirador, Universal Viewer) and
// harvesters off IIIF, so exposing a Presentation v3 Manifest + a Level-0 Image
// service makes AC paintings loadable in the exact tools researchers use.
//
// Pure builders: dimensions are read separately (pngDimensions) and passed in.
// Identifiers live under DATA_BASE/iiif/{code} so they never collide with the
// Linked Art record at DATA_BASE/painting/{code}.
// 2026.07.01

import { DATA_BASE, WEB_BASE, LICENSES, RIGHTS_STATEMENTS } from "./linked-art.mjs";

const PRES_CONTEXT = "http://iiif.io/api/presentation/3/context.json";
const IMAGE_CONTEXT = "http://iiif.io/api/image/3/context.json";

// IIIF `rights` must be a single CC or rightsstatements.org URI — which is
// exactly what our license/rights registry already holds.
function rightsUrl(key) {
  const entry = LICENSES[key] || RIGHTS_STATEMENTS[key];
  return entry ? entry[0] : undefined;
}

// Read a PNG's pixel dimensions from its IHDR header (first 24 bytes), fetched
// via a Range request so we don't download the whole image. Falls back to a full
// GET if the CDN ignores Range. Returns null if it can't be read.
export async function pngDimensions(url) {
  try {
    const res = await fetch(url, { headers: { Range: "bytes=0-33" } });
    if (!res.ok && res.status !== 206) return null;
    const buf = Buffer.from(await res.arrayBuffer());
    if (buf.length < 24 || buf[0] !== 0x89 || buf[1] !== 0x50) return null; // PNG sig
    return { width: buf.readUInt32BE(16), height: buf.readUInt32BE(20) };
  } catch {
    return null;
  }
}

// IIIF Image API 3.0 info.json, Level 0 (only the full static image is served;
// the full-image request redirects to the CDN — see crm.mjs).
export function imageInfo({ code, width, height }) {
  return {
    "@context": IMAGE_CONTEXT,
    id: `${DATA_BASE}/iiif/${code}`,
    type: "ImageService3",
    protocol: "http://iiif.io/api/image",
    profile: "level0",
    width,
    height,
    sizes: [{ width, height }],
    preferredFormats: ["png"],
  };
}

// IIIF Presentation API 3.0 Manifest — one Canvas carrying the painting, with
// rights, attribution, a homepage back to aesthetic.computer, and a seeAlso to
// the Linked Art (CIDOC CRM) record.
export function paintingManifest({ code, handle, when, license, width, height }) {
  const clean = handle.replace(/^@/, "");
  const base = `${DATA_BASE}/iiif/${code}`;
  const rights = rightsUrl(license);
  const label = `painting ${code} by @${clean}`;

  const body = {
    id: `${base}/full/max/0/default.png`,
    type: "Image",
    format: "image/png",
    width,
    height,
    service: [{ id: base, type: "ImageService3", profile: "level0" }],
  };

  return {
    "@context": PRES_CONTEXT,
    id: `${base}/manifest`,
    type: "Manifest",
    label: { en: [label] },
    ...(rights ? { rights } : {}),
    requiredStatement: {
      label: { en: ["Attribution"] },
      value: { en: [`@${clean} — Aesthetic Computer`] },
    },
    homepage: [
      {
        id: `${WEB_BASE}/painting/${code}`,
        type: "Text",
        label: { en: ["View on Aesthetic Computer"] },
        format: "text/html",
      },
    ],
    seeAlso: [
      {
        id: `${DATA_BASE}/painting/${code}`,
        type: "Dataset",
        label: { en: ["Linked Art (CIDOC CRM) record"] },
        format: "application/ld+json",
      },
    ],
    metadata: [
      ...(when
        ? [{ label: { en: ["Created"] }, value: { en: [new Date(when).toISOString().slice(0, 10)] } }]
        : []),
      { label: { en: ["Artist"] }, value: { en: [`@${clean}`] } },
      { label: { en: ["Type"] }, value: { en: ["digital painting"] } },
    ],
    items: [
      {
        id: `${base}/canvas`,
        type: "Canvas",
        height,
        width,
        items: [
          {
            id: `${base}/canvas/page`,
            type: "AnnotationPage",
            items: [
              {
                id: `${base}/canvas/page/annotation`,
                type: "Annotation",
                motivation: "painting",
                target: `${base}/canvas`,
                body,
              },
            ],
          },
        ],
      },
    ],
  };
}
