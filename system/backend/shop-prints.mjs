// shop-prints.mjs — the print lane's products, in the shape the shop lists.
//
// AC has two shops. Shopify holds made objects; the `+` lane prints a
// painting on a mug to order through Printful and Stripe, and each of those
// has a `+code` and a page but no listing anywhere. This turns recent `+`
// products into the same records /api/shop returns for Shopify ones, so one
// list can show both. Nothing here talks to Printful or Stripe.

// What each kind of print is called and costs. The price is the one its own
// function charges (mug.js: $18, all-inclusive); keep the two in step.
export const PRINT_KINDS = {
  mug: { name: "mug", type: "Mug", price: 18 },
};

const SITE = "https://aesthetic.computer";

/** One product record as the shop lists it. `codes` maps a painting slug to its short code; `handle` is the maker's, with its @. */
export function printListing(product, { codes = {}, handle = "" } = {}) {
  const kind = PRINT_KINDS[product?.product];
  if (!kind || !/^[a-z0-9]{3,12}$/i.test(product.code || "")) return null;
  const slug = product.source?.slug || product.source?.code || "";
  const painting = codes[slug] || (product.source?.slug ? product.source.code : "") || slug;
  const via = product.source?.via ? ` in $${product.source.via}` : "";
  const color = product.variant && product.variant !== "white" ? `${product.variant} ` : "";
  const printCode = "+" + product.code;
  return {
    id: printCode,
    handle: printCode,
    printCode,
    source: "print",
    title: `${color}${kind.name}${painting ? ` of #${painting}` : ""}${via}`,
    vendor: handle || "aesthetic.computer",
    productType: kind.type,
    price: `$${kind.price} USD`,
    priceRaw: kind.price,
    imageUrl: product.preview || null,
    images: product.preview ? [product.preview] : [],
    description: "Printed to order.",
    descriptionHtml: "",
    shopUrl: `${SITE}/${printCode}`,
    jump: printCode, // the address inside AC, for a piece that can go there itself
    inventory: null,
    available: true,
    tags: ["print", kind.name, ...(product.variant ? [product.variant] : [])],
    createdAt: product.createdAt ? new Date(product.createdAt).toISOString() : null,
    updatedAt: product.updatedAt ? new Date(product.updatedAt).toISOString() : null,
  };
}

// The newest prints that have a picture to show, newest first. `db` is the
// Mongo database; `handleFor` turns a user id into a handle and may fail,
// in which case the print is listed without a maker.
export async function recentPrints(db, { limit = 12, handleFor = async () => null } = {}) {
  const found = await db.collection("products")
    .find({ product: { $in: Object.keys(PRINT_KINDS) }, preview: { $type: "string", $ne: "" } })
    .sort({ createdAt: -1 }).limit(Math.max(1, Math.min(50, limit))).toArray();
  const slugs = [...new Set(found.filter((p) => p.source?.type === "painting").map((p) => p.source.slug || p.source.code).filter(Boolean))];
  const codes = {};
  if (slugs.length) {
    const paintings = await db.collection("paintings")
      .find({ $or: [{ slug: { $in: slugs } }, { code: { $in: slugs } }] }, { projection: { code: 1, slug: 1 } }).toArray();
    for (const p of paintings) { if (p.slug && p.code) codes[p.slug] = p.code; if (p.code) codes[p.code] = p.code; }
  }
  const handles = new Map();
  for (const user of new Set(found.map((p) => p.source?.user).filter(Boolean))) {
    try { const handle = await handleFor(user); if (handle) handles.set(user, "@" + String(handle).replace(/^@/, "")); } catch {}
  }
  return found.map((p) => printListing(p, { codes, handle: handles.get(p.source?.user) || "" })).filter(Boolean);
}
