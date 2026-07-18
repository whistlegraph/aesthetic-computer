const ENDPOINT = "https://Kunaki.com/HTTPService.ASP";

export const CASSETTE_SPECS = Object.freeze({
  source: "https://kunaki.com/product-cassette.html",
  checked: "2026-07-18",
  priceUsd: 5,
  manufactureHours: 24,
  audio: {
    maxMinutesPerSide: 40,
    accepted: ["wav", "mp3", "aac", "wmv", "m4a"],
  },
  artwork: {
    format: "jpeg",
    dpi: 300,
    bleedRequired: false,
    jCard: { width: 1200, height: 1110 },
    labelA: { width: 1062, height: 496 },
    labelB: { width: 1062, height: 496 },
  },
});

const required = (value, name) => {
  if (value === undefined || value === null || value === "") throw new Error(`Missing ${name}`);
  return String(value);
};

function products(params, items) {
  if (!Array.isArray(items) || items.length === 0) throw new Error("At least one product is required");
  for (const [index, item] of items.entries()) {
    params.append("ProductId", required(item.productId, `items[${index}].productId`));
    params.append("Quantity", required(item.quantity ?? 1, `items[${index}].quantity`));
  }
}

export function shippingOptionsUrl({ country, stateProvince = "", postalCode, items }) {
  const params = new URLSearchParams({
    RequestType: "ShippingOptions",
    State_Province: stateProvince,
    PostalCode: required(postalCode, "postalCode"),
    Country: required(country, "country"),
    ResponseType: "xml",
  });
  products(params, items);
  return `${ENDPOINT}?${params}`;
}

export function orderUrl({ credentials, recipient, shippingDescription, items, mode = "Test" }) {
  if (String(mode).toLowerCase() === "live" && process.env.KUNAKI_ALLOW_LIVE !== "1") {
    throw new Error("Live Kunaki orders require KUNAKI_ALLOW_LIVE=1");
  }
  const params = new URLSearchParams({
    RequestType: "Order",
    UserId: required(credentials?.userId, "credentials.userId"),
    Password: required(credentials?.password, "credentials.password"),
    Mode: mode,
    Name: required(recipient?.name, "recipient.name"),
    Company: recipient?.company || "",
    Address1: required(recipient?.address1, "recipient.address1"),
    Address2: recipient?.address2 || "",
    City: required(recipient?.city, "recipient.city"),
    State_Province: recipient?.stateProvince || "",
    PostalCode: required(recipient?.postalCode, "recipient.postalCode"),
    Country: required(recipient?.country, "recipient.country"),
    ShippingDescription: required(shippingDescription, "shippingDescription"),
    ResponseType: "xml",
  });
  products(params, items);
  return `${ENDPOINT}?${params}`;
}

export function statusUrl({ credentials, orderId }) {
  return `${ENDPOINT}?${new URLSearchParams({
    RequestType: "OrderStatus",
    UserId: required(credentials?.userId, "credentials.userId"),
    Password: required(credentials?.password, "credentials.password"),
    OrderId: required(orderId, "orderId"),
    ResponseType: "xml",
  })}`;
}

const entity = (value) => value.replace(/&amp;/g, "&").replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, '"').replace(/&#39;/g, "'");
const tags = (xml, name) => [...String(xml).matchAll(new RegExp(`<\\s*${name}\\s*>([\\s\\S]*?)<\\s*\\/\\s*${name}\\s*>`, "gi"))].map((m) => entity(m[1].trim()));

export function parseResponse(xml) {
  const errorCode = tags(xml, "ErrorCode")[0];
  const result = { errorCode: Number(errorCode), errorText: tags(xml, "ErrorText")[0] || "" };
  if (!Number.isFinite(result.errorCode)) throw new Error("Kunaki returned an unreadable response");
  if (result.errorCode !== 0) throw new Error(`Kunaki ${result.errorCode}: ${result.errorText}`);
  const descriptions = tags(xml, "Description");
  const times = tags(xml, "DeliveryTime");
  const prices = tags(xml, "Price");
  if (descriptions.length) result.options = descriptions.map((description, i) => ({ description, deliveryTime: times[i], priceUsd: Number(prices[i]) }));
  for (const name of ["OrderId", "OrderStatus", "TrackingType", "TrackingId"]) {
    const value = tags(xml, name)[0];
    if (value !== undefined) result[name[0].toLowerCase() + name.slice(1)] = value;
  }
  return result;
}

export async function request(url, fetchImpl = fetch) {
  const response = await fetchImpl(url, { headers: { accept: "application/xml,text/xml" } });
  if (!response.ok) throw new Error(`Kunaki HTTP ${response.status}`);
  return parseResponse(await response.text());
}
