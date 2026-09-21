// One bounded inference call. sync_mode keeps the image inline, with no URL fetch.
export async function generateSana({ prompt, key, signal, fetchImpl = fetch }) {
  if (!key) throw new Error("Sana is not configured");
  const response = await fetchImpl("https://fal.run/fal-ai/sana", {
    method: "POST",
    redirect: "error",
    headers: { Authorization: `Key ${key}`, "Content-Type": "application/json" },
    body: JSON.stringify({
      prompt, image_size: { width: 768, height: 768 }, num_images: 1,
      num_inference_steps: 18, sync_mode: true, output_format: "jpeg",
      enable_safety_checker: true,
    }),
    signal,
  });
  if (!response.ok) {
    await response.body?.cancel();
    throw Object.assign(new Error(`Sana HTTP ${response.status}`), {
      status: response.status, retryAfter: response.headers.get("Retry-After"),
    });
  }
  const result = await response.json();
  if (result?.has_nsfw_concepts?.some(Boolean))
    throw Object.assign(new Error("Sana filtered the image"), { reason: "filtered" });
  const image = result?.images?.[0];
  if (result?.images?.length !== 1 || image.width !== 768 || image.height !== 768 ||
      typeof image.url !== "string" || image.url.length > 12 * 1024 * 1024 ||
      !/^data:image\/jpeg;base64,[A-Za-z0-9+/]+={0,2}$/.test(image.url))
    throw Object.assign(new Error("Sana returned an invalid image"), { reason: "invalid_image" });
  const bytes = Buffer.from(image.url.slice(image.url.indexOf(",") + 1), "base64");
  if (bytes.length > 8 * 1024 * 1024 || bytes[0] !== 0xff || bytes[1] !== 0xd8 || bytes[2] !== 0xff)
    throw Object.assign(new Error("Sana returned invalid JPEG bytes"), { reason: "invalid_image" });
  return {
    png: image.url, width: 768, height: 768,
    seed: Number.isInteger(result.seed) ? result.seed : null,
  };
}
