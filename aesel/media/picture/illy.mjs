// Portable Illy adapter. Deliberately no studio vault or implicit provider fallback.
import { createHash } from "node:crypto";
// Schema: https://fal.ai/models/openai/gpt-image-2.5/flare/text-to-image/api
export const FAL_FLARE = "openai/gpt-image-2.5/flare/text-to-image";
export const models = Object.freeze({
  openai: ["gpt-image-2"],
  fal: ["fal-ai/flux/dev", "fal-ai/flux/dev/image-to-image", FAL_FLARE],
});
export function plan(input) {
  const { provider, model, prompt } = input;
  if (!models[provider]?.includes(model))
    throw new Error("Select an explicit supported image provider/model.");
  if (typeof prompt !== "string" || !prompt.trim() || prompt.length > 16000)
    throw new Error("An image prompt is required (maximum 16000 characters).");
  const size = input.size || "1024x1024";
  if (!["1024x1024", "1024x1536", "1536x1024"].includes(size))
    throw new Error("Unsupported image size.");
  const quality = input.quality || "medium";
  if (!["low", "medium", "high"].includes(quality))
    throw new Error("Unsupported quality.");
  const edit = !!input.reference;
  if (
    provider === "fal" &&
    edit !== (model === "fal-ai/flux/dev/image-to-image")
  )
    throw new Error("Choose the fal model matching generate or edit.");
  return {
    provider,
    model,
    prompt,
    size,
    quality,
    mode: edit ? "edit" : "generate",
    promptHash: createHash("sha256").update(prompt).digest("hex"),
    stages: ["generate", "validate", "propose"],
  };
}
export async function generate(
  plan,
  {
    reference,
    env = process.env,
    fetchImpl = fetch,
    signal = AbortSignal.timeout(600000),
    onSubmitted = async () => {},
  } = {},
) {
  const key = env[plan.provider === "openai" ? "OPENAI_API_KEY" : "FAL_KEY"];
  if (!key)
    throw new Error(
      `Set ${plan.provider === "openai" ? "OPENAI_API_KEY" : "FAL_KEY"} outside the project to use this provider.`,
    );
  const checked = async (url, options = {}) => {
    const r = await fetchImpl(url, { ...options, signal, redirect: "error" });
    if (!r.ok)
      throw new Error(
        `Image provider HTTP ${r.status}; request was not retried.`,
      );
    return r;
  };
  // Never automatically retry a paid POST: a network failure may have been charged.
  let result, requestId, bytes;
  if (plan.provider === "openai") {
    let body,
      headers = { Authorization: `Bearer ${key}` };
    if (reference) {
      body = new FormData();
      for (const [k, v] of Object.entries({
        model: plan.model,
        prompt: plan.prompt,
        size: plan.size,
        quality: plan.quality,
        n: "1",
      }))
        body.append(k, v);
      body.append(
        "image[]",
        new Blob([reference], { type: "image/png" }),
        "reference.png",
      );
    } else {
      headers["Content-Type"] = "application/json";
      body = JSON.stringify({
        model: plan.model,
        prompt: plan.prompt,
        size: plan.size,
        quality: plan.quality,
        n: 1,
        output_format: "png",
      });
    }
    const response = await checked(
      `https://api.openai.com/v1/images/${reference ? "edits" : "generations"}`,
      { method: "POST", headers, body },
    );
    result = await response.json();
    requestId = response.headers?.get("x-request-id") || result.id || null;
    if (result.data?.[0]?.b64_json)
      bytes = Buffer.from(result.data[0].b64_json, "base64");
    else throw new Error("Image provider returned no PNG bytes.");
  } else {
    const [width, height] = plan.size.split("x").map(Number),
      headers = {
        Authorization: `Key ${key}`,
        "Content-Type": "application/json",
      };
    const input = {
      prompt: plan.prompt,
      image_size: { width, height },
      num_images: 1,
      output_format: "png",
    };
    if (plan.model === FAL_FLARE) input.quality = plan.quality;
    if (reference)
      input.image_url = `data:image/png;base64,${reference.toString("base64")}`;
    const queued = await (
      await checked(`https://queue.fal.run/${plan.model}`, {
        method: "POST",
        headers,
        body: JSON.stringify(input),
      })
    ).json();
    requestId = queued.request_id;
    const queueURL = (value) => {
      const u = new URL(value);
      if (u.protocol !== "https:" || u.hostname !== "queue.fal.run")
        throw new Error("Unexpected fal queue URL.");
      return u.href;
    };
    await onSubmitted({
      requestId,
      statusURL: queueURL(queued.status_url),
      responseURL: queueURL(queued.response_url),
    });
    for (;;) {
      signal.throwIfAborted();
      const status = await (
        await checked(queueURL(queued.status_url), { headers })
      ).json();
      if (status.status === "COMPLETED") break;
      if (status.status === "FAILED" || status.error)
        throw new Error("fal generation failed.");
      await new Promise((r) => setTimeout(r, 1000));
    }
    result = await (
      await checked(queueURL(queued.response_url), { headers })
    ).json();
    const url = new URL(result.images?.[0]?.url);
    if (url.protocol !== "https:" || !/(^|\.)fal\.media$/.test(url.hostname))
      throw new Error("Unexpected fal image host.");
    bytes = Buffer.from(await (await checked(url)).arrayBuffer());
  }
  if (bytes.length > 32 * 1024 * 1024) throw new Error("Image exceeds 32 MB.");
  return {
    bytes,
    provenance: {
      ...plan,
      requestId,
      usage: result.usage || null,
      completedAt: new Date().toISOString(),
    },
  };
}
