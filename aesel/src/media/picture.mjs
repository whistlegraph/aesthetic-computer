import {
  readFile,
  writeFile,
  mkdir,
  realpath,
  lstat,
  rename,
} from "node:fs/promises";
import { resolve, relative, dirname, join, isAbsolute } from "node:path";
import { createHash } from "node:crypto";
import {
  encode,
  decode,
  dimensions,
  composite,
  blend,
} from "../../media/picture/png.mjs";
import { nopaintProposal, seededRandom } from "../../media/picture/ac-line.mjs";
import { drawPicture } from "../../media/picture/draw.mjs";
import { plan, generate } from "../../media/picture/illy.mjs";
export const kind = "picture";
const schema = (properties = {}, required = []) => ({
  type: "object",
  properties,
  required,
  additionalProperties: false,
});
const text = { type: "string" },
  num = { type: "number" },
  color = {
    type: "array",
    items: { type: "integer", minimum: 0, maximum: 255 },
    minItems: 4,
    maxItems: 4,
  };
export const actions = [
  { name: "draw", description: "Use AC tools: line, flood fill at a point, filled/outlined box or circle, wipe, invert, vertical flip, blur. Coordinates are picture pixels. Creates a preview; accept commits it. Fill colors the connected region, not an approximation with strokes.", inputSchema: schema({ tool: {enum:["line","fill","box","circle","wipe","invert","flip","blur"]}, color, x:num, y:num, width:num, height:num, radius:num, thickness:num, filled:{type:"boolean"}, points:{type:"array",items:schema({x:num,y:num},["x","y"])} }, ["tool"]) },
  {
    name: "propose",
    description:
      "Preview an AC Line brush proposal; accepted layers stay unchanged until accept.",
    inputSchema: schema(
      {
        seed: text,
        color,
        thickness: { type: "integer", minimum: 1, maximum: 50 },
        points: {
          type: "array",
          minItems: 2,
          maxItems: 256,
          items: schema({ x: num, y: num }, ["x", "y"]),
        },
      },
      ["seed"],
    ),
  },
  {
    name: "accept",
    description: "Accept the current brush or image proposal as a new layer.",
    inputSchema: schema(),
  },
  {
    name: "discard",
    description:
      "Discard the pending proposal, preserving the accepted painting.",
    inputSchema: schema(),
  },
  {
    name: "import_png",
    description:
      "Propose an existing PNG inside this artifact directory as a layer. RGB/RGBA, 8 bit, noninterlaced, maximum 2048 pixels.",
    inputSchema: schema({ path: text }, ["path"]),
  },
  {
    name: "layer",
    description: "Set an accepted layer visibility or opacity.",
    inputSchema: schema(
      {
        id: text,
        visible: { type: "boolean" },
        opacity: { type: "number", minimum: 0, maximum: 1 },
      },
      ["id"],
    ),
  },
  {
    name: "export",
    description:
      "Write accepted layers as composite.png, excluding any pending proposal.",
    inputSchema: schema(),
  },
  {
    name: "image_plan",
    description:
      "Describe an explicit Illy provider/model image request. No paid request is made.",
    inputSchema: schema(
      {
        provider: { enum: ["openai", "fal"] },
        model: text,
        prompt: text,
        size: text,
        quality: text,
        reference: text,
      },
      ["provider", "model", "prompt"],
    ),
  },
  {
    name: "generate",
    description:
      "Generate or edit an image with OpenAI or fal when the user asks for remote AI image tools. For an edit use reference: composite.png. Prefer draw for AC drawing/fill. Result becomes a preview; accept commits it. OpenAI and fal GPT Image 2.5 Flare generation can use AC sign-in; other fal models need FAL_KEY. Explicit local provider keys take precedence.",
    inputSchema: schema(
      {
        provider: { enum: ["openai", "fal"] },
        model: text,
        prompt: text,
        size: text,
        quality: text,
        reference: text,
      },
      ["provider", "model", "prompt"],
    ),
  },
];
async function safe(root, path) {
  if (typeof path !== "string" || !path || isAbsolute(path))
    throw new Error("Use a relative artifact path.");
  const base = await realpath(root),
    target = resolve(base, path),
    rel = relative(base, target);
  if (rel === ".." || rel.startsWith("../"))
    throw new Error("Path escapes artifact.");
  let cursor = target;
  while (cursor !== base) {
    try {
      if ((await lstat(cursor)).isSymbolicLink())
        throw new Error("Symlinks are not allowed in artifact paths.");
    } catch (e) {
      if (e.code !== "ENOENT") throw e;
    }
    cursor = dirname(cursor);
  }
  return target;
}
async function put(root, path, bytes) {
  const p = await safe(root, path);
  await mkdir(dirname(p), { recursive: true });
  const temp = p + ".pending";
  await safe(root, path + ".pending");
  await writeFile(temp, bytes);
  await rename(temp, p);
}
async function get(root, path) {
  return readFile(await safe(root, path));
}
function blank(w, h, color = [0, 0, 0, 0]) {
  dimensions(w, h);
  const data = Buffer.alloc(w * h * 4);
  for (let i = 0; i < data.length; i += 4) data.set(color, i);
  return { width: w, height: h, data };
}
function raster(score, w, h) {
  return drawPicture(blank(w, h), "line", { points: score.points, color: score.color, thickness: score.thickness });
}

async function render(root, state, pending) {
  const out = blank(state.width, state.height, state.background);
  for (const l of [
    ...state.layers,
    ...(pending && state.proposal ? [state.proposal] : []),
  ])
    if (l.visible !== false) {
      if (l.mode === "composite" && (l.opacity ?? 1) === 1) out.data.fill(0);
      composite(out, decode(await get(root, l.path)), l.opacity ?? 1);
    }
  return encode(out);
}
async function save(root, state, summary) {
  const preview = await render(root, state, true),
    accepted = await render(root, state, false);
  await put(root, "preview.png", preview);
  await put(root, "composite.png", accepted);
  await put(root, "picture.json", JSON.stringify(state, null, 2) + "\n");
  const paths = [
    "picture.json",
    "preview.png",
    "composite.png",
    ...state.layers.map((l) => l.path),
    ...(state.proposal ? [state.proposal.path] : []),
    ...(state.provenance || []),
  ];
  return {
    files: [...new Set(paths)],
    preview: { path: "preview.png", mime: "image/png" },
    summary,
  };
}
export async function create({ root, name }) {
  await mkdir(root, { recursive: true });
  try {
    await get(root, "picture.json");
    throw new Error("Picture already exists.");
  } catch (e) {
    if (e.code !== "ENOENT") throw e;
  }
  return save(
    root,
    {
      format: 1,
      name: name || "Untitled picture",
      width: 512,
      height: 512,
      background: [255, 255, 255, 255],
      layers: [],
      proposal: null,
      provenance: [],
    },
    "Blank 512 × 512 picture.",
  );
}
export async function run({ root, action, input = {} }) {
  const state = JSON.parse(await get(root, "picture.json"));
  dimensions(state.width, state.height);
  if (action === "image_plan") {
    const p = plan(input);
    return {
      ...(await save(root, state, "Image plan only; no charge.")),
      plan: p,
    };
  }
  if (action === "draw") {
    const accepted = decode(await render(root, state, false));
    const bytes = encode(drawPicture(accepted, input.tool, input));
    const hash = createHash("sha256").update(bytes).digest("hex");
    const path = `assets/${hash}.png`;
    await put(root, path, bytes);
    state.proposal = { id: hash.slice(0,16), path, visible:true, opacity:1, brush:input.tool, mode:"composite", score: input };
  } else if (action === "propose") {
    if (typeof input.seed !== "string" || !input.seed.length)
      throw new Error("A seed is required.");
    const random = seededRandom(input.seed),
      w = state.width,
      h = state.height;
    let score = nopaintProposal.generate({
      random,
      width: w,
      height: h,
      base: {
        x: Math.floor((random() * w) / 2),
        y: Math.floor((random() * h) / 2),
        w: w / 2,
        h: h / 2,
        drift: w / 8,
        color: [
          Math.floor(random() * 256),
          Math.floor(random() * 256),
          Math.floor(random() * 256),
          128,
        ],
      },
    });
    score = structuredClone(score);
    if (input.color !== undefined) {
      if (
        !Array.isArray(input.color) ||
        input.color.length !== 4 ||
        !input.color.every((n) => Number.isInteger(n) && n >= 0 && n <= 255)
      )
        throw new Error("Color must be RGBA bytes.");
      score.color = input.color;
      score.brush.params = input.color.map(String);
    }
    if (input.thickness !== undefined) {
      if (
        !Number.isInteger(input.thickness) ||
        input.thickness < 1 ||
        input.thickness > 50
      )
        throw new Error("Thickness must be 1–50.");
      score.thickness = input.thickness;
      score.brush.colon = [String(input.thickness)];
    }
    if (input.points !== undefined) {
      if (
        !Array.isArray(input.points) ||
        input.points.length < 2 ||
        input.points.length > 256 ||
        !input.points.every(
          (p) =>
            Number.isFinite(p.x) &&
            Number.isFinite(p.y) &&
            p.x >= 0 &&
            p.y >= 0 &&
            p.x < w &&
            p.y < h,
        )
      )
        throw new Error("Points must lie within the picture.");
      score.points = input.points;
    }
    score.brush.parameters = {
      ...score.brush.parameters,
      thickness: score.thickness,
      alpha: score.color[3],
      pointCount: score.points.length,
    };
    const bytes = encode(raster(score, w, h)),
      hash = createHash("sha256").update(bytes).digest("hex"),
      path = `assets/${hash}.png`;
    await put(root, path, bytes);
    state.proposal = {
      id: hash.slice(0, 16),
      path,
      visible: true,
      opacity: 1,
      brush: "line",
      seed: input.seed,
      score,
    };
  } else if (action === "import_png" || action === "generate") {
    let bytes, provenance;
    if (action === "import_png") bytes = await get(root, input.path);
    else {
      const p = plan(input);
      if (input.authorized !== true)
        throw new Error(
          "Explicit user authorization is required before a paid image request.",
        );
      if (!/^[a-zA-Z0-9_-]{1,80}$/.test(input.jobId || ""))
        throw new Error(
          "Provide a unique jobId (letters, numbers, dash, underscore).",
        );
      const reference = input.reference
        ? await get(root, input.reference)
        : undefined;
      if (reference) decode(reference);
      const jobPath = `jobs/${input.jobId}.json`;
      const job = await safe(root, jobPath);
      await mkdir(dirname(job), { recursive: true });
      try {
        await writeFile(
          job,
          JSON.stringify({
            status: "submitted",
            plan: p,
            at: new Date().toISOString(),
          }),
          { flag: "wx" },
        );
      } catch (e) {
        if (e.code === "EEXIST")
          throw new Error(
            "This job already exists. Inspect its receipt; do not resubmit a potentially charged request.",
          );
        throw e;
      }
      try {
        const imageOptions = {
          reference,
          onSubmitted: async (receipt) =>
            put(
              root,
              jobPath,
              JSON.stringify({ status: "queued", plan: p, ...receipt }),
            ),
        };
        const result = process.env[p.provider === 'openai' ? 'OPENAI_API_KEY' : 'FAL_KEY']
          ? await generate(p, imageOptions)
          : await (await import('../picture-image.mjs')).hostedPictureImage(p, {reference,jobId:input.jobId});
        bytes = result.bytes;
        await put(root, `jobs/${input.jobId}.png`, bytes);
        provenance = {
          ...result.provenance,
          references: input.reference ? [input.reference] : [],
        };
        await put(
          root,
          jobPath,
          JSON.stringify({ status: "complete", provenance }),
        );
      } catch (e) {
        await put(
          root,
          jobPath,
          JSON.stringify({
            status: "failed-or-unknown",
            plan: p,
            message: String(e.message),
          }),
        );
        throw e;
      }
    }
    decode(bytes);
    const hash = createHash("sha256").update(bytes).digest("hex"),
      path = `assets/${hash}.png`;
    await put(root, path, bytes);
    state.proposal = { id: hash.slice(0, 16), path, visible: true, opacity: 1 };
    if (provenance?.mode === "edit") state.proposal.mode = "composite";
    if (provenance) {
      await put(root, path + ".illy.json", JSON.stringify(provenance, null, 2));
      state.provenance.push(path + ".illy.json");
    }
  } else if (action === "accept") {
    if (!state.proposal) throw new Error("No pending proposal.");
    state.layers.push({
      ...state.proposal,
      id: `layer-${state.layers.length + 1}-${state.proposal.id}`,
    });
    state.proposal = null;
  } else if (action === "discard") state.proposal = null;
  else if (action === "layer") {
    const layer = state.layers.find((l) => l.id === input.id);
    if (!layer) throw new Error("Layer not found.");
    if (input.visible !== undefined) {
      if (typeof input.visible !== "boolean")
        throw new Error("visible must be boolean.");
      layer.visible = input.visible;
    }
    if (input.opacity !== undefined) {
      if (
        !Number.isFinite(input.opacity) ||
        input.opacity < 0 ||
        input.opacity > 1
      )
        throw new Error("Opacity must be 0–1.");
      layer.opacity = input.opacity;
    }
  } else if (action !== "export")
    throw new Error(`Unknown picture action: ${action}`);
  return save(
    root,
    state,
    action === "export"
      ? "Exported accepted painting to composite.png."
      : `${action}: ${state.layers.length} accepted layers${state.proposal ? ", proposal awaiting accept" : ""}.`,
  );
}
