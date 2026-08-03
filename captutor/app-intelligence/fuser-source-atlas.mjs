import { execFileSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve } from "node:path";

const SETTINGS = [
  ["profile", "account", "Profile", "always"],
  ["security", "account", "Security", "always"],
  ["editor", "account", "Editor", "always"],
  ["notifications", "account", "Notifications", "always"],
  ["api-keys", "account", "Providers", "personal-workspace-only"],
  ["billing", "account", "Plans & Billing", "personal-workspace-only"],
  ["members", "workspace", "Members", "team-workspace"],
  ["teams", "workspace", "Teams", "team-workspace"],
  ["access", "workspace", "Access", "team-admin"],
  ["workspace-settings", "workspace", "Settings", "team-workspace"],
  ["workspace-billing", "workspace", "Plans & Billing", "team-admin"],
  ["spend-limits", "workspace", "Spend Limits", "team-admin"],
  ["recovery", "workspace", "Recovery", "team-admin-with-active-flow"],
].map(([slug, group, label, visibility]) => ({ slug, group, label, visibility }));

const EDITOR_SETTINGS = [
  { key:"theme", label:"Theme", kind:"cycle", values:["light", "dark", "system"], persistedBy:"theme provider" },
  { key:"curatedNodes", label:"Curated Nodes", kind:"boolean", default:true },
  { key:"zoomWhenCommandbarAdd", label:"Focus on Node on Add (Search)", kind:"boolean", default:false },
  { key:"zoomWhenToolbarAdd", label:"Focus on Node on Add (Toolbar)", kind:"boolean", default:false },
  { key:"selectionMode", label:"Selection Mode", kind:"choice", default:"partial", values:["full", "partial"] },
  { key:"nodeCreditEstimateDisplay", label:"Generation Estimate Display", kind:"choice", default:"corner", values:["above", "corner", "disabled"] },
  { key:"showMiniMap", label:"Show Mini Map", kind:"boolean", default:false },
  { key:"playSounds", label:"Play Sounds", kind:"boolean", default:true },
  { key:"coloredEdges", label:"Colored Edges", kind:"boolean", default:false },
];

const EDITOR_BEHAVIORS = [
  {
    id:"native-annotation-tour", available:true,
    contract:"A node annotation beginning with @tour N Title becomes an ordered anchored step; Play tour focuses each node and restores the prior viewport.",
    evidence:["packages/flow/src/overlays/GenerateTourButton.tsx", "packages/flow/src/neoonboarding/engine/parseAnnotationTour.ts"],
  },
  {
    id:"fit-view", available:true,
    contract:"React Flow fitView frames all nodes or a requested node with animated padding and a maximum zoom.",
    evidence:["packages/flow/src/overlays/NavigationOverlay.tsx", "packages/flow/src/hooks/useFlowZoom.ts"],
  },
  {
    id:"snap-to-grid", available:true,
    contract:"The project canvas passes a persisted snapToGrid view option into React Flow.",
    evidence:["packages/flow/src/store/AppStore.ts", "packages/flow/src/components/FlowCanvas.tsx"],
  },
  {
    id:"selection-box", available:true,
    contract:"Editor Settings switches the selection box between full containment and partial intersection.",
    evidence:["packages/flow/src/store/SettingsStore.ts", "packages/flow/src/components/FlowCanvas.tsx"],
  },
  {
    id:"connect-disconnect", available:true,
    contract:"Connection drags are validated before edge creation; removing a selected edge dispatches the persisted edge deletion path.",
    evidence:["packages/flow/src/components/FlowCanvas.tsx", "packages/flow/src/store/FlowStore.tsx"],
  },
  {
    id:"groups", available:true,
    contract:"Selected nodes can be grouped and retain synchronized parent-relative positions.",
    evidence:["packages/flow/src/hooks/useFlowCanvasGrouping.ts", "packages/flow/src/hooks/useGroupMembershipSync.ts"],
  },
  {
    id:"automatic-graph-layout", available:false,
    contract:"No user-facing Dagre, ELK, arrange, align, distribute, or pack command is registered in the project editor source. Neat graph packing is an app-intelligence automation opportunity, not a current native editor feature.",
    evidence:["source audit: packages/flow/src"],
  },
];

function revision(source) {
  try {
    return execFileSync("git", ["-C", source, "rev-parse", "HEAD"], { encoding:"utf8" }).trim();
  } catch { return null; }
}
export function buildFuserSourceAtlas(sourceRoot = join(homedir(), "Developer", "fuser")) {
  const source = resolve(sourceRoot);
  const manifestPath = join(source, "packages/node-manifest/data/manifest.generated.json");
  if (!existsSync(manifestPath)) throw new Error(`missing Fuser node manifest: ${manifestPath}`);
  const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
  if (!Array.isArray(manifest.nodes)) throw new Error("Fuser node manifest has no nodes array");

  const nodes = manifest.nodes.map((node) => ({
    type:node.key,
    name:node.meta?.name || node.key,
    category:node.meta?.category || "other",
    description:node.meta?.description || "",
    inputs:(node.inputs || []).map(({ id, label, kind, isArray, required, section }) => ({ id, label, kind, isArray, required, section })),
    outputs:(node.outputs || []).map(({ id, label, type }) => ({ id, label, type })),
    sections:(node.sections || []).map(({ key, label, description }) => ({ key, label, description })),
    billable:Array.isArray(node.endpoints) && node.endpoints.some((endpoint) => Number(endpoint.cost?.max || 0) > 0),
    source:node.source?.file || null,
  })).sort((a, b) => a.category.localeCompare(b.category) || a.name.localeCompare(b.name));

  const categories = Object.entries(nodes.reduce((counts, node) => {
    counts[node.category] = (counts[node.category] || 0) + 1;
    return counts;
  }, {})).map(([category, count]) => ({ category, count }));

  return {
    schema:"captutor-fuser-source-atlas/v1",
    generatedAt:new Date().toISOString(),
    source:{ root:source, revision:revision(source), manifest:"packages/node-manifest/data/manifest.generated.json" },
    coverage:{ registeredNodes:nodes.length, categories, settingsTabs:SETTINGS.length, editorSettings:EDITOR_SETTINGS.length },
    nodes,
    settings:{ tabs:SETTINGS, editor:EDITOR_SETTINGS },
    editorBehaviors:EDITOR_BEHAVIORS,
    missionContract:{
      nodeTour:"Visit every manifest node exactly once, grouped by category; never execute billable nodes.",
      unavailable:"Record gated or absent live entries explicitly instead of substituting a fuzzy match.",
      checkpoint:"Persist per-node pass, unavailable, or failed status so a later batch resumes without replaying accepted visits.",
    },
  };
}

export function compactFuserSourceAtlas(atlas) {
  return {
    schema:atlas.schema, generatedAt:atlas.generatedAt, source:atlas.source, coverage:atlas.coverage,
    nodes:atlas.nodes.map(({ type, name, category, inputs, outputs, billable, source }) => ({
      type, name, category, inputs:inputs.map(({ id, label, kind }) => ({ id, label, kind })),
      outputs:outputs.map(({ id, label, type:outputType }) => ({ id, label, type:outputType })),
      billable, source,
    })),
    settings:atlas.settings, editorBehaviors:atlas.editorBehaviors, missionContract:atlas.missionContract,
  };
}
