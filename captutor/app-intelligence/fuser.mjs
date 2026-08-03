import {
  confidenceFromChecks, defineAppIntelligence, localizedConcepts,
} from "../lib/app-intelligence.mjs";
import { buildFuserSourceAtlas } from "./fuser-source-atlas.mjs";

export const FUSER_SELECTORS = Object.freeze({
  canvas: ".react-flow",
  pane: ".react-flow__pane",
  viewport: ".react-flow__viewport",
  settingsDialog: '[role="dialog"][aria-label="Settings"]',
  settingsNav: 'nav[aria-label="Settings"]',
  editorSettingsAnchor: '[data-neo-anchor="editor-theme-toggle"]',
  imageNode: ".react-flow__node-ImageNode",
  // The handle id is a per-node runtime UUID in current Fuser source. Scope by
  // node type + side + React Flow direction, then prove the node contract.
  imageInput: ".react-flow__node-ImageNode .react-flow__handle-left.target",
  // ImageNode renders an outgoing passthrough for its input socket. The model
  // also owns `outputs.imageOut`, but there is no `imageOut` canvas handle.
  imagePassthrough: ".react-flow__node-ImageNode .react-flow__handle-right.source",
});

export const FUSER_SOURCE_CHECKS = Object.freeze([
  {
    id:"image-node-contract",
    path:"packages/flow/src/nodes/ImageNode.tsx",
    contains:["type: 'ImageNode'", "socketKey=\"image\"", "outputs?.imageOut?.data"],
  },
  {
    id:"image-data-contract",
    path:"packages/flow/src/nodes/ImageNode.tool.ts",
    contains:["inputs:", "image:", "outputs:", "imageOut:"],
  },
  {
    id:"visible-passthrough-contract",
    path:"packages/flow/src/components/NodeSection.tsx",
    contains:["const isPassthrough = type === 'source' && socket.type === 'target'", "<SocketOutput socket={socket}"],
  },
  {
    id:"runtime-handle-identity",
    path:"packages/flow/src/types/NodeTypes.tsx",
    contains:["uuid6()"],
  },
  {
    id:"editor-gesture-contract",
    path:"packages/flow/src/components/FlowCanvas.tsx",
    contains:["nodeClickDistance={4}", "nodeDragThreshold={4}", "onConnectStart={onConnectStart}"],
  },
  {
    id:"settings-registry-contract",
    path:"packages/flow/src/settings/settingsRegistry.ts",
    contains:["SETTINGS_REGISTRY", "slug: 'editor'", "slug: 'recovery'", "resolveTab"],
  },
  {
    id:"native-tour-contract",
    path:"packages/flow/src/overlays/GenerateTourButton.tsx",
    contains:["parseAnnotationTour", "fitView", "Play tour", "setViewport(savedViewport"],
  },
  {
    id:"editor-settings-contract",
    path:"packages/flow/src/store/SettingsStore.ts",
    contains:["curatedNodes", "selectionMode", "showMiniMap", "coloredEdges"],
  },
]);

export const FUSER_INTELLIGENCE = defineAppIntelligence({
  schema:"captutor-app-intelligence/v1",
  id:"fuser-editor",
  hostMatch:"fuser.studio",
  source:{
    repository:"fuser",
    branch:"staging",
    verifiedRevision:"1d871cf0b1e94c12f6e2efa0789ddc7da38d33d8",
    verifiedAt:"2026-07-29",
  },
  glossary:{
    canvas:{
      term:"canvas",
      meaning:"The infinite React Flow workspace containing nodes and edges.",
      evidence:["packages/flow/src/components/FlowCanvas.tsx:2015-2140"],
    },
    node:{
      term:"node",
      meaning:"A movable, selectable unit with its own inputs, outputs, and controls.",
      evidence:["packages/flow/src/components/FlowCanvas.tsx:2032-2039"],
    },
    connectionPoint:{
      term:"connection point",
      aliases:["port", "socket", "React Flow handle"],
      meaning:"The visible endpoint where a connection starts or lands.",
      evidence:["packages/flow/src/components/NodeSection.tsx:126-215"],
    },
    interactionZone:{
      term:"interaction zone",
      meaning:"The padded wrapper around a connection point that owns hover feedback and its delayed tooltip.",
      evidence:["packages/flow/src/components/NodeSection.tsx:219-355"],
    },
    passthrough:{
      term:"input passthrough",
      meaning:"A source-side connection point generated from an input socket so the same value can continue downstream.",
      evidence:["packages/flow/src/components/NodeSection.tsx:144-215", "packages/flow/src/components/NodeSection.tsx:288-355"],
    },
    settings:{
      term:"settings",
      meaning:"A route-backed modal whose visible account and workspace tabs depend on the active workspace and permissions.",
      evidence:["packages/flow/src/settings/settingsRegistry.ts", "packages/flow/src/hooks/useSettings.ts"],
    },
    annotationTour:{
      term:"annotation tour",
      meaning:"An ordered native walkthrough generated from @tour markers at the beginning of node annotations.",
      evidence:["packages/flow/src/neoonboarding/engine/parseAnnotationTour.ts", "packages/flow/src/overlays/GenerateTourButton.tsx"],
    },
  },
  concepts:{
    canvas:{ kind:"surface", selector:FUSER_SELECTORS.canvas, label:"Canvas" },
    pane:{ kind:"surface", selector:FUSER_SELECTORS.pane, label:"Canvas pane" },
    imageNode:{
      kind:"node", selector:FUSER_SELECTORS.imageNode,
      labelKey:"flow.nodes.ImageNode.name",
      intent:"Store and view image content in a workflow.",
      evidence:[
        "packages/flow/src/nodes/ImageNode.tsx:24-109",
        "apps/docs/content/docs/nodes/primitive/image.mdx:1-26",
      ],
    },
    imageInput:{
      kind:"input", selector:FUSER_SELECTORS.imageInput, socketKey:"image",
      selectorStrategy:"framework-contract", expect:{ count:1, nodeType:"ImageNode", side:"left", direction:"target" },
      labelKey:"flow.nodes.ImageNode.inputs.image.label",
      intent:"Accept image content into the Image node.",
      evidence:["packages/flow/src/nodes/ImageNode.tool.ts:17-29"],
    },
    imagePassthrough:{
      kind:"output-passthrough", selector:FUSER_SELECTORS.imagePassthrough,
      socketKey:"image", modelOutputKey:"imageOut",
      selectorStrategy:"framework-contract", expect:{ count:1, nodeType:"ImageNode", side:"right", direction:"source" },
      labelKey:"flow.nodes.ImageNode.inputs.image.label",
      modelLabelKey:"flow.nodes.ImageNode.outputs.imageOut.label",
      intent:"Pass the Image input onward from the right side of the node.",
      teaching:{
        visibleLabel:"Use the localized Image input label.",
        modelDistinction:"Image Out is an underlying output field, not this visible handle id or tooltip label.",
        avoid:["Reusable image output", "labeling the visible port Image Out"],
      },
      evidence:[
        "packages/flow/src/nodes/ImageNode.tsx:50-63",
        "packages/flow/src/nodes/ImageNode.tsx:99-105",
        "packages/flow/src/components/NodeSection.tsx:397-403",
        "packages/flow/src/nodes/ImageNode.tool.ts:31-44",
        "packages/flow/src/types/NodeTypes.tsx:195-221",
      ],
    },
    settingsDialog:{
      kind:"dialog", selector:FUSER_SELECTORS.settingsDialog, label:"Settings",
      intent:"Configure account, editor, provider, billing, and permission-dependent workspace behavior.",
      evidence:["packages/flow/src/settings/SettingsModal.tsx", "packages/flow/src/settings/settingsRegistry.ts"],
    },
    editorSettings:{
      kind:"settings-panel", selector:FUSER_SELECTORS.editorSettingsAnchor, label:"Editor Settings",
      intent:"Control node discovery, add-node focus, selection, generation estimates, minimap, sounds, and edge color.",
      evidence:["packages/flow/src/settings/EditorSettings.tsx", "packages/flow/src/store/SettingsStore.ts"],
    },
  },
  behaviors:{
    exactNodeChoice:{
      intent:"Choose the requested node, not the first fuzzy search result.",
      gesture:"Click the unique picker option whose accessible name equals the localized node name.",
      verifies:["selected node type matches the requested React Flow node class"],
    },
    selectNode:{
      intent:"Make one node the active editor object.",
      gesture:"Click the node body with no more than four pixels of pointer drift.",
      effect:"The node gains React Flow's selected state.",
      evidence:["packages/flow/src/components/FlowCanvas.tsx:2038-2039", "packages/flow/src/components/FlowCanvas.tsx:2083-2096"],
    },
    moveNode:{
      intent:"Reposition a node without operating one of its controls or ports.",
      gesture:"Drag the node body beyond the four-pixel drag threshold.",
      effect:"The node position changes and the drag is persisted as one operation.",
      evidence:["packages/flow/src/components/FlowCanvas.tsx:1483-1524", "packages/flow/src/components/FlowCanvas.tsx:2083-2096"],
    },
    inspectPort:{
      intent:"Reveal what a connection point accepts or emits before connecting it.",
      gesture:"Hover its padded interaction zone for one second.",
      effect:"The point grows and its tooltip appears when no connection is in progress.",
      evidence:["packages/flow/src/components/NodeSection.tsx:219-355"],
    },
    connectPorts:{
      intent:"Route compatible data between nodes.",
      gesture:"Drag from a source connection point to a compatible target connection point.",
      effect:"Fuser validates the pair and creates an edge when editing is enabled.",
      evidence:["packages/flow/src/components/FlowCanvas.tsx:2026-2051"],
    },
    navigateCanvas:{
      intent:"Move around the workflow without moving a node.",
      gesture:"Drag or scroll the empty pane; pinch to zoom. Use the zoom menu for an exact scale.",
      evidence:["packages/flow/src/components/FlowCanvas.tsx:2117-2138"],
    },
    disconnectEdge:{
      intent:"Remove a route without deleting either endpoint node.",
      gesture:"Select the edge, then use the editor deletion command.",
      effect:"Fuser dispatches a persisted edge removal while both nodes remain.",
      evidence:["packages/flow/src/components/FlowCanvas.tsx:1351-1404", "packages/flow/src/store/AppStore.ts:91-101"],
    },
    arrangeNodes:{
      intent:"Make a multi-node flow readable without changing its data graph.",
      gesture:"Select and drag node bodies; use snap-to-grid when desired, then Fit View.",
      effect:"Only layout coordinates and viewport change.",
      nativeAutoLayout:false,
      note:"The source exposes snap, grouping, node movement, and fitView, but no project-editor Dagre/ELK/pack/align/distribute command.",
      evidence:["packages/flow/src/store/AppStore.ts:41-62", "packages/flow/src/components/FlowCanvas.tsx:1244-1344", "packages/flow/src/overlays/NavigationOverlay.tsx:350-461"],
    },
    playAnnotationTour:{
      intent:"Present a workflow in an author-defined order.",
      gesture:"Start each node annotation with @tour N Title, then press Play tour.",
      effect:"Fuser animates focus from node to node and restores the saved viewport at the end.",
      evidence:["packages/flow/src/neoonboarding/engine/parseAnnotationTour.ts", "packages/flow/src/overlays/GenerateTourButton.tsx"],
    },
  },
});

export function fuserNodePickerResult(name) {
  return `[role="option"][aria-label=${JSON.stringify(name)}]`;
}

export function fuserEditor(t) {
  const concepts = localizedConcepts(FUSER_INTELLIGENCE, t);
  concepts.imagePassthrough.modelLabel = t(concepts.imagePassthrough.modelLabelKey);
  return {
    id:FUSER_INTELLIGENCE.id,
    glossary:FUSER_INTELLIGENCE.glossary,
    behaviors:FUSER_INTELLIGENCE.behaviors,
    source:FUSER_INTELLIGENCE.source,
    selectors:FUSER_SELECTORS,
    concepts,
    image:{
      node:concepts.imageNode,
      input:concepts.imageInput,
      passthrough:concepts.imagePassthrough,
    },
  };
}

function semanticPort(handle, nodeType, labels) {
  const isImage = nodeType === "ImageNode" &&
    ((handle.kind === "target" && (!handle.side || handle.side === "left")) ||
      (handle.kind === "source" && (!handle.side || handle.side === "right")));
  if (!isImage) return {
    ...handle,
    role:handle.kind === "target" ? "input" : "output",
    label:handle.ariaLabel || null,
  };
  if (handle.kind === "target") return {
    ...handle,
    runtimeHandleId:handle.id,
    role:"input",
    key:"image",
    label:labels.image,
    intent:"Accept image content into the node.",
  };
  return {
    ...handle,
    runtimeHandleId:handle.id,
    role:"input-passthrough",
    key:"image",
    label:labels.image,
    modelOutput:{ key:"imageOut", label:labels.imageOut },
    intent:"Pass the same Image value onward.",
    teaching:"Call the visible connection point Image; reserve Image Out for the data model.",
  };
}

export function interpretFuserFrame(dom, genericFrame, { locale, t, atlas = null }) {
  const labels = {
    image:t("flow.nodes.ImageNode.inputs.image.label"),
    imageOut:t("flow.nodes.ImageNode.outputs.imageOut.label"),
  };
  const catalog = new Map((atlas?.nodes || []).map((node) => [node.type, node]));
  const nodes = (dom.nodes || []).map((node) => {
    const sourceNode = catalog.get(node.type);
    return {
      ...node,
      ...(sourceNode ? { catalog:{
        name:sourceNode.name, category:sourceNode.category, description:sourceNode.description,
        billable:sourceNode.billable, source:sourceNode.source,
        inputs:sourceNode.inputs, outputs:sourceNode.outputs,
      } } : {}),
      ports:(node.handles || []).map((handle) => semanticPort(handle, node.type, labels)),
      handles:undefined,
    };
  });
  const imageNodes = nodes.filter((node) => node.type === "ImageNode");
  const imagePorts = imageNodes.flatMap((node) => node.ports);
  const checks = {
    fuserHost:Boolean(dom.url?.includes(FUSER_INTELLIGENCE.hostMatch)),
    editorSurfacePresent:Boolean(dom.canvas?.present || dom.settings?.present),
    knownLiveNodeTypes:atlas ? nodes.every((node) => catalog.has(node.type)) : true,
    ...(imageNodes.length ? {
      onePrimitiveImageNode:imageNodes.length === 1,
      imageInputPresent:imagePorts.some((port) => port.role === "input" && port.key === "image"),
      imagePassthroughPresent:imagePorts.some((port) => port.role === "input-passthrough" && port.key === "image"),
      visibleSurfaceDistinctFromModelOutput:imagePorts
        .some((port) => port.role === "input-passthrough" && port.key === "image" && port.modelOutput?.key === "imageOut"),
    } : {}),
  };
  const issues = Object.entries(checks).filter(([, pass]) => !pass).map(([name]) => name);
  return {
    schema:"captutor-app-frame/v1",
    capturedAt:dom.capturedAt,
    app:{ id:FUSER_INTELLIGENCE.id, locale, url:dom.url, title:dom.title },
    source:FUSER_INTELLIGENCE.source,
    confidence:{ score:confidenceFromChecks(checks), checks, issues },
    viewport:dom.viewport,
    editor:{
      canvas:dom.canvas,
      selection:{ nodeIds:nodes.filter((node) => node.selected).map((node) => node.id) },
      nodes,
      edgeCount:dom.edgeCount,
    },
    settings:dom.settings?.present ? {
      ...dom.settings,
      catalog:atlas?.settings || null,
    } : { present:false },
    atlas:atlas ? { schema:atlas.schema, coverage:atlas.coverage, editorBehaviors:atlas.editorBehaviors } : null,
    intent:{
      surface:"A node-based workflow editor where users place, inspect, connect, and move typed units on an infinite canvas.",
      applicableBehaviors:Object.keys(FUSER_INTELLIGENCE.behaviors),
    },
    genericFrameSchema:genericFrame?.schema || null,
  };
}

export async function readFuserFrame(cdp, { locale, t }) {
  let atlas = null;
  try { atlas = buildFuserSourceAtlas(process.env.FUSER_SOURCE_ROOT); } catch {}
  const [genericFrame, dom] = await Promise.all([
    cdp.frame(),
    cdp.eval(`(() => {
      const clean = (value, limit = 160) => String(value || '').replace(/\\s+/g, ' ').trim().slice(0, limit);
      const rect = (element) => {
        if (!element) return null;
        const r = element.getBoundingClientRect();
        return { x:Math.round(r.left), y:Math.round(r.top), width:Math.round(r.width),
          height:Math.round(r.height), cx:Math.round(r.left+r.width/2), cy:Math.round(r.top+r.height/2) };
      };
      const handle = (element) => {
        const style = getComputedStyle(element);
        const zone = element.parentElement;
        return {
          id:element.getAttribute('data-handleid') || '',
          kind:element.classList.contains('target') ? 'target' : element.classList.contains('source') ? 'source' : '',
          side:['left','right','top','bottom'].find(side =>
            element.classList.contains(side) || element.classList.contains('react-flow__handle-' + side)) || '',
          ariaLabel:clean(element.getAttribute('aria-label')),
          connected:element.classList.contains('connecting') || element.classList.contains('valid'),
          rect:rect(element), interactionRect:rect(zone),
          state:{ opacity:style.opacity, transform:style.transform, cursor:style.cursor,
            pointerEvents:style.pointerEvents },
        };
      };
      const nodes = [...document.querySelectorAll('.react-flow__node')].map(node => ({
        id:node.getAttribute('data-id') || node.id || '',
        type:[...node.classList].find(name => name.startsWith('react-flow__node-'))?.replace('react-flow__node-', '') || '',
        selected:node.classList.contains('selected'),
        rect:rect(node),
        text:clean(node.innerText || node.textContent, 220),
        handles:[...node.querySelectorAll('.react-flow__handle')].map(handle),
      }));
      const canvas=document.querySelector('.react-flow');
      const viewport=document.querySelector('.react-flow__viewport');
      const settingsDialog=document.querySelector('[role="dialog"][aria-label="Settings"]');
      const settingsNav=settingsDialog?.querySelector('nav[aria-label="Settings"]');
      const activeSetting=settingsNav?.querySelector('[aria-current="true"]');
      const settingControls=settingsDialog ? [...settingsDialog.querySelectorAll('button,[role="switch"],[role="combobox"]')]
        .map(element => ({
          role:element.getAttribute('role') || element.tagName.toLowerCase(),
          label:clean(element.getAttribute('aria-label') || element.innerText || element.textContent, 100),
          selected:element.getAttribute('aria-selected') || element.getAttribute('aria-checked') || null,
          rect:rect(element),
        })).filter(control => control.label).slice(0, 80) : [];
      return {
        capturedAt:new Date().toISOString(), url:location.href, title:document.title,
        viewport:{ width:innerWidth, height:innerHeight, dpr:devicePixelRatio },
        canvas:{ present:Boolean(canvas), rect:rect(canvas),
          transform:getComputedStyle(viewport || document.documentElement).transform,
          zoomLabel:[...document.querySelectorAll('button')].map(button => (button.innerText || '').trim())
            .find(text => /^\\d+\\s*%$/.test(text)) || '' },
        nodes,
        edgeCount:document.querySelectorAll('.react-flow__edge').length,
        settings:{ present:Boolean(settingsDialog), rect:rect(settingsDialog),
          active:clean(activeSetting?.innerText || activeSetting?.textContent), controls:settingControls },
      };
    })()`),
  ]);
  return interpretFuserFrame(dom, genericFrame, { locale, t, atlas });
}
