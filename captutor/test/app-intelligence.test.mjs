import assert from "node:assert/strict";
import test from "node:test";

import {
  FUSER_INTELLIGENCE, FUSER_SELECTORS, fuserEditor, fuserNodePickerResult,
  interpretFuserFrame,
} from "../app-intelligence/fuser.mjs";
import { translator } from "../lib/i18n.mjs";

test("Fuser intelligence distinguishes the visible Image passthrough from imageOut", () => {
  const editor = fuserEditor(translator("en"));
  assert.match(FUSER_SELECTORS.imagePassthrough, /handle-right\.source$/);
  assert.doesNotMatch(FUSER_SELECTORS.imagePassthrough, /imageOut/);
  assert.equal(editor.image.passthrough.label, "Image");
  assert.equal(editor.image.passthrough.modelLabel, "Image Out");
  assert.match(editor.image.passthrough.teaching.modelDistinction, /not this visible handle/i);
  assert.equal(FUSER_INTELLIGENCE.source.branch, "staging");
});

test("Fuser picker selector requires the exact localized accessible name", () => {
  assert.equal(fuserNodePickerResult("Image"), '[role="option"][aria-label="Image"]');
  assert.equal(fuserNodePickerResult("Imagen"), '[role="option"][aria-label="Imagen"]');
});

test("Fuser frame compresses DOM facts into semantic ports with confidence", () => {
  const frame = interpretFuserFrame({
    capturedAt:"2026-07-29T00:00:00.000Z",
    url:"https://app.fuser.studio/flow/example", title:"Fuser",
    viewport:{ width:1280, height:720, dpr:2 },
    canvas:{ present:true, zoomLabel:"100%", transform:"matrix(1, 0, 0, 1, 0, 0)" },
    edgeCount:0,
    nodes:[{ id:"n1", type:"ImageNode", selected:true, rect:{ x:400, y:180, width:320, height:280 }, handles:[
      { id:"019runtime-a", kind:"target", side:"left", rect:{ width:8, height:8 }, interactionRect:{ width:24, height:24 } },
      { id:"019runtime-a", kind:"source", side:"right", rect:{ width:8, height:8 }, interactionRect:{ width:24, height:24 } },
    ] }],
  }, { schema:"captutor-cdp-frame/v1" }, { locale:"en", t:translator("en") });
  assert.equal(frame.schema, "captutor-app-frame/v1");
  assert.equal(frame.confidence.score, 1);
  assert.deepEqual(frame.editor.selection.nodeIds, ["n1"]);
  assert.equal(frame.editor.nodes[0].ports[0].role, "input");
  assert.equal(frame.editor.nodes[0].ports[1].role, "input-passthrough");
  assert.equal(frame.editor.nodes[0].ports[1].modelOutput.label, "Image Out");
});

test("Image port semantics survive a missing side class once direction is scoped to ImageNode", () => {
  const frame = interpretFuserFrame({
    capturedAt:"2026-07-29T00:00:00.000Z", url:"https://app.fuser.studio/flow/x", title:"Fuser",
    viewport:{ width:1280, height:720, dpr:2 }, canvas:{ present:true }, edgeCount:0,
    nodes:[{ id:"n1", type:"ImageNode", selected:false, handles:[
      { id:"runtime", kind:"target", side:"" }, { id:"runtime", kind:"source", side:"" },
    ] }],
  }, null, { locale:"en", t:translator("en") });
  assert.equal(frame.confidence.score, 1);
  assert.deepEqual(frame.editor.nodes[0].ports.map((port) => port.role), ["input", "input-passthrough"]);
});

test("Fuser frame confidence is contextual for Settings without an Image node", () => {
  const frame = interpretFuserFrame({
    capturedAt:"2026-07-29T00:00:00.000Z", url:"https://app.fuser.studio/flow/x/settings?tab=editor", title:"Fuser",
    viewport:{ width:1280, height:720, dpr:2 }, canvas:{ present:true }, edgeCount:0, nodes:[],
    settings:{ present:true, active:"Editor", controls:[{ role:"switch", label:"Curated Nodes" }] },
  }, null, { locale:"en", t:translator("en") });
  assert.equal(frame.confidence.score, 1);
  assert.deepEqual(frame.confidence.issues, []);
  assert.equal(frame.settings.active, "Editor");
});
