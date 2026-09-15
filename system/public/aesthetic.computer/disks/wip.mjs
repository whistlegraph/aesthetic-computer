// WIP, 26.09.15
// Watch a painting in progress, continue your own, or start a new one from it.
import { readPaintingWip } from "../lib/painting-wip.mjs";
import { decodePaintingState } from "../lib/painting-state.mjs";
import { buttonLabelSize, paintDecisionButton } from "../lib/nopaint-buttons.mjs";

let code, painting, state, error, timer, active, edit, fork, testChannel;

function boot($) {
  code = $.params[0];
  painting = state = error = null;
  active = true;
  if ($.debug && typeof BroadcastChannel !== "undefined") testChannel = new BroadcastChannel("ac-nopaint-test");
  edit = new $.ui.Button();
  fork = new $.ui.Button();
  $.hud.label(" ", [0, 0, 0, 0]);
  $.net.rewrite(`/#${code}`);
  async function refresh() {
    try {
      const next = await readPaintingWip($, code, false);
      if (!active) return;
      if (next.status === "done") { $.jump(`painting~${code}`); return; }
      if (!painting || next.revision !== state?.revision) {
        const full = await readPaintingWip($, code, true);
        if (!active) return;
        if (full.state) painting = (await decodePaintingState(full.state)).composite;
      }
      state = next;
      error = null;
    } catch (failure) { if (active) error = failure.message; }
    if (active) { $.needsPaint(); timer = setTimeout(refresh, 2000); }
  }
  refresh();
}

function paint($) {
  $.wipe(18);
  const barHeight = Math.max(56, Math.floor($.screen.height * 0.2));
  const barY = $.screen.height - barHeight;
  if (painting) {
    const scale = Math.min($.screen.width / painting.width, (barY - 22) / painting.height);
    $.paste(painting, Math.floor(($.screen.width - painting.width * scale) / 2),
      22 + Math.floor((barY - 22 - painting.height * scale) / 2), scale);
  }
  $.ink(255).write(error || `WIP #${code}`, { x: 6, y: 6 });
  const canEdit = state?.canEdit;
  const editWidth = canEdit ? Math.floor($.screen.width * 0.38) : 0;
  Object.assign(edit, { box: new $.geo.Box(0, barY, editWidth, barHeight) });
  Object.assign(fork, { box: new $.geo.Box(canEdit ? editWidth + 4 : 0, barY,
    $.screen.width - (canEdit ? editWidth + 4 : 0), barHeight) });
  const controls = [...(canEdit ? [[edit, "Edit", "back"]] : []), [fork, "Paint with", "paint"]];
  const size = Math.min(...controls.map(([button, label]) => buttonLabelSize($, button, label)));
  for (const [button, label, flavor] of controls) paintDecisionButton($, button, label, flavor, size);
  testChannel?.postMessage({ version: "wip", ready: Boolean(painting), code,
    revision: state?.revision, canEdit: Boolean(canEdit), error,
    controls: { ...(canEdit ? { edit: { ...edit.box } } : {}), fork: { ...fork.box } },
    layout: { screenResolution: { width: $.screen.width, height: $.screen.height } } });
  return true;
}

function act($) {
  if (state?.canEdit) edit.act($.event, () => $.jump(`nopaint~resume~${code}`));
  if (painting) fork.act($.event, () => $.jump(`nopaint~from~${code}`));
}

function leave() { active = false; clearTimeout(timer); testChannel?.close(); testChannel = null; }
export { boot, paint, act, leave };
