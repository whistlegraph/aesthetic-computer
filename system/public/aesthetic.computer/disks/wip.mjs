// WIP, 26.09.15
// Watch the saved steps of a painting in progress, or continue your own.
import { readPaintingWip } from "../lib/painting-wip.mjs";
import { decodePaintingState } from "../lib/painting-state.mjs";
import { paintingFrame } from "../lib/painting-playback.mjs";

let code, painting, piece, state, error, timer, active, edit, previous, playback, next, testChannel;
let step, playing, nextStepAt;

function showStep($, index) {
  step = Math.max(0, Math.min(index, piece.layers.length - 1));
  painting = null;
  nextStepAt = Date.now() + (step === piece.layers.length - 1 ? 2500 : 1500);
  $.needsPaint();
}

function boot($) {
  code = $.params[0];
  painting = piece = state = error = null;
  step = 0;
  playing = true;
  active = true;
  if ($.debug && typeof BroadcastChannel !== "undefined") testChannel = new BroadcastChannel("ac-nopaint-test");
  edit = new $.ui.TextButton("Edit");
  previous = new $.ui.TextButton("<");
  playback = new $.ui.TextButton("Pause");
  next = new $.ui.TextButton(">");
  $.hud.label(" ", [0, 0, 0, 0]);
  $.net.rewrite(`/#${code}`);
  async function refresh() {
    try {
      const next = await readPaintingWip($, code, false);
      if (!active) return;
      if (next.status === "done") { $.jump(`painting~${code}`); return; }
      if (!piece || next.revision !== state?.revision) {
        const full = await readPaintingWip($, code, true);
        if (!active) return;
        if (full.state) {
          const decoded = await decodePaintingState(full.state);
          if (!active) return;
          piece = decoded;
          showStep($, step);
        }
        state = full;
      } else {
        state = next;
      }
      error = null;
    } catch (failure) { if (active) error = failure.message; }
    if (active) { $.needsPaint(); timer = setTimeout(refresh, 2000); }
  }
  refresh();
}

function paint($) {
  $.wipe(18);
  const multiple = piece?.layers.length > 1;
  const barY = $.screen.height - (multiple ? 52 : 6);
  if (piece && !painting) painting = paintingFrame($, piece, step);
  if (painting) {
    const scale = Math.max(0, Math.min($.screen.width / painting.width, (barY - 28) / painting.height));
    $.paste(painting, Math.floor(($.screen.width - painting.width * scale) / 2),
      28 + Math.floor((barY - 28 - painting.height * scale) / 2), scale);
  }
  $.ink(255).write(error || `WIP #${code}`, { x: 6, y: 6 });
  const canEdit = state?.canEdit;
  if (canEdit) {
    edit.reposition({ right: 6, top: 3, screen: $.screen });
    edit.paint($);
  }
  if (multiple) {
    $.ink(255).write(`Step ${step}/${piece.layers.length - 1}`, { center: "x", y: barY + 4 });
    previous.disabled = step === 0;
    next.disabled = step === piece.layers.length - 1;
    previous.reposition({ left: 6, bottom: 6, screen: $.screen });
    playback.reposition({ center: "x", bottom: 6, screen: $.screen }, playing ? "Pause" : "Play");
    next.reposition({ right: 6, bottom: 6, screen: $.screen });
    for (const button of [previous, playback, next]) button.paint($);
  }
  testChannel?.postMessage({ version: "wip", ready: Boolean(painting), code,
    revision: state?.revision, canEdit: Boolean(canEdit), error,
    step, steps: piece ? piece.layers.length - 1 : 0, playing,
    controls: { ...(canEdit ? { edit: { ...edit.btn.box } } : {}),
      ...(multiple ? { previous: { ...previous.btn.box }, playback: { ...playback.btn.box }, next: { ...next.btn.box } } : {}) },
    layout: { screenResolution: { width: $.screen.width, height: $.screen.height } } });
  return true;
}

function act($) {
  if (state?.canEdit) edit.act($.event, () => $.jump(`nopaint~resume~${code}`));
  if (piece?.layers.length > 1) {
    const seek = direction => { playing = false; showStep($, step + direction); };
    const toggle = () => {
      playing = !playing;
      showStep($, playing && step === piece.layers.length - 1 ? 0 : step);
    };
    previous.act($.event, () => seek(-1));
    next.act($.event, () => seek(1));
    playback.act($.event, toggle);
    if ($.event.is("keyboard:down:arrowleft")) seek(-1);
    if ($.event.is("keyboard:down:arrowright")) seek(1);
    if ($.event.is("keyboard:down:space")) toggle();
  }
}

function sim($) {
  if (active && playing && piece?.layers.length > 1 && Date.now() >= nextStepAt)
    showStep($, (step + 1) % piece.layers.length);
}

function leave() { active = false; clearTimeout(timer); piece = painting = null; testChannel?.close(); testChannel = null; }
export { boot, paint, sim, act, leave };
