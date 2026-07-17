// No Paint, 26.07.17.15.39
// Collaborate with the proposing machine: press No to discard or Paint to keep.

import {
  NOPAINT_LOOP_STATES,
  makeProposal,
  seededRandom,
  seedFrom,
} from "../lib/nopaint-proposals.mjs";

let loopState = "choosing";
let stateBeforePause = "proposing";
let proposal = null;
let proposalFrame = 0;
let proposalNumber = 0;
let sessionSeed = 0;
let random = seededRandom(1);
let cameraFeed = null;
let noButton;
let paintButton;
let saveButton;

function transition(next) {
  if (!NOPAINT_LOOP_STATES.includes(next)) {
    throw new Error(`Unknown No Paint loop state: ${next}`);
  }
  loopState = next;
}

function positionButtons(screen) {
  const gap = 8;
  const totalWidth = noButton.width + paintButton.width + gap;
  const left = Math.floor(screen.width / 2 - totalWidth / 2);
  noButton.reposition({ left, bottom: 8, screen });
  paintButton.reposition({ left: left + noButton.width + gap, bottom: 8, screen });
  saveButton.reposition({ right: 8, top: 8, screen });
}

function clearProposal({ flatten, needsPaint, page, screen, system }) {
  if (!system.nopaint.buffer) return;
  page(system.nopaint.buffer).wipe(255, 255, 255, 0);
  flatten();
  page(screen);
  system.nopaint.needsPresent = true;
  needsPaint();
}

function chooseProposal(api) {
  transition("choosing");
  proposal = makeProposal(
    random,
    api.system.painting.width,
    api.system.painting.height,
  );
  proposalNumber += 1;
  proposalFrame = 0;
  transition("proposing");
  api.system.nopaint.needsPresent = true;
  api.needsPaint();
}

function discardProposal(api) {
  if (loopState !== "proposing" && loopState !== "paused") return;
  transition("discarding");
  clearProposal(api);
  chooseProposal(api);
}

function persistPainting({ store, system }) {
  store["painting"] = {
    width: system.painting.width,
    height: system.painting.height,
    pixels: new Uint8ClampedArray(system.painting.pixels),
  };
  store.persist("painting", "local:db");
}

function commitProposal(api) {
  if (loopState !== "proposing" && loopState !== "paused") return;
  transition("committing");

  api.page(api.system.painting).paste(api.system.nopaint.buffer);
  api.flatten();
  api.system.nopaint.addUndoPainting(
    api.system.painting,
    `nopaint:${proposal.kind}`,
  );
  persistPainting(api);
  clearProposal(api);
  chooseProposal(api);
}

function togglePaused({ needsPaint }) {
  if (loopState === "paused") {
    transition(stateBeforePause);
  } else if (loopState === "proposing") {
    stateBeforePause = loopState;
    transition("paused");
  }
  needsPaint();
}

function savePainting({ canShare, download, num, system }) {
  download(`nopaint-${num.timestamp()}.png`, system.painting, {
    scale: 2,
    cropToScreen: true,
    sharing: canShare,
  });
}

// 🥾 Boot
function boot({ colon, hud, net, num, params, screen, system, ui, ...api }) {
  const requestedSeed = colon[0] || params[0];
  const numericSeed = /^\d+$/.test(requestedSeed || "")
    ? Number(requestedSeed) >>> 0
    : null;
  sessionSeed = numericSeed || seedFrom(
    requestedSeed || `${num.timestamp()}-${num.randIntRange(0, 0x7fffffff)}`,
  );
  random = seededRandom(sessionSeed);
  proposal = null;
  proposalFrame = 0;
  proposalNumber = 0;
  cameraFeed = null;
  stateBeforePause = "proposing";

  noButton = new ui.TextButton("No");
  paintButton = new ui.TextButton("Paint");
  saveButton = new ui.TextButton("Save");
  positionButtons(screen);

  hud.label("No Paint — No [N] / Paint [Enter]");
  net.rewrite(`/nopaint:${sessionSeed}`);
  chooseProposal({ ...api, screen, system });
}

// 🧮 Sim
function sim({ needsPaint }) {
  if (loopState === "proposing") {
    proposalFrame += 1;
    needsPaint();
  }
}

function animatedColor(color, phase) {
  const pulse = Math.round(Math.sin(phase) * 14);
  return [
    Math.max(0, Math.min(255, color[0] + pulse)),
    Math.max(0, Math.min(255, color[1] + pulse)),
    Math.max(0, Math.min(255, color[2] + pulse)),
    color[3],
  ];
}

function renderProposal($) {
  const { ink, page, paste, system, video } = $;
  const buffer = system.nopaint.buffer;
  const phase = proposalFrame / 24;
  const wave = Math.sin(phase);
  const drift = Math.round(wave * proposal.drift);
  const color = animatedColor(proposal.color, phase);

  page(buffer).wipe(255, 255, 255, 0);

  if (proposal.kind === "rect") {
    ink(color).box(
      proposal.x + drift,
      proposal.y - drift,
      proposal.w,
      proposal.h,
    );
  } else if (proposal.kind === "oval") {
    const pulse = 1 + wave * 0.12;
    ink(color).oval(
      proposal.x + proposal.w / 2,
      proposal.y + proposal.h / 2,
      proposal.w * pulse,
      proposal.h * pulse,
      true,
    );
  } else if (proposal.kind === "line") {
    ink(color).line(
      proposal.x - drift,
      proposal.y + drift,
      proposal.x + proposal.w + drift,
      proposal.y + proposal.h - drift,
      proposal.thickness,
    );
  } else if (proposal.kind === "wipe") {
    ink(color[0], color[1], color[2], 255).box(
      0,
      0,
      system.painting.width,
      system.painting.height,
    );
  } else if (proposal.kind === "camera") {
    cameraFeed ||= video("camera", {
      width: system.painting.width,
      height: system.painting.height,
      facing: "environment",
    });
    const frame = cameraFeed();
    if (frame) {
      paste(frame);
    } else {
      ink(24, 24, 24, 230).box(0, 0, buffer.width, buffer.height);
      ink(255).write("waiting for camera", {
        x: Math.max(4, Math.floor(buffer.width / 2 - 54)),
        y: Math.floor(buffer.height / 2),
      });
    }
  }

  page($.screen);
}

// 🎨 Paint
function paint($) {
  if (!proposal || !$.system.nopaint.buffer) return false;
  renderProposal($);
  $.system.nopaint.needsPresent = true;

  const paused = loopState === "paused" ? " · paused" : "";
  $.ink(255).write(
    `${proposal.kind} ${proposalNumber}${paused}`,
    { x: 8, y: 8 },
  );
  $.ink(255, 150).write(`seed ${sessionSeed}`, { x: 8, y: 20 });

  positionButtons($.screen);
  noButton.paint($, [[20, 20, 20], [255, 255, 255], [255, 255, 255]]);
  paintButton.paint($, [[245, 245, 245], [255, 255, 255], [0, 0, 0]]);
  saveButton.paint($, [[20, 20, 20], [180, 180, 180], [255, 255, 255]]);
  return loopState === "proposing";
}

function isAny(e, names) {
  return names.some((name) => e.is(name));
}

// 🎪 Act — every input surface reaches the same two decision functions.
function act($) {
  const { event: e } = $;

  noButton.btn.act(e, () => discardProposal($));
  paintButton.btn.act(e, () => commitProposal($));
  saveButton.btn.act(e, () => savePainting($));

  if (isAny(e, [
    "keyboard:down:n",
    "keyboard:down:escape",
    "voice:no",
    "robot:no",
    "nopaint:no",
  ])) discardProposal($);

  if (isAny(e, [
    "keyboard:down:enter",
    "keyboard:down:p",
    "voice:paint",
    "robot:paint",
    "nopaint:paint",
  ])) commitProposal($);

  if (e.is("keyboard:down:space")) togglePaused($);
  if (e.is("keyboard:down:s")) savePainting($);
}

// The conductor makes decisions explicitly; generic pointer-lift baking must
// never commit a proposal behind the participant's back.
function bake() {}

function meta() {
  return {
    title: "No Paint",
    desc: "Collaborate with a proposing machine: press No to discard or Paint to keep.",
    controls: "No: N/Escape · Paint: Enter/P · pause: Space · save/share: S",
    params: "optional deterministic session seed",
    example: "nopaint award-entry",
  };
}

// Pointer events still pass through the inherited nopaint gesture handler
// before this conductor sees its buttons. Bake-on-leave suppresses that
// handler's automatic lift bake; our no-op bake below also means leaving saves
// the accepted painting without silently accepting the live proposal.
export const system = "nopaint:bake-on-leave";

export { act, bake, boot, meta, paint, sim };
