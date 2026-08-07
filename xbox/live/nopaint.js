// @bundle-qr
// No Paint Xbox prototype: command-backed paintings.

const background = [247, 243, 232];
const inkColors = [
  [24, 27, 35],
  [238, 70, 101],
  [30, 124, 214],
  [244, 172, 42],
  [57, 170, 116],
  [133, 82, 186],
];
const uiInk = [24, 27, 35];
const uiPaper = [247, 243, 232];
const uiRule = [205, 198, 181];
const noHover = [238, 70, 101];
const paintHover = [30, 124, 214];
const acceptedMarks = [];
const brushKinds = ["line", "box", "triangle", "rays", "echo", "grid"];
const effectKinds = ["mirror", "turn", "zoom", "blur", "invert",
  "flip", "scroll", "skew", "breathe", "saturate", "contrast",
  "posterize", "recurse", "vignette", "noise"];
const proposalKinds = [...brushKinds, ...effectKinds];
const acceptedEffects = [];
const maxPaintingCommands = 1600;

let proposal = null;
let proposalNumber = 0;
let rejectedCount = 0;
let mode = "making";
let previousButtons = [];
let randomState = 1;
let view = { width: 1920, height: 1080 };
let proposalFrame = 0;
let decisionHeld = false;

function hashSeed(value) {
  let hash = 2166136261;
  for (const character of String(value)) {
    hash ^= character.charCodeAt(0);
    hash = Math.imul(hash, 16777619);
  }
  return hash >>> 0 || 1;
}

function random() {
  randomState ^= randomState << 13;
  randomState ^= randomState >>> 17;
  randomState ^= randomState << 5;
  return (randomState >>> 0) / 4294967296;
}

function clamp(value, low, high) {
  return Math.max(low, Math.min(high, value));
}

function syncView() {
  const next = typeof gameView === "function" ? gameView() : null;
  view.width = clamp(Math.round(Number(next?.width) || 1920), 480, 3840);
  view.height = clamp(Math.round(Number(next?.height) || 1080), 480, 2160);
}

function ink() {
  return inkColors[Math.floor(random() * inkColors.length)].slice();
}

function lineCommand(x1, y1, x2, y2, width, color) {
  return { primitive: "line", x1, y1, x2, y2, width, color: color.slice() };
}

function makeLine() {
  const angle = random() * Math.PI * 2;
  const length = .16 + random() * .58;
  const centerX = .14 + random() * .72;
  const centerY = .13 + random() * .62;
  const dx = Math.cos(angle) * length * .5;
  const dy = Math.sin(angle) * length * .5;
  return [lineCommand(clamp(centerX - dx, .055, .945),
    clamp(centerY - dy, .055, .78), clamp(centerX + dx, .055, .945),
    clamp(centerY + dy, .055, .78), .004 + random() * .022, ink())];
}

function makeBox() {
  const width = .09 + random() * .32;
  const height = .07 + random() * .28;
  return [{ primitive: "box", x: .06 + random() * (.88 - width),
    y: .06 + random() * (.72 - height), width, height, color: ink() }];
}

function makeTriangle() {
  const centerX = .18 + random() * .64;
  const centerY = .16 + random() * .48;
  const radius = .08 + random() * .18;
  const angle = random() * Math.PI * 2;
  const points = [0, 1, 2].map((index) => {
    const theta = angle + index * Math.PI * 2 / 3;
    return { x: clamp(centerX + Math.cos(theta) * radius, .04, .96),
      y: clamp(centerY + Math.sin(theta) * radius, .04, .79) };
  });
  return [{ primitive: "triangle", x1: points[0].x, y1: points[0].y,
    x2: points[1].x, y2: points[1].y, x3: points[2].x, y3: points[2].y,
    color: ink() }];
}

function makeRays() {
  const centerX = .18 + random() * .64;
  const centerY = .16 + random() * .48;
  const radius = .10 + random() * .20;
  const count = 7 + Math.floor(random() * 7);
  const color = ink();
  const width = .003 + random() * .009;
  const commands = [];
  for (let index = 0; index < count; index++) {
    const angle = index / count * Math.PI * 2 + random() * .08;
    const reach = radius * (.72 + random() * .36);
    commands.push(lineCommand(centerX, centerY,
      clamp(centerX + Math.cos(angle) * reach, .035, .965),
      clamp(centerY + Math.sin(angle) * reach, .035, .79), width, color));
  }
  return commands;
}

function makeEcho() {
  const color = ink();
  const angle = random() * Math.PI * 2;
  const length = .18 + random() * .38;
  const centerX = .22 + random() * .56;
  const centerY = .18 + random() * .42;
  const dx = Math.cos(angle) * length * .5;
  const dy = Math.sin(angle) * length * .5;
  const nx = -Math.sin(angle) * .025;
  const ny = Math.cos(angle) * .025;
  return [-2, -1, 0, 1, 2].map((offset) => lineCommand(
    clamp(centerX - dx + nx * offset, .035, .965),
    clamp(centerY - dy + ny * offset, .035, .79),
    clamp(centerX + dx + nx * offset, .035, .965),
    clamp(centerY + dy + ny * offset, .035, .79),
    .003 + Math.abs(offset) * .0015, color));
}

function makeGrid() {
  const color = ink();
  const left = .08 + random() * .52;
  const top = .08 + random() * .38;
  const width = .18 + random() * Math.min(.34, .89 - left);
  const height = .14 + random() * Math.min(.27, .76 - top);
  const columns = 2 + Math.floor(random() * 4);
  const rows = 2 + Math.floor(random() * 4);
  const commands = [];
  for (let column = 0; column <= columns; column++) {
    const x = left + width * column / columns;
    commands.push(lineCommand(x, top, x, top + height, .0025, color));
  }
  for (let row = 0; row <= rows; row++) {
    const y = top + height * row / rows;
    commands.push(lineCommand(left, y, left + width, y, .0025, color));
  }
  return commands;
}

function nextProposal() {
  proposalNumber++;
  const kind = proposalKinds[(proposalNumber - 1) % proposalKinds.length];
  const factories = { line: makeLine, box: makeBox, triangle: makeTriangle,
    rays: makeRays, echo: makeEcho, grid: makeGrid };
  proposal = effectKinds.includes(kind)
    ? { kind, family: "effect", effect: { kind,
      seed: Math.floor(random() * 1000000) } }
    : { kind, family: "brush", commands: factories[kind]() };
  proposalFrame = 0;
}

function drawCommand(command) {
  const scale = Math.min(view.width, view.height);
  if (command.primitive === "line") {
    line(command.x1 * view.width, command.y1 * view.height,
      command.x2 * view.width, command.y2 * view.height,
      Math.max(2, command.width * scale), ...command.color);
  } else if (command.primitive === "box") {
    box(command.x * view.width, command.y * view.height,
      command.width * view.width, command.height * view.height, ...command.color);
  } else if (command.primitive === "triangle") {
    triangle(command.x1 * view.width, command.y1 * view.height,
      command.x2 * view.width, command.y2 * view.height,
      command.x3 * view.width, command.y3 * view.height, ...command.color);
  }
}

function drawMark(mark) {
  for (const command of mark.commands) drawCommand(command);
}

function cloneMark(mark) {
  return { kind: mark.kind, family: mark.family || "brush",
    ...(mark.effect ? { effect: { ...mark.effect } } : {}),
    ...(mark.commands ? { commands: mark.commands.map((command) => ({
      ...command, color: command.color.slice(),
    })) } : {}) };
}

function mix(from, to, amount) {
  return from + (to - from) * amount;
}

function animatedCommand(command, index, frame) {
  const phase = frame / 18 + index * .71;
  const dx = Math.sin(phase) * .006;
  const dy = Math.cos(phase * .83) * .005;
  const pulse = 1 + Math.sin(phase * .73) * .035;
  const color = command.color.map((value, channel) => channel < 3
    ? clamp(Math.round(value + Math.sin(phase + channel) * 9), 0, 255) : value);
  if (command.primitive === "line") return { ...command,
    x1: clamp(command.x1 + dx, -.1, 1.1),
    y1: clamp(command.y1 + dy, -.1, 1.1),
    x2: clamp(command.x2 - dx * .55, -.1, 1.1),
    y2: clamp(command.y2 - dy * .55, -.1, 1.1),
    width: command.width * pulse, color };
  if (command.primitive === "box") return { ...command,
    x: command.x + dx, y: command.y + dy,
    width: command.width * pulse, height: command.height / pulse, color };
  return { ...command,
    x1: command.x1 + dx, y1: command.y1 + dy,
    x2: command.x2 - dy, y2: command.y2 + dx,
    x3: command.x3 - dx, y3: command.y3 - dy, color };
}

function animatedMark(mark, frame = proposalFrame) {
  return { kind: mark.kind, family: "brush",
    commands: mark.commands.map((command, index) =>
      animatedCommand(command, index, frame)) };
}

function transformCommand(command, transform, colorTransform = (color) => color) {
  const color = colorTransform(command.color.slice());
  if (command.primitive === "line") {
    const a = transform(command.x1, command.y1);
    const b = transform(command.x2, command.y2);
    return [{ ...command, x1: a.x, y1: a.y, x2: b.x, y2: b.y, color }];
  }
  if (command.primitive === "triangle") {
    const a = transform(command.x1, command.y1);
    const b = transform(command.x2, command.y2);
    const c = transform(command.x3, command.y3);
    return [{ ...command, x1: a.x, y1: a.y, x2: b.x, y2: b.y,
      x3: c.x, y3: c.y, color }];
  }
  const a = transform(command.x, command.y);
  const b = transform(command.x + command.width, command.y);
  const c = transform(command.x + command.width, command.y + command.height);
  const d = transform(command.x, command.y + command.height);
  return [
    { primitive: "triangle", x1: a.x, y1: a.y, x2: b.x, y2: b.y,
      x3: c.x, y3: c.y, color: color.slice() },
    { primitive: "triangle", x1: a.x, y1: a.y, x2: c.x, y2: c.y,
      x3: d.x, y3: d.y, color: color.slice() },
  ];
}

function transformMarks(marks, transform, colorTransform) {
  return marks.map((mark) => ({ kind: mark.kind, family: "brush",
    commands: mark.commands.flatMap((command) =>
      transformCommand(command, transform, colorTransform)) }));
}

function supportsOrderedEffects() {
  return typeof capabilities === "function" &&
    capabilities()?.orderedEffects === true;
}

function softenMarks(marks, frame = proposalFrame) {
  const spread = .002 + blurRadius(frame) * .0007;
  const offsets = [[-1, 0], [1, 0], [0, -1], [0, 1]];
  let remaining = maxPaintingCommands;
  return marks.map((mark) => {
    const commands = [];
    for (const command of mark.commands) {
      for (const [x, y] of offsets) {
        const ghost = command.color.map((value, channel) => channel < 3
          ? Math.round(mix(value, background[channel], .62)) : value);
        commands.push(...transformCommand(command,
          (px, py) => ({ x: px + x * spread, y: py + y * spread }), () => ghost));
      }
      commands.push({ ...command, color: command.color.slice() });
      if (commands.length >= remaining) break;
    }
    const kept = commands.slice(0, remaining);
    remaining -= kept.length;
    return { kind: mark.kind, family: "brush", commands: kept };
  });
}

function applyEffect(marks, effect, frame = proposalFrame) {
  const phase = frame / 24;
  if (effect.kind === "mirror") {
    const scaleX = Math.cos(phase * .9);
    return transformMarks(marks,
      (x, y) => ({ x: .5 + (x - .5) * scaleX, y }));
  }
  if (effect.kind === "turn") {
    const angle = Math.sin(phase * .7) * Math.PI * .32;
    const cosine = Math.cos(angle);
    const sine = Math.sin(angle);
    return transformMarks(marks, (x, y) => ({
      x: .5 + (x - .5) * cosine - (y - .42) * sine,
      y: .42 + (x - .5) * sine + (y - .42) * cosine,
    }));
  }
  if (effect.kind === "flip") {
    const scaleY = Math.cos(phase * .9);
    return transformMarks(marks,
      (x, y) => ({ x, y: .42 + (y - .42) * scaleY }));
  }
  if (effect.kind === "zoom") {
    const scale = 1 + Math.sin(phase * .8) * .24;
    return transformMarks(marks,
      (x, y) => ({ x: .5 + (x - .5) * scale, y: .42 + (y - .42) * scale }));
  }
  if (effect.kind === "scroll") {
    const dx = Math.sin(phase * .72) * .22;
    const dy = Math.cos(phase * .61) * .12;
    return transformMarks(marks, (x, y) => ({ x: x + dx, y: y + dy }));
  }
  if (effect.kind === "skew") {
    const amount = Math.sin(phase * .8) * .62;
    return transformMarks(marks,
      (x, y) => ({ x: x + (y - .42) * amount, y }));
  }
  if (effect.kind === "breathe") {
    const pulse = Math.sin(phase * .85);
    return transformMarks(marks, (x, y) => ({
      x: .5 + (x - .5) * (1 + pulse * .18),
      y: .42 + (y - .42) * (1 - pulse * .12),
    }));
  }
  if (effect.kind === "invert") {
    const amount = .5 + Math.sin(phase) * .5;
    return transformMarks(marks, (x, y) => ({ x, y }), (color) =>
      color.map((value, channel) => channel < 3
        ? Math.round(mix(value, 255 - value, amount)) : value));
  }
  if (effect.kind === "saturate") {
    const amount = .35 + (.5 + Math.sin(phase) * .5) * 2.3;
    return transformMarks(marks, (x, y) => ({ x, y }), (color) => {
      const gray = color[0] * .299 + color[1] * .587 + color[2] * .114;
      return color.map((value, channel) => channel < 3
        ? clamp(Math.round(gray + (value - gray) * amount), 0, 255) : value);
    });
  }
  if (effect.kind === "contrast") {
    const amount = .55 + (.5 + Math.sin(phase * .82) * .5) * 1.7;
    return transformMarks(marks, (x, y) => ({ x, y }), (color) =>
      color.map((value, channel) => channel < 3
        ? clamp(Math.round(128 + (value - 128) * amount), 0, 255) : value));
  }
  if (effect.kind === "posterize") {
    const levels = 2 + Math.round((.5 + Math.sin(phase * .7) * .5) * 5);
    return transformMarks(marks, (x, y) => ({ x, y }), (color) =>
      color.map((value, channel) => channel < 3
        ? Math.round(Math.round(value / 255 * (levels - 1)) * 255 / (levels - 1))
        : value));
  }
  if (effect.kind === "recurse") {
    const scale = .38 + Math.sin(phase * .63) * .07;
    const centers = [[.25, .21], [.75, .21], [.25, .63], [.75, .63]];
    return marks.map((mark) => ({ kind: mark.kind, family: "brush",
      commands: centers.flatMap(([cx, cy]) => mark.commands.flatMap((command) =>
        transformCommand(command, (x, y) => ({
          x: cx + (x - .5) * scale, y: cy + (y - .42) * scale,
        })))).slice(0, maxPaintingCommands) }));
  }
  if (effect.kind === "vignette") {
    const depth = .035 + (.5 + Math.sin(phase) * .5) * .08;
    const shade = Math.round(34 + (.5 + Math.cos(phase) * .5) * 42);
    return [...marks.map(cloneMark), { kind: "vignette", family: "brush", commands: [
      { primitive: "box", x: 0, y: 0, width: 1, height: depth,
        color: [shade, shade, shade] },
      { primitive: "box", x: 0, y: .84 - depth, width: 1, height: depth,
        color: [shade, shade, shade] },
      { primitive: "box", x: 0, y: depth, width: depth, height: .84 - depth * 2,
        color: [shade, shade, shade] },
      { primitive: "box", x: 1 - depth, y: depth, width: depth,
        height: .84 - depth * 2, color: [shade, shade, shade] },
    ] }];
  }
  if (effect.kind === "noise") {
    const unit = (index, axis) => {
      const value = Math.sin(effect.seed * .013 + index * 78.233 + axis * 31.177) *
        43758.5453;
      return value - Math.floor(value);
    };
    const commands = Array.from({ length: 96 }, (_, index) => {
      const drift = Math.sin(phase + index * .37) * .006;
      const value = Math.round(24 + unit(index, 2) * 207);
      const size = .002 + unit(index, 3) * .009;
      return { primitive: "box", x: unit(index, 0) + drift,
        y: unit(index, 1) * .84 - drift, width: size, height: size,
        color: [value, value, value] };
    });
    return [...marks.map(cloneMark), { kind: "noise", family: "brush", commands }];
  }
  return marks.map(cloneMark);
}

function blurRadius(frame = proposalFrame) {
  return 2 + Math.round((.5 + Math.sin(frame / 24) * .5) * 8);
}

function applyRenderEffect(effect) {
  if (effect.kind === "blur" && supportsOrderedEffects() &&
      typeof blur === "function") blur(effect.radius);
}

// This is the complete painting compositor. It deliberately knows nothing
// about proposals, buttons, labels, or progress UI.
function renderMarks(marks, effects = acceptedEffects) {
  wipe(...background);
  const ordered = effects.slice().sort((a, b) => a.after - b.after);
  let effectIndex = 0;
  while (effectIndex < ordered.length && ordered[effectIndex].after === 0)
    applyRenderEffect(ordered[effectIndex++]);
  for (let index = 0; index < marks.length; index++) {
    drawMark(marks[index]);
    while (effectIndex < ordered.length && ordered[effectIndex].after === index + 1)
      applyRenderEffect(ordered[effectIndex++]);
  }
}

function renderPainting() {
  renderMarks(acceptedMarks);
}

function controlLocale() {
  const device = typeof capabilities === "function" ? capabilities() || {} : {};
  const platform = String(device.platform || "").toLowerCase();
  const family = String(device.inputFamily || "").toLowerCase();
  if (family === "touch") return {
    no: "TAP NO", paint: "TAP PAINT", done: "TAP DONE",
    noKey: "TAP", paintKey: "TAP", doneKey: "TAP",
    noAction: "No", paintAction: "Paint", doneAction: "Done", family: "touch",
  };
  if (family === "xbox" || platform.includes("xbox")) return {
    no: "LEFT  NO", paint: "RIGHT  PAINT", done: "MENU  DONE",
    noKey: "LEFT", paintKey: "RIGHT", doneKey: "MENU",
    noAction: "No", paintAction: "Paint", doneAction: "Done", family: "xbox",
  };
  if (platform === "web") return {
    no: "B  NO", paint: "A  PAINT", done: "RETURN  DONE",
    noKey: "B", paintKey: "A", doneKey: "RETURN",
    noAction: "No", paintAction: "Paint", doneAction: "Done", family: "web",
  };
  return {
    no: "LEFT  NO", paint: "RIGHT  PAINT", done: "RETURN  DONE",
    noKey: "LEFT", paintKey: "RIGHT", doneKey: "RETURN",
    noAction: "No", paintAction: "Paint", doneAction: "Done", family: "keyboard",
  };
}

function uiLayout() {
  const compact = view.width < view.height * .8;
  const height = compact ? Math.max(210, Math.round(view.height * .20))
    : Math.max(140, Math.round(view.height * .15));
  const top = view.height - height;
  const pad = Math.max(18, Math.round(view.width * .018));
  const headerHeight = 0;
  const buttonTop = top;
  const buttonHeight = Math.max(56, view.height - buttonTop);
  const buttonWidth = view.width / 2;
  return { compact, height, top, pad, headerHeight,
    no: { x: 0, y: buttonTop, width: buttonWidth, height: buttonHeight },
    paint: { x: buttonWidth, y: buttonTop,
      width: buttonWidth, height: buttonHeight } };
}

function pointInRect(x, y, rect) {
  return x >= rect.x && x <= rect.x + rect.width &&
    y >= rect.y && y <= rect.y + rect.height;
}

function hitTest(x, y) {
  const layout = uiLayout();
  if (pointInRect(x, y, layout.no)) return "B";
  if (pointInRect(x, y, layout.paint)) return "A";
  return null;
}

function hoveredButton() {
  if (typeof pointer !== "function") return null;
  const cursor = pointer() || {};
  if (cursor.inside === false) return null;
  const hit = hitTest(Number(cursor.x), Number(cursor.y));
  return hit === "A" || hit === "B" ? hit : null;
}

function drawDecisionButton(rect, action, active, fill) {
  const border = Math.max(3, Math.round(view.height * .004));
  const x = rect.x;
  const y = rect.y;
  const height = rect.height;
  box(x, y, rect.width, height, ...uiInk);
  box(x + border, y + border, rect.width - border * 2, height - border * 2,
    ...(active ? fill : uiPaper));
  const inkColor = active ? [255, 255, 255] : uiInk;
  const actionSize = Math.max(38, Math.round(view.height * .052));
  const textWidth = (text, size) => String(text).length * size * .61;
  const centeredX = (text, size) => x + (rect.width - textWidth(text, size)) / 2;
  const centeredY = y + (height - actionSize) / 2;
  write(action, centeredX(action, actionSize), centeredY, actionSize, ...inkColor);
}

function drawMakingUi() {
  const layout = uiLayout();
  const { pad } = layout;
  const small = Math.max(22, Math.round(view.height * .027));
  const prompt = proposal.kind.toUpperCase();
  const edge = Math.max(1, Math.round(view.height * .002));
  for (const [x, y] of [[-edge, 0], [edge, 0], [0, -edge], [0, edge]])
    write(prompt, pad + x, pad + y, small, 255, 255, 255);
  write(prompt, pad, pad, small, ...uiInk);
  const controls = controlLocale();
  const hover = hoveredButton();
  drawDecisionButton(layout.no, controls.noAction,
    hover === "B" || previousButtons.includes("B") ||
      previousButtons.includes("ArrowLeft"), noHover);
  drawDecisionButton(layout.paint, controls.paintAction,
    hover === "A" || previousButtons.includes("A") ||
      previousButtons.includes("ArrowRight"), paintHover);
}

function acceptProposal() {
  if (mode !== "making" || !proposal) return;
  if (proposal.family === "effect") {
    if (proposal.effect.kind === "blur") {
      if (supportsOrderedEffects()) {
        acceptedEffects.push({ kind: "blur", after: acceptedMarks.length,
          radius: blurRadius() });
      } else {
        const softened = softenMarks(acceptedMarks);
        acceptedMarks.splice(0, acceptedMarks.length, ...softened);
      }
    } else {
      const transformed = applyEffect(acceptedMarks, proposal.effect);
      acceptedMarks.splice(0, acceptedMarks.length, ...transformed);
    }
  } else {
    acceptedMarks.push(animatedMark(proposal));
  }
  nextProposal();
}

function rejectProposal() {
  if (mode !== "making") return;
  rejectedCount++;
  nextProposal();
}

function toggleMode() {
  mode = mode === "making" ? "finished" : "making";
}

function boot() {
  syncView();
  const seed = typeof nopaintSeed === "string"
    ? nopaintSeed : "nopaint-xbox-line-v1";
  randomState = hashSeed(seed);
  acceptedMarks.length = 0;
  acceptedEffects.length = 0;
  proposalNumber = 0;
  rejectedCount = 0;
  mode = "making";
  previousButtons = [];
  proposalFrame = 0;
  decisionHeld = false;
  nextProposal();
}

function sim() {
  syncView();
  const down = typeof gamepad === "function" ? gamepad(0)?.down || [] : [];
  const pressed = (button) => down.includes(button) && !previousButtons.includes(button);
  const groupDown = (buttons, values = down) => buttons.some((button) =>
    values.includes(button));
  const released = (buttons) => !groupDown(buttons) && groupDown(buttons, previousButtons);
  const noButtons = ["ArrowLeft", "B"];
  const paintButtons = ["ArrowRight", "A"];
  const paintingButtons = ["Canvas"];
  if (pressed("Menu") || pressed("View")) toggleMode();
  else if (released(paintingButtons)) toggleMode();
  else if (released(paintButtons))
    mode === "finished" ? toggleMode() : acceptProposal();
  else if (released(noButtons) && mode === "making") rejectProposal();
  decisionHeld = mode === "making" &&
    (groupDown(noButtons) || groupDown(paintButtons) || groupDown(paintingButtons));
  if (mode === "making" && !decisionHeld) proposalFrame++;
  previousButtons = down.slice();
}

function paint() {
  if (mode === "making" && proposal.family === "effect") {
    if (proposal.effect.kind === "blur") {
      if (supportsOrderedEffects()) {
        renderMarks(acceptedMarks, [...acceptedEffects,
          { kind: "blur", after: acceptedMarks.length, radius: blurRadius() }]);
      } else renderMarks(softenMarks(acceptedMarks));
    } else renderMarks(applyEffect(acceptedMarks, proposal.effect));
  } else renderPainting();
  if (mode !== "making") return;
  if (proposal.family === "brush") drawMark(animatedMark(proposal));
  drawMakingUi();
}

function act(button) {
  // The native host also sends edge events. Polling gamepad() is authoritative,
  // as it is in oskiewar, so act remains side-effect free.
  void button;
}

function leave() {}

function snapshot() {
  return {
    mode,
    proposalNumber,
    proposalFrame,
    decisionHeld,
    rejectedCount,
    accepted: acceptedMarks.map(cloneMark),
    effects: acceptedEffects.map((effect) => ({ ...effect })),
    proposal: proposal ? cloneMark(proposal) : null,
    controls: controlLocale(),
    hover: hoveredButton(),
    view: { ...view },
  };
}
