import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import test from "node:test";
import { qrcode } from
  "../../../system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs";
import { decodeDump, dumpRows } from
  "../../../system/netlify/functions/oskiewar-dump.mjs";

const source = await readFile(new URL("../oskiewar.js", import.meta.url), "utf8");
const webShell = await readFile(new URL("../mac-test.html", import.meta.url), "utf8");
const reelRenderer = await readFile(new URL(
  "../marketing/render.mjs", import.meta.url), "utf8");
const replayOven = await readFile(new URL(
  "../marketing/replay-oven.mjs", import.meta.url), "utf8");
const frameDriverSource = await readFile(new URL("../frame-driver.mjs", import.meta.url));
const socialRenderer = await readFile(new URL(
  "../render-social-preview.mjs", import.meta.url));
const socialManifest = JSON.parse(await readFile(new URL(
  "../social/manifest.json", import.meta.url), "utf8"));
const socialPoster = await readFile(new URL(
  "../social/oskiewar-title.jpg", import.meta.url));
const socialVideo = await readFile(new URL(
  "../social/oskiewar-title.mp4", import.meta.url));
const lithDeploy = await readFile(new URL(
  "../../../lith/deploy.fish", import.meta.url), "utf8");
const lithWebhook = await readFile(new URL(
  "../../../lith/webhook.sh", import.meta.url), "utf8");
const activeCursor = await readFile(new URL(
  "../../../system/public/aesthetic.computer/cursors/active.svg",
  import.meta.url), "utf8");
const uiSource = await readFile(new URL(
  "../../../system/public/aesthetic.computer/lib/ui.mjs",
  import.meta.url), "utf8");
const nativeApp = await readFile(new URL("../../native-bios/App.cpp", import.meta.url), "utf8");
const nativeShellApp = await readFile(new URL(
  "../../../apple/oskiewar/Sources/OskiewarApp.swift", import.meta.url),
  "utf8");
const pieceLog = await readFile(new URL(
  "../../../system/netlify/functions/piece-log.mjs", import.meta.url), "utf8");
// Stage furniture rides build flags; its tests sleep while the flags are off
// and wake, unchanged, when they flip back.
const PLATFORM = /const PLATFORM = true/.test(source);
const WIND_FLAG = /const WIND_FLAG = true/.test(source);

const limbPartsForTest = ["left-arm", "right-arm", "left-leg", "right-leg"];

// `triangleHost` picks which of the three drawing entries the host offers:
// "triangles3d" is the Xbox BIOS batch, "triangle3d" the per-face 3D call, and
// "triangle" the flat 2D fallback the web shell ships with.
function createFight(startImmediately = true, enterGame = true,
  platform = "xbox-uwp", roundBridge = null,
  viewport = { width: 1920, height: 1080 }, livePublisher = null,
  drumVoice = null, triangleHost = "triangle3d", startUs = 0) {
  let now = startUs;
  const signals = [];
  const replays = [];
  const liveFrames = [];
  const analyticsEvents = [];
  const telemetryEvents = [];
  const drums = [];
  const triangles = [];
  const boxes = [];
  const lines = [];
  let hostErrorStatus = "";
  const pads = [0, 1].map(() => ({ connected: true, down: [], leftX: 0, leftY: 0 }));
  const noOp = () => {};
  const drawTriangle = (...values) => {
    for (const value of values.slice(0, 6))
      assert.ok(Number.isFinite(value) && Math.abs(value) <= 32768);
    triangles.push(values);
  };
  const drawTriangle3d = (...values) => {
    for (const value of values.slice(0, 9))
      assert.ok(Number.isFinite(value) && Math.abs(value) <= 32768);
    triangles.push(values);
  };
  const batches = [];
  const drawTriangles3d = (buffer, count) => {
    assert.ok(buffer instanceof Float32Array);
    assert.equal(buffer.byteLength % (12 * Float32Array.BYTES_PER_ELEMENT), 0);
    assert.ok(count > 0 && count <= buffer.length / 12 && count <= 8192);
    batches.push({ buffer, count });
    for (let at = 0; at < count * 12; at += 12)
      drawTriangle3d(...buffer.subarray(at, at + 12));
  };
  const drawLine = (...values) => lines.push(values);
  const fight = new Function(
    "runtime", "gamepad", "capabilities", "telemetry", "gameSignal", "saveReplay", "publishLive", "analytics", "drum", "wipe", "box", "line", "triangle", "triangle3d", "triangles3d", "write", "systemWrite", "gameView",
    `${source}\nreturn { boot, sim, paint, playDrum, captureClientError, drawDetachedPart, clientErrorState: () => clientError, clientErrorDetailState: () => clientErrorDetail, errorReportStatus, errorRestartSeconds, combatLegend, commandFade, dummyPopLine, filledDisc, spectatorCode, runShadow, glyphColor, contrastShadow, stateDumpRows, dumpTokens, dumpTokenInk, clientErrorDumpState: () => ({ url: clientErrorDumpUrl, modules: clientErrorQr ? clientErrorQr.getModuleCount() : 0 }), controlLocale, animatedTitleColor, comicGlyphAdvance, handleWidth, displayTheme, players, ball, balls, bullets, grenades, gunPickups, swordPickups, grenadePickups, bodyTrees, treeFruit, detachedParts, runnerWorldGeometry, fighterAnimationPhase, runnerDistanceToPoint, segmentSegmentClosest, meleeLimbContact, damagePart, isPogo, isHeadOnly, resultCardText, pacificTimeLabel, projectedBallRadius, deathCinematicState: () => deathCinematic ? { ...deathCinematic, age: deathCinematicAge() } : null, disableBall: () => { ballEnabled = false; for (const item of balls) item.active = false; }, enableBall: (index = 0) => { ballEnabled = true; const item = balls[index]; item.active = true; item.serveAt = 0; item.safeUntil = 0; item.safePlayers = 0; }, setWind: (value) => { windAcceleration = value; }, setDebugHitboxes: (value) => { debugHitboxes = Boolean(value); }, debugState: () => debugHitboxes, windState: () => ({ direction: windDirection, mph: windMph }), nextRound: () => resetRound(runtime().monotonicUs, false), knockOut: () => killPlayer(players[1], 0, runtime().monotonicUs, "KO"), selfBallDummy: () => killPlayer(players[1], 1, runtime().monotonicUs, "BALLED"), startAttack: (kind) => startMelee(players[0], kind, runtime().monotonicUs), bootFirstBall: () => bootBall(ball, players[0], runtime().monotonicUs), wackBall: () => { players[0].attackKind = "KICK"; returnBall(ball, players[0], runtime().monotonicUs, false); }, shieldBall: () => returnBall(ball, players[0], runtime().monotonicUs, true), crossWackBall: (contact = 1) => crossWackBall(ball, players.map((player) => ({ player, contact })), runtime().monotonicUs), enterGame: () => enterGame(runtime().monotonicUs), shellState: () => ({ mode: shellMode }), startFight: () => { shellMode = "GAME"; selecting = false; players[1].npc = false; players[1].bot = false; applyRoster(players[1], 2); startReplay(runtime().monotonicUs); matchBallType = "soccer"; resetRound(runtime().monotonicUs, true); }, startFightAgainst: (kind) => startFightAgainst(kind, runtime().monotonicUs), palSelect: () => PAL_SELECT, titleToyState: () => ({ title: titleToys.map((toy) => ({ ...toy })), prompt: promptToys.map((toy) => ({ ...toy })), bounce: promptBounce }), selectionState: () => ({ selecting, step: selectionStep, cursor: selectionCursor, ready: selectionReady.slice() }), selectionOptions: () => selectionOptions().map((option) => ({ kind: option.kind, label: option.fighter.handle, disabled: Boolean(option.disabled) })), cameraState: () => ({ cameraWidth, cameraCenter, cameraCenterY, cameraAspect, stageRight, stageTop, stageBottom, viewHeight, cameraContainFloor, doll: { width: cameraDoll.width, target: { ...cameraDoll.target }, position: { ...cameraDoll.position }, perspective: cameraDoll.perspective, roll: cameraDoll.roll } }), screenBounds: () => players.map((player) => runnerScreenBounds(player, runtime().monotonicUs / 1e6)), dumpTokens, dumpTokenInk, drawCornerCrops, playerStatLines, playerHandleLayout, statStackHeight, setBallKind: (type) => { matchBallType = type; resetBalls(runtime().monotonicUs); }, ballTypeState: () => matchBallType, seriesBallType, seriesState: () => seriesName, sessionState: () => sessionName, selectionLayout: selectionTouchLayout, actionSafeRect, hudSafeRect, projectPoint, terrainSpan, stageGeometry: () => ({ platformY, platformLeft, platformRight, floorY, ceilingY, worldLeft, worldRight, worldNear, worldFar }), platformTable: () => platforms.map((rung) => ({ ...rung })), frameRect: () => fighterFrameRect(), roundState: () => ({ roundResult, roundElapsedUs, matchOver }), viewerState: () => ({ active: Boolean(roundViewer), mode: roundViewerMode, status: roundViewerStatus, name: matchName }), instantReplayState: () => instantReplay ? { active: true, paused: instantReplay.paused, cursor: instantReplay.cursor, frames: instantReplay.frames.length, speed: instantReplay.speed, action: instantReplay.action } : { active: false }, replayFrameCount: () => roundReplayFrames.length, inputPadDown: (index) => inputPads[index]?.down?.slice() || [], startSelfPlay: () => startSelfPlay(runtime().monotonicUs), selfPlayState: () => selfPlay, replayActionCurve, replayRampStep, startInstantReplay: (now) => startInstantReplay(now) };`
  )(
    () => ({ monotonicUs: now, unixMs: 1785870000000 + Math.floor(now / 1000),
      simCount: Math.floor(now / 16667), paintCount: 0,
      clientErrorReportStatus: hostErrorStatus }),
    (index = 0) => ({ ...pads[index], down: pads[index].down.slice() }),
    () => ({ platform, inputFamily: platform === "xbox-uwp" ? "xbox"
      : platform === "touch" ? "touch"
      : platform === "mouse" ? "mouse" : "keyboard" }),
    (event, detail) => telemetryEvents.push([event, detail]),
    (...signal) => signals.push(signal), (payload) => {
      replays.push(payload);
      return Promise.resolve(true);
    },
    (matchId, payload) => livePublisher
      ? livePublisher(matchId, payload)
      : liveFrames.push([matchId, JSON.parse(payload)]),
    (action, properties) => analyticsEvents.push([action, properties]),
    drumVoice || ((name, velocity, pan) => drums.push([name, velocity, pan])), noOp,
    (...values) => boxes.push(values), drawLine,
    drawTriangle, triangleHost === "triangle" ? undefined : drawTriangle3d,
    triangleHost === "triangles3d" ? drawTriangles3d : undefined,
    noOp, noOp, () => viewport
  );
  globalThis.__oskiewarRoundBridge = roundBridge;
  globalThis.__oskiewarAttractVariant = "still";
  const opponentOverride = globalThis.__oskiewarOpponent;
  if (opponentOverride === undefined) globalThis.__oskiewarOpponent = "dummy";
  fight.boot();
  globalThis.__oskiewarRoundBridge = null;
  delete globalThis.__oskiewarAttractVariant;
  if (opponentOverride === undefined) delete globalThis.__oskiewarOpponent;
  else globalThis.__oskiewarOpponent = opponentOverride;
  if (enterGame) fight.enterGame();

  const tick = (elapsedUs = 16667) => {
    now += elapsedUs;
    fight.sim();
  };
  const tap = (pad, button, gapUs = 70000) => {
    pads[pad].down = [button];
    tick();
    pads[pad].down = [];
    tick();
    tick(gapUs);
  };
  fight.disableBall();
  if (startImmediately) {
    fight.startFight();
    tick(3000001);
  }
  return { fight, pads, signals, replays, liveFrames, analyticsEvents,
    telemetryEvents, drums, triangles, batches, boxes, lines,
    tick, tap, now: () => now,
    setHostErrorStatus: (status) => { hostErrorStatus = status; } };
}

test("browser matches emit one category-only start milestone", () => {
  const { analyticsEvents } = createFight(true, false, "web");
  assert.deepEqual(analyticsEvents, [["match_started", {
    source_system: "browser",
    surface: "web",
    input_family: "keyboard",
    opponent_type: "local-player",
  }]]);
});

test("control copy follows the platform while every title says start", () => {
  const xbox = createFight(false, false, "xbox-uwp").fight.controlLocale();
  const mac = createFight(false, false, "macos").fight.controlLocale();
  const touch = createFight(false, false, "touch").fight.controlLocale();
  assert.equal(xbox.title, "start");
  assert.match(xbox.select, /A READY/);
  assert.equal(mac.title, "start");
  assert.match(mac.select, /P1 A\/D \+ SPACE/);
  assert.doesNotMatch(mac.select, /A READY/);
  assert.equal(touch.title, "start");
  assert.equal(touch.select, "");
  assert.equal(touch.replayPaused, "paused");
});

test("desktop Space kicks and Enter punches whatever the fighter carries", () => {
  assert.match(webShell, /\["Space", \[0, "A"\]\]/);
  assert.match(webShell, /\["Enter", \[0, "B"\]\]/);
  assert.match(webShell, /\["ShiftLeft", \[0, "X"\]\]/);
  assert.match(webShell, /\["ShiftRight", \[0, "X"\]\]/);
  assert.match(webShell, /\["AltLeft", \[0, "Y"\]\]/);
  assert.match(webShell, /\["AltRight", \[0, "Y"\]\]/);
  const { fight, pads, tick } = createFight();
  const player = fight.players[0];
  player.gunAmmo = 4;
  player.grenadeAmmo = 2;
  pads[0].down = ["A"];
  tick();
  assert.equal(player.attackKind, "KICK");
  assert.equal(player.gunAmmo, 4, "the kick button never spends ammo");
  assert.equal(player.grenadeAmmo, 2);
  assert.equal(fight.bullets.length, 0);
  assert.equal(fight.grenades.length, 0);
});

test("mobile controls live in the canvas rather than DOM buttons", () => {
  assert.doesNotMatch(webShell, /id="touch-controls"|data-key=/);
  assert.match(webShell, /function touchKeyAt\(point\)/);
  assert.match(source, /function drawTouchControls\(\)/);
  assert.match(source, /drawTouchControls\(\);/);
});

test("touch controls need no duplicate tutorial legend", () => {
  const legend = source.slice(source.indexOf("function drawControlLegend"),
    source.indexOf("function drawTouchControls"));
  assert.match(legend, /capabilities\(\)\.inputFamily === "touch"\) return/);
  assert.match(webShell, /\.touch-enabled #account/);
});

test("phone action buttons speak the canonical command stream", () => {
  const controls = source.slice(source.indexOf("function drawTouchControls"),
    source.indexOf("function keycapRunWidth"));
  assert.match(controls, /commandGlyph = \{ A: "\/", B: "\*", X: "\)", Y: "\+" \}/);
  assert.match(controls, /commandGlyph\[key\]/);
});

test("phone names shrink and the title clock fits beside the notch", () => {
  assert.match(webShell, /viewport-fit=cover/);
  assert.match(source, /const size = touch \? 24 : hudTypeSize/);
  assert.match(source, /const nameSize = touch \? 28 : compactLayout\(\) \? 38 : 54/);
  const clock = source.slice(source.indexOf("function hudClockBox"),
    source.indexOf("function hudStatusTray"));
  assert.match(clock, /const size = touch \? 16 : hudTypeSize/);
  assert.match(clock, /left: textRight - handleWidth\(label, size\)/);
  assert.match(clock, /function drawHudClock\(clock, y, ink, unixMs\)/);
  assert.match(clock, /const colors = \[syntax\[0\], ink, syntax\[1\], ink,/);
  assert.match(clock, /const seconds = \(\(unixMs \/ 1000\) % 60 \+ 60\) % 60/);
  assert.match(clock, /const filled = seconds \/ 60 \* segments/);
});

test("the iOS shell can pulse debug without inventing another input", () => {
  assert.match(webShell, /globalThis\.__oskiewarToggleDebug = \(\) =>/);
  assert.match(webShell, /keys\[0\]\.add\("View"\)/);
  assert.match(webShell, /keys\[0\]\.delete\("View"\)/);
});

test("every combat button is reachable from a keyboard on both pads", () => {
  const keymap = /const keyMap = new Map\(\[([\s\S]*?)\]\);/.exec(webShell)[1];
  const bound = [...keymap.matchAll(/\[(\d), "([A-Za-z]+)"\]/g)]
    .reduce((pads, [, pad, button]) => {
      (pads[pad] ||= new Set()).add(button);
      return pads;
    }, {});
  // Y is the item button; before this it had no key at all and the action
  // was unreachable in the browser.
  for (const pad of ["0", "1"])
    for (const button of ["A", "B", "X", "Y", "ArrowUp", "ArrowDown",
      "ArrowLeft", "ArrowRight"])
      assert.ok(bound[pad]?.has(button),
        `pad ${Number(pad) + 1} cannot press ${button}`);
});

// The web shell's controller plumbing runs inside the page, so lift the block
// out of the HTML and hand it a navigator whose buttons the test can press.
function createPadHost({ pads = [], touch = false, agent = "Mac" } = {}) {
  const opens = webShell.indexOf("    // Browsers hand out gamepad indices");
  const closes = webShell.indexOf("    function noiseSource(");
  assert.ok(opens > 0 && closes > opens, "the web shell's pad block moved");
  const keys = [new Set(), new Set()];
  const listeners = new Map();
  const status = { textContent: "" };
  const host = new Function("navigator", "keys", "touchEnabled", "matchMedia",
    "location", "document", "addEventListener", "mouseFighting",
    `${webShell.slice(opens, closes)}
     return { scanPads, sampleGamepads, gamepad, controllers, capabilities,
       seats: () => padSeats.slice() };`)(
    { userAgent: agent, getGamepads: () => pads },
    keys, touch, () => ({ matches: false }), { search: "" },
    { querySelector: () => status,
      body: { classList: { contains: () => false } } },
    (type, handler) => listeners.set(type, handler), false);
  return { ...host, keys, pads, status,
    connect: (pad) => listeners.get("gamepadconnected")({ gamepad: pad }),
    disconnect: () => listeners.get("gamepaddisconnected")({}) };
}

function fakePad(index, { mapping = "standard", pressed = [],
  axes = [0, 0, 0, 0] } = {}) {
  return { index, mapping, connected: true, axes,
    id: `Xbox Wireless Controller ${index}`,
    buttons: Array.from({ length: 16 }, (unused, at) =>
      ({ pressed: pressed.includes(at), value: pressed.includes(at) ? 1 : 0 })) };
}

test("a browser pad takes a player seat from whatever index it landed on", () => {
  // Browsers recycle freed slots, so the pad the player is holding is often
  // not at getGamepads()[0] or [1].
  const host = createPadHost({ pads: [null, null, fakePad(2, { pressed: [0] }),
    null, fakePad(4, { pressed: [2] })] });
  host.sampleGamepads();
  assert.deepEqual(host.seats(), [2, 4]);
  assert.deepEqual(host.gamepad(0).down, ["A"]);
  assert.deepEqual(host.gamepad(1).down, ["X"]);
  assert.equal(host.capabilities().inputFamily, "xbox");
  assert.equal(host.controllers()[0].name, "Xbox Wireless Controller 2");
});

test("a seated pad keeps its player across connects and drops", () => {
  const host = createPadHost({ pads: [null, null, null, fakePad(3)] });
  host.sampleGamepads();
  assert.deepEqual(host.seats(), [3, null]);
  host.pads[1] = fakePad(1);
  host.connect(host.pads[1]);
  assert.deepEqual(host.seats(), [3, 1], "player one does not slide to pad 1");
  // Player two walks away mid-match; player one stays exactly where they were.
  host.pads[1] = null;
  host.disconnect();
  assert.deepEqual(host.seats(), [3, null]);
  host.pads[0] = fakePad(0, { pressed: [1] });
  host.sampleGamepads();
  assert.deepEqual(host.seats(), [3, 0]);
  assert.deepEqual(host.gamepad(0).down, []);
  assert.deepEqual(host.gamepad(1).down, ["B"]);
});

test("a non-standard pad is refused instead of mis-mapped", () => {
  const host = createPadHost({ pads: [fakePad(0, { mapping: "", pressed: [0] })] });
  host.sampleGamepads();
  assert.deepEqual(host.seats(), [null, null]);
  assert.deepEqual(host.gamepad(0).down, []);
  assert.equal(host.capabilities().inputFamily, "keyboard");
  host.connect(host.pads[0]);
  assert.match(host.status.textContent, /non-standard layout/);
  assert.equal(host.controllers()[0].standard, false);
});

test("a gamepad and a keyboard can share the fight", () => {
  const host = createPadHost({ pads: [fakePad(0, { pressed: [3] })] });
  host.keys[1].add("A");
  host.sampleGamepads();
  assert.deepEqual(host.gamepad(0).down, ["Y"]);
  assert.deepEqual(host.gamepad(1).down, ["A"]);
  host.keys[0].add("ArrowLeft");
  host.sampleGamepads();
  assert.deepEqual(host.gamepad(0).down.sort(), ["ArrowLeft", "Y"]);
});

test("the web stick passes through to the one 0.48 gate the console reads", () => {
  const host = createPadHost({ pads: [fakePad(0, { axes: [.3, -.6, 0, 0] })] });
  host.sampleGamepads();
  const pad = host.gamepad(0);
  assert.equal(pad.leftX, .3, "no second deadzone squashes the raw axis");
  assert.equal(pad.leftY, .6, "screen up is stick up");
  assert.match(source, /Math\.abs\(pad\.leftX\) >= 0\.48/);
  assert.match(nativeApp, /std::abs\(state\.left_x\) >= \.48f/);
  const { fight, pads, tick } = createFight();
  pads[0].leftX = .3;
  tick();
  assert.equal(fight.players[0].vx, 0, "under the gate the fighter holds still");
  pads[0].leftX = .6;
  tick();
  assert.ok(fight.players[0].vx > 0);
});

test("a detected pad swaps the round legend onto gamepad wording", () => {
  const host = createPadHost({ pads: [fakePad(0)] });
  host.sampleGamepads();
  assert.equal(host.capabilities().inputFamily, "xbox");
  const { fight } = createFight(false, false, "xbox-uwp");
  assert.equal(fight.combatLegend(fight.players[0]),
    "LEFT,RIGHT MOVE   RIGHT,RIGHT >> DASH   A KICK   B PUNCH   X SHIELD   Y USE ITEM   STICK JUMP");
});

test("every instructional keyboard key uses the shared gray keycap renderer", () => {
  assert.match(source, /function selectionControlKeys\(\)/);
  assert.match(source, /\[\["A", "D"\], "SELECT"/);
  assert.match(source, /\[\["A", "D"\], "SCRUB"/);
  assert.match(source, /\["Q", "REPLAY", "Y"\]/);
  assert.match(source, /drawCenteredKeycapRun\(controls/);
  assert.match(source, /drawCenteredKeycapRun\(replayControlKeys/);
  assert.match(source, /drawCenteredKeycapRun\(replayOfferKeys/);
  assert.doesNotMatch(source, /typeWrite\(controls, viewCenterX/);
  assert.doesNotMatch(source, /typeWrite\(replayControl, viewCenterX/);
  const streamSource = source.slice(source.indexOf("function drawCommandStream"),
    source.indexOf("function drawFightIntro"));
  assert.match(streamSource, /LEFT: "<", RIGHT: ">", UP: "\^", DOWN: "v"/);
  assert.match(streamSource, /"\/": "A", "\*": "B", "\)": "X"/);
  assert.doesNotMatch(streamSource, /keyboardCaps|drawKeycap\(entry\.text/);
  assert.match(streamSource, /typeWrite\(entry\.text, cursor, y, size, \.\.\.glyphInk\)/);
  assert.doesNotMatch(streamSource, /drawPadButton\(entry\.text/);
});

test("player-controlled play keeps its top-left key guide after start", () => {
  assert.match(source, /const dummyGuideDurationUs = 150000000/);
  assert.match(source, /if \(dummyGuideStartedAt === null && players\[1\]\.npc && !players\[1\]\.bot\)/);
  assert.match(source, /run\.monotonicUs - dummyGuideStartedAt < dummyGuideDurationUs/);
  assert.match(source, /if \(!selfPlay\) drawControlLegend\(titleInk\)/);
  const resetSource = source.slice(source.indexOf("function resetRound"),
    source.indexOf("function fighterFrameRect"));
  assert.doesNotMatch(resetSource, /dummyGuideStartedAt\s*=/);
});

test("the round intro names what each button does", () => {
  const { fight } = createFight(false, false);
  const locale = fight.controlLocale();
  for (const action of ["KICK", "PUNCH", "SHIELD", "USE ITEM"])
    assert.match(locale.combat, new RegExp(action));
  // X reads as whatever the hand is carrying.
  const player = fight.players[0];
  player.gunAmmo = 3;
  assert.match(fight.combatLegend(player), /WHIP/);
  assert.doesNotMatch(fight.combatLegend(player), /PUNCH/);
  player.gunAmmo = 0;
  player.grenadeAmmo = 2;
  assert.match(fight.combatLegend(player), /BASH/);
  player.grenadeAmmo = 0;
  assert.match(fight.combatLegend(player), /PUNCH/);
});

test("web canvas forwards touch coordinates to the fighter selector", () => {
  assert.match(webShell, /globalThis\.__oskiewarTouch = \{ taps: selectTaps,/);
  assert.match(webShell, /selectTaps\.push\(point\)/);
  assert.match(webShell, /logicalWidth \/ bounds\.width/);
});

test("web selector uses the AC precise cursor and mouse hover bridge", () => {
  assert.match(webShell,
    /cursor: url\("\/aesthetic\.computer\/cursors\/precise\.svg"\) 12 12, auto/);
  assert.match(webShell, /canvas\.addEventListener\("pointermove"/);
  assert.match(webShell, /pointer: \{ x: 0, y: 0, active: false \}/);
  assert.match(webShell,
    /cursor: url\("\/aesthetic\.computer\/cursors\/active\.svg"\) 12 12, pointer/);
  assert.match(webShell, /button:hover, input:hover, label:hover/);
  assert.doesNotMatch(webShell, /body:has\(/);
  assert.match(webShell, /syncSelectionCursor\(\);/);
  assert.doesNotMatch(activeCursor, /<circle|<ellipse/);
  assert.match(activeCursor, /M13 2v3 M13 21v3 M2 13h3 M21 13h3/);
  assert.match(uiSource, /event\?\.cursor\?\.\(hoveredButtons\.size \? "active" : "precise"\)/);
  assert.doesNotMatch(webShell, /if \(!touchEnabled\) return;/);
  assert.match(source, /function selectionHover\(/);
  assert.match(source, /box\(rect\.x, rect\.y \+ rect\.height - \(hovered \? 8 : 5\)/);
  // A wordmark letter is grabbable, so the pointer says so over one.
  assert.match(webShell, /titleHover \|\| titleGlyph >= 0/);
});

test("web relayout observes live viewport element resizing", () => {
  assert.match(webShell,
    /new ResizeObserver\(resizeCanvas\)\.observe\(document\.documentElement\)/);
  assert.match(source, /syncGameView\(\);/);
  assert.match(webShell, /window\.devicePixelRatio \|\| 1/);
  assert.match(webShell, /context\.setTransform\(density, 0, 0, density, 0, 0\)/);
});

test("update-ready panel automatically reloads at the round boundary", () => {
  assert.doesNotMatch(webShell, /id="auto-update"/);
  assert.match(webShell, /update is ready/);
  assert.match(webShell, /pieceScreen === "title" && previousPieceScreen !== "title"/);
  assert.match(webShell, /if \(updateAvailable\)/);
  assert.match(webShell, /__oskiewarTouch\?\.screen === "title"/);
});

test("web client errors post stateful reports and expose the server receipt", () => {
  assert.match(webShell, /async function postClientError\(message\)/);
  assert.match(webShell, /fetch\("\/api\/piece-log"/);
  assert.match(webShell, /phase: "error"/);
  assert.match(webShell, /clientErrorReportStatus = `posted to server/);
  assert.match(webShell, /frameRate: 60, clientErrorReportStatus/);
});

test("start cue uses cached ElevenLabs speech with echo and a rising beep", () => {
  assert.match(webShell, /Cached ElevenLabs female:3 rendition/);
  assert.match(webShell, /tts-cache\/4f0fe9fc3486f07488f3d20685a8b635d1e8a3aad77f00c378f89f70aa77cf23\.mp3/);
  assert.match(webShell, /\[1, \.3, \.12\]/);
  assert.match(webShell, /index \* 155/);
  assert.match(webShell, /frequency\.exponentialRampToValueAtTime\(930/);
  assert.match(source, /typeof titleVoice === "function"/);
  assert.match(source, /typeof titleBeep === "function"/);
});

test("an older native spectator boundary cannot stop a match", () => {
  let attempts = 0;
  const livePublisher = () => {
    attempts++;
    throw new RangeError("invalid oskiewar live payload");
  };
  const { fight, tick } = createFight(false, false, "xbox-uwp", null,
    { width: 1920, height: 1080 }, livePublisher);
  assert.doesNotThrow(() => fight.startFight());
  assert.doesNotThrow(() => tick());
  assert.equal(attempts, 1);
  assert.doesNotThrow(() => fight.nextRound());
  assert.equal(attempts, 1);
});

test("oskiewar typography uses the packaged KidLisp Comic Relief face", () => {
  assert.match(source, /typeof comicWrite === "function"/);
  assert.match(source, /comicGlyphAdvance/);
  assert.match(source, /String\(text\)\.toLowerCase\(\)/);
  assert.match(source, /player\.handleColors\?\.map\(runShadow\)/);
  assert.match(source, /const hudTypeSize = 42/);
  assert.match(source, /const timerSize = timedRound \? hudTypeSize/);
});

test("colored glyph runs share Comic Relief advances across every host", () => {
  const { fight } = createFight(false, false);
  assert.ok(Math.abs(fight.comicGlyphAdvance("@", 100) - 93.1) < .001);
  assert.ok(Math.abs(fight.comicGlyphAdvance("i", 100) - 28) < .001);
  assert.equal(Math.round(fight.handleWidth("@fifi", 100) * 10) / 10, 250.7);
  assert.match(source,
    /cursor \+= comicGlyphAdvance\(characters\[index\], size\)/);
  assert.doesNotMatch(source, /handle\[index\] === "@" \? \.88 : \.58/);
});

test("web theme follows the system while arena color still follows Los Angeles", () => {
  assert.match(webShell,
    /matchMedia\("\(prefers-color-scheme: light\)"\)\.matches \? "light" : "dark"/);
  assert.match(source, /const sun = losAngelesSun\(\)/);
  assert.match(source,
    /caps\.platform === "web" \|\| caps\.platform === "macos"/);
  assert.match(source,
    /return \{ \.\.\.sun, light: caps\.colorScheme === "light" \? 1 : 0 \}/);
  assert.match(source, /visualTheme = displayTheme\(\)/);
});

test("tab title animates playful phoneme spacing", () => {
  assert.match(webShell,
    /\["oskiewar", "oskie war", "os ki ewar", "osk ie war",/);
  assert.match(webShell, /document\.title = tabTitles\[index\]/);
  assert.match(source, /Math\.sin\(t \* \.63 \+ index \* 1\.71\)/);
});

test("Open Graph uses a landscape fallback and silent vertical title loop", () => {
  for (const tag of ["og:image", "og:image:secure_url", "og:image:type",
    "og:video", "og:video:secure_url", "og:video:type", "twitter:image"])
    assert.match(webShell, new RegExp(`(?:property|name)="${tag}"`));
  assert.match(webShell, /content="video\/mp4"/);
  assert.match(webShell, /property="og:image:width" content="1200"/);
  assert.match(webShell, /property="og:image:height" content="630"/);
  assert.match(webShell, /property="og:video:width" content="720"/);
  assert.match(webShell, /property="og:video:height" content="1280"/);
  assert.equal(socialManifest.theme, "light");
  assert.equal(socialManifest.videoWidth, 720);
  assert.equal(socialManifest.videoHeight, 1280);
  assert.equal(socialManifest.durationSeconds, 4);
  assert.equal(socialManifest.audio, false);
  assert.match(source, /const socialTitleSize = Math\.min\(220,/);
  assert.match(source,
    /\(stageRight - stageLeft - 56\) \/ handleWidth\(title, 1\)/);
  assert.equal(socialPoster[0], 0xff);
  assert.equal(socialPoster[1], 0xd8);
  assert.equal(socialVideo.subarray(4, 8).toString(), "ftyp");
  // A floor against an empty or black clip, not a quality bar — burn-to-burn
  // compression varies with the attract scene the clock deals.
  assert.ok(socialVideo.length > 60000);
  const expectedBuild = createHash("sha256").update(source).update(webShell)
    .update(frameDriverSource).update(socialRenderer).digest("hex").slice(0, 16);
  assert.equal(socialManifest.build, expectedBuild);
  assert.match(lithDeploy, /render-social-preview\.mjs --check/);
  assert.match(lithWebhook, /render-social-preview\.mjs --check/);
});

test("title letters carry distinct colors that animate between palettes", () => {
  const { animatedTitleColor } = createFight(false, false).fight;
  const firstFrame = Array.from({ length: 8 }, (_, index) =>
    animatedTitleColor(index, 0, 0));
  assert.equal(new Set(firstFrame.map((color) => color.join(","))).size, 8);
  assert.notDeepEqual(animatedTitleColor(0, 0, 0),
    animatedTitleColor(0, 1.25, 0));
  for (const time of [-.25, 0, 99.25]) {
    for (const light of [0, .5, 1]) {
      for (const channel of animatedTitleColor(5, time, light))
        assert.ok(channel >= 0 && channel <= 255);
    }
  }
});

test("title and fighter select inherit the root light or dark theme", () => {
  assert.match(source,
    /const menuArena = mixColor\(\[7, 10, 26\], \[235, 241, 248\], visualTheme\.light\)/);
  assert.match(webShell, /@media \(prefers-color-scheme: light\)/);
  assert.match(source, /drawTitleScreen\(t, menuInk, transitionAge\)/);
  assert.match(source, /drawSelectionScreen\(t, menuInk, menuPanel\)/);
});

// Pal select is deprecated, not deleted: you are your own handle, so there is
// nobody to pick. The two-step screen stays whole behind one flag while the
// entry UI is still moving.
test("pal select is flagged out of the build but kept intact", () => {
  const { fight } = createFight(false, false);
  assert.equal(fight.palSelect(), false);
  assert.equal(fight.selectionState().selecting, false);
  assert.match(source, /const PAL_SELECT = false/);
  assert.match(source, /if \(PAL_SELECT && selecting\)/);
  assert.match(source, /if \(PAL_SELECT\) beginSelect\(now\)/);
  // The screen it guards is still all there, ready to be switched back on.
  assert.match(source, /"pick your pal" : "who are you fighting\?"/);
  assert.match(source, /const stepText = \(selectionStep \+ 1\) \+ "\/2"/);
  assert.doesNotMatch(source, /READY TO FIGHT|STANDING BY/);
});

test("title shows a live Pacific clock and version timestamp", () => {
  const { fight } = createFight(false, false);
  assert.match(fight.pacificTimeLabel(1785870000000),
    /^\d{1,2}:\d{2}:\d{2}$/);
  // The number itself is the release tool's to enforce — it must equal the
  // file's commit count at deploy time; here it only has to exist.
  assert.match(source, /const buildVersion = \d+/);
  assert.match(source, /const clock = hudClockBox\(titleUnixMs\)/);
  assert.match(source, /drawHudClock\(clock, safe\.top \+ 2, ink, titleUnixMs\)/);
  assert.match(source, /const version = "v" \+ buildVersion/);
});

test("web camera follows landscape, 16:9, and portrait viewports", () => {
  for (const [viewport, expectedWidth] of [
    [{ width: 2560, height: 1080 }, 2560],
    [{ width: 1920, height: 1080 }, 1920],
    [{ width: 608, height: 1080 }, 608],
  ]) {
    const { fight } = createFight(false, false, "web", null, viewport);
    const state = fight.cameraState();
    assert.equal(state.stageRight, expectedWidth);
    assert.equal(state.viewHeight, 1080);
    assert.equal(state.cameraAspect,
      expectedWidth / (state.stageBottom - state.stageTop));
    assert.doesNotThrow(() => fight.paint());
  }
});

test("hardware insets stand the layout off the occluded screen edges", () => {
  // A phone's home indicator and notch arrive through gameView as insets —
  // env() in a browser, native injection in the iOS shell. The stage, the
  // HUD safe frame, and the touch clusters all stand off by them; a control
  // under the home indicator is a control the system swallows.
  const flat = createFight(false, false, "touch", null,
    { width: 499, height: 1080 }).fight;
  const inset = createFight(false, false, "touch", null,
    { width: 499, height: 1080,
      inset: { top: 74, right: 0, bottom: 44, left: 0 } }).fight;
  const flatState = flat.cameraState();
  const insetState = inset.cameraState();
  assert.equal(insetState.stageTop, flatState.stageTop + 74);
  assert.equal(insetState.stageBottom, flatState.stageBottom - 44);
  assert.ok(inset.hudSafeRect().top >= 74 + 10);
  assert.ok(inset.hudSafeRect().bottom <= 1080 - 44 - 10);
  assert.doesNotThrow(() => inset.paint());
  // The clusters shift in the game's painter and in the shell's hit test as
  // one arithmetic — drawn buttons and touched buttons must agree.
  assert.match(source,
    /const cy = viewHeight - 140 - viewInset\.bottom/);
  assert.match(source, /const dpadX = 130 \+ viewInset\.left/);
  assert.match(source,
    /const actionX = viewWidth\(\) - 130 - viewInset\.right/);
  assert.match(webShell,
    /const cy = logicalHeight - 140 - logicalInset\.bottom/);
  assert.match(webShell, /\[130 \+ logicalInset\.left,/);
  assert.match(webShell,
    /\[logicalWidth - 130 - logicalInset\.right,/);
  assert.match(webShell, /__oskiewarSafeInsets/);
  assert.match(nativeShellApp, /safeAreaInsetsDidChange/);
  assert.match(nativeShellApp, /__oskiewarSafeInsets/);
});

test("fighters start three tiles apart astride the cube's center", () => {
  // Tiles 3 and 6: mirrored across the middle, three tiles clear of the
  // corner weapons so the bell never hands one out by proximity.
  assert.match(source, /pad: 0, spawnX: tileCenterX\(3\), x: tileCenterX\(3\)/);
  assert.match(source, /pad: 1, spawnX: tileCenterX\(6\), x: tileCenterX\(6\)/);
  assert.match(source, /cameraWidth = 960;/);
});

test("custom web aspects derive play bounds around their own HUD", () => {
  const portrait = createFight(false, false, "touch", null,
    { width: 608, height: 1080 }).fight.cameraState();
  const landscape = createFight(false, false, "web", null,
    { width: 1920, height: 1080 }).fight.cameraState();
  assert.ok(portrait.stageBottom < portrait.viewHeight - 300);
  assert.ok(landscape.stageBottom > landscape.viewHeight - 160);
  assert.ok(portrait.cameraAspect > 0);
  assert.ok(landscape.cameraAspect > portrait.cameraAspect);
});

test("HUD safe area keeps a generous equal inset on all four screen edges", () => {
  for (const viewport of [
    { width: 2560, height: 1080 },
    { width: 1920, height: 1080 },
    { width: 608, height: 1080 },
  ]) {
    const { fight } = createFight(false, false, "web", null, viewport);
    const safe = fight.hudSafeRect();
    const gaps = [safe.left, safe.top, viewport.width - safe.right,
      viewport.height - safe.bottom];
    assert.deepEqual(gaps, [gaps[0], gaps[0], gaps[0], gaps[0]]);
    assert.ok(gaps[0] >= Math.floor(Math.min(viewport.width,
      viewport.height) * .049));
    assert.doesNotThrow(() => fight.paint());
  }
});

test("debug geometry renders above actors but behind screen UI", () => {
  const { fight, triangles } = createFight(false, false, "web", null,
    { width: 1920, height: 1080 });
  fight.startFight();
  fight.setDebugHitboxes(false);
  fight.paint();
  const plain = triangles.splice(0);
  fight.setDebugHitboxes(true);
  fight.paint();
  const debug = triangles.splice(0);
  assert.equal(fight.clientErrorState(), "");
  assert.ok(debug.length > plain.length);
  assert.doesNotMatch(source, /icons\.push\("bug"\)/);
  assert.match(source, /let debugHitboxes = false/);
  assert.match(source, /function drawCornerCrops[\s\S]*?filledCapsule/);
  assert.match(source, /triangleDepth = -1\.4;\n  drawDebugHitboxes/);
  const paintLayer = source.lastIndexOf("for (const renderable of renderables)");
  assert.ok(source.indexOf("drawBallHitboxes();", paintLayer) > paintLayer);
  assert.match(source, /drawSafeZones\(\);\n  triangleDepth = -1\.42/);
});

test("camera contains both complete fighters at every supported aspect", () => {
  for (const viewport of [
    { width: 2560, height: 1080 },
    { width: 1920, height: 1080 },
    { width: 608, height: 1080 },
  ]) {
    const { fight, tick } = createFight(false, false, "web", null, viewport);
    const assertContained = () => {
      fight.paint();
      const safe = fight.actionSafeRect();
      for (const bounds of fight.screenBounds()) {
        const label = `${viewport.width}x${viewport.height}`;
        assert.ok(bounds.left >= safe.left - .5,
          `${label}: ${bounds.left} crossed left edge ${safe.left}`);
        assert.ok(bounds.right <= safe.right + .5,
          `${label}: ${bounds.right} crossed right edge ${safe.right}`);
        assert.ok(bounds.top >= safe.top - .5,
          `${label}: ${bounds.top} crossed top edge ${safe.top}`);
        assert.ok(bounds.bottom <= safe.bottom + .5,
          `${label}: ${bounds.bottom} crossed bottom edge ${safe.bottom}`);
      }
    };

    fight.startFight();
    fight.players[0].x = 800;
    fight.players[0].y = 11800;
    fight.players[0].z = -560;
    fight.players[1].x = 11200;
    fight.players[1].y = 1300;
    fight.players[1].z = 560;
    tick();
    assertContained();

    fight.knockOut();
    tick();
    assertContained();
  }
});

test("camera zoom-out remains continuous as fighters approach safe edges", () => {
  const { fight, tick } = createFight(false, false, "web");
  fight.startFight();
  tick(3000001);
  for (let frame = 0; frame < 300; frame++) {
    tick();
    fight.paint();
  }
  let previousWidth = fight.cameraState().doll.width;
  let largestFrameRatio = 1;
  for (let frame = 0; frame < 100; frame++) {
    fight.players[0].x -= 18;
    fight.players[1].x += 18;
    tick();
    fight.paint();
    const state = fight.cameraState();
    largestFrameRatio = Math.max(largestFrameRatio,
      state.doll.width / previousWidth);
    previousWidth = state.doll.width;
    const safe = fight.actionSafeRect();
    for (const bounds of fight.screenBounds()) {
      assert.ok(bounds.left >= safe.left - .5,
        `frame ${frame}: left ${bounds.left} crossed ${safe.left}`);
      assert.ok(bounds.right <= safe.right + .5,
        `frame ${frame}: right ${bounds.right} crossed ${safe.right}`);
    }
  }
  assert.ok(largestFrameRatio < 1.08,
    `camera width jumped ${largestFrameRatio.toFixed(3)}x in one frame`);
});

// Settle a live fight into a given horizontal separation and report the frame.
function settleFraming(fight, tick, separation, frames = 420) {
  // Mid-room on the arena floor, read from the arena rather than written
  // down: a separation straddling a stale center clamps against a wall and
  // silently measures a narrower pair than the caller asked for.
  const stage = fight.stageGeometry();
  const middle = (stage.worldLeft + stage.worldRight) / 2;
  const left = middle - separation / 2;
  const right = middle + separation / 2;
  const floorY = stage.floorY;
  for (let frame = 0; frame < frames; frame++) {
    for (const [player, x] of [[fight.players[0], left], [fight.players[1], right]]) {
      player.x = x;
      player.y = floorY;
      player.vx = 0;
      player.vy = 0;
      player.grounded = true;
    }
    tick();
    fight.paint();
  }
  const safe = fight.actionSafeRect();
  const bounds = fight.screenBounds();
  return { state: fight.cameraState(), safe,
    height: Math.max(...bounds.map((box) => box.bottom)) -
      Math.min(...bounds.map((box) => box.top)),
    width: Math.max(...bounds.map((box) => box.right)) -
      Math.min(...bounds.map((box) => box.left)) };
}

test("close quarters keep both fighters dominant after the opening frame", () => {
  const { fight, tick } = createFight(false, false, "web", null,
    { width: 608, height: 1080 });
  fight.startFight();
  tick(3000001);
  const opening = settleFraming(fight, tick, 600);
  const close = settleFraming(fight, tick, 120);
  assert.ok(close.state.doll.width < opening.state.doll.width * .8,
    `close framing ${close.state.doll.width.toFixed(0)} against ` +
    `opening ${opening.state.doll.width.toFixed(0)}`);
  // The pair has to genuinely dominate the frame, not merely grow a little.
  // Which axis they dominate follows the stage: a television has height to
  // fill, a phone held upright has width — and forcing height-fill on a
  // phone by cropping their arms off is exactly what the old portrait lens
  // did wrong.
  const safeHeight = close.safe.bottom - close.safe.top;
  const safeWidth = close.safe.right - close.safe.left;
  assert.ok(Math.max(close.height / safeHeight, close.width / safeWidth) > .5,
    `fighters filled only ${(close.height / safeHeight * 100).toFixed(0)}% ` +
    `of action-safe height and ` +
    `${(close.width / safeWidth * 100).toFixed(0)}% of its width`);
  assert.ok(close.height > opening.height * 1.5,
    `close fighters ${close.height.toFixed(0)}px against ` +
    `opening ${opening.height.toFixed(0)}px`);
});

test("the pack fits the fighters' own rect on whichever axis binds", () => {
  const { fight, tick } = createFight(false, false, "web");
  fight.startFight();
  tick(3000001);
  const stacked = settleFraming(fight, tick, 120);
  // The cube's widest stance: a fighter pressed to each wall.
  const spread = settleFraming(fight, tick, 760);
  // Stacked runs out of height first, spread runs out of width first.
  const stackedSafe = stacked.safe.bottom - stacked.safe.top;
  const spreadSafe = spread.safe.right - spread.safe.left;
  assert.ok(stacked.height / stackedSafe > stacked.width /
    (stacked.safe.right - stacked.safe.left));
  assert.ok(spread.width / spreadSafe > spread.height /
    (spread.safe.bottom - spread.safe.top));
  // A near-square room on a 16:9 stage can never fill the wide axis the way
  // the 3000-unit tower could; half of action-safe is the cube's honest
  // ceiling with the camera's own margins on.
  assert.ok(spread.width > spreadSafe * .5,
    `spread pair filled only ${(spread.width / spreadSafe * 100).toFixed(0)}% ` +
    `of action-safe width`);
  const rect = fight.frameRect();
  assert.ok(rect.right - rect.left > 900 && rect.bottom - rect.top > 150);
});

test("the frame pans vertically with the action instead of hanging fixed", { skip: !PLATFORM }, () => {
  const { fight, tick } = createFight(false, false, "web");
  fight.startFight();
  tick(3000001);
  const stage = fight.stageGeometry();
  settleFraming(fight, tick, 240);
  const grounded = fight.cameraState().cameraCenterY;
  const ledge = (stage.platformLeft + stage.platformRight) / 2;
  for (let frame = 0; frame < 120; frame++) {
    for (const player of fight.players) {
      player.x = ledge + (player.pad ? 60 : -60);
      player.y = stage.platformY;
      player.vy = 0;
      player.grounded = true;
    }
    tick();
    fight.paint();
  }
  const raised = fight.cameraState().cameraCenterY;
  assert.ok(grounded - raised > (stage.floorY - stage.platformY) * .7,
    `frame only panned ${(grounded - raised).toFixed(0)} of ` +
    `${stage.floorY - stage.platformY}`);
  // And back down when they drop off, without ever aiming under the stage.
  const settled = settleFraming(fight, tick, 240);
  assert.ok(settled.state.cameraCenterY - raised >
    (stage.floorY - stage.platformY) * .7,
    `frame only panned back ${(settled.state.cameraCenterY - raised).toFixed(0)}`);
  assert.ok(settled.state.cameraCenterY <= stage.floorY);
});

test("the center platform is inside the 16:9 frame at match start", () => {
  const { fight, tick } = createFight(false, false, "web");
  fight.startFight();
  tick(3000001);
  for (let frame = 0; frame < 30; frame++) { tick(); fight.paint(); }
  const stage = fight.stageGeometry();
  const safe = fight.actionSafeRect();
  const ledge = fight.projectPoint(
    (stage.platformLeft + stage.platformRight) / 2, stage.platformY, 0);
  assert.ok(ledge.y > safe.top && ledge.y < safe.bottom,
    `platform projected to ${ledge.y.toFixed(0)} outside ` +
    `${safe.top}..${safe.bottom}`);
  // Comfortably inside, not clinging to the top edge.
  assert.ok(ledge.y > safe.top + (safe.bottom - safe.top) * .1);
});

test("a close lens against a wall still submits arena terrain", () => {
  const { fight, tick } = createFight(false, false, "web");
  fight.startFight();
  tick(3000001);
  for (const place of [[520, 660], [11340, 11480], [5940, 6060]]) {
    for (let frame = 0; frame < 240; frame++) {
      fight.players[0].x = place[0];
      fight.players[1].x = place[1];
      tick();
      fight.paint();
    }
    const stage = fight.stageGeometry();
    const span = fight.terrainSpan();
    const corners = [
      { x: span.left, y: stage.floorY, z: stage.worldNear },
      { x: span.right, y: stage.floorY, z: stage.worldNear },
      { x: span.right, y: stage.floorY, z: stage.worldFar },
      { x: span.left, y: stage.floorY, z: stage.worldFar },
      { x: span.left, y: span.top, z: stage.worldFar },
      { x: span.right, y: span.bottom, z: stage.worldFar },
    ].map((point) => fight.projectPoint(point.x, point.y, point.z));
    for (const corner of corners)
      assert.ok(Math.abs(corner.x) < 30000 && Math.abs(corner.y) < 30000,
        `terrain corner ${corner.x.toFixed(0)},${corner.y.toFixed(0)} would ` +
        `be culled at ${place[0]}`);
    // The visible frame must stay inside the span the terrain covers.
    const state = fight.cameraState();
    assert.ok(span.right - span.left >= state.doll.width ||
      (span.left === stage.worldLeft && span.right === stage.worldRight));
  }
});

test("the lens holds still while fighters idle at close range", () => {
  const { fight, tick } = createFight(false, false, "web");
  fight.startFight();
  tick(3000001);
  settleFraming(fight, tick, 140);
  let previous = fight.cameraState().doll.width;
  let widest = previous;
  let tightest = previous;
  for (let frame = 0; frame < 240; frame++) {
    fight.players[0].x = 2430;
    fight.players[1].x = 2570;
    fight.players[0].vx = 0;
    fight.players[1].vx = 0;
    tick();
    fight.paint();
    const width = fight.cameraState().doll.width;
    assert.ok(width / previous < 1.02 && previous / width < 1.02,
      `frame ${frame} zoomed ${(width / previous).toFixed(4)}x`);
    widest = Math.max(widest, width);
    tightest = Math.min(tightest, width);
    previous = width;
  }
  assert.ok(widest / tightest < 1.03,
    `idle breathing pumped the lens ${(widest / tightest).toFixed(3)}x`);
});

test("camera containment releases and zooms back in when fighters converge", () => {
  const { fight, tick } = createFight(false, false, "web");
  fight.startFight();
  tick(3000001);
  fight.players[0].x = 70;
  fight.players[1].x = 830;
  for (let frame = 0; frame < 120; frame++) { tick(); fight.paint(); }
  const wide = fight.cameraState().doll.width;
  // A 250 gap is the cube's honest close-up: tight enough to halve the wide
  // shot, wide enough that the floor-locked frame still clears the heads.
  fight.players[0].x = 325;
  fight.players[1].x = 575;
  for (let frame = 0; frame < 300; frame++) { tick(); fight.paint(); }
  const close = fight.cameraState().doll.width;
  assert.ok(close < wide * .55,
    `camera stayed latched at ${close.toFixed(1)} after ${wide.toFixed(1)}`);
  const safe = fight.actionSafeRect();
  for (const bounds of fight.screenBounds()) {
    assert.ok(bounds.left >= safe.left - .5);
    assert.ok(bounds.right <= safe.right + .5);
    assert.ok(bounds.top >= safe.top - .5);
    assert.ok(bounds.bottom <= safe.bottom + .5);
  }
});

test("gameplay camera has no procedural viewport shake", () => {
  assert.match(source,
    /const framedWidth = Math\.max\(naturalWidth, cameraContainFloor\)/);
  assert.match(source, /cameraContainFloor = lerp\(cameraContainFloor, naturalWidth/);
  assert.match(source, /position: \{ x: cameraCenter,/);
  assert.match(source, /roll: 0 \}, dt, 10/);
  assert.doesNotMatch(source, /const cameraTime = now \/ 1000000/);
  assert.doesNotMatch(source, /cameraContainTouchedAt/);
  assert.match(source, /const gameplayContainment = !roundResult &&/);
  assert.match(source,
    /runtime\(\)\.monotonicUs - roundStartedAt >= introDurationUs/);
});

test("intro camera introduces both faces before returning to the fight", () => {
  const { fight, tick } = createFight(false, false);
  fight.startFight();
  const left = fight.players[0].x;
  const right = fight.players[1].x;
  const middle = (left + right) / 2;
  let previous = fight.cameraState().doll.position.x;
  let largestStep = 0;
  let largestFrame = 0;
  const targets = [];
  for (let frame = 0; frame < 180; frame++) {
    tick();
    const state = fight.cameraState().doll;
    targets.push(state.target.x);
    if (frame && Math.abs(state.position.x - previous) > largestStep) {
      largestStep = Math.abs(state.position.x - previous);
      largestFrame = frame;
    }
    previous = state.position.x;
  }
  assert.ok(Math.abs(targets[55] - left) < 80,
    `first face target ${targets[55].toFixed(0)} missed ${left.toFixed(0)}`);
  assert.ok(Math.abs(targets[115] - right) < 80,
    `second face target ${targets[115].toFixed(0)} missed ${right.toFixed(0)}`);
  assert.ok(targets.slice(120).some((target) => Math.abs(target - middle) < 80),
    `intro never returned from ${targets[115].toFixed(0)} toward ` +
    `${middle.toFixed(0)}`);
  assert.ok(largestStep < 160,
    `intro camera stepped ${largestStep}px at frame ${largestFrame}`);
  assert.match(source, /function drawFightIntro/);
  assert.match(source, /const title = "oskiewar"/);
  assert.match(source, /const drawHeadName/);
  assert.match(source, /point\.y - radius - flashingSize \* 1\.28/);
  assert.match(source, /animatedTitleColor\(index, titleTime\)/);
  assert.match(source, /const inRoundIntro = !roundResult/);
  assert.match(source, /if \(introSeconds < 1\) \{\s*drawHeadName\(players\[0\]\)/);
  assert.match(source, /if \(introSeconds < 2\) \{\s*drawHeadName\(players\[1\]\)/);
  assert.doesNotMatch(source, /const andText = "and"/);
  assert.match(source, /function visibleHandle\(player\)/);
  assert.match(source, /return player\.name\.toLowerCase\(\)/);
  assert.doesNotMatch(source, /typeWrite\("v"/);
});

test("death camera closes on both fighters even when they are far apart", () => {
  const { fight, tick } = createFight(false, false, "web");
  fight.startFight();
  tick(3000001);
  fight.players[0].x = 400;
  fight.players[1].x = 4600;
  fight.knockOut();
  for (let frame = 0; frame < 120; frame++) {
    tick();
    fight.paint();
  }
  const state = fight.cameraState();
  assert.ok(Math.abs(state.doll.target.x - 2500) < 4,
    `death target followed one fighter: ${state.doll.target.x}`);
  assert.ok(state.doll.width < 11500,
    `death shot did not close in: ${state.doll.width}`);
  for (const bounds of fight.screenBounds()) {
    assert.ok(bounds.left >= -.5,
      `death bounds left ${bounds.left} left the viewport`);
    assert.ok(bounds.right <= 1920.5,
      `death bounds right ${bounds.right} left the viewport`);
  }
});

test("loss sequence freezes, enters killer cam, breaks the body, and returns", () => {
  const { fight, tick } = createFight();
  fight.knockOut();
  tick();
  assert.equal(fight.deathCinematicState().loserPad, 1);
  assert.equal(fight.deathCinematicState().winnerPad, 0);
  for (let frame = 0; frame < 28; frame++) tick();
  assert.equal(fight.cameraState().doll.perspective, 0);
  assert.match(source, /function drawBrokenRunner\(player, age\)/);
  assert.match(source, /function drawDeathFlash\(\)/);
  assert.match(source, /if \(age < \.86\)/);
  for (let frame = 0; frame < 80; frame++) tick();
  assert.ok(fight.deathCinematicState().age > 1.45);
});

// The killcam used to be shot from inside the winner's head, which meant the
// winner had to be culled out of the render list or their body would have
// filled the lens — so winning made your fighter disappear for three quarters
// of a second. The shot stands off their shoulder now and hides nobody.
test("the killcam keeps the winner in frame instead of deleting them", () => {
  const { fight, tick } = createFight();
  for (let frame = 0; frame < 90; frame++) { tick(); fight.paint(); }
  fight.knockOut();
  let framed = 0;
  for (let frame = 0; frame < 70; frame++) {
    tick();
    fight.paint();
    const cinematic = fight.deathCinematicState();
    if (!cinematic || cinematic.age < .11 || cinematic.age >= .86) continue;
    const winner = fight.screenBounds()[cinematic.winnerPad];
    assert.ok(winner.left >= 0 && winner.right <= 1920,
      `winner left the frame at ${cinematic.age.toFixed(2)}s: ` +
      `[${winner.left.toFixed(0)}, ${winner.right.toFixed(0)}] ` +
      "outside [0, 1920]");
    framed += 1;
  }
  assert.ok(framed > 30, `killcam only held for ${framed} frames`);
  // No fighter is ever taken out of the frame to make a camera move work, and
  // a fighter who is down draws as the broken body rather than as nothing.
  assert.doesNotMatch(source, /deathCinematic\?\.winnerPad === item\.pad/);
  assert.doesNotMatch(source, /if \(!player\.alive && !roundResult\) return;/);
});

test("a dismembered diving winner keeps animating through the KO shot", () => {
  const { fight, tick } = createFight();
  const winner = fight.players[0];
  winner.removedParts = ["left-arm", "right-leg"];
  winner.y = 1800;
  winner.vy = 420;
  winner.grounded = false;
  const before = winner.y;
  fight.knockOut();
  tick();
  assert.equal(winner.frozenGeometry, undefined);
  assert.ok(winner.y > before, "winner should continue the dive under neutral physics");
  assert.ok(fight.runnerWorldGeometry(winner, 0).segments
    .every((segment) => !winner.removedParts.includes(segment.part)));
  assert.match(source, /updatePlayer\(cinematicWinner,/);
  assert.match(source, /updateDetachedParts\(dt\)/);
  assert.match(source, /const victoryAmount = deathCinematic\?\.winnerPad/);
  assert.match(source, /Math\.sign\(defeated\.x - player\.x\)/);
});

test("hilly terrain shares one deterministic surface with physics and foot IK", () => {
  assert.match(source, /function terrainFloorAt\(x\)/);
  assert.match(source, /const broad = Math\.sin/);
  assert.match(source, /const detail = Math\.sin/);
  assert.match(source, /function drawTerrainSurface\(/);
  assert.match(source, /player\.y >= terrainFloorAt\(player\.x\)/);
  // Foot IK reads the same surface physics stands on — terrain plus rungs —
  // through one probe, clamped at the leg's own reach past a lip.
  assert.match(source,
    /const footPlant = \(footX\) => Math\.min\(surfaceYAt\(footX, feet\), feet \+ 20\)/);
  assert.match(source, /const leadGround = footPlant\(leadFoot\)/);
  assert.match(source, /const rearGround = footPlant\(rearFoot\)/);
  assert.match(source, /terrainFloorAt\(ball\.x\) - ball\.radius/);
  assert.match(source, /terrainPhase = terrainSeed\("oskiewar-physics-1-hills"\)/);
});

test("wind flag lives on the platform without an MPH HUD label", { skip: !WIND_FLAG }, () => {
  assert.match(source, /const poleBottom = platformY/);
  assert.match(source, /const poleX = \(platformLeft \+ platformRight\) \/ 2/);
  assert.match(source, /const poleZ = 480/);
  assert.match(source, /worldCapsule\(poleX, poleBottom/);
  assert.match(source, /function worldCapsule/);
  assert.doesNotMatch(source, /MPH/);
  assert.match(source, /const safe = actionSafeRect\(\)/);
  assert.doesNotMatch(source, /const span = cameraWidth \* 1\.18/);
  const flagSource = source.slice(source.indexOf("function drawWindFlag"),
    source.indexOf("function hashUnit"));
  assert.match(flagSource, /projectedTriangle\(flagPoints\[0\]/);
  assert.match(flagSource, /const poleInk = mixColor/);
  assert.match(flagSource, /const fabric = calm/);
  assert.match(flagSource, /\[92, 205, 255\], \[35, 112, 190\]/);
  assert.doesNotMatch(flagSource, /width \+ 5|\[255, 210, 54\]/);
  assert.match(source, /if \(shellMode === "GAME"\) drawWindFlag\(t, windInk\)/);
});

test("gun and grenade pickups are unlabeled world-scale objects", () => {
  const gunSource = source.slice(source.indexOf("function drawGunPickup"),
    source.indexOf("function drawBullet"));
  const grenadeSource = source.slice(source.indexOf("function drawGrenadePickup"),
    source.indexOf("function drawGrenade("));
  assert.doesNotMatch(gunSource, /typeWrite|circle\(/);
  assert.doesNotMatch(grenadeSource, /typeWrite|circle\(/);
  assert.match(gunSource, /rocket \? 58 : smg \? 42 : 16/);
  assert.match(gunSource, /rocket \? 62 : smg \? 48 : 20/);
  assert.match(grenadeSource, /9 \* scale/);
  assert.match(gunSource, /const metal = mixColor/);
  assert.match(gunSource, /const grip = mixColor/);
  assert.match(grenadeSource, /const shell = mixColor/);
  assert.doesNotMatch(gunSource, /outline|\[238, 197, 64\]/);
  assert.doesNotMatch(grenadeSource, /outline|radius \+ 3/);
  assert.match(gunSource, /\* 8/);
  assert.match(grenadeSource, /\* 8/);
});

test("bullets and their blink trails shrink with the projected world", () => {
  const bulletSource = source.slice(source.indexOf("function drawBullet"),
    source.indexOf("function drawGrenadePickup"));
  assert.match(bulletSource, /const scale = cameraScale\(\)/);
  assert.match(bulletSource, /Math\.max\(\.6, \(bullet\.rubber \? 4 : 3\) \* scale\)/);
  assert.match(bulletSource, /Math\.max\(\.9, 7 \* scale\)/);
  assert.doesNotMatch(bulletSource, /Math\.max\(4, 7 \* cameraScale\(\)\)/);
  assert.match(source, /function spitMouthPose\(player\)/);
  assert.match(source, /x: mouth\.x, y: mouth\.y, z: player\.z/);
  assert.match(source,
    /const toss = \{ x: mouth\.outX \* speed, y: mouth\.outY \* speed \}/);
  assert.match(source, /const aimLocalX = -direction/);
  assert.match(source, /const aimLocalY = -\.22/);
  assert.match(source, /const downwardAimY = Math\.abs\(aimY\)/);
  assert.match(source, /safeUntil: now \+ 220000/);
  assert.match(source,
    /fragment\.owner === bullet\.owner && now < bullet\.safeUntil/);
  assert.match(source, /const spitting = isHeadOnly\(player\)/);
  assert.match(source, /faceX \+ direction \* r \* \.12, mouthY/);
  assert.match(bulletSource,
    /Math\.max\(2, \(bullet\.heavy \? 10 : 7\) \* scale\)/);
});

test("ambient air is a simulated world-entity field", () => {
  const moteSource = source.match(/function airSeedValue[\s\S]*?\n}\n\nfunction drawSelectPortrait/)[0];
  assert.match(moteSource, /hashUnit\("oskiewar-air:"/);
  assert.match(moteSource, /function drawAmbientMotes/);
  assert.match(moteSource, /const count = 18/);
  assert.match(moteSource, /id: "air:" \+ index/);
  assert.match(moteSource, /kind: "air"/);
  assert.match(moteSource, /function airFlowAt\(position, seconds, phase\)/);
  assert.match(moteSource, /function simulateAirParticles\(dt, now\)/);
  assert.match(moteSource, /entity\.position\.x = wrapWorld/);
  assert.match(moteSource, /const point = projectPoint\(x, y, z\)/);
  assert.match(moteSource, /filledDisc\(point\.x, point\.y, radius, ink\)/);
  assert.doesNotMatch(moteSource, /actionSafeRect\(\)/);
  assert.doesNotMatch(moteSource, /filledCapsule\(/);
});

// The read-out reports what the host actually measured and nothing else. On the
// console that is a refresh rate, a render surface and per-stage timings; in a
// browser it is the frame rate alone, because the browser reports no surface and
// no timings, and printing zeroed milliseconds read as a stall. It stacks above
// the nameplate rather than across it.
test("debug HUD reports only measured performance, above the nameplate", () => {
  const perf = source.match(/function drawDebugPerformance[\s\S]*?\n}\n/)[0];
  assert.match(perf, /const refreshHz = Number\(run\.refreshHz\) \|\| 0/);
  // Measured frames lead everywhere; the display's refresh rate is a static
  // suffix, not the headline — a console read-out that only said "120 Hz"
  // answered a question nobody asked.
  assert.match(perf,
    /Math\.round\(displayFps \|\| 0\) \+ " fps" \+\n\s*\(refreshHz \? " @ "/);
  assert.match(perf, /renderWidth && renderHeight\n\s*\? "  ·  "/);
  assert.match(perf, /const frameMs = Number\(run\.frameMs\) \|\| 0/);
  assert.match(perf, /const timing = frameMs\n\s*\? "frame "/);
  assert.match(perf, /const lane = playerHandleLayout\(players\[0\], 0\)/);
  assert.match(perf, /let y = lane\.y - metaSize - 6/);
  assert.match(perf, /typeWrite\(label, lane\.x, y, size/);
  // Never anchored to the bottom edge again — that lane belongs to a handle.
  assert.doesNotMatch(perf, /safe\.bottom/);
  assert.doesNotMatch(source, /const gameLabel = "oskiewar"/);
});

// One fps read-out, in one place. The wordmark screen used to carry its own
// centered copy plus a caption measuring the title type, which sat on the
// kerning cells the debug scaffold was drawing.
test("the wordmark screen carries no performance or type read-out", () => {
  const title = source.match(/function drawTitleScreen[\s\S]*?\n}\n/)[0];
  assert.doesNotMatch(title, /fps/);
  assert.doesNotMatch(title, /"size "/);
});

test("the round timer cannot leak through the start screen", () => {
  assert.match(source, /let gameplayStarted = false/);
  assert.match(source, /function beginTraining[\s\S]{0,180}gameplayStarted = false/);
  const enter = source.slice(source.indexOf("function enterGame"),
    source.indexOf("function updateShell"));
  assert.match(enter, /gameplayStarted = true/);
  assert.match(source, /if \(shellMode === "GAME" && gameplayStarted\) \{/);
});

// Was "becomes player versus dummy": the second chair is no longer a fixed
// inert dummy but whichever sparring partner the session was dealt, so what
// this pins is that Start asks rather than assumes. The live hand-off is
// exercised in the fresh-training-session test above.
test("starting after action attract re-seats the dealt sparring partner", () => {
  const enter = source.slice(source.indexOf("function enterGame"),
    source.indexOf("function updateShell"));
  assert.match(enter, /players\[0\]\.bot = false/);
  assert.match(enter, /players\[1\]\.npc = true/);
  assert.match(enter, /const training = trainingOpponentKind\(\)/);
  assert.match(enter, /players\[1\]\.bot = training === "trainingbot"/);
  assert.match(enter, /players\[1\]\.spiderDummy = training === "spiderdummy"/);
});

test("raw left stick gives pupils subtle pre-deadzone gaze", () => {
  const face = source.slice(source.indexOf("function drawFace"),
    source.indexOf("function drawInventory"));
  assert.match(face, /const movementDeadzone = \.48/);
  assert.match(face, /const gazeNoiseFloor = \.015/);
  assert.match(face, /const rawMagnitude = Math\.hypot\(rawX, rawY\)/);
  assert.match(face, /movementDeadzone - gazeNoiseFloor/);
  assert.match(face, /lerp\(direction \* \.2, rawX, gazeAmount\)/);
  assert.doesNotMatch(face, /Math\.abs\(rawX\) >= deadzone/);
});

// The bug is a large bottom-center global-mode indicator. MIDI remains in its
// own clock-side peripheral tray.
test("debug starts hidden and shows its enlarged bottom-center bug", () => {
  assert.match(source, /let debugHitboxes = false/);
  assert.match(source, /function drawDebugBug\(x, y, scale = 1\)/);
  assert.match(source, /statusCellSize = \(\) => shellMode === "GAME" && debugHitboxes \? 56 : 26/);
  assert.match(source, /drawDebugBug\(viewCenterX\(\), top \+ statusCell \/ 2 \+ 2/);
  assert.match(source, /const top = safe\.bottom - statusCell/);
  assert.doesNotMatch(source, /icons\.push\("bug"\)/);
});

test("web title offers the shared account logout without entering a fight", () => {
  assert.match(webShell, /<button id="logout" type="button">log out<\/button>/);
  assert.match(webShell, /hi\.aesthetic\.computer\/v2\/logout/);
  assert.match(webShell, /target\.searchParams\.set\("client_id",/);
  // Auth0 checks returnTo against its allowed logout URLs, so the shell hands
  // back the origin it is actually being served from rather than a hardcoded
  // oskiewar.com that no preview or local host could ever match.
  assert.match(webShell, /target\.searchParams\.set\("returnTo", location\.origin \+ "\/"\)/);
  assert.match(webShell, /body\.social-preview #logout \{ display: none; \}/);
  assert.match(webShell, /event\.target instanceof HTMLButtonElement/);
});

test("web UI drums route through the unlocked procedural sound bank", () => {
  assert.match(webShell,
    /__oskiewarSfx\.drum\(name, amount, pan\)\) return/);
  assert.doesNotMatch(webShell,
    /if \(globalThis\.__oskiewarSfx\?\.unlocked\) return;/);
  assert.match(webShell, /oskiewarSfx\.drum\("hat", \.26, 0\)/);
});

test("debug off hides safe-zone boxes including frozen round impacts", () => {
  assert.match(source,
    /function drawSafeZones\(\) \{\n  if \(!debugHitboxes \|\| renderFlags\.hud === false\) return;/);
  // Two sessions left two different spellings of this line behind, neither of
  // them the one that shipped. The impact flash is debug-only, so `debugHitboxes`
  // gates it first and the round-result freeze only decides how long it lasts.
  assert.match(source,
    /const impactDebug = debugHitboxes && !roundResult && now < impactHitboxesUntil/);
});

// @jeffrey: "legs seem to get rly long when jumping on platforms". The
// grounded pose planted its feet with the bare terrain probe — the arena
// floor — so a fighter standing on a rung six storeys up stretched his shins
// all the way down to it. Feet now plant on the surface underfoot and a foot
// past a rung's lip dangles at the leg's own reach; these capsules double as
// hitboxes, so a leg is no longer shootable a storey below its owner.
test("feet plant on the rung underfoot instead of the storey below", { skip: !PLATFORM }, () => {
  const { fight } = createFight();
  const player = fight.players[0];
  const stage = fight.stageGeometry();
  const rung = fight.platformTable()[0];
  const legs = (x, y) => {
    player.x = x;
    player.y = y;
    player.vx = 0;
    player.grounded = true;
    const pieces = fight.runnerWorldGeometry(player, 0).segments
      .filter((piece) => piece.part.endsWith("-leg"));
    assert.ok(pieces.length >= 4);
    return pieces;
  };
  const span = (piece) => Math.hypot(piece.x2 - piece.x1, piece.y2 - piece.y1);
  const longestOnFloor = Math.max(
    ...legs(1500, stage.floorY).map(span));
  // Standing mid-rung: same silhouette as the floor, one storey up.
  for (const piece of legs((rung.left + rung.right) / 2, rung.y)) {
    assert.ok(span(piece) <= longestOnFloor + 1,
      `${piece.role} stretched to ${Math.round(span(piece))}`);
    assert.ok(Math.max(piece.y1, piece.y2) <= rung.y + 21,
      `${piece.role} reached below the rung`);
  }
  // Standing on the lip: the forward foot hangs at the leg's reach instead
  // of planting on the floor below.
  for (const piece of legs(rung.right, rung.y))
    assert.ok(Math.max(piece.y1, piece.y2) <= rung.y + 21,
      `${piece.role} reached past the lip`);
  // The low kick sweeps the ground underfoot, not the arena floor.
  assert.match(source,
    /const sweepY = lowKick\n\s*\? Math\.min\(surfaceYAt\(reachX, player\.y\), player\.y \+ 20\) - 5 : 0;/);
});

// The remote profiling loop @jeffrey asked for: flip layers off on a live
// session through the relay, price each one in the machine's own fps, then
// choose defaults from data. The flags arrive via the shell's publisher
// socket; the game reads them once per painted frame and defaults every
// missing flag to the full picture.
test("render experiment flags thin the frame and default to everything", () => {
  const { fight, lines, triangles, tick } = createFight(false, false);
  try {
    tick();
    fight.paint();
    const fullLines = lines.length, fullTriangles = triangles.length;
    lines.length = triangles.length = 0;
    globalThis.__oskiewarRenderFlags = { grass: false, shadows: false,
      dust: false, keys: false, bands: 1 };
    fight.paint();
    assert.ok(lines.length < fullLines,
      `grass and dust off should shed lines (${lines.length} vs ${fullLines})`);
    assert.ok(triangles.length < fullTriangles,
      `shadows, keys and bands off should shed faces ` +
      `(${triangles.length} vs ${fullTriangles})`);
    lines.length = triangles.length = 0;
    globalThis.__oskiewarRenderFlags = {};
    fight.paint();
    assert.equal(lines.length, fullLines);
    assert.equal(triangles.length, fullTriangles);
  } finally {
    delete globalThis.__oskiewarRenderFlags;
    delete globalThis.__oskiewarPlayerPans;
  }
  assert.match(webShell, /message\.type === "oskiewar:flags"/);
  assert.match(webShell, /globalThis\.__oskiewarRenderFlags = \{\n              \.\.\.globalThis\.__oskiewarRenderFlags, \.\.\.message\.content \}/);
});

test("fighter geometry connects the head and renders solid capsule joints", () => {
  const { fight } = createFight(false, false);
  const geometry = fight.runnerWorldGeometry(fight.players[0], 0);
  const connector = geometry.segments[0];
  assert.equal(connector.x1, geometry.head.x);
  assert.equal(connector.y1, geometry.head.y + geometry.head.radius * .78);
  assert.match(source, /function drawSkeletonSegments/);
  assert.match(source, /function filledCapsule/);
  assert.match(source, /function filledRing/);
  assert.match(source, /filledCapsule\(segment\.x1/);
  assert.doesNotMatch(source, /capWidth = Math\.max/);
  const skeletonSource = source.slice(source.indexOf("function drawSkeletonSegments"),
    source.indexOf("function runnerWorldGeometry"));
  assert.doesNotMatch(skeletonSource, /\bline\(/);
  assert.match(source, /const color = player\.color/);
});

test("native telemetry chunks every camera and player simulation frame", () => {
  const { fight, tick, telemetryEvents } = createFight(false, false);
  fight.startFight();
  tick(3000001);
  for (let frame = 0; frame < 70; frame++) tick();
  const chunks = telemetryEvents.filter(([event]) => event === "FIGHT_TRACE")
    .map(([, detail]) => JSON.parse(detail));
  assert.ok(chunks.length >= 2);
  assert.equal(chunks[0].format, "ac.oskiewar.frames");
  assert.match(chunks[0].round, /^ow-[a-z]{4,7}[0-9]{1,3}$/);
  assert.ok(chunks.some((chunk) => chunk.frames.length >= 55));
  for (const chunk of chunks) {
    for (const frame of chunk.frames)
      assert.equal(frame.length, chunk.schema.length);
  }
});

test("spectator QR uses the raw Meet-style round URL", () => {
  assert.match(source, /https:\/\/oskiewar\.com\/" \+ matchName/);
  assert.doesNotMatch(source, /https:\/\/oskiewar\.com\/watch\//);
  assert.doesNotMatch(source, /https:\/\/aesthetic\.computer\/oskiewar:/);
  assert.match(source, /triangleDepth = -1\.43/);
  assert.match(source, /screenRect\(left \+ 3, top \+ 3, size, size, shadow\)/);
  const qrSource = source.slice(source.indexOf("function drawSpectatorQr"),
    source.indexOf("function drawCornerCrops"));
  assert.doesNotMatch(qrSource, /matchName|labelTop/);
  assert.match(source, /spectatorCode\("https:\/\/oskiewar\.com"\)/);
  // Encoding is ~59ms; the same URL must reuse the code it already made.
  assert.match(source, /if \(url === spectatorQrUrl && spectatorQrCache\)/);
  // The wordmark now sits on top of a live fight, so the code is drawn after
  // it and only once the start flash is not covering the screen.
  assert.match(source,
    /drawTitleScreen\(t, menuInk, transitionAge\);\n    if \(transitionAge >= 0\) return;\n  \}\n  drawSpectatorQr\(titleInk\);/);
});

test("the spectator code is encoded once per URL, not once per round", () => {
  const { fight } = createFight();
  let encodes = 0;
  const realQr = globalThis.qrcode;
  globalThis.qrcode = (text) => {
    encodes += 1;
    return { url: text, getModuleCount: () => 25, isDark: () => false };
  };
  try {
    const room = "https://oskiewar.com/bafegu-dorimi-kunapo";
    const first = fight.spectatorCode(room);
    assert.equal(encodes, 1);
    // The same room must hand back the very code it already built.
    const again = fight.spectatorCode(room);
    assert.equal(encodes, 1, "the same URL re-encoded");
    assert.strictEqual(again, first);
    // A different room is a different code, and must be encoded.
    const next = fight.spectatorCode("https://oskiewar.com/sezzi7");
    assert.equal(encodes, 2);
    assert.notStrictEqual(next, first);
    // Returning to the first room encodes again -- only the last is kept,
    // which is all a single on-screen code needs.
    fight.spectatorCode(room);
    assert.equal(encodes, 3);
  } finally {
    if (realQr) globalThis.qrcode = realQr; else delete globalThis.qrcode;
  }
});

test("raw live and demo rooms run through the canonical game engine", () => {
  let deliver;
  const bridge = { name: "bafegu-dorimi-kunapo",
    start(listener) { deliver = listener; return () => {}; } };
  const { fight, tick } = createFight(false, false, "web", bridge);
  assert.equal(fight.viewerState().active, true);
  const live = { phase: "fight", fighters: [
    { name: "@FIFI", color: [209, 100, 216], x: 5100, y: 12000, z: 0,
      facing: 1, alive: true, grounded: true, ducking: false, blocking: false,
      score: 2, roundWins: 1, attack: "KICK" },
    { name: "@SAT", color: [130, 204, 213], x: 6900, y: 12000, z: 0,
      facing: -1, alive: true, grounded: true, ducking: false, blocking: false,
      score: 1, roundWins: 0, attack: "" },
  ], ball: { x: 6000, y: 11958, z: 0, radius: 42, active: true },
  camera: { x: 6000, y: 9600, width: 2200 }, wind: { direction: -1, mph: 14 },
  round: { remainingMs: 22000, result: "" } };
  deliver({ type: "state", content: live, live: true,
    roundName: bridge.name });
  tick();
  assert.equal(fight.viewerState().mode, "LIVE");
  assert.equal(fight.players[0].name, "@FIFI");
  assert.equal(fight.players[0].x, 5100);

  const checkpoint = Array(26).fill(0);
  checkpoint[1] = 5400; checkpoint[2] = 12000; checkpoint[6] = 3;
  checkpoint[9] = 6600; checkpoint[10] = 12000; checkpoint[14] = 3;
  checkpoint[17] = 6000; checkpoint[18] = 11958; checkpoint[22] = 1;
  checkpoint[23] = 6000; checkpoint[24] = 9600; checkpoint[25] = 2200;
  deliver({ type: "demo", roundName: bridge.name, content: {
    tickRate: 60, durationTicks: 1, roundIndex: 0,
    fighters: ["@FIFI", "@SAT"], rounds: [[0, 1, 8, 1]],
    checkpoints: [checkpoint], events: [], winner: "@FIFI",
  } });
  tick();
  assert.equal(fight.viewerState().mode, "DEMO");
  assert.equal(fight.players[0].x, 5400);
});

test("start button flashes yellow green lime before lifting off the fight", () => {
  const { fight, pads, signals, drums, tick } = createFight(false, false);
  assert.equal(fight.shellState().mode, "MENU");
  assert.equal(fight.selectionState().selecting, false);
  assert.match(source, /const prompt = "start"/);
  assert.match(source, /const button = titleButtonRect\(\)/);
  // The word floats: no panel, shadow, or edge is drawn behind it.
  assert.doesNotMatch(source, /box\(button\.x/);
  assert.match(source, /\[255, 238, 82\]/);
  pads[0].down = ["Y"];
  tick();
  assert.equal(fight.shellState().mode, "MENU");
  tick(700001);
  assert.equal(fight.shellState().mode, "GAME");
  assert.equal(fight.selectionState().selecting, false);
  // Start lifts the wordmark; it never restarts the round underneath it.
  assert.equal(fight.roundState().roundElapsedUs > 0, true);
  assert.deepEqual(drums.map(([name]) => name), ["hat"]);
  assert.ok(signals.some(([event]) => event === "select"));
  assert.match(source,
    /const flashPalette = \[\[255, 226, 48\], \[70, 224, 92\], \[181, 255, 48\]\]/);
  assert.match(source, /if \(transitionAge >= 0 \|\| socialPreview \|\|/);
});

test("title start is a bounded pointer button and negative space only duds", () => {
  assert.match(source, /function titleButtonRect\(\)/);
  assert.match(source, /titleButton = button/);
  assert.match(source, /titleHover = hovered/);
  assert.match(webShell, /if \(!inside\) \{[\s\S]{0,100}drum\("block", \.32, 0\)/);
  assert.match(webShell, /tapTitle\(point\)/);
  assert.doesNotMatch(webShell, /body\.selection-hover, body\.title-open/);
});

// Each letter of the wordmark is its own target and its own toy: hovering one
// swells and shudders that letter, its neighbours hold their place, and the
// word settles once the pointer leaves.
test("hovering a wordmark letter grows and wiggles that letter alone", () => {
  const { fight, tick } = createFight(false, false, "web", null,
    { width: 1920, height: 1080 });
  const pointer = { x: 0, y: 0, active: false };
  globalThis.__oskiewarTouch = { taps: [], pointer };
  const at = (x, y) => Object.assign(pointer, { x, y, active: true });
  const row = 1080 * .35 + 70;
  const run = (frames) => {
    for (let frame = 0; frame < frames; frame++) { tick(); fight.paint(); }
  };
  try {
    fight.paint();
    // Sweep the wordmark's row: every letter should answer somewhere, and
    // nothing outside the word should.
    const hits = new Map();
    for (let x = 0; x < 1920; x += 4) {
      at(x, row);
      fight.paint();
      const glyph = globalThis.__oskiewarTouch.titleGlyph;
      if (glyph < 0) continue;
      const span = hits.get(glyph) || [x, x];
      hits.set(glyph, [Math.min(span[0], x), Math.max(span[1], x)]);
    }
    assert.equal(hits.size, 8, "oskiewar has eight separate letters to touch");
    at(0, row);
    fight.paint();
    assert.equal(globalThis.__oskiewarTouch.titleGlyph, -1);

    // A letter kicks the instant it is touched, then swells while held. Aim
    // at the middle of its cell: the letters drift, and an edge slides away.
    const [left, right] = hits.get(3);
    at((left + right) / 2, row);
    run(1);
    assert.ok(fight.titleToyState().title[3].kick > .9, "no shudder on contact");
    run(30);
    const held = fight.titleToyState().title;
    assert.ok(held[3].grow > .8, "the held letter never grew");
    assert.ok(held.every((toy, index) => index === 3 || toy.grow < .05),
      "a neighbour reacted to a letter that was not touched");

    // And it settles back once the pointer leaves the word.
    pointer.active = false;
    run(45);
    assert.ok(fight.titleToyState().title.every((toy) => toy.grow < .05));
  } finally {
    delete globalThis.__oskiewarTouch;
  }
});

test("hovering start bounces its letters and lets them fall back", () => {
  const { fight, tick } = createFight(false, false, "web", null,
    { width: 1920, height: 1080 });
  const pointer = { x: 0, y: 0, active: false };
  globalThis.__oskiewarTouch = { taps: [], pointer };
  const run = (frames) => {
    for (let frame = 0; frame < frames; frame++) { tick(); fight.paint(); }
  };
  try {
    fight.paint();
    const button = globalThis.__oskiewarTouch.titleButton;
    Object.assign(pointer, { x: button.x + button.width / 2,
      y: button.y + button.height / 2, active: true });
    run(30);
    assert.equal(globalThis.__oskiewarTouch.titleHover, true);
    assert.ok(fight.titleToyState().bounce > .9);
    // The letter under the pointer swells on top of the shared bounce.
    assert.ok(fight.titleToyState().prompt.some((toy) => toy.grow > .5));
    // The bounce is a wave: each letter takes it a beat after the one before.
    assert.match(source, /Math\.abs\(Math\.sin\(t \* 5\.4 - index \* \.62\)\)/);
    // The sharp offset shadow still rides every letter.
    assert.match(source,
      /typeWrite\(character, x \+ offset, y \+ offset, size, \.\.\.shadowInk\)/);
    pointer.active = false;
    run(60);
    assert.equal(globalThis.__oskiewarTouch.titleHover, false);
    assert.ok(fight.titleToyState().bounce < .05);
  } finally {
    delete globalThis.__oskiewarTouch;
  }
});

test("debug mode boxes every title glyph against its own advance", () => {
  const { fight, boxes, tick } = createFight(false, false);
  fight.paint();
  const quiet = boxes.length;
  fight.setDebugHitboxes(true);
  tick();
  fight.paint();
  const inked = (r, g, b) => boxes.slice(quiet)
    .filter((values) => values[4] === r && values[5] === g && values[6] === b);
  // Four sides around the whole wordmark, four around each of the eight
  // glyphs in "oskiewar", and one advance rule apiece.
  assert.equal(inked(92, 132, 255).length, 4);
  assert.equal(inked(255, 92, 116).length, 8 * 4);
  assert.equal(inked(116, 255, 184).length, 8);
  assert.match(source, /glyphCells\.push\(\[cursor \+ drift, titleY \+ bob, advance\]\)/);
  assert.match(source, /strokeBox\(titleX, titleY, titleWidth, titleSize/);
  fight.setDebugHitboxes(false);
});

test("active matches publish bounded phone spectator snapshots", () => {
  const { liveFrames, tick } = createFight();
  tick(50000);
  assert.ok(liveFrames.length > 0);
  const [matchId, frame] = liveFrames.at(-1);
  assert.match(matchId, /^ow-[a-z]{5,6}[0-9]{1,3}$/);
  assert.equal(frame.format, "ac.oskiewar.live");
  assert.equal(frame.fighters.length, 2);
  assert.ok(frame.camera.width >= 100);
  assert.ok(JSON.stringify(frame).length < 7168);
});

// @jeffrey plays oskiewar.com in Edge on an Xbox, where there are no devtools
// and the console's own AC_NATIVE_PROFILE line only reaches a Device Portal on
// the same LAN. The live socket is the one channel that already leaves the box,
// so the frame numbers ride out with the round — and have to stay small enough
// that they never crowd the relay's 8 KiB message cap.
test("live rounds carry the frame numbers the debug HUD prints", () => {
  const { liveFrames, tick } = createFight();
  tick(50000);
  const frame = liveFrames.at(-1)[1];
  assert.ok(Number.isInteger(frame.perf.fps));
  // This harness's host times no stages, and an unmeasured stage is left out
  // rather than published as a zero a reader would mistake for a stall.
  assert.deepEqual(Object.keys(frame.perf), ["fps"]);
  const measured = { ...frame,
    perf: { fps: 59, frameMs: 16.72, renderMs: 3.41, hz: 60 } };
  const cost = JSON.stringify(measured).length -
    JSON.stringify({ ...frame, perf: undefined }).length;
  assert.ok(cost <= 64, `the whole read-out costs ${cost} bytes on the wire`);
  assert.ok(JSON.stringify(measured).length < 7168);
});

// The browser used to answer every timing question with a zero, so the read-out
// printed nothing at all on the surface @jeffrey actually plays on. Both inputs
// are measurements the driver already takes — the rAF timestamp it is handed,
// and the paint cost it already clocks — so nothing here asks for a new reading.
test("the web shell times its own frames from the driver's profile", () => {
  const driver = String(frameDriverSource);
  assert.match(driver, /const profileWindow = 120/);
  assert.match(driver, /profileSpanMs \+= current - profileAt/);
  assert.match(driver, /profileRenderMs \+= stats\.lastRenderCostMs/);
  assert.match(driver, /stats\.refreshHz = stats\.frameMs > 0/);
  // A backgrounded tab returns with one span minutes long; the anchor is
  // dropped so that span is thrown away rather than averaged in.
  assert.match(driver, /const forgetProfileFrame = \(\) => \{ profileAt = 0; \}/);
  // The offline lane steps by hand, so its frame span means nothing and a reel
  // burn must never feed the profile.
  const offline = driver.slice(driver.indexOf("stepOffline()"),
    driver.indexOf("    start()"));
  assert.doesNotMatch(offline, /profileFrame\(/);
  assert.match(webShell, /frameMs: frames\?\.frameMs \|\| 0/);
  assert.match(webShell, /renderCpuMs: frames\?\.renderCpuMs \|\| 0/);
  assert.match(webShell, /refreshHz: frames\?\.refreshHz \|\| 0/);
  // A browser cannot time the compositor's present, so that stage stays out of
  // the row instead of printing a measured-looking 0.00ms.
  const perf = source.match(/function drawDebugPerformance[\s\S]*?\n}\n/)[0];
  assert.match(perf, /presentMs \? "  present "/);
});

// @jeffrey: "if we are in debug mode and reading telemetry on a device can we
// show a little agent icon to show our linked in connection on the telemetry of
// that round". Only a `role=agent` socket reaches the count — a phone that
// scanned the round QR is a viewer and lights nothing.
test("a linked telemetry agent marks the read-out it is reading", () => {
  // @jeffrey: "can the little agent icon be near the debug ladybug, not by
  // the fps" — the antenna keeps the bug company bottom-center, opposite the
  // session name it reports on, and the fps row carries numbers alone.
  const perf = source.match(/function drawDebugPerformance[\s\S]*?\n}\n/)[0];
  assert.doesNotMatch(perf, /drawAgentLink/);
  assert.match(source,
    /const agents = linkedAgents\(\);\n    if \(agents\) \{\n      const scale = statusCell \/ 34;\n      drawAgentLink\(viewCenterX\(\) - statusCell \* \.62 - 14 \* scale/);
  assert.match(source,
    /function linkedAgents\(\)[\s\S]{0,220}capabilities\(\)\.liveAgents/);
  assert.match(source, /function drawAgentLink\(x, y, scale = 1, count = 1\)/);
  // The shell keeps the count the relay hands the publisher, and clears it when
  // the room closes: every round is its own room, so a watcher has to rejoin.
  assert.match(webShell, /liveAgents: globalThis\.__oskiewarLiveAgents \|\| 0/);
  assert.match(webShell, /message\.type === "oskiewar:viewers"/);
  assert.match(webShell, /Number\(message\.content\?\.agents\) \|\| 0/);
  assert.match(webShell, /globalThis\.__oskiewarLiveAgents = 0/);
  // The publisher socket also carries the one instruction an agent may send
  // back — reload — and the shell obliges with the update poll's own safety:
  // now on the title, armed for the next title otherwise.
  assert.match(webShell, /message\.type === "oskiewar:reload"/);
  const reloadBranch = webShell.slice(
    webShell.indexOf('message.type === "oskiewar:reload"'),
    webShell.indexOf('socket.addEventListener("close"'));
  assert.match(reloadBranch,
    /__oskiewarTouch\?\.screen === "title"[\s\S]{0,140}location\.reload\(\)/);
  assert.match(reloadBranch, /updateAvailable = true/);
});

test("every round gets a new URL and tells spectators where the room moved", () => {
  const { fight, liveFrames, tick } = createFight();
  tick(50000);
  const firstId = liveFrames.at(-1)[0];
  fight.nextRound();
  const transition = liveFrames.find(([id, frame]) =>
    id === firstId && frame.nextRoundId);
  assert.ok(transition);
  tick(50000);
  const secondId = liveFrames.at(-1)[0];
  assert.notEqual(secondId, firstId);
  assert.equal(transition[1].nextRoundId, secondId);
  assert.equal(transition[1].seriesId, liveFrames.at(-1)[1].seriesId);
  assert.equal(liveFrames.at(-1)[1].previousRoundId, firstId);
});

// @jeffrey: "can every 'session' have an id that we can agentically
// telemetrically attach to — as opposed to every 'round'". Rounds are named
// and buried every thirty seconds; the app run keeps one pronounceable name
// from boot to quit and stamps it into every live frame it publishes.
test("the app run names itself once and stamps every live frame", () => {
  const { fight, liveFrames, tick } = createFight();
  const session = fight.sessionState();
  assert.match(session, /^[a-z]{4,7}[0-9]{1,3}$/);
  tick(50000);
  const [roundId, frame] = liveFrames.at(-1);
  assert.equal(frame.sessionId, "ow-" + session);
  assert.notEqual(frame.sessionId, roundId);
  fight.nextRound();
  assert.equal(fight.sessionState(), session);
});

// The session room is a debug facility, not an audience: it only publishes
// while the bug is lit, so a thousand visitors idling on the title screen
// cannot drain the relay's room pool — and with the bug lit, the title screen
// finally has something an agent can attach to.
test("a lit debug bug feeds the session room while nothing is scored", async () => {
  const { validateOskiewarLiveState } = await import(
    "../../../session-server/oskiewar-live-manager.mjs");
  const { fight, liveFrames, tick } = createFight(false, false);
  tick(50000);
  assert.equal(liveFrames.length, 0);
  fight.setDebugHitboxes(true);
  tick(50000);
  const sessionId = "ow-" + fight.sessionState();
  assert.ok(liveFrames.length > 0);
  const [room, frame] = liveFrames.at(-1);
  assert.equal(room, sessionId);
  assert.equal(frame.sessionId, sessionId);
  // A title-screen frame carries no series, no round and no demo link — an
  // empty name must stay off the wire rather than ride out as "ow-".
  assert.equal(frame.seriesId, undefined);
  assert.equal(frame.roundId, undefined);
  assert.equal(frame.replayUrl, undefined);
  assert.equal(frame.round.timed, false);
  assert.equal(frame.round.remainingMs, 0);
  // The relay's own validator is the door this frame has to walk through.
  assert.equal(validateOskiewarLiveState(frame), null);
  fight.setDebugHitboxes(false);
});

// The native shell owns exactly one publisher socket, so the session room and
// a live round can never publish side by side: the session room gets one
// hand-off frame naming the round, then goes quiet until the scoring stops.
test("a live round takes the socket and the session room learns where", () => {
  const { fight, liveFrames, tick } = createFight(false, false);
  fight.setDebugHitboxes(true);
  for (let step = 0; step < 3; step++) tick(260000);
  const sessionId = "ow-" + fight.sessionState();
  const idle = liveFrames.filter(([room]) => room === sessionId);
  assert.ok(idle.length >= 2);
  const sequences = idle.map(([, frame]) => frame.seq);
  assert.ok(sequences.every((seq, at) => at === 0 || seq > sequences[at - 1]));
  fight.enterGame();
  fight.startFight();
  for (let step = 0; step < 12; step++) tick(50000);
  const handoffs = liveFrames.filter(([room, frame]) =>
    room === sessionId && frame.nextRoundId);
  assert.equal(handoffs.length, 1);
  const roundId = handoffs[0][1].nextRoundId;
  assert.ok(liveFrames.some(([room]) => room === roundId));
  // Past its single hand-off frame the session room stays silent.
  assert.equal(liveFrames.filter(([room]) => room === sessionId).at(-1)[1]
    .nextRoundId, roundId);
  fight.setDebugHitboxes(false);
});

// The read-out survives the title screen now — debug mode reads the machine,
// not the match — and the session's name prints beside the bottom-center bug,
// where whoever wants to attach a telemetry agent can read it off the screen.
test("debug fps survives the title and the bug wears the session name", () => {
  const perf = source.match(/function drawDebugPerformance[\s\S]*?\n}\n/)[0];
  assert.doesNotMatch(perf, /shellMode/);
  assert.match(perf, /FIGHT_DEBUG_PERF/);
  assert.match(source,
    /typeWrite\(sessionName, viewCenterX\(\) \+ statusCell \* \.62/);
  const { fight, boxes, tick } = createFight(false, false);
  fight.setDebugHitboxes(true);
  tick();
  fight.paint();
  assert.ok(boxes.length > 0);
});

// Dummy play is free and anonymous, so there is no screen in front of it:
// booting lands you in the training fight with the wordmark floating on top.
test("the matchup card yields its seat to the wordmark", () => {
  const { fight, tick } = createFight(false, false);
  assert.equal(fight.shellState().mode, "MENU");
  // Entry keeps restarting training rounds, and each one counts itself off
  // with a card that names both fighters in the middle of the screen --
  // exactly where the wordmark sits.
  assert.match(source,
    /if \(counting && shellMode === "GAME"\)\n\s*drawFightIntro/);
  // Long enough to cross a round boundary and start another countdown.
  for (let frame = 0; frame < 600; frame += 1) {
    tick(40000);
    fight.paint();
  }
  assert.equal(fight.shellState().mode, "MENU", "entry left the wordmark");
  assert.equal(fight.clientErrorState(), "");
});

test("entry A/B tests cross-legged against a still standing face-off", () => {
  const { fight, pads, tick } = createFight(false, false);
  assert.equal(fight.shellState().mode, "MENU");
  const action = fight.players[0].name === "BOT";
  assert.equal(fight.players[1].name, action ? "BOT" : "DUMMY");
  assert.equal(fight.players[1].npc, true);
  assert.equal(fight.players[1].bot, action);
  assert.match(source, /hashUnit\(matchName\) < \.5 \? "still" : "action"/);
  assert.match(source, /if \(titleAttractMode === "action"\) \{/);
  assert.match(source, /player\.bot && shellMode === "GAME"/);
  assert.match(source, /titleAttractMode === "action"\)\n\s*drawFace/);
  // No countdown between arriving and moving: the intro is spent up front, so
  // the very first press walks the fighter while the wordmark is still up.
  const start = fight.players[0].x;
  pads[0].down = ["ArrowRight"];
  for (let frame = 0; frame < 8; frame++) tick();
  if (!action) assert.notEqual(fight.players[0].x, start);
  assert.equal(fight.shellState().mode, "MENU");
});

test("training rounds stay off the wire while the bot doorway is shut", () => {
  const { fight, replays, liveFrames, analyticsEvents, tick } =
    createFight(false, false);
  for (let frame = 0; frame < 60; frame++) tick();
  // Training has no series, so it publishes no match, no live frame, and no
  // replay — the things a handle will buy.
  assert.equal(fight.seriesState(), "");
  assert.deepEqual(replays, []);
  assert.deepEqual(liveFrames, []);
  assert.deepEqual(analyticsEvents, []);
  assert.match(source, /function startFightAgainst\(kind, now\)/);
  assert.match(source, /startFightAgainst\("dummy", now\)/);
});

test("selection cards use the shared comic typeface without readiness copy", () => {
  assert.match(source, /drawHandle\(label,/);
  assert.match(source, /typeWrite\("< back"/);
  assert.doesNotMatch(source, /READY TO FIGHT|STANDING BY|\? "READY" : "SELECT"/);
});

// The tap queue existed to drive the pal wheel. With that screen gone a tap
// on the canvas is the shell's business (mac-test's `tapTitle`) and the piece
// must not hold on to stale points once the fight is running.
test("portrait touch leaves no taps queued now that the wheel is gone", () => {
  const { fight, tick } = createFight(false, false, "touch", null,
    { width: 499, height: 1080 });
  fight.enterGame();
  globalThis.__oskiewarTouch = { taps: [] };
  try {
    assert.equal(fight.players[1].name, "DUMMY");
    globalThis.__oskiewarTouch.taps.push({ x: 250, y: 300 });
    tick();
    assert.equal(globalThis.__oskiewarTouch.taps.length, 0);
    assert.equal(fight.players[1].name, "DUMMY", "a tap chooses nobody");
  } finally {
    delete globalThis.__oskiewarTouch;
  }
});

// Kept whole behind PAL_SELECT: if the wheel comes back it comes back intact.
test("the pal wheel keeps one option in focus with neighbours receding", () => {
  for (const viewport of [{ width: 1920, height: 1080 },
    { width: 499, height: 1080 }, { width: 480, height: 900 }]) {
    const { fight } = createFight(false, false, "touch", null, viewport);
    fight.enterGame();
    const wheel = fight.selectionLayout();
    const focus = wheel.options.find((option) => option.slot === 0);
    assert.deepEqual(wheel.options.map((option) => option.slot), [-1, 0, 1]);
    assert.equal(focus.index, fight.selectionState().cursor);
    for (const side of wheel.options.filter((option) => option.slot !== 0)) {
      // Neighbours recede: smaller, clear of the focus, and on screen.
      assert.ok(side.width < focus.width);
      assert.ok(side.x + side.width <= focus.x + 1 ||
        side.x + 1 >= focus.x + focus.width);
      assert.ok(side.x >= 0 && side.x + side.width <= viewport.width);
    }
    assert.doesNotThrow(() => fight.paint());
  }
});

test("Menu returns a round to title while View or web Tab toggles debug geometry", () => {
  for (const pad of [0, 1]) {
    const { fight, tap } = createFight();
    assert.equal(fight.selectionState().selecting, false);
    tap(pad, "Menu");
    assert.equal(fight.shellState().mode, "MENU");
    assert.equal(fight.selectionState().selecting, false);
  }
  const { fight, tap } = createFight();
  assert.equal(fight.debugState(), false);
  tap(1, "View");
  assert.equal(fight.debugState(), true);
  assert.equal(fight.selectionState().selecting, false);
  tap(0, "View");
  assert.equal(fight.debugState(), false);
  assert.match(webShell, /\["Tab", \[0, "View"\]\]/);
});

// The bot is now reached through the one door auth will guard, not a menu.
test("bot is a direct opponent choice behind the one fight door", () => {
  const { fight } = createFight(false);
  assert.equal(fight.players[1].name, "DUMMY");
  fight.startFightAgainst("bot");
  assert.equal(fight.players[1].name, "BOT");
  assert.equal(fight.players[1].npc, true);
  assert.equal(fight.players[1].bot, true);
  assert.equal(fight.shellState().mode, "GAME");
  assert.equal(fight.selectionState().selecting, false);
  // Unlike training, the bot is a real match: it carries a series.
  assert.ok(fight.seriesState());
});

test("bot uses the player input and physics path to pursue and strike", () => {
  const { fight, tick } = createFight();
  const bot = fight.players[1];
  bot.npc = true;
  bot.bot = true;
  bot.name = "BOT";
  // Keep the bot off tile 9 — the corner pistol takes any limb within 90,
  // and an armed bot answers with a WHIP instead of the bare-handed strike
  // this test is listening for.
  fight.players[0].x = 150;
  bot.x = 650;
  tick();
  assert.ok(bot.vx < 0);
  fight.players[0].x = 480;
  bot.x = 600;
  bot.botAttackAt = 0;
  tick();
  assert.ok(bot.attackKind === "PUNCH" || bot.attackKind === "KICK");
});

// Every unbroken run of a held bot button, in frames, on both pads at once.
// A synthetic press only reads as human if it has a length and a release.
function botPressRuns(harness, frames) {
  const runs = [];
  const open = [new Map(), new Map()];
  for (let frame = 0; frame < frames; frame++) {
    harness.tick();
    for (const pad of [0, 1]) {
      const down = harness.fight.inputPadDown(pad);
      for (const button of down)
        if (!open[pad].has(button))
          open[pad].set(button, { pad, button, start: frame, frames: 0 });
      for (const [button, run] of [...open[pad]]) {
        if (down.includes(button)) run.frames += 1;
        else {
          runs.push(run);
          open[pad].delete(button);
        }
      }
    }
  }
  return runs;
}

test("a bot holds its jump long enough to land on the platform it chases", { skip: !PLATFORM }, () => {
  const harness = createFight();
  const { fight, tick } = harness;
  const stage = fight.stageGeometry();
  const bot = fight.players[1];
  const chased = fight.players[0];
  const ledge = (stage.platformLeft + stage.platformRight) / 2;
  bot.npc = true;
  bot.bot = true;
  bot.name = "BOT";
  bot.x = ledge;
  bot.botJumpAt = 0;
  let upFrames = 0;
  let landed = false;
  for (let frame = 0; frame < 90; frame++) {
    // Park the chased fighter on the ledge so the bot has a reason to climb.
    chased.x = ledge;
    chased.y = stage.platformY;
    chased.vy = 0;
    chased.grounded = true;
    tick();
    if (fight.inputPadDown(1).includes("ArrowUp")) upFrames += 1;
    if (bot.grounded && bot.y <= stage.platformY + .5) landed = true;
  }
  // A one-frame up is always a cut jump, and a cut jump tops out around 109
  // against a 240 ledge — the bot could never get there.
  assert.ok(upFrames >= 24, `up was held for only ${upFrames} frames`);
  assert.ok(landed, "the bot never reached the platform");
  assert.ok(stage.floorY - stage.platformY > 200);
});

test("every synthetic press has a human hold and a clean release", () => {
  const harness = createFight(false, false);
  harness.fight.startSelfPlay();
  harness.fight.disableBall();
  harness.tick(3000001);
  const runs = botPressRuns(harness, 480);
  assert.ok(runs.length > 12, `only ${runs.length} bot presses in eight seconds`);
  for (const run of runs)
    assert.ok(run.frames >= 4,
      `${run.button} on pad ${run.pad + 1} was held ${run.frames} frame(s)`);
  // Double-tap detection reads `lastRelease`, so a re-press always leaves a
  // gap the sampler can see.
  for (const pad of [0, 1])
    for (const button of new Set(runs.map((run) => run.button))) {
      const same = runs.filter((run) => run.pad === pad && run.button === button);
      for (let index = 1; index < same.length; index++) {
        const gap = same[index].start -
          (same[index - 1].start + same[index - 1].frames);
        assert.ok(gap >= 3,
          `${button} reopened after only ${gap} frame(s) up`);
      }
    }
  // Lengths are per action, not one house number: a stab is short and fixed,
  // a shield leans, and a pursuit renews for as long as it is chasing.
  const lengths = (buttons) => runs
    .filter((run) => buttons.includes(run.button)).map((run) => run.frames);
  const strikes = lengths(["A", "B"]);
  const shields = lengths(["X"]);
  const walks = lengths(["ArrowLeft", "ArrowRight"]);
  assert.ok(strikes.length && shields.length && walks.length,
    "the bots never struck, shielded, and chased in the same run");
  assert.deepEqual([...new Set(strikes)], [5], "a stab is one fixed length");
  assert.ok(Math.min(...shields) > Math.max(...strikes),
    "a shield should outlast a stab");
  assert.ok(Math.max(...walks) > Math.min(...walks),
    "a pursuit should renew rather than repeat one canned length");
});

test("bot presses reach the HUD command stream and the input read-out", () => {
  const { fight, pads, tick } = createFight();
  const bot = fight.players[1];
  bot.npc = true;
  bot.bot = true;
  bot.name = "BOT";
  fight.players[0].x = 5000;
  assert.deepEqual(pads[1].down, [], "nothing is plugged into pad two");
  for (let frame = 0; frame < 30; frame++) tick();
  const down = fight.inputPadDown(1);
  assert.ok(down.length > 0, "the pad the bot fighter feels reports nothing");
  assert.ok(bot.commandStream.some((entry) => entry.label === "LEFT"));
  assert.notEqual(bot.lastButton, "NONE");
  // drawCommandStream lights an entry only while its button is still down and
  // reads that from the same pad the fighter feels.
  const buttonFor = { LEFT: "ArrowLeft", RIGHT: "ArrowRight",
    UP: "ArrowUp", DOWN: "ArrowDown" };
  assert.ok(bot.commandStream.some((entry) =>
    down.includes(buttonFor[entry.label] || entry.label)),
    "no bot press was ever drawn as held");
  assert.doesNotThrow(() => fight.paint());
});

test("two fresh bot-versus-bot sims move and press identically", () => {
  const run = () => {
    const harness = createFight(false, false);
    harness.fight.startSelfPlay();
    harness.fight.disableBall();
    harness.tick(3000001);
    const trace = [];
    for (let frame = 0; frame < 300; frame++) {
      harness.tick();
      trace.push([harness.fight.inputPadDown(0).join("+"),
        harness.fight.inputPadDown(1).join("+"),
        ...harness.fight.players.map((player) => [
          Math.round(player.x * 1000), Math.round(player.y * 1000),
          Math.round(player.vx * 1000), Math.round(player.vy * 1000),
          player.stance, player.lastButton, player.score])]);
    }
    return JSON.stringify(trace);
  };
  assert.equal(run(), run());
});

test("self play runs bot against bot and says so on both nameplates", () => {
  const { fight, tick } = createFight(false, false);
  assert.equal(fight.selfPlayState(), false);
  fight.startSelfPlay();
  tick(3000001);
  assert.equal(fight.selfPlayState(), true);
  assert.equal(fight.shellState().mode, "GAME");
  assert.equal(fight.selectionState().selecting, false);
  for (const player of fight.players) {
    assert.equal(player.bot, true);
    assert.equal(player.npc, true);
  }
  // A self-play bot is called what it wears — a CSS color name — so the
  // nameplates teach the color book while staying impossible to mix up.
  for (const player of fight.players) {
    assert.match(player.name, /^[A-Z]+$/, "nameplate is a color word");
    assert.equal(player.colorName, player.name.toLowerCase());
  }
  assert.notEqual(fight.players[0].name, fight.players[1].name);
  assert.ok(!fight.players.some((player) => player.name.startsWith("@")),
    "a mechanical test must not fly a handle nobody is holding");
  assert.notDeepEqual(fight.players[0].color, fight.players[1].color);
  // Both sides are really driven, not just relabelled.
  for (let frame = 0; frame < 120; frame++) tick();
  for (const pad of [0, 1])
    assert.ok(fight.players[pad].commandStream.length > 0,
      `pad ${pad + 1} never pressed anything`);
  assert.doesNotThrow(() => fight.paint());
});

test("self play rolls rounds over forever with nobody at the controls", () => {
  const { fight, replays, tick } = createFight(false, false);
  fight.startSelfPlay();
  fight.disableBall();
  tick(3000001);
  const results = [];
  let previous = "";
  // Two full thirty-second rounds and the gaps around them.
  for (let frame = 0; frame < 2400; frame++) {
    tick(40000);
    const result = fight.roundState().roundResult;
    if (result && result !== previous) results.push(result);
    previous = result;
    // A title screen would end the run: nobody is there to press start.
    assert.notEqual(fight.shellState().mode, "MENU");
  }
  assert.ok(results.length >= 2, `only ${results.length} rounds finished`);
  for (const result of results) assert.match(result, /^([A-Z]+ WINS|TIE)/);
  assert.equal(fight.roundState().roundResult, "");
  assert.equal(fight.selfPlayState(), true);
  assert.ok(replays.length >= 1, "self play never published a round");
  for (const player of fight.players)
    assert.match(player.name, /^[A-Z]+$/, "nameplate is a color word");
  assert.notEqual(fight.players[0].name, fight.players[1].name);
});

test("self play cannot be reached by pressing through the entry fight", () => {
  const { fight, tap } = createFight(false, false);
  for (const button of ["ArrowRight", "ArrowLeft", "A", "B", "X", "Y", "Menu"])
    for (let press = 0; press < 3; press++) {
      tap(0, button);
      assert.equal(fight.selfPlayState(), false, `${button} press ${press}`);
    }
  assert.match(source, /if \(globalThis\.__oskiewarSelfPlay\) \{/);
  assert.equal(fight.selfPlayState(), false);
});

test("a harness can arm self play before boot", () => {
  globalThis.__oskiewarSelfPlay = true;
  try {
    const { fight, tick } = createFight(false, false);
    tick();
    assert.equal(fight.selfPlayState(), true);
    assert.equal(fight.shellState().mode, "GAME");
    for (const player of fight.players)
      assert.match(player.name, /^[A-Z]+$/, "nameplate is a color word");
    assert.notEqual(fight.players[0].name, fight.players[1].name);
  } finally {
    delete globalThis.__oskiewarSelfPlay;
  }
  assert.equal(createFight(false, false).fight.selfPlayState(), false);
});

test("perspective intro never submits invalid ground triangles", () => {
  const { fight, pads, triangles, tick } = createFight(false);
  pads[0].down = ["A"];
  tick();
  fight.paint();
  tick(500000);
  fight.paint();
  assert.ok(triangles.length > 0);
});

test("death camera culls balls that project outside native triangle bounds", () => {
  const { fight, tick } = createFight();
  fight.enableBall();
  fight.ball.x = 100;
  fight.ball.y = 11958;
  fight.ball.z = 0;
  fight.knockOut();
  tick();
  for (let frame = 0; frame < 120; frame++) {
    tick(16667);
    fight.paint();
  }
});

test("melee and movement edges emit bounded Ableton signals", () => {
  const { signals, tap } = createFight();
  tap(0, "A");
  assert.ok(signals.some(([event, player]) => event === "kick" && player === 0));
  tap(0, "B");
  assert.ok(signals.some(([event, player]) => event === "punch" && player === 0));
  tap(0, "ArrowRight");
  assert.ok(signals.some(([event, player, horizontal]) =>
    event === "move" && player === 0 && horizontal === 1));
});

// The armed-fighter matrix: Y and either shoulder spend an item, B takes the
// item's flavor, and A stays a kick no matter what is in the hand.
const armAndPress = (button, ammo = {}) => {
  const context = createFight();
  // Point-blank cube: at spawn distance a fired round reaches the other
  // fighter inside its first tick and vanishes into the hit. Lift the
  // second fighter out of the line of fire — this matrix is about what the
  // hand does, not what it hits.
  context.fight.players[1].y -= 600;
  context.fight.players[1].grounded = false;
  Object.assign(context.fight.players[0], ammo);
  context.pads[0].down = [button];
  context.tick();
  return context;
};

test("Y spends the held item and reaches for the gun before the grenade", () => {
  const armed = armAndPress("Y", { gunAmmo: 2, grenadeAmmo: 2 });
  assert.equal(armed.fight.bullets.length, 1);
  assert.equal(armed.fight.grenades.length, 0);
  assert.equal(armed.fight.players[0].gunAmmo, 1);
  assert.equal(armed.fight.players[0].grenadeAmmo, 2);
  const dry = armAndPress("Y", { gunAmmo: 0, grenadeAmmo: 2 });
  assert.equal(dry.fight.bullets.length, 0);
  assert.equal(dry.fight.grenades.length, 1);
  assert.equal(dry.fight.players[0].grenadeAmmo, 1);
  const empty = armAndPress("Y");
  assert.equal(empty.fight.bullets.length, 0);
  assert.equal(empty.fight.grenades.length, 0);
  assert.equal(empty.fight.players[0].attackKind, "");
});

test("either shoulder is a synonym for use item", () => {
  for (const shoulder of ["LeftShoulder", "RightShoulder"]) {
    const armed = armAndPress(shoulder, { gunAmmo: 2 });
    assert.equal(armed.fight.bullets.length, 1);
    assert.equal(armed.fight.players[0].gunAmmo, 1);
  }
});

test("B punches bare, whips a pistol, and bashes a grenade", () => {
  const bare = armAndPress("B");
  assert.equal(bare.fight.players[0].attackKind, "PUNCH");
  const gun = armAndPress("B", { gunAmmo: 3 });
  assert.equal(gun.fight.players[0].attackKind, "WHIP");
  assert.equal(gun.fight.players[0].gunAmmo, 3, "a whip is not a shot");
  assert.ok(gun.signals.some(([event, pad]) => event === "whip" && pad === 0));
  assert.deepEqual(gun.drums.at(-1), ["whoosh", 1.15, gun.drums.at(-1)[2]]);
  const grenade = armAndPress("B", { grenadeAmmo: 2 });
  assert.equal(grenade.fight.players[0].attackKind, "BASH");
  assert.equal(grenade.fight.players[0].grenadeAmmo, 2);
  assert.ok(grenade.signals.some(([event, pad]) =>
    event === "bash" && pad === 0));
  assert.deepEqual(grenade.drums.at(-1),
    ["kick", 1.3, grenade.drums.at(-1)[2]]);
});

test("a whip out-reaches a bash while a bash hits far harder", () => {
  const swing = (ammo, gap) => {
    const { fight, pads, tick, now } = createFight();
    fight.setWind(0);
    const attacker = fight.players[0];
    const target = fight.players[1];
    Object.assign(attacker, ammo);
    attacker.x = 6060 - gap;
    target.x = 6060;
    pads[0].down = ["B"];
    let tip = 0;
    let knock = 0;
    // Knockback decays every frame and the three kinds land on different
    // frames, so only the peak is a fair comparison.
    for (let frame = 0; frame < 12; frame++) {
      tick();
      knock = Math.max(knock, Math.abs(target.knockVx));
      const forearm = fight.runnerWorldGeometry(attacker, now() / 1e6)
        .segments.find((segment) => segment.role === "attack-forearm");
      if (forearm) tip = Math.max(tip, forearm.x2 - attacker.x);
    }
    return { kind: attacker.attackKind, knock, tip,
      stunned: target.hitStunUntil > 0, alive: target.alive };
  };
  // Only the pistol lash still lands from outside the pushbox.
  const whipFar = swing({ gunAmmo: 3 }, 170);
  assert.equal(whipFar.kind, "WHIP");
  assert.ok(whipFar.stunned, "the pistol lash should connect at range");
  assert.ok(!swing({}, 170).stunned);
  assert.ok(!swing({ grenadeAmmo: 2 }, 170).stunned);

  const punch = swing({}, 130);
  const whip = swing({ gunAmmo: 3 }, 130);
  const bash = swing({ grenadeAmmo: 2 }, 130);
  for (const strike of [punch, whip, bash])
    assert.ok(strike.stunned && strike.alive);
  assert.ok(whip.tip > punch.tip, "the pistol lengthens the striking arm");
  assert.ok(punch.tip > bash.tip, "a fist around a grenade shortens it");
  assert.ok(bash.knock > punch.knock);
  assert.ok(punch.knock > whip.knock);
});

test("item swings run their own animation and publish attack capsules", () => {
  const swingRoles = (ammo) => {
    const { fight, pads, tick, now } = createFight();
    Object.assign(fight.players[0], ammo);
    pads[0].down = ["B"];
    tick();
    tick(70000);
    const geometry = fight.runnerWorldGeometry(fight.players[0], now() / 1e6);
    return { animation: fight.fighterAnimationPhase(fight.players[0]),
      roles: geometry.segments.filter((segment) =>
        segment.role?.startsWith("attack-")).map((segment) => segment.role),
      label: fight.players[0].lastButton };
  };
  const whip = swingRoles({ gunAmmo: 3 });
  assert.deepEqual(whip.roles, ["attack-upper-arm", "attack-forearm"]);
  assert.equal(whip.animation.state, "WHIP");
  assert.equal(whip.label, "WHIP");
  const bash = swingRoles({ grenadeAmmo: 2 });
  assert.deepEqual(bash.roles, ["attack-upper-arm", "attack-forearm"]);
  assert.equal(bash.animation.state, "BASH");
  assert.equal(bash.label, "BASH");
  // The bash is the slower, more committed swing of the two.
  assert.ok(bash.animation.steps > whip.animation.steps);
  assert.ok(["STARTUP", "ACTIVE", "RECOVERY"].includes(whip.animation.phase));
});

test("a lost lead arm disarms both the item button and the item swing", () => {
  const { fight, pads, tick, now } = createFight();
  const player = fight.players[0];
  player.gunAmmo = 3;
  player.facing = 1;
  for (let hit = 0; hit < 5; hit++) {
    const geometry = fight.runnerWorldGeometry(player, now() / 1e6);
    const index = geometry.segments.findIndex((segment) =>
      segment.part === "right-arm");
    fight.damagePart(player, index, fight.players[1].x, 1, now());
  }
  pads[0].down = ["Y"];
  tick();
  assert.equal(fight.bullets.length, 0, "a missing hand cannot fire");
  pads[0].down = [];
  tick();
  pads[0].down = ["B"];
  tick();
  assert.equal(player.attackKind, "", "a missing hand cannot whip");
  // The surviving arm becomes the lead hand once the fighter turns around.
  pads[0].down = [];
  tick();
  player.facing = -1;
  pads[0].down = ["B"];
  tick();
  assert.equal(player.attackKind, "WHIP");
});

test("a bot spends a held item at range and still swings up close", () => {
  const { fight, tick } = createFight();
  const bot = fight.players[1];
  bot.npc = true;
  bot.bot = true;
  bot.gunAmmo = 4;
  fight.players[0].x = 5000;
  bot.x = 7000;
  bot.botItemAt = 0;
  tick();
  assert.equal(bot.gunAmmo, 3);
  assert.equal(fight.bullets.length, 1);
  fight.players[0].x = 5850;
  bot.x = 6000;
  bot.botAttackAt = 0;
  tick();
  assert.ok(["WHIP", "KICK"].includes(bot.attackKind));
  assert.equal(bot.gunAmmo, 3, "close range melee never spends the magazine");
});

test("an armed exchange resimulates identically from the same inputs", () => {
  const run = () => {
    const { fight, pads, tick } = createFight();
    fight.setWind(0);
    Object.assign(fight.players[0], { gunAmmo: 3, grenadeAmmo: 2 });
    for (const down of [[], ["Y"], [], ["X"], [], ["A"], [], ["Y"], ["X"], []]) {
      pads[0].down = down;
      for (let frame = 0; frame < 4; frame++) tick();
    }
    return JSON.stringify([fight.players.map((player) => [player.x, player.y,
      player.vx, player.knockVx, player.attackKind, player.gunAmmo,
      player.grenadeAmmo, player.hitStunUntil]),
      fight.bullets.length, fight.grenades.length]);
  };
  assert.equal(run(), run());
});

test("gun drops grant ammo and Y fires in the quantized aim direction", () => {
  const { fight, pads, signals, tick } = createFight();
  const player = fight.players[0];
  const pickup = fight.gunPickups[0];
  pickup.active = true;
  pickup.x = player.x;
  pickup.y = player.y - 70;
  tick();
  assert.equal(player.gunAmmo, pickup.amount);
  const fireX = player.x;
  const fireY = player.y;
  pads[0].down = ["ArrowUp", "ArrowRight", "Y"];
  tick();
  assert.equal(player.gunAmmo, pickup.amount - 1);
  assert.equal(fight.bullets.length, 1);
  assert.ok(fight.bullets[0].vx > 0);
  assert.ok(fight.bullets[0].vy < 0);
  const aimLength = Math.sqrt(2);
  const frameSeconds = 16667 / 1000000;
  // The hand follows the aim now, so the muzzle rides a diagonal arm rather
  // than sitting off the end of a forward-pointing one. Both the 108 reach and
  // the 54 of barrel are measured along the aim; the resting -115 shoulder
  // height gives way to the aimed pose's -102 plus its own 92 of lift.
  const muzzleX = fireX + (108 + 54) / aimLength;
  const muzzleY = fireY - 102 - (92 + 54) / aimLength;
  assert.ok(Math.abs(fight.bullets[0].x -
    (muzzleX + fight.bullets[0].vx * frameSeconds)) < .001);
  assert.ok(Math.abs(fight.bullets[0].y -
    (muzzleY + fight.bullets[0].vy * frameSeconds)) < .001);
  assert.ok(signals.some(([event, pad]) => event === "bullet" && pad === 0));
});

test("grenade drops grant ammo and Y throws an expanding grenade", () => {
  const { fight, pads, signals, tick } = createFight();
  const player = fight.players[0];
  // The cube's map authors no grenades; a dropped cache pushed at runtime is
  // the shape a grenade pickup can still take, so seed one to keep the
  // throw mechanic covered.
  const pickup = { amount: 2, x: player.x, y: player.y - 70, z: 0,
    active: true, startsActive: false, respawnAt: 0 };
  fight.grenadePickups.push(pickup);
  tick();
  assert.equal(player.grenadeAmmo, pickup.amount);
  pads[0].down = ["Y"];
  tick();
  assert.equal(player.grenadeAmmo, pickup.amount - 1);
  assert.equal(fight.grenades.length, 1);
  assert.ok(signals.some(([event, pad]) => event === "grenade" && pad === 0));
});

test("the sword arms by walk-over, stays for the round, and can be stolen", () => {
  const { fight, tick } = createFight();
  const player = fight.players[0];
  const pickup = fight.swordPickups[0];
  pickup.x = player.x;
  pickup.y = player.y - 70;
  tick();
  assert.equal(player.swordHeld, true);
  assert.equal(pickup.active, false);
  assert.equal(player.lastButton, "SWORD");
  // A blade in the item hand stretches every hand strike.
  assert.match(source, /reach: spec\.reach \* 1\.5/);
  // One sword a round: only the pistol returns on the powerup cycle, and
  // the STOLE table is the only way steel changes hands.
  assert.match(source, /const pickup = gunPickups\[0\]/);
  assert.match(source, /target\.swordHeld && !player\.swordHeld/);
  // Round reset returns it to its tile and empties every hand.
  fight.nextRound();
  assert.equal(player.swordHeld, false);
  assert.equal(pickup.active, true);
});

test("opposing bullets cancel one another", () => {
  const { fight, tick } = createFight();
  fight.bullets.push(
    { x: 5900, y: 8000, z: 0, vx: 2600, vy: 0, owner: 0, life: 1 },
    { x: 6100, y: 8000, z: 0, vx: -2600, vy: 0, owner: 1, life: 1 },
  );
  tick(40000);
  assert.equal(fight.bullets.length, 0);
});

test("body shots recoil and stun a limb while headshots alone knock out", () => {
  const bodyFight = createFight();
  bodyFight.fight.setWind(0);
  const bodyTarget = bodyFight.fight.players[1];
  const bodyGeometry = bodyFight.fight.runnerWorldGeometry(
    bodyTarget, bodyFight.now() / 1000000);
  const torso = bodyGeometry.segments[1];
  const torsoAmount = .25;
  bodyFight.fight.bullets.push({
    x: torso.x1 + (torso.x2 - torso.x1) * torsoAmount,
    y: torso.y1 + (torso.y2 - torso.y1) * torsoAmount,
    z: torso.z1, vx: 1, vy: 0, owner: 0, life: 1,
  });
  bodyFight.tick(1);
  assert.equal(bodyTarget.alive, true);
  assert.ok(bodyTarget.hitStunUntil > bodyFight.now());
  assert.equal(bodyTarget.hitSegment, 1);
  assert.notEqual(bodyTarget.vx, 0);

  const headFight = createFight();
  headFight.fight.setWind(0);
  const headTarget = headFight.fight.players[1];
  const head = headFight.fight.runnerWorldGeometry(
    headTarget, headFight.now() / 1000000).head;
  headFight.fight.bullets.push({ x: head.x, y: head.y, z: head.z,
    vx: 1, vy: 0, owner: 0, life: 1 });
  headFight.tick(1);
  assert.equal(headTarget.alive, false);
  assert.match(source,
    /else if \(headshot\) \{[\s\S]*?if \(isHeadOnly\(target\)\) \{[\s\S]*?killPlayer/);
  assert.match(source, /contact\.headDistance <= grenade\.blastRadius/);
});

test("double-tap directions trigger dash, ultra-jump, and fast-drop", () => {
  const { fight, tap, tick } = createFight();
  tap(0, "ArrowRight");
  tap(0, "ArrowRight");
  assert.equal(fight.players[0].lastButton, "DASH RIGHT");
  assert.ok(fight.players[0].vx > 2000);
  tick(40000);
  assert.ok(Math.abs(fight.players[0].vx) < 100);

  tap(0, "ArrowUp");
  tap(0, "ArrowUp");
  assert.equal(fight.players[0].lastButton, "ULTRA JUMP");
  assert.ok(fight.players[0].vy < -1100);

  // Airborne, double-tap DOWN is the ground pound, not a nameless fast drop —
  // it commits the fighter and remembers the height it started from.
  tap(0, "ArrowDown");
  tap(0, "ArrowDown");
  assert.equal(fight.players[0].lastButton, "GROUND POUND");
  assert.equal(fight.players[0].pounding, true);
  assert.ok(fight.players[0].vy > 1200);
});

test("holding one direction never becomes a double-tap dash", () => {
  const { fight, pads, tick } = createFight();
  pads[0].down = ["ArrowLeft"];
  for (let frame = 0; frame < 30; frame++) tick(16667);
  assert.notEqual(fight.players[0].lastButton, "DASH LEFT");
  assert.equal(fight.players[0].dashUntil, 0);
});

test("opposite input and wall contact cancel dash lock immediately", () => {
  const { fight, pads, tap, tick } = createFight();
  tap(0, "ArrowRight");
  tap(0, "ArrowRight");
  pads[0].down = ["ArrowLeft"];
  tick();
  assert.equal(fight.players[0].dashUntil, 0);
  assert.ok(fight.players[0].vx < 0);

  fight.players[0].x = 12000;
  fight.players[0].dashUntil = Number.MAX_SAFE_INTEGER;
  fight.players[0].dashVx = 2400;
  pads[0].down = ["ArrowRight"];
  tick();
  assert.equal(fight.players[0].dashUntil, 0);
});

test("a direction held across round reset waits for a real release", () => {
  const { fight, pads, tick } = createFight();
  pads[0].down = ["ArrowRight"];
  tick();
  fight.nextRound();
  tick(3000001);
  assert.ok(fight.players[0].vx < 500);
  pads[0].down = [];
  tick();
  pads[0].down = ["ArrowRight"];
  tick();
  assert.ok(fight.players[0].vx > 1000);
});

test("X shield repels a grounded melee rush", () => {
  // At kick range the shield's own bash answers first — the ring is wider
  // than any bare strike's reach — so the grounded exchange is bash, not
  // block. The block path stays alive in source for aerial strikes and
  // back-blocks, which the bash's height gate lets through.
  const { fight, pads, signals, tick } = createFight();
  fight.players[1].npc = false;
  fight.players[0].x = 400;
  fight.players[1].x = 500;
  pads[0].down = ["A"];
  pads[1].down = ["X"];
  for (let frame = 0; frame < 5; frame++) tick(16667);
  assert.equal(fight.players[1].alive, true);
  assert.equal(fight.players[0].score, 0);
  assert.ok(fight.players[0].x < 400,
    "a shielded strike should bounce the attacker backward");
  assert.ok(fight.players[0].knockVx < -1000,
    "shield recoil should survive the next movement update");
  assert.ok(signals.some(([event, player]) =>
    event === "shield-bash" && player === 1));
  assert.match(source, /emitSignal\("block", target\.pad, attacker\.pad/);
});

test("shield is emitted on the foreground triangle path", () => {
  const { fight, triangles } = createFight();
  fight.setDebugHitboxes(false);
  triangles.length = 0;
  fight.players[0].blocking = false;
  fight.paint();
  const withoutShield = triangles.length;
  triangles.length = 0;
  fight.players[0].blocking = true;
  fight.paint();
  assert.ok(triangles.length > withoutShield + 40);
  const shieldSource = source.slice(source.indexOf("if (player.blocking) {"),
    source.indexOf("function drawDebugHitboxes"));
  assert.match(shieldSource, /filledRing/);
  assert.match(shieldSource, /filledCapsule/);
});

test("shielding suppresses new ground and air control while preserving momentum", () => {
  const { fight, pads, tick } = createFight();
  const player = fight.players[1];
  player.vx = -620;
  player.vy = -380;
  player.grounded = false;
  const facing = player.facing;
  pads[1].down = ["X", "ArrowRight", "ArrowUp"];
  tick();
  assert.equal(player.blocking, true);
  assert.equal(player.inputX, 0);
  assert.equal(player.inputY, 0);
  assert.equal(player.facing, facing);
  assert.ok(player.vx < 0);
  assert.ok(player.vy > -380);
  pads[1].down = ["X", "ArrowLeft", "ArrowDown"];
  tick();
  assert.ok(player.vx < 0);
  assert.equal(player.ducking, false);
});

test("fighters walking into one another push apart without ending the round", () => {
  const { fight, pads, tick } = createFight();
  fight.players[0].x = 5950;
  fight.players[1].x = 6050;
  pads[0].down = ["ArrowRight"];
  pads[1].down = ["ArrowLeft"];
  for (let frame = 0; frame < 20; frame++) tick(16667);
  assert.equal(fight.players[0].alive, true);
  assert.equal(fight.players[1].alive, true);
  assert.equal(fight.roundState().roundResult, "");
  assert.ok(Math.abs(fight.players[1].x - fight.players[0].x) >= 137);
});

test("an airborne fighter can cross over the other fighter", () => {
  const { fight, pads, tick } = createFight();
  const jumper = fight.players[0];
  const standing = fight.players[1];
  jumper.x = 390;
  jumper.y = standing.y - 190;
  jumper.vy = 0;
  jumper.grounded = false;
  standing.x = 510;
  pads[0].down = ["ArrowRight"];
  for (let frame = 0; frame < 8; frame++) tick(16667);
  assert.ok(jumper.x > standing.x);
  assert.equal(jumper.alive, true);
  assert.equal(standing.alive, true);
});

test("a falling fighter can stand on the opponent head hitbox", () => {
  const { fight, tick, now } = createFight();
  const rider = fight.players[0];
  const base = fight.players[1];
  rider.x = base.x;
  rider.y = base.y - 210;
  rider.vy = 900;
  rider.grounded = false;
  tick(100000);
  const head = fight.runnerWorldGeometry(base, now() / 1000000).head;
  assert.equal(rider.standingOn, base.pad);
  assert.ok(Math.abs(rider.y - (head.y - head.radius)) < 1);
  assert.equal(rider.grounded, true);
});

test("a neutral fighter cannot defeat an attacking fighter by contact", () => {
  const { fight, pads, tick } = createFight();
  fight.players[0].x = 5940;
  fight.players[1].x = 6060;
  pads[0].down = ["B"];
  for (let frame = 0; frame < 10; frame++) tick(16667);
  assert.equal(fight.players[0].alive, true);
  assert.equal(fight.players[1].alive, true);
  assert.ok(fight.players[1].hitStunUntil > 0);
});

test("melee collision uses the animated attacking limb capsules", () => {
  const { fight, pads, tick, now } = createFight();
  fight.players[0].x = 5940;
  fight.players[1].x = 6060;
  pads[0].down = ["B"];
  tick();
  tick(70000);
  const geometry = fight.runnerWorldGeometry(fight.players[0], now() / 1000000);
  assert.deepEqual(geometry.segments
    .filter((segment) => segment.role?.startsWith("attack-"))
    .map((segment) => segment.role), ["attack-upper-arm", "attack-forearm"]);
  const contact = fight.meleeLimbContact(
    fight.players[0], fight.players[1], now() / 1000000);
  assert.ok(contact);
  assert.ok(Number.isFinite(contact.separation));
  assert.match(source, /function segmentSegmentClosest/);
  assert.doesNotMatch(source.match(/function resolveMelee[\s\S]*?\n}\n\nfunction resolvePlayerPushboxes/)[0],
    /runnerContactToPoint/);
});

test("arms and legs take localized damage and detach from collision geometry", () => {
  const { fight, now } = createFight();
  const target = fight.players[1];
  const damage = (part, hits) => {
    for (let hit = 0; hit < hits; hit++) {
      const geometry = fight.runnerWorldGeometry(target, now() / 1000000);
      const index = geometry.segments.findIndex((segment) => segment.part === part);
      assert.notEqual(index, -1);
      fight.damagePart(target, index, fight.players[0].x, 0, now());
    }
  };
  damage("left-arm", 2);
  assert.ok(target.removedParts.includes("left-arm"));
  assert.ok(fight.detachedParts.some((fragment) => fragment.part === "left-arm"));
  assert.equal(fight.runnerWorldGeometry(target, now() / 1000000).segments
    .some((segment) => segment.part === "left-arm"), false);
});

test("spiderdummy is a giant inert foe with destructible segmented legs", () => {
  const { fight, now } = createFight();
  fight.startFightAgainst("spiderdummy");
  const spider = fight.players[1];
  const geometry = fight.runnerWorldGeometry(spider, now() / 1000000);
  assert.equal(spider.name, "SPIDERDUMMY");
  assert.equal(spider.npc, true);
  assert.equal(spider.bot, false);
  assert.equal(spider.spiderDummy, true);
  assert.equal(geometry.segments
    .filter((segment) => segment.role.startsWith("spider-leg-")).length, 40);
  assert.equal(geometry.segments
    .filter((segment) => segment.role.startsWith("spider-hitbox-")).length, 8);
  assert.ok(new Set(geometry.segments
    .filter((segment) => segment.role.startsWith("spider-leg-"))
    .flatMap((segment) => [segment.z1, segment.z2])).size >= 8,
  "legs occupy distinct depth lanes instead of one stacked plane");
  assert.equal(geometry.head.radius, 22);
  const xs = geometry.segments.flatMap((segment) => [segment.x1, segment.x2]);
  assert.ok(Math.max(...xs) - Math.min(...xs) > 480,
    "compact articulated legs still make the spider wider than a fighter");
  for (let hit = 0; hit < 3; hit++) {
    const current = fight.runnerWorldGeometry(spider, now() / 1000000);
    const index = current.segments.findIndex((segment) =>
      segment.part === "spider-leg-1");
    fight.damagePart(spider, index, fight.players[0].x, 0, now());
  }
  assert.equal(fight.runnerWorldGeometry(spider, now() / 1000000).segments
    .some((segment) => segment.part === "spider-leg-1"), false);
  assert.ok(fight.detachedParts.filter((part) =>
    part.part === "spider-leg-1").length >= 5);
});

test("spiderdummy remains directly reachable while pal select is retired", () => {
  globalThis.__oskiewarOpponent = "spiderdummy";
  const { fight } = createFight(false, false);
  delete globalThis.__oskiewarOpponent;
  assert.equal(fight.players[1].name, "SPIDERDUMMY");
  assert.equal(fight.players[1].spiderDummy, true);
  assert.match(webShell, /get\("opponent"\)/);
});

// Replaces the 50/50 dummy-or-spiderdummy deal this test used to pin.
// @jeffrey, playtesting: "can we switch dummy to bot now?" A solo session is a
// fight rather than target practice, so the free opponent thinks — and the
// thing that then has to be held down is the wire: a sparring partner with the
// bot's brain must still buy no series, no demo, no live frame and no
// analytics, because the front door never asks anybody to sign in. Both inert
// targets stay reachable by name for damage, geometry and sync work.
test("a fresh training session spars against the bot and stays off the wire",
  () => {
  const previousOpponent = globalThis.__oskiewarOpponent;
  // An empty string is a real answer — "nothing was asked for" — so the
  // harness leaves it alone instead of substituting its usual dummy.
  globalThis.__oskiewarOpponent = "";
  let fresh;
  try {
    fresh = createFight(false, false);
  } finally {
    if (previousOpponent === undefined) delete globalThis.__oskiewarOpponent;
    else globalThis.__oskiewarOpponent = previousOpponent;
  }
  const { fight, replays, liveFrames, analyticsEvents, tap, tick } = fresh;
  assert.equal(fight.players[1].npc, true);
  assert.equal(fight.players[1].bot, true, "the free opponent hits back");
  assert.equal(fight.players[1].spiderDummy, false);
  assert.equal(fight.players[1].name, "BOT");
  fight.enterGame();
  for (let frame = 0; frame < 60; frame++) tick();
  assert.equal(fight.seriesState(), "", "training carries no series");
  assert.deepEqual(replays, []);
  assert.deepEqual(liveFrames, []);
  assert.deepEqual(analyticsEvents, []);

  // Half of sessions open on the standing face-off, where attract borrows the
  // first seat and keeps a cool neutral body opposite it so the matchup reads.
  // Start has to hand that seat back AND re-seat the sparring partner: while
  // it hardcoded an inert dummy, exactly those sessions kept punching a post.
  globalThis.__oskiewarAttractVariant = "action";
  tap(0, "Menu");
  delete globalThis.__oskiewarAttractVariant;
  assert.equal(fight.shellState().mode, "MENU");
  assert.equal(fight.players[0].bot, true, "attract borrowed the first seat");
  assert.equal(fight.players[1].name, "DUMMY", "the tableau stays legible");
  fight.enterGame();
  assert.equal(fight.players[0].bot, false, "start hands the seat back");
  assert.equal(fight.players[1].bot, true, "start re-seats the sparring bot");

  const choice = source.slice(source.indexOf("function trainingOpponentKind"),
    source.indexOf("const selectionReady"));
  assert.match(choice, /requested === "dummy" \|\| requested === "spiderdummy"/);
  assert.match(choice, /trainingOpponent = "trainingbot"/);
});

test("the skateboard is retired from the cube's deck", () => {
  // @jeffrey: no boards on this map. The series draw can only deal a ball,
  // no round hands one out, and training carries a plain ball instead.
  assert.doesNotMatch(source, /\{ type: "skateboard"/);
  assert.doesNotMatch(source, /matchBallType = "skateboard"/);
  assert.doesNotMatch(source, /players\[0\]\.skateboard = true/);
  // The mechanic sleeps in place — resetRound still clears the flag — but
  // nothing on the map can set it.
  assert.match(source, /player\.skateboard = false/);
});

test("losing both legs grounds the pelvis in a low crouched form", () => {
  const { fight, now } = createFight();
  const target = fight.players[1];
  const standing = fight.runnerWorldGeometry(target, now() / 1000000);
  for (const part of ["left-leg", "right-leg"]) {
    for (let hit = 0; hit < 2; hit++) {
      const geometry = fight.runnerWorldGeometry(target, now() / 1000000);
      const index = geometry.segments.findIndex((segment) => segment.part === part);
      assert.notEqual(index, -1);
      fight.damagePart(target, index, fight.players[0].x, 0, now());
    }
  }
  const crouched = fight.runnerWorldGeometry(target, now() / 1000000);
  const torso = crouched.segments.find((segment) => segment.role === "torso");
  assert.ok(torso);
  assert.ok(torso.y2 + torso.width / 2 >= 12000,
    "the pelvis capsule should touch the floor");
  assert.ok(crouched.head.y > standing.head.y + 40,
    "the head should settle into a visibly lower form");
  assert.equal(crouched.segments.some((segment) =>
    segment.part === "left-leg" || segment.part === "right-leg"), false);
});

test("a removed torso leaves a rolling, pump-bouncing, gun-firing head", () => {
  const { fight, pads, tick, now } = createFight();
  const target = fight.players[0];
  const damage = (part, hits) => {
    for (let hit = 0; hit < hits; hit++) {
      const geometry = fight.runnerWorldGeometry(target, now() / 1000000);
      const index = geometry.segments.findIndex((segment) => segment.part === part);
      assert.notEqual(index, -1);
      fight.damagePart(target, index, fight.players[1].x, 1, now());
    }
  };
  for (const part of ["left-arm", "right-arm", "left-leg", "right-leg"])
    damage(part, 2);
  assert.equal(fight.isPogo(target), true);
  pads[0].down = ["ArrowUp"];
  tick();
  tick(90000);
  assert.ok(target.vy < 0);
  pads[0].down = ["B"];
  tick();
  assert.equal(target.attackKind, "");
  damage("torso", 3);
  assert.equal(fight.isHeadOnly(target), true);
  assert.equal(fight.runnerWorldGeometry(target, now() / 1000000).segments.length, 0);
  target.y = 12000;
  target.vy = 20;
  tick();
  assert.equal(target.vy, 0, "the head should settle onto the floor to roll");
  assert.equal(target.grounded, true);
  pads[0].down = ["ArrowUp"];
  tick();
  pads[0].down = [];
  tick();
  pads[0].down = ["ArrowDown"];
  tick();
  assert.ok(target.vy < 0, "alternating up/down should pump a head bounce");
  assert.ok(target.headBounceCharge > 0);
  target.gunAmmo = 2;
  pads[0].down = [];
  tick();
  pads[0].down = ["Y"];
  tick();
  assert.equal(target.gunAmmo, 1, "the mouth-held gun should fire");
  pads[0].down = ["X", "A"];
  tick();
  assert.equal(target.attackKind, "");
});

test("a limbless airborne torso dives on down and pogo-bounces from the floor", () => {
  const { fight, pads, tick, now } = createFight();
  const target = fight.players[0];
  for (const part of ["left-arm", "right-arm", "left-leg", "right-leg"]) {
    for (let hit = 0; hit < 2; hit++) {
      const geometry = fight.runnerWorldGeometry(target, now() / 1000000);
      const index = geometry.segments.findIndex((segment) => segment.part === part);
      fight.damagePart(target, index, fight.players[1].x, 1, now());
    }
  }
  assert.equal(fight.isPogo(target), true);
  target.y = 11000;
  target.vy = -300;
  target.grounded = false;
  pads[0].down = ["ArrowDown"];
  tick();
  assert.equal(target.pogoDive, true);
  assert.ok(target.vy > 2200);
  assert.equal(target.stance, "POGO DOWN");
  target.y = 11995;
  target.vy = 2250;
  pads[0].down = [];
  tick();
  assert.equal(target.pogoDive, false);
  assert.equal(target.grounded, false);
  assert.ok(target.vy < 0, "the downward torso attack should rebound as a pogo");
});

test("damaged limbs redden without perpendicular segmentation marks", () => {
  assert.match(source, /function damagedPartColor/);
  assert.match(source, /mixColor\(color, \[244, 34, 50\], amount\)/);
  assert.doesNotMatch(source, /middleX - nx[\s\S]{0,180}middleX \+ nx/);
});

test("simultaneous body strikes recoil without player-order bias", () => {
  const { fight, pads, tick } = createFight();
  fight.players[0].x = 5940;
  fight.players[1].x = 6060;
  pads[0].down = ["B"];
  pads[1].down = ["B"];
  for (let frame = 0; frame < 5; frame++) tick(16667);
  assert.equal(fight.players[0].alive, true);
  assert.equal(fight.players[1].alive, true);
  assert.ok(fight.players[0].hitStunUntil > 0);
  assert.ok(fight.players[1].hitStunUntil > 0);
  assert.equal(fight.roundState().roundResult, "");
});

test("player lands on the center platform", { skip: !PLATFORM }, () => {
  const { fight, tick } = createFight();
  const stage = fight.stageGeometry();
  const player = fight.players[0];
  player.x = (stage.platformLeft + stage.platformRight) / 2;
  player.y = stage.platformY - 200;
  player.vy = 300;
  player.grounded = false;
  for (let step = 0; step < 10 && !player.grounded; step++) tick(40000);
  assert.equal(player.y, stage.platformY);
  assert.equal(player.grounded, true);
});

test("one plain jump from the spawn floor reaches the center platform", { skip: !PLATFORM }, () => {
  const { fight, pads, tick } = createFight();
  const stage = fight.stageGeometry();
  const player = fight.players[0];
  player.x = (stage.platformLeft + stage.platformRight) / 2;
  // Held, not tapped: releasing up cuts the rise to 55% and the short hop
  // that leaves deliberately falls short of the ledge.
  pads[0].down = ["ArrowUp"];
  let landed = false;
  for (let step = 0; step < 90 && !landed; step++) {
    tick();
    landed = player.grounded && player.y === stage.platformY;
  }
  assert.ok(landed, `jump peaked at ${player.y} short of ${stage.platformY}`);

  // And a tapped jump must not reach it, or the hold would mean nothing.
  const tapped = createFight();
  const hopper = tapped.fight.players[0];
  hopper.x = (stage.platformLeft + stage.platformRight) / 2;
  tapped.pads[0].down = ["ArrowUp"];
  for (let step = 0; step < 2; step++) tapped.tick();
  tapped.pads[0].down = [];
  let reached = false;
  for (let step = 0; step < 90 && !reached; step++) {
    tapped.tick();
    reached = hopper.grounded && hopper.y === stage.platformY;
  }
  assert.ok(!reached, "a tapped hop should fall short of the ledge");
});

test("crouch and jump use readable multi-frame pose transitions", () => {
  const { fight, pads, tick } = createFight();
  const player = fight.players[0];
  pads[0].down = ["ArrowDown"];
  tick();
  assert.ok(player.crouchBlend > 0 && player.crouchBlend < 1);
  assert.equal(player.ducking, false);
  for (let frame = 0; frame < 6; frame++) tick();
  assert.equal(player.ducking, true);

  const jump = createFight();
  jump.pads[0].down = ["ArrowUp"];
  jump.tick();
  assert.ok(jump.fight.players[0].jumpLaunchAt > jump.now());
  assert.equal(jump.fight.players[0].grounded, true);
  jump.tick(40000);
  assert.equal(jump.fight.players[0].grounded, true);
  jump.tick(50000);
  assert.ok(jump.fight.players[0].vy < 0);
  assert.match(source, /const animation = fighterAnimationPhase\(player\)/);
  assert.match(source, /function drawFighterSilhouette/);
});

// A stride used to run its twelve frames in twelve ticks — five whole cycles a
// second, which the eye reads as a fighter vibrating rather than as one taking
// steps. The cycle is 600ms now, so the steps are countable.
test("a walk cycle reads as steps rather than a vibration", () => {
  const { fight, pads, tick } = createFight();
  pads[0].down = ["ArrowRight"];
  for (let frame = 0; frame < 30; frame++) tick();
  const player = fight.players[0];
  const opening = fight.fighterAnimationPhase(player);
  assert.equal(opening.state, "WALK");
  assert.equal(opening.steps, 12);
  let advances = 0;
  let previous = opening.step;
  for (let frame = 0; frame < 36; frame++) {
    tick();
    const { step } = fight.fighterAnimationPhase(player);
    if (step !== previous) advances += 1;
    previous = step;
  }
  // 36 ticks is 600ms: one stride, twelve steps — not five strides.
  assert.ok(advances >= 11 && advances <= 13,
    `stride advanced ${advances} times across 36 ticks`);
});

// `fighterAnimationPhase` dates most poses from `player.lastButtonAt || now`.
// The unpressed sentinel was -10000000, which is truthy, so the fallback never
// fired and a fighter who had touched nothing yet dated their pose from ten
// seconds before the round — far enough back that a non-looping pose like the
// hit stun opened already pinned to its final frame.
test("a fighter's pose clock never opens from a negative sentinel", () => {
  assert.doesNotMatch(source, /lastButtonAt: -\d/);
  assert.doesNotMatch(source, /lastButtonAt = -\d/);
  const { fight, tick } = createFight();
  tick();
  const player = fight.players[0];
  assert.equal(player.lastButtonAt, 0,
    "an unpressed fighter carries 0 so `|| now` can fall through");
  player.hitStunUntil = Number.MAX_SAFE_INTEGER;
  const stun = fight.fighterAnimationPhase(player);
  assert.equal(stun.state, "HIT");
  assert.equal(stun.step, 1,
    `a fresh stun opened on frame ${stun.step} of ${stun.steps}`);
});

test("fighters can crouch in air and carry a crouch into a jump", () => {
  const airborne = createFight();
  const airPlayer = airborne.fight.players[0];
  airPlayer.y -= 400;
  airPlayer.grounded = false;
  airborne.pads[0].down = ["ArrowDown"];
  for (let frame = 0; frame < 7; frame++) airborne.tick();
  assert.equal(airPlayer.ducking, true);
  assert.equal(airborne.fight.fighterAnimationPhase(airPlayer).state,
    "AIR CROUCH");

  const crouchJump = createFight();
  const jumpPlayer = crouchJump.fight.players[0];
  crouchJump.pads[0].down = ["ArrowDown"];
  for (let frame = 0; frame < 7; frame++) crouchJump.tick();
  crouchJump.pads[0].down = ["ArrowUp"];
  crouchJump.tick();
  assert.equal(jumpPlayer.crouchJump, true);
  assert.equal(jumpPlayer.lastButton, "CROUCH JUMP");
  crouchJump.tick(90000);
  assert.ok(jumpPlayer.vy < 0);
  assert.equal(jumpPlayer.ducking, true);
});

// Airtime and apex are the two numbers the jump is tuned against, so they are
// measured here rather than asserted through the constants.
function jumpArc(harness, pad = 0, holdFrames = 200) {
  const player = harness.fight.players[pad];
  // Clear of the ledge: it now sits inside a plain jump's arc, and landing on
  // it mid-measurement would truncate both the apex and the airtime.
  // The shaft is the one column with no rung over it, so an arc measured
  // there is the arc itself rather than an arc truncated by a landing.
  if (PLATFORM) player.x = 1020;
  const floor = player.y;
  const startedAt = harness.now();
  let liftAt = 0;
  let apex = 0;
  for (let frame = 0; frame < 200; frame++) {
    if (frame === holdFrames) harness.pads[pad].down = [];
    harness.tick();
    if (!liftAt && !player.grounded) liftAt = harness.now();
    if (!liftAt) continue;
    apex = Math.max(apex, floor - player.y);
    if (player.grounded)
      return { latency: liftAt - startedAt, airtime: harness.now() - liftAt, apex };
  }
  return { latency: liftAt - startedAt, airtime: Infinity, apex };
}

test("a jump lifts off within four frames and lands inside two thirds of a second", () => {
  const harness = createFight();
  harness.pads[0].down = ["ArrowUp"];
  const arc = jumpArc(harness);
  assert.ok(arc.latency <= 70000, `liftoff took ${arc.latency}us`);
  assert.ok(arc.airtime < 700000, `airtime was ${arc.airtime}us`);
  assert.ok(arc.apex > 290, `apex was only ${arc.apex}`);
});

test("holding up jumps high while a tapped up becomes a short hop", () => {
  const held = createFight();
  held.pads[0].down = ["ArrowUp"];
  const full = jumpArc(held);
  const tapper = createFight();
  tapper.pads[0].down = ["ArrowUp"];
  const cut = jumpArc(tapper, 0, 2);
  assert.ok(cut.apex > 60, `a cut jump still leaves the ground: ${cut.apex}`);
  assert.ok(cut.apex < full.apex * .5,
    `cut apex ${cut.apex} should be well under full apex ${full.apex}`);
  assert.ok(cut.airtime < full.airtime * .7,
    `cut airtime ${cut.airtime} vs full ${full.airtime}`);
});

test("the ultra jump still clears the platform after the gravity retune", { skip: !PLATFORM }, () => {
  const { fight, tap, tick } = createFight();
  const stage = fight.stageGeometry();
  const player = fight.players[0];
  player.x = (stage.platformLeft + stage.platformRight) / 2;
  tap(0, "ArrowUp");
  tap(0, "ArrowUp");
  assert.equal(player.lastButton, "ULTRA JUMP");
  for (let frame = 0; frame < 200 && !player.grounded; frame++) tick();
  assert.ok(player.grounded);
  assert.ok(player.y < stage.floorY,
    `an ultra jump landed back on the floor at ${player.y}`);
});

test("a direction flick out of crouch becomes a low crouch hop", () => {
  const { fight, pads, tick, signals } = createFight();
  const player = fight.players[0];
  const floor = player.y;
  const startX = player.x;
  pads[0].down = ["ArrowDown"];
  for (let frame = 0; frame < 8; frame++) tick();
  assert.equal(player.ducking, true);
  pads[0].down = ["ArrowDown", "ArrowRight"];
  tick();
  assert.equal(player.grounded, false);
  assert.equal(player.lastButton, "CROUCH HOP");
  assert.equal(player.stance, "CROUCH HOP");
  assert.equal(fight.fighterAnimationPhase(player).state, "CROUCH HOP");
  assert.ok(signals.some(([event, pad]) => event === "crouchhop" && pad === 0));
  let apex = 0;
  let frames = 0;
  for (; frames < 120; frames++) {
    tick();
    apex = Math.max(apex, floor - player.y);
    if (player.grounded) break;
  }
  assert.ok(apex > 60 && apex < 170, `crouch hop apex was ${apex}`);
  assert.ok(frames < 24, `crouch hop took ${frames} frames`);
  assert.ok(player.x - startX > 250,
    `crouch hop only travelled ${player.x - startX}`);
});

test("a crouch hop stays lower and shorter than a crouch jump", () => {
  const arcFrom = (crouched, press) => {
    const harness = createFight();
    harness.pads[0].down = ["ArrowDown"];
    for (let frame = 0; frame < 8; frame++) harness.tick();
    harness.pads[0].down = crouched ? ["ArrowDown", press] : [press];
    return jumpArc(harness);
  };
  const hop = arcFrom(true, "ArrowRight");
  const crouchJump = arcFrom(false, "ArrowUp");
  assert.ok(hop.apex < crouchJump.apex * .4,
    `hop apex ${hop.apex} vs crouch jump ${crouchJump.apex}`);
  assert.ok(hop.airtime < crouchJump.airtime,
    `hop airtime ${hop.airtime} vs crouch jump ${crouchJump.airtime}`);
  assert.equal(hop.latency, 16667, "a crouch hop leaves the ground immediately");
});

test("double-tapping crouch sinks through the platform but never the floor", { skip: !PLATFORM }, () => {
  const { fight, tick, tap, signals } = createFight();
  const stage = fight.stageGeometry();
  const player = fight.players[1];
  player.x = (stage.platformLeft + stage.platformRight) / 2;
  player.y = stage.platformY - 400;
  player.vy = 0;
  player.grounded = false;
  for (let frame = 0; frame < 200 && !player.grounded; frame++) tick();
  const platformY = player.y;
  assert.equal(platformY, stage.platformY,
    "the fighter should be resting on the platform");
  tap(1, "ArrowDown");
  tap(1, "ArrowDown");
  assert.equal(player.lastButton, "SINK");
  assert.equal(player.stance, "SINK");
  assert.equal(fight.fighterAnimationPhase(player).state, "SINK");
  assert.ok(signals.some(([event, pad]) => event === "sink" && pad === 1));
  assert.equal(player.grounded, false);
  assert.ok(player.y > platformY);
  for (let frame = 0; frame < 200 && !player.grounded; frame++) tick();
  // One rung down, and the rung below the first one is the floor.
  assert.equal(player.y, stage.floorY);
  assert.notEqual(player.stance, "SINK");
});

test("a lone crouch tap on the platform never sinks", () => {
  const { fight, tick, tap } = createFight();
  const stage = fight.stageGeometry();
  const player = fight.players[0];
  player.x = (stage.platformLeft + stage.platformRight) / 2;
  player.y = stage.platformY - 400;
  player.grounded = false;
  for (let frame = 0; frame < 200 && !player.grounded; frame++) tick();
  const platformY = player.y;
  tap(0, "ArrowDown");
  for (let frame = 0; frame < 20; frame++) tick();
  assert.equal(player.y, platformY);
  assert.equal(player.grounded, true);
});

test("double-tapping crouch on the floor still fast-drops instead of sinking", () => {
  const { fight, tap } = createFight();
  const player = fight.players[0];
  const ground = player.y;
  tap(0, "ArrowDown");
  tap(0, "ArrowDown");
  assert.equal(player.lastButton, "DASH DOWN");
  assert.equal(player.y, ground);
});

test("a bot stranded on the platform sinks back into reach", { skip: !PLATFORM }, () => {
  const { fight, tick } = createFight();
  const stage = fight.stageGeometry();
  const bot = fight.players[1];
  bot.bot = true;
  bot.x = (stage.platformLeft + stage.platformRight) / 2;
  bot.y = stage.platformY - 400;
  bot.grounded = false;
  for (let frame = 0; frame < 200 && !bot.grounded; frame++) tick();
  const platformY = bot.y;
  assert.equal(platformY, stage.platformY);
  // The sink is a double-tap, so it has to arrive as two separate presses
  // with a gap `lastRelease` can see — not one long hold.
  const taps = [];
  let held = 0;
  let dropped = false;
  for (let frame = 0; frame < 120; frame++) {
    tick();
    if (fight.inputPadDown(1).includes("ArrowDown")) held += 1;
    else if (held) {
      taps.push(held);
      held = 0;
    }
    dropped ||= bot.y > platformY;
  }
  assert.ok(dropped, "the bot should drop off the platform on its own");
  assert.ok(taps.length >= 2, `the sink used ${taps.length} press(es)`);
  for (const tap of taps)
    assert.ok(tap >= 3 && tap <= 6, `a sink tap ran ${tap} frames`);
});

test("hit detection follows the animated runner geometry", () => {
  const { fight, tick } = createFight();
  const player = fight.players[0];
  const resting = fight.runnerWorldGeometry(player, 0);
  tick(33334);
  const breathing = fight.runnerWorldGeometry(player, .5);
  assert.notEqual(resting.head.y, breathing.head.y);
  assert.equal(fight.runnerDistanceToPoint(
    player, 0, resting.head.x, resting.head.y, resting.head.z), 0);
  assert.ok(fight.runnerDistanceToPoint(
    player, 0, resting.head.x + 140, resting.head.y, resting.head.z) > 60);
});

test("a grounded ball ignores wind", () => {
  const { fight, signals, tick } = createFight();
  fight.enableBall();
  fight.setWind(1200);
  fight.ball.x = 450;
  fight.ball.y = fight.stageGeometry().floorY - fight.ball.radius;
  fight.ball.vx = 0;
  fight.ball.vy = 0;
  tick(16667);
  assert.equal(fight.ball.vx, 0);
  assert.equal(signals.some(([event]) => event === "boot"), false);
});

test("a ball resting on the platform also ignores wind", () => {
  const { fight, tick } = createFight();
  fight.enableBall();
  fight.setWind(1200);
  fight.ball.x = 450;
  fight.ball.y = fight.stageGeometry().platformY - fight.ball.radius;
  fight.ball.vx = 0;
  fight.ball.vy = 0;
  tick(16667);
  assert.equal(fight.ball.vx, 0);
});

test("a match inflates exactly one ball and spawns it for its own kind", () => {
  const { fight } = createFight();
  assert.equal(fight.balls.length, 1);
  assert.equal(fight.ball, fight.balls[0]);
  for (const type of ["soccer", "basketball", "beach"]) {
    fight.setBallKind(type);
    assert.equal(fight.balls.length, 1);
    assert.equal(fight.ball.type, type);
    assert.equal(fight.ball.vx, 0);
    assert.equal(fight.ball.vy, 0);
    if (fight.ball.spawnOwner >= 0) {
      const owner = fight.players[fight.ball.spawnOwner];
      assert.equal(fight.ball.x, owner.x + owner.facing * 180);
      assert.equal(fight.ball.y, 12000 - fight.ball.radius);
    } else {
      assert.ok(fight.ball.x > 7500);
      assert.equal(fight.ball.y, 12000 - 920);
    }
  }
});

test("the match ball kind is seeded on the series, never the clock", () => {
  const { fight } = createFight(false, false);
  const types = new Set();
  for (const name of ["gubba1", "dorra9", "kimmy44", "sattu2", "fezzo7",
    "nolly3", "vazzi8", "muddy5"]) {
    assert.equal(fight.seriesBallType(name), fight.seriesBallType(name));
    types.add(fight.seriesBallType(name));
  }
  // The roll varies by match instead of pinning every match to one ball.
  assert.ok(types.size > 1);
  for (const type of types)
    assert.ok(["soccer", "basketball", "beach"].includes(type));
});

test("a match records the seed its ball kind was drawn from", () => {
  // The series name is hashed to pick the ball, and ball kind carries radius
  // and mass. The name used to come straight from Math.random, so the same
  // inputs could re-run with different physics.
  const run = (seed) => {
    const original = Math.random;
    Math.random = () => seed;
    try {
      const { fight } = createFight(false);
      // Training carries no series, so a seeded match needs the bot.
      fight.startFightAgainst("bot");
      return { series: fight.seriesState(), ball: fight.ball.type,
        radius: fight.ball.radius, mass: fight.ball.mass };
    } finally { Math.random = original; }
  };
  // Same entropy in, same match identity and same physics out.
  const a = run(0.4242);
  const b = run(0.4242);
  assert.equal(a.series, b.series);
  assert.equal(a.ball, b.ball);
  assert.equal(a.radius, b.radius);

  // Different entropy still gives a distinct public name, as it must.
  const other = run(0.9137);
  assert.notEqual(other.series, a.series);

  // And the demo carries what it needs to reproduce, rather than re-deriving.
  assert.match(source, /nameSeed: nameSeedUsed, ballType: matchBallType/);
  assert.doesNotMatch(source, /onsets\[Math\.floor\(Math\.random/);
});

test("one ball kind survives every round of the same match", () => {
  const { fight } = createFight(false);
  // A dummy round is training and carries no series, so a match needs the bot.
  fight.startFightAgainst("bot");
  const series = fight.seriesState();
  assert.ok(series, "the bot opponent should start a series");
  assert.equal(fight.ball.type, fight.seriesBallType(series));
  fight.nextRound();
  assert.equal(fight.balls.length, 1);
  assert.equal(fight.ball.type, fight.seriesBallType(series));
});

test("soccer, basketball, and beach balls have distinct physical properties", () => {
  const { fight } = createFight();
  const kinds = {};
  for (const type of ["soccer", "basketball", "beach"]) {
    fight.setBallKind(type);
    kinds[type] = { ...fight.ball };
  }
  const { soccer, basketball, beach } = kinds;
  assert.ok(soccer.mass < basketball.mass);
  assert.ok(beach.mass < soccer.mass);
  assert.ok(beach.windFactor > soccer.windFactor);
  assert.ok(beach.gravityFactor < 1);
  // Redressing in place must never leave the beach ball's float behind.
  assert.equal(soccer.gravityFactor, 1);
  assert.ok(soccer.hitScale > basketball.hitScale);
  assert.ok(soccer.bounce < basketball.bounce);
  assert.match(source, /ball\.type === "soccer"/);
  assert.match(source, /ball\.type === "beach"/);
  assert.match(source, /const panels = \[/);
  assert.match(source, /polygon\(point\.x, point\.y, radius \* \.22, 5/);
  assert.match(source, /for \(const direction of \[-1, 1\]\)/);
});

test("fighter animation state is a fixed simulation-tick phase", () => {
  const { fight, tick } = createFight();
  let animation = fight.fighterAnimationPhase(fight.players[0]);
  assert.equal(animation.state, "IDLE");
  assert.equal(animation.steps, 48);
  const firstStep = animation.step;
  tick(33334);
  animation = fight.fighterAnimationPhase(fight.players[0]);
  assert.equal(animation.step, firstStep % animation.steps + 1);
  assert.match(source, /"anim::" \+ animation\.state/);
  assert.match(source, /animation\.step \+ "\/" \+\n      animation\.steps/);
  assert.doesNotMatch(source, /Math\.floor\(t \* 12\) \/ 12/);
});

test("dash momentum strengthens kicks and launches grounded balls", () => {
  const { fight } = createFight();
  const player = fight.players[0];
  player.vx = 2200;
  player.dashVx = 2200;
  player.dashUntil = Infinity;
  fight.startAttack("KICK");
  assert.ok(player.attackMomentum > 1);
  fight.enableBall();
  fight.ball.x = player.x + 50;
  fight.ball.y = 12000 - fight.ball.radius;
  fight.bootFirstBall();
  assert.equal(player.lastButton, "DASH BOOT");
  assert.ok(fight.ball.vy <= -420);
  assert.match(source, /spec\.force \* momentum, spec\.lift \* momentum/);
  assert.match(source, /KICK: \{ reach: 75, swell: 62, span: 74, height: 55/);
});

test("weather is calm while ambient dust remains visible", () => {
  const { fight, signals, tick } = createFight();
  // Wind is rolled once per round start — the entry training round and the
  // versus round here — and then never again while a round is running.
  const rolls = signals.filter(([event]) => event === "wind").length;
  for (let frame = 0; frame < 375; frame++) tick(40000);
  assert.equal(fight.windState().mph, 0);
  assert.equal(signals.filter(([event]) => event === "wind").length, rolls);
  assert.match(source, /function randomWindMph\(\) \{\n  return 0/);
  assert.match(source, /nextWindChangeAt = Infinity/);
  assert.match(source, /drawAmbientMotes\(windInk\)/);
});

test("only one visible-surface powerup appears at each ten-second interval", () => {
  const { fight, tick } = createFight();
  for (let step = 0; step < 251; step++) tick(50000);
  let active = [...fight.gunPickups, ...fight.grenadePickups]
    .filter((pickup) => pickup.active);
  assert.equal(active.length, 1);
  assert.equal(active[0].x, 4600);
  assert.equal(active[0].y, PLATFORM
    ? fight.stageGeometry().platformY - 70
    : fight.stageGeometry().floorY - 35);
  for (let step = 0; step < 250; step++) tick(50000);
  active = [...fight.gunPickups, ...fight.grenadePickups]
    .filter((pickup) => pickup.active);
  assert.equal(active.length, 1);
});

test("running into a grounded ball boots it instead of killing the player", () => {
  const { fight, pads, signals, tick } = createFight();
  const player = fight.players[0];
  fight.enableBall();
  fight.ball.x = player.x;
  fight.ball.y = 12000 - fight.ball.radius;
  fight.ball.z = player.z;
  fight.ball.vx = 0;
  fight.ball.vy = 0;
  pads[0].down = ["ArrowRight"];
  tick(16667);
  assert.equal(player.alive, true);
  assert.equal(player.lastButton, "BOOT");
  assert.ok(fight.ball.vx > 0);
  assert.ok(signals.some(([event, pad]) => event === "boot" && pad === 0));
});

test("a served ball stays in the player's lane and can be approached", () => {
  const { fight, pads, tick } = createFight();
  const player = fight.players[0];
  fight.enableBall();
  assert.equal(fight.ball.z, player.z);
  pads[0].down = ["ArrowRight"];
  for (let frame = 0; frame < 20 && player.lastButton !== "BOOT"; frame++)
    tick();
  assert.equal(player.lastButton, "BOOT");
  assert.ok(fight.ball.vx > 0);
  assert.ok(fight.ball.vx < 1000);
  assert.ok(fight.ball.vy > -120);
});

test("running into a center-platform ball boots it", { skip: !PLATFORM }, () => {
  const { fight, pads, tick } = createFight();
  const stage = fight.stageGeometry();
  const platformY = stage.platformY;
  const player = fight.players[0];
  fight.enableBall();
  player.x = (stage.platformLeft + stage.platformRight) / 2;
  player.y = platformY;
  player.grounded = true;
  fight.ball.x = player.x + 36;
  fight.ball.y = platformY - fight.ball.radius;
  fight.ball.z = player.z;
  fight.ball.vx = 0;
  fight.ball.vy = 0;
  pads[0].down = ["ArrowRight"];
  tick();
  assert.equal(player.lastButton, "BOOT");
  assert.ok(fight.ball.vx > 0);
});

test("walking contact carries a grounded ball in the player's direction", () => {
  const { fight, pads, tick } = createFight();
  const player = fight.players[0];
  fight.enableBall();
  fight.ball.x = player.x;
  fight.ball.y = 12000 - fight.ball.radius;
  fight.ball.z = player.z;
  fight.ball.vx = 0;
  fight.ball.vy = 0;
  pads[0].down = ["ArrowLeft"];
  tick();
  assert.ok(fight.ball.vx < 0);
});

test("A plus B grabs and carries a ball through a jump, then releases it", () => {
  const { fight, pads, tick } = createFight();
  const player = fight.players[0];
  fight.enableBall();
  fight.ball.x = player.x + 80;
  fight.ball.y = player.y - 90;
  fight.ball.z = player.z;
  pads[0].down = ["A", "B"];
  tick();
  assert.equal(player.heldBall, 0);
  assert.equal(fight.ball.heldBy, 0);
  assert.equal(player.attackKind, "");
  pads[0].down = ["A", "B", "ArrowUp"];
  tick();
  tick(90000);
  assert.ok(player.vy < 0);
  assert.equal(fight.ball.heldBy, 0);
  pads[0].down = [];
  tick();
  assert.equal(player.heldBall, -1);
  assert.equal(fight.ball.heldBy, -1);
  pads[0].down = ["B"];
  tick();
  assert.equal(player.attackKind, "PUNCH");
});

test("A plus B shows reaching and two-hand holding poses", () => {
  const { fight, pads, tick } = createFight();
  const player = fight.players[0];
  pads[0].down = ["A", "B"];
  tick();
  assert.equal(player.stance, "REACHING");
  assert.equal(player.lastButton, "REACHING");
  pads[0].down = [];
  tick();
  fight.enableBall();
  fight.ball.x = player.x + 100;
  fight.ball.y = player.y - 82;
  pads[0].down = ["A", "B"];
  tick();
  assert.equal(player.stance, "HOLDING");
  assert.equal(player.lastButton, "HOLDING");
  assert.match(source, /if \(player\.grabHeld\) \{/);
  assert.match(source, /const hands = \[/);
});

test("player command streams retain recent directions and buttons", () => {
  const { fight, pads, tick } = createFight();
  pads[0].down = ["ArrowRight", "A"];
  tick();
  assert.deepEqual(fight.players[0].commandStream.map((entry) => entry.label),
    ["RIGHT", "/"]);
  assert.match(source, /function drawCommandStream\(player, side\)/);
  assert.match(source,
    /const glyph = \{ LEFT: "<", RIGHT: ">", UP: "\^", DOWN: "v" \}/);
  assert.match(source, /fade: commandFade\(index, count, idle\)/);
  assert.match(source,
    /while \(player\.commandStream\.length > commandStreamDepth\)/);
  assert.match(source,
    /commandStreamStackHeight\(player\) -\s*\n\s*size - 7/);
  assert.doesNotMatch(source, /recordCommand\(player, "RUN", now\)/);
  assert.match(source, /buttonFor\[entry\.label\]\.some\(\(button\) => held\.includes\(button\)\)/);
  // The held tint lives in the pad-button renderer now, not a local palette.
  assert.match(source, /padButtonInk\[label\]/);
  const streamSource = source.slice(source.indexOf("function drawCommandStream"),
    source.indexOf("function drawFightIntro"));
  assert.match(streamSource, /const size = commandStreamTypeSize\(handle\.size\)/);
  assert.match(streamSource,
    /let cursor = side === 0 \? handle\.x : handle\.x \+ handle\.width - width/);
  assert.match(streamSource,
    /handle\.y - statStackHeight\(\) - \(row \+ 1\) \* \(size \+ 7\) - rise/);
});

// The buffer is a record of an exchange, so it holds one — twenty presses, and
// nothing dissolves until the pad has actually gone quiet. The fade then walks
// oldest-to-newest rather than dimming the block as a single sheet.
test("the command stream holds an exchange and ages out oldest first", () => {
  assert.match(source, /const commandStreamDepth = 20/);
  assert.match(source, /const commandStreamRows = 5/);
  const { fight, pads, tick } = createFight();
  const player = fight.players[0];
  const buttons = ["ArrowLeft", "ArrowRight", "ArrowUp", "A", "B", "X", "Y"];
  for (let press = 0; press < 30; press++) {
    pads[0].down = [buttons[press % buttons.length]];
    tick();
    pads[0].down = [];
    tick(60000);
  }
  assert.equal(player.commandStream.length, 20,
    "twenty presses stay on the wall");
  const fadesAt = (idle) =>
    Array.from({ length: 20 }, (unused, index) =>
      fight.commandFade(index, 20, idle));
  // Still playing, and the whole record is legible.
  assert.deepEqual(fadesAt(0).filter((fade) => fade < 1), [],
    "nothing fades while the pad is still saying something");
  assert.deepEqual(fadesAt(900000).filter((fade) => fade < 1), [],
    "a pause between inputs is not an idle");
  // Idle: the oldest glyph is gone while the newest is still solid.
  const middle = fadesAt(2050000);
  assert.equal(middle[0], 0, "the oldest glyph leaves first");
  assert.equal(middle.at(-1), 1, "the newest glyph is still solid");
  assert.ok(middle.every((fade, index) => index === 0 || fade >= middle[index - 1]),
    "the fade only ever climbs toward the newest glyph");
  // Long idle: the wall is clear.
  assert.deepEqual(fadesAt(9000000).filter((fade) => fade > 0), [],
    "the buffer eventually leaves entirely");
  const streamSource = source.slice(source.indexOf("function drawCommandStream"),
    source.indexOf("function drawFightIntro"));
  assert.match(streamSource,
    /entries\.slice\(0, commandStreamDepth\)/);
  assert.match(streamSource, /const columns = commandStreamColumnsNow\(\)/);
  assert.match(streamSource, /index \+= columns/);
  assert.match(source,
    /const commandStreamColumnsNow = \(\) => compactLayout\(\) \? 4 : commandStreamColumns/);
});

// `frameNow` used to be the animation's position in its cycle rather than a
// clock. The looping states date from match start, so that number wrapped back
// into the first second forever, and every `timestamp > poseNow` test answered
// yes for the rest of the round. One landing was enough to pin a fighter in
// the crouch pose — which draws static legs — so walking stopped moving them.
test("a fighter who has landed still walks", () => {
  const { fight, pads, tick } = createFight();
  const player = fight.players[0];
  const leadFoot = () => {
    const shin = fight.runnerWorldGeometry(player, 0).segments
      .find((segment) => segment.role === "lead-shin");
    return shin ? Math.round(shin.x2 - player.x) : null;
  };
  const strideSpread = () => {
    const feet = [];
    for (let frame = 0; frame < 40; frame++) {
      tick();
      feet.push(leadFoot());
    }
    assert.ok(feet.every((foot) => foot !== null),
      "a grounded walk draws lead legs");
    return Math.max(...feet) - Math.min(...feet);
  };
  pads[0].down = ["ArrowRight"];
  assert.ok(strideSpread() > 20, "the fresh walk takes steps");
  // Jump, land, then walk again.
  pads[0].down = [];
  for (let frame = 0; frame < 30; frame++) tick();
  pads[0].down = ["ArrowUp"];
  for (let frame = 0; frame < 5; frame++) tick();
  pads[0].down = [];
  for (let frame = 0; frame < 90; frame++) tick();
  assert.equal(player.grounded, true, "the fighter has landed");
  pads[0].down = ["ArrowRight"];
  assert.ok(strideSpread() > 20, "the walk after a landing still takes steps");
  assert.equal(fight.fighterAnimationPhase(player).state, "WALK");
  // A crouch hop is a landing too, and it was the reported way in.
  pads[0].down = ["ArrowDown"];
  for (let frame = 0; frame < 30; frame++) tick();
  pads[0].down = []; tick();
  pads[0].down = ["ArrowRight"]; tick();
  pads[0].down = []; tick();
  pads[0].down = ["ArrowRight"];
  for (let frame = 0; frame < 60; frame++) tick();
  assert.ok(strideSpread() > 20, "the walk after a crouch hop still takes steps");
});

// The dummy fight runs live under the title screen, so the one statistic the
// whole site shares is how many of its heads have come off lately.
test("popping a dummy is announced, and popping yourself is not", () => {
  const { fight, signals, tick } = createFight(false, false);
  fight.startFightAgainst("dummy");
  tick(3000001);
  const popsSoFar = () => signals.filter(([event]) => event === "dummy-popped");
  assert.equal(popsSoFar().length, 0);
  fight.knockOut();
  assert.equal(popsSoFar().length, 1, "a popped dummy is counted once");
  assert.deepEqual(popsSoFar()[0].slice(0, 3), ["dummy-popped", 0, 1]);

  // A live opponent is somebody's fighter, not the shared punching bag.
  const versus = createFight();
  versus.fight.knockOut();
  assert.deepEqual(
    versus.signals.filter(([event]) => event === "dummy-popped"), []);

  // And a dummy that balls itself — a ground pound is the way — has not been
  // popped by anyone, so nobody gets the credit.
  const solo = createFight(false, false);
  solo.fight.startFightAgainst("dummy");
  solo.tick(3000001);
  solo.fight.selfBallDummy();
  assert.equal(solo.fight.players[1].alive, false, "the dummy did die");
  assert.deepEqual(
    solo.signals.filter(([event]) => event === "dummy-popped"), []);
});

test("the tally reads as a sentence, and says nothing when it is empty", () => {
  const { fight } = createFight(false, false);
  const line = fight.dummyPopLine;
  assert.equal(line({ pops: 1, hours: 1 }),
    "1 dummy head popped in the last hour");
  assert.equal(line({ pops: 37, hours: 1 }),
    "37 dummy heads popped in the last hour");
  assert.equal(line({ pops: 204, hours: 6 }),
    "204 dummy heads popped in the last 6 hours");
  assert.equal(line({ pops: 2, hours: 48 }),
    "2 dummy heads popped in the last 48 hours");
  // Nothing to boast about, nothing said. Anything the host has not answered
  // yet, or answered strangely, reads the same way.
  assert.equal(line({ pops: 0, hours: 24 }), "");
  assert.equal(line(null), "");
  assert.equal(line(undefined), "");
  assert.equal(line({ pops: -5, hours: 24 }), "");
  assert.equal(line({ pops: 9, hours: 0 }), "");
  // Forty-eight hours is the ceiling the endpoint quotes; a longer window is
  // a host talking nonsense and gets ignored rather than printed.
  assert.equal(line({ pops: 9, hours: 72 }), "");
});

test("the burned poster never carries the moving tally", () => {
  // The preview's hash is checked at deploy, so a number that ticks hourly
  // would make the shipped poster stale on its own.
  const titleSource = source.slice(source.indexOf("function drawDummyPopLine"),
    source.indexOf("function titleButtonRect"));
  assert.match(titleSource, /capabilities\(\)\.socialPreview === true\) return/);
  assert.match(webShell, /async function readDummyPops/);
  assert.match(webShell, /if \(socialPreview\) return;/);
  assert.match(webShell, /if \(event === "dummy-popped"\) reportDummyPop\(\)/);
  assert.match(webShell, /const popsEndpoint = "\/api\/oskiewar-pops"/);
});

test("owned gun and grenade counts render above each bottom handle", () => {
  assert.match(source, /function drawHudInventory\(player, side\)/);
  assert.match(source, /items\.push\("gun " \+ player\.gunAmmo\)/);
  assert.match(source, /items\.push\("grenade " \+ player\.grenadeAmmo\)/);
  assert.match(source, /drawHudInventory\(player, side\)/);
  const { fight } = createFight();
  fight.players[0].gunAmmo = 3;
  assert.ok(fight.runnerWorldGeometry(fight.players[0], 0).segments
    .some((segment) => segment.role === "item-forearm"));
  assert.doesNotMatch(source, /gunPips|grenadePips/);
  const inventorySource = source.slice(source.indexOf("function drawInventory"),
    source.indexOf("function drawDigitalHeadBurst"));
  assert.match(inventorySource,
    /filledCapsule\(hand\.x, hand\.y, barrel\.x, barrel\.y/);
  assert.doesNotMatch(inventorySource, /\bline\(/);
});

test("player animation state reads as a boxed syntax diagram", () => {
  const { fight, pads, tick } = createFight();
  pads[0].down = ["ArrowRight"];
  tick();
  const player = fight.players[0];
  const lines = fight.playerStatLines(player);
  assert.equal(lines.length, 3);
  assert.match(lines[0], /^p1 :: /);
  assert.match(lines[1], /^in\[ 1, 0\] -> stk\[ 0\.00\] vx\[\s*-?\d+\]$/);
  assert.match(lines[2], /^anim::\w+\s+ step\[\s*\d+\/\s*\d+\] t\[\s*\d+\]$/);
  const handle = fight.playerHandleLayout(player, 0);
  assert.equal(handle.size, 42);
  fight.setDebugHitboxes(true);
  assert.ok(fight.statStackHeight() > 80);
  fight.setDebugHitboxes(false);
  assert.equal(fight.statStackHeight(), 0);
  assert.match(source, /triangleDepth = -1\.445/);
  assert.match(source, /screenStrokeRect\(x, y, width, height, 2, edge\)/);
  assert.doesNotMatch(source.slice(source.indexOf("function drawPlayerStats"),
    source.indexOf("function drawHudInventory")), /\bbox\(/);
  assert.match(source, /const width = compactLayout\(\) \? 360 : 520/);
  assert.doesNotMatch(source.slice(source.indexOf("function drawPlayerStats"),
    source.indexOf("function drawHudInventory")), /const track|segmentWidth/);
  assert.match(source, /const hudPlayers = spatialHudPlayers\(\)/);
  assert.match(source, /drawPlayerStats\(player, side, t\)/);
  assert.match(source, /const handle = playerHandleLayout\(player, side\)/);
  assert.match(source, /handle\.y - height - 12/);
  // The read-out left the world; nothing labels the fighters any more.
  assert.doesNotMatch(source.slice(source.indexOf("function drawDebugHitboxes"),
    source.indexOf("function drawPlayerHud")), /typeWrite/);
});

test("HUD ownership follows fighter position with crossing hysteresis", () => {
  assert.match(source, /function spatialHudPlayers\(\)/);
  assert.match(source, /if \(other\.x < left\.x - 36\) hudLeftPad = other\.pad/);
  assert.match(source, /drawPlayerHandle\(player, t, side\)/);
  assert.match(source, /drawCommandStream\(player, side\)/);
});

test("persistent controls teach the two-button hold chord", () => {
  assert.match(source, /both \? "GRAB" : held\.includes\("A"\) \? "KICK"/);
  assert.match(source, /!both && held\.includes\("B"\) \? "PUNCH"/);
  assert.doesNotMatch(source, /= HOLD/);
  assert.match(source, /safe\.top \+ row \* step/);
  assert.match(source, /const controlRailWidth/);
  assert.match(source, /pad\.leftX < -\.12/);
  assert.match(source, /pad\.leftX > \.12/);
  assert.match(source, /pad\.leftY > \.12/);
  assert.match(source, /const step = Math\.round\(size \* 1\.82\)/);
  assert.match(source, /\[\["RIGHT", "RIGHT"\], ">> DASH"/);
  assert.match(source, /\[\["LEFT", "RIGHT"\], "MOVE"/);
  assert.match(source, /Object\.hasOwn\(padGlyph, label\)/);
  assert.match(source,
    /label === "STICK_UP" \|\| label === "LEFT" \|\| label === "RIGHT"/);
});

test("handle palette gradients survive on detached limbs and anchor the head at @", () => {
  assert.match(source, /function drawPaletteCapsule\(/);
  // Six bands is the look; the experiment dial can flatten it remotely.
  assert.match(source,
    /const bands = colors\?\.length > 1\n\s*\? clamp\(Math\.round\(renderFlags\.bands \?\? 6\), 1, 6\) : 1/);
  assert.match(source, /player\.handleColors\[0\] : color/);
  assert.match(source, /colors: player\.npc \? \[\] : player\.handleColors/);
  assert.match(source, /paletteCoordinate/);
});

test("dummy state card describes an inert destructible target", () => {
  const { fight } = createFight();
  fight.startFightAgainst("dummy");
  const lines = fight.playerStatLines(fight.players[1]);
  assert.deepEqual(lines.length, 3);
  assert.match(lines[0], /^p2 :: training dummy$/);
  assert.match(lines[1], /^target::inert parts\[ 5\/ 5\] dmg\[ 0\]$/);
  assert.doesNotMatch(lines.join(" "), /stk|vx/);
});

test("diagnostic lines color handles, numbers, units, labels, and punctuation", () => {
  const { fight } = createFight(false, false);
  const line = "@jeffrey vx -240, stk 0.50";
  const tokens = fight.dumpTokens(line);
  assert.equal(tokens.join(""), line);
  const ink = (token) => fight.dumpTokenInk(token).join(",");
  assert.equal(new Set([ink("@jeffrey"), ink("-240"), ink("vx"),
    ink("stance"), ink(",")]).size, 5);
  // A colored run advances on the same comic metrics a plain line would.
  assert.equal(
    Math.round(tokens.reduce((width, token) =>
      width + fight.handleWidth(token, 42), 0) * 1000),
    Math.round(fight.handleWidth(line, 42) * 1000));
  assert.match(source, /writeDumpLine\(wrapped, 132, cursorY, 27\)/);
  assert.match(source, /writeTokens\(typeWrite, text, x, y, size\)/);
});

test("safe-zone debug marks corners instead of drawing full boxes", () => {
  const { fight, triangles } = createFight(false, false);
  const rect = { left: 100, top: 100, right: 900, bottom: 700 };
  triangles.length = 0;
  fight.drawCornerCrops(rect, 40, 3, [255, 214, 84]);
  assert.ok(triangles.length > 0);
  const corners = [[100, 100], [900, 100], [100, 700], [900, 700]];
  for (const values of triangles)
    for (const index of [0, 3, 6])
      assert.ok(corners.some(([x, y]) => Math.abs(values[index] - x) <= 48 &&
        Math.abs(values[index + 1] - y) <= 48));
  const zoneSource = source.slice(source.indexOf("function drawSafeZones"),
    source.indexOf("function gamePaint"));
  assert.doesNotMatch(zoneSource, /typeWrite|drawRectOutline/);
  assert.match(zoneSource, /drawCornerCrops\(hudSafeRect\(\), 46, 3/);
  assert.match(zoneSource, /drawCornerCrops\(actionSafeRect\(\), 34, 2/);
});

test("a console whose clock reads negative can still fight", () => {
  // App.cpp overflows int64 converting QPC ticks past ~10 days of uptime.
  // Every deadline here starts at 0, so `now < hitStunUntil` was permanently
  // true: acting was impossible, no attack fired, and no round could be won.
  const uptimeUs = -3753801036;
  const { fight, pads, tick } = createFight(true, true, "xbox-uwp", null,
    { width: 1920, height: 1080 }, null, null, "triangle3d", uptimeUs);
  const attacker = fight.players[0];
  const target = fight.players[1];
  attacker.x = target.x - 120;
  pads[0].down = ["B"];
  let struck = false;
  for (let frame = 0; frame < 30 && !struck; frame += 1) {
    tick();
    struck = Boolean(attacker.attackKind);
  }
  assert.ok(struck, "no attack could fire on a negative clock");
  assert.equal(fight.clientErrorState(), "");

  // Time itself must read forward from zero, not from the host's number.
  const shown = fight.cameraState();
  assert.ok(shown, "the frame never resolved");
  assert.match(source, /const hostRuntime = runtime;/);
  assert.match(source, /info\.monotonicUs = raw - clockEpoch;/);
});

test("a negative native clock still paints the beach ball's panels", () => {
  // App.cpp overflows int64 converting QPC ticks, so monotonicUs reads
  // negative past ~10 days of uptime. A negative palette step used to index
  // off the front of the panel list and crash paint with a TypeError.
  const { fight, tick } = createFight(true, true, "xbox-uwp", null,
    { width: 1920, height: 1080 }, null, null, "triangle3d", -3753801036);
  fight.setBallKind("beach");
  fight.enableBall();
  tick();
  // paint() swallows throws into the error card by design, so the assertion
  // has to read what it captured rather than watch for an exception.
  fight.paint();
  assert.equal(fight.clientErrorState(), "",
    "a negative clock crashed paint: " + fight.clientErrorState());
});

test("ball graphics rotate from physics only and use no white line outline", () => {
  const { fight, tick } = createFight();
  fight.enableBall();
  fight.ball.x = 9000;
  fight.ball.y = 12000 - fight.ball.radius;
  fight.ball.vx = 0;
  fight.ball.vy = 0;
  const stillRotation = fight.ball.rotation;
  tick();
  assert.equal(fight.ball.rotation, stillRotation);
  fight.ball.vx = 500;
  tick();
  assert.notEqual(fight.ball.rotation, stillRotation);
  assert.match(source, /function drawBall\(ball\)/);
  assert.doesNotMatch(source,
    /circle\(point\.x, point\.y, radius,[^\n]*\[245, 248, 255\]/);
});

test("fighters and balls share one projected global-light shadow system", () => {
  assert.match(source, /const globalLight = normalize3/);
  assert.match(source, /drawSpotShadow\(player\.x, player\.y, player\.z/);
  assert.match(source, /drawSpotShadow\(item\.x, item\.y, item\.z/);
  assert.match(source, /const radiusY = Math\.max\(2, radiusX/);
  assert.match(source, /projectPoint\(x, y, z\)\.z \+ \.018/);
});

test("ball visuals and debug hitboxes share one projected circular radius", () => {
  const { fight } = createFight();
  fight.enableBall();
  assert.ok(fight.projectedBallRadius(fight.ball) > 0);
  assert.match(source, /function drawBallHitboxes\(\)/);
  assert.match(source, /filledRing\(point\.x, point\.y, radius \+ 5, radius \+ 2/);
});

test("native terrain and actors carry real depth with computed face normals", () => {
  const { fight, triangles } = createFight();
  triangles.length = 0;
  fight.paint();
  const depths = triangles.flatMap((values) => [values[2], values[5], values[8]]);
  assert.ok(new Set(depths.map((value) => value.toFixed(4))).size > 3);
  assert.ok(depths.every((value) => value >= -1.5 && value <= 1.5));
  // The quad normal runs in scalars now — same cross, normalize and dot,
  // without the per-panel vector objects the interpreter was paying for.
  assert.match(source, /const nx = aby \* acz - abz \* acy/);
  assert.match(source, /const magnitude = Math\.hypot\(nx, ny, nz\) \|\| 1/);
  assert.match(source, /typeof triangle3d === "function"/);
  assert.doesNotMatch(source, /worldLine\(worldLeft, floorY/);
});

const batchedFight = () => createFight(true, true, "xbox-uwp", null,
  { width: 1920, height: 1080 }, null, null, "triangles3d");

// Two fights only draw the same frame with the wall clock held still — the sun
// angle tints every color — and with the random match name pinned, since the
// name's width sizes its HUD plate.
function stillFrame(run) {
  const [random, now] = [Math.random, Date.now];
  Math.random = () => .5;
  Date.now = () => 1785870000000;
  try { return run(); } finally { Math.random = random; Date.now = now; }
}

// A frame's faces are quantized to float32 on the way into the batch, which is
// exactly what the host casts them to anyway.
const asFloat32 = (values) => values.map((value) => Math.fround(value));

// Detached limbs are the cheapest way to push one paint past the host's cap.
function litterDetachedParts(fight, count) {
  for (let index = 0; index < count; index++)
    fight.detachedParts.push({
      x1: 5200 + index, y1: 9000, z1: 0,
      x2: 5320 + index, y2: 9200, z2: 0,
      width: 44, color: [180, 90, 60], life: 2,
    });
}

test("capsule ends fan from the rim instead of the center", () => {
  const { fight, triangles } = createFight();
  fight.paint();
  triangles.length = 0;
  fight.drawDetachedPart({ x1: 5200, y1: 9000, z1: 0,
    x2: 5320, y2: 9200, z2: 0, width: 44, color: [180, 90, 60] });
  // Silhouette pass + color pass, each 2 body faces and two end caps whose
  // face count follows the cap radius rather than a fixed ring.
  const caps = (triangles.length / 2 - 2) / 2;
  assert.equal(triangles.length, 2 * (2 + caps * 2));
  const uses = new Map();
  for (const values of triangles)
    for (let at = 0; at < 9; at += 3) {
      const key = `${values[at]},${values[at + 1]}`;
      uses.set(key, (uses.get(key) || 0) + 1);
    }
  // A center fan touches its hub once per side; a rim fan never exceeds
  // sides - 2, so the hub count is the tell.
  assert.ok(Math.max(...uses.values()) <= caps);
});

test("a disc spends its faces on the silhouettes that show them", () => {
  const { fight, triangles } = createFight();
  const facesFor = (radius) => {
    triangles.length = 0;
    fight.filledDisc(400, 400, radius, [10, 20, 30]);
    return triangles.length;
  };
  const cap = facesFor(4);
  const head = facesFor(90);
  // A head used to be the same decagon as a limb cap, and showed its corners.
  assert.ok(head > cap * 4, `head ${head} faces against cap ${cap}`);
  // And a tiny cap is cheaper than the one fixed ring it replaced.
  assert.ok(cap < 8, `a 4px cap still costs ${cap} faces`);
  // Monotonic: never fewer faces for a larger disc.
  let previous = 0;
  for (const radius of [2, 8, 20, 45, 90, 200]) {
    const faces = facesFor(radius);
    assert.ok(faces >= previous, `radius ${radius} dropped to ${faces}`);
    previous = faces;
  }
});

test("both per-face host shapes still receive every face", () => {
  const perFace = createFight();
  const flat = createFight(true, true, "xbox-uwp", null,
    { width: 1920, height: 1080 }, null, null, "triangle");
  for (const host of [perFace, flat]) {
    host.triangles.length = 0;
    host.fight.paint();
    assert.ok(host.triangles.length > 400,
      `only ${host.triangles.length} faces`);
  }
  // The point is parity: the same scene, whichever entry the host offers.
  assert.equal(perFace.triangles.length, flat.triangles.length);
  assert.ok(perFace.triangles.every((values) => values.length === 12));
  assert.ok(flat.triangles.every((values) => values.length === 9));
});

test("an airborne ball head hit damages every limb once without killing", () => {
  const { fight, tick, now } = createFight();
  const player = fight.players[0];
  const head = fight.runnerWorldGeometry(player, now() / 1000000).head;
  fight.enableBall();
  fight.ball.x = head.x;
  fight.ball.y = head.y;
  fight.ball.z = head.z;
  fight.ball.vx = 900;
  fight.ball.vy = 0;
  tick(1000);
  assert.equal(player.alive, true);
  assert.equal(player.lastButton, "HEAD HIT");
  assert.equal(fight.ball.active, true);
  assert.deepEqual(Object.fromEntries(limbPartsForTest.map((part) =>
    [part, player.partDamage[part]])), Object.fromEntries(
    limbPartsForTest.map((part) => [part, 1])));
  assert.deepEqual(player.removedParts, []);
});

test("swept ball head collision damages a dummy without tunneling", () => {
  const { fight, tick, now } = createFight();
  const dummy = fight.players[1];
  dummy.name = "DUMMY";
  dummy.npc = true;
  const head = fight.runnerWorldGeometry(dummy, now() / 1000000).head;
  fight.enableBall();
  fight.ball.x = head.x - 210;
  fight.ball.y = head.y;
  fight.ball.z = head.z;
  // Fast enough to cross the whole head inside one fixed step — the
  // tunneling case. Fixed 60 Hz ticks, because one oversized elapsed gets
  // clamped to a single 40ms integration and the sweep never happens.
  fight.ball.vx = 12000;
  fight.ball.vy = 0;
  for (let frame = 0; frame < 3; frame++) tick(16667);
  assert.equal(dummy.alive, true);
  assert.equal(dummy.lastButton, "HEAD HIT");
  for (const part of limbPartsForTest)
    assert.equal(dummy.partDamage[part], 1);
});

test("an airborne ball bounces off non-head body geometry", () => {
  const { fight, signals, tick, now } = createFight();
  const player = fight.players[0];
  const geometry = fight.runnerWorldGeometry(player, now() / 1000000);
  const arm = geometry.segments.at(-1);
  fight.enableBall();
  fight.ball.x = arm.x2 + player.facing * 22;
  fight.ball.y = arm.y2;
  fight.ball.z = arm.z2;
  fight.ball.vx = -1100;
  fight.ball.vy = -200;
  tick(1000);
  assert.equal(player.alive, true);
  assert.equal(fight.ball.active, true);
  assert.deepEqual(player.partDamage, {});
  assert.ok(signals.some(([event, pad]) => event === "bodybounce" && pad === 0));
});

test("melee returns label and signal the ball as WACK", () => {
  const { fight, signals } = createFight();
  const player = fight.players[0];
  fight.enableBall();
  fight.ball.x = player.x + 185;
  fight.ball.y = player.y - 55;
  fight.ball.vx = -500;
  fight.ball.vy = -220;
  fight.wackBall();
  assert.equal(player.lastButton, "WACK");
  assert.ok(Math.abs(fight.ball.vx) >= 1300);
  assert.ok(signals.some(([event, pad]) => event === "wack" && pad === 0));
});

test("shielding pops a ball upward without the old extreme launch", () => {
  const { fight } = createFight();
  fight.enableBall();
  fight.ball.x = fight.players[0].x + 90;
  fight.ball.vx = -500;
  fight.ball.vy = 0;
  fight.wackBall();
  const normalSpeed = Math.hypot(fight.ball.vx, fight.ball.vy);
  fight.ball.x = fight.players[0].x + 90;
  fight.ball.vx = -500;
  fight.ball.vy = 0;
  fight.shieldBall();
  const shieldSpeed = Math.hypot(fight.ball.vx, fight.ball.vy);
  assert.ok(shieldSpeed > normalSpeed * 1.5);
  assert.ok(shieldSpeed < normalSpeed * 2.8);
  assert.ok(fight.ball.vy < -1500);
});

test("shielding a grounded ball blasts it instead of booting it", () => {
  const { fight, pads, signals, tick } = createFight();
  const player = fight.players[0];
  fight.enableBall();
  // The normal serve distance is beyond the fighter body but inside the
  // visible shield, so this catches body-only shield collision regressions.
  fight.ball.x = player.x + 180;
  fight.ball.y = 12000 - fight.ball.radius;
  fight.ball.z = player.z;
  fight.ball.vx = -80;
  fight.ball.vy = 0;
  pads[0].down = ["X"];
  tick();
  assert.ok(fight.ball.vx >= 1800);
  assert.ok(fight.ball.vx <= 4200);
  assert.ok(fight.ball.vy < -1200);
  assert.ok(signals.some(([event, pad]) => event === "ballblock" && pad === 0));
  assert.ok(!signals.some(([event, pad]) => event === "boot" && pad === 0));
});

test("shield blast strength increases as a grounded ball gets closer", () => {
  const blastAt = (offset) => {
    const { fight, pads, tick } = createFight();
    const player = fight.players[0];
    fight.enableBall();
    fight.ball.x = player.x + offset;
    fight.ball.y = 12000 - fight.ball.radius;
    fight.ball.z = player.z;
    fight.ball.vx = 0;
    fight.ball.vy = 0;
    pads[0].down = ["X"];
    tick();
    return Math.abs(fight.ball.vx);
  };
  assert.ok(blastAt(20) > blastAt(180));
});

test("attack poses preserve two-bone limb lengths", () => {
  const { fight, pads, tick, now } = createFight();
  pads[0].down = ["B"];
  tick(90000);
  const geometry = fight.runnerWorldGeometry(
    fight.players[0], now() / 1000000);
  const lengths = geometry.segments.map((segment) => Math.hypot(
    segment.x2 - segment.x1, segment.y2 - segment.y1));
  assert.ok(Math.max(...lengths) < 80);
});

test("a close simultaneous punch and kick produce a stronger CROSS WACK", () => {
  const { fight, signals } = createFight();
  fight.enableBall();
  fight.ball.vx = 0;
  fight.ball.vy = 0;
  fight.players[0].x = 5800;
  fight.players[1].x = 6200;
  fight.players[0].facing = 1;
  fight.players[1].facing = -1;
  fight.crossWackBall(1);
  assert.equal(fight.players[0].lastButton, "CROSS WACK");
  assert.equal(fight.players[1].lastButton, "CROSS WACK");
  assert.ok(Math.abs(fight.ball.vx) > 5000);
  assert.ok(signals.some(([event, pad]) => event === "crosswack" && pad === -1));
});

test("round clock can end in a tie and returns to title", () => {
  const { fight, tick } = createFight();
  for (let frame = 0; frame < 750; frame++) tick(40000);
  assert.equal(fight.roundState().roundResult, "TIE");
  tick(3000001);
  assert.equal(fight.roundState().roundResult, "");
  assert.equal(fight.shellState().mode, "MENU");
  assert.equal(fight.players[0].score, 0);
  assert.equal(fight.players[1].score, 0);
});

test("dummy rounds are untimed and omit the round clock", () => {
  const { fight, tick } = createFight();
  fight.players[1].npc = true;
  fight.players[1].bot = false;
  fight.players[1].name = "DUMMY";
  for (let frame = 0; frame < 800; frame++) tick(40000);
  assert.equal(fight.roundState().roundResult, "");
  assert.ok(fight.roundState().roundElapsedUs > 30000000);
  // The reel harness may put a scripted dummy bout on the clock; the
  // untimed default survives beneath that exception.
  assert.match(source, /function roundIsTimed\(\)[\s\S]{0,600}!\(players\[1\]\.npc && !players\[1\]\.bot\)/);
  assert.match(source, /timedRound \? String\(remainingSeconds\)\.padStart\(2, "0"\) : ""/);
  assert.doesNotMatch(source, /timerText === "∞"/);
});

test("the deprecated endless clock is absent", () => {
  assert.doesNotMatch(source, /timerText === "∞"/);
  assert.doesNotMatch(source, /Math\.sin\(t \* 4\.6\)/);
});

test("time of day is title-only and absent from the in-match HUD", () => {
  const boxSource = source.match(
    /function hudClockBox\(unixMs\) \{[\s\S]*?\n\}/)[0];
  assert.match(boxSource, /pacificTimeLabel\(unixMs\)/);
  assert.match(boxSource, /qrBox \? qrBox\.left - 14 : safe\.right/);
  const titleSource = source.slice(source.indexOf("function drawTitleScreen"),
    source.indexOf("function pacificTimeLabel"));
  assert.match(titleSource, /drawHudClock\(clock,/);
  const gameSource = source.slice(source.indexOf("function gamePaint"),
    source.indexOf("\nfunction boot() {", source.indexOf("function gamePaint")));
  assert.doesNotMatch(gameSource, /drawHudClock\(/);
  assert.match(gameSource, /drawHudStatusTray\(null, titleInk, nowMs\)/);
  const { fight, tick } = createFight();
  // Removing the wall clock does not disturb the rest of the gameplay HUD.
  fight.players[1].npc = true;
  fight.players[1].bot = false;
  tick();
  assert.doesNotThrow(() => fight.paint());
});

test("dummy training has no QR, analytics, spectator feed, or replay upload", () => {
  const { fight, analyticsEvents, liveFrames, replays, tick } =
    createFight(false, false);
  fight.startFightAgainst("dummy");
  assert.equal(fight.players[1].name, "DUMMY");
  tick(3000001);
  tick(100000);
  assert.deepEqual(analyticsEvents, []);
  assert.deepEqual(liveFrames, []);
  assert.deepEqual(replays, []);
  assert.match(source,
    /function spectatorQrBox\(\)[\s\S]{0,180}if \(shellMode === "GAME" && !roundIsTimed\(\)\) return null/);
  assert.match(source,
    /if \(!roundIsTimed\(\)\) \{[\s\S]{0,300}replay = null/);
});

test("the final ten seconds ring bells and turn the top clock into tie", () => {
  const { fight, drums, tick } = createFight();
  for (let frame = 0; frame < 500; frame++) tick(40000);
  assert.ok(drums.some(([name]) => name === "bell"));
  assert.match(source, /emitSignal\("countdown", -1, countdownSecond, 0\)/);
  assert.match(source, /roundResult === "TIE" \? "tie!"/);
  assert.match(source, /const timerSize = timedRound \? hudTypeSize/);
});

test("legacy Xbox drum allowlists cannot stop bell or whoosh cues", () => {
  const played = [];
  const legacyDrum = (name, velocity, pan) => {
    if (name === "bell" || name === "whoosh")
      throw new RangeError("unknown drum");
    played.push([name, velocity, pan]);
  };
  const { fight } = createFight(false, false, "xbox-uwp", null,
    { width: 1920, height: 1080 }, null, legacyDrum);
  assert.doesNotThrow(() => fight.playDrum("bell", 1, 0));
  assert.doesNotThrow(() => fight.playDrum("whoosh", 1, .5));
  assert.deepEqual(played.slice(-2).map(([name]) => name), ["hat", "block"]);
  assert.equal(fight.clientErrorState(), "");
});

test("client failures become an on-screen error state", () => {
  const { fight, telemetryEvents, setHostErrorStatus } = createFight(false, false);
  const error = new RangeError("unknown drum");
  error.stack = "RangeError: unknown drum\n    at playDrum (oskiewar.js:724:9)";
  fight.captureClientError("sim", error);
  assert.match(fight.clientErrorState(), /sim: RangeError: unknown drum/);
  assert.equal(fight.clientErrorDetailState().source.line, 724);
  const payload = JSON.parse(telemetryEvents.find(([event]) =>
    event === "CLIENT_ERROR")[1]);
  assert.equal(payload.phase, "sim");
  assert.equal(payload.source.file, "oskiewar.js");
  assert.equal(payload.state.players.length, 2);
  setHostErrorStatus("posted to server xbox-oskiewar-test");
  assert.equal(fight.errorReportStatus(), "posted to server xbox-oskiewar-test");
  assert.doesNotThrow(() => fight.paint());
  assert.match(source, /errorTypeWrite\("the game needs a moment"/);
  assert.match(source, /errorTypeWrite\("state dump"/);
  assert.match(source, /line " \+ detail\.source\.line/);
  assert.match(source, /errorTypeWrite\("relaunch or deploy an update"/);
  assert.match(source, /function errorTypeWrite[\s\S]{0,120}typeWrite\(/);
  assert.match(source, /typeof comicWrite === "function"/);
  assert.match(source, /telemetry\("CLIENT_ERROR", JSON\.stringify/);
  assert.match(nativeApp, /QueueClientErrorUpload\(safe\.substr\(prefix\.size\(\)\)\)/);
  assert.match(nativeApp, /posted to server/);
  assert.match(nativeApp, /native lifecycle:/);
  assert.match(nativeApp, /live deploy rejected:/);
  assert.match(nativeApp,
    /https:\/\/aesthetic\.computer\/api\/piece-log/);
  assert.match(nativeApp, /FlushClientErrorUploads\(\)/);
  assert.match(pieceLog,
    /if \(phase === "error"\)[\s\S]{0,240}\$setOnInsert/);
});

test("the error screen carries its whole dump in a scannable QR link", () => {
  globalThis.qrcode = qrcode;
  try {
    const { fight } = createFight(true, true);
    const error = new RangeError("invalid 3d triangle coordinates");
    error.stack = "RangeError: invalid 3d triangle coordinates\n" +
      "    at screenTriangle (live:1162:16)\n    at gamePaint (live:5028:3)";
    fight.captureClientError("paint", error);
    const { url, modules } = fight.clientErrorDumpState();
    assert.ok(url.startsWith("https://oskiewar.com/api/oskiewar-dump?d="));
    // A code the console can actually render large enough to scan off a TV.
    assert.ok(modules > 0 && modules <= 125, `QR is ${modules} modules`);
    const dump = decodeDump(url.slice(url.indexOf("?d=") + 3));
    assert.equal(dump.p, "paint");
    assert.equal(dump.n, "RangeError");
    assert.equal(dump.m, "invalid 3d triangle coordinates");
    assert.equal(dump.src.line, 1162);
    assert.match(dump.k, /screenTriangle/);
    assert.equal(dump.s.players.length, 2);
    assert.equal(dump.s.build, "v" +
      Number(/const buildVersion = (\d+)/.exec(source)[1]));
    assert.ok(dumpRows(dump).some(([label]) => label === "camera"));
    assert.doesNotThrow(() => fight.paint());
    assert.match(source, /errorTypeWrite\("scan to share this dump"/);
  } finally {
    delete globalThis.qrcode;
  }
});

test("house opponents wear one flat color, community handles do not", () => {
  const { fight } = createFight(false, false);
  // BOT, DUMMY and PPL are not people; per-glyph color is handle identity.
  for (const handle of ["BOT", "DUMMY", "PPL"]) {
    const roster = new RegExp(
      `handle: "${handle}", color: (\\[[^\\]]+\\]), colors: (\\[[^\\]]*\\])`);
    const match = roster.exec(source);
    assert.ok(match, `${handle} is not declared with a flat color`);
    assert.deepEqual(JSON.parse(match[2]), [], `${handle} still speckles`);
    const flat = JSON.parse(match[1]);
    const glyphs = [...handle].map((_, index) =>
      fight.glyphColor([], index, flat));
    assert.equal(new Set(glyphs.map(String)).size, 1,
      `${handle} drew ${new Set(glyphs.map(String)).size} colors`);
  }
  // A community handle keeps its per-glyph palette.
  assert.ok(fight.players[0].handleColors.length > 1);
});

test("a run of type shadows in one direction instead of flipping a letter", () => {
  const { fight } = createFight(false, false);
  // DUMMY is five letters dressed by three colors, and its second color sits
  // just over the old luminance threshold — the 'u' used to take the opposite
  // shadow from every other glyph and read as a mistake.
  const palette = [[105, 125, 150], [135, 155, 180], [105, 125, 150]];
  const fallback = [105, 125, 150];
  const glyphs = [..."dummy"].map((_, index) =>
    fight.glyphColor(palette, index, fallback));

  // The tail cycles the palette rather than dropping onto another source.
  assert.deepEqual(glyphs[3], palette[0]);
  assert.deepEqual(glyphs[4], palette[1]);
  assert.deepEqual(fight.glyphColor([], 2, fallback), fallback);
  assert.deepEqual(fight.glyphColor(null, 0, fallback), fallback);

  const luminance = ([r, g, b]) => r * .2126 + g * .7152 + b * .0722;
  const shadows = glyphs.map((color) => fight.runShadow(color));
  const sides = new Set(shadows.map((shadow) => luminance(shadow) > 128));
  assert.equal(sides.size, 1, "every glyph in a run shadows the same way");
  // The old per-glyph contrast did single that letter out.
  assert.equal(new Set(glyphs.map((color) =>
    luminance(fight.contrastShadow(color)) > 128)).size, 2);
  // Each shadow still carries a trace of its own glyph.
  assert.notDeepEqual(shadows[0], shadows[1]);
});

test("the state dump is icon-led and colored token by token", () => {
  const { fight } = createFight(true, true);
  fight.captureClientError("paint", new RangeError("invalid 3d triangle"));
  const rows = fight.stateDumpRows(fight.clientErrorDetailState().state);
  assert.deepEqual(rows.map((row) => row.icon),
    ["build", "mode", "camera", "player", "player", "ball"]);

  const ink = (token) => fight.dumpTokenInk(token).join(",");
  const handle = ink("@jeffrey");
  const number = ink("6779");
  const label = ink("camera");
  const unit = ink("vel");
  assert.equal(new Set([handle, number, label, unit]).size, 4,
    "each token class gets its own ink");
  assert.equal(ink("@OSKIE"), handle);
  assert.equal(ink("-87"), number);
  assert.equal(ink("2.255"), number);
  assert.equal(ink("pos"), unit);
  assert.notEqual(ink(","), number);

  // Tokens must sum back to the source line or colored text would drift.
  const line = rows.find((row) => row.icon === "player").text;
  assert.equal(fight.dumpTokens(line).join(""), line);
  assert.ok(fight.dumpTokens(line).includes("@JEFFREY"));
  assert.match(source, /writeDumpLine\(wrapped, 132, cursorY, 27\)/);
  assert.match(source, /drawDumpIcon\(row\.icon, 94/);
});

test("a crashed console counts down and restarts itself", () => {
  const { fight, tick, telemetryEvents } = createFight(true, true);
  fight.captureClientError("paint", new RangeError("invalid 3d triangle"));
  assert.equal(fight.errorRestartSeconds(), 16);
  tick(6000000);
  assert.equal(fight.errorRestartSeconds(), 10);
  assert.ok(fight.clientErrorState());
  assert.match(source, /drawErrorCountdown\(width\)/);
  assert.match(source, /String\(Math\.min\(99, remaining\)\)\.padStart\(2, "0"\)/);
  tick(9000000);
  assert.equal(fight.errorRestartSeconds(), 1);
  assert.ok(fight.clientErrorState(), "holds the screen for the full countdown");
  tick(1000000);
  assert.equal(fight.clientErrorState(), "", "restarts once the countdown lands");
  assert.equal(fight.clientErrorDetailState(), null);
  assert.equal(fight.shellState().mode, "MENU");
  assert.ok(telemetryEvents.some(([event, detail]) =>
    event === "SHELL" && detail === "error->restart"));
});

test("off-camera detached limbs are culled before native triangle submission", () => {
  const { fight, triangles } = createFight(false, false);
  const before = triangles.length;
  assert.doesNotThrow(() => fight.drawDetachedPart({
    x1: 1e9, y1: 1e9, z1: 0, x2: 1e9 + 100, y2: 1e9 + 100,
    z2: 0, width: 30, color: [255, 80, 90],
  }));
  assert.equal(triangles.length, before);
  assert.match(source, /Math\.abs\(first\.x\) \+ margin > 30000/);
});

test("round end card names the winner and the finishing action", () => {
  const { fight, tick } = createFight();
  fight.knockOut();
  tick();
  assert.deepEqual(fight.resultCardText(),
    { winner: "@jeffrey", action: "" });
  assert.doesNotMatch(source, /winner\.toLowerCase\(\) \+ " wins!"/);
  assert.doesNotMatch(source,
    /box\(viewCenterX\(\) - causeWidth \/ 2 - 36/);
});

test("every head knockout busts the head into digital blood trails", () => {
  const { fight, tick } = createFight();
  const loser = fight.players[1];
  fight.knockOut();
  assert.ok(loser.headBustedAt >= 0);
  tick(140000);
  assert.doesNotThrow(() => fight.paint());
  assert.match(source, /target\.headBustedAt = now/);
  assert.match(source, /function drawDigitalHeadBurst/);
  assert.match(source, /\[255, 48, 96\], \[176, 18, 54\]/);
  assert.match(source, /screenRect\(point\.x - pixel \/ 2, point\.y - pixel \/ 2/);
  assert.doesNotMatch(source, /filledCapsule\(start\.x, start\.y, end\.x/);
  assert.match(source, /drawDigitalHeadBurst\(player, world\.head, age\)/);
});

test("jump framing cannot reveal a contrasting clear-color flash", () => {
  assert.match(source, /const outside = sky/);
});

test("facing and opponent mode are visible in fighter faces", () => {
  assert.match(source, /const faceX = head\.x \+ direction \* r \* \.08/);
  assert.match(source, /The facing-side foot is visibly planted forward/);
  assert.match(source, /const inertDummy = player\.npc && !player\.bot/);
  assert.match(source, /player\.bot && player\.alive && !player\.blocking/);
});

test("instant replay is deprecated out of the match flow", () => {
  const { fight, pads, tick } = createFight();
  for (let frame = 0; frame < 220; frame++) tick(33334);
  assert.equal(fight.replayFrameCount(), 0);
  fight.players[0].score = 1;
  for (let frame = 0; frame < 680; frame++) tick(33334);
  assert.match(fight.roundState().roundResult, /WINS ROUND/);
  pads[0].down = ["Y"];
  tick();
  assert.equal(fight.instantReplayState().active, false);
  assert.match(source, /const INSTANT_REPLAY = false/);
  assert.match(source, /if \(!INSTANT_REPLAY\) return false/);
  assert.match(source, /if \(!roundViewer && INSTANT_REPLAY\)/);
});

test("a completed demo plays one modem receipt only after upload", async () => {
  const { fight, drums, tick } = createFight();
  fight.players[0].score = 1;
  for (let frame = 0; frame < 920; frame++) tick(33334);
  await new Promise((resolve) => setImmediate(resolve));
  assert.equal(drums.filter(([name]) => name === "modem").length, 1);
  assert.match(source, /upload\.then\(\(saved\) =>/);
  assert.match(source, /playDrum\("modem", \.72, 0\)/);
  assert.match(webShell, /name === "modem"/);
  assert.match(webShell, /if \(!response\.ok\) throw new Error/);
  assert.match(webShell, /return true/);
});

test("one completed round saves its replay and closes the loop at title", () => {
  const { fight, replays, tick } = createFight();
  fight.players[0].score = 1;
  for (let frame = 0; frame < 750; frame++) tick(40000);
  assert.equal(fight.players[0].roundWins, 1);
  assert.equal(fight.roundState().matchOver, false);
  assert.equal(fight.roundState().roundResult, "@JEFFREY WINS ROUND");
  tick(3000001);
  assert.equal(fight.shellState().mode, "MENU");
  assert.equal(fight.roundState().roundResult, "");
  assert.equal(replays.length, 1);
  const demos = replays.map(JSON.parse);
  const demo = demos.at(-1);
  assert.equal(demo.format, "ac.oskiedemo");
  assert.equal(demo.version, 1);
  assert.equal(demo.winner, "@JEFFREY");
  assert.match(demo.matchName, /^[a-z]{5,6}[0-9]{1,3}$/);
  assert.equal(demo.matchId, "ow-" + demo.matchName);
  assert.equal(demo.roundId, demo.matchId);
  assert.equal(demo.roundIndex, 0);
  assert.equal(demo.roundIds.length, 1);
  assert.equal(demo.previousRoundId, "");
  assert.ok(demos.every((entry) => entry.seriesId === demo.seriesId));
  assert.equal(new Set(demos.map((entry) => entry.roundId)).size, 1);
  assert.ok(demo.commands.length > 0);
  assert.ok(demo.checkpoints.length > 0);
});

// A shield that eats a hit is spent doing it. These four cover the trade the
// break is meant to buy: the hitter is opened for long enough to be answered,
// and the shielder is free to answer immediately even with block still held.
function shieldBreakFight(kind = "KICK") {
  const harness = createFight();
  harness.fight.setWind(0);
  const [attacker, shielder] = harness.fight.players;
  attacker.facing = 1;
  shielder.facing = -1;
  shielder.x = attacker.x + 70;
  harness.pads[1].down = ["X"];
  harness.tick();
  assert.equal(shielder.blocking, true, "X raises the shield");
  harness.fight.startAttack(kind);
  for (let step = 0; step < 12 && !shielder.shieldLocked; step++) harness.tick(16000);
  return { ...harness, attacker, shielder };
}

test("a shielded hit spends the shield and stuns the hitter", () => {
  const { attacker, shielder, now } = shieldBreakFight();
  assert.equal(shielder.shieldLocked, true, "the shield breaks on the hit it eats");
  assert.equal(shielder.blocking, false, "and drops even though B is still held");
  assert.ok(attacker.hitStunUntil > now(), "the hitter is left stunned");
  assert.equal(attacker.attackKind, "", "their swing is spent, not still live");
});

test("shield-break stun scales with what the shield ate", () => {
  const heavy = shieldBreakFight("BASH");
  const light = shieldBreakFight("WHIP");
  const heavyStun = heavy.attacker.hitStunUntil - heavy.now();
  const lightStun = light.attacker.hitStunUntil - light.now();
  assert.ok(heavyStun > lightStun,
    `BASH (${heavyStun}us) should open the hitter longer than WHIP (${lightStun}us)`);
  assert.ok(lightStun >= 100000, "even the lightest shielded hit is answerable");
});

test("a broken shield frees the shielder to swing without releasing block", () => {
  const { shielder, pads, tick } = shieldBreakFight();
  pads[1].down = ["X", "B"];
  tick();
  assert.equal(shielder.attackKind, "PUNCH",
    "the opening the break bought is spendable straight away");
});

test("a shielded bullet reflects from the shield surface normal", () => {
  const { fight, pads, tick } = createFight();
  fight.setWind(0);
  const shielder = fight.players[1];
  shielder.facing = -1;
  pads[1].down = ["X"];
  tick();
  // Enter near the upper-left surface so the circle normal contributes both
  // a horizontal return and a vertical glancing angle.
  fight.bullets.push({ x: shielder.x + shielder.facing * 30 - 150,
    y: shielder.y - 150,
    z: shielder.z, vx: 2600, vy: 0, owner: 0, life: 1 });
  tick(1);
  assert.equal(fight.bullets.length, 1, "the shot survives to travel back");
  assert.ok(fight.bullets[0].vx < 0, "reversed");
  assert.ok(fight.bullets[0].vy < 0,
    "an off-centre hit ricochets at an angle instead of using a fixed return");
  assert.equal(fight.bullets[0].owner, 1, "and now belongs to whoever shielded it");
  assert.equal(shielder.shieldLocked, true, "returning a shot costs the shield too");
});

test("shielding plants a runner but preserves crouch input", () => {
  assert.match(source, /player\.shieldVx = 0;/);
  assert.match(source,
    /vertical: Math\.min\(0, rawInput\.vertical\)/);
});

test("victory controls select directional attack and movement frames", () => {
  assert.match(source, /A: "KICK", B: "PUNCH"/);
  assert.match(source, /ArrowLeft: "DASH", ArrowRight: "DASH", ArrowUp: "JUMP"/);
  assert.match(source, /player\.resultReactionAt \|\| now/);
});

test("shield directions aim the eyes while guard suppresses movement", () => {
  assert.match(source, /const digitalX = \(pad\.down \|\| \[\]\)\.includes\("ArrowRight"\)/);
  assert.match(source, /player\.blocking\s*\? \{ horizontal: 0/);
});

test("held objects belong to one intact arm, including on a skateboard", () => {
  assert.match(source, /const itemHand = \(player\) => player\.itemArm/);
  assert.match(source, /target\.itemArm === part/);
  assert.match(source, /target\.carryArm === part/);
  assert.match(source,
    /player\.skateboard && player\.grounded && !armAttack && !heldItem\(player\)/);
});

test("debug stats are unboxed and HUD shadows remain dark", () => {
  assert.doesNotMatch(source, /screenStrokeRect\(x, y, width, height, 2, edge\)/);
  assert.match(source,
    /visualTheme\.light > \.5 \? \[30, 20, 50\] : \[6, 4, 16\]/);
});

test("automatic cameras remain orthographic and outside the arena", () => {
  assert.match(source, /width: framedWidth, perspective: 0/);
  assert.match(source, /Math\.abs\(worldNear\) \+ headGap/);
  assert.doesNotMatch(source, /perspective: \.36/);
  assert.ok(source.indexOf("const focus =") < source.indexOf("const shoulder ="),
    "death camera focus must exist before its stand-off position reads it");
});

test("double jumps leave a brief upward motion-line burst", () => {
  assert.match(source, /player\.doubleJumpLinesUntil = now \+ 280000/);
  assert.match(source, /function drawDoubleJumpMotion/);
  assert.match(source, /drawDoubleJumpMotion\(renderable\.item, t\)/);
});

test("the Oskiewar wordmark remains visible with start", () => {
  assert.match(source, /const title = "oskiewar";/);
  assert.match(source, /const prompt = "start";/);
});

test("title keeps QR in the top-right HUD, version after title, and pops beneath", () => {
  assert.doesNotMatch(source, /drawTitleQr|qrTarget/);
  // Still the top-right HUD corner — except under debug, where the code
  // steps below the safe-rect's yellow corner crop instead of covering it.
  assert.match(source,
    /const top = safe\.top \+ \(debugHitboxes \? 52 : 0\);\n  return \{ left: safe\.right - size, top/);
  assert.match(source, /const versionX = titleX \+ titleWidth \+ lockupGap/);
  assert.match(source, /drawDummyPopLine\(titleY, titleSize, transitionInk\)/);
  assert.doesNotMatch(source, /if \(!placement && shellMode === "MENU"\) return/);
});

test("the Replay Oven captures full-UI 60 fps high-quality masters", () => {
  assert.match(webShell, /replayOven/);
  assert.match(webShell, /reelHud/);
  assert.match(webShell, /reelFullUi/);
  assert.match(source, /const matchHud = !replayOven \|\| reelHud/);
  // The standard published lane explicitly requests the full match UI. The
  // minimal lane remains available for legacy review renders.
  assert.match(source, /const reelMinimal = replayOven && reelHud/);
  assert.match(source, /capabilities\(\)\.reelFullUi !== true/);
  assert.match(source, /function drawReelSectionProgress\(now, titleInk\)/);
  assert.match(source, /const labels = \["INTRO", "FIGHT", "OUTRO"\]/);
  assert.match(source, /drawReelSectionProgress\(run\.monotonicUs, titleInk\)/);
  assert.match(source, /function recordFightHit\(sourcePad, decisive\)/);
  assert.match(source, /for \(const mark of hitMarks\)/);
  assert.match(source, /const reached = roundElapsedUs \/ roundDurationUs >= mark\.at/);
  assert.match(source, /globalThis\.__oskiewarFightHitForecast/);
  assert.match(source, /collapseRenderCameraInterpolation\(\)/);
  assert.match(source, /const reelGroundCamera = \(\)/);
  assert.match(source, /const groundNear = reelGroundCamera\(\)/);
  assert.match(source,
    /if \(matchHud && !reelMinimal && shellMode === "GAME" && gameplayStarted\)/);
  assert.match(source, /if \(reelMinimal && roundResult && resultUiReady\)/);
  assert.match(source, /if \(!replayOven\) drawDebugPerformance/);
  assert.match(source,
    /if \(!replayOven && !selfPlay && renderFlags\.keys !== false\)\n\s*drawControlLegend/);
  assert.match(reelRenderer, /&reel-hud/);
  assert.match(reelRenderer, /&reel-full-ui/);
  assert.match(reelRenderer, /quality: 100/);
  assert.match(reelRenderer, /"-r", "60"/);
  assert.match(reelRenderer, /"-crf", "14"/);
  assert.match(replayOven, /hud: true/);
});

test("the cube keeps its tile size, flat floor, and thin walls", () => {
  assert.match(source, /const tileSize = 90/);
  assert.match(source, /const gridCols = 10/);
  assert.match(source, /const gridRows = 10/);
  assert.match(source, /const wallThickness = 40/);
  // The walls stand outside the lattice so tile edges land on clean
  // multiples of ninety, flush with the playable faces.
  assert.match(source, /const worldLeft = gridLeft - wallThickness/);
  assert.match(source,
    /const worldRight = gridLeft \+ gridWidth \+ wallThickness/);
  assert.match(source, /const ceilingY = floorY - gridHeight - wallThickness/);
  assert.match(source, /const terrainAmplitude = 0/);
  // The side skate ramps are gone: no quarter-pipe rise anywhere.
  assert.doesNotMatch(source, /transitionRadius/);
  assert.doesNotMatch(source, /halfPipeRise/);
});

// The grid is the map. Simulation stays continuous, but every piece of
// structure — spawns, weapons, lanes — is authored on tile centers, and the
// per-tile field is the surface tiled computation runs on.
test("the map is a 10x10 grid of 90-unit tiles and is authored on it", () => {
  const { fight } = createFight(false, false, "web");
  const stage = fight.stageGeometry();
  // Playable faces sit exactly one grid apart on both axes.
  assert.equal(stage.worldRight - stage.worldLeft, 10 * 90 + 2 * 40);
  assert.equal(stage.floorY - stage.ceilingY, 10 * 90 + 40);
  // The cube holds a plain jump and a double jump under its ceiling.
  const apex = 1760 ** 2 / (2 * 4800);
  assert.ok(apex * 2 + 180 < 10 * 90,
    `a double jump (${(apex * 2).toFixed(0)}) plus a fighter breaks the roof`);
  // Spawn marks and the pistol sit on tile centers: 45 past a tile edge.
  for (const x of [...fight.players.map((player) => player.spawnX),
    fight.gunPickups[0].x])
    assert.equal((x - 45) % 90, 0, `${x} is off the tile lattice`);
  // Both weapons are authored in tile coordinates in the source.
  assert.match(source, /x: tileCenterX\(9\), startsActive: true/);
  assert.match(source, /\{ x: tileCenterX\(0\), startsActive: true/);
  // The tile field exists, one number per tile, and the overlay draws it.
  assert.match(source, /const gridField = new Float32Array\(gridCols \* gridRows\)/);
  assert.match(source, /function drawGridOverlay/);
});

test("camera bounds only cull an immutable globally mapped arena", () => {
  assert.match(source,
    /const step = \(worldRight - worldLeft\) \/ terrainSamples/);
  assert.doesNotMatch(source,
    /const step = \(right - left\) \/ terrainSamples/);
  // The wall is one plain full-arena sheet now — the most immutable mapping
  // there is. @jeffrey: "can the level background be plain colored? not
  // stripey?" — so the checkered plaster columns are gone entirely.
  assert.match(source,
    /worldQuad\(\{ x: worldLeft, y: ceilingY, z: worldFar \},\n    \{ x: worldRight, y: ceilingY, z: worldFar \}/);
  assert.doesNotMatch(source, /const checker =/);
  assert.match(source,
    /const x = lerp\(worldLeft, worldRight, \(index \+ \.5\) \/ count\)/);
  assert.match(source, /terrainFloorAt\(worldLeft\) \+ 120/);
});

// @jeffrey: "can audio be mixed according to the player's position, like
// stereo mixed, for each player, if its not local multiplayer". The game
// publishes each fighter's screen pan every painted frame; the shell hands
// the pair to the sound bank, whose per-player pan map was sitting exported
// and uncalled. Couch multiplayer keeps one centered image on purpose.
test("solo fights pan each fighter's audio to where they stand", () => {
  const { fight } = createFight(false, false);
  try {
    fight.paint();
    const pans = globalThis.__oskiewarPlayerPans;
    assert.ok(Array.isArray(pans) && pans.length === 2);
    // Training seats the dummy opposite the fighter: distinct stereo seats.
    assert.ok(pans.every((pan) => pan >= -1 && pan <= 1));
    assert.notEqual(pans[0], pans[1]);
    // Two humans on one couch share a centered image.
    fight.startFight();
    fight.paint();
    assert.deepEqual(globalThis.__oskiewarPlayerPans, [0, 0]);
  } finally {
    delete globalThis.__oskiewarPlayerPans;
  }
  assert.match(webShell,
    /__oskiewarSfx\.setPlayerPan\(0, pans\[0\]\);\n            globalThis\.__oskiewarSfx\.setPlayerPan\(1, pans\[1\]\)/);
});

test("skateboard deck trucks and wheels scale together with camera zoom", () => {
  assert.match(source, /return Math\.max\(\.5, Math\.abs\(edge\.x - center\.x\)\)/);
  assert.doesNotMatch(source, /Math\.max\(8, radius \* \.22\)/);
  assert.doesNotMatch(source, /Math\.max\(5, radius \* \.14\)/);
  assert.doesNotMatch(source, /Math\.max\(3, radius \* \.09\)/);
  assert.doesNotMatch(source, /const reach = Math\.max\(28,/);
});

// The fighters ride one plane, so a board is only ever seen edge-on and the
// profile is what has to be right: two trucks under the deck with one wheel
// each. The trucks used to straddle the deck and put a wheel on either face,
// which from the only available angle read as wheels above the plank too.
test("the skateboard is drawn as a side profile with its wheels underneath", () => {
  const symbol = source.match(/function drawSkateboardSymbol[\s\S]*?\n}\n/)[0];
  assert.match(symbol,
    /drawSkateboardSymbol\(point, radius, rotation = 0, underside = 1\)/);
  assert.match(symbol, /const downX = -Math\.sin\(rotation\) \* underside/);
  assert.match(symbol, /const downY = Math\.cos\(rotation\) \* underside/);
  // One wheel and one hub per truck — not a wheel per face of the deck.
  assert.equal(symbol.match(/filledDisc\(/g).length, 2);
  assert.doesNotMatch(symbol, /for \(const side of \[-1, 1\]\)/);
  // Trucks and wheels are placed at positive `down`, i.e. beneath the deck.
  assert.match(symbol,
    /plank\(at\(direction \* \.44, \.07\), at\(direction \* \.44, \.15\)/);
  assert.match(symbol, /const hub = at\(direction \* \.44, \.23\)/);
  // Riding the right-hand wall flips which way the wheels hang.
  assert.match(source,
    /drawSkateboardSymbol\(board, reach, rotation,\n\s*player\.skateWallSide > 0 \? -1 : 1\)/);
});

test("skateboard momentum follows half-pipe tangents into wallrides", () => {
  assert.match(source, /function terrainTangentAt\(x, span = 12\)/);
  assert.match(source, /terrainSlope = clamp\(terrainTangentAt\(player\.x\)/);
  assert.match(source, /player\.skateWallSide = -1/);
  assert.match(source, /player\.skateWallSide = 1/);
  assert.match(source, /player\.vy = -Math\.max\(820, Math\.abs\(player\.skateVx\) \* \.82\)/);
  assert.match(source, /emitSignal\("skate-wallride"/);
  assert.match(source, /Math\.hypot\(boardEdge\.x - leftEdge\.x,/);
});

test("nation flags are optional HUD identity and replay metadata", () => {
  assert.match(webShell, /\/api\/oskiewar-country/);
  assert.match(source, /nations: players\.map/);
  assert.match(source, /function nationFlag\(country\)/);
  assert.match(source, /systemWrite\(flag/);
});

test("crouched shield aim latches the pose and keeps directional eyes open", () => {
  assert.match(source, /player\.shieldCrouched = rawInput\.vertical < 0/);
  assert.match(source, /vertical: player\.shieldCrouched \? -1 : 0/);
  assert.match(source, /player\.shieldAimX = rawInput\.horizontal/);
  assert.doesNotMatch(source, /player\.blocking \|\| blink/);
});

test("held-item shield aim previews the same eight-way body and muzzle vector", () => {
  assert.match(source, /player\.gunAimX = rawInput\.horizontal/);
  assert.match(source, /player\.gunAimY = -rawInput\.vertical/);
  // The lock is no longer the only thing that aims: a held vertical direction
  // tilts the hand on its own, so every aim site reads the pair.
  assert.match(source, /player\.itemAimLocked \|\| player\.gunAimLive/);
  assert.match(source, /if \(player\.itemAimLocked \|\|/);
  assert.match(source, /player\.gunAimX = rawInput\.horizontal \|\| player\.facing/);
});

test("a primary web click maps to use item after the title", () => {
  assert.match(webShell, /event\.button === 0/);
  assert.match(webShell, /touchPointers\.set\(event\.pointerId, \{ key: "Y" \}\)/);
});

test("Tab is the only PC keyboard debug binding", () => {
  assert.match(webShell, /\["Tab", \[0, "View"\]\]/);
  assert.doesNotMatch(webShell, /\["KeyQ", \[0, "View"\]\]/);
});

test("update readiness is canvas HUD beneath the clock, never DOM chrome", () => {
  assert.doesNotMatch(webShell, /id="update-ready"/);
  assert.match(webShell, /updateReady: updateAvailable/);
  assert.match(source, /const label = "update ready"/);
  assert.match(source, /const y = clock\.top \+ clock\.size \+ 12/);
});

test("a victory laugh opens the mouth and emits floating notes", () => {
  assert.match(source, /laughChord/);
  assert.match(source, /emitSignal\("laugh"/);
  assert.match(source, /const note = index % 2 \? "♫" : "♪"/);
  assert.match(source, /Math\.sin\(age \* 18\)/);
});

test("anonymous web players can log in without receiving a fake handle", () => {
  assert.match(webShell, /<button id="logout" type="button">log in<\/button>/);
  assert.match(webShell, /loginWithRedirect/);
  assert.match(webShell, /handle: "", colors:/);
  assert.match(source, /const anonymousFighter = \{ handle: ""/);
});

test("the persistent PC rail uses keyboard action caps", () => {
  assert.match(source, /A: "SPACE", B: "ENTER", X: "SHIFT", Y: "ALT"/);
  assert.match(source, /drawKeycap\(keyboardCap\[cap\] \|\| cap/);
});

test("result simulation keeps projectiles and explosions moving without combat", () => {
  assert.match(source, /updateBullets\(dt, now, false\)/);
  assert.match(source, /updateGrenades\(dt, now, false\)/);
});

// The ground pound is a trade: the crater scales with the fall, and the
// fighter gives up their body while remaining playable as a bouncing head.
test("a ground pound craters on landing and leaves its fighter head-only", () => {
  const { fight, pads, tick, signals } = createFight();
  const [pounder] = fight.players;
  const { floorY, platformLeft } = fight.stageGeometry();
  // Off the platform, so the drop is measured against the real floor rather
  // than being caught 240 units early by the ledge.
  pounder.x = platformLeft - 1500;
  pounder.y = floorY - 700;
  pounder.grounded = false;
  pounder.pounding = true;
  pounder.poundFrom = floorY - 700;
  pads[0].down = ["ArrowDown"];
  for (let frame = 0; frame < 90 && pounder.pounding; frame++) tick();
  const named = signals.map((signal) => signal[0]);
  assert.ok(named.includes("blast"), "the landing sets off a crater");
  assert.equal(named.includes("balled"), false, "the pounder is not killed");
  assert.equal(pounder.alive, true);
  assert.equal(fight.isHeadOnly(pounder), true,
    "the pounder's torso and limbs pay for the crater");
  assert.equal(fight.runnerWorldGeometry(pounder, 0).segments.length, 0);
  const blast = signals.find((signal) => signal[0] === "blast");
  assert.ok(blast[3] > 0.75,
    `a 700-unit fall of a 900 maximum buys most of the crater, got ${blast[3]}`);
});

test("a ground pound from a standing start is spent, not sold cheap", () => {
  const { fight, tap } = createFight();
  const [player] = fight.players;
  assert.equal(player.grounded, true);
  tap(0, "ArrowDown");
  tap(0, "ArrowDown");
  assert.equal(player.pounding, false, "there is no height to convert");
  assert.equal(player.alive, true, "so nothing is traded away");
});

// The wall trees are the only thing in a round that gives a body back.
test("a ripe wall tree returns a body and fifteen seconds", () => {
  const { fight, tick } = createFight();
  const [player] = fight.players;
  const tree = fight.bodyTrees[0];
  player.removedParts = ["left-arm", "right-leg"];
  assert.ok(player.removedParts.length > 0, "the fighter starts this broken");
  // Real frames, not one-second jumps: the sim clamps a long delta, so a
  // coarse tick would never actually spend eighteen seconds of growth.
  for (let frame = 0; frame < 1400 && tree.growth < 1; frame++) tick();
  assert.equal(tree.growth, 1, "the tree ripens inside the round");
  // Ripening takes eighteen of the round's thirty seconds, which leaves the
  // clock sitting right on the fifteen-second clamp. Spend a few more so the
  // bonus has somewhere to go and the assertion means something.
  for (let frame = 0; frame < 420; frame++) tick();
  const before = fight.roundState().roundElapsedUs;
  assert.ok(before > 15000000, "there is more than the bonus left to wind back");
  const fruit = fight.treeFruit(tree);
  player.x = fruit.x;
  player.y = fruit.y + 90;
  tick();
  assert.deepEqual(player.removedParts, [], "the fruit is a whole new body");
  // Give or take the frame the harvest landed on, which advances the clock
  // in the same tick that winds it back.
  const wound = before - fight.roundState().roundElapsedUs;
  assert.ok(Math.abs(wound - 15000000) < 20000,
    `fifteen seconds back onto the clock, got ${wound}us`);
  assert.equal(tree.growth, 0, "the tree starts over rather than being farmed");
});

// Replay slow-motion reads the footage rather than being told where the action
// is, so these check the curve it derives and the ramp it eases along.
test("a replay slows itself into a hit and comes back up after", () => {
  const { fight } = createFight();
  const quiet = { players: [{ alive: true, hit: false, blockFlash: 0,
    attackKind: "", attackUntilOffset: 0 }], balls: [] };
  const landing = { players: [{ alive: true, hit: true, blockFlash: 0,
    attackKind: "PUNCH", attackUntilOffset: 1000 }], balls: [] };
  const frames = [...Array(20).fill(quiet), landing, ...Array(20).fill(quiet)];
  const curve = fight.replayActionCurve(frames);
  assert.equal(curve.length, frames.length);
  assert.equal(curve[20], 1, "the landing frame is full action");
  assert.ok(curve[14] > 0, "the ramp starts before the hit lands");
  assert.equal(curve[0], 0, "and long-quiet stretches stay at full speed");
  assert.ok(curve[19] > curve[14], "approaching the hit slows further");
  assert.equal(curve[25], 0, "speed returns once the action passes");
});

test("replay speed eases rather than snapping to its target", () => {
  const { fight } = createFight();
  const frame = 1 / 30;
  let speed = fight.replayRampStep(1, 1, frame);
  assert.ok(speed < 1, "it starts slowing on the first frame of action");
  assert.ok(speed > .18, "but does not arrive in one frame");
  for (let step = 0; step < 60; step++) speed = fight.replayRampStep(speed, 1, frame);
  assert.ok(speed < .2, "and settles into slow motion");
  for (let step = 0; step < 60; step++) speed = fight.replayRampStep(speed, 0, frame);
  assert.ok(speed > .95, "then recovers to full speed once the action passes");
  assert.equal(fight.replayRampStep(1, 0, frame), 1, "quiet footage never slows");
});
