import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const source = await readFile(new URL("../hello.js", import.meta.url), "utf8");
const webShell = await readFile(new URL("../mac-test.html", import.meta.url), "utf8");
const activeCursor = await readFile(new URL(
  "../../../system/public/aesthetic.computer/cursors/active.svg",
  import.meta.url), "utf8");
const uiSource = await readFile(new URL(
  "../../../system/public/aesthetic.computer/lib/ui.mjs",
  import.meta.url), "utf8");
const nativeApp = await readFile(new URL("../../native-bios/App.cpp", import.meta.url), "utf8");
const pieceLog = await readFile(new URL(
  "../../../system/netlify/functions/piece-log.mjs", import.meta.url), "utf8");
const limbPartsForTest = ["left-arm", "right-arm", "left-leg", "right-leg"];

function createFight(startImmediately = true, enterGame = true,
  platform = "xbox-uwp", roundBridge = null,
  viewport = { width: 1920, height: 1080 }, livePublisher = null,
  drumVoice = null) {
  let now = 0;
  const signals = [];
  const replays = [];
  const liveFrames = [];
  const analyticsEvents = [];
  const telemetryEvents = [];
  const drums = [];
  const triangles = [];
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
  const drawLine = (...values) => lines.push(values);
  const fight = new Function(
    "runtime", "gamepad", "capabilities", "telemetry", "gameSignal", "saveReplay", "publishLive", "analytics", "drum", "wipe", "box", "line", "triangle", "triangle3d", "write", "systemWrite", "gameView",
    `${source}\nreturn { boot, sim, paint, playDrum, captureClientError, drawDetachedPart, clientErrorState: () => clientError, clientErrorDetailState: () => clientErrorDetail, errorReportStatus, controlLocale, animatedTitleColor, players, ball, balls, bullets, grenades, gunPickups, grenadePickups, detachedParts, runnerWorldGeometry, fighterAnimationPhase, runnerDistanceToPoint, segmentSegmentClosest, meleeLimbContact, damagePart, isPogo, isHeadOnly, resultCardText, pacificTimeLabel, projectedBallRadius, deathCinematicState: () => deathCinematic ? { ...deathCinematic, age: deathCinematicAge() } : null, disableBall: () => { ballEnabled = false; for (const item of balls) item.active = false; }, enableBall: (index = 0) => { ballEnabled = true; const item = balls[index]; item.active = true; item.serveAt = 0; item.safeUntil = 0; item.safePlayers = 0; }, setWind: (value) => { windAcceleration = value; }, setDebugHitboxes: (value) => { debugHitboxes = Boolean(value); }, debugState: () => debugHitboxes, windState: () => ({ direction: windDirection, mph: windMph }), nextRound: () => resetRound(runtime().monotonicUs, false), knockOut: () => killPlayer(players[1], 0, runtime().monotonicUs, "KO"), startAttack: (kind) => startMelee(players[0], kind, runtime().monotonicUs), bootFirstBall: () => bootBall(ball, players[0], runtime().monotonicUs), wackBall: () => { players[0].attackKind = "KICK"; returnBall(ball, players[0], runtime().monotonicUs, false); }, shieldBall: () => returnBall(ball, players[0], runtime().monotonicUs, true), crossWackBall: (contact = 1) => crossWackBall(ball, players.map((player) => ({ player, contact })), runtime().monotonicUs), enterGame: () => enterGame(runtime().monotonicUs), shellState: () => ({ mode: shellMode }), startFight: () => { shellMode = "GAME"; selecting = false; startReplay(runtime().monotonicUs); resetRound(runtime().monotonicUs, true); }, selectionState: () => ({ selecting, ready: selectionReady.slice() }), cameraState: () => ({ cameraWidth, cameraCenter, cameraCenterY, cameraAspect, stageRight, stageTop, stageBottom, viewHeight, cameraContainFloor, doll: { width: cameraDoll.width, target: { ...cameraDoll.target }, position: { ...cameraDoll.position }, perspective: cameraDoll.perspective, roll: cameraDoll.roll } }), screenBounds: () => players.map((player) => runnerScreenBounds(player, runtime().monotonicUs / 1e6)), actionSafeRect, hudSafeRect, roundState: () => ({ roundResult, roundElapsedUs, matchOver }), viewerState: () => ({ active: Boolean(roundViewer), mode: roundViewerMode, status: roundViewerStatus, name: matchName }), instantReplayState: () => instantReplay ? { active: true, paused: instantReplay.paused, cursor: instantReplay.cursor, frames: instantReplay.frames.length } : { active: false }, replayFrameCount: () => roundReplayFrames.length };`
  )(
    () => ({ monotonicUs: now, unixMs: 1785870000000 + Math.floor(now / 1000),
      simCount: Math.floor(now / 16667), paintCount: 0,
      clientErrorReportStatus: hostErrorStatus }),
    (index = 0) => ({ ...pads[index], down: pads[index].down.slice() }),
    () => ({ platform, inputFamily: platform === "xbox-uwp" ? "xbox"
      : platform === "touch" ? "touch" : "keyboard" }),
    (event, detail) => telemetryEvents.push([event, detail]),
    (...signal) => signals.push(signal), (payload) => replays.push(payload),
    (matchId, payload) => livePublisher
      ? livePublisher(matchId, payload)
      : liveFrames.push([matchId, JSON.parse(payload)]),
    (action, properties) => analyticsEvents.push([action, properties]),
    drumVoice || ((name, velocity, pan) => drums.push([name, velocity, pan])), noOp, noOp, drawLine,
    drawTriangle, drawTriangle3d, noOp, noOp, () => viewport
  );
  globalThis.__oskiewarRoundBridge = roundBridge;
  fight.boot();
  globalThis.__oskiewarRoundBridge = null;
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
    telemetryEvents, drums, triangles, lines,
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

test("desktop space kicks and B punches without changing Xbox buttons", () => {
  assert.match(webShell, /\["Space", \[0, "A"\]\]/);
  assert.match(webShell, /\["KeyB", \[0, "X"\]\]/);
  assert.match(source, /button === "A"[\s\S]{0,140}startMelee\(player, "KICK"/);
  assert.match(source, /button === "X"[\s\S]{0,80}startMelee\(player, "PUNCH"/);
});

test("web touch labels follow the current combat mapping", () => {
  assert.match(webShell, /data-key="A" aria-label="kick or fire"/);
  assert.match(webShell, /data-key="X" aria-label="punch or toggle player two"/);
  assert.match(webShell, /data-key="B" aria-label="shield"/);
  assert.match(webShell, /data-key="Y" aria-label="grenade or replay"/);
});

test("web canvas forwards touch coordinates to the fighter selector", () => {
  assert.match(webShell, /globalThis\.__oskiewarTouch = \{ taps: selectTaps,/);
  assert.match(webShell, /selectTaps\.push\(point\)/);
  assert.match(webShell, /canvas\.width \/ bounds\.width/);
});

test("web selector uses the AC precise cursor and mouse hover bridge", () => {
  assert.match(webShell,
    /cursor: url\("\/aesthetic\.computer\/cursors\/precise\.svg"\) 12 12, auto/);
  assert.match(webShell, /canvas\.addEventListener\("pointermove"/);
  assert.match(webShell, /pointer: \{ x: 0, y: 0, active: false \}/);
  assert.match(webShell,
    /cursor: url\("\/aesthetic\.computer\/cursors\/active\.svg"\) 12 12, pointer/);
  assert.match(webShell, /syncSelectionCursor\(\);/);
  assert.doesNotMatch(activeCursor, /<circle|<ellipse/);
  assert.match(activeCursor, /M13 1v3 M13 20v3 M1 13h3 M20 13h3/);
  assert.match(uiSource, /event\?\.cursor\?\.\(hoveredButtons\.size \? "active" : "precise"\)/);
  assert.doesNotMatch(webShell, /if \(!touchEnabled\) return;/);
  assert.match(source, /function selectionHover\(/);
  assert.match(source, /hovered \? \.38/);
  assert.match(source, /readyHovered \? 29 : 27/);
});

test("web relayout observes live viewport element resizing", () => {
  assert.match(webShell,
    /new ResizeObserver\(resizeCanvas\)\.observe\(document\.documentElement\)/);
  assert.match(source, /syncGameView\(\);/);
});

test("update-ready panel offers a persisted auto-update checkbox", () => {
  assert.match(webShell, /id="auto-update" type="checkbox"/);
  assert.match(webShell, /oskiewar-auto-update/);
  assert.match(webShell, /if \(autoUpdate\.checked\) \{/);
});

test("web client errors post stateful reports and expose the server receipt", () => {
  assert.match(webShell, /async function postClientError\(message\)/);
  assert.match(webShell, /fetch\("\/api\/piece-log"/);
  assert.match(webShell, /phase: "error"/);
  assert.match(webShell, /clientErrorReportStatus = `posted to server/);
  assert.match(webShell, /frameRate: 60, clientErrorReportStatus/);
});

test("start cue uses cached ElevenLabs speech with echo and a rising beep", () => {
  assert.match(webShell, /Cached ElevenLabs male:3 rendition/);
  assert.match(webShell, /tts-cache\/f9af6e0d19a4896793f66cd146d9a72e4160cdab9b85ef35c15a707e52c2d2ba\.mp3/);
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
    throw new RangeError("invalid OSKIEWAR live payload");
  };
  const { fight, tick } = createFight(false, false, "xbox-uwp", null,
    { width: 1920, height: 1080 }, livePublisher);
  assert.doesNotThrow(() => fight.startFight());
  assert.doesNotThrow(() => tick());
  assert.equal(attempts, 1);
  assert.doesNotThrow(() => fight.nextRound());
  assert.equal(attempts, 1);
});

test("OSKIEWAR typography uses the packaged KidLisp Comic Relief face", () => {
  assert.match(source, /typeof comicWrite === "function"/);
  assert.match(source, /comicGlyphAdvance/);
  assert.match(source, /String\(text\)\.toLowerCase\(\)/);
  assert.match(source, /player\.handleColors\?\.map\(contrastShadow\)/);
  assert.match(source, /const hudTypeSize = 42/);
  assert.match(source, /const timerSize = timedRound \? hudTypeSize/);
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

test("title and fighter select use a dark background", () => {
  assert.match(source, /const menuArena = \[7, 10, 26\]/);
  assert.match(source, /drawTitleScreen\(t, menuInk, transitionAge\)/);
  assert.match(source, /drawSelectionScreen\(t, menuInk, menuPanel\)/);
});

test("title shows a live Pacific clock and version timestamp", () => {
  const { fight } = createFight(false, false);
  assert.match(fight.pacificTimeLabel(1785870000000),
    /^\d{1,2}:\d{2}(am|pm) P(D|S)T$/);
  assert.match(source, /const buildTimestamp = "\d{4}\.\d{2}\.\d{2}\.\d{4} PDT"/);
  assert.match(source, /typeWrite\(titleNow,[\s\S]*hudTypeSize, \.\.\.ink\)/);
  assert.match(source, /"build " \+ stamp\[2\]/);
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

test("debug view renders on the foreground triangle layer", () => {
  const { fight, triangles } = createFight(false, false, "web", null,
    { width: 1920, height: 1080 });
  fight.startFight();
  fight.setDebugHitboxes(false);
  fight.paint();
  const normalTriangleCount = triangles.length;
  fight.setDebugHitboxes(true);
  fight.paint();
  assert.ok(triangles.length - normalTriangleCount > normalTriangleCount);
  assert.match(source, /let debugHitboxes = true/);
  assert.match(source, /function drawRectOutline[\s\S]*?filledCapsule/);
  assert.match(source,
    /drawDebugHitboxes\(players\[1\], t\);\n  drawBallHitboxes\(\);\n  drawImpacts\(\);/);
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

test("close 16:9 action uses the tightest full-body containment frame", () => {
  assert.match(source,
    /const horizontalPadding = clamp\(180 \+ Math\.max\(0, separation - 700\) \* \.12/);
  assert.match(source, /compactLayout\(\) \? 560 : 640/);
  assert.match(source, /fighterContainmentRequiredWidth\([\s\S]{0,80}\* 1\.11/);
});

test("camera containment releases and zooms back in when fighters converge", () => {
  const { fight, tick } = createFight(false, false, "web");
  fight.startFight();
  tick(3000001);
  fight.players[0].x = 900;
  fight.players[1].x = 11100;
  for (let frame = 0; frame < 120; frame++) { tick(); fight.paint(); }
  const wide = fight.cameraState().doll.width;
  fight.players[0].x = 5700;
  fight.players[1].x = 6300;
  for (let frame = 0; frame < 180; frame++) { tick(); fight.paint(); }
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

test("intro camera keeps one smooth midpoint target through name handoffs", () => {
  const { fight, tick } = createFight(false, false);
  fight.startFight();
  let previous = fight.cameraState().doll.position.x;
  let largestStep = 0;
  for (let frame = 0; frame < 180; frame++) {
    tick();
    const state = fight.cameraState().doll;
    assert.ok(Math.abs(state.target.x - 6000) < .01);
    largestStep = Math.max(largestStep, Math.abs(state.position.x - previous));
    previous = state.position.x;
  }
  assert.ok(largestStep < 40, `intro camera stepped ${largestStep}px`);
  assert.match(source, /function drawFightIntro/);
  assert.match(source, /const startText = "start"/);
  assert.doesNotMatch(source, /const beginText = "begin"/);
  assert.doesNotMatch(source, /const fightText = "fight!"/);
  assert.match(source, /const andText = "and"/);
  assert.match(source, /function visibleHandle\(player\)/);
  assert.match(source, /return player\.name\.toLowerCase\(\)/);
  assert.doesNotMatch(source, /typeWrite\("v"/);
});

test("death camera closes on both fighters even when they are far apart", () => {
  const { fight, tick } = createFight(false, false, "web");
  fight.startFight();
  tick(3000001);
  fight.players[0].x = 1200;
  fight.players[1].x = 10800;
  fight.knockOut();
  for (let frame = 0; frame < 120; frame++) {
    tick();
    fight.paint();
  }
  const state = fight.cameraState();
  assert.ok(Math.abs(state.doll.target.x - 6000) < 4,
    `death target followed one fighter: ${state.doll.target.x}`);
  assert.ok(state.doll.width < 11500,
    `death shot did not close in: ${state.doll.width}`);
  const safe = fight.actionSafeRect();
  for (const bounds of fight.screenBounds()) {
    assert.ok(bounds.left >= safe.left - .5);
    assert.ok(bounds.right <= safe.right + .5);
  }
});

test("loss sequence freezes, enters killer cam, breaks the body, and returns", () => {
  const { fight, tick } = createFight();
  fight.knockOut();
  tick();
  assert.equal(fight.deathCinematicState().loserPad, 1);
  assert.equal(fight.deathCinematicState().winnerPad, 0);
  for (let frame = 0; frame < 28; frame++) tick();
  assert.ok(fight.cameraState().doll.perspective > .45);
  assert.match(source, /function drawBrokenRunner\(player, age\)/);
  assert.match(source, /function drawDeathFlash\(\)/);
  assert.match(source, /if \(age < \.86\)/);
  for (let frame = 0; frame < 80; frame++) tick();
  assert.ok(fight.deathCinematicState().age > 1.45);
});

test("wind flag lives on the platform without an MPH HUD label", () => {
  assert.match(source, /const poleBottom = platformY/);
  assert.match(source, /const poleX = \(platformLeft \+ platformRight\) \/ 2/);
  assert.match(source, /const poleZ = 480/);
  assert.match(source, /worldCapsule\(poleX, poleBottom/);
  assert.match(source, /function worldCapsule/);
  assert.doesNotMatch(source, /MPH/);
  assert.match(source, /const safe = actionSafeRect\(\)/);
  assert.doesNotMatch(source, /const span = cameraWidth \* 1\.18/);
});

test("round-seeded ambient dust uses tiny drifting points at varied depth", () => {
  const moteSource = source.match(/function seededWindValue[\s\S]*?\n}\n\nfunction drawSelectPortrait/)[0];
  assert.match(moteSource, /matchName \|\| seriesName/);
  assert.match(moteSource, /function drawAmbientMotes/);
  assert.match(moteSource, /const count = 12/);
  assert.match(moteSource, /filledDisc\(x, y, radius, ink\)/);
  assert.doesNotMatch(moteSource, /filledCapsule\(/);
});

test("debug HUD shows FPS without repeating oskiewar beside the round QR", () => {
  assert.match(source, /Math\.round\(displayFps \|\| 0\) \+ " fps"/);
  assert.match(source, /if \(debugHitboxes\) \{\n    const fpsLabel/);
  assert.doesNotMatch(source, /const gameLabel = "oskiewar"/);
});

test("debug off hides safe-zone boxes including frozen round impacts", () => {
  assert.match(source, /function drawSafeZones\(\) \{\n  if \(!debugHitboxes\) return;/);
  assert.match(source, /const impactDebug = !roundResult && now < impactHitboxesUntil/);
  assert.match(source, /const impactDebug = !roundResult &&\n    runtime\(\)\.monotonicUs < impactHitboxesUntil/);
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
  const qrSource = source.match(/function drawSpectatorQr[\s\S]*?\n}\n\nfunction drawRectOutline/)[0];
  assert.doesNotMatch(qrSource, /matchName|labelTop/);
  assert.match(source, /qrcode\("https:\/\/oskiewar\.com"/);
  assert.match(source,
    /drawTitleScreen\(t, menuInk, transitionAge\);\n    if \(transitionAge < 0\) drawSpectatorQr\(menuInk\);/);
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

test("start flashes yellow green lime before fading into pal select", () => {
  const { fight, pads, signals, drums, tick } = createFight(false, false);
  assert.equal(fight.shellState().mode, "MENU");
  assert.equal(fight.selectionState().selecting, true);
  assert.match(source, /Math\.floor\(t \* 2\.4\) % 2 === 0/);
  assert.match(source, /const prompt = "start"/);
  assert.match(source, /\[255, 238, 82\]/);
  pads[0].down = ["Y"];
  tick();
  assert.equal(fight.shellState().mode, "MENU");
  tick(700001);
  assert.equal(fight.shellState().mode, "GAME");
  assert.equal(fight.selectionState().selecting, true);
  assert.deepEqual(drums.map(([name]) => name), ["hat"]);
  assert.ok(signals.some(([event]) => event === "select"));
  assert.match(source,
    /const flashPalette = \[\[255, 226, 48\], \[70, 224, 92\], \[181, 255, 48\]\]/);
  assert.match(source, /if \(transitionAge >= 0\) return/);
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

test("character select offers the four AC fighters and waits for both pads", () => {
  const { fight, pads, tick } = createFight(false);
  pads[0].down = ["ArrowRight"];
  tick();
  pads[0].down = [];
  tick();
  assert.equal(fight.players[0].name, "@FIFI");
  pads[0].down = ["A"];
  tick();
  assert.equal(fight.selectionState().selecting, true);
  assert.equal(fight.players[1].name, "@OSKIE");
  assert.equal(fight.players[1].npc, false);
  pads[0].down = [];
  pads[1].down = ["A"];
  tick();
  assert.equal(fight.selectionState().selecting, false);
});

test("character select status labels use the shared comic typeface", () => {
  assert.equal((source.match(/typeWrite\(player\.bot \? "READY TO FIGHT"/g) || []).length, 2);
  assert.doesNotMatch(source, /write\(player\.bot \? "READY TO FIGHT"/);
});

test("portrait touch can choose both pals, toggle P2 mode, and ready", () => {
  const { fight, tick } = createFight(false, false, "touch", null,
    { width: 499, height: 1080 });
  fight.enterGame();
  globalThis.__oskiewarTouch = { taps: [] };
  const touch = (x, y) => {
    globalThis.__oskiewarTouch.taps.push({ x, y });
    tick();
  };
  try {
    touch(100, 242);
    assert.equal(fight.players[0].name, "@FIFI");
    touch(390, 592);
    assert.equal(fight.players[1].npc, true);
    touch(50, 540);
    touch(100, 336);
    assert.equal(fight.players[1].name, "@SAT");
    assert.equal(fight.players[1].npc, false);
    touch(160, 450);
    assert.equal(fight.selectionState().ready[0], true);
    touch(160, 590);
    assert.equal(fight.selectionState().selecting, false);
    globalThis.__oskiewarTouch.taps.push({ x: 100, y: 242 });
    tick();
    assert.equal(globalThis.__oskiewarTouch.taps.length, 0);
  } finally {
    delete globalThis.__oskiewarTouch;
  }
});

test("Menu returns to select while View or web Tab toggles debug geometry", () => {
  for (const pad of [0, 1]) {
    const { fight, tap } = createFight();
    assert.equal(fight.selectionState().selecting, false);
    tap(pad, "Menu");
    assert.equal(fight.selectionState().selecting, true);
  }
  const { fight, tap } = createFight();
  assert.equal(fight.debugState(), true);
  tap(1, "View");
  assert.equal(fight.debugState(), false);
  assert.equal(fight.selectionState().selecting, false);
  tap(0, "View");
  assert.equal(fight.debugState(), true);
  assert.match(webShell, /\["Tab", \[0, "View"\]\]/);
});

test("P1 X cycles P2 between controller, dummy, and attacking bot", () => {
  const { fight, pads, tick } = createFight(false);
  pads[0].down = ["X"];
  tick();
  assert.equal(fight.players[1].name, "DUMMY");
  assert.equal(fight.players[1].npc, true);
  assert.equal(fight.players[1].bot, false);
  assert.equal(fight.selectionState().ready[1], true);
  pads[0].down = [];
  tick();
  pads[0].down = ["X"];
  tick();
  assert.equal(fight.players[1].name, "BOT");
  assert.equal(fight.players[1].npc, true);
  assert.equal(fight.players[1].bot, true);
  assert.equal(fight.selectionState().ready[1], true);
  pads[0].down = [];
  tick();
  pads[0].down = ["X"];
  tick();
  assert.equal(fight.players[1].name, "@OSKIE");
  assert.equal(fight.players[1].npc, false);
  assert.equal(fight.players[1].bot, false);
  assert.equal(fight.selectionState().ready[1], false);
});

test("bot uses the player input and physics path to pursue and strike", () => {
  const { fight, tick } = createFight();
  const bot = fight.players[1];
  bot.npc = true;
  bot.bot = true;
  bot.name = "BOT";
  fight.players[0].x = 5000;
  bot.x = 6500;
  tick();
  assert.ok(bot.vx < 0);
  fight.players[0].x = 5850;
  bot.x = 6000;
  bot.botAttackAt = 0;
  tick();
  assert.ok(bot.attackKind === "PUNCH" || bot.attackKind === "KICK");
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
  tap(0, "X");
  assert.ok(signals.some(([event, player]) => event === "punch" && player === 0));
  tap(0, "ArrowRight");
  assert.ok(signals.some(([event, player, horizontal]) =>
    event === "move" && player === 0 && horizontal === 1));
});

test("gun drops grant ammo and A fires in the quantized aim direction", () => {
  const { fight, pads, signals, tick } = createFight();
  const player = fight.players[0];
  const pickup = fight.gunPickups[0];
  pickup.active = true;
  pickup.x = player.x;
  pickup.y = player.y - 70;
  tick();
  assert.equal(player.gunAmmo, pickup.amount);
  pads[0].down = ["ArrowUp", "ArrowRight", "A"];
  tick();
  assert.equal(player.gunAmmo, pickup.amount - 1);
  assert.equal(fight.bullets.length, 1);
  assert.ok(fight.bullets[0].vx > 0);
  assert.ok(fight.bullets[0].vy < 0);
  assert.ok(signals.some(([event, pad]) => event === "bullet" && pad === 0));
});

test("grenade drops grant ammo and Y throws an expanding grenade", () => {
  const { fight, pads, signals, tick } = createFight();
  const player = fight.players[0];
  const pickup = fight.grenadePickups[0];
  pickup.active = true;
  pickup.x = player.x;
  pickup.y = player.y - 70;
  tick();
  assert.equal(player.grenadeAmmo, pickup.amount);
  pads[0].down = ["Y"];
  tick();
  assert.equal(player.grenadeAmmo, pickup.amount - 1);
  assert.equal(fight.grenades.length, 1);
  assert.ok(signals.some(([event, pad]) => event === "grenade" && pad === 0));
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
  assert.match(source, /else if \(headshot\) killPlayer/);
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

  tap(0, "ArrowDown");
  tap(0, "ArrowDown");
  assert.equal(fight.players[0].lastButton, "DASH DOWN");
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

test("B shield blocks melee geometry", () => {
  const { fight, pads, signals, tick } = createFight();
  fight.players[1].npc = false;
  fight.players[0].x = 5000;
  fight.players[1].x = 5100;
  pads[0].down = ["A"];
  pads[1].down = ["B"];
  for (let frame = 0; frame < 5; frame++) tick(16667);
  assert.equal(fight.players[1].alive, true);
  assert.equal(fight.players[0].score, 0);
  assert.ok(fight.players[0].x < 5000,
    "a shielded strike should bounce the attacker backward");
  assert.ok(fight.players[0].knockVx < -1000,
    "shield recoil should survive the next movement update");
  assert.ok(signals.some(([event, player]) => event === "block" && player === 1));
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
  pads[1].down = ["B", "ArrowRight", "ArrowUp"];
  tick();
  assert.equal(player.blocking, true);
  assert.equal(player.inputX, 0);
  assert.equal(player.inputY, 0);
  assert.equal(player.facing, facing);
  assert.ok(player.vx < 0);
  assert.ok(player.vy > -380);
  pads[1].down = ["B", "ArrowLeft", "ArrowDown"];
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
  jumper.x = 5940;
  jumper.y = standing.y - 190;
  jumper.vy = 0;
  jumper.grounded = false;
  standing.x = 6060;
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
  pads[0].down = ["X"];
  for (let frame = 0; frame < 10; frame++) tick(16667);
  assert.equal(fight.players[0].alive, true);
  assert.equal(fight.players[1].alive, true);
  assert.ok(fight.players[1].hitStunUntil > 0);
});

test("melee collision uses the animated attacking limb capsules", () => {
  const { fight, pads, tick, now } = createFight();
  fight.players[0].x = 5940;
  fight.players[1].x = 6060;
  pads[0].down = ["X"];
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

test("a limbless torso becomes a pogo and a removed torso leaves a bouncing head", () => {
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
  pads[0].down = ["X"];
  tick();
  assert.equal(target.attackKind, "");
  damage("torso", 3);
  assert.equal(fight.isHeadOnly(target), true);
  assert.equal(fight.runnerWorldGeometry(target, now() / 1000000).segments.length, 0);
  target.y = 12000;
  target.vy = 20;
  tick();
  assert.ok(target.vy < 0, "the final head form should rebound from the floor");
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
  pads[0].down = ["X"];
  pads[1].down = ["X"];
  for (let frame = 0; frame < 5; frame++) tick(16667);
  assert.equal(fight.players[0].alive, true);
  assert.equal(fight.players[1].alive, true);
  assert.ok(fight.players[0].hitStunUntil > 0);
  assert.ok(fight.players[1].hitStunUntil > 0);
  assert.equal(fight.roundState().roundResult, "");
});

test("player lands on the center platform", () => {
  const { fight, tick } = createFight();
  const player = fight.players[0];
  player.x = 6000;
  player.y = 10200;
  player.vy = 300;
  player.grounded = false;
  for (let step = 0; step < 10 && !player.grounded; step++) tick(40000);
  assert.equal(player.y, 10400);
  assert.equal(player.grounded, true);
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
  fight.ball.x = 6000;
  fight.ball.y = 12000 - fight.ball.radius;
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
  fight.ball.x = 6000;
  fight.ball.y = 10400 - fight.ball.radius;
  fight.ball.vx = 0;
  fight.ball.vy = 0;
  tick(16667);
  assert.equal(fight.ball.vx, 0);
});

test("each round starts player balls plus an airborne off-platform beach ball", () => {
  const { fight } = createFight();
  assert.equal(fight.balls.length, 3);
  for (let index = 0; index < fight.players.length; index++) {
    const player = fight.players[index];
    const ball = fight.balls[index];
    assert.equal(ball.spawnOwner, index);
    assert.equal(ball.x, player.x + player.facing * 180);
    assert.equal(ball.y, 12000 - ball.radius);
    assert.equal(ball.vx, 0);
    assert.equal(ball.vy, 0);
  }
  const beach = fight.balls[2];
  assert.equal(beach.type, "beach");
  assert.equal(beach.spawnOwner, -1);
  assert.ok(beach.x > 7500);
  assert.equal(beach.y, 12000 - 920);
  assert.ok(beach.y < fight.balls[0].y);
  assert.ok(beach.y < fight.balls[1].y);
});

test("soccer, basketball, and beach balls have distinct physical properties", () => {
  const { fight } = createFight();
  const [soccer, basketball, beach] = fight.balls;
  assert.equal(soccer.type, "soccer");
  assert.equal(basketball.type, "basketball");
  assert.equal(beach.type, "beach");
  assert.ok(soccer.mass < basketball.mass);
  assert.ok(beach.mass < soccer.mass);
  assert.ok(beach.windFactor > soccer.windFactor);
  assert.ok(beach.gravityFactor < 1);
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
  assert.match(source, /ANIM " \+ animation\.state/);
  assert.match(source, /STEP " \+ animation\.step \+ "\/" \+ animation\.steps/);
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
  assert.match(source, /const force = \(attacker\.attackKind === "KICK" \? 1550 : 1200\) \* momentum/);
});

test("weather is calm while ambient dust remains visible", () => {
  const { fight, signals, tick } = createFight();
  for (let frame = 0; frame < 375; frame++) tick(40000);
  assert.equal(fight.windState().mph, 0);
  assert.equal(signals.filter(([event]) => event === "wind").length, 1);
  assert.match(source, /function randomWindMph\(\) \{\n  return 0/);
  assert.match(source, /nextWindChangeAt = Infinity/);
  assert.match(source, /drawAmbientMotes\(t, windInk\)/);
});

test("only one center-platform powerup appears at each ten-second interval", () => {
  const { fight, tick } = createFight();
  for (let step = 0; step < 251; step++) tick(50000);
  let active = [...fight.gunPickups, ...fight.grenadePickups]
    .filter((pickup) => pickup.active);
  assert.equal(active.length, 1);
  assert.equal(active[0].x, 6000);
  assert.equal(active[0].y, 10400 - 70);
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

test("running into a center-platform ball boots it", () => {
  const { fight, pads, tick } = createFight();
  const player = fight.players[0];
  fight.enableBall();
  player.x = 6000;
  player.y = 10400;
  player.grounded = true;
  fight.ball.x = player.x + 36;
  fight.ball.y = 10400 - fight.ball.radius;
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

test("A plus X grabs and carries a ball through a jump, then releases it", () => {
  const { fight, pads, tick } = createFight();
  const player = fight.players[0];
  fight.enableBall();
  fight.ball.x = player.x + 80;
  fight.ball.y = player.y - 90;
  fight.ball.z = player.z;
  pads[0].down = ["A", "X"];
  tick();
  assert.equal(player.heldBall, 0);
  assert.equal(fight.ball.heldBy, 0);
  assert.equal(player.attackKind, "");
  pads[0].down = ["A", "X", "ArrowUp"];
  tick();
  tick(90000);
  assert.ok(player.vy < 0);
  assert.equal(fight.ball.heldBy, 0);
  pads[0].down = [];
  tick();
  assert.equal(player.heldBall, -1);
  assert.equal(fight.ball.heldBy, -1);
  pads[0].down = ["X"];
  tick();
  assert.equal(player.attackKind, "PUNCH");
});

test("A plus X shows reaching and two-hand holding poses", () => {
  const { fight, pads, tick } = createFight();
  const player = fight.players[0];
  pads[0].down = ["A", "X"];
  tick();
  assert.equal(player.stance, "REACHING");
  assert.equal(player.lastButton, "REACHING");
  pads[0].down = [];
  tick();
  fight.enableBall();
  fight.ball.x = player.x + 100;
  fight.ball.y = player.y - 82;
  pads[0].down = ["A", "X"];
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
    ["RIGHT", "A"]);
  assert.match(source, /function drawCommandStream\(player, side\)/);
  assert.match(source,
    /const glyph = \{ LEFT: "<", RIGHT: ">", UP: "\^", DOWN: "v" \}/);
  assert.match(source, /idle - 150000/);
  assert.match(source, /nextLength > 8/);
  assert.match(source, /const training = !roundIsTimed\(\)/);
  assert.match(source, /safe\.top \+ \(debugHitboxes \? hudTypeSize \+ 12 : 4\)/);
  assert.match(source, /held: held\.includes\(buttonFor\[entry\.label\]\)/);
  assert.match(source, /heldPalette\[entry\.label\]/);
  assert.match(source, /const size = hudTypeSize/);
});

test("owned gun and grenade counts render above each bottom handle", () => {
  assert.match(source, /function drawHudInventory\(player, side\)/);
  assert.match(source, /items\.push\("gun " \+ player\.gunAmmo\)/);
  assert.match(source, /items\.push\("grenade " \+ player\.grenadeAmmo\)/);
  assert.match(source,
    /drawHudInventory\(players\[0\], 0\);\n    drawHudInventory\(players\[1\], 1\);/);
  const { fight } = createFight();
  fight.players[0].gunAmmo = 3;
  assert.ok(fight.runnerWorldGeometry(fight.players[0], 0).segments
    .some((segment) => segment.role === "item-forearm"));
  assert.doesNotMatch(source, /gunPips|grenadePips/);
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
  assert.match(source, /const radiusY = Math\.max\(3, radiusX/);
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
  assert.match(source, /const normal = normalize3\(cross3\(ab, ac\)\)/);
  assert.match(source, /typeof triangle3d === "function"/);
  assert.doesNotMatch(source, /worldLine\(worldLeft, floorY/);
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
  fight.ball.vx = 12000;
  fight.ball.vy = 0;
  tick(40000);
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
  pads[0].down = ["B"];
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

test("round clock can end in a tie and resets", () => {
  const { fight, tick } = createFight();
  for (let frame = 0; frame < 750; frame++) tick(40000);
  assert.equal(fight.roundState().roundResult, "TIE");
  tick(3000001);
  assert.equal(fight.roundState().roundResult, "");
  assert.equal(fight.players[0].score, 0);
  assert.equal(fight.players[1].score, 0);
});

test("dummy rounds are untimed and use an infinity clock", () => {
  const { fight, tick } = createFight();
  fight.players[1].npc = true;
  fight.players[1].bot = false;
  fight.players[1].name = "DUMMY";
  for (let frame = 0; frame < 800; frame++) tick(40000);
  assert.equal(fight.roundState().roundResult, "");
  assert.ok(fight.roundState().roundElapsedUs > 30000000);
  assert.match(source, /function roundIsTimed\(\)[\s\S]{0,100}!\(players\[1\]\.npc && !players\[1\]\.bot\)/);
  assert.match(source, /timedRound \? String\(remainingSeconds\)\.padStart\(2, "0"\) : "∞"/);
  assert.match(source,
    /timedRound \? hudTypeSize : Math\.round\(hudTypeSize \* 1\.65\)/);
});

test("dummy training has no QR, analytics, spectator feed, or replay upload", () => {
  const { fight, analyticsEvents, liveFrames, replays, tick } =
    createFight(false, false);
  fight.players[1].npc = true;
  fight.players[1].bot = false;
  fight.players[1].name = "DUMMY";
  fight.startFight();
  tick(3000001);
  tick(100000);
  assert.deepEqual(analyticsEvents, []);
  assert.deepEqual(liveFrames, []);
  assert.deepEqual(replays, []);
  assert.match(source,
    /function drawSpectatorQr\(ink\) \{\n  if \(shellMode === "GAME" && !roundIsTimed\(\)\) return/);
  assert.match(source,
    /if \(!roundIsTimed\(\)\) \{[\s\S]{0,180}replay = null/);
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
  error.stack = "RangeError: unknown drum\n    at playDrum (hello.js:724:9)";
  fight.captureClientError("sim", error);
  assert.match(fight.clientErrorState(), /sim: RangeError: unknown drum/);
  assert.equal(fight.clientErrorDetailState().source.line, 724);
  const payload = JSON.parse(telemetryEvents.find(([event]) =>
    event === "CLIENT_ERROR")[1]);
  assert.equal(payload.phase, "sim");
  assert.equal(payload.source.file, "hello.js");
  assert.equal(payload.state.players.length, 2);
  setHostErrorStatus("posted to server xbox-oskiewar-test");
  assert.equal(fight.errorReportStatus(), "posted to server xbox-oskiewar-test");
  assert.doesNotThrow(() => fight.paint());
  assert.match(source, /errorTypeWrite\("oskieware error"/);
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
    { winner: "@jeffrey wins!", action: "knocked out" });
  assert.match(source, /winner\.toLowerCase\(\) \+ " wins!"/);
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

test("round result offers an instant replay with pause, scrub, and exit", () => {
  const { fight, pads, tick } = createFight();
  for (let frame = 0; frame < 220; frame++) tick(33334);
  assert.ok(fight.replayFrameCount() > 100);
  fight.players[0].score = 1;
  for (let frame = 0; frame < 680; frame++) tick(33334);
  assert.match(fight.roundState().roundResult, /WINS ROUND/);
  pads[0].down = ["Y"];
  tick();
  assert.equal(fight.instantReplayState().active, true);
  pads[0].down = [];
  tick();
  pads[0].down = ["A"];
  tick();
  assert.equal(fight.instantReplayState().paused, true);
  const beforeScrub = fight.instantReplayState().cursor;
  pads[0].down = [];
  tick();
  pads[0].down = ["ArrowRight"];
  tick();
  assert.ok(fight.instantReplayState().cursor > beforeScrub);
  pads[0].down = [];
  tick();
  pads[0].down = ["B"];
  tick();
  assert.equal(fight.instantReplayState().active, false);
});

test("first to five round wins takes the match", () => {
  const { fight, replays, tick } = createFight();
  for (let round = 1; round <= 5; round++) {
    fight.players[0].score = 1;
    for (let frame = 0; frame < 750; frame++) tick(40000);
    assert.equal(fight.players[0].roundWins, round);
    assert.equal(fight.roundState().matchOver, round === 5);
    assert.equal(fight.roundState().roundResult,
      round === 5 ? "@JEFFREY WINS MATCH" : "@JEFFREY WINS ROUND");
    tick(round === 5 ? 5000001 : 3000001);
    if (round < 5) tick(3000001);
  }
  assert.equal(fight.players[0].roundWins, 0);
  assert.equal(fight.roundState().roundResult, "");
  assert.equal(replays.length, 5);
  const demos = replays.map(JSON.parse);
  const demo = demos.at(-1);
  assert.equal(demo.format, "ac.oskiedemo");
  assert.equal(demo.version, 1);
  assert.equal(demo.winner, "@JEFFREY");
  assert.match(demo.matchName, /^[a-z]{5,6}[0-9]{1,3}$/);
  assert.equal(demo.matchId, "ow-" + demo.matchName);
  assert.equal(demo.roundId, demo.matchId);
  assert.equal(demo.roundIndex, 4);
  assert.equal(demo.roundIds.length, 5);
  assert.equal(demo.previousRoundId, demos[3].roundId);
  assert.ok(demos.every((entry) => entry.seriesId === demo.seriesId));
  assert.ok(new Set(demos.map((entry) => entry.roundId)).size === 5);
  assert.ok(demo.commands.length > 0);
  assert.ok(demo.checkpoints.length > 0);
});
