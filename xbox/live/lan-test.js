// Temporary two-device test, appended by the dev deploy tool only.
// No public matchmaking or persistent account identity is changed.
(function installLanTest(config) {
  if (!config || ![0, 1].includes(config.seat) ||
      !/^ow-[a-z]{4,7}[0-9]{1,3}$/.test(config.room) ||
      !Number.isFinite(config.expires) || Date.now() >= config.expires) return;
  globalThis.performance ||= { now: () => Date.now() };
  // QuickJS has no browser structuredClone. Preserve typed arrays, aliases,
  // cycles and non-finite numbers used by rollback (JSON would corrupt them).
  globalThis.structuredClone ||= function clone(value, seen = new Map()) {
    if (value === null || typeof value !== "object") return value;
    if (seen.has(value)) return seen.get(value);
    if (value instanceof ArrayBuffer) return value.slice(0);
    if (ArrayBuffer.isView(value)) return new value.constructor(value);
    const copy = Array.isArray(value) ? [] : value instanceof Set ? new Set()
      : value instanceof Map ? new Map() : {};
    seen.set(value, copy);
    if (value instanceof Set) for (const entry of value) copy.add(clone(entry, seen));
    else if (value instanceof Map) for (const [key, entry] of value)
      copy.set(clone(key, seen), clone(entry, seen));
    else for (const key of Object.keys(value)) copy[key] = clone(value[key], seen);
    return copy;
  };
  globalThis.__oskiewarHashTrace = true;
  const peerTraces = new Map();
  let halted = false, retryAt = 0, retryCount = 0;
  function recoverMismatch(detail) {
    if (halted) return;
    halted = true;
    retryAt = Date.now() + Math.min(30000, 3000 * 2 ** Math.min(4, retryCount++));
    globalThis.__oskiewarLanMismatch = {...detail, retryAt, retryCount};
    telemetry("CLIENT_ERROR", JSON.stringify({phase:"netplay", name:"StateMismatch",
      message:"Oskiewar state mismatch at frame " + detail.frame,
      state:{build:buildVersion, seat:config.seat, ...detail}, recovery:"automatic match reset"}));
  }
  const originalNoteHash = netNotePeerHash;
  netNotePeerHash = function(session, frame, hash) {
    const mine = session.hashes.get(frame);
    if (mine !== undefined && mine !== hash) {
      recoverMismatch({frame, mine:netHashTexts.get(frame), theirs:peerTraces.get(frame)});
      telemetry("NET_TEST_MISMATCH", JSON.stringify(globalThis.__oskiewarLanMismatch));
    }
    return originalNoteHash(session, frame, hash);
  };
  const originalSim = sim;
  const originalPaint = paint;
  const originalLeave = leave;
  let profileAt = Date.now(), simCost = 0, paintCost = 0, simCalls = 0, paintCalls = 0;
  let priorProfile = null;
  // Opt-in inclusive CPU timing. Disabled builds install no wrappers at all.
  const phaseEnabled = config.phaseProfile === true || globalThis.__oskiewarPhaseProfile === true;
  let phaseLane = "other", phaseStats = {};
  function profilePhase(name, fn) {
    return function () {
      const key = phaseLane + "." + name, started = performance.now();
      try { return fn.apply(this, arguments); }
      finally {
        const row = phaseStats[key] ||= { ms: 0, calls: 0 };
        row.ms += performance.now() - started; row.calls++;
      }
    };
  }
  if (phaseEnabled) {
    updatePlayer = profilePhase("player", updatePlayer);
    resolvePlayerStanding = profilePhase("standing", resolvePlayerStanding);
    resolveMelee = profilePhase("melee", resolveMelee);
    resolvePogoAttacks = profilePhase("pogo", resolvePogoAttacks);
    updateBall = profilePhase("ball", updateBall);
    updateBullets = profilePhase("bullets", updateBullets);
    updateGrenades = profilePhase("grenades", updateGrenades);
    updateDetachedParts = profilePhase("fragments", updateDetachedParts);
    updateBodyTrees = profilePhase("trees", updateBodyTrees);
    updateCameraDoll = profilePhase("camera", updateCameraDoll);
    captureRoundReplay = profilePhase("replay", captureRoundReplay);
    netSnapshot = profilePhase("snapshot", netSnapshot);
    netStateHash = profilePhase("hash", netStateHash);
    buildRunnerWorldGeometry = profilePhase("pose", buildRunnerWorldGeometry);
    sampleCombatBoxes = profilePhase("boxes", sampleCombatBoxes);
    drawRunner = profilePhase("runner", drawRunner);
    drawDebugHitboxes = profilePhase("debugBoxes", drawDebugHitboxes);
    drawFrameMeter = profilePhase("frameMeter", drawFrameMeter);
    drawGridOverlay = profilePhase("grid", drawGridOverlay);
    drawRoomSurfaces = profilePhase("room", drawRoomSurfaces);
    drawTerrainSurface = profilePhase("terrain", drawTerrainSurface);
    drawTerrainBackWall = profilePhase("backWall", drawTerrainBackWall);
    drawTerrainFrontWall = profilePhase("frontWall", drawTerrainFrontWall);
  }
  // A diagnostic transport limit, independent of the 60 Hz simulation. Each
  // input carries ten recent frames; hash and stage packets always go through.
  // This lets the LAN trial expose a native send FIFO that is falling behind.
  const inputInterval = Number.isFinite(config.inputSendHz) && config.inputSendHz >= 20 &&
    config.inputSendHz <= 60 ? 1000 / config.inputSendHz : 0;
  let inputSentAt = -Infinity;
  const send = packet => {
    if (packet.t === "i") {
      const now = Date.now();
      if (!packet.h && now - inputSentAt < inputInterval - .5) return false;
      inputSentAt = now;
    }
    if (packet.h) {
      const trace = netHashTexts.get(packet.h[0]);
      if (trace?.length < 5000) packet.trace = trace;
    }
    if (typeof globalThis.__oskiewarNetSend === "function")
      return globalThis.__oskiewarNetSend(config.room, packet);
    if (typeof oskiewarNetSend === "function")
      return oskiewarNetSend(config.room, JSON.stringify(packet));
    return false;
  };
  let stageId = "", stageSentAt = 0, stagePacket = null, stageStopFrame = config.curtain === true ? 0 : Infinity;
  function sendStageIfDue() {
    if (stagePacket && Date.now() >= stageSentAt) {
      send(stagePacket); stageSentAt = Date.now() + 250;
    }
  }
  globalThis.__oskiewarStageState = {curtain:config.curtain === true, performance:null, peerAck:null};
  function applyStage(packet) {
    const previous = globalThis.__oskiewarStageState;
    const suspended = packet.curtain === true || packet.performance?.active === true;
    const wasSuspended = previous.curtain || previous.performance?.active;
    if (suspended && !wasSuspended) stageStopFrame = packet.stopFrame;
    if (!suspended) {
      stageStopFrame = Infinity;
      if (wasSuspended && netSession) netSession.lastPacketAt = Date.now();
    }
    globalThis.__oskiewarStageState = {id:packet.id, curtain:packet.curtain === true,
      performance:packet.performance || null, receivedAt:Date.now(),
      peerAck:previous.peerAck, stopFrame:stageStopFrame};
  }
  let ready = false, helloAt = 0, peerRun = "", expired = false;
  let run = Date.now().toString(36) + Math.random().toString(36).slice(2, 8);
  const name = config.seat === 0 ? "XBOX" : "AC";
  // Device roles retain their transport responsibilities; player seats
  // place AC on the left and Xbox on the right on both displays.
  const playerSeat = config.seat === 0 ? 1 : 0;
  const hasAccounts = typeof accountState === "function" && typeof accountAction === "function";
  let menuOpen = false, menuSelection = 0, menuPrevious = [], menuSuppress = false;
  let publicAccount = {}, identitySentAt = 0, leaderboardAt = 0, resultKey = "";
  let pendingMatch = null, reportAttemptAt = 0, pairingRetryAt = 0;
  const reportedMatches = new Set();
  const deviceHandles = ["", ""];
  const deviceHandleColors = [[], []];
  globalThis.__oskiewarDeviceHandles = deviceHandles;
  globalThis.__oskiewarDeviceHandleColors = deviceHandleColors;
  const physicalGamepad = gamepad;
  // Menus are local UI. Send neutral input while open, without pausing the
  // simulation or ever placing Start/Menu into the rollback command stream.
  if (hasAccounts) gamepad = function(index = 0) {
    const pad = physicalGamepad(index);
    if (index !== 0 || !pad) return pad;
    return menuSuppress ? {...pad, down:[], leftX:0, leftY:0, rightX:0, rightY:0,
      leftTrigger:0, rightTrigger:0} : {...pad, down:(pad.down || []).filter(k => k !== "Menu")};
  };
  const cleanHandle = value => /^@?[a-z0-9_-]{1,64}$/i.test(String(value || ""))
    ? "@" + String(value).replace(/^@/, "").toLowerCase() : "";
  function updateDeviceMenu() {
    if (!hasAccounts) return;
    publicAccount = accountState() || {};
    globalThis.__oskiewarDeviceAccount = {status:publicAccount.status,handle:publicAccount.handle || ""};
    deviceHandles[playerSeat] = publicAccount.status === "signed-in" ? cleanHandle(publicAccount.handle) : "";
    for (let seat=0;seat<2;seat++) {
      const row=publicAccount.leaderboard?.players?.find(row=>cleanHandle(row.handle) === deviceHandles[seat]);
      deviceHandleColors[seat]=handlePalette(row?.colors,deviceHandles[seat]);
    }
    const physical = physicalGamepad(0) || {};
    const down = [...(physical.down || [])];
    // Some USB pads expose their D-pad as the left stick rather than buttons.
    if (Math.abs(physical.leftY || 0) > .55) down.push("Navigate");
    if (down.some(k => ["ArrowUp","ArrowDown","DPadUp","DPadDown","Up","Down"].includes(k))) down.push("Navigate");
    const pressed = key => down.includes(key) && !menuPrevious.includes(key);
    const wasOpen = menuOpen;
    if (pressed("Menu")) { menuOpen = !menuOpen; menuSelection = 0; }
    else if (menuOpen) {
      if (publicAccount.status === "signed-in" && pressed("Navigate")) menuSelection = 1 - menuSelection;
      if (pressed("B")) menuOpen = false;
      if (pressed("A")) {
        if (publicAccount.status !== "signed-in" || menuSelection === 0) menuOpen = false;
        else if (publicAccount.status === "signed-in") accountAction("logout");

      }
    }
    if ((menuOpen || roundResult) && ["signed-out","error"].includes(publicAccount.status) && Date.now() >= pairingRetryAt) {
      pairingRetryAt = Date.now() + 10000;
      accountAction("login");
    }
    menuPrevious = down.slice();
    menuSuppress = menuOpen || wasOpen || pressed("Menu");
    globalThis.__oskiewarDeviceMenu = {open:menuOpen, selection:menuSelection};
    if (Date.now() >= leaderboardAt || (roundResult && resultKey !== roundResult + roundOverAt)) {
      leaderboardAt = Date.now() + 30000;
      resultKey = roundResult ? roundResult + roundOverAt : "";
      accountAction("leaderboard", JSON.stringify(deviceHandles.map(h => h.replace(/^@/, ""))));
    }
  }
  function accountMaintenance() {
    if (!hasAccounts) return;
    // Identity rides a public metadata packet, never a simulation snapshot.
    if (netSession && Date.now() >= identitySentAt) {
      send({t:"identity", run, handle:deviceHandles[playerSeat]});
      identitySentAt = Date.now() + 1000;
    }
    if (!netSession || !matchOver || !roundResult) { pendingMatch = null; return; }
    const id = "ow-" + (netSession.deal.accountSeries || String(netSession.originUs)) + "-" + Math.round(roundOverAt);
    if (!pendingMatch || pendingMatch.matchId !== id) pendingMatch = {
      matchId:id, frame:netSession.frame, handles:deviceHandles.map(h=>h.replace(/^@/, "")),
      roundWins:players.map(p=>p.roundWins), winner:players.findIndex(p=>p.roundWins >= matchWins)
    };
    if (netSession.confirmed < pendingMatch.frame || reportedMatches.has(id) ||
        Date.now() < reportAttemptAt || typeof accountReport !== "function") return;
    const p = pendingMatch;
    if (!p.handles.every(Boolean) || p.handles[0] === p.handles[1] || p.winner < 0 ||
        p.handles[playerSeat] !== deviceHandles[playerSeat].replace(/^@/, "")) return;
    reportAttemptAt = Date.now() + 2000;
    if (accountReport(JSON.stringify({matchId:p.matchId, seat:playerSeat,
        handles:p.handles, roundWins:p.roundWins, winner:p.winner}))) reportedMatches.add(id);
  }
  let loginQrUrl = "", loginQr = null;
  function handlePalette(colors, handle) {
    return handle && Array.isArray(colors) && colors.length === handle.length &&
      colors.every(c=>c && [c.r,c.g,c.b].every(v=>Number.isFinite(v) && v>=0 && v<=255))
      ? colors.map(c=>[c.r,c.g,c.b]) : [];
  }
  function drawLoginQr(url, left, top, cell = 4) {
    if (!url || typeof qrcode !== "function") return 0;
    if (loginQrUrl !== url) { loginQrUrl = url; loginQr = qrcode(url, {errorCorrectLevel:1}); }
    const count = loginQr.getModuleCount(), size = (count + 8) * cell;
    screenRect(left, top, size, size, [255,255,255]);
    for (let row=0; row<count; row++) {
      let start=-1;
      for (let col=0; col<=count; col++) {
        const dark=col<count && loginQr.isDark(row,col);
        if (dark && start<0) start=col;
        if (!dark && start>=0) {
          screenRect(left+(start+4)*cell, top+(row+4)*cell, (col-start)*cell, cell, [0,0,0]); start=-1;
        }
      }
    }
    return size;
  }
  function drawDeviceUi() {
    if (!hasAccounts || performanceStageActive()) return;
    const oldDepth = triangleDepth; triangleDepth = -1.49;
    const top=Math.max(60,viewHeight/2-244), white=[238,235,247];
    if (menuOpen) {
      const center = viewCenterX();
      const shadowText = (label, y, size, color = white) => {
        const x = center - handleWidth(label, size) / 2;
        typeWrite(label, x + 2, y + 3, size, 4, 5, 12);
        typeWrite(label, x, y, size, ...color);
      };
      const handle = deviceHandles[playerSeat] || name.toLowerCase();
      const handleX = center - handleWidth(handle, 28) / 2;
      drawHandle(handle, handleX + 2, top + 25, 28, [], [4,5,12]);
      drawHandle(handle, handleX, top + 22, 28,
        deviceHandleColors[playerSeat], players[playerSeat].color);
      const signedIn = publicAccount.status === "signed-in";
      const labels = signedIn ? ["resume", "log out"] : [];
      labels.forEach((label, index) => {
        shadowText((menuSelection === index ? "> " : "") + label,
          top + 80 + index * 48, 28,
          menuSelection === index ? white : [160,169,188]);
      });
      if (publicAccount.status === "waiting" && publicAccount.pairUrl) {
        // The white quiet zone belongs to the QR; all surrounding text floats.
        if (loginQrUrl !== publicAccount.pairUrl && typeof qrcode === "function") {
          loginQrUrl = publicAccount.pairUrl;
          loginQr = qrcode(loginQrUrl, {errorCorrectLevel:1});
        }
        const size = loginQr ? (loginQr.getModuleCount() + 8) * 4 : 164;
        shadowText("scan with your phone", top + 76, 22);
        drawLoginQr(publicAccount.pairUrl, center - size / 2, top + 115, 4);
        shadowText(String(publicAccount.code || ""), top + 133 + size, 30);
      } else if (publicAccount.status === "creating")
        shadowText("creating sign-in code...", top + 185, 22);
      else if (publicAccount.status === "error")
        shadowText("sign-in reconnecting...", top + 185, 22, [245,157,146]);
      else if (signedIn)
        shadowText("your match wins count globally", top + 205, 22);
      shadowText(signedIn ? "d-pad select   A choose   B back" : "A / B / Menu: resume",
        top + 425, 20, [170,178,198]);
    } else if (roundResult) {
      const board=publicAccount.leaderboard;
      const rows=Array.isArray(board?.top)?board.top.slice(0,3):[];
      const anon=publicAccount.status !== "signed-in";
      const winner=roundWinner();
      const label=roundResult === "TIE" || !winner ? "TIE"
        : winner.pad === playerSeat ? "WIN" : "LOSE";
      const resultNow=netSession ? netSession.originUs+netSession.frame*NET_TICK_US : runtime().monotonicUs;
      const age=Math.max(0,(resultNow-roundOverAt)/1000000);
      const reveal=Math.max(0,Math.min(1,(age-.65)/.55));
      const heroSize=Math.max(72,Math.min(280,(viewHeight-430)*.85,
        (stageRight-stageLeft-64)/(label.length*.75)));
      const size=heroSize+(Math.min(100,heroSize)-heroSize)*reveal*reveal*(3-2*reveal);
      const width=[...label].reduce((total,glyph)=>total+
        (typeof systemWrite.measureGlyph === "function"
          ? systemWrite.measureGlyph(glyph,size) : size*.65),0);
      const color=label === "WIN" ? players[playerSeat].color
        : label === "LOSE" ? [255,132,157] : white;
      if (typeof drawPhotoRoundOutcome === "function")
        drawPhotoRoundOutcome(label,age,viewCenterX()-width/2,24,width,size);
      // A huge reveal settles above the action so each replay angle stays clear.
      systemWrite(label,viewCenterX()-width/2,24,size,...color);
      const textX=stageRight-460,y=viewHeight-268;
      screenRect(textX-10,y+18,444,36,[12,14,23]);
      typeWrite("global wins",textX,y+24,24,...white);
      rows.forEach((row,i)=>{
        screenRect(textX-10,y+53+i*26,444,25,[12,14,23]);
        const full=cleanHandle(row.handle), label=full.slice(0,23), prefix=(i+1)+"  ";
        typeWrite(prefix,textX,y+58+i*26,18,...white);
        drawHandle(label,textX+handleWidth(prefix,18),y+58+i*26,18,handlePalette(row.colors,full),white);
        typeWrite("   "+row.matchesWon+" W / "+row.matchesPlayed,
          textX+handleWidth(prefix+label,18),y+58+i*26,18,...white);
      });
      if (!rows.length) {
        screenRect(textX-10,y+54,444,30,[12,14,23]);
        typeWrite(publicAccount.leaderboardError ? "leaderboard unavailable" : board?.verification ? "no ranked matches yet" : "leaderboard loading...",textX,y+60,18,170,178,198);
      }
      const stats=Array.isArray(board?.players)?board.players:[];
      stats.slice(0,2).forEach((row,i)=> {
        screenRect(textX-10,y+145+i*24,444,24,[12,14,23]);
        const seat=Math.max(0,deviceHandles.findIndex(handle=>handle.replace(/^@/,"")===row.handle));
        const full=cleanHandle(row.handle), label=full.slice(0,23);
        drawHandle(label,textX,y+150+i*24,16,handlePalette(row.colors,full),players[seat].color);
        typeWrite("  "+row.matchesWon+" W / "+row.matchesLost+" L",
          textX+handleWidth(label,16),y+150+i*24,16,...players[seat].color);
      });
      const qrX=stageLeft+32,qrY=viewHeight-224;
      if (anon && publicAccount.pairUrl) {
        const pulse=.5+.5*platformMath.sin(Date.now()/650);
        const border=[130+pulse*80,100+pulse*50,200+pulse*50];
        screenRect(qrX-8,qrY-40,220,36,[12,14,23]);
        typeWrite("sign in",qrX,qrY-32,24,...white);
        // Pulse only the surround; every QR module stays still and readable.
        const count=loginQrUrl === publicAccount.pairUrl && loginQr ? loginQr.getModuleCount() : 33;
        const qrSize=(count+8)*4;
        screenRect(qrX-6,qrY-6,qrSize+12,qrSize+12,border);
        drawLoginQr(publicAccount.pairUrl,qrX,qrY,4);
        screenRect(qrX-8,qrY+qrSize+8,220,34,[12,14,23]);
        typeWrite(String(publicAccount.code || ""),qrX,qrY+qrSize+14,24,...white);
      } else if (anon) {
        screenRect(qrX-8,qrY,260,34,[12,14,23]);
        typeWrite("creating sign-in code...",qrX,qrY+8,18,...white);
      }
      const signed=deviceHandles.every(Boolean);
      screenRect(textX-10,y+220,444,30,[12,14,23]);
      typeWrite(String(signed ? (publicAccount.reportStatus || "both signed in: first to 5 counts") : "scan to record your wins").slice(0,48),textX,y+228,16,170,178,198);
    }
    triangleDepth = oldDepth;
  }

  globalThis.__oskiewarTheme = "light";
  // This room carries inputs only, so a spectator publish cannot switch the
  // native socket out from under the test. Test rounds are never uploaded.
  publishVersus = () => {};
  publishSpectator = () => {};
  publishSession = () => {};
  globalThis.saveReplay = () => {};
  sim = function () {
    updateDeviceMenu();
    if (expired) return originalSim();
    if (Date.now() >= config.expires) {
      netLeave("test-expired"); expired = true;
      globalThis.__oskiewarLanStatus = "expired";
      gameBoot(); return;
    }
    if (!ready) {
      ready = true;
      versusRoomName = config.room.slice(3);
      beginVersusLobby(runtime().monotonicUs);
      players[0].name = name;
    }
    if (halted && Date.now() >= retryAt) {
      netEnd("desync-retry"); halted = false; peerRun = ""; helloAt = 0;
      run = Date.now().toString(36) + "-retry" + retryCount;
    }
    if (!halted && netSession?.frame > 1800) retryCount = 0;
    const command = globalThis.__oskiewarStageCommand;
    if (config.seat === 1 && command?.id !== stageId && typeof command?.id === "string") {
      stageId = command.id;
      stagePacket = {t:"stage", id:stageId, curtain:command.curtain === true,
        performance:command.performance || null,
        stopFrame:netSession ? Math.max(netSession.frame, netSession.remoteSimFrame) + 120 : 0};
      applyStage(stagePacket);
      stageSentAt = 0;
    }
    // The native WebSocket currently has one outgoing slot. Before joining,
    // let a hello replace a progress update; during play, send control last
    // so a same-frame input cannot overwrite a curtain/resume command.
    if (!netSession) sendStageIfDue();
    const packets = typeof oskiewarNetPoll === "function" ? oskiewarNetPoll() : [];
    const inbox = globalThis.__oskiewarNetInbox;
    if (Array.isArray(inbox)) packets.push(...inbox.splice(0));
    for (const packet of packets) {
      if (!packet || typeof packet !== "object") continue;
      if (packet.t === "stage" && config.seat === 0 && typeof packet.id === "string" &&
          Number.isInteger(packet.stopFrame) && packet.stopFrame >= 0) {
        if (packet.id !== stageId) { stageId = packet.id; applyStage(packet); }
        send({t:"stageAck", id:packet.id});
        continue;
      }
      if (packet.t === "stageAck" && packet.id === stageId) {
        globalThis.__oskiewarStageState.peerAck = packet.id;
        continue;
      }
      if (packet.t === "identity") {
        deviceHandles[1-playerSeat] = cleanHandle(packet.handle);
        continue;
      }
      if (packet.h && typeof packet.trace === "string") {
        peerTraces.set(packet.h[0], packet.trace);
        if (peerTraces.size>32) peerTraces.delete(peerTraces.keys().next().value);
      }
      if (!halted && packet.t === "hello" && packet.v === 2 && config.seat === 0 &&
          packet.test === config.room && typeof packet.run === "string") {
        if (!netSession || peerRun !== packet.run) {
          netPeerHello = { name: "AC", colors: [[38, 82, 176]] };
          players[0].name = "XBOX";
          const deal = netMakeDeal();
          deal.test = config.room;
          deal.accountSeries = run;
          deal.fighters = [
            {name:"AC", rosterIndex:-1, color:[159,122,232], handleColors:[[159,122,232]]},
            {name:"XBOX", rosterIndex:-1, color:[120,200,72], handleColors:[[120,200,72]]},
          ];
          if (send(deal)) { peerRun = packet.run; netBegin(deal, playerSeat, send); }
        } else send(netSession.deal);
      } else if (!halted && packet.t === "start" && config.seat === 1 &&
          packet.test === config.room && packet.v === 2 &&
          Number.isInteger(packet.origin) && packet.delay === NET_INPUT_DELAY &&
          packet.fighters?.[0]?.name === "AC" && packet.fighters?.[1]?.name === "XBOX") {
        if (netSession?.originUs !== packet.origin) netBegin(packet, playerSeat, send);
      } else if (netSession) {
        if (packet.t === "desync" && packet.o === netSession.originUs)
          recoverMismatch({frame:packet.f, mine:netHashTexts.get(packet.f), peerReported:true});
        netInbox.push(packet);
      }
    }
    if (halted) {
      sendStageIfDue();
      globalThis.__oskiewarLanStatus = "state mismatch: restarting in " +
        Math.max(1, Math.ceil((retryAt - Date.now()) / 1000)) + "s";
      return;
    }
    if (!netSession && Date.now() >= helloAt) {
      helloAt = Date.now() + 1000;
      send({ t: "hello", v: 2, test: config.room, run, name });
    }
    const stage = globalThis.__oskiewarStageState;
    if ((stage.curtain || stage.performance?.active) &&
        (!netSession || netSession.frame >= stageStopFrame)) {
      if (netSession) netSession.lastPacketAt = Date.now();
      globalThis.__oskiewarLanStatus = stage.curtain ? "curtain" : "performance";
      sendStageIfDue();
      return;
    }
    globalThis.__oskiewarLanStatus = netSession ? "connected" : "waiting for " +
      (config.seat === 0 ? "ac" : "xbox");
    // A missing peer is a waiting room, not a dummy fight or a local match.
    if (netSession) {
      const started = Date.now();
      phaseLane = "sim";
      try { originalSim(); } finally { phaseLane = "other"; }
      simCost += Date.now() - started; simCalls++;
    }
    accountMaintenance();
    sendStageIfDue();
  };
  paint = function () {
    const started = Date.now();
    phaseLane = "paint";
    if (hasAccounts && (menuOpen || roundResult) && !performanceStageActive()) {
      // AC composites CPU text/lines after the GPU scene. Suppress that HUD
      // while painting the arena so it cannot overwrite the login QR later.
      const saved={write,systemWrite,box,line,debugHitboxes,renderFlags,
        globalFlags:globalThis.__oskiewarRenderFlags};
      try {
        write=systemWrite=box=line=()=>{};
        debugHitboxes=false;
        globalThis.__oskiewarRenderFlags={...renderFlags,hud:false,keys:false};
        originalPaint();
      } finally {
        ({write,systemWrite,box,line,debugHitboxes,renderFlags}=saved);
        globalThis.__oskiewarRenderFlags=saved.globalFlags;
      }
    } else originalPaint();
    drawDeviceUi();
    phaseLane = "other";
    paintCost += Date.now() - started; paintCalls++;
    if (Date.now() - profileAt >= 2000) {
      const now = Date.now(), elapsed = now - profileAt, session = netSession;
      const current = session ? { origin: session.originUs, frame: session.frame,
        snapshotMs: session.stats.snapshotMs, rolledFrames: session.stats.rolledFrames,
        waits: session.stats.waits, stalls: session.stats.stalls,
        mathHits: netMathMemoStats.hits, mathMisses: netMathMemoStats.misses } : null;
      const continuous = current && priorProfile?.origin === current.origin;
      telemetry("NET_PERF", JSON.stringify({ surface: name, build:buildVersion, theme:globalThis.__oskiewarGraphicsThemeStatus, windowMs: elapsed,
        paintFps: paintCalls * 1000 / elapsed,
        simMs: simCost / Math.max(1, simCalls), paintMs: paintCost / Math.max(1, paintCalls),
        gameFps: continuous ? (current.frame - priorProfile.frame) * 1000 / elapsed : null,
        snapshotMs: continuous ? current.snapshotMs - priorProfile.snapshotMs : null,
        rolledFrames: continuous ? current.rolledFrames - priorProfile.rolledFrames : null,
        mathHits: continuous ? current.mathHits - priorProfile.mathHits : null,
        mathMisses: continuous ? current.mathMisses - priorProfile.mathMisses : null,
        waits: continuous ? current.waits - priorProfile.waits : null,
        stalls: continuous ? current.stalls - priorProfile.stalls : null }));
      if (phaseEnabled) for (const lane of ["sim", "paint", "other"]) {
        const phases = {};
        for (const key of Object.keys(phaseStats)) if (key.startsWith(lane + ".")) {
          const row = phaseStats[key];
          phases[key.slice(lane.length + 1)] = [Math.round(row.ms * 10) / 10, row.calls];
        }
        if (Object.keys(phases).length) telemetry("NET_PHASES", JSON.stringify({
          surface: name, build: buildVersion, lane, windowMs: elapsed, phases }));
      }
      profileAt = now; priorProfile = current; phaseStats = {};
      simCost = 0; paintCost = 0; simCalls = 0; paintCalls = 0;
    }
    const stage = globalThis.__oskiewarStageState;
    if (!stage?.curtain && !stage?.performance?.active && !expired && !menuOpen && (halted || !netSession))
      typeWrite(globalThis.__oskiewarLanStatus || "connecting", 36, 330, 24, 25, 35, 55);
  };
  leave = function () { netLeave("test-left"); originalLeave(); };
})(globalThis.__oskiewarLanTest);
