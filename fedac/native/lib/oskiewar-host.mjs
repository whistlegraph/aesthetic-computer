// Oskiewar's Xbox host contract on AC Native's existing piece API.
// Keep the game in xbox/live/oskiewar.js; this file only adapts the host.
export const KEY_BUTTONS = {
  a: "ArrowLeft", arrowleft: "ArrowLeft", d: "ArrowRight", arrowright: "ArrowRight",
  w: "ArrowUp", arrowup: "ArrowUp", s: "ArrowDown", arrowdown: "ArrowDown",
  space: "A", f: "A", enter: "B", return: "B", g: "B",
  shift: "X", r: "X", alt: "Y", q: "Y", e: "Y",
  tab: "View", p: "Menu", backspace: "Menu", "-": "SpeedDown", "=": "SpeedUp", "+": "SpeedUp",
};

// Invert AC Native's fixed 70-degree perspective projection. The game has
// already projected its world: preserve those pixel coordinates and its z
// ordering while submitting one native Form per frame.
export function screenVertex(x, y, z, width, height) {
  const distance = Math.max(.02, 2 + z);
  const halfHeight = Math.tan(35 * Math.PI / 180) * distance;
  return [(x * 2 / width - 1) * halfHeight * width / height,
    (1 - y * 2 / height) * halfHeight, -distance, 1];
}

// Advances from src/font-matrix-chunky8.h. Keep layout on the same integer
// pixel grid as the native bitmap renderer, including per-glyph handle colors.
const matrixAdvances = [2, 2, 4, 6, 4, 4, 5, 2, 3, 3, 6, 4, 2, 4, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 4, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 5, 5, 4, 4, 4, 4, 6, 5, 5, 4, 5, 4, 4, 4, 4, 4, 6, 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 6, 4, 4, 4, 4, 4, 4, 4, 4, 4, 6, 4, 4, 4, 4, 2, 4, 4];
export function pixelGlyph(character, size, screenScale) {
  const code = character.codePointAt(0);
  const unicode = code > 126;
  const font = unicode ? "unifont" : size * screenScale >= 16 ? "matrix" : "font_1";
  const height = unicode ? 16 : font === "matrix" ? 8 : 10;
  const zoom = Math.max(1, Math.floor(size * screenScale / height));
  const advance = unicode ? 8 : font === "matrix"
    ? matrixAdvances[Math.max(0, Math.min(94, code - 32))] : 6;
  return { font, zoom, advance: advance * zoom, height: height * zoom };
}

// Xbox App.cpp PlayDrum is the canonical Oskiewar instrument bank. Render
// these short clips once, not six oscillators per hit on the audio thread.
// Dedicated replay playback leaves microphone/sample and surround routing alone.
export const OSKIEWAR_DRUM_LAYERS = {
  "kick": [
    [
      "noise",
      2500.0,
      0.0025,
      0.5,
      0.0002,
      0.0022
    ],
    [
      "sine",
      200.0,
      0.012,
      1.1,
      0.0005,
      0.011
    ],
    [
      "sine",
      150.0,
      0.045,
      1.3,
      0.001,
      0.044
    ],
    [
      "sine",
      90.0,
      0.08,
      0.85,
      0.002,
      0.078
    ],
    [
      "sine",
      55.0,
      0.35,
      1.0,
      0.003,
      0.345
    ]
  ],
  "snare": [
    [
      "noise",
      3500.0,
      0.004,
      0.95,
      0.0001,
      0.004
    ],
    [
      "sine",
      238.0,
      0.03,
      0.35,
      0.0003,
      0.029
    ],
    [
      "sine",
      476.0,
      0.03,
      0.28,
      0.0003,
      0.029
    ],
    [
      "noise",
      3500.0,
      0.11,
      0.85,
      0.0005,
      0.108
    ],
    [
      "noise",
      1800.0,
      0.07,
      0.38,
      0.0008,
      0.068
    ],
    [
      "triangle",
      180.0,
      0.025,
      0.22,
      0.001,
      0.024
    ]
  ],
  "clap": [
    [
      "noise",
      1000.0,
      0.025,
      0.9,
      0.005,
      0.02
    ],
    [
      "noise",
      1100.0,
      0.035,
      0.95,
      0.015,
      0.02
    ],
    [
      "noise",
      900.0,
      0.045,
      0.85,
      0.025,
      0.02
    ],
    [
      "noise",
      3000.0,
      0.008,
      0.55,
      0.001,
      0.007
    ],
    [
      "noise",
      1000.0,
      0.14,
      0.85,
      0.045,
      0.095
    ]
  ],
  "hat": [
    [
      "square",
      800.0,
      0.008,
      0.18,
      0.0005,
      0.0075
    ],
    [
      "square",
      540.0,
      0.008,
      0.18,
      0.0005,
      0.0075
    ],
    [
      "square",
      522.7,
      0.008,
      0.18,
      0.0005,
      0.0075
    ],
    [
      "square",
      369.6,
      0.008,
      0.18,
      0.0005,
      0.0075
    ],
    [
      "noise",
      8000.0,
      0.04,
      0.38,
      0.0005,
      0.038
    ]
  ],
  "bell": [
    [
      "sine",
      880.0,
      0.34,
      0.72,
      0.001,
      0.338
    ],
    [
      "sine",
      1320.0,
      0.27,
      0.38,
      0.001,
      0.268
    ],
    [
      "triangle",
      1760.0,
      0.18,
      0.2,
      0.001,
      0.178
    ]
  ],
  "whoosh": [
    [
      "noise",
      1450.0,
      0.34,
      0.68,
      0.018,
      0.32
    ],
    [
      "noise",
      4200.0,
      0.18,
      0.28,
      0.008,
      0.17
    ],
    [
      "triangle",
      110.0,
      0.28,
      0.24,
      0.012,
      0.26
    ]
  ],
  "block": [
    [
      "noise",
      5000.0,
      0.002,
      0.35,
      0.0001,
      0.0018
    ],
    [
      "triangle",
      2500.0,
      0.05,
      0.52,
      0.0003,
      0.048
    ],
    [
      "triangle",
      1250.0,
      0.05,
      0.18,
      0.0005,
      0.048
    ]
  ]
};
const oskiewarSoundRate = 48000;
const oskiewarDrumClips = new Map();
export function renderOskiewarDrum(name, rate = oskiewarSoundRate, seed = 0x6f736b69) {
  const layers = OSKIEWAR_DRUM_LAYERS[name] || OSKIEWAR_DRUM_LAYERS.block;
  const frames = Math.floor(rate * Math.max(...layers.map(layer => layer[2])));
  const mixed = new Float64Array(frames);
  const tau = 2 * Math.PI;
  seed >>>= 0;
  for (const [wave, frequency, duration, volume, attack, decay] of layers) {
    let phase = 0, filteredNoise = 0;
    const alpha = 1 - Math.exp(-tau * Math.min(frequency, rate * .45) / rate);
    const count = Math.floor(rate * duration);
    for (let i = 0; i < count && i < frames; i++) {
      const time = i / rate;
      let envelope = attack > 0 && time < attack ? time / attack : 1;
      const decayStart = Math.max(0, duration - decay);
      if (decay > 0 && time > decayStart) envelope *= 1 - (time - decayStart) / decay;
      let sample;
      if (wave === "noise") {
        seed ^= seed << 13; seed ^= seed >>> 17; seed ^= seed << 5; seed >>>= 0;
        const noise = seed / 4294967295 * 2 - 1;
        filteredNoise += alpha * (noise - filteredNoise);
        sample = filteredNoise;
      } else {
        phase += frequency / rate; phase -= Math.floor(phase);
        sample = wave === "sine" ? Math.sin(tau * phase)
          : wave === "square" ? phase < .5 ? 1 : -1
          : 1 - 4 * Math.abs(phase - .5);
      }
      mixed[i] += sample * envelope * volume;
    }
  }
  return Float32Array.from(mixed, sample => sample * .28);
}

export function createOskiewarSound(getSound) {
  let currentReplay = null;
  let voices = [];
  const clamp = (value, low, high) => Math.max(low, Math.min(high, value));
  const replayAvailable = () => typeof getSound()?.replay?.loadData === "function" &&
    typeof getSound()?.replay?.play === "function";
  // Initialization happens before play. Only small native buffer copies occur
  // on hits; clip synthesis and its trigonometry never run in a gameplay frame.
  if (replayAvailable()) for (const name of Object.keys(OSKIEWAR_DRUM_LAYERS)) {
    if (!oskiewarDrumClips.has(name)) oskiewarDrumClips.set(name, renderOskiewarDrum(name));
  }
  function stop() {
    const sound = getSound();
    if (currentReplay) sound?.replay?.kill?.(currentReplay, 0);
    currentReplay = null;
    for (const voice of voices) sound?.kill?.(voice, 0);
    voices = [];
  }
  return {
    synth(tone, duration = .05) {
      stop();
      const length = clamp(Number(duration) || .05, .005, 2);
      // Xbox's JS synth entry accepts frequency/duration only and always uses
      // a .25-volume sine with a linear decay. Native's centered oscillator
      // sends half its volume to each channel, so compensate that split here.
      const voice = getSound()?.synth?.({type:"sine", tone:clamp(Number(tone) || 440,20,5000),
        duration:length, volume:.5, attack:0, decay:length, pan:0});
      if (voice !== undefined && voice !== null) voices.push(voice);
    },
    drum(name, velocity = 1) {
      stop();
      const key = Object.prototype.hasOwnProperty.call(OSKIEWAR_DRUM_LAYERS, name) ? name : "block";
      const hit = clamp(Number.isFinite(velocity) ? velocity : 1,.1,1.5);
      const sound = getSound();
      if (replayAvailable()) {
        const clip = oskiewarDrumClips.get(key);
        if (clip && sound.replay.loadData(clip,oskiewarSoundRate) !== false) {
          // Xbox's effects voice is mono and replaces the previous event.
          currentReplay = sound.replay.play({tone:440,base:440,volume:hit,pan:0,loop:false});
          return;
        }
      }
      // Older hosts retain the same layers/envelopes, with at most six voices.
      // Their triangle/noise filters are approximate; current AC uses PCM.
      for (const [type,tone,duration,volume,attack,decay] of OSKIEWAR_DRUM_LAYERS[key]) {
        const voice = sound?.synth?.({type,tone,duration,volume:volume*hit*.56,attack,decay,pan:0});
        if (voice !== undefined && voice !== null) voices.push(voice);
      }
    },
    stop,
  };
}

// Browser-only game identity; the USB file is outside HTTP-served /pieces and
// never changes the OS's developer credentials. Only public fields leave here.
export function createOskiewarAccount(getSystem, clock = () => Date.now()) {
  const accountPath = "/mnt/.oskiewar-account.json";
  let state = {status:"signed-out",handle:"",code:"",pairUrl:"",expiresAt:0,error:"",
    reportStatus:"",leaderboard:{},leaderboardError:""};
  let secret="",session=null,pending=null,nextPoll=0,nextRead=0;
  let requestNumber=0,ownsPty=false,pendingReport=null,pendingBoard=null,lastBoard=-Infinity;
  let reportRetries=0,nextReport=0;
  const prefix="/tmp/oskiewar-pair-"+Math.trunc(clock()).toString(36);
  const validHandle=value=>typeof value==="string"&&/^[A-Za-z0-9._-]{1,64}$/.test(value);
  const validPair=value=>value&&typeof value.code==="string"&&
    /^[ABCDEFGHJKLMNPQRSTUVWXYZ23456789]{6}$/.test(value.code)&&
    typeof value.pollSecret==="string"&&/^[A-Za-z0-9_-]{32,128}$/.test(value.pollSecret)&&
    Number.isFinite(value.expiresAt)&&value.expiresAt>clock()&&value.expiresAt<=clock()+600000;
  function saveAccount(value) {
    try {return getSystem().writeFile(accountPath,JSON.stringify(value),true)!==false;}
    catch(_){return false;}
  }
  try {
    const saved=JSON.parse(getSystem().readFile(accountPath)||"null");
    if(validHandle(saved?.handle)&&typeof saved?.session?.accessToken==="string"&&saved.session.accessToken) {
      session=saved.session;state.handle=saved.handle;state.status="signed-in";
    } else if(validPair(saved?.pending)) {
      secret=saved.pending.pollSecret;state.code=saved.pending.code;
      state.pairUrl="https://aesthetic.computer/api/device-pair-login?code="+state.code;
      state.expiresAt=saved.pending.expiresAt;state.status="waiting";nextPoll=clock()+1500;
    } else if(saved?.pending&&!saveAccount({})) {
      state.status="error";state.error="Could not clear expired sign-in";
    }
  } catch(_) {}
  function erase(job) {
    if(!job)return;
    for(const suffix of [".cfg",".body",".json",".next",".rc",".http"])
      getSystem().writeFile(job.base+suffix,"",false);
  }
  function cancelRequest() {
    if(pending&&ownsPty)getSystem().pty2?.kill?.();
    erase(pending);pending=null;
  }
  function clearPair() {secret="";state.code=state.pairUrl="";state.expiresAt=0;}
  function fail(message) {
    cancelRequest();clearPair();state.status="error";
    state.error=!session&&!saveAccount({})?"Could not clear sign-in":message;
  }
  function request(kind,url,body=null,bearer="") {
    const system=getSystem();
    if(!system.pty2?.spawn||(system.pty2.active&&!ownsPty))return false;
    const base=prefix+"-"+(++requestNumber);
    let config='url = '+JSON.stringify(url)+'\n';
    if(body!==null) {
      if(system.writeFile(base+".body",JSON.stringify(body),false)===false)return false;
      config+='request = "POST"\nheader = "Content-Type: application/json"\ndata-binary = "@'+base+'.body"\n';
    }
    if(bearer) {
      if(/[\r\n]/.test(bearer))return false;
      config+='header = '+JSON.stringify('Authorization: Bearer '+bearer)+'\n';
    }
    config+='silent\nwrite-out = "%{http_code}"\nconnect-timeout = 5\nmax-time = 12\nmax-filesize = 65536\n'+
      'cacert = "/etc/pki/tls/certs/ca-bundle.crt"\noutput = "'+base+'.next"\n';
    if(system.writeFile(base+".cfg",config,false)===false)return false;
    // No secret enters argv, terminal output, shared fetchResult or native logs.
    const command=`umask 077; chmod 600 ${base}.cfg ${base}.body 2>/dev/null; `+
      `curl --config ${base}.cfg >${base}.http 2>/dev/null; result=$?; `+
      `if [ "$result" = 0 ]; then mv ${base}.next ${base}.json; fi; `+
      `rm -f ${base}.cfg ${base}.body ${base}.next; printf '%s' "$result" > ${base}.rc`;
    if(!system.pty2.spawn("/bin/sh",["-c",command],80,24,{raw:true})) {
      erase({base});return false;
    }
    ownsPty=true;pending={kind,base,body,started:clock()};nextRead=0;return true;
  }
  function receive(job,body,ok,now,httpStatus=0) {
    if(job.kind==="board") {
      state.leaderboardError=ok?"":"Leaderboard unavailable";
      if(ok)state.leaderboard=body;
      return;
    }
    if(job.kind==="report") {
      if(!ok&&httpStatus>=400&&httpStatus<500&&httpStatus!==408&&httpStatus!==429) {
        pendingReport=null;state.reportStatus=httpStatus===409?"Result reports disagreed":
          [401,403].includes(httpStatus)?"Sign in again to submit results":"Result rejected";
      } else if(!ok&&session) {
        pendingReport=job.body;reportRetries++;nextReport=now+Math.min(30000,1500*2**Math.min(reportRetries,5));
        state.reportStatus="Retrying result submission";
      } else {
        reportRetries=0;state.reportStatus=body?.recorded?"Result recorded":
          body?.status==="pending"?"Waiting for opponent confirmation":"Result submitted";
      }
      return;
    }
    if(!ok) {
      if(job.kind==="create")fail("Sign-in unavailable. Try again.");
      else {state.error="Reconnecting...";nextPoll=now+3500;}
      return;
    }
    state.error="";
    if(job.kind==="create") {
      if(!/^[ABCDEFGHJKLMNPQRSTUVWXYZ23456789]{6}$/.test(body.code||"")||
          !/^[A-Za-z0-9_-]{32,128}$/.test(body.pollSecret||"")) {fail("Could not create sign-in");return;}
      const savedPair={code:body.code,pollSecret:body.pollSecret,expiresAt:now+600000};
      if(!saveAccount({pending:savedPair})) {fail("Could not save sign-in to USB");return;}
      secret=body.pollSecret;state.code=body.code;
      state.pairUrl="https://aesthetic.computer/api/device-pair-login?code="+state.code;
      state.status="waiting";state.expiresAt=now+600000;nextPoll=now+1500;
    } else if(body.status==="claimed") {
      if(!validHandle(body.handle)||typeof body.session?.accessToken!=="string"||!body.session.accessToken) {
        fail("Sign-in response was incomplete");return;
      }
      if(!saveAccount({handle:body.handle,session:body.session})) {
        fail("Could not save sign-in to USB");return;
      }
      session=body.session;state.handle=body.handle;clearPair();state.status="signed-in";
    } else nextPoll=now+2500;
  }
  function update() {
    const now=clock();
    if(state.status==="waiting"&&now>=state.expiresAt) {fail("Code expired. Sign in again.");return;}
    if(pending&&now>=nextRead) {
      nextRead=now+250;
      const system=getSystem(),result=system.readFile(pending.base+".rc");
      if(result!==null&&result!==undefined&&String(result).trim()!=="") {
        const job=pending;pending=null;let body;
        try {body=JSON.parse(system.readFile(job.base+".json")||"null");}catch(_){}
        const httpStatus=Number(system.readFile(job.base+".http")||0);
        erase(job);receive(job,body,String(result).trim()==="0"&&httpStatus>=200&&httpStatus<300&&!!body,now,httpStatus);
      } else if(now-pending.started>16000) {
        const job=pending;cancelRequest();receive(job,null,false,now);
      }
    }
    if(pending)return;
    if(state.status==="waiting"&&now>=nextPoll) {
      nextPoll=now+2500;
      if(!request("poll","https://aesthetic.computer/api/device-pair?code="+state.code+"&secret="+secret))
        state.error="Sign-in connection unavailable";
    } else if(pendingReport&&session&&now>=nextReport) {
      const report=pendingReport;pendingReport=null;
      if(!request("report","https://aesthetic.computer/api/oskiewar-leaderboard",report,session.accessToken)) {
        pendingReport=report;nextReport=now+3500;state.reportStatus="Retrying result submission";
      }
    } else if(pendingBoard!==null&&now-lastBoard>=30000) {
      const handles=pendingBoard;pendingBoard=null;lastBoard=now;
      if(!request("board","https://aesthetic.computer/api/oskiewar-leaderboard?handles="+handles.join(",")))
        state.leaderboardError="Leaderboard unavailable";
    }
  }
  return {
    state() {update();return {...state};},
    action(action,payload="") {
      if(action==="leaderboard") {
        try {const supplied=JSON.parse(payload||"[]");
          if(!Array.isArray(supplied)||supplied.length>2)return false;
          const handles=[...new Set(supplied.filter(value=>value!==""))];
          if(!handles.every(validHandle))return false;
          if(clock()-lastBoard<30000)return false;
          pendingBoard=handles;update();return true;
        }catch(_){return false;}
      }
      if(!["login","logout","cancel"].includes(action))return false;
      if(action==="login"&&(state.status==="creating"||state.status==="waiting"&&clock()<state.expiresAt))return true;
      if((action==="logout"||action==="cancel"&&!session)&&!saveAccount({})) {
        state.error="Could not clear sign-in";return false;
      }
      cancelRequest();clearPair();state.error="";
      if(action==="logout") {
        session=null;pendingReport=null;state.reportStatus="";state.handle="";state.status="signed-out";
      } else if(action==="cancel"||session)state.status=session?"signed-in":"signed-out";
      else {
        state.status="creating";
        if(!request("create","https://aesthetic.computer/api/device-pair",{action:"create",kind:"browser"}))
          fail("Sign-in connection unavailable. Try again shortly.");
      }
      return true;
    },
    report(payload) {
      try {
        if(typeof payload!=="string"||payload.length>4096)return false;
        const p=JSON.parse(payload);
        if(!session||![0,1].includes(p.seat)||![0,1].includes(p.winner)||
            !/^[A-Za-z0-9._:-]{1,128}$/.test(p.matchId||"")||
            !Array.isArray(p.handles)||p.handles.length!==2||!p.handles.every(validHandle)||
            p.handles[p.seat]!==state.handle||!Array.isArray(p.roundWins)||p.roundWins.length!==2||
            !p.roundWins.every(n=>Number.isInteger(n)&&n>=0&&n<=100))return false;
        pendingReport={matchId:p.matchId,seat:p.seat,handles:p.handles,roundWins:p.roundWins,winner:p.winner};
        reportRetries=0;nextReport=0;state.reportStatus="Submitting result";update();return true;
      }catch(_){return false;}
    },
  };
}

// Explicit development override; credentials and public matchmaking stay HTTPS.
export function oskiewarRelayBase(value) {
  const fallback = "wss://session-server.aesthetic.computer/oskiewar-live";
  if (typeof value !== "string") return fallback;
  const match = /^ws:\/\/(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3}):(\d{1,5})\/oskiewar-live$/.exec(value.trim());
  if (!match) return fallback;
  const parts = match.slice(1, 5).map(Number), port = Number(match[5]);
  if (parts.some(n => n > 255) || port < 1 || port > 65535) return fallback;
  const [a, b] = parts;
  return a === 10 || (a === 172 && b >= 16 && b <= 31) || (a === 192 && b === 168)
    ? value.trim() : fallback;
}

export function createNativeHost(initialApi) {
  let api = initialApi;
  const gameSound = createOskiewarSound(() => api.sound);
  const gameAccount = createOskiewarAccount(() => api.system);
  let tick = 0, paints = 0;
  let positions = [], colors = [], text = [], overlays = [], gpu = false;
  const useGpu = api.system.readFile("/tmp/oskiewar-gpu") === "1";
  const held = new Set();
  let lastSignal = "", lastError = "", lastReportAt = 0;
  let reports = [], reportPending = false, reportRetryAt = 0, reportStatus = "";
  try { reports = JSON.parse(api.system.readFile("/pieces/oskiewar-error-queue.json") || "[]"); } catch (_) {}
  const saveReports = () => api.system.writeFile("/pieces/oskiewar-error-queue.json", JSON.stringify(reports), false);
  function reportErrors() {
    if (reportPending && api.system.fetchResult) {
      try {
        if (JSON.parse(api.system.fetchResult).ok) {
          reports.shift(); saveReports(); reportStatus = "reported to server";
        }
      } catch (_) {}
      reportPending = false; reportRetryAt = Date.now() + 10000;
    } else if (reportPending && api.system.fetchError) {
      reportPending = false; reportRetryAt = Date.now() + 10000;
      reportStatus = "retrying server report";
    }
    if (!reportPending && reports.length && Date.now() >= reportRetryAt) {
      reportPending = api.system.fetchPost("https://aesthetic.computer/api/piece-log",
        JSON.stringify(reports[0]), '{"Content-Type":"application/json"}') === true;
      reportStatus = reportPending ? "posting to server" : "queued for server";
    }
  }
  let canvas, captureId = "", captureRequest = "";
  let triangleCount = 0, pads = [];
  let netRoom = "", netRetryAt = 0, stageReadAt = 0, stageReceipt = "";
  const netEvents = [];
  const wire = { sent:0, received:0, rejected:0, last:null };
  const view = () => ({ width: Math.round(768 * api.screen.width / api.screen.height), height: 768 });
  const scale = () => api.screen.height / 768;
  function record(event, detail) {
    if (event === "CLIENT_ERROR") {
      let data; try { data = JSON.parse(detail); } catch (_) { data = {message:String(detail)}; }
      reports.push({pieceId:"oskiewar-ac-" + Date.now() + "-" + tick, phase:"error",
        meta:{slug:"oskiewar",host:"ac-native",platform:"ac-os"},data});
      if (reports.length > 8) reports.shift();
      saveReports();
    }
    if (event.startsWith("NET_")) { netEvents.push([event, detail]); if (netEvents.length > 8) netEvents.shift(); }
    if (/error/i.test(event)) {
      lastError = String(detail);
      api.system?.writeFile?.("/tmp/oskiewar-error.json", JSON.stringify({ event, detail }));
    }
  }
  function triangle3d(x1, y1, z1, x2, y2, z2, x3, y3, z3, r = 255, g = 255, b = 255) {
    triangleCount++;
    if (api.triangle3d) {
      const s = scale();
      api.triangle3d(x1*s,y1*s,z1,x2*s,y2*s,z2,x3*s,y3*s,z3,r,g,b);
      return;
    }
    const { width, height } = view();
    positions.push(screenVertex(x1, y1, z1, width, height),
      screenVertex(x2, y2, z2, width, height), screenVertex(x3, y3, z3, width, height));
    const color = [r / 255, g / 255, b / 255, 1];
    colors.push(color, color, color);
  }
  function write(value, x, y, size = 20, r = 255, g = 255, b = 255) {
    text.push([String(value), x, y, size, r, g, b]);
  }
  write.measureGlyph = (character, size) => pixelGlyph(character, size, scale()).advance / scale();
  function synth(tone, duration = .05) { gameSound.synth(tone, duration); }
  const host = {
    accountState: () => gameAccount.state(),
    accountAction: (action,payload) => gameAccount.action(action,payload),
    accountReport: payload => gameAccount.report(payload),
    runtime: () => ({ monotonicUs: Math.round(tick * 1e6 / 60), paintMonotonicUs: Date.now() * 1000,
      simMonotonicUs: Math.round(tick * 1e6 / 60), paintMonotonicUs: Date.now() * 1000, unixMs: Date.now(),
      simCount: tick, paintCount: paints, renderAlpha: 1, clientErrorReportStatus: reportStatus }),
    gameView: view,
    controllers: () => pads,
    gamepad: (index = 0) => {
      const pad = pads[index] || { connected: index === 0, down: [], leftX: 0, leftY: 0, rightX: 0, rightY: 0 };
      return { ...pad, down: [...new Set([...(pad.down || []), ...(index === 0
        ? [...held].map(key => KEY_BUTTONS[key]).filter(Boolean) : [])])] };
    },
    capabilities: () => ({ platform: "ac-native", inputFamily: pads[0]?.connected ? "xbox" : "keyboard",
      showControlLegend: true, showControllerDiagram: false, hudOverlay: true }),
    telemetry: record,
    gameSignal: (name) => { lastSignal = String(name); },
    wipe: (r, g, b) => { triangleCount = 0; positions = []; colors = []; text = []; overlays = []; api.wipe(r, g, b); gpu = useGpu && api.gpuBegin?.(r, g, b) === true; },
    box: (x, y, w, h, r = 255, g = 255, b = 255) => {
      const s = scale();
      overlays.push(["box", r,g,b,x*s,y*s,w*s,h*s]);
    },
    line: (x1, y1, x2, y2, width = 1, r = 255, g = 255, b = 255) => {
      const s = scale();
      overlays.push(["line", r,g,b,x1*s,y1*s,x2*s,y2*s]);
    },
    triangle3d, write, systemWrite: write,
    themeReady: () => useGpu && api.themeReady?.() === true,
    themeAssetReady: (asset) => useGpu && api.themeAssetReady?.(asset) === true,
    themeSprite: (asset,sx,sy,sw,sh,x,y,w,h,angle,flip,z,depthWrite=true) => {
      if (!gpu || !api.themeSprite) return false;
      const s = scale();
      return api.themeSprite(asset,sx,sy,sw,sh,x*s,y*s,w*s,h*s,angle,flip,z,depthWrite);
    },
    themeQuad: (asset,sx,sy,sw,sh,...corners) => {
      if (!gpu || !api.themeQuad || corners.length !== 12) return false;
      const s = scale();
      return api.themeQuad(asset,sx,sy,sw,sh,...corners.map((value,i) => i%3 === 2 ? value : value*s));
    },
    disc3d: (x, y, z, radius, r, g, b) => {
      if (!gpu || !api.disc3d) return false;
      const sides = radius < 6 ? 6 : radius < 13 ? 8 : radius < 26 ? 12
        : radius < 52 ? 16 : radius < 110 ? 24 : 32;
      const s = scale();
      const count = api.disc3d(x*s,y*s,z,radius*s,sides,r,g,b);
      triangleCount += count;
      return count > 0;
    },
    triangle: (x1, y1, x2, y2, x3, y3, r, g, b) =>
      triangle3d(x1, y1, -.5, x2, y2, -.5, x3, y3, -.5, r, g, b),
    synth,
    drum: (name, velocity = 1) => gameSound.drum(name, velocity),
    update(next) { api = next; },
    connectNet(room) {
      netRoom = room; netRetryAt = Date.now() + 5000;
      let relay;
      try { relay = api.system.readFile("/pieces/oskiewar-relay.txt"); } catch (_) {}
      api.system.ws.connect(oskiewarRelayBase(relay) + "?match=" +
        room + "&role=challenger&surface=ac");
    },
    sendNet(room, packet) {
      if (room !== netRoom || !api.system.ws.connected) { wire.rejected++; return false; }
      wire.sent++;
      api.system.ws.send(JSON.stringify({ type: "oskiewar:net", content: packet }));
      return true;
    },
    readNet() {
      reportErrors();
      if (Date.now() >= stageReadAt) {
        stageReadAt = Date.now() + 100;
        try {
          const command = JSON.parse(api.system.readFile("/pieces/oskiewar-control.json") || "null");
          if (command && typeof command.id === "string") {
            globalThis.__oskiewarStageCommand = command;
            if (!netRoom) globalThis.__oskiewarStageState = command;
          }
        } catch (_) {}
      }
      if (!netRoom) return;
      const ws = api.system.ws;
      for (const raw of ws.messages || []) {
        try {
          const message = JSON.parse(raw);
          if (message.type === "oskiewar:net" && message.content &&
              globalThis.__oskiewarNetInbox.length < 64)
            { wire.received++; wire.last = message.content; globalThis.__oskiewarNetInbox.push(message.content); }
        } catch (_) {}
      }
      if (!ws.connected && !ws.connecting && Date.now() >= netRetryAt)
        host.connectNet(netRoom);
    },
    closeNet() { if (netRoom) api.system.ws.close(); netRoom = ""; },
    readGamepads() {
      try {
        const value = JSON.parse(api.system.readFile("/tmp/oskiewar-gamepads.json") || "null");
        pads = value && Math.abs(Date.now() - value.at) < 2000 ? value.pads : [];
      } catch (_) { pads = []; }
    },
    begin() {
      if (paints % 60 === 0) {
        const request = api.system?.readFile?.("/tmp/oskiewar-capture");
        if (request && request !== captureId) captureRequest = request;
      }
      if (captureRequest) {
        if (!canvas || canvas.width !== api.screen.width || canvas.height !== api.screen.height)
          canvas = api.painting(api.screen.width, api.screen.height);
        api.page(canvas);
      } else api.page();
    },
    step() { tick++; },
    act(event) {
      const key = String(event.key || "").toLowerCase();
      if (event.is("keyboard:down")) held.add(key);
      if (event.is("keyboard:up")) held.delete(key);
    },
    clearInput() { held.clear(); },
    flush(state = {}) {
      if (positions.length) {
        const form = new api.Form({ type: "triangle", positions, colors });
        form.noFade = true;
        api.ink(255, 255, 255).form(form);
      }
      if (gpu) api.gpuEnd();
      for (const [kind,r,g,b,...args] of overlays) { api.ink(r,g,b); api[kind](...args); }
      const s = scale();
      for (const [value, x, y, size, r, g, b] of text) {
        api.ink(r, g, b);
        const style = pixelGlyph("a", size, s);
        const px = Math.round(x * s), py = Math.round(y * s);
        if (/^[\x20-\x7e]*$/.test(value)) {
          api.write(value, {x: px, y: py, size: style.zoom, font: style.font});
        } else {
          let cursor = px;
          for (const character of value) {
            const glyph = pixelGlyph(character, size, s);
            api.write(character, {x: cursor, y: py, size: glyph.zoom, font: glyph.font});
            cursor += glyph.advance;
          }
        }
      }
      const battery = api.system.battery;
      if (battery && battery.percent >= 0 && !globalThis.__oskiewarStageState?.curtain) {
        const minutes = battery.minutesLeft;
        const time = !battery.charging && minutes > 0
          ? " " + Math.floor(minutes / 60) + "h " + minutes % 60 + "m" : "";
        const label = battery.percent + "%" + (battery.charging ? " +" : time);
        const width = label.length * 6 + 10;
        const x = api.screen.width - width - 5;
        api.ink(20, 27, 42).box(x, 4, width, 16);
        api.ink(...(battery.percent < 15 && !battery.charging
          ? [255, 150, 110] : [232, 245, 255]));
        api.write(label, {x: x + 5, y: 7, size: 1, font: "6x10"});
      }
      const stage = globalThis.__oskiewarStageState;
      const receipt = stage ? stage.id + "/" + stage.peerAck : "";
      if (receipt && receipt !== stageReceipt) {
        stageReceipt = receipt;
        api.system.writeFile("/pieces/oskiewar-stage-status.json", JSON.stringify(stage), false);
      }
      paints++;
      api.page();
      if (captureRequest) api.paste(canvas, 0, 0);
      if (Date.now() - lastReportAt > 2000) {
        lastReportAt = Date.now();
        api.system?.writeFile?.("/tmp/oskiewar-status.json", JSON.stringify({
          ...state, nativeMath: api.oskiewarMath?.version || null, reportStatus, stage: globalThis.__oskiewarStageState || null, netEvents, wire, tick, paints, triangles: triangleCount, renderer: gpu ? "gpu-triangles" : api.triangle3d ? "native-triangles" : "form",
          gamepads: pads.map(p => ({ connected: p.connected, id: p.id, down: p.down })),
          screen: api.screen, battery: api.system.battery, lastSignal, lastError, at: lastReportAt,
        }), false);
      }
        if (captureRequest) {
          captureId = captureRequest;
          captureRequest = "";
          const pixels = new Uint32Array(canvas.pixels.buffer);
          const runs = [];
          for (let i = 0; i < pixels.length;) {
            const color = pixels[i]; let end = i + 1;
            while (end < pixels.length && pixels[end] === color) end++;
            runs.push(color, end - i); i = end;
          }
          api.system.writeFile("/tmp/oskiewar-frame.json", JSON.stringify({
            id: captureId, width: canvas.width, height: canvas.height,
            stride: pixels.length / canvas.height, runs,
          }), false);
        }
      positions = []; colors = []; text = [];
    },
  };
  return host;
}
