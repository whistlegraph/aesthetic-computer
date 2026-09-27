import {verifyAccount, requireHandle} from "/easel/src/account-access.mjs";
// The headless half of a phone session: everything that is not a renderer.
//
// `app.mjs` draws the browser UI and `apple/aesel` draws a SwiftUI one. Both
// want the same things to happen — mount a piece, run a turn through the shared
// bridge, publish what the model writes, report progress — so that lives here
// and the hosts only render.
//
// This is the same division Oskiewar makes. Its engine emits a triangle stream
// and canvas, D3D and Metal each draw it; `MetalSceneView.swift` says so in its
// header. Aesel's equivalent of that stream already existed before anyone
// needed a second renderer: `AcServer` emits six notification kinds, and the
// desktop TUI is just one thing that reads them. A SwiftUI view is another.
//
// Nothing here touches the DOM, so it runs the same in a visible page and in
// the off-screen WKWebView the iOS app keeps for exactly this purpose.

import { AcServer, DEFAULT_AC_MODEL } from "/easel/src/ac-server.mjs";
import { fetchHandleColors, handleCharacterColors } from "/easel/src/handle-colors.mjs";
import { isTransientNetworkError, withNetworkDeadline, httpError } from "/easel/src/network.mjs";
import { publishPiece } from "/easel/src/publish.mjs";
import * as vfs from "/easel/phone/shim/fs.mjs";
import { createCredits } from "./credits.mjs";
import {revisionSummary} from "/easel/src/revision-summary.mjs";
import {migrateRevisionCheckpoints} from "./revision-history.mjs";
import {validatePieceSource} from "./shim/revisions.mjs";
import { NativeProvider } from "./native-provider.mjs";

export const SITE = "https://aesthetic.computer";
export const AUTH_DOMAIN = "hi.aesthetic.computer";
export const MODEL_CHOICES = []; // Braincell routing is managed by AC.

// Fetched rather than bundled, at the paths the bridge's own `bundledContext()`
// builds, so editing a guide reaches the phone on reload.
const GUIDES = [
  "/easel/context/pieces.md",
  "/easel/context/screen.md",
  "/easel/context/hand.md",
  "/easel/context/kidlisp.md",
  "/easel/context/api.json",
];

const STARTER = `// A new piece.
export function paint({ wipe }) {
  wipe("purple");
}
`;

// A slug a person can say out loud.
export function freshSlug() {
  const vowels = "aeiou";
  const consonants = "bcdfghjklmnprstvwz";
  let out = "";
  for (let i = 0; i < 3; i += 1) {
    out += consonants[Math.floor(Math.random() * consonants.length)];
    out += vowels[Math.floor(Math.random() * vowels.length)];
  }
  return out;
}

// `publish.mjs` sets a `User-Agent` header, which is right for Node and wrong
// in a webview: it is a forbidden header name, so a browser is supposed to drop
// it. Chrome does, silently — the browser client publishes fine. WebKit leaks
// it into the CORS preflight instead, and `/presigned-upload-url` answers
// `Access-Control-Allow-Headers: Content-Type, Authorization, X-Requested-With`
// with no `user-agent`, so the preflight is refused and the whole publish comes
// back as the uninformative "Load failed".
//
// Stripping it here rather than in `publish.mjs` keeps the shared file correct
// for the desktop, which genuinely wants to identify itself.
const FORBIDDEN = ["user-agent"];

function browserFetch(input, init = {}) {
  if (!init.headers) return fetch(input, init);
  const headers = {};
  for (const [name, value] of Object.entries(init.headers)) {
    if (FORBIDDEN.includes(name.toLowerCase())) continue;
    headers[name] = value;
  }
  return fetch(input, { ...init, headers });
}

// Storage is injected because the two hosts keep a session in different places
// — localStorage in a browser, the app's Documents container on iOS — and that
// is the only thing about persistence they disagree on.
const memoryStore = () => {
  const map = new Map();
  return {
    get: (key) => map.get(key) ?? null,
    set: (key, value) => void map.set(key, value),
  };
};

export function createSession({ storage = memoryStore(), emit = () => {}, hostRPC = null, retryOptions = {}, accountFetch = (...args) => fetch(...args) } = {}) {
  const state = {
    accountVerified: false,
    token: "",
    handle: "",
    slug: "",
    file: "",
    server: null,
    busy: false,
    publishing: null,
    dirty: false,
    published: false,
    id: "",
    medium: "piece",
    transcript: [],
    engine: null,
    title: "",
    owner: "",
    model: DEFAULT_AC_MODEL,
    composer: "",
    provider: "ac",
    providerModels: {},
    hostOperation: null,
    revisions: [],
    selectedRevision: null,
    publication: null,
    autoPublish: true,
  };
  Object.defineProperty(state,"version",{enumerable:true,get:()=>state.revisions.at(-1)?.version ?? 0});
  let hostProviders = [];
  let mcpAutoAllow = true, supportsApprovalPolicy = false;
  let accountEpoch = 0;
  const credits = createCredits({ token: () => state.token, emit, site: SITE });

  const read = () => {
    try {
      return JSON.parse(storage.get("session") || "{}");
    } catch {
      return {};
    }
  };

  const write = (patch) => {
    try {
      storage.set("session", JSON.stringify({ ...read(), ...patch }));
    } catch {
      // A full or unavailable store should cost persistence, not the session.
    }
  };

  // Credentials stay in the separate account record, never in a thread.
  const readThreads = () => {
    try {
      const value = JSON.parse(storage.get("threads") || "{}");
      return value.schema === 1 && Array.isArray(value.items) ? value.items : [];
    } catch { return []; }
  };
  let saveTimer;
  let pendingRevision = null;
  const say = (type, payload = {}) => {
    const event = { type, ...payload };
    const ephemeral = type === "bridge" && ["turn/progress", "item/modelCode/delta"].includes(payload.method);
    if (state.id && !ephemeral && ["you", "note", "bad", "bridge"].includes(type)) {
      const last = state.transcript.at(-1);
      if (type === "bridge" && payload.method === "item/agentMessage/delta" && last?.method === payload.method && !state.revisions.some(r=>r.transcriptEnd === state.transcript.length)) {
        last.params.delta += payload.params?.delta || "";
      } else {
        state.transcript.push(JSON.parse(JSON.stringify(event)));
      }
      if (type === "bridge" && payload.method === "turn/completed" && pendingRevision !== null) {
        const revision = state.revisions.find(r=>r.version === pendingRevision);
        if (revision) revision.transcriptEnd = state.transcript.length;
        pendingRevision = null;
      }
      clearTimeout(saveTimer);
      saveTimer = setTimeout(saveCurrent, 250);
    }
    emit(event);
  };

  function history() {
    return readThreads().sort((a, b) => b.savedAt.localeCompare(a.savedAt)).map(item => ({
      id: item.id, title: item.title || item.slug, medium: item.medium,
      route: item.handle ? `@${item.handle}/${item.slug}` : item.slug,
      updatedAt: item.savedAt,
    }));
  }

  function saveCurrent() {
    clearTimeout(saveTimer);
    if (!state.id || !state.file) return;
    const engine = state.server && state.provider === "ac" ? {
      threadId: state.server.threadId || "", messages: state.server.messages || [], turns: state.server.turns || 0, model: state.model,
    } : state.engine;
    const item = {
      id: state.id, title: state.title || state.slug, medium: state.medium, model: state.model,
      savedAt: new Date().toISOString(), handle: state.owner || state.handle, slug: state.slug,
      source: vfs.readFileSync(state.file), published: state.published, version:state.version,
      events: state.transcript, engine, composer: state.composer,
      revisionSchema: 2, revisions: state.revisions, publication: state.publication, autoPublish: state.autoPublish,
      selectedRevision: state.selectedRevision,
      provider: state.provider, providerModels: state.providerModels, hostOperation: state.hostOperation,
    };
    const items = readThreads().filter(entry => entry.id !== state.id);
    items.push(item);
    try {
      const encoded = JSON.stringify({schema: 1, items});
      if (encoded.length > 32 * 1024 * 1024) throw new Error("Saved threads exceed 32 MB.");
      storage.set("threads", encoded);
      write({ threadID: state.id });
      emit({ type: "history", items: history() });
    } catch (error) {
      emit({type: "bad", text: `Could not save this thread: ${error.message}`});
    }
  }

  async function settleCurrent() {
    if (state.hostOperation && !state.busy) throw new Error("Reconnect to resolve the current host turn before changing threads.");
    stop();
    const deadline = Date.now() + 15000;
    while (state.busy || state.publishing) {
      if (Date.now() > deadline) throw new Error("Wait for the current turn or upload to finish before switching threads.");
      await new Promise(resolve => setTimeout(resolve, 50));
    }
    saveCurrent();
  }

  function loadThread(item) {
    state.selectedRevision = null;
    pendingRevision = null;
    state.id = item.id;
    state.owner = item.handle || state.handle;
    state.medium = item.medium || "piece";
    state.title = item.title || item.slug;
    state.transcript = Array.isArray(item.events) ? item.events : [];
    state.engine = item.engine || null;
    state.provider = ["ac", "claude", "codex"].includes(item.provider) ? item.provider : "ac";
    state.providerModels = item.providerModels || {};
    state.hostOperation = item.hostOperation || null;
    state.model = state.provider === "ac" ? DEFAULT_AC_MODEL : item.model || "";
    state.composer = typeof item.composer === "string" ? item.composer : "";
    state.revisions = Array.isArray(item.revisions) ? item.revisions : [];
    state.publication = item.publication || null;
    if(item.revisionSchema !== 2 && state.revisions.length) {
      state.revisions=state.revisions.map(record=>({...record,version:Math.max(0,record.version-1)}));
      if(state.publication)state.publication={...state.publication,version:Math.max(0,state.publication.version-1)};
    }
    if(!state.revisions.length && Number.isInteger(item.version) && item.version>=0) {
      state.revisions=[{version:item.version,source:item.source || STARTER,reason:"opened",summary:item.version===0?"First version.":"Saved version.",at:item.savedAt || new Date().toISOString()}];
    }
    state.revisions = migrateRevisionCheckpoints(state.revisions, state.transcript);
    state.autoPublish = item.autoPublish !== false;
    mountPiece(item.slug, item.source || STARTER);
    recordRevision(item.source || STARTER, "opened");
    state.published = Boolean(item.published && (!item.handle || item.handle === state.handle));
    write({ threadID: state.id, published: state.published });
    emit({type: "thread", id: state.id, medium: state.medium, events: state.transcript, composer: state.composer});
    reportProvider();
    reportRevisions();
    if (state.published && state.handle) say("preview", {url: pieceUrl()});
    else if (item.source && item.source !== STARTER) say("source", {source: item.source});
    say("status", {text: state.token ? "ready" : "signed out", kind: "idle"});
    if (Number.isInteger(item.selectedRevision) && state.revisions.some(r=>r.version === item.selectedRevision) && !state.hostOperation) selectRevision(item.selectedRevision);
  }

  async function resumeSession(id) {
    let item = readThreads().find(entry => entry.id === id);
    if (!item) throw new Error("That saved thread could not be found.");
    if (item.medium !== "piece") throw new Error("This medium is not supported on iPhone yet.");
    await settleCurrent();
    item = readThreads().find(entry => entry.id === id);
    loadThread(item);
    emit({type: "history", items: history()});
    await prepublishBlank();
  }

  let blankPublication = null;
  function prepublishBlank() {
    if (!state.accountVerified || !state.token || !state.handle || !state.id ||
        !state.autoPublish || state.published || state.selectedRevision !== null ||
        vfs.readFileSync(state.file) !== STARTER) return Promise.resolve();
    // Account restoration and opening a notebook can finish in either order.
    // Coalesce both paths, including verification before the upload starts.
    if (!blankPublication) {
      blankPublication = publish().finally(() => { blankPublication = null; });
    }
    return blankPublication;
  }

  async function newSession(medium = "piece") {
    if (medium !== "piece") throw new Error("This medium is not supported on iPhone yet.");
    await settleCurrent();
    const slug = freshSlug();
    loadThread({id: `${Date.now()}-${slug}`, slug, medium, source: STARTER, events: [], published: false});
    saveCurrent();
    await prepublishBlank();
  }

  function setDraft(text, threadID = state.id) {
    requireCurrentRevision();
    if (!threadID || threadID !== state.id) throw new Error("The draft belongs to another thread.");
    if (typeof text !== "string" || new TextEncoder().encode(text).length > 32768) throw new Error("Drafts are limited to 32 KB.");
    if (state.composer === text) return;
    state.composer = text;
    saveCurrent();
  }

  function exportNotebook() {
    const notebook={format:"aesel-notebook",schema:1,revisionStart:0,medium:"piece",title:state.title,source:vfs.readFileSync(state.file),
      composer:state.composer,revisions:state.revisions.map(({version,source,reason,at,summary,transcriptEnd})=>({version,source,reason,at,summary,
        ...(Number.isInteger(transcriptEnd)?{transcriptEnd:state.transcript.slice(0,transcriptEnd).filter(e=>e.type==="you" || (e.type==="bridge" && e.method==="item/agentMessage/delta")).length}:{})})),
      transcript:state.transcript.filter(e=>e.type==="you" || (e.type==="bridge" && e.method==="item/agentMessage/delta"))
        .map(e=>({role:e.type==="you"?"user":"assistant",text:e.text || e.params?.delta || ""}))};
    const json=JSON.stringify(notebook,null,2);
    if(new TextEncoder().encode(json).length>8*1024*1024)throw new Error("Notebook export exceeds 8 MB.");
    say("export",{json});return json;
  }

  async function importNotebook(text) {
    if(typeof text!=="string" || new TextEncoder().encode(text).length>8*1024*1024)throw new Error("Notebook import exceeds 8 MB.");
    const value=JSON.parse(text);
    if(value.format!=="aesel-notebook" || value.schema!==1 || value.medium!=="piece")throw new Error("Choose an Aesel piece notebook.");
    const check=async source=>{
      if(typeof source!=="string" || new TextEncoder().encode(source).length>100000)throw new Error("Imported source exceeds 100 KB.");
      await validatePieceSource(source,"piece.mjs");
    };
    await check(value.source);
    if(!Array.isArray(value.revisions) || value.revisions.length>50)throw new Error("Invalid revision history.");
    const revisions=[];
    for(const record of value.revisions){
      if(!Number.isInteger(record.version) || record.version<0 || record.version<=(revisions.at(-1)?.version ?? -1))throw new Error("Invalid revision order.");
      await check(record.source);
      revisions.push({version:record.version,source:record.source,...(Number.isInteger(record.transcriptEnd) && record.transcriptEnd>=0 && record.transcriptEnd <= (value.transcript?.length ?? 0)?{transcriptEnd:record.transcriptEnd}:{}),summary:String(record.summary || "").slice(0,160),reason:String(record.reason || "imported").slice(0,100),at:String(record.at || "").slice(0,40)});
    }
    const composer=typeof value.composer==="string"?value.composer:"";
    if(new TextEncoder().encode(composer).length>32768)throw new Error("Imported draft exceeds 32 KB.");
    const events=[];
    for(const entry of Array.isArray(value.transcript)?value.transcript:[]){
      if(!['user','assistant'].includes(entry.role) || typeof entry.text!=="string" || entry.text.length>100000)throw new Error("Invalid transcript.");
      events.push(entry.role==="user"?{type:"you",text:entry.text}:{type:"bridge",method:"item/agentMessage/delta",params:{delta:entry.text}});
    }
    await settleCurrent();
    const slug=freshSlug();
    loadThread({id:`${Date.now()}-${slug}`,slug,title:String(value.title || slug).slice(0,120),medium:"piece",source:value.source,
      composer,revisions,revisionSchema:value.revisionStart===0?2:1,events,autoPublish:false,published:false});
    saveCurrent();return state.id;
  }

  function route() {
    return state.handle ? `@${state.handle}/${state.slug}` : state.slug;
  }

  function pieceUrl() {
    if (!state.handle) return "";
    // Cache-busted: the URL is stable and the bytes behind it are not.
    return `${SITE}/@${state.handle}/${state.slug}?nolabel=true&nogap=true&autoreload=true#${Date.now()}`;
  }

  function mountPiece(slug, source) {
    state.slug = slug;
    state.file = `/piece/${slug}.mjs`;
    state.server = null; // a new piece is a new conversation
    vfs.mount(state.file, source);
    state.published = false;
    write({ slug, source, published: false });
    say("piece", { route: route(), slug, source, version:state.version });
  }

  function recordRevision(source, reason) {
    if (state.revisions.at(-1)?.source === source) return;
    const version = (state.revisions.at(-1)?.version ?? -1) + 1;
    const restoredFrom=/^restored v(\d+)$/.exec(reason)?.[1];
    state.revisions.push({version, source, reason, at:new Date().toISOString(), transcriptEnd:state.transcript.length,
      summary:revisionSummary(state.revisions.at(-1)?.source,source,restoredFrom!==undefined?{restoredFrom:Number(restoredFrom)}:{})});
    while (state.revisions.length > 50) state.revisions.shift();
    if (state.busy) pendingRevision = version;
  }

  function reportRevisions() {
    say("revisions", {items:state.revisions.map(({source,...item})=>item),
      current:state.revisions.at(-1)?.version || 0,
      published:state.publication?.version ?? null,autoPublish:state.autoPublish});
  }

  function editable(threadID) {
    requireCurrentRevision();
    if(threadID !== state.id) throw new Error("This edit belongs to another thread.");
    if(state.busy || state.hostOperation) throw new Error("Finish the current turn before editing source.");
  }

  async function editSource(source, threadID = state.id, reason = "edited") {
    editable(threadID);
    if(typeof source !== "string" || new TextEncoder().encode(source).length > 100000) throw new Error("Source is limited to 100 KB.");
    await validatePieceSource(source,state.file);
    editable(threadID);
    vfs.mount(state.file,source);
    onWritten(state.file,source,reason);
  }

  function previewRevision(version,threadID = state.id) {
    if(threadID!==state.id)throw new Error("This version belongs to another thread.");
    const revision=state.revisions.find(item=>item.version===version);
    if(!revision)throw new Error("This version is no longer saved.");
    say("revisionPreview",{threadID,version,source:revision.source,summary:revision.summary || revision.reason});
  }

  function requireCurrentRevision() {
    if (state.selectedRevision !== null) throw new Error(`Return to v${state.version} to make changes.`);
  }

  function selectRevision(version, threadID = state.id) {
    if (threadID !== state.id) throw new Error("This version belongs to another thread.");
    if (state.busy || state.publishing || state.hostOperation) throw new Error("Wait for the current turn and upload before viewing history.");
    const revision = state.revisions.find(r=>r.version === version);
    if (!revision) throw new Error("This version is no longer saved.");
    const current = version === state.version;
    state.selectedRevision = current ? null : version;
    const available = current || Number.isInteger(revision.transcriptEnd);
    emit({type:"revisionSelection", threadID, version, current:state.version, source:current ? vfs.readFileSync(state.file) : revision.source,
      events:current ? state.transcript : available ? state.transcript.slice(0, revision.transcriptEnd) : [], transcriptAvailable:available});
    saveCurrent();
  }

  async function restoreRevision(version,threadID = state.id) {
    editable(threadID);
    const revision=state.revisions.find(item=>item.version===version);
    if(!revision)throw new Error("This revision is no longer saved.");
    return editSource(revision.source,threadID,`restored v${version}`);
  }

  function setAutoPublish(enabled) {
    if(typeof enabled!=="boolean")throw new Error("Invalid publication setting");
    state.autoPublish=enabled;saveCurrent();reportRevisions();
  }

  function onWritten(path, source, reason = "generated") {
    if (path !== state.file) return;
    recordRevision(source,reason);
    state.published = state.publication?.source === source && state.publication?.handle === state.handle;
    write({ slug: state.slug, source, published:state.published });
    saveCurrent();reportRevisions();
    state.dirty = true;
    say("source", { source, version:state.version });
    say("publication",{url:state.published?state.publication.url:null});
    if(state.autoPublish)void publish();
  }

  async function publish() {
    try { await verifyWorkAccount(); } catch (error) { say("notice", {scope:"account",text:error.message,action:"signIn"}); return; }
    requireCurrentRevision();
    if (state.publishing) return state.publishing;
    if (!state.token || !state.handle) {
      say("notice", {scope:"publish", text: "Sign in with an Aesthetic.Computer @handle to publish.", action:"signIn" });
      return;
    }
    state.dirty = false;
    const snapshot={source:vfs.readFileSync(state.file),handle:state.handle,token:state.token,
      version:state.revisions.at(-1)?.version ?? 0,threadID:state.id,file:state.file,slug:state.slug};
    let lastStep = "starting";
    state.publishing = (async () => {
      try {
        say("notice", {scope:"publish", text:"Publishing…", working:true});
        const result=await publishPiece({version:null,source:snapshot.source,
          file:snapshot.file,slug:snapshot.slug,cwd:"/piece",site:SITE,
          session:{handle:snapshot.handle,signedIn:true,token:async()=>snapshot.token},fetch:browserFetch,
          retryOptions,
          onRetry:({attempt})=>say("notice",{scope:"publish",text:`Connection interrupted—retrying upload (${attempt}/2)…`,working:true}),
          onStep:step=>{lastStep=step;},
        });
        if(!result.verified)throw new Error("Upload sent, but its public bytes could not be verified. Retry Publish to verify the saved source.");
        if(state.id!==snapshot.threadID)return;
        state.publication={source:snapshot.source,handle:snapshot.handle,version:snapshot.version,url:result.route,verifiedAt:new Date().toISOString()};
        state.published=state.handle===snapshot.handle && vfs.readFileSync(state.file)===snapshot.source;
        state.owner=snapshot.handle;
        write({published:state.published});saveCurrent();reportRevisions();
        say("notice",{scope:"publish",text:""});
        say("publication",{url:state.published?result.route:null});
        if(state.published) {
          say("status",{text:"live",kind:"live"});
          say("preview",{url:pieceUrl()});
        }
      } catch(error) {
        if(state.id!==snapshot.threadID)return;
        say("diagnostic",{operation:"publish",step:lastStep,message:error.message});
        const auth=error.status===401 || error.status===403;
        const text=isTransientNetworkError(error) ? "Upload paused. Your piece is saved here." :
          auth ? "Sign in again to publish. Your piece is saved here." :
          lastStep==="verifying" ? "Your piece is saved here; publication could not be verified." : "Couldn’t publish: "+error.message;
        say("notice",{scope:"publish",text,action:auth?"signIn":"publish"});
      } finally {
        state.publishing=null;
        if(state.dirty && state.autoPublish)void publish();
      }
    })();
    return state.publishing;
  }

  function handoff() {
    return state.transcript.filter(e=>e.type==="you" || (e.type==="bridge" && e.method==="item/agentMessage/delta"))
      .map(e=>(e.type==="you"?"User: "+e.text:"Assistant: "+(e.params?.delta || "")))
      .join("\n").slice(-16000);
  }

  function buildServer() {
    if (state.provider !== "ac") {
      if (!hostRPC) throw new Error("Connect Aesel Host before using this provider.");
      const server = new NativeProvider({rpc:hostRPC,sessionID:state.id,provider:state.provider,model:state.model,
        source:()=>vfs.readFileSync(state.file),onSource:source=>vfs.writeFileSync(state.file,source),
        operation:state.hostOperation,context:handoff(),
        onOperation:operation=>{
          const changed=JSON.stringify(state.hostOperation)!==JSON.stringify(operation);
          state.hostOperation=operation?{...operation}:null;
          if(changed) { saveCurrent(); say("hostOperation",{operation:state.hostOperation}); }
        }});
      server.on("notification",event=>say("bridge",event));
      server.on("approval",approval=>say("approval",{approval}));
      return server;
    }
    const server = new AcServer({
      cwd: "/piece",
      model: state.model,
      piece: { file: state.file },
      // The bridge awaits `token()` per turn so a desktop session can refresh
      // a stale one mid-conversation; the phone has nothing to refresh yet.
      token: async () => state.token,
      site: SITE,
      // Stored and called as `this.fetch(…)`. Node tolerates that; a browser
      // throws "Illegal invocation" unless window.fetch is bound to window.
      fetch: globalThis.fetch.bind(globalThis),
    });
    if (!state.engine && state.transcript.length > 1) server.messages=[{role:"user",content:"Prior visible conversation:\n"+handoff()}];
    if (state.engine) {
      server.threadId = state.engine.threadId || "";
      server.messages = JSON.parse(JSON.stringify(state.engine.messages || []));
      server.turns = state.engine.turns || 0;
    }
    server.on("notification", ({ method, params }) => {
      if (method === "model/reported") say("model", {requested: params.requested, reported: params.reported});
      if(method==="turn/completed" && params.turn?.error) {
        const error=params.turn.error;
        say("diagnostic",{operation:"inference",message:error.message});
        say("notice",{scope:"inference",text:error.network ? "Connection interrupted. Your saved changes are safe. Send a message to continue." : error.message});
        params={...params,turn:{...params.turn,error:undefined}};
      }
      say("bridge", { method, params });
    });
    return server;
  }

  function setModel(input) {
    if (state.busy || state.publishing || state.hostOperation) throw new Error("Wait for this turn and upload to finish before changing models.");
    if (state.provider !== "ac") {
      const choice=hostProviders.find(p=>p.id===state.provider);
      if (!choice?.available || !choice.models?.some(m=>m.id===input)) throw new Error("That model is not available from the connected host.");
      state.model=input;state.providerModels[state.provider]=input;state.server=null;
      reportProvider();saveCurrent();return input;
    }
    if (input !== DEFAULT_AC_MODEL) throw new Error("Braincell models are managed automatically.");
    state.model = DEFAULT_AC_MODEL;
    if (state.server) state.server.model = state.model;
    if (state.engine) state.engine.model = state.model;
    say("model", {requested: state.model, choices: MODEL_CHOICES});
    saveCurrent();
    return state.model;
  }

  function reportProvider() {
    const providers=[{id:"ac",available:true,models:[{id:DEFAULT_AC_MODEL,title:"Automatic"}]},
      ...["claude","codex"].map(id=>hostProviders.find(p=>p.id===id)||{id,available:false,models:[],notice:"Connect Aesel Host on your Mac."})];
    say("providers",{selected:state.provider,choices:providers,mcpAutoAllow,supportsApprovalPolicy});
    const selected=providers.find(p=>p.id===state.provider);
    say("model",{requested:state.model,choices:selected.models});
    say("hostOperation",{operation:state.hostOperation});
  }

  async function refreshProviders() {
    if (hostRPC) {
      try {
        const capabilities=await hostRPC("capabilities",{});
        hostProviders=capabilities.providers || [];
        supportsApprovalPolicy=typeof capabilities.mcpAutoAllow === "boolean";
        mcpAutoAllow=capabilities.mcpAutoAllow !== false;
      }
      catch(error) { supportsApprovalPolicy=false;hostProviders=["claude","codex"].map(id=>({id,available:false,models:[],notice:error.message})); }
    }
    reportProvider();
  }

  async function setMcpAutoAllow(value) {
    if (!hostRPC || !supportsApprovalPolicy) throw new Error("Update Aesel Host to change MCP permissions.");
    const result=await hostRPC("approvalPolicy",{mcpAutoAllow:value});
    mcpAutoAllow=result.mcpAutoAllow;
    reportProvider();
  }

  function setProvider(id) {
    if (state.busy || state.publishing || state.hostOperation) throw new Error("Finish or reconnect the current turn before changing providers.");
    const choice=hostProviders.find(p=>p.id===id);
    if (id!=="ac" && !choice?.available) throw new Error("This provider is not connected.");
    if (id===state.provider) return;
    state.providerModels[state.provider]=state.model;
    state.provider=id;state.model=id==="ac"?DEFAULT_AC_MODEL:state.providerModels[id]??choice.model??"";
    state.server?.close?.();state.server=null;state.engine=null;
    reportProvider();saveCurrent();
  }

  async function resumeTurn() {
    await verifyWorkAccount();
    if (!state.hostOperation || state.busy) return;
    state.busy=true;say("busy",{busy:true});
    try { if(!state.server)state.server=buildServer();await state.server.follow(); }
    catch(error) { say("bad",{text:error.message}); }
    finally { state.busy=false;say("busy",{busy:false});say("approval",{approval:null});saveCurrent(); }
  }

  async function respondToApproval(id, decision) {
    if (decision !== "decline") await verifyWorkAccount();
    try {
      if (!state.server?.respond) throw new Error("This approval is no longer available.");
      await state.server.respond(id,decision);
    } finally { say("approval",{approval:null}); }
    if (decision === "always") await refreshProviders();
  }

  async function ask(text) {
    requireCurrentRevision();
    const command = text.trim().match(/^\/model(?:\s+(.+))?$/i);
    if (command) {
      requireAccountReady();
      if (command[1]) setModel(command[1]);
      else say("note", {text:"Braincell models are managed automatically."});
      return;
    }
    if (!text.trim() || state.busy) return;
    if (state.hostOperation) throw new Error("Reconnect to resolve the previous host turn before sending another request.");
    state.busy = true;
    say("busy", {busy:true});
    try {
      await verifyWorkAccount();
      if (state.provider !== "ac" && !hostProviders.some(p => p.id === state.provider && p.available)) {
        throw new Error("This provider is not connected. Open Settings to check its availability.");
      }
      if (!state.title || state.title === state.slug) state.title = text.trim().slice(0, 120);
      say("notice", {scope:"inference",text:""});
      say("you", {text});
      if (!state.server) state.server = buildServer();
      await state.server.startTurn(text);
    } catch (error) {
      say("bad", {text:error.message});
      say("status", {text:"failed",kind:"failed"});
    } finally {
      state.busy = false;
      say("busy", {busy:false});
      say("approval", {approval:null});
      saveCurrent();
      if (state.accountVerified && state.handle) void credits.refresh();
    }
  }

  function stop() {
    Promise.resolve(state.server?.interrupt?.()).catch(error=>say("bad",{text:error.message}));
    state.server?.controller?.abort?.();
  }

  // Two calls, because a token is worth nothing here without the @handle that
  // says where a piece goes.
  async function resolveHandle(token) {
    return (await verifyAccount(token, {fetch:accountFetch})).handle;
  }

  function requireAccountReady() {
    if (!state.accountVerified || !state.token || !state.handle) {
      throw new Error("Sign in to Aesthetic Computer and claim an @handle to use Aesel.");
    }
  }

  async function verifyWorkAccount() {
    const epoch = accountEpoch, token = state.token;
    try {
      const account = await verifyAccount(token, {fetch:accountFetch});
      if (epoch !== accountEpoch || token !== state.token) throw new Error("Your AC account changed. Sign in again.");
      state.accountVerified = true;
      state.handle = account.handle;
      write({handle:account.handle});
      say("signedIn", {handle:account.handle});
      requireHandle(account);
      return account;
    } catch (error) {
      if (epoch === accountEpoch) {
        state.accountVerified = false;
        say("accountRequired", {signedIn:error.code === "no-handle", text:error.message});
        stop();
      }
      throw error;
    }
  }

  async function loadHandleColors(handle) {
    let colors = handleCharacterColors(`@${handle}`);
    try { colors = await fetchHandleColors(`@${handle}`); } catch { }
    if (state.handle !== handle) return;
    say("handleColors", {handle, colors: colors.map(rgb => "#" + rgb.map(n => n.toString(16).padStart(2, "0")).join(""))});
  }

  async function adoptToken(token) {
    const epoch = ++accountEpoch;
    const handle = await resolveHandle(token);
    if (epoch !== accountEpoch) return "";
    state.accountVerified = true;
    state.token = token;
    state.handle = handle;
    write({ token, handle });
    say("signedIn", { handle });
    void loadHandleColors(handle);
    void credits.refresh();
    await prepublishBlank();
    return handle;
  }

  // Resolves to true when a stored session was still good.
  async function restore() {
    const saved = read();
    const epoch = ++accountEpoch;
    if (!saved.token) return false;
    state.accountVerified = false;
    state.token = saved.token;
    state.handle = "";
    try {
      const handle = await resolveHandle(saved.token);
      if (epoch !== accountEpoch) return false;
      state.accountVerified = true;
      state.handle = handle;
      write({ handle: state.handle });
      say("signedIn", { handle: state.handle });
      void loadHandleColors(state.handle);
      void credits.refresh();
      await prepublishBlank();
      return true;
    } catch (error) {
      if (epoch !== accountEpoch) return false;
      if (error.status !== 401 && error.status !== 403) {
        // A connection failure is not evidence that the saved login expired.
        state.accountVerified = false;
        say("accountRequired", {signedIn:false, text:"Connect to verify your AC account, then try again."});
        return false;
      }
      state.token = "";
      state.handle = "";
      write({ token: "", handle: "" });
      say("signedOut");
      credits.clear();
      return false;
    }
  }

  function signOut() {
    accountEpoch++;
    saveCurrent();
    stop();
    state.accountVerified = false;
    state.token = "";
    state.handle = "";
    state.server = null;
    write({ token: "", handle: "" });
    say("signedOut");
    credits.clear();
  }

  async function open() {
    const saved = read();
    const prior = readThreads().find(item => item.id === saved.threadID);
    if (prior) {
      loadThread(prior);
    } else {
      const slug = saved.slug || freshSlug();
      loadThread({id: `${Date.now()}-${slug}`, slug, source: saved.source || STARTER,
        published: Boolean(saved.published), handle: saved.handle || state.handle,
        medium: "piece", events: []});
      saveCurrent();
    }
    emit({type: "history", items: history()});
    await prepublishBlank();
  }

  async function begin() {
    vfs.setWriteHandler(onWritten);
    const missing = await vfs.preload(GUIDES);
    if (missing.length) say("note", { text: `Guides missing: ${missing.join(", ")}` });
    say("model", { requested: state.model, choices: MODEL_CHOICES });
    say("ready", { signedIn: Boolean(state.token) });
    void refreshProviders();
  }

  return {
    state,
    begin,
    restore,
    signOut,
    adoptToken,
    open,
    ask,
    stop,
    publish,
    exportNotebook: (...args) => { requireAccountReady(); return exportNotebook(...args); },
    importNotebook: (...args) => { requireAccountReady(); return importNotebook(...args); },
    editSource: (...args) => { requireAccountReady(); return editSource(...args); },
    restoreRevision: (...args) => { requireAccountReady(); return restoreRevision(...args); },
    selectRevision: (...args) => { requireAccountReady(); return selectRevision(...args); },
    previewRevision: (...args) => { requireAccountReady(); return previewRevision(...args); },
    setAutoPublish: (...args) => { requireAccountReady(); return setAutoPublish(...args); },
    newPiece: (...args) => { requireAccountReady(); return newSession(...args); },
    newSession: (...args) => { requireAccountReady(); return newSession(...args); },
    resumeSession: (...args) => { requireAccountReady(); return resumeSession(...args); },
    saveCurrent,
    setDraft: (...args) => { requireAccountReady(); return setDraft(...args); },
    setModel: (...args) => { requireAccountReady(); return setModel(...args); },
    setProvider: (...args) => { requireAccountReady(); return setProvider(...args); },
    refreshProviders,
    setMcpAutoAllow: (...args) => { requireAccountReady(); return setMcpAutoAllow(...args); },
    resumeTurn,
    respondToApproval,
    history,
    refreshCredits: credits.refresh,
    buyCredits: credits.buy,
    checkCheckout: credits.check,
    route,
    pieceUrl,
  };
}
