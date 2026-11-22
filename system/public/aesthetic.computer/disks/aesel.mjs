// The AC prompt edition of Aesel shares the hosted agent loop and write_piece
// tool with AC OS. Each visit owns its working file and conversation.
import { AcBridge } from "../lib/aesel-os/aesel-ac.mjs";
import { randomSlug, wrapRows } from "../lib/aesel-os/aesel-bridge.mjs";

let desk;

function transport() {
  let request, result;
  return {
    start(body, headers) {
      this.cancel();
      const controller = new AbortController();
      request = controller;
      result = { text: "", done: false };
      const output = result;
      (async () => {
        try {
          const response = await fetch("https://aesthetic.computer/api/easel-inference", {
            method: "POST", headers: { ...headers, "Content-Type": "application/json" },
            body, signal: controller.signal,
          });
          if (!response.ok) {
            const failure = await response.json().catch(() => ({}));
            throw Error(failure.error?.message || failure.message || "Aesel returned HTTP " + response.status);
          }
          const reader = response.body.getReader(), decoder = new TextDecoder();
          for (;;) {
            const { done, value } = await reader.read();
            if (done) break;
            output.text += decoder.decode(value, { stream: true });
          }
          output.text += decoder.decode();
        } catch (error) { output.error = error.message; }
        finally { output.done = true; }
      })();
      return true;
    },
    poll: () => result,
    cancel() { request?.abort(); request = null; },
  };
}

function boot(api) {
  const name = randomSlug() + ".mjs";
  const state = desk = { source: "", name, lines: [], busy: false, turn: 0, status: "Ready", offset: 0 };
  api.hud.label("Aesel TUI");
  state.bridge = new AcBridge({
    transport: transport(), file: name, readPiece: () => state.source,
    writePiece(source) { state.source = source; return true; },
    instructions: "You are Aesel, making a JavaScript piece for the Aesthetic Computer web runtime. Use write_piece for every revision. Send the complete module with exported paint($) and optionally boot, act or sim. Use $.wipe, $.ink, $.box and $.line. Aesthetic Computer runs the module as a piece. The user can save the finished .mjs file.",
  });
  state.bridge.launch();
  state.fields = new api.ui.TextFields(api, [{name:"line", label:">", placeholder:"Describe a piece…"}], ({line}) => submit(api, state, line));
  state.fields.input.mute = false;
  state.status = api.user?.sub ? "Describe a piece, then press Enter." : "Sign in with AC to start a thread.";
}

async function submit(api, state, line) {
  const text = line.trim();
  if (!text || state.busy || !api.user?.sub) return;
  const turn = ++state.turn;
  state.busy = true;
  state.status = "Connecting…";
  try {
    const token = await api.authorize();
    if (desk !== state || state.turn !== turn) return;
    if (!token) throw Error("Sign in with AC, then reopen Aesel.");
    state.bridge.token = token;
    state.bridge.handshake();
    state.fields.reset(api);
    state.lines.push("> " + text);
    state.offset = 0;
    state.reply = null;
    state.bridge.say(text);
    state.status = "Aesel is working…";
  } catch (error) { if (desk === state && state.turn === turn) { state.status = error.message; state.busy = false; } }
}

function sim() {
  if (!desk) return;
  const s = desk;
  s.bridge.tick();
  for (const e of s.bridge.take()) {
    if (e.type === "delta") {
      if (s.reply === null) { s.reply = s.lines.length; s.lines.push(""); }
      s.lines[s.reply] += e.text;
    }
    if (e.type === "tool" && e.status === "completed") s.lines.push("Wrote " + s.name);
    if (e.type === "turn" && e.status !== "started") {
      s.busy = false;
      s.bridge.token = "";
      s.status = e.error || (e.status === "interrupted" ? "Stopped." : "Ready");
    }
  }
}

function paint(api) {
  if (!desk) return;
  const {screen, ink, wipe} = api, s = desk;
  wipe(37,25,48);
  const cols = Math.max(8,Math.floor((screen.width-10)/6)), visible = Math.max(1,Math.floor((screen.height-65)/10));
  const rows = s.lines.flatMap(line=>wrapRows(line,cols).map(row=>row.text));
  s.offset = Math.min(s.offset,Math.max(0,rows.length-visible));
  rows.slice(Math.max(0,rows.length-visible-s.offset),rows.length-s.offset).forEach((line,i)=>{
    ink(line.startsWith("> ")?"pink":"white").write(line,{x:5,y:15+i*10});
  });
  ink("wheat").write(s.status.slice(0,cols),{x:5,y:screen.height-45});
  s.fields.paint(api,{x:2,y:screen.height-32,width:screen.width-4,height:16});
  s.actions = [api.user?.sub ? (s.busy?"Stop":"Send") : "Login", ...(s.source?["Save .mjs"]:[]), ...(rows.length>visible?["Older","Newer"]:[])];
  let x=4;
  s.hits=s.actions.map(label=>{
    const width=6*(label.length+2)+4;
    ink("plum").write("["+label+"]",{x,y:screen.height-12});
    const hit={label,x,width}; x+=width; return hit;
  });
}

function act(api) {
  const s=desk,e=api.event;
  if(!s)return;
  if(e.is("touch")&&e.y>=api.screen.height-15){
    const hit=s.hits?.find(h=>e.x>=h.x&&e.x<h.x+h.width);
    if(hit?.label==="Login")api.jump("prompt~login~!autorun");
    if(hit?.label==="Send")s.fields.advance(api);
    if(hit?.label==="Stop"){s.turn++;s.bridge.interrupt();s.bridge.token="";s.busy=false;s.status="Stopped.";}
    if(hit?.label==="Save .mjs")api.download(s.name,s.source,{type:"text/javascript"});
    if(hit?.label==="Older")s.offset+=5;
    if(hit?.label==="Newer")s.offset=Math.max(0,s.offset-5);
    return;
  }
  if(api.user?.sub)s.fields.act(api);
}

function leave() {
  if(desk){desk.bridge.interrupt();desk.bridge.token="";desk=null;}
}
function meta(){return {title:"Aesel TUI",desc:"Make a piece with Aesel in the AC prompt."};}
export {boot,sim,paint,act,leave,meta};
