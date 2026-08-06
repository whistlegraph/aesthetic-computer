import http from "node:http";

export const EMPTY_DIRECTOR_STATE = Object.freeze({
  schema:"captutor-director-state/v1",
  goal:"Waiting for Panda",
  phase:"idle",
  status:"idle",
  beatIndex:null,
  beatCount:0,
  currentLine:"",
  nextLine:"",
  words:[],
  beatStartedAt:null,
  updatedAt:new Date(0).toISOString(),
});

export function directorMonitorHtml() {
  return `<!doctype html>
<html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Captutor Director</title>
<style>
  :root { color-scheme:dark; font-family:-apple-system,BlinkMacSystemFont,"SF Pro Display",sans-serif; }
  * { box-sizing:border-box; }
  body { margin:0; min-height:100vh; overflow:hidden; background:#080808; color:#f4f4f2; }
  main { min-height:100vh; display:grid; grid-template-rows:auto 1fr auto; padding:clamp(28px,5vw,76px); gap:clamp(28px,5vh,64px); }
  header { display:flex; justify-content:space-between; align-items:flex-start; gap:32px; }
  #goal { max-width:78%; font-size:clamp(26px,3.3vw,54px); line-height:1.02; letter-spacing:-.035em; font-weight:750; }
  #phase { border:1px solid #4a4a47; border-radius:999px; padding:10px 16px; font-size:clamp(13px,1.2vw,19px); letter-spacing:.08em; text-transform:uppercase; white-space:nowrap; }
  #phase.recording { color:#ffb6d6; border-color:#b52d6d; box-shadow:0 0 28px #b52d6d55; }
  section { align-self:center; }
  #progress { color:#94948f; font-size:clamp(16px,1.7vw,25px); margin-bottom:20px; }
  #line { max-width:1200px; font-size:clamp(42px,6.8vw,108px); line-height:1.04; letter-spacing:-.045em; font-weight:680; }
  #line span { color:#686864; transition:color 80ms linear, text-shadow 80ms linear; }
  #line span.spoken { color:#d6d6d0; }
  #line span.active { color:#fff; text-shadow:0 0 24px #ff4fa877; }
  footer { display:grid; grid-template-columns:1fr auto; align-items:end; gap:30px; border-top:1px solid #2c2c2a; padding-top:24px; }
  #nextLabel { color:#777772; font-size:clamp(13px,1.2vw,18px); text-transform:uppercase; letter-spacing:.1em; margin-bottom:8px; }
  #next { color:#b6b6b0; font-size:clamp(20px,2.2vw,34px); line-height:1.18; max-width:1000px; }
  #toward { font-size:clamp(18px,2vw,30px); color:#858580; white-space:nowrap; }
  #stale { position:fixed; inset:auto 28px 22px auto; color:#ffb454; font-size:14px; opacity:0; }
  #stale.visible { opacity:1; }
</style></head>
<body><main>
  <header><div id="goal">Waiting for Panda</div><div id="phase">idle</div></header>
  <section><div id="progress">No active take</div><div id="line">Director channel ready.</div></section>
  <footer><div><div id="nextLabel">Next</div><div id="next">The current mission will appear here.</div></div><div id="toward">PANDA LIVE</div></footer>
</main><div id="stale">Panda feed paused</div>
<script>
let state=${JSON.stringify(EMPTY_DIRECTOR_STATE)};
const $=id=>document.getElementById(id);
function escapeHtml(value){return String(value||'').replace(/[&<>"']/g,ch=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[ch]));}
function renderStatic(){
  $('goal').textContent=state.goal||'Captutor mission';
  $('phase').textContent=state.phase||state.status||'working';
  $('phase').className=state.status==='recording'?'recording':'';
  $('progress').textContent=Number.isInteger(state.beatIndex)?('Beat '+(state.beatIndex+1)+' of '+state.beatCount):state.format?((state.locale||'en')+' · '+state.format):'No active take';
  $('next').textContent=state.nextLine||state.currentAction||'Awaiting the next beat.';
}
function renderWords(){
  const words=Array.isArray(state.words)?state.words:[];
  if(!words.length){$('line').textContent=state.currentLine||'Director channel ready.';return;}
  const elapsed=state.beatStartedAt?Date.now()-Date.parse(state.beatStartedAt):-1;
  $('line').innerHTML=words.map(word=>{
    const cls=elapsed>=word.toMs?'spoken':elapsed>=word.fromMs?'active':'';
    return '<span class="'+cls+'">'+escapeHtml(word.text)+'</span>';
  }).join(' ');
}
function receive(next){state=next||state;renderStatic();renderWords();}
const events=new EventSource('/events');
events.onmessage=event=>{try{receive(JSON.parse(event.data));}catch{}};
setInterval(()=>{
  renderWords();
  const age=Date.now()-Date.parse(state.updatedAt||0);
  $('stale').className=age>45000&&state.status!=='complete'?'visible':'';
},80);
fetch('/state').then(r=>r.json()).then(receive).catch(()=>{});
</script></body></html>`;
}

export function createDirectorMonitor({
  initialState = EMPTY_DIRECTOR_STATE,
  token = process.env.CAPTUTOR_DIRECTOR_TOKEN || "",
} = {}) {
  let state = { ...initialState };
  const clients = new Set();
  const broadcast = () => {
    const line = `data: ${JSON.stringify(state)}\n\n`;
    for (const response of clients) response.write(line);
  };
  const server = http.createServer((request, response) => {
    const url = new URL(request.url || "/", "http://director.local");
    if (request.method === "GET" && url.pathname === "/") {
      response.writeHead(200, { "Content-Type":"text/html; charset=utf-8", "Cache-Control":"no-store" });
      response.end(directorMonitorHtml());
      return;
    }
    if (request.method === "GET" && url.pathname === "/state") {
      response.writeHead(200, { "Content-Type":"application/json", "Cache-Control":"no-store" });
      response.end(JSON.stringify(state));
      return;
    }
    if (request.method === "GET" && url.pathname === "/events") {
      response.writeHead(200, { "Content-Type":"text/event-stream", "Cache-Control":"no-store", Connection:"keep-alive" });
      response.write(`data: ${JSON.stringify(state)}\n\n`);
      clients.add(response);
      request.on("close", () => clients.delete(response));
      return;
    }
    if (request.method === "POST" && url.pathname === "/state") {
      if (token && request.headers.authorization !== `Bearer ${token}`) {
        response.writeHead(401, { "Content-Type":"application/json" }).end('{"error":"unauthorized"}');
        return;
      }
      let body = "";
      request.on("data", (chunk) => {
        body += chunk;
        if (body.length > 128 * 1024) request.destroy();
      });
      request.on("end", () => {
        try {
          const next = JSON.parse(body);
          if (next?.schema !== "captutor-director-state/v1") throw new Error("invalid schema");
          state = next;
          broadcast();
          response.writeHead(204).end();
        } catch {
          response.writeHead(400, { "Content-Type":"application/json" }).end('{"error":"invalid director state"}');
        }
      });
      return;
    }
    response.writeHead(404).end();
  });
  return { server, getState:() => state };
}
