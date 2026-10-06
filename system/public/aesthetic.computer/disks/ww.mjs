// Whistlegraph, 26.09.30
// Open your running Whistlegraph piece by its code.
let socket,child,api,problem='Connecting…',generation=0,stopped=false;
export function boot($) {api=$;void connect($).catch(error=>problem=error.message);}
async function connect($) {
  api=$;const code=$.params[0];
  if(!/^(?:wg|ww)[a-z]{5,12}$/i.test(code||'')){problem='Enter a Whistlegraph code';return;}
  let token;try{token=await $.authorize?.();}catch{}
  if(stopped)return;
  if(!token){problem='Sign in to open your Whistlegraph piece';return;}
  socket=new WebSocket('wss://aesthetic.computer/api/whistlegraph-stream');
  socket.onopen=()=>socket.send(JSON.stringify({type:'authenticate',role:'agent',token,code}));
  let current='';
  socket.onmessage=async event=>{
    const m=JSON.parse(event.data);
    if(m.type==='error'){problem=m.error;return;}
    if(!['ready','saved'].includes(m.type))return;
    const ledger=m.thread.ledger,source=ledger?.versions.find(v=>v.id===ledger.head)?.source;
    if(!source||source===current)return;
    const turn=++generation;
    try {
      const next=await import('data:text/javascript;charset=utf-8,'+encodeURIComponent(source));
      if(stopped||turn!==generation)return;
      child?.leave?.(api);child=null;
      await next.boot?.(api);
      if(stopped||turn!==generation){next.leave?.(api);return;}
      child=next;current=source;problem='';
    }catch(error){problem=error.message;}
  };
  socket.onerror=()=>problem='Connection unavailable';
  socket.onclose=()=>{if(!stopped)problem='Disconnected · reopen this code to reconnect';};
}
export function paint($){if(child?.paint)try{return child.paint($);}catch(error){problem=error.message;child=null;}$.wipe(24,18,30);$.ink(240,230,255).write(problem,8,12);}
export function sim($){try{child?.sim?.($);}catch(error){problem=error.message;child=null;}}
export function act($){child?.act?.($);}
export function leave($){stopped=true;generation++;socket?.close();child?.leave?.($);}
