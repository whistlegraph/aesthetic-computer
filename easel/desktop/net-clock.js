// Net clock — the shared musical time Aesel plays against.
//
// Mirrors the AC disk api (`clock.offset / resync / time` in lib/disk.mjs): one
// round trip to /api/clock, the server's moment taken as the midpoint of the
// request, and the offset eased a quarter of the way on every resync after the
// first. Every machine that syncs holds the same virtual UTC, so a beat index
// counted from the epoch is the same beat everywhere — that is how `clock.mjs`
// makes separate devices land together: `floor(time / interval)`, no local
// start moment involved.
//
// Pure. The fetch and the Date are injected so the math runs in Electron's
// main process, in the renderer, and in node tests with a fake of each.
(function(scope){
 const BLEND=.25;
 // One round trip. `now` is the caller's Date.now so the sample and the clock
 // that adopts it measure the same local clock.
 async function sample({fetch,site='https://aesthetic.computer',now=Date.now,headers}={}){
  const t0=now();
  const response=await fetch(`${site}/api/clock`,{headers});
  if(!response.ok)throw new Error(`clock: ${response.status}`);
  const serverTime=new Date((await response.text()).trim()).getTime();
  const t1=now();
  if(!Number.isFinite(serverTime))throw new Error('clock: unreadable server time');
  return {t0,t1,serverTime};
 }
 // The server answered halfway through the round trip.
 const estimate=({t0,t1,serverTime})=>serverTime-(t0+(t1-t0)/2);
 function createClock({sample:take,now=Date.now}={}){
  let offset=0,synced=0,rtt=0,fetching=null;
  const clock={
   get offset(){return offset;},get synced(){return synced;},get rtt(){return rtt;},
   // The first sample is taken whole; later ones ease in so one slow round
   // trip cannot yank the beat.
   resync(){
    if(fetching||!take)return fetching||Promise.resolve(offset);
    fetching=take().then(s=>{rtt=s.t1-s.t0;const target=estimate(s);offset+=(target-offset)*(synced?BLEND:1);synced++;return offset;}).finally(()=>{fetching=null;});
    return fetching;
   },
   // Take an offset another process already measured on this same machine.
   adopt(value){if(Number.isFinite(value)){offset=value;synced++;}},
   time:()=>now()+offset,
   toJSON:()=>({offset,synced,rtt}),
  };
  return clock;
 }
 const beatMs=bpm=>60000/bpm;
 // Which beat the shared moment `t` sits on, counted from the UTC epoch.
 function beat(t,bpm){const period=beatMs(bpm),index=Math.floor(t/period),start=index*period;return {index,period,start,phase:(t-start)/period,next:start+period};}
 // A loop of `beats` beats starts on every beat whose index is a multiple of
 // its length, so equal scores on different machines wrap on the same beat.
 function loop(t,{bpm,beats}){const period=beatMs(bpm)*beats,index=Math.floor(t/period),start=index*period;return {index,period,start,phase:(t-start)/period,next:start+period,beat:Math.floor((t-start)/beatMs(bpm))};}
 const nextBeat=(t,bpm)=>beat(t,bpm).next;
 const api={BLEND,sample,estimate,createClock,beatMs,beat,loop,nextBeat};
 if(typeof module!=='undefined'&&module.exports)module.exports=api;else scope.NetClock=api;
})(typeof window==='undefined'?globalThis:window);
