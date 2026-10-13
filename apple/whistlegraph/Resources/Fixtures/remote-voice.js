// Local transport fixture: real voice bridge, engine, ledger adoption and native state.
(() => {
  window.__whistlegraphDisableThread = false;
  window.__whistlegraphAIConsent = {creation:true,handle:'fixture'};
  localStorage.setItem('whistlegraph-remote-turns','on');
  localStorage.setItem('whistlegraph-source-thread',JSON.stringify({id:'00000000-0000-4000-8000-000000000012',code:'wgFixture'}));
  localStorage.setItem('whistlegraph-source','export function paint({wipe}) {wipe("navy");}');
  // The HTTP completion path also works when the thread socket is offline.
  window.WebSocket = class extends EventTarget { readyState=0; send() {} close() {} };
  const original = window.fetch.bind(window);
  let turn=0, outcome='done', finished=false;
  window.__remoteVoiceOutcome = 'done';
  window.fetch = async (input,init={}) => {
    const url=String(input);
    if(url.includes('/userinfo'))return Response.json({sub:'fixture-maker'});
    if(url.includes('/handle?for='))return Response.json({handle:'fixture'});
    if(url.includes('/api/handle-colors'))return Response.json({colors:[]});
    if(url.includes('/api/easel-credits'))return Response.json({remaining:100000,used:0,limit:100000,purchased:0});
    if(url.includes('/api/whistlegraph-turn')) {
      if(init.method==='POST') {
        turn++;outcome=window.__remoteVoiceOutcome;finished=false;
        return outcome==='rejected'?Response.json({error:'Fixture rejected'},{status:503}):Response.json({id:'fixture-'+turn},{status:202});
      }
      if(init.method==='DELETE')return Response.json({ok:true});
      if(url.includes('?code='))return Response.json({turns:[]});
      if(outcome==='stalled')return new Promise(resolve=>window.__releaseRemotePoll=()=>resolve(Response.json({status:'failed',result:{error:'Late result'}})));
      if(outcome==='waiting')return Response.json({status:'running'});
      finished=true;
      return Response.json({status:outcome,result:{versionID:turn,acceptance:'reviewed',error:'Fixture failure'}});
    }
    if(url.includes('/api/whistlegraph?code=')) {
      const ledger=JSON.parse(localStorage.getItem('whistlegraph-source-versions'));
      if(finished&&outcome==='done') {
        ledger.versions.push({id:turn,parent:ledger.head,source:'export function paint({wipe}) {wipe("#64a0a9");}',request:'Add three people',createdAt:new Date().toISOString(),layers:1});ledger.head=turn;
      }
      return Response.json({revision:turn,ledger});
    }
    if(url.includes('/api/whistlegraph'))return Response.json({threads:[]});
    if(url.includes('/api/easel-inference')||url.includes('/api/easel-musical-jev'))throw Error('Fixture must not buy inference');
    return original(input,init);
  };
})();
