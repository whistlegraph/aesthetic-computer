// Native live speech and the shared Aesel generation stream.
(() => {
  if (!window.webkit?.messageHandlers?.whistlegraph) return;
  document.title = 'Whistlegraph';
  $('home').innerHTML = 'whistlegraph<i></i>';
  $('home').setAttribute('aria-label', 'Whistlegraph');
  $('home').onclick = () => $('info').showModal();
  window.addEventListener('error', e => { if(e.message) toast('Could not load generation: '+e.message); });
  document.querySelector('#initial .mark').innerHTML = '<svg width="72" height="72" viewBox="0 0 72 72" aria-hidden="true"><g stroke="currentColor" stroke-width="8" stroke-linecap="round"><path d="M36 8v56M12 22l48 28M12 50l48-28"/></g></svg>';
  const style = document.createElement('style');
  style.textContent = '.whistlegraph .top .brand{font-size:32px;letter-spacing:-1.5px}.whistlegraph .top .piece-name,.whistlegraph .top .divider{display:none}.whistlegraph .top{padding:0 21px}.voice-note{font-size:11px;margin-top:13px}.voice-note button{background:none;color:#d9ef9f;padding:5px 9px;font-size:12px;text-decoration:underline}.whistlegraph .whistlegraph-dock{bottom:30px}.whistlegraph .play-deck{bottom:220px}.whistlegraph .conversation{bottom:315px}';
  style.textContent += `
    #whistlegraph-thread{position:absolute;top:62px;left:21px;right:21px;z-index:7;font:24px/1.2 Comic,Arial;color:#d9ef9f;pointer-events:none}
    .whistlegraph #live-piece{top:100px;height:max(120px,calc(100% - 600px))}
    html,body,body *{-webkit-user-select:none;user-select:none;-webkit-touch-callout:none}
    input,textarea,input *,textarea *{-webkit-user-select:text;user-select:text}
    button{touch-action:manipulation}#speak{touch-action:none}

    .whistlegraph .voice-state{background:#19172e;z-index:6}
    .whistlegraph .voice-state .wave-bars{height:24px;transform:scale(.5);flex-shrink:0}
    .whistlegraph .voice-state h2{font:16px/1.3 Comic,Arial;color:#f7b474;margin:16px 0}
    .whistlegraph .voice-state p{font:32px/1.3 Comic,Arial;color:#f4efdd;max-width:100%;max-height:45vh;overflow:auto;overflow-wrap:anywhere;text-align:left;white-space:pre-wrap}
    #live-piece{position:absolute;inset:80px 0 415px;width:100%;height:calc(100% - 495px);border:0;visibility:hidden;background:#19172e;z-index:2}
    .live-preview #live-piece{visibility:visible}.live-mode #initial,.live-mode #art,.live-mode .stage-top,.live-mode .composer-wrap,.live-mode #play-deck{display:none!important}
    #live-work{position:absolute;z-index:6;bottom:220px;left:21px;right:21px;padding:12px;background:#211d36f5;border-radius:8px;font:13px/1.4 Comic,Arial;max-height:255px;color:#f4efdd}
    .live-line{display:flex;align-items:center;gap:10px}.live-line strong{flex:1;color:#d9ef9f}.live-line button{background:#f7b474;color:#19172e;padding:7px 12px}
    #live-request{margin:7px 0;max-height:38px;overflow:auto;color:#eee5d1}#live-details{max-height:95px;overflow:auto;color:#e6dced}#live-code{color:#f4efdd;font:12px/1.35 monospace;white-space:pre-wrap;overflow-wrap:anywhere;max-height:90px;overflow:auto}#live-events{padding-left:18px;color:#c7bbd7;font:11px/1.4 monospace}
    #live-decisions{display:flex;gap:12px;margin-top:8px}#live-decisions button{flex:1;padding:12px;background:#d9ef9f;color:#19172e;font:22px Comic,Arial}#live-decisions button+button{background:#f59d98}
  `;
  style.textContent += `
    .whistlegraph .whistlegraph-dock{bottom:22px}.speak-button{font-size:34px;min-height:132px}
    .whistlegraph .voice-note{display:flex;gap:12px;margin-top:14px;letter-spacing:0;font-size:0}
    .whistlegraph .voice-note button{flex:1;min-height:52px;font:22px/1.2 Comic,Arial;text-decoration:none;border:2px solid #665973;border-radius:6px;padding:10px;color:#f4efdd;background:#272139}
    .whistlegraph .voice-state{bottom:225px}.whistlegraph .voice-state h2{font-size:22px}.whistlegraph .voice-state p{font-size:36px}
    #live-work{bottom:244px;max-height:260px;font-size:18px;padding:14px}
    #live-time{font-size:16px}.live-line button{font-size:18px;min-height:44px}
    #live-request{max-height:52px;font-size:19px}#live-details summary{font-size:20px;min-height:36px}
    #live-code{font-size:16px;line-height:1.35}#live-events{font:15px/1.4 Comic,Arial}
    #live-decisions button{font-size:28px;min-height:60px}
    #live-piece{bottom:500px;height:calc(100% - 580px)}
  `;
  style.textContent += `
    #version-feed{list-style:none;padding:0;margin:0;max-height:145px;overflow:auto;overscroll-behavior:contain}
    #version-feed li{display:grid;grid-template-columns:48px minmax(0,1fr);gap:4px 14px;cursor:pointer;padding:10px 0;border-bottom:1px solid #463b54;font:22px/1.3 Comic,Arial;overflow-wrap:anywhere}
    #version-feed li>span:first-child{flex:0 0 48px;color:#b7a8c9}
    #version-feed li>span+span{min-width:0}
    #version-feed li[aria-current]{color:#d9ef9f}
    #version-feed .version-parent{grid-column:2;font:18px/1.3 Comic,Arial;color:#b7a8c9}
    #version-feed time{grid-column:2;font:18px/1.3 Comic,Arial;color:#b7a8c9}
    #version-feed .utterance-sound{grid-column:2;width:100%;height:54px}
    #version-feed li:focus-visible{outline:2px solid #d9ef9f;outline-offset:-2px}
    #live-phase[hidden],#live-time[hidden],#live-details[hidden],#live-request[hidden]{display:none!important}
  `;
  style.textContent += `
    .whistlegraph #live-piece{left:50%;right:auto;bottom:auto;transform:translateX(-50%);width:min(calc(100% - 28px),calc(max(120px,100dvh - 560px) * 4 / 3));height:auto;aspect-ratio:4/3;box-sizing:content-box;border:0;border-radius:0;background:#19172e;box-shadow:none;filter:none}
  `;
  style.textContent += `
    #whistlegraph-identity{position:absolute;top:64px;right:21px;left:21px;z-index:7;display:flex;justify-content:flex-end;align-items:center;font:22px/1.2 Comic,Arial;color:#d9ef9f}
    #whistlegraph-identity #whistlegraph-thread{position:static;font:inherit;white-space:pre;pointer-events:auto}
    #whistlegraph-identity #connect-ac{font:inherit;color:inherit;background:none;border:0;padding:6px 0;min-height:32px}
    .whistlegraph .voice-note{display:none}#live-work{bottom:170px}.whistlegraph .voice-state{bottom:165px}
  `;
  style.textContent += `
    @font-face{font-family:ComicTitle;src:url('ComicRelief-Bold.ttf');font-weight:700}
    #whistlegraph-identity,.whistlegraph .top .brand{font-family:ComicTitle,Comic,Arial;font-weight:700;-webkit-text-stroke:1px #141414;paint-order:stroke fill;text-shadow:.75px .75px #ed629d,1px 1px #a478df,2px 2px #55cdd9}
    #whistlegraph-identity #whistlegraph-thread{color:#fff6e8}
    #connect-ac>span{display:inline-block}
  `;
  style.textContent += `
    .whistlegraph #live-preview-box{position:absolute;top:100px;left:50%;transform:translateX(-50%);width:min(calc(100% - 28px),calc(max(120px,100dvh - 560px) * 4 / 3));aspect-ratio:4/3;z-index:2}
    .whistlegraph #live-preview-box #live-piece{inset:0;transform:none;width:100%;height:100%;aspect-ratio:auto}
    #live-preview-box #whistlegraph-identity{top:calc(100% + 8px);bottom:auto;right:0;left:0;pointer-events:none}
    #live-preview-box #connect-ac{pointer-events:auto}
  `;
  style.textContent += `
    #live-work{position:fixed;bottom:170px;max-height:none;display:flex;flex-direction:column;min-height:0}
    #version-feed{flex:1;min-height:0;max-height:none}
    .live-line{flex-shrink:0}
  `;
  style.textContent += `
    .whistlegraph #art,.whistlegraph .stage-top,.whistlegraph .composer-wrap,.whistlegraph #play-deck,.whistlegraph #conversation,.whistlegraph .initial-form{display:none!important}
    .whistlegraph:not(.live-mode) #live-preview-box{visibility:hidden}
  `;
  if(window.__whistlegraphNativeShell){
    document.documentElement.classList.add('native-shell');
    const nativeStyle=document.createElement('style');nativeStyle.textContent=`html.native-shell body>*:not(#stage){display:none!important}html.native-shell #stage{position:fixed;inset:0}html.native-shell #stage>*:not(#live-preview-box){display:none!important}html.native-shell .whistlegraph #live-preview-box{position:absolute;inset:0;transform:none;width:100%;height:100%;aspect-ratio:auto;visibility:visible}html.native-shell #whistlegraph-identity{display:none!important}`;
    document.head.append(nativeStyle);
  }
  document.head.append(style);
  const liveStyle=document.createElement('style');liveStyle.id='whistlegraph-live-layout';document.head.append(liveStyle);
  function nativeLayout(){
    if(!window.__whistlegraphNativeShell)return;
    const computed=getComputedStyle(document.documentElement),layout={};
    for(const name of ['spacing','page-inset','history-size','talk-height','title-size']){const value=parseFloat(computed.getPropertyValue('--ww-'+name));if(Number.isFinite(value))layout[name]=value;}
    window.webkit.messageHandlers.whistlegraph.postMessage({action:'layout',id:'engine',layout});
  }
  window.whistlegraphApplyLayout=css=>{localStorage.setItem('whistlegraph-layout-css',css);liveStyle.textContent=css;nativeLayout();};
  try{liveStyle.textContent=localStorage.getItem('whistlegraph-layout-css')||(!localStorage.getItem('whistlegraph-storage-migrated')&&localStorage.getItem('walkieware-layout-css'))||'';}catch{}
  nativeLayout();
  $('speak').setAttribute('aria-label','Hold to talk, up to eight seconds');
  const note = document.querySelector('.voice-note');
  note.textContent = '';
  const blocked = () => window.whistlegraphIsBusy ? window.whistlegraphIsBusy() : gameMode === 'review';
  $('info').querySelector('h2').textContent = 'Whistlegraph';
  $('info').querySelectorAll('p')[0].textContent = 'Hold to talk. Release to make. Device speech is the default. Optional cloud speech sends audio to OpenAI only after you allow it in AI & privacy. Recordings are saved on this phone for playback. Allow Speech and Microphone access the first time, then hold again.';
  $('info').querySelectorAll('p')[1].textContent = 'Your code identifies this piece. Source, versions, and live errors sync privately to your AC account so your other devices and agents can inspect and edit it. Drawings and measured sound cues travel with your requests. Each generated edit also sends cropped, timed preview frames to AC for a visual check before saving. Raw recordings leave this phone only when cloud speech is enabled.';
  $('speak').setAttribute('aria-label', 'Hold to talk to Whistlegraph');
  $('export').hidden = true;
  $('reset').hidden = true;
  $('new').hidden = true;
  $('export').onclick = () => {
    $('more-menu').hidden = true;
    window.webkit.messageHandlers.whistlegraph.postMessage({action: 'share', id: 'image', data: canvas.toDataURL('image/png')});
  };
  let id = '', starting = false, completing = false, performanceCapture = false, voiceClock;
  window.whistlegraphLatchPerformance = () => { performanceCapture = true; clearClock(); $('speak-label').textContent='Draw + talk · Send when done'; };

  const clearClock=()=>{clearInterval(voiceClock);voiceClock=null;};
  const send = action => window.webkit.messageHandlers.whistlegraph.postMessage({action, id});
  function clearVoice() { performanceCapture = false; window.webkit.messageHandlers.whistlegraph.postMessage({action:'voiceIdle',id:'engine'}); clearClock(); talking = false; voiceBusy = false; starting = false; completing = false; $('voice-state').hidden = true; $('speak').classList.remove('holding'); document.body.classList.remove('making'); $('speak').disabled = blocked(); $('speak-label').textContent = blocked() ? 'Working…' : 'Hold to talk'; }
  voiceStart = () => {
    if (talking || voiceBusy || blocked()) return;
    window.webkit.messageHandlers.whistlegraph.postMessage({action:'account',id:'engine'});
    id = 'voice-' + Date.now(); talking = true; starting = true;
    $('speak').classList.add('holding'); $('speak-label').textContent = 'Release when done';
    $('voice-state').hidden = false; $('voice-heading').textContent = 'Opening the microphone…';
    $('voice-transcript').textContent = 'Allow access if asked, then hold again.';
    window.whistlegraphInputStart?.();
    send('start');
  };
  voiceEnd = (cancel = false) => {
    if (completing || (!talking && !voiceBusy) || (voiceBusy && !cancel)) return;
    if (cancel || starting) { window.whistlegraphInputCancel?.(); send('cancel'); id = ''; clearVoice(); return; }
    clearClock(); talking = false; voiceBusy = true; $('speak').classList.remove('holding'); $('speak').disabled = true;
    $('speak-label').textContent = 'Finishing…'; $('voice-heading').textContent = 'Finishing…';
    send('stop');
  };
  async function makeFromWords(text, sound=false, drawing=null, performance=false) {
    if (completing) return;
    clearClock(); completing = true; voiceBusy = true; $('speak').disabled = true;
    $('voice-state').hidden = true;
    $('speak-label').textContent = 'Working…';
    if (!window.whistlegraphAsk) { id = ''; clearVoice(); toast('Generation is still loading. Please try again.'); return; }
    id = '';
    try { await (sound ? window.whistlegraphAskSound({...JSON.parse(text),drawing,...(performance?{performance:{schema:"whistlegraph-performance/v1",timeline:"audio-start"}}:{})}) : window.whistlegraphAskDrawing(text,drawing)); }
    catch { clearVoice(); toast("Could not interpret this sound. Please try again."); }
  }
  window.whistlegraphRecording=()=>talking||voiceBusy||starting||completing;
  window.whistlegraphStartFixture = () => { voiceStart(); };
  window.whistlegraphWorkFinished = () => { id = ''; clearVoice(); };

  window.whistlegraphNativeEvent = event => {
    if (event.id !== id || !id) return;
    if (event.kind === 'listening') { starting = false; clearClock(); const began=performance.now(); $('speak-label').textContent='8s · Release to send'; voiceClock=setInterval(()=>{if(performanceCapture){clearClock();return;}const left=Math.max(0,8-(performance.now()-began)/1000);$('speak-label').textContent=Math.ceil(left)+'s · Release to send';if(left<=0)voiceEnd();},100); $('voice-heading').textContent = 'Listening…'; $('voice-transcript').textContent = ''; }
    if (event.kind === 'partial') { $('voice-transcript').textContent = event.text; $('voice-transcript').scrollTop = $('voice-transcript').scrollHeight; requestAnimationFrame(()=>window.webkit.messageHandlers.whistlegraph.postMessage({action:'benchmark',id:'engine',event:'transcriptPainted'})); }
    if (event.kind === 'musicalObservation') { try { window.whistlegraphObserveSound?.(JSON.parse(event.text)); } catch {} }
    if (event.kind === 'sound') { $('voice-heading').textContent = 'Listening · '+event.text; }
    if (event.kind === 'mixedFinal') { talking = false; makeFromWords(event.text, true, event.drawing, event.performance===true); }
    if (event.kind === 'final') { talking = false; makeFromWords(event.text, false, event.drawing); }
    if (event.kind === 'error') { const message = event.text; id = ''; clearVoice(); toast(message); }
  };
  $('words-form').onsubmit = event => {
    event.preventDefault(); const text = $('words').value.trim(); if (!text) return;
    $('prompt-modal').close(); makeFromWords(text);
  };
  window.webkit.messageHandlers.whistlegraph.postMessage({action:'ready',id:'startup'});
  document.addEventListener('visibilitychange', () => { if (document.hidden) voiceEnd(true); });
})();
