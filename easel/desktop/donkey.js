// Aesel: a status companion. No animation-frame loop or remote resources.
(function(scope){
  const ANIMATIONS={idle:{frames:[0,1],durations:[1400,180]},awake:{frames:[2,3],durations:[500,500]},sleeping:{frames:[4,5,6,7],durations:[900,900,900,900]},working:{frames:[8,9,10,11],durations:[180,180,180,180]},running:{frames:[12,13,14,15],durations:[120,120,120,120]}};
  function statusPhase(status){
    if(/working|generating|streaming|writing|rendering|thinking|tool|benchmark|publishing/.test(status))return 'working';
    if(/starting|connecting|queued|opening|loading/.test(status))return 'running';
    if(/awaiting|approval|error|failed|offline|interrupt/.test(status))return 'awake';
    return 'ready';
  }
  function phaseFor({status='ready',now,lastActivity,awakeUntil=0}){
    const active=statusPhase(status);if(active!=='ready')return active;
    if(now<awakeUntil)return 'awake';
    return now-lastActivity>=60000?'sleeping':'idle';
  }
  function animationFrame(phase,elapsed,reduced=false,animations=ANIMATIONS){
    const animation=animations[phase]||ANIMATIONS.idle;
    if(reduced)return {frame:animation.frames[0],delay:null};
    const total=animation.durations.reduce((sum,value)=>sum+value,0);let offset=Math.max(0,elapsed)%total;
    for(let i=0;i<animation.frames.length;i++){if(offset<animation.durations[i])return {frame:animation.frames[i],delay:animation.durations[i]-offset};offset-=animation.durations[i];}
    return {frame:animation.frames[0],delay:animation.durations[0]};
  }
  function manifestAnimations(value){
    const source=value?.animations||value?.states||value,result={};
    for(const name of Object.keys(ANIMATIONS)){
      const a=source?.[name];if(!a||!Array.isArray(a.frames)||!Array.isArray(a.durations)||!a.frames.length||a.frames.length!==a.durations.length||a.frames.some(n=>!Number.isInteger(n)||n<0||n>15)||a.durations.some(n=>!Number.isFinite(n)||n<80||n>10000))return ANIMATIONS;
      result[name]={frames:a.frames,durations:a.durations};
    }return result;
  }
  function createDonkey({canvas,document:doc=scope.document,clock=()=>Date.now()}={}){
    if(!canvas)return {update(){},destroy(){}};
    const context=canvas.getContext('2d');if(!context)return {update(){},destroy(){}};
    const media=scope.matchMedia('(prefers-reduced-motion: reduce)'),image=new scope.Image();
    let animations=ANIMATIONS,loaded=false,destroyed=false,timer=null,status='ready',lastActivity=clock(),awakeUntil=lastActivity+2500,phase=null,phaseStarted=lastActivity,lastFrame=-1;
    canvas.width=canvas.height=64;context.imageSmoothingEnabled=false;
    function clear(){if(timer!==null)scope.clearTimeout(timer);timer=null;}
    function tick(){
      clear();if(destroyed||doc.hidden)return;
      const now=clock(),nextPhase=phaseFor({status,now,lastActivity,awakeUntil});
      if(nextPhase!==phase){phase=nextPhase;phaseStarted=now;canvas.dataset.state=phase;canvas.setAttribute('aria-label',`Aesel the donkey: ${phase}`);}
      const frame=animationFrame(phase,now-phaseStarted,media.matches,animations);
      if(loaded&&frame.frame!==lastFrame){context.clearRect(0,0,64,64);context.drawImage(image,(frame.frame%4)*64,Math.floor(frame.frame/4)*64,64,64,0,0,64,64);lastFrame=frame.frame;}
      let delay=frame.delay;
      if(statusPhase(status)==='ready'){
        const deadline=now<awakeUntil?awakeUntil:lastActivity+60000;
        if(deadline>now)delay=delay===null?deadline-now:Math.min(delay,deadline-now);
      }
      if(delay!==null)timer=scope.setTimeout(tick,Math.max(1,delay));
    }
    function wake(){lastActivity=clock();awakeUntil=lastActivity+2000;if(phaseFor({status,now:lastActivity,lastActivity,awakeUntil})!==phase)tick();}
    function visibility(){clear();if(!doc.hidden){phaseStarted=clock();tick();}}
    image.onload=()=>{loaded=image.naturalWidth===256&&image.naturalHeight===256;canvas.hidden=!loaded;if(loaded){lastFrame=-1;tick();}};
    image.onerror=()=>{canvas.hidden=true;};image.src='assets/aesel.png';
    scope.fetch('assets/aesel.json').then(r=>r.ok?r.json():null).then(value=>{animations=manifestAnimations(value);tick();}).catch(()=>{});
    doc.addEventListener('pointermove',wake,{passive:true});doc.addEventListener('pointerdown',wake,{passive:true});doc.addEventListener('keydown',wake);doc.addEventListener('visibilitychange',visibility);media.addEventListener('change',tick);tick();
    return {
      update(state={}){const next=String(state.status||'ready').toLowerCase();if(statusPhase(next)!==statusPhase(status)){if(statusPhase(next)==='ready'){lastActivity=clock();awakeUntil=lastActivity+2500;}status=next;tick();}else status=next;},
      destroy(){destroyed=true;clear();doc.removeEventListener('pointermove',wake);doc.removeEventListener('pointerdown',wake);doc.removeEventListener('keydown',wake);doc.removeEventListener('visibilitychange',visibility);media.removeEventListener('change',tick);},
    };
  }
  const api={ANIMATIONS,statusPhase,phaseFor,animationFrame,manifestAnimations,createDonkey};
  if(typeof module!=='undefined'&&module.exports)module.exports=api;else scope.AeselDonkey=api;
})(typeof window==='undefined'?globalThis:window);
