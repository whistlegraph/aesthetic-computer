// Menu Band's modifier grammar. Command/Option are sided; Control alters both.
export function chordIntervals({meta=false,alt=false,ctrl=false}={}) {
  if(meta&&alt) return ctrl?[5,7]:[2,7];
  if(ctrl) return alt?[3,6]:[4,8];
  return alt?[3,7]:meta?[4,7]:[];
}

// Keep the root and common intervals alive during a modifier change.
export function createChordVoice({root,start,intervals=[],onRelease=()=>{}}) {
  const voices=new Map([[0,start(root)]]);
  let released=false;
  const group={
    get notes(){return [...voices.keys()].map(n=>root+n)},
    get startedAt(){return voices.get(0)?.startedAt},
    morph(next){
      if(released)return;
      const wanted=new Set([0,...next.filter(n=>root+n>=0&&root+n<=127)]);
      for(const [n,v] of voices) if(!wanted.has(n)){v?.kill?.(.02);voices.delete(n)}
      for(const n of wanted) if(!voices.has(n)) voices.set(n,start(root+n));
    },
    kill(fade){
      if(!released){released=true;onRelease()}
      for(const voice of voices.values()) voice?.kill?.(fade);
    },
    update(value){for(const [interval,voice] of voices) voice?.update?.(value,interval)},
  };
  group.morph(intervals);
  return group;
}

// Scope physical-key normalization to the musical canvas: Option otherwise
// produces symbols, and the browser's Command shortcuts swallow note releases.
export function createNotepatKeyRoute() {
  const held=new Map();
  const names={Semicolon:';',Quote:"'",BracketLeft:'[',BracketRight:']',
    MetaLeft:'Meta',MetaRight:'Meta',AltLeft:'Alt',AltRight:'Alt',
    ControlLeft:'Control',ControlRight:'Control',ShiftLeft:'Shift',ShiftRight:'Shift',CapsLock:'CapsLock'};
  return (e,down,active,emit)=>{
    const key=/^Key[A-Z]$/.test(e.code)?e.code.slice(3).toLowerCase():names[e.code];
    if(!key || (!active&&(down||!held.has(e.code))))return false;
    if(down && e.code?.startsWith('Bracket') && (e.metaKey||e.altKey||e.ctrlKey))return false;
    if(down)held.set(e.code,key);else held.delete(e.code);
    e.preventDefault();
    emit({name:`keyboard:${down?'down':'up'}:${key.toLowerCase()}`,key,code:e.code,
      repeat:!!e.repeat,shift:!!e.shiftKey,alt:!!e.altKey,ctrl:!!e.ctrlKey,meta:!!e.metaKey,
      caps:e.getModifierState?.('CapsLock'),velocity:e.velocity??127,notepatMusical:true});
    return true;
  };
}
