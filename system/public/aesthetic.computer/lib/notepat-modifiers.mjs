// Menu Band: sided Shift linger, Caps Lock across both octaves, bracket drum latches.
export const LINGER_SECONDS = 4;
export function createNotepatModifiers() {
  const shifts = new Set(), meta = new Set(), alt = new Set();
  let ctrl = false;
  const handed=(set,note)=>set.has("both")||set.has(note.startsWith("+")?"right":"left");
  let caps = false, left = false, right = false;
  return {
    observe(e) {
      if (!e.name?.startsWith('keyboard:')) return null;
      const down=e.name.startsWith('keyboard:down:'), key=e.key?.toLowerCase();
      for(const [name,set,flag] of [['meta',meta,e.meta],['alt',alt,e.alt]]) {
        if(key===name) {
          const side=e.code?.endsWith('Left')?'left':e.code?.endsWith('Right')?'right':'both';
          if(down)set.add(side);else if(flag===false)set.clear();else set.delete(side);
        } else if(flag===false)set.clear();else if(flag&&!set.size)set.add('both');
      }
      if(key==='control')ctrl=down;else if(typeof e.ctrl==='boolean')ctrl=e.ctrl;
      if (typeof e.caps === 'boolean') caps=e.caps;
      else if (down && key==='capslock' && !e.repeat) caps=!caps;
      if (key==='shift') {
        if (down) shifts.add(e.code || 'Shift');
        else if (e.shift===false) shifts.clear(); else if (e.code) shifts.delete(e.code); else shifts.clear();
      } else if (e.shift === false) shifts.clear();
      else if (e.shift && !shifts.size) shifts.add('Shift');
      const side=e.code==='BracketLeft'||key==='['||key==='{' ? 'left' : e.code==='BracketRight'||key===']'||key==='}' ? 'right' : null;
      if (side && !e.ctrl && !e.meta && !e.alt) {
        if (down && !e.repeat) { if(side==='left') left=!left; else right=!right; }
        return {side,changed:down&&!e.repeat};
      }
      return null;
    },
    chord(note) { return {meta:handed(meta,note),alt:handed(alt,note),ctrl}; },
    linger(note) { return caps || shifts.has('Shift') || shifts.has(note.startsWith('+')?'ShiftRight':'ShiftLeft'); },
    drums(note) { return note.startsWith('+')?right:left; },
    get state() { return {caps,left,right,shift:shifts.size>0}; },
    resetMomentary() { shifts.clear();meta.clear();alt.clear();ctrl=false; },
  };
}
