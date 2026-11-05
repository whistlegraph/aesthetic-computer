// Menu Band: sided Shift linger, Caps Lock across both octaves, bracket drum latches.
export const LINGER_SECONDS = 4;
export function createNotepatModifiers() {
  const shifts = new Set();
  let caps = false, left = false, right = false;
  return {
    observe(e) {
      if (!e.name?.startsWith('keyboard:')) return null;
      const down=e.name.startsWith('keyboard:down:'), key=e.key?.toLowerCase();
      if (typeof e.caps === 'boolean') caps=e.caps;
      else if (down && key==='capslock' && !e.repeat) caps=!caps;
      if (key==='shift') {
        if (down) shifts.add(e.code || 'Shift');
        else if (e.code) shifts.delete(e.code); else shifts.clear();
      } else if (e.shift === false) shifts.clear();
      else if (e.shift && !shifts.size) shifts.add('Shift');
      const side=e.code==='BracketLeft'||key==='['||key==='{' ? 'left' : e.code==='BracketRight'||key===']'||key==='}' ? 'right' : null;
      if (side && !e.ctrl && !e.meta && !e.alt) {
        if (down && !e.repeat) { if(side==='left') left=!left; else right=!right; }
        return {side,changed:down&&!e.repeat};
      }
      return null;
    },
    linger(note) { return caps || shifts.has('Shift') || shifts.has(note.startsWith('+')?'ShiftRight':'ShiftLeft'); },
    drums(note) { return note.startsWith('+')?right:left; },
    get state() { return {caps,left,right,shift:shifts.size>0}; },
    resetMomentary() { shifts.clear(); },
  };
}
