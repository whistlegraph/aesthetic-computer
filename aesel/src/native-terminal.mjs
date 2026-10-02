import {fstatSync,writeSync} from 'node:fs';

// An optional private pipe to the native terminal host. Opening drafts stay
// local until the editor owns input; they can never answer an account gate.
export function nativeTerminalPhase(phase){
  if(process.env.AESEL_NATIVE_CONTROL_FD!=='3'||!['boot','gate','ready'].includes(phase))return;
  try{if(fstatSync(3).isFIFO())writeSync(3,`AESEL/1 ${phase}\n`);}catch{}
}
