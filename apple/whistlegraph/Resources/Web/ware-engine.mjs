import {currentWare,selectWare} from './wares.mjs';
import {migrateLegacyStorage,compactLedgerCopies} from './legacy-storage.mjs';
import {ledgerMark} from '/easel/src/whistlegraph-thread.mjs';
Object.defineProperty(window, 'localStorage', {value:migrateLegacyStorage(window.localStorage), configurable:true});
try{const freed=compactLedgerCopies(localStorage,ledgerMark);if(freed)console.log('[whistlegraph] compacted ledger copies, freed '+freed.toLocaleString('en-US')+' characters');}catch{}
window.whistlegraphSelectWare=id=>{
  if(window.whistlegraphIsBusy?.()||window.whistlegraphRecording?.())return false;
  try {
    if(currentWare(localStorage)===id)return true;
    selectWare(localStorage,id);
    window.webkit.messageHandlers.whistlegraph.postMessage({id:'ware',action:'wareSwitching'});
    location.reload();
    return true;
  } catch(error) {window.toast?.(error.message);return false;}
};
try {
  await import(currentWare(localStorage)==='roblox'?'./roblox-engine.mjs':'./engine.mjs');
} catch (error) {
  window.webkit.messageHandlers.whistlegraph.postMessage({action:'startupError',text:String(error?.message||error)});
}
