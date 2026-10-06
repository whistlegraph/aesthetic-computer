import {currentWare,selectWare} from './wares.mjs';
import {migrateLegacyStorage} from './legacy-storage.mjs';
migrateLegacyStorage(localStorage);
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
await import(currentWare(localStorage)==='roblox'?'./roblox-engine.mjs':'./engine.mjs');
