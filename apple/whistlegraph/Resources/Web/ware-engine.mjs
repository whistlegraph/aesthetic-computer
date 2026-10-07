import {currentWare,selectWare} from './wares.mjs';
import {migrateLegacyStorage} from './legacy-storage.mjs';
Object.defineProperty(window, 'localStorage', {value:migrateLegacyStorage(window.localStorage), configurable:true});
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
