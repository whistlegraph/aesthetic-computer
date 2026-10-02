import {connectionFailure} from './connection-status.mjs';

// The provider owns a live turn, including its own retries. Only a terminal
// failure (or a closed bridge) permits another turn on the same conversation.
export const CONTINUE_INTERRUPTED_TURN = 'The connection interrupted this turn. Continue the latest unfinished user request from the current conversation and workspace. Check the results of earlier actions before continuing; do not repeat completed actions or paid media requests.';

export class TurnRecovery {
  constructor({run, changed=()=>{}, exhausted=()=>{}, stalled=()=>{}, delays=[1000,3000,10000], providerTimeout=90000,
    setTimer=setTimeout, clearTimer=clearTimeout}={}) {
    Object.assign(this,{run,changed,exhausted,stalled,delays,providerTimeout,setTimer,clearTimer});
    this.attempt=0;this.generation=0;this.timer=null;this.active=false;this.providerWaiting=false;
  }
  reset() {
    this.generation++;this.clearTimer(this.timer);this.timer=null;
    this.active=false;this.providerWaiting=false;this.attempt=0;this.pendingFailure=null;this.changed('');
  }
  progress() {
    if(!this.providerWaiting)return;
    this.clearTimer(this.timer);this.timer=null;this.providerWaiting=false;this.changed('');
  }
  providerRetry() {
    if(this.providerWaiting)return;
    this.providerWaiting=true;this.changed('Reconnecting · Ctrl-C stop');
    this.timer=this.setTimer(()=>{
      this.timer=null;this.providerWaiting=false;this.stalled();
    },this.providerTimeout);
    this.timer?.unref?.();
  }
  schedule(error) {
    if(this.active){if(!this.timer)this.pendingFailure=error;return;}
    this.progress();
    if(this.attempt>=this.delays.length){this.changed('Recovery paused · /retry');this.exhausted(error);return;}
    const delay=this.delays[this.attempt++],generation=this.generation;
    this.active=true;
    this.changed(`Retry ${this.attempt}/${this.delays.length} in ${delay/1000}s · Ctrl-C stop`);
    this.timer=this.setTimer(async()=>{
      this.timer=null;
      try{
        this.changed(`Reconnecting ${this.attempt}/${this.delays.length} · Ctrl-C stop`);
        await this.run({isCurrent:()=>generation===this.generation});
        if(generation===this.generation){
          this.active=false;
          const pending=this.pendingFailure;this.pendingFailure=null;
          if(pending)this.schedule(pending);else if(!this.providerWaiting)this.changed('');
        }
      }catch(failure){
        if(generation!==this.generation)return;
        this.active=false;this.pendingFailure=null;
        if(connectionFailure(failure)||failure?.bridgeFailure)this.schedule(failure);
        else {this.changed('Recovery paused · /retry');this.exhausted(failure);}
      }
    },delay);
    this.timer?.unref?.();
  }
}
