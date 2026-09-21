import { execFile } from 'node:child_process';
import { setTimeout as delay } from 'node:timers/promises';
const osa=script=>new Promise((resolve,reject)=>{
  const p=execFile('osascript',['-'],(error,stdout)=>error?reject(error):resolve(stdout.trim()));p.stdin.end(script);
});
export async function focusFinder(){await osa('tell application "Finder" to activate');}
export async function openTerminalHelper(){
  const value=await osa(`tell application "Terminal"
    set t to do script ""
    activate
    repeat with w in windows
      repeat with candidate in tabs of w
        if tty of candidate is tty of t then return (id of w as text) & "|" & tty of t
      end repeat
    end repeat
  end tell`);
  const [id,tty]=value.split('|');
  if(!/^\d+$/.test(id)||!/^\/dev\/ttys\d+$/.test(tty))throw new Error('Unable to identify owned Terminal window');
  return {windowId:Number(id),tty};
}
export async function closeTerminalHelper({windowId,tty}){
  if(!Number.isInteger(windowId)||!/^\/dev\/ttys\d+$/.test(tty))throw new Error('Invalid Terminal ownership record');
  // Close only our single idle tab. Never quit Terminal or close extra work.
  for(let i=0;i<30;i++){
    const state=await osa(`tell application "Terminal"
      if not (exists window id ${windowId}) then return "closed"
      set w to window id ${windowId}
      if (count of tabs of w) is not 1 then return "changed"
      set t to tab 1 of w
      if tty of t is not ${JSON.stringify(tty)} then return "changed"
      if busy of t then return "busy"
      close w
      return "requested"
    end tell`);
    if(state==='closed')return true;
    if(state==='changed')return false;
    if(state==='requested'){
      // Terminal may return from close while its per-profile confirmation
      // sheet is still open. A request is not proof of closure.
      const confirmed=await osa(`tell application "Terminal"
        if not (exists window id ${windowId}) then return "closed"
        set w to window id ${windowId}
        if (count of tabs of w) is not 1 then return "changed"
        if tty of tab 1 of w is not ${JSON.stringify(tty)} then return "changed"
        if busy of tab 1 of w then return "busy"
        set index of w to 1
        activate
      end tell
      tell application "System Events" to tell process "Terminal"
        if exists sheet 1 of window 1 then
          if (exists button "Close" of sheet 1 of window 1) and (exists button "Cancel" of sheet 1 of window 1) then
            click button "Close" of sheet 1 of window 1
          end if
        end if
      end tell
      return "requested"`);
      if(confirmed==='closed')return true;
      if(confirmed==='changed')return false;
    }
    await delay(100);
  }
  return false;
}
