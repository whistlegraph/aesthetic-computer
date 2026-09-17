import {spawnSync} from 'node:child_process';
const stateScript=`// CAPTUTOR_BOOKMARKS_STATE
const se=Application('System Events');
const chrome=se.processes.byName('Google Chrome');
const item=chrome.menuBars[0].menuBarItems.byName('View').menus[0].menuItems.byName('Always Show Bookmarks Bar');
const mark=item.attributes.byName('AXMenuItemMarkChar').value();
JSON.stringify(Boolean(mark));
`;
export function chromeBookmarksStage({exec=spawnSync}={}) {
 const capture=()=>{
  const result=exec('/usr/bin/osascript',['-l','JavaScript','-e',stateScript],{encoding:'utf8',timeout:10000});
  if(result.status!==0)throw Error('Cannot verify Chrome bookmarks bar visibility');
  const shown=JSON.parse(result.stdout);if(typeof shown!=='boolean')throw Error('Invalid Chrome bookmarks visibility');return shown;
 };
 const set=shown=>{
  if(capture()===shown)return;
  const result=exec('/usr/bin/osascript',['-l','JavaScript','-e',`const se=Application('System Events');se.processes.byName('Google Chrome').frontmost=true;se.keystroke('b',{using:['command down','shift down']});delay(0.25);`],{encoding:'utf8',timeout:10000});
  if(result.status!==0||capture()!==shown)throw Error('Chrome bookmarks bar did not reach requested visibility');
 };
 return {capture,hide:()=>set(false),restore:shown=>{if(typeof shown==='boolean')set(shown);}};
}
export function assertBookmarksHidden(options) {
 if(chromeBookmarksStage(options).capture())throw Error('Chrome bookmarks bar is visible; stop capture');
 return {chromeBookmarksHidden:true};
}
