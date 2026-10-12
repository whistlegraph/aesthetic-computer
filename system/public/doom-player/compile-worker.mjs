// Keep BSP and artwork compilation isolated from the running game's globals and UI loop.
import {compileProject,assetCatalog} from './project.mjs?v=2026-10-12.8';
let queue=Promise.resolve();
self.onmessage=event=>{const {id,operation,project,game}=event.data;
 queue=queue.then(async()=>{
  try{
   if(operation==='catalog'){self.postMessage({type:'doom:compiled',id,catalog:await assetCatalog(game)});return;}
   const wad=await compileProject(project);self.postMessage({type:'doom:compiled',id,wad:wad.buffer},[wad.buffer]);
  }catch(error){self.postMessage({type:'doom:compiled',id,error:error.message});}
 });
};
