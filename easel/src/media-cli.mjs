#!/usr/bin/env node
import { Artifacts } from './artifacts.mjs';
const [command, cwd=process.cwd(), argument, raw]=process.argv.slice(2);
const store=new Artifacts(cwd);
try {
  let result;
  if(command==='context') result={context:await store.context(),tools:await store.tools()};
  else if(command==='run') result=await store.run(argument,JSON.parse(raw||'{}'));
  else if(command==='list') result=await store.read();
  else if(command==='create') result=await store.create(argument,raw||'');
  else if(command==='select') result=await store.select(argument);
  else if(command==='preview') result=await store.preview();
  else if(command==='export') result=await store.export(argument);
  else throw new Error('Use media-cli.mjs context|list|preview WORKSPACE, create WORKSPACE MEDIUM [NAME], select WORKSPACE UUID, run WORKSPACE ACTION JSON, or export WORKSPACE DESTINATION.');
  process.stdout.write(JSON.stringify(result)+'\n');
}catch(error){process.stderr.write(error.message+'\n');process.exitCode=1;}
