// Isolate account/storage for the PTY benchmark; keep the real TUI and agent loop.
import os from 'node:os';
import {syncBuiltinESMExports} from 'node:module';
import {markStartup} from '../src/startup-trace.mjs';
markStartup('fixture-start');
const root=process.env.AESEL_BENCH_ROOT;
if(!root)throw Error('AESEL_BENCH_ROOT is required');
// The launcher uses NODE_OPTIONS for this preload. Keep the fixture out of
// provider launchers and worker threads; they must run their real startup.
if(process.env.NODE_OPTIONS?.includes('latency-fixture.mjs'))delete process.env.NODE_OPTIONS;
if(process.env.BUN_OPTIONS?.includes('latency-fixture.mjs'))delete process.env.BUN_OPTIONS;
os.homedir=()=>root;syncBuiltinESMExports();
const fetch=globalThis.fetch;
const endpoint=process.env.AESEL_BENCH_ENDPOINT;
if(!/^http:\/\/127\.0\.0\.1:\d+$/.test(endpoint))throw Error('Benchmark requires loopback');
globalThis.fetch=async(url,options)=>{
  if(String(url).startsWith(endpoint+'/'))return fetch(url,options);
  if(String(url)==='https://hi.aesthetic.computer/userinfo')return {ok:true,json:async()=>({sub:'latency-fixture'})};
  if(String(url)==='https://aesthetic.computer/handle?for=latency-fixture')return {ok:true,json:async()=>({handle:'bench'})};
  if(String(url).endsWith('/api/easel-transcripts'))return {ok:true,json:async()=>({})};
  throw Error('External network disabled in latency fixture');
};
if(process.env.AESEL_BENCH_BACKEND==='ac'){
  const [{BACKENDS},{deferredEngine}]=await Promise.all([import('../src/backends.mjs'),import('../src/deferred-engine.mjs')]);
BACKENDS.ac.Engine=deferredEngine(async()=>{
  const {AcServer}=await import('../src/ac-server.mjs');
  return class extends AcServer{
    constructor(options){super({...options,endpoint:endpoint+'/v1/messages',apiKey:'fixture',jev:null,workspace:true});}
  };
});
}
markStartup('fixture-ready');
