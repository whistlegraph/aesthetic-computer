#!/usr/bin/env node
import * as module from 'node:module';
import {homedir} from 'node:os';
import {join,dirname,delimiter} from 'node:path';
import {fileURLToPath} from 'node:url';
import {readFileSync,realpathSync,statSync,accessSync,constants} from 'node:fs';
import {markStartup} from './startup-trace.mjs';

markStartup('cli-imports');
process.env.NODE_COMPILE_CACHE ||= join(process.env.XDG_CACHE_HOME||join(homedir(),'.cache'),'aesel','node-compile');
module.enableCompileCache?.(process.env.NODE_COMPILE_CACHE);
const root=dirname(fileURLToPath(import.meta.url));
const version=JSON.parse(readFileSync(join(root,'../package.json'),'utf8')).version;
const setting=name=>process.env[`AESEL_${name}`]||process.env[`EASEL_${name}`]||'';
const fail=message=>{process.stderr.write(`aesthetic: ${message}\n`);process.exit(1);};
const out=value=>process.stdout.write(value+'\n');
const args=process.argv.slice(2);
async function run(file,arguments_){
  process.argv=[process.execPath,join(root,file),...arguments_];
  await import(new URL(file,import.meta.url));
}

if(['--help','-h','help'].includes(args[0])){
  process.stdout.write(readFileSync(join(root,'terminal-help.txt'),'utf8'));
}else if(['--version','-V','version'].includes(args[0])){
  out(`aesel ${version}`);
}else if(args[0]==='doctor'){
  if(args.length!==1)fail('doctor takes no arguments');
  const available=command=>(process.env.PATH||'').split(delimiter).some(dir=>{
    try{accessSync(join(dir,command),constants.X_OK);return true;}catch{return false;}
  });
  const {ACSession}=await import('./ac-session.mjs');
  const session=new ACSession();
  out(`aesel ${version}\ninterface: ${join(root,'tui.mjs')}\ncontrol plane: local\ntelemetry: off\nnode: ${process.execPath}`);
  for(const name of ['claude','codex'])out(`engine bridge ${name}: ${available(name)?'ready':`unavailable (install the ${name} CLI)`}`);
  out(`engine bridge ac: ${session.handle?'ready (hosted, no subscription needed)':'needs an @handle — run ac, then /login'}`);
  out(`default engine bridge: ${setting('BACKEND')||'the one you chose last (first launch: aesthetic)'}`);
  out(`account: ${session.label()}`);
}else if(args[0]==='history'){
  await run('history-cli.mjs',args.slice(1));
}else if(['login','logout','whoami','publish','colors','profile','mood','handle','check','mime'].includes(args[0])){
  await run('cli.mjs',args);
}else{
  const {BACKENDS,BACKEND_ALIASES}=await import('./backends.mjs');
  const {DEFAULT_CLAUDE_MODEL}=await import('./provider-defaults.mjs');
  const values={};let directory='',pro='',private_='',autopublish='';
  const pair={resume:'a thread id',piece:'a file',prompt:'text',backend:'an engine',engine:'an engine',model:'a name',runtime:'a language',genre:'piece or nopaint'};
  for(let i=0;i<args.length;i++){
    const arg=args[i];
    if(['--pro','pro'].includes(arg)){pro='on';continue;}
    if(['--piece-mode','piece'].includes(arg)){pro='off';continue;}
    if(arg==='--private'){private_='on';continue;}
    if(arg==='--autopublish'){autopublish='on';continue;}
    if(arg==='--no-autopublish'){autopublish='off';continue;}
    if(arg==='--'){
      if(args.length-i>2||(directory&&args.length-i>1))fail('expected one workspace directory');
      directory=args[++i]||directory;break;
    }
    if(arg.startsWith('--')&&pair[arg.slice(2)]){
      const key=arg.slice(2);
      if(++i===args.length)fail(`${arg} requires ${pair[key]}`);
      values[key==='engine'?'backend':key]=args[i];continue;
    }
    if(arg.startsWith('-'))fail(`unknown option: ${arg}`);
    if(directory)fail('expected one workspace directory; run aesthetic --help');
    directory=arg;
  }
  if(values.resume!==undefined&&!/^[0-9A-Fa-f-]{36}$/.test(values.resume))fail('invalid thread id');
  if(values.prompt?.length>4000)fail('prompt exceeds 4000 characters');
  if(values.model!==undefined&&!/^[A-Za-z0-9._@:-]+$/.test(values.model))fail(`invalid model name: ${values.model}`);
  if(values.runtime!==undefined&&!['mjs','lisp','lua','js','kidlisp','processing','l5'].includes(values.runtime))fail(`unknown runtime: ${values.runtime} (mjs, lisp, processing)`);
  if(values.genre!==undefined){
    const genres={piece:'piece',ac:'piece',blank:'piece',nopaint:'nopaint',brush:'nopaint'};
    if(!Object.hasOwn(genres,values.genre))fail(`unknown genre: ${values.genre} (piece, nopaint)`);
    values.genre=genres[values.genre];
  }
  if(values.genre==='nopaint'&&values.runtime&&!['mjs','js'].includes(values.runtime))fail('nopaint brushes use the javascript runtime');
  const wanted=values.backend||setting('BACKEND');
  const backend=Object.hasOwn(BACKEND_ALIASES,wanted)?BACKEND_ALIASES[wanted]:wanted;
  if(backend&&!Object.hasOwn(BACKENDS,backend))fail(`unknown backend: ${wanted} (claude, codex, ac, open)`);
  directory ||= process.cwd();
  try{if(!statSync(directory).isDirectory())throw Error();directory=realpathSync(directory);}catch{fail(`directory does not exist: ${directory}`);}
  if(!pro&&!values.piece&&!values.genre)pro='on';
  if(setting('DRY_RUN')==='1'){
    const dry={interface:'easel',directory,js_runtime:setting('JS_RUNTIME')||'node',resume:values.resume?'yes':'no',initial_prompt:values.prompt?'yes':'no',
      runtime:values.genre==='nopaint'?'mjs':values.runtime||'mjs',genre:values.genre||'choose',
      autopublish:autopublish||(/^(1|on|true|yes)$/.test(setting('AUTOPUBLISH'))?'on':'off'),
      backend:backend||'remembered',model:values.model||(backend==='claude'?DEFAULT_CLAUDE_MODEL:''),pro:pro||'off',private:private_||'off'};
    for(const [key,value] of Object.entries(dry))out(`${key}=${value}`);
  }else{
    if(!process.stdin.isTTY||!process.stdout.isTTY)fail('an interactive terminal is required');
    Object.assign(process.env,{AESTHETIC_CODE:'1',AESEL_VERSION:version,EASEL_VERSION:version});
    const forward=['--cwd',directory];
    for(const key of ['resume','prompt','piece','runtime','genre','model'])if(values[key])forward.push('--'+key,values[key]);
    if(backend)forward.push('--backend',backend);
    if(autopublish)forward.push(autopublish==='on'?'--autopublish':'--no-autopublish');
    if(pro==='on')forward.push('--pro');
    if(private_)forward.push('--private');
    markStartup('cli-ready');
    await run('launch.mjs',forward);
  }
}
