// The App Sandbox cannot launch node-pty's POSIX terminal helper. Run the same
// TUI in Electron's signed utility helper and carry terminal input over IPC.
const {utilityProcess}=require('electron');
const {join}=require('node:path');
exports.createMasTerminal=function(args,{cwd,env,cols,rows}){
  const childEnv={...env,EASEL_COLUMNS:String(cols),EASEL_ROWS:String(rows)};
  delete childEnv.ELECTRON_RUN_AS_NODE;
  const child=utilityProcess.fork(join(__dirname,'mas-terminal-worker.cjs'),args,{cwd,env:childEnv,stdio:'pipe',serviceName:'aesel workspace'});
  return {
    onData(fn){for(const stream of [child.stdout,child.stderr]){stream.setEncoding('utf8');let previous='';stream.on('data',data=>{const normalized=(previous+data).replace(/(?<!\r)\n/g,'\r\n');fn(previous?normalized.slice(1):normalized);previous=data.endsWith('\r')?'\r':'';});}},
    onExit(fn){child.on('exit',exitCode=>fn({exitCode}));},
    write(data){child.postMessage({type:'input',data});},
    resize(cols,rows){child.postMessage({type:'resize',cols,rows});},
    kill(signal){if(signal==='SIGUSR2')child.postMessage({type:'signal',signal});else child.kill();}
  };
};
