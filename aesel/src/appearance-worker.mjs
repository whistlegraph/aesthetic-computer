import {execFileSync} from 'node:child_process';
import {parentPort} from 'node:worker_threads';

let value=null;
try{
  const result=execFileSync('defaults',['read','-g','AppleInterfaceStyle'],{encoding:'utf8',timeout:1500,stdio:['ignore','pipe','ignore']});
  value=/dark/i.test(result)?'dark':'light';
}catch(error){
  // macOS removes this preference in light mode. Failure to launch or a
  // timeout says nothing about the theme, so keep the cached value instead.
  if(error.status===1)value='light';
}
parentPort.postMessage(value);
