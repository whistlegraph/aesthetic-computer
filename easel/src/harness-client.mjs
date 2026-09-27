import {request} from 'node:http';
import {validateSettingsRequest} from './harness-contract.mjs';

export async function callSettings(args,{socket=process.env.EASEL_HARNESS_SOCKET}={}){
 validateSettingsRequest(args);
 if(!socket)throw Error('Aesel settings are unavailable outside a running session');
 return new Promise((resolve,reject)=>{
  const req=request({socketPath:socket,path:'/settings',method:'POST',headers:{'Content-Type':'application/json'},timeout:10000},res=>{
   let body='';res.on('data',chunk=>{body+=chunk;if(body.length>65536)req.destroy(Error('Oversized settings response'));});
   res.on('end',()=>{try{const value=JSON.parse(body);if(res.statusCode!==200)throw Error(value.error||'Settings request failed');resolve(value);}catch(error){reject(error);}});
  });
  req.on('error',reject);req.on('timeout',()=>req.destroy(Error('Aesel settings timed out')));req.end(JSON.stringify(args));
 });
}

