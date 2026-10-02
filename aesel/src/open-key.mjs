import {readFileSync} from 'node:fs';
import {homedir} from 'node:os';
import {join} from 'node:path';
import {parseEnv} from 'node:util';
export function openRouterKey({env=process.env,home=homedir()}={}){
  if(env.OPENROUTER_API_KEY)return env.OPENROUTER_API_KEY;
  for(const file of ['.config/aesthetic-computer/openrouter.env','.config/aesthetic-computer/jev.env']){
    try{const key=parseEnv(readFileSync(join(home,file),'utf8')).OPENROUTER_API_KEY;if(key)return key;}catch{}
  }
  return '';
}
