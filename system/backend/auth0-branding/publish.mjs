#!/usr/bin/env node
// Publish this directory to the aesthetic Auth0 tenant through the Auth0 CLI
// (`auth0 login` first; the dashboard session is jeffrey's, never a repo credential).
// Usage: node publish.mjs [--only template,theme,prompts,text,partials] [--dry-run]
import {readFile} from 'node:fs/promises';
import {spawnSync} from 'node:child_process';
const args=process.argv.slice(2);
const dry=args.includes('--dry-run');
const only=args.includes('--only')?args[args.indexOf('--only')+1].split(','):['template','theme','prompts','text','partials'];
const here=new URL('./',import.meta.url);
const settings=JSON.parse(await readFile(new URL('settings.json',here),'utf8'));
const template=await readFile(new URL('template.html',here),'utf8');
function api(method,path,body){
  const label=`${method.toUpperCase()} ${path}`;
  if(dry){console.log(`[dry-run] ${label} (${JSON.stringify(body).length} bytes)`);return;}
  const r=spawnSync('auth0',['api',method,path,'--data',JSON.stringify(body)],{encoding:'utf8'});
  const out=(r.stdout+r.stderr).trim();
  if(r.status!==0||/"error"/.test(out.slice(0,200))){console.error(`${label} FAILED\n${out}`);process.exit(1);}
  console.log(`${label} ok`);
}
if(only.includes('template'))api('put','branding/templates/universal-login',{template});
if(only.includes('theme')){const {themeId,...theme}=settings.theme;api('patch',`branding/themes/${themeId}`,theme);}
if(only.includes('prompts'))api('patch','prompts',settings.prompts);
if(only.includes('text'))for(const [prompt,screens] of Object.entries(settings.text))api('put',`prompts/${prompt}/custom-text/en`,screens);
if(only.includes('partials'))for(const [prompt,screens] of Object.entries(settings.partials))api('put',`prompts/${prompt}/partials`,screens);
