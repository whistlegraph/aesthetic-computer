#!/usr/bin/env node
import {readFile,writeFile,mkdir} from "node:fs/promises";
import {dirname,resolve} from "node:path";
import {fileURLToPath} from "node:url";
import {createRequire} from "node:module";
import {ROZ_GRAPH} from "../graph/roz-plan.mjs";
const {build}=createRequire(new URL("../../system/package.json",import.meta.url))("esbuild");
const [compute,display,sceneShader,sceneSource,snapshot]=await Promise.all([
  "../graph/roz-graph.wgsl","../graph/roz-present.wgsl","../scene/wanderer.wgsl","../scene/wanderer.lisp","../conformance/top100.json",
].map(name=>readFile(new URL(name,import.meta.url),"utf8")));
const define=Object.fromEntries(Object.entries({ROZ_GRAPH_SHADER:compute,ROZ_DISPLAY_SHADER:display,SCENE_SHADER:sceneShader,SCENE_SOURCE:sceneSource,TOP_HITS:JSON.parse(snapshot)}).map(([k,v])=>[k,JSON.stringify(v)]));
const result=await build({entryPoints:[fileURLToPath(new URL("../graph/roz-preview.mjs",import.meta.url))],bundle:true,write:false,format:"iife",platform:"browser",minify:true,external:["https","url","module"],define});
const out=resolve(process.argv[2]||"/tmp/kidlisp-roz/preview.html");
await mkdir(dirname(out),{recursive:true});
await writeFile(resolve(dirname(out),"roz-graph.json"),JSON.stringify(ROZ_GRAPH,null,2)+"\n");
await writeFile(out,`<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>KidLisp preview</title>
<style>
*{box-sizing:border-box}html,body{margin:0;width:100%;height:100%;overflow:hidden;background:#090b11;color:#eef2fa;font:16px system-ui}.stage{position:fixed;inset:0}canvas,iframe{display:block;width:100%;height:100%;border:0;image-rendering:pixelated}canvas{touch-action:none;outline:none}[hidden]{display:none!important}iframe[hidden]{display:block!important;visibility:hidden;pointer-events:none}
header{z-index:10;position:fixed;left:10px;right:10px;top:10px;display:flex;gap:6px;align-items:start;pointer-events:none}header>*{pointer-events:auto}button,select,summary,output{font:inherit;color:inherit;background:#080b17d9;border:1px solid #ffffff45;border-radius:7px;padding:7px 10px;backdrop-filter:blur(12px)}button,select,summary{cursor:pointer}select{max-width:38vw;width:180px}output{margin-left:auto;font-size:13px;font-variant-numeric:tabular-nums;white-space:nowrap;line-height:23px}a{color:#e6d5ff}
summary{list-style:none}summary::-webkit-details-marker{display:none}details[open] .panel{position:fixed;right:10px;top:58px;width:min(660px,calc(100vw - 20px));max-height:calc(100dvh - 70px);overflow:auto;background:#080b17f5;border:1px solid #ffffff45;border-radius:8px;padding:16px}pre{text-align:left;white-space:pre-wrap;overflow-wrap:anywhere;font:17px/1.5 ui-monospace,monospace;margin:14px 0}p{font-size:14px;color:#c1c9dc;line-height:1.5;margin:12px 0 0}.links{display:flex;gap:8px;align-items:center;flex-wrap:wrap}.links button{font-size:14px}.syntax-rainbow{background:linear-gradient(90deg,#ff8585,#ffd47a,#9ef895,#89ceff,#e3a1ff);background-clip:text;color:transparent}#notice{position:fixed;z-index:11;left:10px;bottom:10px;max-width:calc(100vw - 20px);background:#080b17ec;padding:8px;border-radius:7px}#notice:empty{display:none}@media(max-width:500px){header{gap:4px}select{width:120px}button,select,summary{padding:7px}output{padding:7px 6px}pre{font-size:15px}}
</style>
<main><div class="stage"><canvas id="gpu" aria-label="$roz feedback image"></canvas><canvas id="cpu" hidden aria-label="$roz CPU fallback"></canvas><canvas id="scene" hidden tabindex="0" aria-label="First person scene: click, then WASD to walk; drag to look"></canvas><iframe id="reference" hidden title="KidLisp reference runtime" allow="autoplay; fullscreen" referrerpolicy="no-referrer"></iframe></div>
<header><select id="piece" aria-label="Choose a KidLisp piece"></select><button id="pause" aria-label="Pause animation">Pause</button><details id="code"><summary>Code</summary><div class="panel"><div class="links"><button id="reset">Restart</button><a id="permalink" target="_blank" rel="noopener">Open on AC ↗</a></div><p id="profile"></p><div class="links" id="sources"></div><pre id="source"></pre><p id="timing"></p></div></details><output id="stats" aria-live="off">Preparing…</output></header><p id="notice" role="status"></p></main>
<script>${result.outputFiles[0].text.replaceAll("</script","<\\/script")}</script></html>`);
console.log(out);
