#!/usr/bin/env node
// Every browser shell packages the same complete runtime; no HTML rewriting.
import {stageRuntime} from '../../tools/oskiewar-manifest.mjs';
import {copyFile,mkdir,rm} from 'node:fs/promises';
import {dirname,join} from 'node:path';
import {fileURLToPath} from 'node:url';
const staged=join(dirname(fileURLToPath(import.meta.url)),'staged');
await rm(staged,{recursive:true,force:true});await mkdir(staged,{recursive:true});
const manifest=stageRuntime(staged);
await copyFile(join(staged,'mac-test.html'),join(staged,'index.html'));
console.log(`staged shared runtime v${manifest.build} · ${manifest.release}`);
