#!/usr/bin/env node
import * as module from 'node:module';
import {homedir} from 'node:os';
import {dirname,join} from 'node:path';
import {fileURLToPath} from 'node:url';
import {launchTarget} from './launch-target.mjs';
import {markStartup} from './startup-trace.mjs';

markStartup('launcher-imports');
process.env.NODE_COMPILE_CACHE ||= join(process.env.XDG_CACHE_HOME||join(homedir(),'.cache'),'aesel','node-compile');
module.enableCompileCache?.(process.env.NODE_COMPILE_CACHE);
const entry=launchTarget(dirname(fileURLToPath(import.meta.url)));
markStartup('build-verified');
process.env.AESEL_STARTUP_ENTRY=entry;
await import(new URL(entry,import.meta.url));
