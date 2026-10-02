import {createRequire} from 'node:module';

// Plain chat should not initialize the JavaScript grammar. Keep parsing
// synchronous once code arrives, so highlighting never flashes uncolored.
const require=createRequire(import.meta.url);
let parser;
const acorn=()=>parser??=require('./vendor/acorn.mjs');
export const parse=(...args)=>acorn().parse(...args);
export const tokenizer=(...args)=>acorn().tokenizer(...args);
