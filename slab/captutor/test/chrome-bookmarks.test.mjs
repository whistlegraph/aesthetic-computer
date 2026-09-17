import test from 'node:test';import assert from 'node:assert/strict';
import {chromeBookmarksStage,assertBookmarksHidden} from '../lib/chrome-bookmarks.mjs';
function fixture(initial,{stuck=false}={}){let shown=initial,toggles=0;const exec=(_,args)=>{if(args.at(-1).includes('CAPTUTOR_BOOKMARKS_STATE'))return {status:0,stdout:JSON.stringify(shown)};toggles++;if(!stuck)shown=!shown;return {status:0};};return {stage:chromeBookmarksStage({exec}),exec,shown:()=>shown,toggles:()=>toggles};}
test('visible bookmarks hide and restore; repeated hide never reopens',()=>{const f=fixture(true),prior=f.stage.capture();f.stage.hide();f.stage.hide();assert.equal(f.toggles(),1);assert.equal(f.shown(),false);f.stage.restore(prior);assert.equal(f.shown(),true);});
test('already hidden bookmarks stay hidden after filming',()=>{const f=fixture(false),prior=f.stage.capture();f.stage.hide();f.stage.restore(prior);assert.equal(f.toggles(),0);assertBookmarksHidden({exec:f.exec});});
test('recording guard refuses visible bar and unknown native state',()=>{assert.throws(()=>assertBookmarksHidden({exec:fixture(true).exec}),/visible/);assert.throws(()=>chromeBookmarksStage({exec:()=>({status:1})}).hide(),/verify/);});
test('failed toggle blocks capture instead of claiming clean',()=>assert.throws(()=>fixture(true,{stuck:true}).stage.hide(),/requested visibility/));
