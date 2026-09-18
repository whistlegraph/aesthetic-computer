import test from 'node:test';
import assert from 'node:assert/strict';
import {notebookConversationEntry} from '../src/notebook-conversation.mjs';
import {renderFrame} from '../src/render.mjs';

test('desktop notebook excludes operational notices and unfinished inference chatter', () => {
  const entries = [
    {kind:'notice',text:'REMOTE INFERENCE'}, {kind:'notice',text:'Transcript: shared'},
    {kind:'tool',text:'Reading source'}, {kind:'assistant',text:'Looking now',activityOnly:true},
    {kind:'user',text:'Make it orange'}, {kind:'assistant',text:'The circle is orange.'},
    {kind:'error',text:'Sign in to continue'},
  ];
  assert.deepEqual(entries.filter(notebookConversationEntry).map(e=>e.text),
    ['Make it orange','The circle is orange.','Sign in to continue']);
});

test('desktop status lives outside the terminal footer without hiding approvals', () => {
  const state={desktop:true,desktopProsePrompt:true,mode:'remote',status:'thinking',account:'@test',workspace:'/private/work',entries:[],input:''};
  assert.equal(renderFrame(state,80,24,false).trim(),'');
  const approval=renderFrame({...state,desktopProsePrompt:false,approval:{subject:'Write piece'}},80,24,false);
  assert.match(approval,/ALLOW Write piece/);
  assert.doesNotMatch(approval,/REMOTE|THINKING|\/private\/work|@test/);
  assert.match(renderFrame({...state,desktop:false,desktopProsePrompt:false},80,24,false),/REMOTE · THINKING/);
});
