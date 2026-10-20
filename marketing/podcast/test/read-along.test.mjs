import {test} from 'node:test';
import assert from 'node:assert/strict';
import {alignWords,alignWhisperWords,sentenceCues,spokenTranscript} from '../lib/read-along.mjs';
import {contentPalette,readAlongThemes} from '../lib/read-along-palette.mjs';
test('passage colors follow subject matter and keep dark text legible on pastel',()=>{
 const samples=['A paper card catalog and a legal pad.','The Doom game has candy WADs and keyboard controls.','Dance music with singing and semitones.'];
 const themes=readAlongThemes(samples.join('\n\n'));
 assert.deepEqual(themes.map(t=>t.name),['paper','play','music']);
 const luminance=rgb=>rgb.map(c=>c/255).map(c=>c<=.04045?c/12.92:((c+.055)/1.055)**2.4).reduce((sum,c,i)=>sum+c*[.2126,.7152,.0722][i],0);
 for(const theme of themes){
  assert.equal(samples.join('\n\n').slice(theme.start,theme.end),samples[themes.indexOf(theme)]);
  for(const key of ['foreground','highlight','link'])assert.ok((luminance(theme.background)+.05)/(luminance(theme[key])+.05)>4.5,key);
 }
 assert.deepEqual(contentPalette('An unusual quiet day.'),contentPalette('An unusual quiet day.'));
});
const heard=(text,from,to)=>({text,offsets:{from,to}});
test('the complete transcript includes the opening, canonical body and closing in audio order',()=>{
 const cues=[{role:'intro',start:2,end:5,text:'Computer Radio. Today.'},{role:'body',start:7,end:9,text:'Fía made a mark.'},{role:'outro',start:11,end:14,text:"That's today. Write to us."}];
 const result=spokenTranscript('Fía made a mark.',{cues});
 assert.equal(result.body,"Computer Radio. Today.\n\nFía made a mark.\n\nThat's today. Write to us.");
 assert.deepEqual(result.sections.map(s=>result.body.slice(s.start,s.end)),cues.map(c=>c.text));
 assert.deepEqual(result.sentences.map(s=>s.from),[2000,7000,11000]);
 assert.throws(()=>spokenTranscript('body',{cues:[cues[2],cues[0]]}),/ordered/);
 assert.throws(()=>spokenTranscript('body',{cues:[cues[0]]}),/no body/);
});
test('readalong retains canonical names with measured boundaries and explicit substitutions',()=>{
 const body='Aesel meets Loopboy.';
 const result=alignWords(body,[heard('Asil',0,300),heard('meets',300,600),heard('Loop',600,800),heard('Boy',800,1000)],12000);
 assert.deepEqual(result.words.map(w=>[body.slice(w.start,w.end),w.fromMs,w.toMs,w.match]),[['Aesel',12000,12300,'substitution'],['meets',12300,12600,'exact'],['Loopboy',12600,13000,'exact']]);
 assert.equal(result.alignment.substitutions,1);
});
test('DTW token onsets replace heuristic segments and sentence boundaries preserve pauses',()=>{
 const token=(text,t_dtw)=>({text,t_dtw});
 const transcript=[
  {...heard('one.',0,700),tokens:[token(' one',10),token('.',40)]},
  {...heard('two',700,1000),tokens:[token(' two',80),token('.',95)]},
 ];
 const sentences=sentenceCues('1\n00:00:01,000 --> 00:00:01,300\none.\n\n2\n00:00:01,750 --> 00:00:02,000\ntwo\n');
 const result=alignWhisperWords('one. two',transcript,1000,sentences);
 assert.deepEqual(result.words.map(w=>[w.text,w.fromMs,w.toMs]),[['one',1100,1300],['two',1800,1950]]);
 assert.match(result.alignment.method,/dtw/);
 assert.throws(()=>sentenceCues('bad'),/Expected sentence/);
});
test('old Whisper JSON remains usable and standalone punctuation is not a spoken word',()=>{
 const result=alignWhisperWords('one two',[heard('one',0,100),heard(',',100,150),heard('two',150,300)]);
 assert.equal(result.alignment.missingWords,0);assert.equal(result.alignment.substitutions,0);
 assert.deepEqual(result.words.map(w=>w.fromMs),[0,150]);
});
test('a quantity abbreviated by ASR retains both measured spoken tokens',()=>{
 const result=alignWhisperWords('eight gigabytes',[{...heard(' 8GB',0,1000),tokens:[{text:' 8',t_dtw:10},{text:'GB',t_dtw:30},{text:'.',t_dtw:70}]}]);
 assert.deepEqual(result.words.map(w=>[w.text,w.fromMs,w.toMs]),[['eight',100,300],['gigabytes',300,700]]);
 assert.equal(result.alignment.missingWords,0);
});
test('a number printed by ASR is spelled the way the script reads it, one timed word each',()=>{
 const turned=alignWhisperWords('I turned it two hundred seventy degrees.',[{...heard(' 270',0,900),tokens:[{text:' 2',t_dtw:10},{text:'70',t_dtw:40},{text:'.',t_dtw:90}]}]);
 assert.deepEqual(turned.words.map(w=>[w.text,w.fromMs,w.toMs]),[['two',100,367],['hundred',367,633],['seventy',633,900]]);
 assert.equal(turned.alignment.timedWords,3);
 const pack=alignWhisperWords('a pack of two hundred fifty thousand cells for a dollar fifty in tez',[
  {...heard(' 250,000',0,1200),tokens:[{text:' 250',t_dtw:10},{text:',',t_dtw:50},{text:'000',t_dtw:60},{text:'.',t_dtw:120}]},
  {...heard(' $1.50',1200,2000),tokens:[{text:' $',t_dtw:120},{text:'1',t_dtw:130},{text:'.',t_dtw:150},{text:'50',t_dtw:170},{text:'.',t_dtw:200}]},
 ]);
 assert.deepEqual(pack.words.map(w=>w.text),['two','hundred','fifty','thousand','a','dollar','fifty']);
 assert.equal(pack.alignment.timedWords,7);
 const digits=alignWhisperWords('Room 270 is open.',[{...heard(' 270',0,900),tokens:[{text:' 2',t_dtw:10},{text:'70',t_dtw:40},{text:'.',t_dtw:90}]}]);
 assert.deepEqual(digits.words.map(w=>w.text),['270']);
});
test('a domain printed by ASR preserves every spoken name and dot token',()=>{
 const result=alignWhisperWords('Aesthetic dot computer.',[{...heard(' Aesthetic.computer.',0,2000),tokens:[
  {text:' A',t_dtw:10},{text:'esthetic',t_dtw:20},{text:'.',t_dtw:40},
  {text:'com',t_dtw:60},{text:'puter',t_dtw:80},{text:'.',t_dtw:100},
 ]}]);
 assert.deepEqual(result.words.map(w=>[w.text,w.fromMs,w.toMs]),[['Aesthetic',100,400],['dot',400,600],['computer',600,1000]]);
 assert.equal(result.alignment.missingWords,0);
});
test('missing words are omitted rather than given invented times; repeated words retain distinct offsets',()=>{
 const result=alignWords('one missing two two',[heard('one',0,100),heard('two',200,300),heard('two',400,500)]);
 assert.equal(result.alignment.missingWords,1);
 assert.deepEqual(result.words.map(w=>w.start),[0,12,16]);
});
