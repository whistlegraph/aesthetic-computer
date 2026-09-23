// Three bodies, three monophonic vocal parts: lead, bass, hum/scat.
// Syllable/rhythm sources and arrangement decisions: ../CHORUS-ARRANGEMENT.md.
const SCALE = [2, 4, 6, 7, 9, 11, 1]; // D major pitch classes
const parse = (s) => s.split(",").filter(Boolean).map(t => {
  const [n, d] = t.split(":"); return [n === "r" ? n : Number(n), Number(d)];
});
const length = ns => ns.reduce((a, [, d]) => a + d, 0);
const pc = n => ((n % 12) + 12) % 12;
const distance = (a, b) => Math.min(Math.abs(pc(a)-pc(b)), 12-Math.abs(pc(a)-pc(b)));
const inKey = n => Array.from({length:5}, (_,j) => n+j-2).filter(x => SCALE.includes(pc(x)))
  .sort((a,b) => Math.abs(a-n)-Math.abs(b-n) || a-b)[0];
const below = (n, steps) => {
  while(steps) { n--; if(SCALE.includes(pc(n))) steps--; }
  return n;
};
const near = (p, home) => Array.from({length:6}, (_,i) => pc(p)+12*(i+2))
  .sort((a,b) => Math.abs(a-home)-Math.abs(b-home))[0];

export function splitVocalLines(v) {
  const notes = parse(v.notes), lines = v.lyrics.split("/").map(s => s.trim()).filter(Boolean);
  let cursor=0, beat=0;
  return lines.map(lyrics => {
    const count=lyrics.split(/\s+/).reduce((n,w) => n+w.split("-").length,0);
    while (cursor<notes.length && notes[cursor][0]==="r") beat+=notes[cursor++][1];
    const at=beat, ns=[]; let n=0;
    while (cursor<notes.length && n<count) {
      const token=notes[cursor++]; ns.push([...token]); beat+=token[1]; if(token[0]!=="r") n++;
    }
    if(n!==count) throw new Error(`Missing notes: ${lyrics}`);
    return {at, notes:ns, lyrics, role:"lead", gain:1};
  });
}

export function arrangeChorus(voices) {
  const parts=voices.map((v,i) => splitVocalLines(v).map(line => ({...line,
    notes:line.notes.map(([n,d]) => [n==="r" ? n : (line.at<66 ? inKey(n) : n)-(i===1?12:0),d])})));
  const turns=[0,16,40,64], familyStart=66;
  const chords=[ [2,6,9], [11,2,6], [7,11,2], [9,1,4], [4,7,11] ];
  const changes=[];
  let previous=0;
  for(let bar=0;bar<64;bar+=4) {
    const lead=bar<16?0:bar<40?1:2;
    const sounding=[];
    for(const line of parts[lead].filter(l=>l.role==="lead")) {
      let at=line.at;
      for(const [n,d] of line.notes) {
        const overlap=Math.min(at+d,bar+4)-Math.max(at,bar);
        if(n!=="r"&&overlap>0) sounding.push([n,overlap]); at+=d;
      }
    }
    const costs=chords.map((ch,i) => sounding.reduce((sum,[n,d]) =>
      sum+d*Math.min(...ch.map(c=>distance(n,c)))**2,0)+(i===0?0:.2)+(i===previous?0:.12));
    const index=costs.indexOf(Math.min(...costs)), chord=chords[index]; previous=index;
    changes.push({beat:bar,root:chord[0],pitchClasses:chord,lead});
    // The newborn gets its first four beats completely alone.
    if(bar===40) continue;
    const backing=[0,1,2].filter(i=>i!==lead);
    const bass=backing.includes(1)?1:2;
    const upper=backing.find(i=>i!==bass);
    const root=near(chord[0],bass===1?38:49), fifth=near(chord[2],bass===1?42:49);
    // Leave the first half of "I run warm" exposed except for one soft hum.
    if(bar!==8) parts[bass].push({at:bar,notes:[[root,1],["r",1],[fifth,2/3],[root,1/3],["r",1]],
      lyrics:"dum bum dum",role:"bass",gain:.55});
    const inner=near(chord[1],voices[upper].singBase ?? 60);
    if(bar%8===0 || bar===8) {
      parts[upper].push({at:bar+.5,notes:[[inner,2.5]],lyrics:"hmm",role:"hum",gain:.40});
    } else {
      const top=near(chord[2],voices[upper].singBase ?? 60);
      parts[upper].push({at:bar+2,notes:[[inner,2/3],[top,1/3],[inner,2/3],[top,1/3]],
        lyrics:upper===2?"la la la la":"doo la la la",role:"scat",gain:.48});
    }
  }
  // Three explicit harmony phrases: a moving third and a lower fifth.
  // Clear the old accompaniment so each machine still has only one mouth.
  const harmonyPhrases=[];
  for(const [lead,at] of [[0,8.5],[1,25.5],[2,45.5],[2,52.5]]) {
    const line=parts[lead].find(p=>p.at===at && p.role==="lead");
    const stop=at+length(line.notes);
    for(const other of [0,1,2].filter(i=>i!==lead)) {
      parts[other]=parts[other].filter(p=>p.role==="lead" || p.at>=stop || p.at+length(p.notes)<=at);
      const bass=other===1;
      const ns=line.notes.map(([n,d])=>[n==="r"?n:
        lead===1 ? below(n+24,other===0?2:4) : below(n,bass?4:2)-(bass?12:0),d]);
      parts[other].push({at,notes:ns,lyrics:at===8.5?line.lyrics:ns.filter(([n])=>n!=="r").map(()=>bass?"dum":"la").join(" "),
        role:at===8.5?"harmony":bass?"bass":"scat",gain:bass?.66:.62});
    }
    harmonyPhrases.push({beat:at,end:stop,lead});
  }
  // Two-beat handoff into the family line; the lead lyric starts at 66.
  parts[1].push({at:64,notes:[[38,.75],["r",1.25]],lyrics:"dum",role:"bass",gain:.60});
  parts[0].push({at:64.5,notes:[[57,.5],[59,.5],[61,.5]],lyrics:"doo bee dah",role:"scat",gain:.38});
  parts[2].push({at:65,notes:[[54,1]],lyrics:"hmm",role:"hum",gain:.26});
  const end=Math.max(...parts.flat().map(p=>p.at+length(p.notes)));
  const tag=end+1;
  parts[0].push({at:tag,notes:[[57,2/3],[59,1/3],[57,2/3],[54,1/3]],lyrics:"doo bah dee dah",role:"scat",gain:.65});
  parts[1].push({at:tag,notes:[[38,1],[45,.5],[38,.5]],lyrics:"dum bum dum",role:"bass",gain:.75});
  parts[2].push({at:tag,notes:[[54,2/3],[55,1/3],[54,2/3],[57,1/3]],lyrics:"la la la la",role:"scat",gain:.72});
  const total=tag+5;
  for(let i=0;i<3;i++) parts[i].push({at:tag+2,notes:[[[57,38,54][i],3]],lyrics:"hmm",role:"hum",gain:.65});
  for(let i=0;i<3;i++) {
    parts[i].sort((a,b)=>a.at-b.at);
    const tokens=[],lyrics=[],roles=[],gains=[];let cursor=0;
    for(const p of parts[i]) {
      if(p.at<cursor-1e-6) throw new Error(`Overlapping vocal parts for ${voices[i].name} at ${p.at}`);
      if(p.at>cursor+1e-6) tokens.push(["r",p.at-cursor]);
      tokens.push(...p.notes); lyrics.push(p.lyrics);roles.push(p.role);gains.push(p.gain);
      cursor=p.at+length(p.notes);
    }
    if(cursor<total) tokens.push(["r",total-cursor]);
    const opening=near(changes[0].pitchClasses[[2,0,1][i]],[61,38,57][i]);
    const familyNote=parts[i].find(p=>p.at===familyStart && p.role==="lead").notes[0][0];
    Object.assign(voices[i],{notes:tokens.map(([n,d])=>`${n}:${+d.toFixed(6)}`).join(","),
      lyrics:lyrics.join(" / "),lineRoles:roles,lineGains:gains,double:false,
      // Sparse pure sine support; no whistle melody masking the voices.
      notes2:`${opening}:0.5,r:${familyStart-.5},${familyNote}:0.5,r:${tag+2-familyStart-.5},${[57,38,54][i]}:3`,
      velocity2:16,previewInstrument:"sine"});
    const cast=["Noelle (Enhanced)","Tom (Enhanced)","Zoe (Premium)"];
    Object.assign(voices[i],{name:`${["neo","blueberry","frisbee"][i]} sings (${cast[i]})`,
      singVoice:cast[i],sayVoice:cast[i],singVibCents:8,singVibratoHz:[5,4,5.5][i],singF0Floor:i===1?55:80});
  }
  return {turns,familyStart,tag,total,changes,harmonyPhrases,voiceQuality:"Enhanced or Premium only",bass:"blueberry"};
}
