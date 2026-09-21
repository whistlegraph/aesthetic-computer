const el=id=>document.getElementById(id),total=30;
const random=n=>Math.floor(crypto.getRandomValues(new Uint32Array(1))[0]/2**32*n);
const shuffle=xs=>{for(let i=xs.length-1;i>0;i--){const j=random(i+1);[xs[i],xs[j]]=[xs[j],xs[i]];}return xs;};
let round,score,started,answer,problem;
function show(){
  const a=1+random(20),b=1+random(12),op=['+','−','×'][random(3)];
  answer=op==='+'?a+b:op==='−'?a-b:a*b;problem=`${a} ${op} ${b} = ?`;
  el('progress').textContent=`Round ${round} of ${total}`;el('problem').textContent=problem;
  const values=new Set([answer]);while(values.size<4)values.add(answer+random(19)-9);
  const thisRound=round;
  el('answers').replaceChildren(...shuffle([...values]).map(value=>{
    const button=document.createElement('button');button.textContent=String(value);
    button.onclick=()=>{if(round!==thisRound)return;submit(value);};return button;
  }));started=performance.now();
}
function submit(value){
  const ms=Math.round(performance.now()-started),correct=value===answer;if(correct)score++;
  const li=document.createElement('li');li.textContent=`${problem.replace('?',value)} ${correct?'✓':'✕'} · ${ms} ms`;
  if(!correct)li.className='wrong';el('history').append(li);
  el('score').textContent=`Score: ${score} / ${round}`;
  el('result').textContent=`${correct?'Correct':'Incorrect'} · Round ${round}`;
  round++;
  if(round<=total)show();else{el('progress').textContent='Complete';el('problem').textContent=`${score} / ${total}`;el('answers').replaceChildren();el('again').hidden=false;}
}
function start(){round=1;score=0;el('score').textContent='Score: 0 / 0';el('result').textContent='';el('history').replaceChildren();el('again').hidden=true;show();}
el('again').onclick=start;start();
