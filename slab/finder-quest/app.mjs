const el = id => document.getElementById(id);
let state, busy = false;
async function post(path, body) {
  const response = await fetch(path, { method:'POST', body });
  const data = await response.json(); if (!response.ok) throw new Error(data.error); return data;
}
for (const name of ['start','finder','downloads','new']) el(name).onclick = async () => {
  try { await post('/'+name); await refresh(); } catch (e) { el('message').textContent = e.message; }
};
async function refresh() {
  if (busy) return; busy = true;
  try {
    const response = await fetch('/state'); if (!response.ok) throw new Error('Game server unavailable');
    const next = await response.json();
    if (state?.id !== next.id) { el('links').replaceChildren(); el('message').textContent=''; el('picker').value=''; }
    state = next;
    el('score').textContent = `${state.sorted} / 12 sorted · ${state.returned} / 3 returned`;
    el('path').textContent = state.root;
    el('start').hidden = !!state.startedAt;
    el('finish').hidden = !state.complete;
    el('files').replaceChildren(...state.files.map(file => {
      const row = document.createElement('div'); row.className = 'file';
      const name = document.createElement('span'); name.className = 'name'; name.textContent = file.name;
      const status = document.createElement('span'); status.className = 'status'+(file.sorted?' good':'');
      status.textContent = file.sorted ? `✓ ${file.category}` : file.inDownloads ? 'in Downloads' : file.locations.length ? file.locations.map(l=>l.path.includes('/')?l.path.split('/')[0]:'loose').join(', ') : 'not downloaded';
      row.append(name,status); return row;
    }));
    if (!el('links').children.length) for (const file of state.files.filter(f=>f.download)) {
      const link = document.createElement('a'); link.className='download'; link.href='/download/'+file.id; link.download=file.name;
      link.textContent=file.category; link.setAttribute('aria-label','Download '+file.name); el('links').append(link);
    }
    el('returned').replaceChildren(...state.files.filter(f=>f.download).map(file=>{
      const item=document.createElement('li');item.textContent=`${file.uploaded?'✓':'○'} ${file.name}`;return item;
    }));
  } catch(e) { el('message').textContent=e.message; } finally { busy=false; }
}
async function upload(files) {
  for (const file of files) {
    const expected=state?.files.find(f=>f.download && f.name===file.name);
    if (!expected) { el('message').textContent='Choose one of this quest’s three downloaded files.'; continue; }
    try { await post('/upload/'+expected.id,file);el('message').textContent='Returned '+file.name; }
    catch(e) { el('message').textContent=e.message; }
  }
  await refresh();
}
el('picker').onchange=event=>upload(event.target.files);
el('drop').ondragover=event=>{event.preventDefault();event.dataTransfer.dropEffect='copy';el('drop').classList.add('over');};
el('drop').ondragleave=()=>el('drop').classList.remove('over');
el('drop').ondrop=event=>{event.preventDefault();el('drop').classList.remove('over');upload(event.dataTransfer.files);};
await refresh();setInterval(refresh,1000);
setInterval(()=>{
  const seconds=state?.startedAt?Math.floor(((state.completedAt||Date.now())-state.startedAt)/1000):0;
  el('clock').textContent=`${Math.floor(seconds/60)}:${String(seconds%60).padStart(2,'0')}`;
},250);
