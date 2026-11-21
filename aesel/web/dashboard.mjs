// The server derives ownership from the token. The browser never queries by handle.
export function accountThreads({root,token,valid}) {
  const list=root.querySelector('#account-threads'), reader=root.querySelector('#thread-reader');
  const status=root.querySelector('#threads-status'), more=root.querySelector('#more-threads');
  let pageCursor=null,detailVersion=0,loading=false,items=[];
  async function read(params) {
    const access=await token();
    if(!valid())throw Error('Account changed. Reload to open your threads.');
    const url=new URL('https://aesthetic.computer/api/easel-transcripts');
    url.search=new URLSearchParams({scope:'mine',...params});
    const response=await fetch(url,{headers:{Authorization:'Bearer '+access},cache:'no-store'});
    if(!response.ok)throw Error(response.status===401?'Please sign in again.':'Account history is unavailable.');
    const data=await response.json();
    if(!valid())throw Error('Account changed.');
    return data;
  }
  function button(text,action){
    const b=document.createElement('button');b.type='button';b.className='thread-link';b.textContent=text;b.onclick=action;return b;
  }
  function showThread(item){
    const version=++detailVersion;reader.replaceChildren();
    const heading=document.createElement('h3');heading.textContent=item.title || item.medium+' thread';
    const messages=document.createElement('ol');messages.className='thread-messages';
    const message=document.createElement('p');message.setAttribute('role','status');
    let after=0;
    const next=button('Read more',load);
    async function load(){
      next.disabled=true;message.textContent='Opening thread…';
      try{
        const data=await read({sessionId:item.id,afterSeq:String(after)});
        if(version!==detailVersion || !valid())return;
        for(const record of data.records){
          if(record.type!=='message'&&record.type!=='artifact')continue;
          const row=document.createElement('li'),who=document.createElement('strong');
          who.textContent=record.type==='artifact'?'Revision':record.role==='user'?'You':'Aesel';
          row.append(who,document.createTextNode(record.type==='artifact'?record.medium+' · v'+record.revision:record.text));messages.append(row);
        }
        after=data.nextAfterSeq;next.hidden=after===null;
        message.textContent=!messages.childElementCount&&next.hidden?'No messages in this thread.':'';
      }catch(error){if(version===detailVersion&&valid()){message.textContent=error.message;next.hidden=false;}}
      finally{next.disabled=false;}
    }
    const close=button('Close thread',()=>{++detailVersion;reader.replaceChildren();});
    reader.append(heading,close,messages,message,next);void load();
  }
  async function load(reset=false){
    if(loading)return;loading=true;more.disabled=true;status.textContent='Loading account threads…';
    try{
      const data=await read(!reset&&pageCursor?pageCursor:{});
      if(!Array.isArray(data.threads))throw Error('Account history is unavailable.');
      items=reset?data.threads:[...items,...data.threads];
      items.sort((a,b)=>String(b.createdAt).localeCompare(String(a.createdAt)));
      list.replaceChildren(...items.map(item=>{
        const row=document.createElement('li'),date=document.createElement('small');
        date.textContent=new Date(item.createdAt).toLocaleDateString();
        const title=item.title || (item.medium || 'Aesel')+' thread · '+item.id.slice(0,8);
        row.append(button(title,()=>showThread({...item,title})),date);return row;
      }));
      pageCursor=data.nextBefore;more.hidden=!pageCursor;
      status.textContent=items.length?'':'No account threads yet.';
    }catch(error){if(valid()){status.textContent=error.message;more.hidden=false;more.textContent='Retry';}}
    finally{loading=false;more.disabled=false;if(pageCursor)more.textContent='More threads';}
  }
  more.onclick=()=>load();return {load};
}
