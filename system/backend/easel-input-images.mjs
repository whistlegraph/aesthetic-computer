// The Chalk client submits small, static canvas PNGs. Remote URLs and other
// media remain unsupported; dimensions and bytes bound the paid reservation.
export function imageInputBound(body) {
 let pixels=0,count=0;
 JSON.stringify({system:body.system,messages:body.messages,tools:body.tools},(_,value)=>{
  if(!value||typeof value!=='object')return value;
  if(['document','input_audio','video','image_url'].includes(value.type))throw Error('Unsupported hosted media. Use a bounded PNG image.');
  if(value.type!=='image')return value;
  const s=value.source;
  if(++count>16||s?.type!=='base64'||s.media_type!=='image/png'||typeof s.data!=='string'||s.data.length>700000||!s.data.length||s.data.length%4||!/^[A-Za-z0-9+/]+={0,2}$/.test(s.data))throw Error('Hosted images must be bounded base64 PNGs.');
  const png=Buffer.from(s.data,'base64');
  if(png.length<45||png.subarray(0,8).toString('hex')!=='89504e470d0a1a0a'||png.readUInt32BE(8)!==13||png.toString('ascii',12,16)!=='IHDR')throw Error('Invalid hosted PNG.');
  const width=png.readUInt32BE(16),height=png.readUInt32BE(20);
  if(!width||!height||width>768||height>768)throw Error('Hosted PNG dimensions must be at most 768 by 768.');
  let ended=false,data=false;
  for(let at=8;at<png.length;){
   if(at+12>png.length)throw Error('Invalid hosted PNG.');
   const size=png.readUInt32BE(at),kind=png.toString('ascii',at+4,at+8);
   if(at+size+12>png.length||kind==='acTL')throw Error('Hosted PNG must be static and complete.');
   if(kind==='IDAT')data=true;
   at+=size+12;
   if(kind==='IEND'){ended=size===0&&at===png.length;break;}
  }
  if(!ended||!data)throw Error('Invalid hosted PNG.');
  // Deliberately conservative: one token per pixel plus per-image overhead,
  // in addition to the existing serialized-byte bound. Actual cost settles
  // from provider usage; unused reserved credit returns to the wallet.
  pixels+=width*height+4096;
  return value;
 });
 return pixels;
}
