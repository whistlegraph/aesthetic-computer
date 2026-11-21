// Shared by the guide and the sign-in desk. Keep format, illustration and caption together.
const slides = [
  ["javascript", "text/javascript", "#a67c00", "A little world that follows your cursor.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He reaches on three legs and holds a magic wand with a yellow star tip; the canvas shows a pink field with a large yellow circle, white dots and an arrow cursor."],
  ["lisp", "text/x-lisp", "#a1306f", "A handful of parentheses sets shapes spinning.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He sits and holds a yellow pencil; the canvas shows three rows of plum parentheses enclosing pink, yellow and teal shapes."],
  ["lua", "text/x-lua", "#27308f", "A crescent moon drawn in Lua.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He reaches on three legs and holds a stubby teal crayon; the canvas shows a deep blue-violet night with a pale crescent moon and a small pink circle."],
  ["png", "image/png", "#d6337f", "A pixel heart with a see-through edge.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He reaches on three legs and holds a paintbrush loaded with pink; the canvas shows a pixel-art pink heart on a transparency checkerboard."],
  ["jpeg", "image/jpeg", "#c8571b", "The last light on a violet lake.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He sits and holds a small silver camera; the canvas shows a white-bordered photo of a sunset over violet hills and a lake."],
  ["webp", "image/webp", "#1f8a80", "A daisy for a tiny web page.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He reaches on three legs and holds a teal paint roller; the canvas shows a browser window with a white daisy on teal."],
  ["wav", "audio/wav", "#6b46c1", "Two taps and a ringing chime.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He reaches on three legs and holds a conductor's baton; the canvas shows a violet waveform with a pink playhead."],
  ["mpeg", "audio/mpeg", "#b0329b", "A tune to take along as an MP3.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He reaches on three legs, wears pink headphones and holds a microphone; the canvas shows two beamed eighth notes over five equalizer bars."],
  ["ogg", "audio/ogg", "#2f6f9f", "One clear note rings out.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He reaches on three legs and holds a tuning fork; the canvas shows a teal speaker cone with rings spreading out in teal to violet."],
  ["pdf", "application/pdf", "#b3261e", "A paper with a chart tucked inside.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He sits and holds a black fountain pen; the canvas shows a typeset page with a title bar, two columns of text lines and a boxed pink chart."],
  ["plain", "text/plain", "#555555", "A few lines you can open anywhere.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He sits and holds a feather quill, an ink pot on the ground; the canvas shows rows of ragged gray scribble."],
  ["gameboy", "application/x-gameboy-rom", "#5f7a1a", "A pocket world in four shades of green.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He sits and holds a gray handheld console; the canvas shows an olive four-shade pixel scene with a heart over two shrubs."],
  ["nes", "application/x-nes-rom", "#27306b", "An eight-bit sky for an NES cartridge.", "Aesel the donkey in red glasses and a pink bandana at an easel, cabled to a small server tower on the horizon. He sits and holds a flat gray controller with a hanging cord; the canvas shows an 8-bit night sky with stars, a moon and a pink diamond over teal ground."],
];
for(const figure of document.querySelectorAll('figure[data-mime-slides]')){
  const art=figure.querySelector('img'),badge=figure.querySelector('code'),byline=figure.querySelector('.byline');
  let i=0;
  if(matchMedia('(prefers-reduced-motion: reduce)').matches)continue;
  setInterval(()=>{
    if(document.hidden||!figure.getClientRects().length)return;
    i=(i+1)%slides.length;
    const index=i,[key,mime,color,caption,alt]=slides[i],next=new Image();
    next.onload=()=>{
      if(i!==index)return;
      art.src=next.src;art.alt=alt;
      const [family,format]=mime.split('/');
      badge.className='mime ty-'+family;badge.style.setProperty('--c',color);
      const subtype=document.createElement('u');subtype.textContent=format;
      badge.replaceChildren(document.createTextNode(family+'/'),subtype);
      byline.textContent=caption;
    };
    next.src='/mime/'+key+'.webp';
  },3400);
}
