/* Mobile, keyboard and screen-reader behavior for the existing public archive. */
(() => {
  if (document.documentElement.dataset.tlQuality) return;
  document.documentElement.dataset.tlQuality = '1.0.0';
  const make = (tag, cls, text) => {
    const node = document.createElement(tag);
    if (cls) node.className = cls;
    if (text !== undefined) node.textContent = text;
    return node;
  };
  const visible = node => node.getClientRects().length && !node.closest('[hidden],[aria-hidden="true"]');
  const clean = text => String(text || '').replace(/\s+/g, ' ').trim();
  const main = document.querySelector('main');
  if (!main) return;
  document.body.classList.add('tl-quality');

  // Preserve the typography while giving the existing headings a coherent outline.
  function headings() {
    const nodes = [...main.querySelectorAll('h1,h2,h3,h4,h5,h6')].filter(node => {
      if (!visible(node)) return false;
      if (!clean(node.textContent)) {
        node.setAttribute('role', 'none'); node.removeAttribute('aria-level'); return false;
      }
      return true;
    });
    const about = document.body.classList.contains('page-id-68');
    const firstLevel = Math.min(...nodes.map(n => Number(n.tagName[1])));
    const primary = nodes.find(node => Number(node.tagName[1]) === firstLevel);
    const levels = [...new Set(nodes.slice(nodes.indexOf(primary)).map(n => Number(n.tagName[1])))].sort();
    let previousLevel = 0;
    nodes.forEach(node => {
      if (nodes.indexOf(node) < nodes.indexOf(primary)) {
        node.setAttribute('role', 'none'); node.removeAttribute('aria-level'); return;
      }
      let level = levels.indexOf(Number(node.tagName[1])) + 1;
      if (node !== primary && level === 1) level = 2;
      if (about && node.classList.contains('tl-about-card-title')) level = 3;
      level = Math.min(level, previousLevel + 1);
      previousLevel = level;
      node.setAttribute('role', 'heading');
      node.setAttribute('aria-level', String(Math.min(6, level)));
    });
    if (!nodes.length && document.querySelector('.tl-feature-carousel')) {
      const logo = document.querySelector('#tl-site-header .tl-site-logo');
      if (logo && !logo.closest('h1')) {
        const title = make('h1', 'tl-quality-home-title');
        logo.before(title); title.append(logo);
      }
    }
  }

  function linkNames() {
    main.querySelectorAll('a[href]').forEach(link => {
      if (!visible(link) || clean(link.textContent) || link.getAttribute('aria-label') || link.getAttribute('aria-labelledby')) return;
      const image = link.querySelector('img');
      if (image && clean(image.alt)) return;
      const box = link.closest('figure,.elementor-column,.tl-news-item,.tl-shelf-item');
      const title = box?.querySelector('figcaption,.elementor-heading-title,.tl-news-title,.tl-shelf-item-title');
      const filename = decodeURIComponent(new URL(link.href).pathname.split('/').pop() || '').replace(/\.[^.]+$/, '').replace(/[-_]+/g, ' ');
      const label = clean(title?.textContent) || image?.getAttribute('data-elementor-lightbox-title') || filename;
      if (label) {
        if (!image) link.textContent = label;
        link.setAttribute('aria-label', label + (/\.pdf(?:[?#]|$)/i.test(link.href) ? ' (PDF)' : ''));
      }
    });
  }

  function imageButtons() {
    main.querySelectorAll('img.tl-zoomable,img[role="button"]').forEach(img => {
      if (img.closest('button')) return;
      const caption = clean(img.closest('figure,.elementor-column')?.querySelector('figcaption,.elementor-heading-title')?.textContent);
      const label = caption || img.getAttribute('aria-label') || 'Enlarge image';
      const anchor = img.closest('a');
      if (anchor) {
        anchor.classList.add('tl-zoom'); anchor.dataset.caption = caption || img.alt;
        img.removeAttribute('role'); img.removeAttribute('tabindex'); img.removeAttribute('aria-label');
        return;
      }
      const button = make('button', 'tl-quality-image-button');
      button.type = 'button'; button.setAttribute('aria-label', label);
      img.removeAttribute('role'); img.removeAttribute('tabindex'); img.removeAttribute('aria-label');
      img.before(button); button.append(img);
    });
  }

  function viewer(src, caption, opener) {
    const dialog = make('dialog', 'tl-quality-viewer');
    dialog.setAttribute('aria-label', caption || 'Artwork');
    const close = make('button', 'tl-quality-viewer-close', 'Close');
    close.type = 'button'; close.setAttribute('aria-label', 'Close image');
    const figure = make('figure'); const image = make('img');
    image.src = src; image.alt = caption || 'Enlarged artwork';
    figure.append(image);
    if (caption) figure.append(make('figcaption', '', caption));
    dialog.append(close, figure); document.body.append(dialog);
    close.addEventListener('click', () => dialog.close());
    dialog.addEventListener('keydown', event => {
      if (event.key === 'Tab') { event.preventDefault(); close.focus(); }
    });
    dialog.addEventListener('click', e => { if (e.target === dialog) dialog.close(); });
    dialog.addEventListener('close', () => { dialog.remove(); document.body.classList.remove('tl-quality-viewer-open'); opener.focus({preventScroll:true}); }, {once:true});
    dialog.showModal(); document.body.classList.add('tl-quality-viewer-open'); close.focus();
  }
  document.addEventListener('click', event => {
    const trigger = event.target.closest('.tl-quality-image-button,a.tl-zoom');
    if (!trigger || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey || event.button) return;
    const img = trigger.querySelector('img');
    let source = trigger.matches('a') ? trigger.href : img?.currentSrc || img?.src;
    if (!trigger.matches('a') && img?.srcset) {
      const candidates = img.srcset.split(',').map(s => s.trim().split(/\s+/)).map(([url,width])=>({url,width:parseInt(width,10)||0})).filter(c=>c.width<=2600).sort((a,b)=>b.width-a.width);
      if(candidates[0])source=candidates[0].url;
    }
    if (!source) return;
    event.preventDefault(); event.stopImmediatePropagation();
    viewer(source, trigger.dataset.caption || trigger.getAttribute('aria-label')?.replace(/^Enlarge:?\s*/, '') || img?.alt, trigger);
  }, true);

  // A same-page search result must dismiss the modal before focusing the artwork.
  document.addEventListener('click', event => {
    const card = event.target.closest('.tl-search-card');
    if (!card || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey || event.button) return;
    const url = new URL(card.href);
    if (url.origin === location.origin && url.pathname === location.pathname) {
      event.preventDefault();
      const arrive = () => {
        if (location.hash !== url.hash) location.hash = url.hash;
        requestAnimationFrame(() => window.dispatchEvent(new Event('hashchange')));
      };
      const panel = card.closest('dialog');
      if (panel?.open) { panel.addEventListener('close',arrive,{once:true}); panel.close(); }
      else arrive();
    }
  }, true);

  function carousel() {
    const previous = document.querySelector('.tl-feature-carousel');
    const recent = (window.TL_RECENT || []).filter(w => w?.u && /^https?:\/\//.test(w.u));
    if (!previous || !recent.length) return;
    const small = matchMedia('(max-width: 600px)');
    const reduced = matchMedia('(prefers-reduced-motion: reduce)');
    let works, current = 0, playing = false, timer, touch, suppressClick = false;
    const carousel = make('section', 'tl-feature-carousel tl-quality-carousel');
    carousel.setAttribute('aria-roledescription', 'carousel'); carousel.setAttribute('aria-label', 'Recent work');
    const figure = make('figure', 'tl-feature');
    const link = make('a', 'tl-feature-art'); const image = make('img');
    image.decoding = 'async'; image.fetchPriority = 'high'; link.append(image);
    const footer = make('figcaption', 'tl-feature-foot');
    const caption = make('div', 'tl-feature-caption');
    const status = make('span', 'tl-quality-sr'); status.setAttribute('role','status'); status.setAttribute('aria-atomic','true');
    const controls = make('div', 'tl-feature-controls');
    const play = make('button', 'tl-quality-play', 'Play');
    const prev = make('button', '', '←'); const next = make('button', '', '→');
    for (const button of [play,prev,next]) button.type='button';
    prev.setAttribute('aria-label','Previous work'); next.setAttribute('aria-label','Next work');
    controls.append(play,prev,next); footer.append(caption,controls,status); figure.append(link,footer); carousel.append(figure);
    previous.replaceWith(carousel);
    const sized = (url,width) => url.replace(/\/rs:fit:\d+:\d+\//, `/rs:fit:${width}:0/`);
    function order() {
      const anchor = recent.find(w => w.t?.toLowerCase() === 'candlelight');
      const preferred = recent.filter(w => w.w && w.h && (small.matches ? w.h > w.w : w.w >= w.h));
      works = [...new Set([...(small.matches ? preferred : [anchor,...preferred]),...recent].filter(Boolean))];
    }
    function fit() {
      const work=works[current]; const ratio=work.w&&work.h?work.w/work.h:1.5;
      const holder=carousel.parentElement;
      const available=holder.getBoundingClientRect().width;
      const header=document.querySelector('#tl-site-header')?.getBoundingClientRect().height||90;
      const height=Math.max(200,innerHeight-header-footer.getBoundingClientRect().height-80);
      figure.style.width=Math.floor(Math.min(available,height*ratio))+'px';
    }
    function show(index, announce = true) {
      current=(index+works.length)%works.length; const work=works[current];
      carousel.dataset.index=current; carousel.dataset.title=work.t;
      const file=new URL(work.u).pathname.split('/').pop();
      const original=`/${work.y>=2022?'inthestudio_2022-present':'inthestudio_2020-2022'}/#tl-find=${encodeURIComponent(work.t)}&tl-image=${encodeURIComponent(file)}`;
      link.href=window.TL_QUALITY_ARTWORK_URLS?.[work.t+'|'+file]||original;
      link.setAttribute('aria-label',`${work.t}, ${current+1} of ${works.length}. View artwork.`);
      image.alt=work.t; if(work.w&&work.h){image.width=work.w;image.height=work.h;image.style.aspectRatio=work.w+'/'+work.h;}
      image.src=sized(work.u,small.matches?900:1600);
      const title=make('p','tl-feature-title'); title.append(make('em','',work.t));
      if(work.ys)title.append(document.createTextNode(', '+work.ys));
      const detail=[work.m,String(work.d||'').replace(/(\d)\s*x\s*(?=\d)/gi,'$1 × ')].filter(Boolean).join(' · ');
      caption.replaceChildren(title); if(detail)caption.append(make('p','tl-feature-detail',detail));
      if(announce&&!playing)status.textContent=`${work.t}, ${current+1} of ${works.length}`;
      fit();
    }
    function rotate(value) {
      playing=value&&!reduced.matches; clearInterval(timer);
      play.textContent=playing?'Pause':'Play'; play.setAttribute('aria-label',playing?'Pause slideshow':'Start slideshow');
      status.setAttribute('aria-live',playing?'off':'polite');
      if(playing)timer=setInterval(()=>show(current+1,false),7000);
    }
    play.addEventListener('click',()=>rotate(!playing));
    prev.addEventListener('click',()=>{rotate(false);show(current-1);});
    next.addEventListener('click',()=>{rotate(false);show(current+1);});
    carousel.addEventListener('focusin',()=>rotate(false));
    carousel.addEventListener('mouseenter',()=>rotate(false));
    document.addEventListener('visibilitychange',()=>{if(document.hidden)rotate(false);});
    carousel.addEventListener('keydown',event=>{if(['ArrowLeft','ArrowRight'].includes(event.key)){event.preventDefault();rotate(false);show(current+(event.key==='ArrowLeft'?-1:1));}});
    link.addEventListener('touchstart',e=>{touch={x:e.touches[0].clientX,y:e.touches[0].clientY};},{passive:true});
    link.addEventListener('touchend',e=>{
      if(!touch)return;const dx=e.changedTouches[0].clientX-touch.x,dy=e.changedTouches[0].clientY-touch.y;touch=null;
      if(Math.abs(dx)>45&&Math.abs(dx)>Math.abs(dy)*1.5){e.preventDefault();suppressClick=true;rotate(false);show(current+(dx<0?1:-1));setTimeout(()=>suppressClick=false,400);}
    },{passive:false});
    link.addEventListener('click',event=>{if(suppressClick){event.preventDefault();suppressClick=false;}});
    image.addEventListener('load',fit);addEventListener('resize',fit);
    small.addEventListener('change',()=>{const work=works[current];order();show(Math.max(0,works.indexOf(work)),false);});
    reduced.addEventListener('change',()=>{play.hidden=reduced.matches;if(reduced.matches)rotate(false);});
    order();play.hidden=reduced.matches;rotate(false);show(0,false);
  }
  function init() {
    linkNames();imageButtons();headings();carousel();
    const secondary=document.querySelector('.tl-menu-secondary');
    if(secondary&&!secondary.querySelector('a[href="/art-archive/"]')){const a=make('a','','Archive');a.href='/art-archive/';secondary.append(a);}
  }
  if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',init,{once:true});else init();
})();
