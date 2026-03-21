#!/usr/bin/env node
// test-news-youtube.mjs - Debug YouTube embed in news page

import { CDP } from './cdp.mjs';

const cdp = new CDP({ 
  targetUrl: 'news.aesthetic.computer',
  verbose: true 
});

console.log('🔍 Looking for news page...');

try {
  // Find the news page
  const targets = await cdp.listTargets();
  const newsPage = targets.find(t => 
    t.url && (t.url.includes('news.aesthetic.computer') || t.url.includes('/item/'))
  );
  
  if (!newsPage) {
    console.log('❌ No news page found. Available pages:');
    targets.forEach(t => console.log(`   - ${t.url}`));
    process.exit(1);
  }
  
  console.log(`📰 Found news page: ${newsPage.url}`);
  await cdp.connect(newsPage);
  
  // Enable Runtime for console and DOM inspection
  await cdp.send('Runtime.enable');
  await cdp.send('DOM.enable');
  
  // Check YouTube embed structure
  console.log('\n🎬 Checking YouTube embed...\n');
  
  const result = await cdp.send('Runtime.evaluate', {
    expression: `
      (function() {
        const heroMedia = document.querySelector('.news-hero-media');
        const ytEmbed = document.querySelector('.news-youtube-embed');
        const iframe = document.querySelector('.news-youtube-embed iframe');
        const playerDiv = document.querySelector('#youtube-player');
        
        const getComputedStyles = (el, props) => {
          if (!el) return null;
          const style = window.getComputedStyle(el);
          return props.reduce((acc, p) => ({ ...acc, [p]: style[p] }), {});
        };
        
        return {
          heroMedia: heroMedia ? {
            exists: true,
            dimensions: heroMedia.getBoundingClientRect(),
            styles: getComputedStyles(heroMedia, ['pointerEvents', 'zIndex', 'position', 'overflow'])
          } : { exists: false },
          ytEmbed: ytEmbed ? {
            exists: true,
            dimensions: ytEmbed.getBoundingClientRect(),
            dataId: ytEmbed.dataset.youtubeId,
            innerHTML: ytEmbed.innerHTML.substring(0, 500),
            styles: getComputedStyles(ytEmbed, ['pointerEvents', 'zIndex', 'position', 'overflow', 'aspectRatio'])
          } : { exists: false },
          iframe: iframe ? {
            exists: true,
            src: iframe.src,
            dimensions: iframe.getBoundingClientRect(),
            styles: getComputedStyles(iframe, ['pointerEvents', 'zIndex', 'position', 'width', 'height'])
          } : { exists: false },
          playerDiv: playerDiv ? {
            exists: true,
            tagName: playerDiv.tagName,
            styles: getComputedStyles(playerDiv, ['pointerEvents', 'zIndex', 'position', 'width', 'height'])
          } : { exists: false },
          youtubePlayerReady: typeof window.youtubePlayerReady !== 'undefined' ? window.youtubePlayerReady : 'undefined'
        };
      })()
    `,
    returnByValue: true
  });
  
  console.log(JSON.stringify(result.result.value, null, 2));
  
  // Check for overlapping elements
  console.log('\n🔍 Checking for elements at center of video...\n');
  
  const centerCheck = await cdp.send('Runtime.evaluate', {
    expression: `
      (function() {
        const ytEmbed = document.querySelector('.news-youtube-embed');
        if (!ytEmbed) return { error: 'No YouTube embed found' };
        
        const rect = ytEmbed.getBoundingClientRect();
        const centerX = rect.left + rect.width / 2;
        const centerY = rect.top + rect.height / 2;
        
        const elementsAtPoint = document.elementsFromPoint(centerX, centerY);
        
        return {
          centerPoint: { x: centerX, y: centerY },
          elements: elementsAtPoint.map(el => ({
            tag: el.tagName,
            id: el.id,
            className: el.className,
            pointerEvents: window.getComputedStyle(el).pointerEvents
          }))
        };
      })()
    `,
    returnByValue: true
  });
  
  console.log(JSON.stringify(centerCheck.result.value, null, 2));
  
  cdp.close();
  
} catch (err) {
  console.error('Error:', err.message);
  process.exit(1);
}
