/* Fía's September 30 closeout: verified publication metadata and missing video. */
(() => {
  function apply() {
    const main = document.querySelector('main');
    if (!main) return;

    if (document.body.classList.contains('page-id-1622') && !main.querySelector('#tl-portrait-film')) {
      const intro = [...main.querySelectorAll('.elementor-widget-text-editor')]
        .find(node => /knowing I could rely on Russell Rainbolt/.test(node.textContent));
      if (intro) {
        const figure = document.createElement('figure');
        figure.id = 'tl-portrait-film';
        const play = document.createElement('button');
        play.type = 'button';
        play.className = 'tl-portrait-play';
        play.setAttribute('aria-label', 'Play Portrait of New York by Thomas Lawson');
        const poster = document.createElement('img');
        poster.src = 'https://i.vimeocdn.com/video/2187932618-bcbd4b199519acb1d916020964f6ff2f212877ab120273ea669725275f7298d5-d_960?region=us';
        poster.alt = '';
        poster.loading = 'lazy';
        const label = document.createElement('span');
        label.textContent = '▶ Play film';
        play.append(poster, label);
        play.addEventListener('click', () => {
          const frame = document.createElement('iframe');
          frame.src = 'https://player.vimeo.com/video/1216482818?dnt=1&autoplay=1';
          frame.title = 'Portrait of New York — film by Thomas Lawson';
          frame.allow = 'autoplay; fullscreen; picture-in-picture';
          frame.allowFullscreen = true;
          play.replaceWith(frame);
          frame.focus();
        }, { once: true });
        figure.append(play);
        const caption = document.createElement('figcaption');
        const link = document.createElement('a');
        link.href = 'https://vimeo.com/1216482818';
        link.textContent = 'Portrait of New York — Thomas Lawson · Watch on Vimeo';
        caption.append(link);
        figure.append(caption);
        intro.after(figure);
      }
    }

    // The legacy issue page has the cover, credit and contents. It is not a scan.
    main.querySelectorAll('a[href]').forEach(link => {
      if (/\/REALLIFE-15-cover\.jpg(?:[?#]|$)/i.test(link.href)) {
        link.href = 'https://www.thomaslawson.com/REALLIFE_15.html';
        link.setAttribute('aria-label', 'REALLIFE 15 — issue details and contents');
      }
    });

    const entries = [
      { match: '/beyond-the-studio-portraits-of-new-york/',
        title: 'Portrait of New York in Law & Order',
        author: 'Thomas Lawson', outlet: 'Studio note', date: 'August 7, 2026', iso: '2026-08-07' },
      { match: '/2024-rabkin-prize-winner-thomas-lawson',
        title: 'Thomas Lawson, 2024 Rabkin Prize winner',
        author: 'Mary Louise Schumacher', outlet: 'The Rabkin Foundation', date: 'October 23, 2024', iso: '2024-10-23' },
      { match: '/exhibitions/sunny-and-warm', title: 'Thomas Lawson: Sunny and Warm',
        author: 'Thomas Lawson', outlet: 'Chez Max et Dorothea, Los Angeles', date: 'January–February 2025', iso: null },
      { match: '/The-Studio-Reader.pdf', title: 'The Studio Reader: On the Space of Artists',
        author: 'Mary Jane Jacob and Michelle Grabner, editors', outlet: 'University of Chicago Press', date: '2010', iso: '2010' },
      { match: 'vimeo.com/406160470', title: 'Thomas Lawson: Attending to the Bats',
        author: 'Fellowship', outlet: 'Vimeo', date: 'April 10, 2020', iso: '2020-04-10' },
      { match: '/notes-anthology-for-unseen/', title: 'Anthology for Unseen',
        author: 'Amanda Bauer and Ruoyi Shi, editors', outlet: 'R+A Editions', date: '2023–24', iso: null },
    ];
    main.querySelectorAll('.tl-news-item').forEach(item => {
      const title = item.querySelector('.tl-news-title a');
      const entry = title && entries.find(entry => title.href.includes(entry.match));
      if (!entry || item.querySelector('.tl-news-meta')) return;
      title.textContent = entry.title;
      const cover = item.querySelector('.tl-news-cover');
      if (cover) cover.setAttribute('aria-label', entry.title);
      const meta = document.createElement('p');
      meta.className = 'tl-news-meta';
      const byline = document.createElement('span');
      byline.textContent = entry.author;
      const publication = document.createElement('span');
      publication.textContent = entry.outlet;
      const date = document.createElement(entry.iso ? 'time' : 'span');
      if (entry.iso) date.dateTime = entry.iso;
      date.textContent = entry.date;
      meta.append(byline, publication, date);
      item.append(meta);
    });
  }
  // The existing design plugin constructs its cards in the footer.
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', apply, { once: true });
  else apply();
})();
