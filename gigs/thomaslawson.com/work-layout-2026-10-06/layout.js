/* Fía's October 6 feedback: Studio grids and the Portrait of New York section. */
(() => {
  function studioGrid(main) {
    if (!document.body.classList.contains('tl-studio-detail')) return;
    const content = main.querySelector('[data-elementor-type="wp-page"]');
    if (!content || content.querySelector('.tl-archive-grid')) return;
    const sections = [...content.querySelectorAll(':scope > .elementor-top-section')];
    const header = sections.shift();
    const widgets = sections.flatMap(section =>
      [...section.querySelectorAll('.elementor-widget-image')].filter(widget => widget.querySelector('img')));
    if (!header || !widgets.length) return;

    const grid = document.createElement('section');
    grid.className = 'tl-recent-grid tl-archive-grid';
    grid.setAttribute('aria-label', 'Artworks');
    widgets.forEach(widget => {
      const work = document.createElement('figure');
      work.className = 'tl-recent-work tl-archive-work';
      // Move the original nodes: captions, image controls and search targets survive.
      work.append(widget);
      grid.append(work);
    });
    header.after(grid);
    sections.forEach(section => section.classList.add('tl-archive-grid-source'));
  }

  function portraitProject(main) {
    if (location.pathname.replace(/\/$/, '') !== '/beyond-the-studio-portraits-of-new-york') return;
    if (main.querySelector('.tl-portrait-project')) return;
    const film = main.querySelector('figure#tl-portrait-film');
    const heading = [...main.querySelectorAll('h2,h3')]
      .find(node => node.textContent.trim() === 'Portrait of New York');
    const intro = [...main.querySelectorAll('.elementor-widget-text-editor')]
      .find(node => /knowing I could rely on Russell Rainbolt/.test(node.textContent))
      ?.closest('.elementor-top-section');
    const context = [...main.querySelectorAll('.tl-follow-context')]
      .find(node => node.textContent.includes('Manhattan Municipal Building'));
    if (!film || !heading || !intro || !context) return;

    // The older project metadata gave the heading the film's ID. Its media mover
    // then detached the heading, and the following caption pass appended the
    // context after the entire page. Give the project, title and player unique IDs.
    const project = document.createElement('section');
    project.id = 'tl-portrait-project';
    project.className = 'tl-portrait-project';
    project.setAttribute('aria-labelledby', 'tl-portrait-title');
    const title = document.createElement('h2');
    title.id = 'tl-portrait-title';
    title.textContent = heading.textContent.trim();
    const photos = [];
    for (let next = heading.nextElementSibling;
      next && !next.classList.contains('tl-oct-project-header'); next = next.nextElementSibling) {
      if (next.matches('.elementor-top-section') && next.querySelector('img')) photos.push(next);
    }
    intro.before(project);
    heading.remove();
    intro.classList.remove('tl-follow-project-start');
    project.append(title, context, intro, film, ...photos);

    // Vimeo's supplied poster is an almost-black opening frame. Use the existing
    // project photograph as a legible preview without changing the player.
    const source = intro.querySelector('.elementor-widget-image img');
    const poster = film.querySelector('.tl-portrait-play img');
    if (source && poster) {
      poster.src = source.src;
      if (source.srcset) poster.srcset = source.srcset;
      poster.sizes = '(max-width: 700px) calc(100vw - 36px), min(1188px, 90vw)';
      poster.style.opacity = '1';
      poster.style.filter = 'none';
    }
  }

  function apply() {
    const main = document.querySelector('main');
    if (!main) return;
    studioGrid(main);
    portraitProject(main);
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', apply, { once: true });
  else apply();
})();
