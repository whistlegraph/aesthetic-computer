<?php
/**
 * Plugin Name: TL — Studio grids and mural project layout
 * Description: Fía's October 6 Studio and Portrait of New York corrections.
 * Version: 1.0.0
 */
if (!defined('ABSPATH')) exit;
function tl_october_layout_target() {
    if (!is_page()) return false;
    $slug = get_post_field('post_name', get_queried_object_id());
    return strpos($slug, 'inthestudio_') === 0
        || in_array($slug, array('elementor-428', 'beyond-the-studio-portraits-of-new-york'), true);
}
add_action('wp_head', function () {
    if (!tl_october_layout_target()) return;
    echo <<<'TLLAYOUTCSS'
<style id="tl-layout-css">
/* Reuse the recent periods' grid, retaining uncropped images and original captions. */
body.tl-refresh:not(#tl) .tl-archive-grid-source { display: none !important; }
body.tl-refresh:not(#tl) .tl-archive-grid .tl-archive-work {
  min-width: 0;
  width: 100% !important;
}
body.tl-refresh:not(#tl) .tl-archive-work :is(.elementor-widget-image,.elementor-widget-container,.tl-quality-image-button,a) {
  display: block !important;
  width: 100% !important;
  max-width: 100% !important;
  margin: 0 !important;
  padding: 0 !important;
}
body.tl-refresh:not(#tl) .tl-archive-work img {
  display: block !important;
  width: 100% !important;
  max-width: 100% !important;
  height: auto !important;
  max-height: none !important;
  object-fit: contain !important;
  margin: 0 !important;
}
body.tl-refresh:not(#tl) .tl-archive-work .tl-cap {
  display: block !important;
  width: 100% !important;
  margin: .6rem 0 0 !important;
  text-align: left !important;
}
body.tl-refresh:not(#tl) .tl-archive-work .tl-cap > span { display: block; }

body.tl-refresh:not(#tl) .tl-portrait-project {
  width: min(var(--tl-wide), 100vw - 2 * var(--tl-gutter));
  margin: 64px auto 0;
  padding: 48px 0 0;
  border-top: 1px solid #d8d0c5;
  scroll-margin-top: 100px;
}
body.tl-refresh:not(#tl) #tl-portrait-title {
  margin: 0 !important;
  font: 400 clamp(28px, 3vw, 36px)/1.2 var(--tl-sans) !important;
  letter-spacing: -.025em;
  color: var(--tl-ink);
}
body.tl-refresh:not(#tl) .tl-portrait-project > .tl-follow-context {
  max-width: none;
  margin: 10px 0 32px !important;
}
body.tl-refresh:not(#tl) .tl-portrait-project > .tl-oct-project-header {
  margin-top: 0 !important;
  padding-top: 0 !important;
  border-top: 0 !important;
}
body.tl-refresh:not(#tl) .tl-portrait-project #tl-portrait-film {
  width: 100% !important;
  margin: 32px 0 !important;
  scroll-margin-top: 100px;
}
body.tl-refresh:not(#tl) .tl-portrait-project .tl-portrait-play img {
  opacity: 1 !important;
  filter: none !important;
  object-fit: contain;
  background: var(--tl-bg, #f9f5ec);
}
@media (max-width:700px) {
  body.tl-refresh:not(#tl) .tl-portrait-project { margin-top: 40px; padding-top: 32px; }
}
</style>
TLLAYOUTCSS;
}, PHP_INT_MAX);
// Register after the existing October/followup footer passes.
add_action('wp_loaded', function () {
    add_action('wp_footer', function () {
        if (!tl_october_layout_target()) return;
        echo <<<'TLLAYOUTJS'
<script id="tl-layout-js">
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
</script>
TLLAYOUTJS;
    }, PHP_INT_MAX);
});
