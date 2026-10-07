<?php
/**
 * Plugin Name: TL — Mobile, accessibility and public archive
 * Description: Accessible interactions and server-rendered public artwork records.
 * Version: 1.0.0
 */
if (!defined('ABSPATH')) exit;

// Reuse only the existing, publication-filtered public index. Never fetch inventory.
function tl_quality_rows() {
    static $rows = null;
    if ($rows !== null) return $rows;
    if (!function_exists('tl_search_response')) return array();
    $response = tl_search_response();
    if (is_wp_error($response) || !is_object($response) || !method_exists($response, 'get_data')) return array();
    $data = $response->get_data();
    $rows = isset($data['items']) && is_array($data['items']) ? $data['items'] : array();
    return $rows;
}
function tl_quality_art_slug($row) {
    $prefix = substr(sanitize_title(remove_accents($row['title'])), 0, 70);
    $prefix = preg_replace('/%(?:[0-9a-f])?$/i', '', $prefix);
    return rtrim($prefix, '-') . '-' . substr(hash('sha256', $row['url']), 0, 12);
}
function tl_quality_art_url($row) { return home_url('/artwork/' . tl_quality_art_slug($row) . '/'); }
function tl_quality_display($row) {
    $out = $row;
    // Early public captions combine the name and metadata; preserve all of it.
    if (empty($row['detail']) && preg_match('/^(.+?),\s*((?:19|20)\d{2}\b.*)$/u', $row['title'], $matches)) {
        $out['title'] = $matches[1]; $out['detail'] = $matches[2];
    }
    return $out;
}
function tl_quality_public_url($row) {
    return $row['type'] === 'Artwork' ? tl_quality_art_url($row) : $row['url'];
}
function tl_quality_path() {
    return trim((string) parse_url(isset($_SERVER['REQUEST_URI']) ? $_SERVER['REQUEST_URI'] : '/', PHP_URL_PATH), '/');
}

add_filter('rest_post_dispatch', function ($response, $server, $request) {
    if ($request->get_route() !== '/tl/v1/search-index' || is_wp_error($response) || $response->get_status() !== 200) return $response;
    $data = $response->get_data();
    if (!isset($data['items'])) return $response;
    foreach ($data['items'] as &$row) {
        if ($row['type'] === 'Artwork') $row['url'] = tl_quality_art_url($row);
    }
    unset($row);
    $response->set_data($data);
    return $response;
}, 20, 3);

function tl_quality_route() {
    $path = tl_quality_path();
    if ($path === 'tl-artwork-sitemap.xml') {
        status_header(200); header('Content-Type: application/xml; charset=UTF-8');
        // Publication status is checked on every request, including withdrawn work.
        header('Cache-Control: no-store');
        echo '<?xml version="1.0" encoding="UTF-8"?>' . "\n";
        echo '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">';
        $urls = array(home_url('/art-archive/'), home_url('/exhibitions/'));
        foreach (tl_quality_rows() as $row) if ($row['type'] === 'Artwork') $urls[] = tl_quality_art_url($row);
        foreach (array_unique($urls) as $url) echo '<url><loc>' . esc_xml($url) . '</loc></url>';
        echo '</urlset>'; exit;
    }
    if (!is_404() || ($path !== 'art-archive' && strpos($path, 'artwork/') !== 0)) return;
    $work = null;
    if ($path === 'art-archive') {
        list($types, $type, $page) = tl_quality_archive_selection();
        $count = count(array_filter(tl_quality_rows(), function ($row) use ($type) { return $row['type'] === $type; }));
        if ($page > max(1, (int) ceil($count / 30))) return;
    }
    if ($path !== 'art-archive') {
        $slug = substr($path, strlen('artwork/'));
        foreach (tl_quality_rows() as $row) {
            if ($row['type'] === 'Artwork' && tl_quality_art_slug($row) === $slug) { $work = $row; break; }
        }
        if (!$work) return; // Unknown and withdrawn artwork URLs retain a real 404.
    }
    $GLOBALS['tl_quality_route'] = array('kind' => $work ? 'artwork' : 'archive', 'work' => $work);
    global $wp_query; $wp_query->is_404 = false;
    status_header(200); header('Cache-Control: no-store');
    remove_action('template_redirect', 'redirect_canonical');
    remove_action('wp_head', 'rel_canonical');
    $title = $work ? tl_quality_display($work)['title'] : 'Archive';
    add_filter('pre_get_document_title', function () use ($title) { return $title . ' – Thomas Lawson'; }, 99);
    add_filter('body_class', function ($classes) {
        return array_merge(array_diff($classes, array('error404', 'ast-separate-container', 'ast-two-container')), array('page','ast-page-builder-template','tl-quality-route'));
    }, 99);
    get_header();
    echo '<div id="primary" class="content-area primary"><main id="main" class="site-main tl-quality-archive">';
    if ($work) tl_quality_render_work($work); else tl_quality_render_archive();
    echo '</main></div>'; get_footer(); exit;
}
add_action('template_redirect', 'tl_quality_route', -20);

function tl_quality_render_work($row) {
    $work = tl_quality_display($row);
    echo '<article class="tl-quality-work"><figure>';
    if (!empty($row['image'])) {
        $image = function_exists('tl_refresh_valise_size') ? tl_refresh_valise_size($row['image'], 1600) : $row['image'];
        echo '<a class="tl-zoom" href="' . esc_url($image) . '" data-caption="' . esc_attr($work['title'] . ($work['detail'] ? ', ' . $work['detail'] : '')) . '"><img src="' . esc_url($image) . '" alt="' . esc_attr($work['title']) . '" decoding="async" fetchpriority="high"></a>';
    }
    echo '</figure><div><h1>' . esc_html($work['title']) . '</h1><p class="tl-quality-credit">Thomas Lawson</p>';
    if ($work['detail']) echo '<p>' . esc_html($work['detail']) . '</p>';
    echo '<nav aria-label="Artwork context"><a href="' . esc_url($row['url']) . '">View in the studio archive</a><a href="' . esc_url(home_url('/art-archive/')) . '">All artworks</a></nav></div></article>';
}
function tl_quality_archive_url($type, $page = 1) {
    $args = array();
    if ($type !== 'Artwork') $args['type'] = $type;
    if ($page > 1) $args['pg'] = $page;
    return $args ? add_query_arg($args, home_url('/art-archive/')) : home_url('/art-archive/');
}
function tl_quality_archive_selection() {
    $types = array('Artwork'=>'Artworks','Writing'=>'Writing','Exhibition'=>'Exhibitions','Project'=>'Projects','Page'=>'Pages');
    $type = isset($_GET['type']) && is_string($_GET['type']) ? sanitize_text_field(wp_unslash($_GET['type'])) : 'Artwork';
    if (!isset($types[$type])) $type = 'Artwork';
    $page = isset($_GET['pg']) && is_scalar($_GET['pg']) ? max(1, absint($_GET['pg'])) : 1;
    return array($types, $type, $page);
}
function tl_quality_render_archive() {
    list($types, $type, $page) = tl_quality_archive_selection();
    $rows = array_values(array_filter(tl_quality_rows(), function ($r) use ($type) { return $r['type'] === $type; }));
    $size = 30; $pages = max(1, (int) ceil(count($rows) / $size)); $page = min($page, $pages);
    echo '<h1>Archive</h1><nav aria-label="Archive categories">';
    foreach ($types as $key=>$label) echo '<a href="' . esc_url(tl_quality_archive_url($key)) . '"' . ($type === $key ? ' aria-current="page"' : '') . '>' . esc_html($label) . '</a>';
    echo '</nav><ul class="tl-quality-records">';
    foreach (array_slice($rows, ($page - 1) * $size, $size) as $row) {
        $display = tl_quality_display($row); $url = tl_quality_public_url($row);
        echo '<li><a href="' . esc_url($url) . '">';
        if (!empty($row['image'])) echo '<img src="' . esc_url($row['image']) . '" alt="" loading="lazy" decoding="async">';
        echo '<h2>' . esc_html($display['title']) . '</h2></a>';
        if ($display['detail']) echo '<p>' . esc_html($display['detail']) . '</p>';
        echo '</li>';
    }
    echo '</ul><nav aria-label="Archive pages">';
    for ($n=1; $n<=$pages; $n++) echo '<a href="' . esc_url(tl_quality_archive_url($type,$n)) . '" aria-label="Page ' . $n . '"' . ($n === $page ? ' aria-current="page"' : '') . '>' . $n . '</a>';
    echo '</nav>';
}

function tl_quality_meta() {
    if (is_admin() || is_feed() || is_404()) return;
    $route = isset($GLOBALS['tl_quality_route']) ? $GLOBALS['tl_quality_route'] : null;
    $title = wp_get_document_title(); $url = ''; $description = ''; $image = ''; $entity = null;
    $person = array('@type'=>'Person','@id'=>home_url('/#artist'),'name'=>'Thomas Lawson','url'=>home_url('/'),'jobTitle'=>array('Artist','Writer'));
    if ($route && $route['work']) {
        $raw = $route['work']; $work = tl_quality_display($raw);
        $url = tl_quality_art_url($raw); $image = $raw['image'];
        $description = $work['title'] . ' by Thomas Lawson' . ($work['detail'] ? '. ' . $work['detail'] : '.');
        $entity = array('@type'=>'VisualArtwork','@id'=>$url.'#artwork','url'=>$url,'name'=>$work['title'],'creator'=>array('@id'=>$person['@id']),'image'=>$image,'description'=>$description,'isPartOf'=>array('@id'=>home_url('/art-archive/')));
        if (preg_match('/^((?:19|20)\d{2})(?:\b|\s)/', $work['detail'], $match)) $entity['dateCreated'] = $match[1];
    } elseif ($route) {
        list($types,$type,$page) = tl_quality_archive_selection();
        $url = tl_quality_archive_url($type,$page);
        $description = 'Browse ' . strtolower($types[$type]) . ' in the Thomas Lawson archive.';
    } elseif (is_front_page()) {
        $url = home_url('/'); $description = 'Artworks, writing, exhibitions and projects by artist Thomas Lawson.';
    } elseif (is_singular()) {
        $url = get_permalink();
        foreach (tl_quality_rows() as $row) {
            if ($row['url'] === $url && in_array($row['type'], array('Page','Project'), true)) {
                $description = trim($row['text']); $image = $row['image']; break;
            }
        }
        if (!$description) $description = wp_strip_all_tags(get_the_excerpt());
    } elseif (function_exists('tl_refresh_cv_slug') && tl_refresh_cv_slug()) {
        $url = home_url('/' . tl_refresh_cv_slug() . '/');
        $description = $title . '. Artworks and exhibition records from the Thomas Lawson archive.';
        echo '<link rel="canonical" href="' . esc_url($url) . '">' . "\n";
    }
    if (!$url) return;
    $description = trim(preg_replace('/\s+/u', ' ', html_entity_decode(wp_strip_all_tags($description), ENT_QUOTES | ENT_HTML5, 'UTF-8')));
    if (mb_strlen($description)>180) $description = rtrim(mb_substr($description,0,177)) . '…';
    if ($route) echo '<link rel="canonical" href="' . esc_url($url) . '">' . "\n";
    if (!defined('WPSEO_VERSION') && !defined('RANK_MATH_VERSION')) {
        if ($description) echo '<meta name="description" content="' . esc_attr($description) . '">' . "\n";
        foreach (array('og:title'=>$title,'og:description'=>$description,'og:url'=>$url,'og:type'=>'website','og:site_name'=>'Thomas Lawson','og:image'=>$image) as $key=>$value) {
            if ($value) echo '<meta property="' . esc_attr($key) . '" content="' . esc_attr($value) . '">' . "\n";
        }
    }
    $page = array('@type'=>$route && !$route['work'] ? 'CollectionPage' : 'WebPage','@id'=>$url,'url'=>$url,'name'=>$title,'description'=>$description,'isPartOf'=>array('@id'=>home_url('/#website')),'about'=>array('@id'=>$person['@id']));
    if ($entity) $page['mainEntity'] = array('@id'=>$entity['@id']);
    $graph = array($person,array('@type'=>'WebSite','@id'=>home_url('/#website'),'url'=>home_url('/'),'name'=>'Thomas Lawson','publisher'=>array('@id'=>$person['@id'])),$page);
    if ($entity) $graph[]=$entity;
    echo '<script type="application/ld+json" id="tl-quality-schema">' . wp_json_encode(array('@context'=>'https://schema.org','@graph'=>$graph), JSON_HEX_TAG|JSON_HEX_AMP|JSON_HEX_APOS|JSON_HEX_QUOT|JSON_UNESCAPED_SLASHES) . '</script>' . "\n";
}
add_filter('robots_txt', function ($text, $public) {
    return $public ? rtrim($text) . "\nSitemap: " . home_url('/tl-artwork-sitemap.xml') . "\n" : $text;
}, 20, 2);

// Register last so these assets follow the installed v1.8.0 and closeout layers.
add_action('wp_loaded', function () {
    add_action('wp_head','tl_quality_meta',99);
    add_action('wp_head',function () {
        echo <<<'TLQUALITYCSS'
<style id="tl-quality-css">
.tl-quality-sr{position:absolute!important;width:1px!important;height:1px!important;padding:0!important;margin:-1px!important;overflow:hidden!important;clip:rect(0,0,0,0)!important;white-space:nowrap!important;border:0!important}
.tl-quality :focus-visible{outline:2px solid #35312a!important;outline-offset:5px!important}
.tl-quality-home-title{margin:0!important;padding:0!important;font:inherit!important;line-height:0!important}
.tl-quality .tl-feature-controls button,.tl-quality .tl-panel-close,.tl-quality .tl-header-search{min-width:44px;min-height:44px}
.tl-quality-carousel .tl-feature-art{display:block;touch-action:pan-y}
.tl-quality-carousel .tl-feature-art img{width:100%;height:auto;max-height:none;object-fit:contain}
.tl-quality-carousel .tl-feature-foot{gap:16px;flex-wrap:wrap;align-items:flex-start}
.tl-quality-carousel .tl-feature-caption{min-width:0;flex:1 1 180px}
.tl-quality-carousel .tl-feature-controls{flex:0 0 auto;gap:4px}
.tl-quality-carousel .tl-quality-play{font:12px Inter,sans-serif!important;letter-spacing:0!important;padding:0 8px!important}
.tl-quality .tl-feature-detail{overflow-wrap:anywhere}
.tl-quality-image-button{display:block!important;background:none!important;border:0!important;border-radius:0!important;padding:0!important;width:100%;color:inherit;cursor:zoom-in}
.tl-quality-image-button img{display:block;width:100%;height:auto}
.tl-quality-viewer{box-sizing:border-box;width:min(1400px,96vw);max-width:96vw;height:94dvh;max-height:94dvh;margin:auto;padding:60px 20px 20px;background:#fff9f0;color:#292620;border:0;overflow:auto}
.tl-quality-viewer::backdrop{background:rgba(20,18,15,.9)}
.tl-quality-viewer figure{display:flex;flex-direction:column;align-items:center;justify-content:center;min-height:100%;margin:0;gap:14px}
.tl-quality-viewer img{object-fit:contain;max-width:100%;max-height:calc(94dvh - 160px);width:auto;height:auto}
.tl-quality-viewer figcaption{font:16px/1.5 Inter,sans-serif;text-align:center;max-width:75ch}
.tl-quality-viewer-close{position:absolute;right:16px;top:10px;min-height:44px;min-width:64px;background:transparent;color:#292620;border:1px solid currentColor;padding:8px 16px;font:16px Inter,sans-serif}
.tl-quality-viewer-open{overflow:hidden}
.tl-quality-archive{max-width:1188px;margin:40px auto 80px;padding:0 28px}
.tl-quality-archive h1{font:400 clamp(32px,5vw,60px)/1.1 Inter,sans-serif;margin:0 0 24px}
.tl-quality-archive nav{display:flex;flex-wrap:wrap;gap:10px 22px;margin:24px 0}
.tl-quality-archive nav a{display:inline-flex;align-items:center;min-height:44px;text-underline-offset:5px}
.tl-quality-archive nav a[aria-current]{text-decoration:underline;font-weight:600}
.tl-quality-records{list-style:none!important;margin:32px 0!important;padding:0!important;display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:44px 30px}
.tl-quality-records li{margin:0;min-width:0;overflow-wrap:anywhere}
.tl-quality-records img{display:block;width:100%;height:250px;object-fit:contain;object-position:left bottom;margin:0 0 16px}
.tl-quality-records h2{font:400 22px/1.25 Newsreader,serif;margin:0 0 8px}
.tl-quality-records p{font:14px/1.5 Inter,sans-serif;margin:0;color:#575047}
.tl-quality-work{display:grid;grid-template-columns:minmax(0,1.7fr) minmax(240px,1fr);gap:50px;align-items:start}
.tl-quality-work>figure{margin:0;min-width:0}
.tl-quality-work>figure img{width:100%;height:auto;max-height:75vh;object-fit:contain}
.tl-quality-work p{font:18px/1.6 Newsreader,serif}
.tl-quality-work .tl-quality-credit{font:16px/1.5 Inter,sans-serif}
.tl-quality-work a{display:inline-block;min-height:44px;text-decoration:underline;text-underline-offset:4px}
@media(max-width:600px){.tl-quality-carousel .tl-feature-foot{gap:8px}.tl-quality-carousel .tl-feature-caption{flex-basis:100%}.tl-quality-carousel .tl-feature-controls{margin-left:auto}.tl-quality-archive{padding:0 20px;margin-top:24px}.tl-quality-records{grid-template-columns:repeat(2,minmax(0,1fr));gap:30px 18px}.tl-quality-records img{height:170px}.tl-quality-records h2{font-size:20px}.tl-quality-work{grid-template-columns:1fr;gap:26px}.tl-quality-viewer{padding-left:12px;padding-right:12px}}
@media(prefers-reduced-motion:reduce){.tl-quality *,.tl-quality *::before,.tl-quality *::after{scroll-behavior:auto!important;transition:none!important;animation:none!important}}

</style>
TLQUALITYCSS;
    },PHP_INT_MAX);
    add_action('wp_footer',function () {
        if (is_front_page()) {
            $urls = array();
            foreach (tl_quality_rows() as $row) {
                if ($row['type'] !== 'Artwork' || empty($row['image'])) continue;
                $file = basename((string) parse_url($row['image'], PHP_URL_PATH));
                $urls[$row['title'].'|'.$file] = tl_quality_art_url($row);
            }
            echo '<script>window.TL_QUALITY_ARTWORK_URLS=' . wp_json_encode($urls,JSON_HEX_TAG|JSON_HEX_AMP|JSON_HEX_APOS|JSON_HEX_QUOT) . ';</script>';
        }
        echo <<<'TLQUALITYJS'
<script id="tl-quality-js">
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

</script>
TLQUALITYJS;
    },PHP_INT_MAX);
});
