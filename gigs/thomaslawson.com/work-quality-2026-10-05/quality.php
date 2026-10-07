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
/*__CSS__*/
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
/*__JS__*/
</script>
TLQUALITYJS;
    },PHP_INT_MAX);
});
