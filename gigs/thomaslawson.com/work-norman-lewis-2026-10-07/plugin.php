<?php
/**
 * Plugin Name: TL — Norman Lewis retrospective
 * Description: Exhibition page, context index entry and site search record.
 * Version: 1.0.0
 */
if (!defined('ABSPATH')) exit;
function tl_norman_content() { return <<<'TLNORMANCONTENT'
__CONTENT__
TLNORMANCONTENT;
}
function tl_norman_card() { return <<<'TLNORMANCARD'
__CARD__
TLNORMANCARD;
}
add_action('template_redirect', function () {
    if (trim(parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH), '/') !== 'art-in-context-norman-lewis') return;
    global $wp_query;
    $wp_query->is_404 = false;
    status_header(200);
    remove_action('template_redirect', 'redirect_canonical');
    remove_action('wp_head', 'rel_canonical');
    add_filter('pre_get_document_title', function () { return 'Norman Lewis: A Retrospective – Thomas Lawson'; });
    add_filter('body_class', function ($classes) { $classes[] = 'tl-norman-page'; return $classes; });
    add_action('wp_head', function () { echo '<link rel="canonical" href="'.esc_url(home_url('/art-in-context-norman-lewis/')).'">'; });
    get_header();
    echo '<div id="primary" class="content-area primary">'.tl_norman_content().'</div>';
    get_footer();
    exit;
}, -20);
add_filter('the_content', function ($content) {
    if (!is_admin() && is_main_query() && in_the_loop() && is_page(1147) && strpos($content, 'id="tl-norman-index-entry"') === false) $content .= tl_norman_card();
    return $content;
}, 200);
add_filter('rest_post_dispatch', function ($response, $server, $request) {
    if ($request->get_route() !== '/tl/v1/search-index' || is_wp_error($response) || $response->get_status() !== 200) return $response;
    $data = $response->get_data();
    if (!isset($data['items'])) return $response;
    $url = home_url('/art-in-context-norman-lewis/');
    foreach ($data['items'] as $item) if (isset($item['url']) && $item['url'] === $url) return $response;
    $data['items'][] = array('title' => 'Norman Lewis: A Retrospective', 'type' => 'Project', 'url' => $url,
        'image' => home_url('/wp-content/uploads/tl-refresh/norman-lewis-1976/installation-2.png'),
        'detail' => 'October 12 – November 19, 1976 · CUNY Graduate Center, New York · Curated by Thomas Lawson',
        'text' => json_decode(<<<'TLNORMANSEARCH'
__SEARCH_TEXT__
TLNORMANSEARCH
        , true));
    $response->set_data($data);
    return $response;
}, 60, 3);
add_action('wp_head', function () {
    echo <<<'TLNORMANCSS'
<style id="tl-norman-css">
__CSS__
</style>
TLNORMANCSS;
}, 200);
add_action('wp_footer', function () {
    if (!is_page(1147)) return;
    echo <<<'TLNORMANJS'
<script id="tl-norman-js">
__JS__
</script>
TLNORMANJS;
}, 1000);
