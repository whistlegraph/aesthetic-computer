import { readFile, writeFile } from 'node:fs/promises';
const root = new URL('./', import.meta.url);
const css = await readFile(new URL('layout.css', root), 'utf8');
const js = await readFile(new URL('layout.js', root), 'utf8');
await writeFile(new URL('zzzzzz-tl-layout.php', root), `<?php
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
${css}</style>
TLLAYOUTCSS;
}, PHP_INT_MAX);
// Register after the existing October/followup footer passes.
add_action('wp_loaded', function () {
    add_action('wp_footer', function () {
        if (!tl_october_layout_target()) return;
        echo <<<'TLLAYOUTJS'
<script id="tl-layout-js">
${js}</script>
TLLAYOUTJS;
    }, PHP_INT_MAX);
});
`);
