import { readFile, writeFile } from 'node:fs/promises';
const dir = new URL('./', import.meta.url);
const js = await readFile(new URL('closeout.js', dir), 'utf8');
const css = await readFile(new URL('closeout.css', dir), 'utf8');
await writeFile(new URL('zzz-tl-closeout.php', dir), `<?php
/**
 * Plugin Name: TL — October closeout
 * Description: Portrait of New York film, News credits/dates, REALLIFE 15 issue link.
 * Version: 1.0.0
 * Source: gigs/thomaslawson.com/work-closeout-2026-10-04/
 */
if (!defined('ABSPATH')) exit;
add_action('wp_head', function () {
    if (!is_page(array(1622, 1898, 1819, 808))) return;
    echo <<<'TLCLOSECSS'
<style id="tl-closeout-css">
${css}</style>
TLCLOSECSS;
}, PHP_INT_MAX);
add_action('wp_footer', function () {
    if (!is_page(array(1622, 1898, 1819, 808))) return;
    echo <<<'TLCLOSEJS'
<script id="tl-closeout-js">
${js}</script>
TLCLOSEJS;
}, PHP_INT_MAX);
`);
