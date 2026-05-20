<?php
/**
 * Plugin Name: TL — Disable Animations & Reveal Captions
 * Description: Sitewide kill switch for all CSS transitions/animations and Elementor entrance animations. Forces .elementor-invisible content (image captions, headings) to render immediately so it can never be left hidden by a non-firing reveal script.
 * Version: 1.0
 * Author: Aesthetic Computer
 */

if (!defined('ABSPATH')) exit;

/**
 * Printed dead-last in <head> (after Astra inline CSS and the external
 * Elementor stylesheets) so the !important overrides reliably win.
 */
add_action('wp_head', 'tl_no_animations_css', PHP_INT_MAX);

function tl_no_animations_css() {
    ?>
<style id="tl-no-animations">
/* Remove every CSS transition + animation, sitewide. */
*, *::before, *::after {
    -webkit-transition: none !important;
    transition: none !important;
    -webkit-animation: none !important;
    animation: none !important;
    animation-duration: 0s !important;
    transition-duration: 0s !important;
    scroll-behavior: auto !important;
}

/* Elementor entrance animations leave content at opacity:0 / hidden until a
   scroll-triggered script reveals it. With animations disabled that script
   can never run, so force the pre-animation hidden state to be visible.
   This is what makes the image captions appear. */
.elementor-invisible {
    visibility: visible !important;
    opacity: 1 !important;
}

/* Neutralise the WAY-OUT/IN keyframe states some Elementor/Astra builds
   leave applied (transform/clip) when the animation never plays. */
.elementor-element[data-settings*="_animation"],
.elementor-widget[data-settings*="_animation"],
.animated {
    opacity: 1 !important;
    visibility: visible !important;
    transform: none !important;
}
</style>
<?php
}
