<?php
/**
 * Plugin Name: AC Wrapper Links
 * Description: Restores Elementor Pro wrapper_link functionality for sections/columns.
 * Version: 1.3
 * Author: Aesthetic Computer
 */

if (!defined('ABSPATH')) exit;

add_action('wp_footer', 'ac_wrapper_links_footer');

function ac_wrapper_links_footer() {
    if (!is_singular()) return;
    
    $post_id = get_the_ID();
    $data = get_post_meta($post_id, '_elementor_data', true);
    if (empty($data)) return;
    
    $elements = json_decode($data, true);
    if (!is_array($elements)) return;
    
    $links = [];
    ac_find_wrapper_links($elements, $links);
    
    if (empty($links)) return;
    
    $json = json_encode($links);
    echo "<script>
    (function() {
        var links = $json;
        Object.keys(links).forEach(function(id) {
            var el = document.querySelector('[data-id=\"' + id + '\"]');
            if (el) {
                el.style.cursor = 'pointer';
                el.addEventListener('click', function(e) {
                    if (e.target.closest('a')) return;
                    window.location.href = links[id];
                });
            }
        });
    })();
    </script>\n";
}

function ac_find_wrapper_links($elements, &$links) {
    foreach ($elements as $el) {
        $settings = $el['settings'] ?? [];
        if (!empty($settings['enable_wrapper_link']) && $settings['enable_wrapper_link'] === 'yes') {
            $url = $settings['wrapper_link']['url'] ?? '';
            if ($url && !empty($el['id'])) {
                // Update http to https
                $url = str_replace('http://', 'https://', $url);
                $links[$el['id']] = $url;
            }
        }
        if (!empty($el['elements'])) {
            ac_find_wrapper_links($el['elements'], $links);
        }
    }
}
