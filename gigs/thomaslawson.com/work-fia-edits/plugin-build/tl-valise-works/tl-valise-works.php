<?php
/**
 * Plugin Name: TL — Valise works
 * Plugin URI:  https://www.thomaslawson.com/
 * Description: [valise_works] shortcode — a live, cached grid of Thomas's Valise
 *   artworks filtered by tag, styled to match the In-the-Studio decade pages.
 *   Data is fetched server-side from api.valise.works and cached in a transient;
 *   the API key never reaches the browser. As Thomas tags or re-shoots work in
 *   Valise, the page follows on the next cache cycle. Set the key under
 *   Settings → Valise (or define VALISE_API_KEY in wp-config.php).
 * Version: 0.1.0
 * Author: Aesthetic Computer
 * Requires at least: 5.5
 *
 * Usage:  [valise_works tag="website-P-1980s"]
 *         [valise_works tag="website-P-2000s" cache="3600"]
 *
 * An admin viewing a page can append ?valise_refresh=1 to bypass the cache once.
 */

if (!defined('ABSPATH')) { exit; }

define('TL_VALISE_VER', '0.1.0');
define('TL_VALISE_API_BASE', 'https://api.valise.works/v0');

/* ------------------------------------------------------------------ *
 * Key handling — server-side only, never echoed.
 * ------------------------------------------------------------------ */

function tl_valise_api_key() {
    if (defined('VALISE_API_KEY') && VALISE_API_KEY) { return VALISE_API_KEY; }
    return (string) get_option('tl_valise_api_key', '');
}

function tl_valise_key_source() {
    if (defined('VALISE_API_KEY') && VALISE_API_KEY) { return 'wp-config constant'; }
    if (get_option('tl_valise_api_key', '')) { return 'saved setting'; }
    return '';
}

/* ------------------------------------------------------------------ *
 * Data — fetch every artwork carrying $tag, following pagination.
 * (The API tag query-param did not filter server-side in testing, so
 *  we page the vault and match tags[] here. Caller caches the result.)
 * ------------------------------------------------------------------ */

function tl_valise_fetch_by_tag($tag) {
    $key = tl_valise_api_key();
    if (!$key) { return new WP_Error('tl_valise_no_key', 'Valise API key is not configured (Settings → Valise).'); }

    $out   = array();
    $url   = TL_VALISE_API_BASE . '/artworks?limit=100';
    $guard = 0;

    while ($url && $guard < 25) {
        $resp = wp_remote_get($url, array(
            'headers' => array(
                'Authorization' => 'Bearer ' . $key,
                'Accept'        => 'application/json',
            ),
            'timeout' => 20,
        ));
        if (is_wp_error($resp)) { return $resp; }

        $status = wp_remote_retrieve_response_code($resp);
        if ($status !== 200) {
            return new WP_Error('tl_valise_http_' . $status, 'Valise API returned HTTP ' . $status . '.');
        }

        $body = json_decode(wp_remote_retrieve_body($resp), true);
        if (empty($body['data']) || !is_array($body['data'])) { break; }

        foreach ($body['data'] as $w) {
            if (empty($w['tags']) || !is_array($w['tags'])) { continue; }
            foreach ($w['tags'] as $t) {
                $tt = isset($t['title']) ? $t['title'] : '';
                $ts = isset($t['slug'])  ? $t['slug']  : '';
                if ($tt === $tag || $ts === $tag) { $out[] = $w; break; }
            }
        }

        $url = isset($body['page']['next']) ? $body['page']['next'] : null;
        $guard++;
    }

    usort($out, function ($a, $b) {
        $ay = isset($a['year']) ? (string) $a['year'] : '';
        $by = isset($b['year']) ? (string) $b['year'] : '';
        if ($ay === $by) {
            return strcmp(isset($a['title']) ? $a['title'] : '', isset($b['title']) ? $b['title'] : '');
        }
        return strcmp($ay, $by);
    });

    return $out;
}

function tl_valise_img_url($work, $width = 900) {
    if (empty($work['images'][0]['url'])) { return ''; }
    return preg_replace('#/rs:fit:\d+:\d+/#', '/rs:fit:' . intval($width) . ':0/', $work['images'][0]['url']);
}

function tl_valise_img_tag($work, $alt) {
    $base = tl_valise_img_url($work, 900);
    if (!$base) { return ''; }
    $src600  = tl_valise_img_url($work, 600);
    $src1400 = tl_valise_img_url($work, 1400);
    return sprintf(
        '<img loading="lazy" decoding="async" src="%s" srcset="%s 600w, %s 900w, %s 1400w" sizes="(max-width:640px) 90vw, (max-width:1024px) 45vw, 300px" alt="%s">',
        esc_url($base), esc_url($src600), esc_url($base), esc_url($src1400), esc_attr($alt)
    );
}

/* ------------------------------------------------------------------ *
 * Presentation
 * ------------------------------------------------------------------ */

function tl_valise_print_css_once() {
    static $done = false;
    if ($done) { return ''; }
    $done = true;
    return <<<CSS
<style id="tl-valise-css">
.tl-valise{--vpanel:#fdf4e6;--vmuted:#6f6a61;--vline:#e7e0d1;margin:clamp(1.4rem,4vw,2.6rem) 0 0}
.tl-valise .vgrid{display:grid;grid-template-columns:repeat(auto-fill,minmax(230px,1fr));
  gap:clamp(1.6rem,3.2vw,2.8rem) clamp(1.3rem,2.6vw,2rem);margin:0;padding:0;list-style:none}
.tl-valise .vwork{margin:0;display:flex;flex-direction:column}
.tl-valise figure{margin:0}
.tl-valise .vframe{background:var(--vpanel);border:1px solid var(--vline);aspect-ratio:1/1;
  display:flex;align-items:center;justify-content:center;overflow:hidden}
.tl-valise .vframe img{max-width:100%;max-height:100%;width:auto;height:auto;display:block}
.tl-valise .vframe.is-empty{color:var(--vmuted);font-size:.78rem;letter-spacing:.05em;text-transform:uppercase}
.tl-valise figcaption{display:flex;flex-direction:column;gap:.1rem;padding:.7rem .1rem 0}
.tl-valise .vt{font-weight:600;font-size:.82rem;letter-spacing:.05em;text-transform:uppercase;line-height:1.25}
.tl-valise .vy{color:var(--vmuted);font-size:.92rem}
.tl-valise .vm{color:var(--vmuted);font-style:italic;font-size:.88rem;margin-top:.15rem;line-height:1.4}
.tl-valise .vempty{color:var(--vmuted);font-style:italic}
</style>
CSS;
}

function tl_valise_works_shortcode($atts) {
    $a = shortcode_atts(array(
        'tag'   => '',
        'cache' => '3600',
    ), $atts, 'valise_works');

    $tag = sanitize_text_field($a['tag']);
    if (!$tag) { return ''; }
    $ttl = max(0, intval($a['cache']));

    $ck     = 'tl_valise_' . md5($tag . '|' . TL_VALISE_VER);
    $bust   = current_user_can('manage_options') && isset($_GET['valise_refresh']);
    $cached = $bust ? false : get_transient($ck);
    if ($cached !== false) { return $cached; }

    $works = tl_valise_fetch_by_tag($tag);
    if (is_wp_error($works)) {
        return current_user_can('manage_options')
            ? '<p class="tl-valise vempty">Valise: ' . esc_html($works->get_error_message()) . '</p>'
            : '';
    }
    if (empty($works)) {
        return current_user_can('manage_options')
            ? '<p class="tl-valise vempty">Valise: no works tagged &ldquo;' . esc_html($tag) . '&rdquo;.</p>'
            : '';
    }

    ob_start();
    echo tl_valise_print_css_once();
    echo '<div class="tl-valise"><ul class="vgrid">';
    foreach ($works as $w) {
        $title  = (isset($w['title']) && $w['title'] !== '') ? $w['title'] : 'Untitled';
        $year   = isset($w['year']) ? $w['year'] : '';
        $med    = isset($w['medium']) ? trim($w['medium']) : '';
        $dim    = isset($w['dimensions']) ? trim($w['dimensions']) : '';
        $sub    = trim(implode(' · ', array_filter(array($med, $dim))));
        $imgtag = tl_valise_img_tag($w, $title . ($year ? ', ' . $year : ''));

        echo '<li class="vwork"><figure>';
        echo $imgtag ? '<div class="vframe">' . $imgtag . '</div>' : '<div class="vframe is-empty">image to come</div>';
        echo '<figcaption><span class="vt">' . esc_html($title) . '</span>';
        if ($year !== '') { echo '<span class="vy">' . esc_html($year) . '</span>'; }
        if ($sub !== '')  { echo '<span class="vm">' . esc_html($sub) . '</span>'; }
        echo '</figcaption></figure></li>';
    }
    echo '</ul></div>';
    $html = ob_get_clean();

    if ($ttl) { set_transient($ck, $html, $ttl); }
    return $html;
}
add_shortcode('valise_works', 'tl_valise_works_shortcode');

/* ------------------------------------------------------------------ *
 * Settings — Settings → Valise (one field: the API key)
 * ------------------------------------------------------------------ */

add_action('admin_menu', function () {
    add_options_page('Valise', 'Valise', 'manage_options', 'tl-valise', 'tl_valise_settings_page');
});

add_action('admin_init', function () {
    register_setting('tl_valise_group', 'tl_valise_api_key', array(
        'type'              => 'string',
        'sanitize_callback' => function ($v) { return trim(sanitize_text_field($v)); },
        'default'           => '',
    ));
});

add_filter('plugin_action_links_' . plugin_basename(__FILE__), function ($links) {
    $links[] = '<a href="' . esc_url(admin_url('options-general.php?page=tl-valise')) . '">Settings</a>';
    return $links;
});

function tl_valise_settings_page() {
    if (!current_user_can('manage_options')) { return; }
    $source = tl_valise_key_source();
    $via_constant = (defined('VALISE_API_KEY') && VALISE_API_KEY);
    ?>
    <div class="wrap">
      <h1>Valise</h1>
      <p>Connects the site to Thomas&rsquo;s Valise vault. Drop
        <code>[valise_works tag="website-P-1980s"]</code> onto any page to render a
        live, cached grid of the works carrying that tag.</p>
      <p>Status:
        <?php if ($source): ?>
          <strong style="color:#2e7d32">key configured</strong> (<?php echo esc_html($source); ?>).
        <?php else: ?>
          <strong style="color:#a33">no key set</strong> &mdash; paste it below.
        <?php endif; ?>
      </p>
      <form method="post" action="options.php">
        <?php settings_fields('tl_valise_group'); ?>
        <table class="form-table" role="presentation">
          <tr>
            <th scope="row"><label for="tl_valise_api_key">Valise API key</label></th>
            <td>
              <input name="tl_valise_api_key" id="tl_valise_api_key" type="password"
                     autocomplete="off" spellcheck="false" style="width:32rem;max-width:100%"
                     value="<?php echo esc_attr(get_option('tl_valise_api_key', '')); ?>"
                     <?php echo $via_constant ? 'disabled' : ''; ?> />
              <p class="description">
                <?php if ($via_constant): ?>
                  A <code>VALISE_API_KEY</code> constant in wp-config.php is in use and takes precedence over this field.
                <?php else: ?>
                  Read-only key from Valise → developer settings. Stored server-side; never shown on the site.
                <?php endif; ?>
              </p>
            </td>
          </tr>
        </table>
        <?php if (!$via_constant) { submit_button('Save key'); } ?>
      </form>
      <p class="description">Pages cache for one hour. To force a refresh, open a page with
        <code>?valise_refresh=1</code> while logged in as an admin.</p>
    </div>
    <?php
}
