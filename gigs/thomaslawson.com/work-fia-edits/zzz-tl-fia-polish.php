<?php
/**
 * Plugin Name: TL — Fía polish pass
 * Description: CSS+JS polish on top of the existing TL theme + Elementor build, per Fía's notes (2026-05-19 + her two replies later that night). Header chrome (no underline / no rule_ hrs / no Home), cream-everywhere, home laid out as a 5-up desktop strip (vertical stack on mobile) in Fía's section order with subtitles, divider widgets dropped on the homepage, Notes image-width capped, In-the-Studio + About years reversed newest-first (with !important on the flex parent so the reorder actually applies), Beyond-the-Studio collapsed to one column with centered subsection labels, 1980-82 caption normalisation, and a JS-injected horizontal cover preview strip per shelf on /bookshelf/.
 * Version: 1.5.2
 *
 * v1.5.2 — Fía's 2026-06-08 catch:
 *  - Mobile header was rendering Tom's wordmark twice — the `.custom-logo`
 *    <img> sat above a redundant `<span class="site-title">Thomas
 *    Lawson</span>` because Astra's own mobile media query
 *    (`@media (max-width:921px) { .site-title { display: block } }`) flipped
 *    the title back on while the logo image stayed visible. Hide the
 *    `.ast-site-title-wrap` (title + description) sitewide so only the
 *    image wordmark renders. Desktop was unaffected — the title was
 *    display:none there already.
 *
 * v1.5.1 — Fía's 2026-05-29 corrections:
 *  - Bookshelf: shelf strips re-centred with the first/last-cover
 *    auto-margin trick (the earlier `justify-content: safe center` was
 *    bailing to flex-start whenever the row was wider than its parent
 *    column, leaving the covers stuck left-aligned).
 *  - About: section 3d58b5f turned out to be two columns (text + image),
 *    not one. Without an explicit `flex-direction: column` on the
 *    container, the image column took 100% width via `order:-1` and
 *    pushed the bio text-editors off the right edge — so Fía saw a
 *    cropped banner with no intro text. About joined the column-stack
 *    rule so text + image stack vertically again. The 16/5 crop also
 *    erased Tom (Group-24.png is a 706×775 portrait) — softened to 16/7
 *    with `object-position: center 30%` so Tom's face stays in frame.
 *    The injected "About" heading now lands at the top of the text
 *    column rather than mid-column.
 *
 * v1.5 — Fía's 2026-05-28 batch:
 *  - Bookshelf: shelf subheadings (Artforum, Afterall, …) bumped up in
 *    size and centred over their cover strip; the strip is centred too
 *    when it fits and left-aligned + horizontally scrollable when it
 *    doesn't. Cover lists for the four shelves backed by a sub-page
 *    (Artforum, Afterall, East of Borneo, Writings About TL) expanded
 *    to the full set scraped from those sub-pages so people can actually
 *    scroll horizontally through every cover.
 *  - Art in a Broader Context: "Familie Beck" tile removed (Fía moved
 *    it into Bookshelf); the JS skips that column when it rebuilds the
 *    grid so the remaining tiles re-flow with no gap.
 *  - About (page-id-68): laid out to mirror News / Bookshelf — header
 *    image stretched into a full-bleed horizontal banner, then a bold
 *    "About" heading injected above the bio, paragraphs underneath.
 *
 * v1.4 — Fía's 2026-05-23 batch:
 *  - Studio detail pages: click an artwork to open it in a lightbox
 *    overlay (vanilla JS, no library).
 *  - Bookshelf: shelf subheadings (Artforum, Afterall, Interviews, …)
 *    standardised to one size, centred above each cover strip, with
 *    the anchor underline removed.
 *  - Footer wordmark dropped sitewide (was homepage-only). Bio stays
 *    bottom-left, copyright bottom-right.
 *  - News + In the Studio overview: full-bleed horizontal header image
 *    matching the look of Bookshelf / Beyond / Art-in-a-Broader-Context.
 *    Bookshelf image crop nudged so the wooden shelf reads (less title
 *    text in frame).
 *  - Art in a Broader Context: tile labels match the Beyond style (solid
 *    black, not gray italic); the year is appended inline as ", 1982-
 *    present" via JS so each tile reads as one label, not two.
 *  - Beyond the Studio: project tiles rebuilt as one flat 2-up grid so
 *    Glasgow Projects + Theatre/Dance/Fashion no longer sit alone in a
 *    half-empty row.
 *  - In the Studio overview: year-range sections scaled down + capped to
 *    a narrower max-width so multiple year groupings read on screen at
 *    once.
 *
 * v1.3 — Fía's 2026-05-22 late batch:
 *  - Bookshelf: "Publications" + "Other writings" category headers
 *    removed so all subheadings (Artforum, Interviews, …) sit at one
 *    level. The intro title is pulled tight against the header image.
 *  - Art in a Broader Context: JS rebuilds the project tiles as a single
 *    reverse-chronological grid (Art School first, Pat Douthwaite last);
 *    pairing is by date, not by original Elementor section.
 *  - Beyond the Studio project labels: solid black, slightly larger,
 *    no italic.
 *  - In the Studio overview: year-range section heading sizing reverted
 *    to the theme default (only the detail pages needed sizing down).
 *  - In the Studio detail pages: an en-dash sits between the two year
 *    headings, so "2017 2020" reads "2017 – 2020".
 *  - Contact: email centred vertically on the page.
 *
 * v1.2 — Fía's 2026-05-22 night batch:
 *  - Homepage: titles + painting slightly larger; veil over the painting
 *    is heavier so the subtitles read.
 *  - Art in a Broader Context: project sections reverse-chronological
 *    (Art School first, Pat Douthwaite last).
 *  - Project tiles on Beyond + Art context: more vertical breathing room
 *    between items so the label clearly belongs to its image.
 *  - Bookshelf: intro title + description left-justified (centre reverted),
 *    spacing tightened.
 *  - Contact page: email centred and wrapped in a `mailto:` link.
 *  - In the Studio overview: year-range section headings sized down.
 *  - In the Studio detail pages: the two year headings now sit inline on
 *    one row, sized down.
 *
 * v1.1 — Fía's 2026-05-22 pm batch:
 *  - Homepage: titles+painting block vertically centred in the viewport.
 *  - Section-page intros now stack: horizontal header image on top, then
 *    section title + description, then content underneath.
 *  - Project labels on Beyond the Studio, Art in a Broader Context, and
 *    News sit ABOVE their image — centred, italicised, smaller — so they
 *    read as project labels, not artwork captions.
 *  - Bookshelf: title, description, and cover strips centred.
 *
 * v1.0 — Fía's 2026-05-22 batch:
 *  - Homepage: section titles enlarged + centred; the painting comes back
 *    behind them (veiled) so the titles overlap it again.
 *  - Header: About/Contact dropped to the logo's vertical centre.
 *  - Section pages: intro blurbs standardised — heading left-aligned, the
 *    intro image cropped to a horizontal band.
 *  - In the Studio detail pages: even gap between stacked artworks.
 *  - Beyond the Studio + Art in a Broader Context: project images cropped
 *    to one uniform shape so each reads as a project window, not an artwork.
 *  - Bookshelf: intro photo cropped horizontal, "Publications" pulled up,
 *    empty "Miscellaneous" shelf hidden, cover strips stacked beneath their
 *    subheading instead of beside it.
 *  - /notes/ hero heading reads "News" once inside the section.
 *
 * v0.9 — Fía's 2026-05-21 late batch:
 *  - Homepage relaid as two halves sharing centre: section titles stacked
 *    on the left, Tom's painting (full opacity) on the right — no overlap.
 *  - Footer bio narrowed and left-aligned to the header logo.
 *  - News page captions folded back beneath their own image, left-aligned.
 *  - In the Studio OVERVIEW reverted to its collage layout (v0.8's single
 *    column was wrong); the single-column artwork stack now applies only
 *    to the year-detail pages (slug inthestudio_YYYY-YYYY).
 *  - Bookshelf: "Other Writings" + Interviews shelves open by default too.
 *
 * v0.8 — Fía's 2026-05-21 evening batch:
 *  - Homepage footer wordmark dropped; bio shuffled to the left edge.
 *  - Top-left logo smaller (185px) and padded inward so it breathes.
 *  - Homepage section order: News first, then In the Studio / Beyond the
 *    Studio / Art in a Broader Context / Bookshelf.
 *  - Background painting confined to a centered band (~56vh) with breadth
 *    around it instead of full-bleed cover.
 *  - News page (page-id-1898): every section forced to a single vertical
 *    column; larger images.
 *  - In the Studio (page-id-140): each year section is one vertical column
 *    of large artworks; captions left-aligned to each image's corner.
 *
 * v0.7 — Fía's 2026-05-21 reply: homepage "Notes" tile renamed to "News"
 *  (heading text swapped in JS) with subtitle "plus selections from the
 *  archive".
 *
 * v0.6 — Fía's 2026-05-21 follow-ups: homepage section order revised to
 *  In the Studio / Beyond the Studio / Art in a Broader Context /
 *  Bookshelf / Notes; subtitles "In the Studio" → "artworks" and
 *  "Beyond the Studio" → "exhibitions and public artworks".
 *
 * v0.5 — Fía's 2026-05-21 reply (homepage pass):
 *  - Five section tiles restacked as one centered vertical column (the
 *    5-up row read cluttered); headings centered.
 *  - Faded background painting (Tom's 2010 "Tree") behind the homepage so
 *    it no longer reads as empty; headings stay legible over a cream veil.
 *  - "Art in a Broader Context" subtitle trimmed to "curatorial projects
 *    and pedagogy"; all subtitles enlarged + full-opacity for legibility.
 *  - Tom's wordmark logo scaled down; About/Contact pinned to the right.
 *
 * v0.4 — Fía's 2026-05-20 reply:
 *  - The line still showing under Tom's name + About + Contact was the Astra
 *    header bottom-border, not an Elementor divider. Removed sitewide so the
 *    name breathes.
 *  - Homepage section tiles drop their cover images down to clean
 *    title + subtitle; JS re-homes each tile's link onto the heading so the
 *    titles (and whole columns) stay clickable.
 *  - /bookshelf/ Publications shelves expanded by default — visible without
 *    clicking the "Publications" header.
 * Author: Aesthetic Computer
 *
 * Notes:
 *  - Deploy via sftp -i ssh/thomaslawson_ed25519 ihfdfni4xqvj@208.109.70.142
 *    into public_html/wp-content/mu-plugins/ alongside zzz-tl-no-animations.php.
 *  - All overrides print at PHP_INT_MAX on wp_head so they beat Astra inline +
 *    Elementor external stylesheets.
 *  - Bookshelf preview strip uses cover images already hosted at
 *    thomaslawson.com/wp-content/uploads/ — no new uploads required.
 */

if (!defined('ABSPATH')) exit;

add_action('wp_head',   'tl_fia_polish_css', PHP_INT_MAX);
add_action('wp_footer', 'tl_fia_polish_js',  PHP_INT_MAX);
add_filter('body_class', 'tl_fia_polish_body_class');

/**
 * Tag In the Studio year-detail pages (slug inthestudio_YYYY-YYYY) with a
 * tl-studio-detail body class so the CSS gives just those pages the
 * single-column artwork stack — the In the Studio overview keeps its
 * collage layout (Fía, 2026-05-21 pm).
 */
function tl_fia_polish_body_class($classes) {
    global $post;
    if ($post && isset($post->post_name) &&
        strpos($post->post_name, 'inthestudio_') === 0) {
        $classes[] = 'tl-studio-detail';
    }
    return $classes;
}

function tl_fia_polish_css() {
    ?>
<style id="tl-fia-polish">
/* ---------------------------------------------------------------- *
 * 1. Header chrome — sitewide
 * ---------------------------------------------------------------- */
.ast-primary-header-bar,
.ast-mobile-header-wrap .ast-mobile-header-content,
.ast-desktop-header-content {
    background-color: #fff9ef !important;
}

.site-header .header_rule,
.site-header .rule_1,
.site-header .rule_2,
.site-header .ast-header-html-1,
.site-header .ast-header-html-2,
.site-header .ast-header-html-1 hr,
.site-header .ast-header-html-2 hr {
    display: none !important;
}

.ast-site-identity a,
.site-title a,
.custom-logo-link,
.site-branding a {
    text-decoration: none !important;
    border-bottom: 0 !important;
}

#menu-item-17,
.menu-item-home,
li.menu-item-home {
    display: none !important;
}

/* The single black line spanning under the name + About + Contact was the
   Astra header bottom-border (.ast-primary-header-bar border-bottom:1px
   solid #000), NOT an Elementor divider — so v0.3's divider drop never
   touched it. Remove it sitewide and give the bar a little bottom room so
   the name breathes (Fía, 2026-05-20). */
.ast-primary-header-bar,
.ast-header-break-point .ast-primary-header-bar,
.main-header-bar,
.ast-header-break-point .main-header-bar,
.site-header {
    border-bottom: 0 !important;
}
.ast-primary-header-bar .site-primary-header-wrap {
    padding-bottom: 0.5rem !important;
}

/* About / Contact were floating mid-bar — the right header section spans
   the whole right half but its menu sat at the section's left edge. Pin
   the menu hard into the right corner (Fía, 2026-05-21). */
.ast-desktop-header-content .site-header-primary-section-right,
#ast-desktop-header .site-header-primary-section-right {
    margin-left: auto !important;
    justify-content: flex-end !important;
    padding-right: 2rem !important;
}
.ast-desktop-header-content .main-header-menu {
    gap: 1.5rem;
}

/* Tom's wordmark logo, smaller again + padded inward so it breathes
   (Fía, 2026-05-21 pm). 185px keeps it present without dominating. */
.custom-logo-link,
.custom-logo-link .custom-logo,
#ast-desktop-header .custom-logo {
    max-width: 185px !important;
    height: auto !important;
}
.ast-desktop-header-content .site-header-primary-section-left,
#ast-desktop-header .site-header-primary-section-left,
.ast-desktop-header-content .ast-site-identity {
    padding-left: 2.5rem !important;
}

/* Mobile branding rendered the logotype TWICE — Tom's wordmark image
   (`.custom-logo`) sat directly above a redundant `<span class="site-title">`
   reading "Thomas Lawson" in plain text. Astra's desktop CSS hides the
   site-title (display:none), but its own media query at ≤921px flips it back
   to display:block, producing the doubled header Fía flagged on mobile.
   Hide the title-wrap (site-title + site-description) at every breakpoint —
   the wordmark image is the brand mark and the description is already
   hidden everywhere (Fía, 2026-06-08). */
.ast-site-title-wrap,
.ast-mobile-header-wrap .ast-site-title-wrap,
.ast-mobile-header-wrap .site-title,
.ast-mobile-header-wrap .site-header .site-title,
.ast-mobile-header-wrap .site-description {
    display: none !important;
}

/* About / Contact sit at the logo's vertical centre — the header grid row
   is centred so the menu drops in line with the middle of the wordmark
   (Fía, 2026-05-22). */
.ast-desktop-header-content .ast-builder-grid-row,
#ast-desktop-header .ast-builder-grid-row {
    align-items: center !important;
}
.ast-desktop-header-content .site-header-primary-section-right {
    align-items: center !important;
}
/* Measured: the menu sat ~30px above the logo's vertical centre. Astra
   resets transforms on the menu, so shift the <nav> itself with relative
   positioning — sidesteps Astra's transform/margin rules and the parent's
   flex centring. */
#ast-desktop-header .site-navigation,
.ast-desktop-header-content .site-navigation {
    position: relative !important;
    top: 30px !important;
}

/* ---------------------------------------------------------------- *
 * 2. Home (page-id-10) — section titles centred over the painting
 *    (Fía, 2026-05-22: bring the overlap back; larger, centred titles)
 * ---------------------------------------------------------------- */

/* The painting returns as a veiled backdrop behind the titles — see the
   .tl-home-layout rules below. The page itself just stays cream. */
body.page-id-10 {
    background-color: #fff9ef;
}

/* Drop every horizontal divider widget on the home page — these were
   reading as the "long horizontal line under tom's name". */
body.page-id-10 .elementor-widget-divider {
    display: none !important;
}

/* Drop the cover images from the 5 section tiles — Fía wants a clean
   title + subtitle for each section, not a cluttered cover wall
   (2026-05-20). The headings are plain <h3>s and only the images carried
   the section links, so tl_fia_polish_js_home() below re-homes each
   tile's href onto the heading + whole column to keep them navigable. */
body.page-id-10 [data-elementor-type="wp-page"] .elementor-widget-image {
    display: none !important;
}
body.page-id-10 .tl-home-tile-link,
body.page-id-10 .tl-home-tile-link:visited {
    color: inherit !important;
    text-decoration: none !important;
}
body.page-id-10 .tl-home-tile-link:hover {
    opacity: 0.55;
}
/* The "Notes" heading widget carries an Elementor per-widget margin-top of
   -95px — set so the title floated up onto its (full-bleed) cover image.
   With the cover image hidden that negative margin yanks "Notes" up into
   the header and collapses its tile to zero height. Zero it so the tile
   sits in line with the other four. */
body.page-id-10 .elementor-element-a258823 > .elementor-widget-container {
    margin-top: 0 !important;
}

/* All five tile headings centred + enlarged for readability — the
   vertical stack reads as a clean menu (Fía, 2026-05-22). */
body.page-id-10 .tl-home-titles .elementor-heading-title {
    text-align: center !important;
    font-size: 2.4rem !important;
    line-height: 1.1 !important;
    margin: 0 !important;
}
@media (max-width: 880px) {
    body.page-id-10 .tl-home-titles .elementor-heading-title {
        font-size: 1.7rem !important;
    }
}

/* Section subtitles via ::after on each heading widget data-id. */
body.page-id-10 .elementor-element-04d4a58 .elementor-heading-title::after,
body.page-id-10 .elementor-element-2e51480 .elementor-heading-title::after,
body.page-id-10 .elementor-element-f9919d8 .elementor-heading-title::after,
body.page-id-10 .elementor-element-264abbc .elementor-heading-title::after,
body.page-id-10 .elementor-element-a258823 .elementor-heading-title::after {
    display: block;
    margin-top: 0.35em;
    font-style: italic;
    font-weight: 400;
    font-size: 0.46em;
    line-height: 1.3;
    letter-spacing: 0.01em;
    color: #2e2a24;
}
body.page-id-10 .elementor-element-04d4a58 .elementor-heading-title::after { content: "curatorial projects and pedagogy"; }
body.page-id-10 .elementor-element-2e51480 .elementor-heading-title::after { content: "writings and publications"; }
body.page-id-10 .elementor-element-f9919d8 .elementor-heading-title::after { content: "exhibitions and public artworks"; }
body.page-id-10 .elementor-element-264abbc .elementor-heading-title::after { content: "artworks"; }
body.page-id-10 .elementor-element-a258823 .elementor-heading-title::after { content: "plus selections from the archive"; }

/* Fía (2026-05-22): the painting comes back behind the titles — a single
   centred block, the five section titles stacked and overlaid on a veiled
   crop of Tom's 2010 "Tree". tl_fia_polish_js_home() builds .tl-home-layout
   and moves the five tile columns into .tl-home-titles; the now-empty
   original sections are hidden. */
/* The painting+titles block sits vertically centred in the page — Fía
   flagged it was reading a bit above centre (2026-05-22 pm). */
body.page-id-10 [data-elementor-type="wp-page"] {
    min-height: calc(100vh - 230px);
    padding: 2rem 1rem;
    display: flex;
    align-items: center;
    justify-content: center;
}
body.page-id-10 [data-elementor-type="wp-page"] > .elementor-section {
    display: none !important;
}
body.page-id-10 .tl-home-layout {
    position: relative;
    width: 100%;
    max-width: 760px;
    margin: 0 auto;
    padding: 4.2rem 2.6rem;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    background-image:
        linear-gradient(rgba(255, 249, 239, 0.7), rgba(255, 249, 239, 0.7)),
        url('https://www.thomaslawson.com/wp-content/uploads/2022/09/2010_Tree_HR.jpg');
    background-size: cover;
    background-position: center;
    background-repeat: no-repeat;
}
body.page-id-10 .tl-home-titles {
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
    width: 100%;
}
body.page-id-10 .tl-home-titles .elementor-column {
    width: 100% !important;
    max-width: none !important;
    flex: 0 0 auto !important;
    min-width: 0 !important;
    margin: 0 !important;
    cursor: pointer;
}
body.page-id-10 .tl-home-titles .elementor-column .elementor-widget-wrap {
    padding: 0 !important;
}
/* Fía's order: News, In the Studio, Beyond the Studio, Art in a Broader
   Context, Bookshelf (the JS also appends the columns in this order). */
body.page-id-10 .elementor-element-05de656 { order: 1 !important; } /* News */
body.page-id-10 .elementor-element-6b3030e { order: 2 !important; } /* In the Studio */
body.page-id-10 .elementor-element-b1d6555 { order: 3 !important; } /* Beyond the Studio */
body.page-id-10 .elementor-element-623a2c4 { order: 4 !important; } /* Art in a Broader Context */
body.page-id-10 .elementor-element-5e1a885 { order: 5 !important; } /* Bookshelf */
@media (max-width: 880px) {
    body.page-id-10 .tl-home-layout { max-width: 92vw; padding: 2.4rem 1.4rem; }
}

/* ---------------------------------------------------------------- *
 * 2b. Footer — drop the wordmark sitewide, bio shuffles left
 *     (Fía, 2026-05-21 pm + 2026-05-23 — sitewide, not just home)
 * ---------------------------------------------------------------- */
/* The footer is a 4-up Astra grid: [logo][bio][empty][copyright]. Fía
   wants the bottom wordmark gone on every page and the bio pulled to
   the left edge. Hide the logo cell and lay the row out as a simple
   left/right split — bio far-left, copyright far-right. */
.site-footer-section-1 {
    display: none !important;
}
.ast-builder-footer-grid-columns {
    display: flex !important;
    justify-content: space-between !important;
    align-items: flex-start !important;
    gap: 2rem;
    padding-right: 2.5rem;
}
.site-footer-section {
    flex: 0 1 auto !important;
}
/* Astra centres each footer section's widget-area; pin the bio's hard to
   the left and pull it out so its left edge lines up with the header logo
   (Fía, 2026-05-21 pm). Kept narrow so it doesn't reach toward centre. */
.site-footer-section-2 {
    justify-content: flex-start !important;
    text-align: left !important;
    max-width: 32rem;
    margin-left: -22px !important;
}
.site-footer-section-2 *,
.site-footer-section-2 p {
    text-align: left !important;
}
.site-footer-section-2 .footer-widget-area {
    margin-left: 0 !important;
}

/* ---------------------------------------------------------------- *
 * 3. News (page-id-1898) — one vertical column, larger images
 * ---------------------------------------------------------------- */
/* The page's sections pair items in two col-50 columns — Fía found the
   right-hand item hard to see (2026-05-21 pm). Force every container/row
   to stack its columns vertically so every item runs full width. */
/* The intro section (1553c2e) is left alone so it keeps the standardised
   title-left / image-right blurb layout — only the content sections below
   it collapse to a single vertical column. */
body.page-id-1898 .elementor-top-section:not(.elementor-element-1553c2e) .elementor-container,
body.page-id-1898 .elementor-top-section:not(.elementor-element-1553c2e) .elementor-row {
    flex-direction: column !important;
    flex-wrap: nowrap !important;
    align-items: center !important;
}
body.page-id-1898 .elementor-top-section:not(.elementor-element-1553c2e) .elementor-column {
    width: 100% !important;
    max-width: 100% !important;
    flex: 0 0 auto !important;
}
body.page-id-1898 .elementor-top-section:not(.elementor-element-1553c2e) img {
    display: block;
    margin: 0 auto;
    max-width: 820px !important;
    width: 100% !important;
    height: auto !important;
}
/* Captions sit directly beneath their image, left-justified to its edge.
   tl_fia_polish_js_news() folds any caption that lived in a separate
   section back into its image's column; constraining the widget-wrap to
   the image width lands a left-aligned heading at the image's bottom-left
   corner (Fía, 2026-05-21 pm — "Rabkin Prize" had drifted under Tom). */
body.page-id-1898 .elementor-widget-wrap {
    max-width: 820px;
    margin-left: auto !important;
    margin-right: auto !important;
}
body.page-id-1898 .elementor-widget-heading .elementor-heading-title {
    text-align: left !important;
}

/* ---------------------------------------------------------------- *
 * 4. In the Studio (page-id-140) — newest first
 *    Make the wp-page wrap a flex column so child sections honour `order`.
 *    Previous v0.2 set display:flex on the children, not the parent — fixed.
 * ---------------------------------------------------------------- */
body.page-id-140 [data-elementor-type="wp-page"] {
    display: flex !important;
    flex-direction: column !important;
}
body.page-id-140 .elementor-element-71fa6aa { order: -100 !important; }     /* title */
body.page-id-140 .elementor-element-1cd340d { order: 1 !important;  }       /* 2017-2020 */
body.page-id-140 .elementor-element-9a9b63e { order: 2 !important;  }       /* 2015-2016 */
body.page-id-140 .elementor-element-7abd700 { order: 3 !important;  }       /* 2010-2015 */
body.page-id-140 .elementor-element-095af92 { order: 4 !important;  }       /* 2006-2010 */
body.page-id-140 .elementor-element-9ba1261 { order: 5 !important;  }       /* 1999-2006 */
body.page-id-140 .elementor-element-0eecd79 { order: 6 !important;  }       /* 1994-1998 */
body.page-id-140 .elementor-element-4e57981 { order: 7 !important;  }       /* 1991-1993 */
body.page-id-140 .elementor-element-ad0626b { order: 8 !important;  }       /* 1987-1990 */
body.page-id-140 .elementor-element-3638c96 { order: 9 !important;  }       /* 1983-1987 */
body.page-id-140 .elementor-element-309b9c7 { order: 10 !important; }       /* 1980-1982 */
body.page-id-140 .elementor-element-322cb6e { order: 11 !important; }       /* 1977-1979 */

/* NOTE: v0.8 forced the overview's year sections into a single column —
   Fía liked the original collage feel of each section, so that change is
   reverted. The single-column artwork stack now lives on the year-detail
   pages only (section 4b below). */

/* ---------------------------------------------------------------- *
 * 4b. In the Studio year-detail pages (slug inthestudio_YYYY-YYYY)
 *     — single vertical column of large artworks (Fía, 2026-05-21 pm)
 * ---------------------------------------------------------------- */
/* The tl-studio-detail body class is added in PHP by post-slug prefix.
   Each section pairs two col-50 columns (image + two caption headings);
   stacking the columns gives one tall column, image then captions. */
body.tl-studio-detail .elementor-container,
body.tl-studio-detail .elementor-row {
    flex-direction: column !important;
    flex-wrap: nowrap !important;
    align-items: center !important;
}
body.tl-studio-detail .elementor-column {
    width: 100% !important;
    max-width: 100% !important;
    flex: 0 0 auto !important;
}
/* Constrain each column's widget-wrap to the artwork width so a
   left-aligned caption lands at the image's bottom-left corner. */
body.tl-studio-detail .elementor-column > .elementor-widget-wrap {
    max-width: 900px;
    margin-left: auto !important;
    margin-right: auto !important;
}
body.tl-studio-detail .elementor-widget-image,
body.tl-studio-detail .elementor-widget-image img {
    width: 100% !important;
    max-width: 100% !important;
    height: auto !important;
}
body.tl-studio-detail .elementor-widget-heading .elementor-heading-title {
    text-align: left !important;
}
/* Even gap between stacked artworks — the per-section spacing varied
   (Fía, 2026-05-22). One uniform margin, inline padding zeroed. */
body.tl-studio-detail .elementor-top-section {
    margin-top: 0 !important;
    margin-bottom: 2.6rem !important;
    padding-top: 0 !important;
    padding-bottom: 0 !important;
}
/* Artwork images are clickable to open a lightbox (Fía, 2026-05-23). */
body.tl-studio-detail .elementor-widget-image img {
    cursor: zoom-in;
    transition: opacity 0.15s;
}
body.tl-studio-detail .elementor-widget-image img:hover {
    opacity: 0.85;
}
/* Lightbox overlay — vanilla, no external library. */
.tl-lightbox {
    position: fixed;
    inset: 0;
    background: rgba(20, 18, 14, 0.92);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 99999;
    cursor: zoom-out;
    padding: 2.5vh 2.5vw;
}
.tl-lightbox img {
    max-width: 100%;
    max-height: 100%;
    width: auto;
    height: auto;
    object-fit: contain;
    box-shadow: 0 6px 28px rgba(0,0,0,0.5);
    background: #fff9ef;
}
.tl-lightbox-close {
    position: absolute;
    top: 1.2rem;
    right: 1.4rem;
    color: #fff9ef;
    font-size: 2.2rem;
    line-height: 1;
    background: transparent;
    border: 0;
    cursor: pointer;
    padding: 0.2rem 0.5rem;
    font-family: inherit;
}

/* ---------------------------------------------------------------- *
 * 4c. Section-page intros — full-bleed horizontal header image on top,
 *     then the section title + description, then content beneath
 *     (Fía, 2026-05-22 pm; full-bleed sitewide 2026-05-23).
 * ---------------------------------------------------------------- */
body.page-id-1898 .elementor-element-1553c2e .elementor-container,
body.page-id-808  .elementor-element-825b6e9 .elementor-container,
body.page-id-1177 .elementor-element-1b54d5a .elementor-container,
body.page-id-1147 .elementor-element-ba54885 .elementor-container,
body.page-id-68   .elementor-element-3d58b5f .elementor-container {
    flex-direction: column !important;
    flex-wrap: nowrap !important;
    align-items: stretch !important;
}
body.page-id-1898 .elementor-element-1553c2e .elementor-column,
body.page-id-808  .elementor-element-825b6e9 .elementor-column,
body.page-id-1177 .elementor-element-1b54d5a .elementor-column,
body.page-id-1147 .elementor-element-ba54885 .elementor-column,
body.page-id-68   .elementor-element-3d58b5f .elementor-column {
    width: 100% !important;
    max-width: 100% !important;
    flex: 0 0 auto !important;
}
/* Image column rises to the top, then the title + description follow. */
body.page-id-1898 .elementor-element-1553c2e .elementor-column:has(.elementor-widget-image),
body.page-id-808  .elementor-element-825b6e9 .elementor-column:has(.elementor-widget-image),
body.page-id-1177 .elementor-element-1b54d5a .elementor-column:has(.elementor-widget-image),
body.page-id-1147 .elementor-element-ba54885 .elementor-column:has(.elementor-widget-image),
body.page-id-68   .elementor-element-3d58b5f .elementor-column:has(.elementor-widget-image) {
    order: -1 !important;
}
/* Header image breaks out of its container to the full viewport width —
   the classic "negative-margin escape" — so it reads as a full bleed
   like Bookshelf, Beyond, Art-context already do (Fía, 2026-05-23). The
   :has() rule above puts the image column first, then we yank the image
   itself to the page edges. About joined the family 2026-05-28. */
body.page-id-1898 .elementor-element-1553c2e .elementor-widget-image,
body.page-id-808  .elementor-element-825b6e9 .elementor-widget-image,
body.page-id-1177 .elementor-element-1b54d5a .elementor-widget-image,
body.page-id-1147 .elementor-element-ba54885 .elementor-widget-image,
body.page-id-68   .elementor-element-3d58b5f .elementor-widget-image {
    position: relative !important;
    left: 50% !important;
    right: 50% !important;
    margin-left: -50vw !important;
    margin-right: -50vw !important;
    width: 100vw !important;
    max-width: 100vw !important;
    margin-bottom: 1.4rem !important;
}
body.page-id-1898 .elementor-element-1553c2e .elementor-widget-image img,
body.page-id-808  .elementor-element-825b6e9 .elementor-widget-image img,
body.page-id-1177 .elementor-element-1b54d5a .elementor-widget-image img,
body.page-id-1147 .elementor-element-ba54885 .elementor-widget-image img {
    aspect-ratio: 16 / 5 !important;
    object-fit: cover !important;
    width: 100% !important;
    height: auto !important;
    display: block;
}
/* About's hero image is the Group-24 portrait — a 706x775 painting of
   Tom, so a 16/5 crop slices through the cream background above his head
   and "erases tom" (Fía, 2026-05-29). Render it full-bleed wide but at a
   gentler 16/7 aspect with the crop window pinned to Tom's face. */
body.page-id-68 .elementor-element-3d58b5f .elementor-widget-image img {
    aspect-ratio: 16 / 7 !important;
    object-fit: cover !important;
    object-position: center 30% !important;
    width: 100% !important;
    height: auto !important;
    display: block;
}
/* Intro titles + descriptions default to left-aligned (Bookshelf
   overrides to centred below). */
body.page-id-1898 .elementor-element-1553c2e .elementor-heading-title,
body.page-id-1177 .elementor-element-1b54d5a .elementor-heading-title,
body.page-id-1147 .elementor-element-ba54885 .elementor-heading-title,
body.page-id-140  .elementor-element-71fa6aa .elementor-heading-title {
    text-align: left !important;
}

/* News page-wide 820px cap (section 3) would clip the full-bleed image —
   exempt the intro's image widget so it can escape to viewport width. */
body.page-id-1898 .elementor-element-1553c2e .elementor-widget-image {
    max-width: 100vw !important;
}

/* ---------------------------------------------------------------- *
 * 4d. In the Studio overview (page-id-140) — inject a full-bleed
 *     header image as a CSS background, since the intro section
 *     carries no image of its own (Fía, 2026-05-23).
 * ---------------------------------------------------------------- */
body.page-id-140 .elementor-element-71fa6aa {
    position: relative;
}
body.page-id-140 .elementor-element-71fa6aa::before {
    content: "";
    display: block;
    position: relative;
    left: 50%;
    right: 50%;
    margin-left: -50vw;
    margin-right: -50vw;
    width: 100vw;
    aspect-ratio: 16 / 5;
    background-image: url('https://www.thomaslawson.com/wp-content/uploads/2022/09/2010_Tree_HR.jpg');
    background-size: cover;
    background-position: center;
    background-repeat: no-repeat;
    margin-bottom: 1.4rem;
}

/* ---------------------------------------------------------------- *
 * 5. About (page-id-?) — years reversed newest-first
 *    Found by scanning /about/ headings: 6 year sections.
 * ---------------------------------------------------------------- */
body.page-id-68 [data-elementor-type="wp-page"] {
    display: flex !important;
    flex-direction: column !important;
}
body.page-id-68 .elementor-element-3d58b5f { order: -100 !important; } /* hero */
body.page-id-68 .elementor-element-1024859 { order: 1  !important; }   /* 2020 onwards heading */
body.page-id-68 .elementor-element-09f0adc { order: 2  !important; }   /* 2020 onwards images */
body.page-id-68 .elementor-element-f90ea8e { order: 3  !important; }   /* 2010-2020 heading */
body.page-id-68 .elementor-element-ac379d5 { order: 4  !important; }   /* 2010-2020 images */
body.page-id-68 .elementor-element-e7f58e2 { order: 5  !important; }   /* 2000-2010 heading */
body.page-id-68 .elementor-element-9933892 { order: 6  !important; }   /* 2000-2010 images */
body.page-id-68 .elementor-element-c387832 { order: 7  !important; }   /* 1990-2000 heading */
body.page-id-68 .elementor-element-05803a3 { order: 8  !important; }   /* 1990-2000 images */
body.page-id-68 .elementor-element-81b61fa { order: 9  !important; }   /* 1980-1990 heading */
body.page-id-68 .elementor-element-5017fca { order: 10 !important; }   /* 1980-1990 images */
body.page-id-68 .elementor-element-6041595 { order: 11 !important; }   /* 1975-1980 heading */
body.page-id-68 .elementor-element-8802656 { order: 12 !important; }   /* 1975-1980 images */

/* ---------------------------------------------------------------- *
 * 6. 1980-1982 (page-id-401) captions in line with sibling years
 * ---------------------------------------------------------------- */
body.page-id-401 .elementor-widget-heading h5.elementor-heading-title {
    font-style: italic;
    font-weight: 400;
    margin-bottom: 0;
}
body.page-id-401 .elementor-widget-heading h6.elementor-heading-title {
    font-style: normal;
    font-weight: 400;
    margin-top: 0.15em;
    font-size: 1em;
}
body.page-id-401 .elementor-widget-heading + .elementor-widget-heading {
    margin-top: -0.3em;
}

/* ---------------------------------------------------------------- *
 * 7. Beyond the Studio (1177) + Art in a Broader Context (1147)
 *    — project images cropped to one uniform shape so the page reads as a
 *    grid of project windows, not mismatched artworks (Fía, 2026-05-22).
 *    The intro sections (1b54d5a / ba54885) are excluded — they keep the
 *    standardised intro blurb from section 4c.
 * ---------------------------------------------------------------- */
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-widget-image img,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-widget-image img {
    aspect-ratio: 4 / 3 !important;
    object-fit: cover !important;
    width: 100% !important;
    height: auto !important;
    display: block;
}
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-container,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-container {
    flex-wrap: wrap !important;
    align-items: flex-start !important;
}
/* Elementor leaves stale per-column inline widths (a "col-50" can render
   at 33%/66%) — force every project column to an even half so each page
   reads as a true two-up grid (Fía, 2026-05-22). */
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-column,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-column {
    width: 50% !important;
    max-width: 50% !important;
    flex: 0 0 50% !important;
}
/* The image widget + its link wrapper must be full-width blocks, or a
   small-intrinsic image leaves the cell undersized — that was the uneven
   grid on Art in a Broader Context (Fía, 2026-05-22). */
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-widget-image,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-widget-image,
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-widget-image .elementor-image,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-widget-image .elementor-image,
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-widget-image a,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-widget-image a {
    display: block !important;
    width: 100% !important;
}
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-widget-image,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-widget-image {
    margin-bottom: 0.3rem !important;
}
/* Project labels sit ABOVE their image, centred + italicised + smaller —
   so they read as project labels, not artwork captions (Fía, 2026-05-22
   pm). Same treatment extends to News content sections (intro excluded).
   Widget-wrap becomes a flex column so `order` can swap heading/image. */
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-widget-wrap,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-widget-wrap,
body.page-id-1898 .elementor-top-section:not(.elementor-element-1553c2e) .elementor-widget-wrap {
    display: flex !important;
    flex-direction: column !important;
}
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-widget-heading,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-widget-heading,
body.page-id-1898 .elementor-top-section:not(.elementor-element-1553c2e) .elementor-widget-heading {
    order: 1 !important;
}
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-widget-image,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-widget-image,
body.page-id-1898 .elementor-top-section:not(.elementor-element-1553c2e) .elementor-widget-image {
    order: 2 !important;
}
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-widget-heading .elementor-heading-title,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-widget-heading .elementor-heading-title,
body.page-id-1898 .elementor-top-section:not(.elementor-element-1553c2e) .elementor-widget-heading .elementor-heading-title {
    text-align: center !important;
    font-size: 0.95rem !important;
    font-style: italic !important;
    font-weight: 400 !important;
    letter-spacing: 0.01em !important;
    margin: 0 0 0.55rem !important;
    line-height: 1.35 !important;
    color: #2e2a24 !important;
}
/* Beyond the Studio + Art in a Broader Context project labels: solid
   black, slightly larger, no italic (Fía, 2026-05-22 late + 2026-05-23:
   Art context should match Beyond, not gray italic). */
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-widget-heading .elementor-heading-title,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-widget-heading .elementor-heading-title {
    font-style: normal !important;
    font-weight: 500 !important;
    font-size: 1.1rem !important;
    color: #000 !important;
}
/* The second heading on each Art-context tile holds the year; on Beyond
   tiles there's only the one heading. The JS for Art context (section
   below) merges "Name" + "Year" into one inline label, so hide the
   stand-alone year heading after the JS injects ", YEAR" into the name. */
body.page-id-1147 .tl-ac-year-merged {
    display: none !important;
}

/* ---------------------------------------------------------------- *
 * 8. /bookshelf/ (page-id-808) — JS injects a horizontal cover strip
 *    per shelf. CSS here styles the strip.
 * ---------------------------------------------------------------- */
body.page-id-808 .tl-shelf-strip {
    display: flex;
    flex-wrap: nowrap;
    gap: 0.7rem;
    overflow-x: auto;
    overflow-y: hidden;
    padding: 0.7rem 1rem 1.2rem;
    margin: 0.4rem 0 1.2rem;
    width: 100%;
    box-sizing: border-box;
    scrollbar-width: thin;
    scroll-snap-type: x proximity;
    -webkit-overflow-scrolling: touch;
}
body.page-id-808 .tl-shelf-strip img {
    height: 150px;
    width: auto;
    flex: 0 0 auto;
    border-radius: 2px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.12);
    background: #fff;
    scroll-snap-align: start;
}
/* Centering via auto-margins on the first/last cover (Fía, 2026-05-29 —
   the earlier `justify-content: safe center` was bailing to flex-start
   whenever the row was wider than its parent column). When the covers fit
   the row, both autos absorb the leftover space and the row centers under
   the title; when they overflow, both autos collapse to 0 so the row
   scrolls naturally from the left edge. */
body.page-id-808 .tl-shelf-strip img:first-child  { margin-inline-start: auto; }
body.page-id-808 .tl-shelf-strip img:last-child   { margin-inline-end:   auto; }

/* The bookshelf shelves are *_clickshow sections that the page's own
   inline <style> hides until their *_click heading is tapped. Fía wants
   the bookshelf to read as a full preview — Publications, Interviews and
   Other Writings all open from the get-go (2026-05-20 + 2026-05-21 pm).
   !important beats the page's non-important inline rule; the headings
   also stop reading as clickable. */
body.page-id-808 .publications_clickshow,
body.page-id-808 .interviews_clickshow,
body.page-id-808 .other_clickshow {
    display: block !important;
}
body.page-id-808 .publications_click,
body.page-id-808 .interviews_click,
body.page-id-808 .other_click {
    cursor: default !important;
}

/* "Publications" pulled up under the bookshelf intro photo — its section
   carried a 300px inline margin-top (Fía, 2026-05-22). */
body.page-id-808 .elementor-element-973f55a {
    margin-top: 1.5rem !important;
}

/* "Miscellaneous" shelf has no entries — hide it for now (Fía, 2026-05-22). */
body.page-id-808 .elementor-element-efdbae8 {
    display: none !important;
}

/* Drop the "Publications" and "Other writings" category headers so all
   shelf subheadings (Artforum, Interviews, East of Borneo, …) sit at one
   level (Fía, 2026-05-22 late). */
body.page-id-808 .elementor-element-973f55a,
body.page-id-808 .elementor-element-8528fa3 {
    display: none !important;
}

/* Bookshelf intro: title and description left-justified, sitting tight
   under the horizontal header image (Fía, 2026-05-22 night — text was
   hanging low). The shelf subheadings below stay centred. */
body.page-id-808 .elementor-element-825b6e9 .elementor-heading-title,
body.page-id-808 .elementor-element-825b6e9 .elementor-widget-text-editor,
body.page-id-808 .elementor-element-825b6e9 .elementor-widget-text-editor p {
    text-align: left !important;
}
body.page-id-808 .elementor-element-825b6e9 .elementor-widget-image,
body.page-id-808 .elementor-element-825b6e9 .elementor-widget-image img {
    margin-bottom: 0 !important;
}
body.page-id-808 .elementor-element-825b6e9 .elementor-widget-heading,
body.page-id-808 .elementor-element-825b6e9 .elementor-widget-text-editor {
    margin-top: 0 !important;
    padding-top: 0 !important;
}
body.page-id-808 .elementor-element-825b6e9 .elementor-widget-heading .elementor-widget-container,
body.page-id-808 .elementor-element-825b6e9 .elementor-widget-heading > .elementor-widget-container {
    padding-top: 0 !important;
    margin-top: 0 !important;
}
body.page-id-808 .elementor-element-825b6e9 .elementor-widget-wrap {
    padding-top: 0 !important;
}
body.page-id-808 .elementor-element-825b6e9 .elementor-widget-divider {
    display: none !important;
}
/* Below the intro: shelf subheadings (Artforum, Afterall, …) stay
   centred + standardised to one size with no anchor underline
   (Fía, 2026-05-23: "remove the underline, standardize the size
   throughout, and make sure the text is centered with the thumbnails
   below"). Bumped one tick larger 2026-05-28 — Fía wanted the section
   titles a touch more prominent. */
body.page-id-808 .elementor-top-section:not(.elementor-element-825b6e9) .elementor-heading-title {
    text-align: center !important;
    font-size: 1.95rem !important;
    font-weight: 500 !important;
    line-height: 1.25 !important;
    margin: 0 0 0.6rem !important;
}
body.page-id-808 .elementor-top-section:not(.elementor-element-825b6e9) .elementor-heading-title a,
body.page-id-808 .elementor-top-section:not(.elementor-element-825b6e9) .elementor-heading-title a:visited,
body.page-id-808 .elementor-top-section:not(.elementor-element-825b6e9) .elementor-heading-title a:hover {
    text-decoration: none !important;
    border-bottom: 0 !important;
    color: inherit !important;
}
body.page-id-808 .elementor-top-section:not(.elementor-element-825b6e9) .elementor-widget-heading {
    text-align: center !important;
    width: 100% !important;
    margin-top: 1.4rem !important;
}

/* ---------------------------------------------------------------- *
 * 9. Art in a Broader Context — reverse-chronological order
 *    (Fía, 2026-05-22 night: most recent first).
 * ---------------------------------------------------------------- */
body.page-id-1147 [data-elementor-type="wp-page"] {
    display: flex !important;
    flex-direction: column !important;
}
body.page-id-1147 .elementor-element-ba54885 { order: -100 !important; } /* intro */
body.page-id-1147 .elementor-element-5aa3c95 { order: 1 !important;  }   /* Art School */
body.page-id-1147 .elementor-element-2b904bf { order: 2 !important;  }   /* The Experimental Impulse */
body.page-id-1147 .elementor-element-35e17d9 { order: 3 !important;  }   /* Shimmer */
body.page-id-1147 .elementor-element-e90275b { order: 4 !important;  }   /* The British Art Show */
body.page-id-1147 .elementor-element-2bcae80 { order: 5 !important;  }   /* Nostalgia as Resistance */
body.page-id-1147 .elementor-element-19041d8 { order: 6 !important;  }   /* Livin' in the USA */
body.page-id-1147 .elementor-element-5d93139 { order: 7 !important;  }   /* Critical Perspectives */
body.page-id-1147 .elementor-element-8eae99f { order: 8 !important;  }   /* Pat Douthwaite */

/* ---------------------------------------------------------------- *
 * 10. Project tiles — more vertical breathing room between items so
 *     the label clearly belongs to its image (Fía, 2026-05-22 night).
 * ---------------------------------------------------------------- */
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a) .elementor-column,
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) .elementor-column {
    padding-bottom: 2.4rem !important;
}
body.page-id-1177 .elementor-top-section:not(.elementor-element-1b54d5a),
body.page-id-1147 .elementor-top-section:not(.elementor-element-ba54885) {
    margin-bottom: 1.2rem !important;
}

/* ---------------------------------------------------------------- *
 * 11. In the Studio overview (page-140) — year-range groupings shrunk
 *     so multiple year sections read on one screen instead of each
 *     stretching to fill the viewport (Fía, 2026-05-23). The 2026-05-22
 *     pass had reverted sizing-down to the theme default; the new note
 *     is "make each year grouping a tad smaller".
 * ---------------------------------------------------------------- */
body.page-id-140 .elementor-top-section:not(.elementor-element-71fa6aa) {
    margin-bottom: 1.4rem !important;
    padding-top: 0.5rem !important;
    padding-bottom: 0.5rem !important;
}
body.page-id-140 .elementor-top-section:not(.elementor-element-71fa6aa) .elementor-container {
    max-width: 1080px !important;
    margin: 0 auto !important;
}
/* Year heading sized down slightly so it doesn't tower above the 3
   little artworks beside it. */
body.page-id-140 .elementor-top-section:not(.elementor-element-71fa6aa) .elementor-heading-title {
    font-size: 1.3rem !important;
    margin-bottom: 0.35rem !important;
}
/* Cap the artwork thumbnails so a 3-up row fits comfortably and reads
   as a year sampler, not a full-bleed gallery. */
body.page-id-140 .elementor-top-section:not(.elementor-element-71fa6aa) .elementor-widget-image img {
    max-height: 220px !important;
    width: auto !important;
    object-fit: contain !important;
    display: block;
    margin: 0 auto;
}

/* ---------------------------------------------------------------- *
 * 12. In the Studio detail pages — the two year headings inline on
 *     one row, sized down, divider hidden (Fía, 2026-05-22 night).
 * ---------------------------------------------------------------- */
body.tl-studio-detail .elementor-top-section:first-of-type {
    margin-bottom: 1.8rem !important;
}
body.tl-studio-detail .elementor-top-section:first-of-type .elementor-container,
body.tl-studio-detail .elementor-top-section:first-of-type .elementor-row {
    flex-direction: row !important;
    flex-wrap: nowrap !important;
    justify-content: flex-start !important;
    align-items: baseline !important;
    gap: 0.5rem !important;
}
body.tl-studio-detail .elementor-top-section:first-of-type .elementor-column {
    width: auto !important;
    max-width: none !important;
    flex: 0 0 auto !important;
}
body.tl-studio-detail .elementor-top-section:first-of-type .elementor-widget-divider {
    display: none !important;
}
body.tl-studio-detail .elementor-top-section:first-of-type .elementor-heading-title {
    font-size: 1.35rem !important;
    font-weight: 500 !important;
    margin: 0 !important;
    line-height: 1.2 !important;
}
/* Visible en-dash between the two year headings — "2017 – 2020" reads as
   one time span (Fía, 2026-05-22 late). */
body.tl-studio-detail .elementor-top-section:first-of-type .elementor-column:first-child .elementor-heading-title::after {
    content: " –";
}

/* ---------------------------------------------------------------- *
 * 13. Contact (page-1527) — email centred + wrapped in a mailto link
 *     (Fía, 2026-05-22 night; the JS adds the anchor).
 * ---------------------------------------------------------------- */
body.page-id-1527 [data-elementor-type="wp-page"],
body.page-id-1527 .ast-container .entry-content,
body.page-id-1527 #primary {
    display: flex !important;
    flex-direction: column !important;
    align-items: center !important;
    justify-content: center !important;
    min-height: calc(100vh - 200px) !important;
    padding: 0 !important;
    margin: 0 auto !important;
}
body.page-id-1527 .elementor-element-d42e8f6 {
    padding-top: 0 !important;
    padding-bottom: 0 !important;
    margin-top: 0 !important;
    margin-bottom: 0 !important;
}
body.page-id-1527 .elementor-element-d42e8f6 .elementor-heading-title,
body.page-id-1527 .elementor-element-ab1e9c7 .elementor-heading-title {
    text-align: center !important;
}
body.page-id-1527 .tl-contact-mailto,
body.page-id-1527 .tl-contact-mailto:visited {
    color: inherit !important;
    text-decoration: none !important;
    border-bottom: 1px solid currentColor;
}
body.page-id-1527 .tl-contact-mailto:hover {
    opacity: 0.6;
}
</style>
<?php
}

function tl_fia_polish_js() {
    // Studio-detail lightbox runs on slug-matched detail pages
    // (tl-studio-detail body class is set in PHP by post-slug prefix).
    global $post;
    $is_studio_detail = ($post && isset($post->post_name)
        && strpos($post->post_name, 'inthestudio_') === 0);
    if ($is_studio_detail) { tl_fia_polish_js_studio_lightbox(); return; }

    if (is_page(10))   { tl_fia_polish_js_home();    return; }
    if (is_page(1898)) { tl_fia_polish_js_news();    return; }
    if (is_page(1527)) { tl_fia_polish_js_contact(); return; }
    if (is_page(1147)) { tl_fia_polish_js_artctx();  return; }
    if (is_page(1177)) { tl_fia_polish_js_beyond();  return; }
    if (is_page(68))   { tl_fia_polish_js_about();   return; }
    if (!is_page(808)) return; // only /bookshelf/ below
    // Pre-curated first-N cover URLs from each shelf subpage (covers
    // already live in /wp-content/uploads/, so reusing them costs nothing).
    $shelves = [
        // section data-id  =>  list of cover image URLs
        // Artforum — full set scraped from /bookshelf_artforum/ so the
        // strip overflows and people can scroll through every cover
        // (Fía, 2026-05-28).
        '7f476f6' => [
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/1-ARtforum-November-1980-817x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/2-Artforum-Marchh-1981-872x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/3-Artforum-April-1981-886x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/4-Artforum-May-1981-925x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/5-Artforum-September-1981-956x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/6-Artforum-October-1981-960x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/7-Artforum-December-1981-908x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/8-Artforum-January-1982-922x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/9-Artforum-May-1982-855x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/10-Artforum-Summer-1982-961x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/11-Artforum-October-1982-881x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/12-Artforum-November-1982-841x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/13-Artforum-February-1983-1024x640.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/14-Artforum-March-1983-884x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/15-Artforum-Summer-83-859x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/16-Artforum-September-83-923x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/17-Artforum-January-84-929x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/18-Artforum-MAy-84-928x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/19-Artforum-Summer-84-790x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/20-Artforum-September-84--797x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/21-Artforum-Novemner-84-815x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/22-Artforum-March-1986-890x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/23-Artforum-Janury-1988-929x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/24-Artforum-October-2004-943x1024.png',
        ],
        'be9922f' => [
            'https://www.thomaslawson.com/wp-content/uploads/2023/01/Afterall6-cover-654x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/01/Afterall7-642x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/01/Afterall9-cover-644x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/01/Afterall11-cover-647x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/01/Afterall12-cover-642x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2024/01/Afterall13-656x1024.jpg',
        ],
        // East of Borneo — full set scraped from /bookshelf_eastofborneo/.
        '2fe6730' => [
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/The-Journey-West-1024x716.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Andrea-Bowers-Interview-1024x592.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Institutional-Whitewash-1024x712.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Liz-Glynn-1024x640.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Michael-Asher-obit.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/4-Taxis-1024x712.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Stephen-Prina-1024x465.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Allan-Sekula-obit-1024x496.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Fiona-Connor-1024x721.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/a-visit-to-Man-Ray-1024x809.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Hopps-discovers-Cornell.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Duchamp-Wood-1024x659.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Hopps-at-Arensbergs-1024x818.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Cesar-Pelli-obit-1024x735.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/John-Baldessari.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Betye-Saar.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Millard-Sheets.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/CalArts-story-1024x761.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Luciano-Perna-obit.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Michael-Asher-essay-alt.png',
        ],
        '624f2b7' => [
            'https://www.thomaslawson.com/wp-content/uploads/2023/12/REALLIFE-1-cover.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/12/REALLIFE-2-cover.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/12/REALLIFE-3-cover.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/12/654-Real-Life-review-of-anthology_Page_1-scaled.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/12/683-Real-Life-Show_Page_1-682x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/12/Pfeil-Magazine-cover-and-contents_Page_1-745x1024.jpg',
        ],
        '148197c' => [
            'https://www.thomaslawson.com/wp-content/uploads/2023/12/403-Video-Data-Bank-Profile-1-674x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/12/497-Robbins-Interview-with-TL-1-scaled.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/12/620-Robert-Rooney-Pool-Side-1-740x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/12/793-CAE-Art-Papers-1-scaled.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/12/Cover-for-Christopher-Howard-interview-1024x589.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/12/Cover-for-McEvilly-Sischy-interview-990x1024.png',
        ],
        // Writings About TL — full set scraped from /bookshelf_writingsabouttl/.
        '91e3759' => [
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/650-Thomas-Lawson-at-LAXART-1.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Christopher-Miles.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Donald-Kuspit-1024x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Ingrid-Sischy-1014x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Jeane-Silverthorne-1990.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Jeanne-Silverthorn-Summer-1985.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Joan-Casademont-1024x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Judith-Russi-May-83.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Kate-Linker-1014x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Kuspit-On-Drawing-April-1982.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Artforum-March-84.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Lobel-Singerman-1024x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2022/09/2015_Displacement-1024x793.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Ron-Jones-1987-cover.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Ron-Jones-1985-cover.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Schjeldahl-on-Pictures-Generation_Page_1.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Sydney-Biennale-1024x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Deihl-review.jpg',
        ],
        'f2f992c' => [
            'https://www.thomaslawson.com/wp-content/uploads/2026/03/spike-71-couples-cover-web_f4b47d70-c6c8-4448-ba6c-5ab99feb149d-794x1024.jpeg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/359-Empire-1.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/439-Laura-Owens-1-scaled.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/643-Richard-Wright-1-scaled.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/648-Michael-Hurson-1-scaled.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/652-Paint-1-scaled.jpg',
        ],
        '7c79d3f' => [
            'https://www.thomaslawson.com/wp-content/uploads/2024/03/1-Art-After-Modernism-Rethinking-Representation-721x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2024/03/2-Theories-of-Contemporary-Art-666x1024.jpeg',
            'https://www.thomaslawson.com/wp-content/uploads/2024/03/3-Infotainment.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2024/03/4-Individuals.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2024/03/5-Modern-Dreams.png',
            'https://www.thomaslawson.com/wp-content/uploads/2024/03/6-Blasted-allegories-731x1024.jpeg',
        ],
    ];
    $json = wp_json_encode($shelves);
    ?>
<script id="tl-fia-bookshelf-strips">
(function () {
    var shelves = <?php echo $json; ?>;
    Object.keys(shelves).forEach(function (sid) {
        var section = document.querySelector('.elementor-element-' + sid);
        if (!section) return;
        if (section.querySelector('.tl-shelf-strip')) return; // already injected
        var strip = document.createElement('div');
        strip.className = 'tl-shelf-strip';
        shelves[sid].forEach(function (url) {
            var img = document.createElement('img');
            img.src = url;
            img.loading = 'lazy';
            img.alt = '';
            strip.appendChild(img);
        });
        // Place the strip directly after the shelf's subheading so it
        // stacks beneath it, never beside it (Fía, 2026-05-22).
        var head = section.querySelector('.elementor-widget-heading');
        if (head && head.parentNode) {
            head.parentNode.insertBefore(strip, head.nextSibling);
        } else {
            var inner = section.querySelector('.elementor-container') || section;
            inner.appendChild(strip);
        }
    });
})();
</script>
<?php
}

/**
 * Homepage (page-id-10): rename the Notes tile to News, then stack the five
 * section titles into one centred block overlaid on the veiled painting
 * (Fía, 2026-05-22 — overlap restored). With the cover images hidden by CSS
 * the headings would lose their links, so each column's href is re-homed
 * onto its heading + the whole column.
 */
function tl_fia_polish_js_home() {
    ?>
<script id="tl-fia-home-tiles">
(function () {
    if (!document.body.classList.contains('page-id-10')) return;
    var page = document.querySelector('[data-elementor-type="wp-page"]');
    if (!page) return;

    // The "Notes" tile reads as "News" — swap the heading's text node.
    var notesHeading = document.querySelector('.elementor-element-a258823 .elementor-heading-title');
    if (notesHeading) {
        notesHeading.childNodes.forEach(function (n) {
            if (n.nodeType === 3 && /notes/i.test(n.textContent)) {
                n.textContent = n.textContent.replace(/notes/i, 'News');
            }
        });
    }

    // Build the centred block: the five section titles stacked over the
    // veiled painting (Fía, 2026-05-22 — overlap restored). The painting
    // is a CSS background on .tl-home-layout, so no <img> is needed here.
    if (!page.querySelector('.tl-home-layout')) {
        var layout = document.createElement('div');
        layout.className = 'tl-home-layout';
        var titles = document.createElement('div');
        titles.className = 'tl-home-titles';
        // Move the five tile columns into the titles block, in Fía's order.
        ['05de656','6b3030e','b1d6555','623a2c4','5e1a885'].forEach(function (id) {
            var col = page.querySelector('.elementor-element-' + id);
            if (col) titles.appendChild(col);
        });
        layout.appendChild(titles);
        page.appendChild(layout);
    }

    // Re-home each tile's link onto its heading + whole column.
    var cols = document.querySelectorAll('.tl-home-titles .elementor-column');
    cols.forEach(function (col) {
        var imgLink = col.querySelector('.elementor-widget-image a[href]');
        var title   = col.querySelector('.elementor-heading-title');
        if (!imgLink || !title) return;
        var href = imgLink.getAttribute('href');
        if (!href) return;
        if (!title.querySelector('a.tl-home-tile-link')) {
            var a = document.createElement('a');
            a.href = href;
            a.className = 'tl-home-tile-link';
            while (title.firstChild) a.appendChild(title.firstChild);
            title.appendChild(a);
        }
        col.addEventListener('click', function (e) {
            if (e.target.closest('a')) return; // let real links act
            window.location.href = href;
        });
    });
})();
</script>
<?php
}

/**
 * News page (page-id-1898): some sections hold images while the very next
 * section holds those images' captions as separate headings — so the
 * single-column stack drops a caption far from its image ("Rabkin Prize"
 * landed under Tom). Fold each orphaned caption back into its image's
 * column so captions sit directly beneath their own artwork.
 */
function tl_fia_polish_js_news() {
    ?>
<script id="tl-fia-news-captions">
(function () {
    if (!document.body.classList.contains('page-id-1898')) return;
    var page = document.querySelector('[data-elementor-type="wp-page"]');
    if (!page) return;

    // The hero heading reads "News" once inside the section (Fía, 2026-05-22).
    var hero = document.querySelector('.elementor-element-a7782ed .elementor-heading-title');
    if (hero) {
        hero.childNodes.forEach(function (n) {
            if (n.nodeType === 3 && /notes/i.test(n.textContent)) {
                n.textContent = n.textContent.replace(/notes/i, 'News');
            }
        });
    }

    var secs = [].slice.call(page.children).filter(function (el) {
        return el.classList && el.classList.contains('elementor-top-section');
    });
    secs.forEach(function (sec, idx) {
        var imgs  = sec.querySelectorAll('.elementor-widget-image');
        var heads = sec.querySelectorAll('.elementor-widget-heading');
        if (!imgs.length || heads.length) return; // not an image-only section
        var next = secs[idx + 1];
        if (!next) return;
        var nImgs  = next.querySelectorAll('.elementor-widget-image');
        var nHeads = next.querySelectorAll('.elementor-widget-heading');
        if (nImgs.length || nHeads.length !== imgs.length) return; // no match
        for (var i = 0; i < imgs.length; i++) {
            var wrap = imgs[i].closest('.elementor-widget-wrap') || imgs[i].parentNode;
            wrap.appendChild(nHeads[i]); // caption joins its image's column
        }
        next.style.display = 'none'; // the now-empty caption section
    });
})();
</script>
<?php
}

/**
 * Contact (page-id-1527): wrap any email-shaped heading in a mailto: link
 * so a click opens the visitor's mail client (Fía, 2026-05-22 night).
 */
function tl_fia_polish_js_contact() {
    ?>
<script id="tl-fia-contact-mailto">
(function () {
    if (!document.body.classList.contains('page-id-1527')) return;
    var heads = document.querySelectorAll('.elementor-heading-title');
    var re = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    heads.forEach(function (h) {
        var t = (h.textContent || '').trim();
        if (re.test(t) && !h.querySelector('a')) {
            h.innerHTML = '';
            var a = document.createElement('a');
            a.href = 'mailto:' + t;
            a.className = 'tl-contact-mailto';
            a.textContent = t;
            h.appendChild(a);
        }
    });
})();
</script>
    <?php
}

/**
 * Art in a Broader Context (page-id-1147): the page is authored as a
 * pile of Elementor sections, each carrying 1 or 2 project tiles in its
 * own columns. Fía wants ONE reverse-chronological grid where the tile
 * order is by date, not by which authoring section the tile happens to
 * live in (Fía, 2026-05-22 late). This rebuild collects every project
 * column, sorts by year descending, and re-homes them into a fresh
 * 2-up grid; the original sections are hidden so the layout doesn't
 * double-render.
 */
function tl_fia_polish_js_artctx() {
    ?>
<script id="tl-fia-artctx-grid">
(function () {
    if (!document.body.classList.contains('page-id-1147')) return;
    var page = document.querySelector('[data-elementor-type="wp-page"]');
    if (!page) return;
    var intro = document.querySelector('.elementor-element-ba54885');
    var tiles = [];
    var sections = [].slice.call(page.children).filter(function (el) {
        return el !== intro && el.classList && el.classList.contains('elementor-section');
    });
    sections.forEach(function (sec) {
        var cols = sec.querySelectorAll('.elementor-column');
        for (var ci = 0; ci < cols.length; ci++) {
            var col = cols[ci];
            // only project columns: one that holds an image widget
            if (!col.querySelector('.elementor-widget-image')) continue;
            // Skip "Familie Beck" — Fía moved it to Bookshelf
            // (2026-05-28). Column id from the live DOM is 14673a6;
            // also match by tile name as a safety net.
            if (col.classList.contains('elementor-element-14673a6')) {
                col.style.display = 'none';
                continue;
            }
            var heads = col.querySelectorAll('.elementor-widget-heading');
            var name = heads[0] ? heads[0].textContent.trim() : '';
            var year = heads[1] ? heads[1].textContent.trim() : '';
            if (/^familie\s*beck/i.test(name)) {
                col.style.display = 'none';
                continue;
            }
            var key;
            if (/present|ongoing/i.test(year)) {
                key = 9999;
            } else {
                var m = year.match(/(\d{4})\s*[-–—]\s*(\d{4})/) || year.match(/(\d{4})/);
                if (m) key = parseInt(m[2] || m[1], 10);
                else key = 0;
            }
            // Merge "Name" + "Year" into one inline label so the tile
            // reads as "Art School, 1982-present" — Fía's 2026-05-23 ask.
            if (heads[0] && heads[1] && year) {
                var nameNode = heads[0].querySelector('.elementor-heading-title');
                if (nameNode && !nameNode.dataset.tlAcMerged) {
                    nameNode.dataset.tlAcMerged = '1';
                    nameNode.textContent = name + ', ' + year;
                }
                heads[1].classList.add('tl-ac-year-merged');
            }
            tiles.push({ col: col, name: name, year: year, key: key });
        }
    });
    // newest first
    tiles.sort(function (a, b) { return b.key - a.key; });

    // Build the new grid section.
    var grid = document.createElement('section');
    grid.className = 'elementor-section elementor-top-section elementor-section-boxed tl-ac-grid';
    var container = document.createElement('div');
    container.className = 'elementor-container elementor-column-gap-default';
    grid.appendChild(container);
    tiles.forEach(function (t) { container.appendChild(t.col); });

    // Hide the (now-empty) original project sections.
    sections.forEach(function (sec) { sec.style.display = 'none'; });

    page.appendChild(grid);
})();
</script>
    <?php
}

/**
 * Beyond the Studio (page-id-1177): some authored sections hold only one
 * project column (Glasgow Projects, Theatre/Dance/Fashion). The
 * uniform-width col-50 rule leaves the row half-empty (Fía, 2026-05-23).
 * Flatten every project column into one continuous 2-up grid in document
 * order — preserves Fía's intended sequence, no gaps.
 */
function tl_fia_polish_js_beyond() {
    ?>
<script id="tl-fia-beyond-grid">
(function () {
    if (!document.body.classList.contains('page-id-1177')) return;
    var page = document.querySelector('[data-elementor-type="wp-page"]');
    if (!page) return;
    var intro = document.querySelector('.elementor-element-1b54d5a');
    var tileCols = [];
    var sections = [].slice.call(page.children).filter(function (el) {
        return el !== intro && el.classList && el.classList.contains('elementor-section');
    });
    sections.forEach(function (sec) {
        var cols = sec.querySelectorAll('.elementor-column');
        for (var ci = 0; ci < cols.length; ci++) {
            var col = cols[ci];
            // only columns that carry an image widget are project tiles
            if (col.querySelector('.elementor-widget-image')) {
                tileCols.push(col);
            }
        }
    });

    var grid = document.createElement('section');
    grid.className = 'elementor-section elementor-top-section elementor-section-boxed tl-beyond-grid';
    var container = document.createElement('div');
    container.className = 'elementor-container elementor-column-gap-default';
    grid.appendChild(container);
    tileCols.forEach(function (col) { container.appendChild(col); });
    sections.forEach(function (sec) { sec.style.display = 'none'; });
    page.appendChild(grid);
})();
</script>
    <?php
}

/**
 * Studio-detail pages (slug inthestudio_YYYY-YYYY): click any artwork
 * thumbnail to open it in a lightbox overlay. Vanilla JS, no library
 * (Fía, 2026-05-23).
 */
function tl_fia_polish_js_studio_lightbox() {
    ?>
<script id="tl-fia-studio-lightbox">
(function () {
    if (!document.body.classList.contains('tl-studio-detail')) return;

    function fullSize(img) {
        // WP attaches the full image as a srcset; prefer the largest one.
        if (img.dataset && img.dataset.tlFullsrc) return img.dataset.tlFullsrc;
        var src = img.currentSrc || img.src;
        var srcset = img.srcset || img.getAttribute('srcset');
        if (srcset) {
            var best = null, bestW = 0;
            srcset.split(',').forEach(function (entry) {
                var parts = entry.trim().split(/\s+/);
                var url = parts[0];
                var w = parts[1] ? parseInt(parts[1], 10) : 0;
                if (w >= bestW) { bestW = w; best = url; }
            });
            if (best) src = best;
        }
        return src;
    }

    function openLightbox(src) {
        var ov = document.createElement('div');
        ov.className = 'tl-lightbox';
        var img = document.createElement('img');
        img.src = src;
        img.alt = '';
        ov.appendChild(img);
        var close = document.createElement('button');
        close.className = 'tl-lightbox-close';
        close.setAttribute('aria-label', 'Close');
        close.textContent = '×';
        ov.appendChild(close);
        ov.addEventListener('click', function (e) {
            if (e.target === img) return;
            ov.remove();
            document.removeEventListener('keydown', onKey);
        });
        function onKey(e) {
            if (e.key === 'Escape') { ov.remove(); document.removeEventListener('keydown', onKey); }
        }
        document.addEventListener('keydown', onKey);
        document.body.appendChild(ov);
    }

    var imgs = document.querySelectorAll('.elementor-widget-image img');
    imgs.forEach(function (img) {
        // Disable any existing link wrapper so click opens our lightbox.
        var parentLink = img.closest('a');
        if (parentLink) {
            parentLink.addEventListener('click', function (e) {
                e.preventDefault();
                openLightbox(fullSize(img));
            });
        } else {
            img.addEventListener('click', function () {
                openLightbox(fullSize(img));
            });
        }
    });
})();
</script>
    <?php
}

/**
 * About (page-id-68): inject a bold "About" h1 between the full-bleed
 * banner image and the bio paragraphs so the page reads like News /
 * Bookshelf / Beyond — banner, title, then prose (Fía, 2026-05-28).
 */
function tl_fia_polish_js_about() {
    ?>
<script id="tl-fia-about-heading">
(function () {
    if (!document.body.classList.contains('page-id-68')) return;
    var intro = document.querySelector('.elementor-element-3d58b5f');
    if (!intro) return;
    // Avoid double-inject if WP renders this twice.
    if (intro.querySelector('.tl-about-heading')) return;
    // The section's first column carries the divider + bio text-editors.
    // Drop the "About" heading at the TOP of that column so it reads as
    // the section title above the bio (Fía, 2026-05-29 — the prior
    // before-text-editor placement landed the heading mid-column).
    var textCol = intro.querySelector('.elementor-column:not(:has(.elementor-widget-image))');
    if (!textCol) {
        // Fallback: any column that has a text-editor inside.
        var cols = intro.querySelectorAll('.elementor-column');
        for (var i = 0; i < cols.length; i++) {
            if (cols[i].querySelector('.elementor-widget-text-editor')) {
                textCol = cols[i];
                break;
            }
        }
    }
    if (!textCol) return;
    var wrap = document.createElement('div');
    wrap.className = 'elementor-widget elementor-widget-heading tl-about-heading';
    var inner = document.createElement('div');
    inner.className = 'elementor-widget-container';
    var h = document.createElement('h1');
    h.className = 'elementor-heading-title elementor-size-default';
    h.textContent = 'About';
    inner.appendChild(h);
    wrap.appendChild(inner);
    var widgetWrap = textCol.querySelector('.elementor-widget-wrap') || textCol;
    widgetWrap.insertBefore(wrap, widgetWrap.firstChild);
})();
</script>
<style id="tl-fia-about-style">
body.page-id-68 .tl-about-heading {
    width: 100% !important;
    max-width: 820px !important;
    margin: 0 auto !important;
}
body.page-id-68 .tl-about-heading .elementor-heading-title {
    font-size: 2.4rem !important;
    font-weight: 600 !important;
    margin: 0.4rem 0 1rem !important;
    text-align: left !important;
    letter-spacing: -0.005em;
}
body.page-id-68 .elementor-element-3d58b5f .elementor-widget-text-editor {
    max-width: 820px !important;
    margin: 0 auto !important;
}
body.page-id-68 .elementor-element-3d58b5f .elementor-widget-text-editor p:first-child {
    margin-top: 0 !important;
}
</style>
    <?php
}
