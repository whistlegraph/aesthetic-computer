<?php
/**
 * Plugin Name: TL — Fía polish pass
 * Description: CSS+JS polish on top of the existing TL theme + Elementor build, per Fía's notes (2026-05-19 + her two replies later that night). Header chrome (no underline / no rule_ hrs / no Home), cream-everywhere, home laid out as a 5-up desktop strip (vertical stack on mobile) in Fía's section order with subtitles, divider widgets dropped on the homepage, Notes image-width capped, In-the-Studio + About years reversed newest-first (with !important on the flex parent so the reorder actually applies), Beyond-the-Studio collapsed to one column with centered subsection labels, 1980-82 caption normalisation, and a JS-injected horizontal cover preview strip per shelf on /bookshelf/.
 * Version: 0.3
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

/* Push the About / Contact menu further right on desktop. */
.ast-desktop-header-content .site-header-primary-section-right {
    margin-left: auto !important;
    padding-right: 2.5rem !important;
}
.ast-desktop-header-content .main-header-menu {
    gap: 1.5rem;
}

/* ---------------------------------------------------------------- *
 * 2. Home (page-id-10) — divider widgets dropped + 5-up on desktop
 * ---------------------------------------------------------------- */

/* Drop every horizontal divider widget on the home page — these were
   reading as the "long horizontal line under tom's name". */
body.page-id-10 .elementor-widget-divider {
    display: none !important;
}

/* Section subtitles via ::after on each heading widget data-id. */
body.page-id-10 .elementor-element-04d4a58 .elementor-heading-title::after,
body.page-id-10 .elementor-element-2e51480 .elementor-heading-title::after,
body.page-id-10 .elementor-element-f9919d8 .elementor-heading-title::after,
body.page-id-10 .elementor-element-264abbc .elementor-heading-title::after,
body.page-id-10 .elementor-element-a258823 .elementor-heading-title::after {
    display: block;
    margin-top: 0.25em;
    font-style: italic;
    font-weight: 400;
    font-size: 0.5em;
    line-height: 1.3;
    letter-spacing: 0.01em;
    opacity: 0.85;
}
body.page-id-10 .elementor-element-04d4a58 .elementor-heading-title::after { content: "curatorial projects, exhibitions, and pedagogy"; }
body.page-id-10 .elementor-element-2e51480 .elementor-heading-title::after { content: "writings and publications"; }
body.page-id-10 .elementor-element-f9919d8 .elementor-heading-title::after { content: "public art"; }
body.page-id-10 .elementor-element-264abbc .elementor-heading-title::after { content: "paintings and drawings"; }
body.page-id-10 .elementor-element-a258823 .elementor-heading-title::after { content: "selections from my archive"; }

/* Desktop ≥ 769px: lay all 5 tiles as a single flex row. The home
   structure is 3 top sections of (1 / 2 / 2) columns — flatten by making
   each section display:contents so its child columns participate in the
   parent flex row, then size each column to 1/5 and order via `order`. */
@media (min-width: 769px) {
    body.page-id-10 [data-elementor-type="wp-page"] {
        display: flex !important;
        flex-direction: row !important;
        flex-wrap: nowrap !important;
        gap: 1rem;
        align-items: stretch;
    }
    body.page-id-10 [data-elementor-type="wp-page"] > .elementor-section {
        display: contents !important;
    }
    body.page-id-10 [data-elementor-type="wp-page"] > .elementor-section > .elementor-container,
    body.page-id-10 [data-elementor-type="wp-page"] > .elementor-section > .elementor-container > .elementor-row {
        display: contents !important;
    }
    body.page-id-10 [data-elementor-type="wp-page"] .elementor-column {
        flex: 1 1 0 !important;
        width: 20% !important;
        max-width: 20% !important;
        min-width: 0 !important;
        margin-bottom: 0 !important;
    }
    /* Fía's order: Art / Bookshelf / Beyond / In the Studio / Notes. */
    body.page-id-10 .elementor-element-623a2c4 { order: 1 !important; } /* Art */
    body.page-id-10 .elementor-element-5e1a885 { order: 2 !important; } /* Bookshelf */
    body.page-id-10 .elementor-element-b1d6555 { order: 3 !important; } /* Beyond */
    body.page-id-10 .elementor-element-6b3030e { order: 4 !important; } /* In the Studio */
    body.page-id-10 .elementor-element-05de656 { order: 5 !important; } /* Notes */
}

/* Mobile ≤ 768px: vertical stack, same Fía order. */
@media (max-width: 768px) {
    body.page-id-10 [data-elementor-type="wp-page"] {
        display: flex !important;
        flex-direction: column !important;
    }
    body.page-id-10 [data-elementor-type="wp-page"] > .elementor-section {
        display: contents !important;
    }
    body.page-id-10 [data-elementor-type="wp-page"] > .elementor-section > .elementor-container,
    body.page-id-10 [data-elementor-type="wp-page"] > .elementor-section > .elementor-container > .elementor-row {
        display: contents !important;
    }
    body.page-id-10 [data-elementor-type="wp-page"] .elementor-column {
        width: 100% !important;
        max-width: 100% !important;
        flex: 0 0 100% !important;
        margin-bottom: 2rem !important;
    }
    body.page-id-10 .elementor-element-623a2c4 { order: 1 !important; }
    body.page-id-10 .elementor-element-5e1a885 { order: 2 !important; }
    body.page-id-10 .elementor-element-b1d6555 { order: 3 !important; }
    body.page-id-10 .elementor-element-6b3030e { order: 4 !important; }
    body.page-id-10 .elementor-element-05de656 { order: 5 !important; }
}

/* ---------------------------------------------------------------- *
 * 3. Notes (page-id-1898) — single scroll, capped image size
 * ---------------------------------------------------------------- */
body.page-id-1898 .elementor-section .elementor-column {
    width: 100% !important;
    max-width: 100% !important;
    flex: 0 0 100% !important;
}
body.page-id-1898 .elementor-section img {
    display: block;
    margin: 0 auto;
    max-width: 720px !important;
    width: 100% !important;
    height: auto !important;
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
 * 7. Beyond the Studio (page-id-1177) — 1 column, centered subsection labels
 * ---------------------------------------------------------------- */
body.page-id-1177 .elementor-section .elementor-column {
    width: 100% !important;
    max-width: 720px !important;
    flex: 0 0 100% !important;
    margin-left: auto !important;
    margin-right: auto !important;
}
body.page-id-1177 .elementor-section img {
    display: block;
    margin: 0 auto;
}
body.page-id-1177 .elementor-widget-heading h5.elementor-heading-title {
    font-size: 1.5rem !important;
    font-weight: 600 !important;
    letter-spacing: 0.01em;
    margin: 0.6em 0 0.3em;
    text-align: center !important;
}
/* Hero "Beyond the Studio" + lead paragraph stay normal */
body.page-id-1177 .elementor-element-1b54d5a h5.elementor-heading-title {
    font-size: inherit !important;
    font-weight: 400 !important;
    text-align: inherit !important;
}

/* ---------------------------------------------------------------- *
 * 8. /bookshelf/ (page-id-808) — JS injects a horizontal cover strip
 *    per shelf. CSS here styles the strip.
 * ---------------------------------------------------------------- */
body.page-id-808 .tl-shelf-strip {
    display: flex;
    gap: 0.6rem;
    overflow-x: auto;
    padding: 0.6rem 0 1rem;
    margin: 0.3rem 0 1rem;
    scrollbar-width: thin;
}
body.page-id-808 .tl-shelf-strip img {
    height: 110px;
    width: auto;
    flex: 0 0 auto;
    border-radius: 2px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.12);
    background: #fff;
}
</style>
<?php
}

function tl_fia_polish_js() {
    if (!is_page(808)) return; // only on /bookshelf/
    // Pre-curated first-N cover URLs from each shelf subpage (covers
    // already live in /wp-content/uploads/, so reusing them costs nothing).
    $shelves = [
        // section data-id  =>  list of cover image URLs
        '7f476f6' => [
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/1-ARtforum-November-1980-817x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/2-Artforum-Marchh-1981-872x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/3-Artforum-April-1981-886x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/4-Artforum-May-1981-925x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/5-Artforum-September-1981-956x1024.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/6-Artforum-October-1981-960x1024.png',
        ],
        'be9922f' => [
            'https://www.thomaslawson.com/wp-content/uploads/2023/01/Afterall6-cover-654x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/01/Afterall7-642x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/01/Afterall9-cover-644x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/01/Afterall11-cover-647x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/01/Afterall12-cover-642x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2024/01/Afterall13-656x1024.jpg',
        ],
        '2fe6730' => [
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/The-Journey-West-1024x716.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Andrea-Bowers-Interview-1024x592.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Institutional-Whitewash-1024x712.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Liz-Glynn-1024x640.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/Michael-Asher-obit.png',
            'https://www.thomaslawson.com/wp-content/uploads/2023/02/4-Taxis-1024x712.png',
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
        '91e3759' => [
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/650-Thomas-Lawson-at-LAXART-1.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Christopher-Miles.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Donald-Kuspit-1024x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Ingrid-Sischy-1014x1024.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Jeane-Silverthorne-1990.jpg',
            'https://www.thomaslawson.com/wp-content/uploads/2023/04/Jeanne-Silverthorn-Summer-1985.jpg',
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
        // Append at the end of the section's inner container so it sits
        // beneath the heading + link.
        var inner = section.querySelector('.elementor-container') || section;
        inner.appendChild(strip);
    });
})();
</script>
<?php
}
