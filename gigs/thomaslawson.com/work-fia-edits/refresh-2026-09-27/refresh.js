/* TL design refresh — 2026-09-27. See refresh.css for the system. */
(function () {
    var body = document.body;
    body.classList.add('tl-refresh');
    var path = location.pathname.replace(/\/+$/, '');
    if (/^\/beyond-the-studio-/.test(path)) body.classList.add('tl-project-detail');
    /* 1983–1987 was built as elementor-428, so the polish slug test missed it. */
    if (path === '/elementor-428') body.classList.add('tl-studio-detail');

    function clean(value) {
        return (value || '').replace(/[​-‍﻿]/g, '').replace(/\s+/g, ' ').trim();
    }

    /* ---- 1. All-caps labels → title case (authored caps, not CSS). ---- */
    var KEEP = /^(REALLIFE|MOCA|LACE|LAXART|CAE|PS1|P\.S\.1|USA|UK|US|LA|NY|NYC|TV|BBC|CCA|SVA|UCLA|ICA|II|III|IV|VI|VII|VIII|IX|XI|XII|TL|DJ)$/;
    var SMALL = /^(a|an|and|as|at|by|for|from|in|into|of|on|or|the|to|with)$/;
    function titleCase(text) {
        var index = 0;
        return text.replace(/[A-Z0-9][A-Z0-9.'’&-]*/g, function (word) {
            var first = index++ === 0;
            if (KEEP.test(word.replace(/[.,;:]+$/, ''))) return word;
            var lower = word.toLowerCase();
            if (!first && SMALL.test(lower)) return lower;
            return lower.replace(/(^|[-'’.])([a-z])/g, function (m, sep, ch) {
                return sep + ch.toUpperCase();
            });
        });
    }
    function uncap(node) {
        var text = clean(node.textContent);
        if (text.length < 4 || /[a-z]/.test(text) || !/[A-Z]{3}/.test(text)) return;
        if (KEEP.test(text)) return;
        var walker = document.createTreeWalker(node, NodeFilter.SHOW_TEXT);
        var first = true;
        while (walker.nextNode()) {
            var t = walker.currentNode;
            if (!clean(t.nodeValue)) continue;
            var out = titleCase(t.nodeValue);
            if (!first) out = out.replace(/^(\s*)([a-z])/, function (m, s, c) { return s + c; });
            t.nodeValue = out;
            first = false;
        }
    }
    document.querySelectorAll('.elementor-heading-title, figcaption, .wp-caption-text').forEach(uncap);

    /* ---- 2. Bookshelf detail headers: one title, no echo. ---- */
    if (body.classList.contains('tl-bookshelf-detail')) {
        var head = document.querySelector('[data-elementor-type="wp-page"] > .elementor-top-section');
        if (head) {
            var hs = head.querySelectorAll('.elementor-heading-title');
            if (hs.length >= 2) {
                var h1 = hs[0], sub = hs[1];
                var a = clean(h1.textContent).toLowerCase(), b = clean(sub.textContent).toLowerCase();
                if (/^(other|publications)$/.test(a) && b) {
                    h1.textContent = clean(sub.textContent);
                    sub.closest('.elementor-widget').style.display = 'none';
                } else if (a === b) {
                    sub.closest('.elementor-widget').style.display = 'none';
                }
            }
        }
    }

    /* ---- 3. Snap every heading onto the five-step scale. ---- */
    var skip = [
        '.tl-doorway-sign', '.tl-home-titles', '.page-id-140 .elementor-top-section',
        '.tl-studio-detail [data-elementor-type="wp-page"] > .elementor-top-section:first-child',
        '.page-id-1527', '.site-header', '.site-footer'
    ].join(',');
    document.querySelectorAll('.elementor-heading-title').forEach(function (h) {
        if (h.closest(skip)) return;
        var cs = getComputedStyle(h);
        var px = parseFloat(cs.fontSize);
        var weight = parseInt(cs.fontWeight, 10) || 400;
        var tag = h.tagName;
        var role;
        if (px >= 40) role = 'display';
        else if (px >= 25) role = 'title';
        else if (px >= 19) role = 'subhead';
        else if (px >= 15) role = 'label';
        else if (tag === 'H6' || weight <= 300 || cs.fontStyle === 'italic') role = 'meta';
        else role = 'caption';
        h.setAttribute('data-tl-role', role);
    });
    /* A lone page's first big heading is its title; make it the display step. */
    var firstTitle = document.querySelector('[data-elementor-type="wp-page"] [data-tl-role="title"], [data-elementor-type="wp-page"] [data-tl-role="display"]');
    if (firstTitle && !body.classList.contains('page-id-10')) firstTitle.setAttribute('data-tl-role', 'display');

    /* ---- 4. Eyebrow: every detail page says which section it's in. ---- */
    var sections = [
        ['tl-studio-detail', 'In the Studio', '/in-the-studio/'],
        ['tl-bookshelf-detail', 'Bookshelf', '/bookshelf/'],
        ['tl-exhibition-detail', 'Art in a Broader Context', '/art-in-a-broader-context/'],
        ['tl-project-detail', 'Beyond the Studio', '/beyond-the-studio/']
    ];
    sections.some(function (s) {
        if (!body.classList.contains(s[0]) || document.querySelector('.tl-eyebrow')) return false;
        var link = document.createElement('a');
        link.className = 'tl-eyebrow';
        link.href = s[2];
        link.textContent = s[1];
        if (s[0] === 'tl-studio-detail') {
            var box = document.querySelector('[data-elementor-type="wp-page"] > .elementor-top-section:first-child > .elementor-container');
            if (box) box.insertBefore(link, box.firstChild);
        } else if (firstTitle) {
            var widget = firstTitle.closest('.elementor-widget');
            if (widget) widget.parentNode.insertBefore(link, widget);
        }
        return true;
    });
})();
