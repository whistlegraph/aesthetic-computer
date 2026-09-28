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
            /* Capitalise after a hyphen or initial's period, never after an
               apostrophe ("Architect's", not "Architect'S"). */
            return lower.replace(/(^|[-.])([a-z])/g, function (m, sep, ch) {
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

    /* ---- 5. Fía, 2026-09-27: Bookshelf sections as one tight list. ----
       Each cover becomes a row — cover, title (↗ when it opens a text),
       caption — in page order. Captions come from Tom's CV where a cover's
       own link matches a CV entry (window.TL_SHELVES, refresh-data.js). */
    var SHELVES = window.TL_SHELVES || {};
    function coverKey(src) {
        return (src || '').split('?')[0].split('/').pop().replace(/-\d+x\d+(?=\.\w+$)/, '').toLowerCase();
    }
    function opensText(href) {
        return /\.pdf$/i.test(href) || (/^https?:/i.test(href) && !/thomaslawson\.com\/(?!wp-content)/i.test(href));
    }
    if (body.classList.contains('tl-bookshelf-detail')) {
        var page = document.querySelector('[data-elementor-type="wp-page"]');
        var sectionsList = page ? Array.prototype.slice.call(page.querySelectorAll(':scope > .elementor-top-section, :scope > .e-con')) : [];
        var itemSections = sectionsList.slice(1);
        var list = document.createElement('div');
        list.className = 'tl-shelf-list';
        itemSections.forEach(function (section) {
            var widgets = Array.prototype.slice.call(section.querySelectorAll('.elementor-widget-image, .elementor-widget-heading'));
            var current = null;
            widgets.forEach(function (w) {
                if (w.classList.contains('elementor-widget-image')) {
                    var img = w.querySelector('img');
                    if (!img) return;
                    current = { img: img, link: w.querySelector('a'), heads: [] };
                    list.appendChild(buildItem(current));
                    current.row = list.lastChild;
                } else if (current) {
                    var h = w.querySelector('.elementor-heading-title');
                    if (h && clean(h.textContent).replace(/^[-–—]+$/, '')) current.heads.push(h);
                    fillCaption(current);
                }
            });
        });
        if (list.children.length && itemSections.length) {
            itemSections[0].parentNode.insertBefore(list, itemSections[0]);
            itemSections.forEach(function (sec) { sec.classList.add('tl-shelf-source'); });
        }
        function buildItem(item) {
            var k = coverKey(item.img.getAttribute('src'));
            var data = SHELVES[k] || null;
            item.data = data;
            var row = document.createElement('article');
            row.className = 'tl-shelf-item';
            row.id = 'tl-item-' + k.replace(/\.\w+$/, '').replace(/[^a-z0-9]+/g, '-');
            var href = (data && data.h) || (item.link && item.link.getAttribute('href')) || '';
            var cover = document.createElement(href ? 'a' : 'div');
            cover.className = 'tl-shelf-item-cover';
            if (href) { cover.href = href; if (opensText(href)) { cover.target = '_blank'; cover.rel = 'noopener'; } cover.setAttribute('aria-hidden', 'true'); cover.tabIndex = -1; }
            var im = item.img.cloneNode(true);
            im.removeAttribute('width'); im.removeAttribute('height'); im.setAttribute('sizes', '120px'); // picks the ~260w variant, not the 1 MB 768w PNG
            im.loading = 'lazy';
            cover.appendChild(im);
            var text = document.createElement('div');
            text.className = 'tl-shelf-item-text';
            var title = document.createElement('h3');
            title.className = 'tl-shelf-item-title';
            var meta = document.createElement('p');
            meta.className = 'tl-shelf-item-meta';
            text.appendChild(title); text.appendChild(meta);
            row.appendChild(cover); row.appendChild(text);
            item.href = href;
            return row;
        }
        function fillCaption(item) {
            var data = item.data;
            var t = (data && data.t) || (item.heads[0] ? clean(item.heads[0].textContent) : '');
            var m = (data && data.m) || (item.heads[1] ? clean(item.heads[1].textContent) : '');
            var title = item.row.querySelector('.tl-shelf-item-title');
            var meta = item.row.querySelector('.tl-shelf-item-meta');
            title.textContent = '';
            if (item.href) {
                var a = document.createElement('a');
                a.href = item.href;
                a.textContent = t;
                if (opensText(item.href)) { a.target = '_blank'; a.rel = 'noopener'; a.className = 'tl-opens'; }
                title.appendChild(a);
            } else {
                title.textContent = t;
            }
            meta.textContent = m;
            meta.hidden = !m;
        }
        list.querySelectorAll('.tl-shelf-item').forEach(function (row) {
            if (!row.querySelector('.tl-shelf-item-title').textContent) {
                var k = row.id.replace(/^tl-item-/, '');
                var data = Object.keys(SHELVES).filter(function (x) { return x.replace(/\.\w+$/, '').replace(/[^a-z0-9]+/g, '-') === k; })[0];
                if (data) fillCaption({ row: row, data: SHELVES[data], heads: [], href: row.querySelector('a') && row.querySelector('a').getAttribute('href') });
            }
        });
        /* Arriving from a Bookshelf cover: bring that item into view. */
        function reveal() {
            var target = location.hash && document.getElementById(location.hash.slice(1));
            if (!target || !target.classList.contains('tl-shelf-item')) return;
            target.classList.add('is-target');
            target.scrollIntoView({ block: 'center' });
        }
        reveal();
        window.addEventListener('hashchange', reveal);
    }

    /* Bookshelf landing: each cover opens its own item, not the whole shelf. */
    if (body.classList.contains('page-id-808')) {
        function relink() {
            document.querySelectorAll('.tl-shelf-cover').forEach(function (a) {
                var img = a.querySelector('img');
                var k = img && coverKey(img.getAttribute('src'));
                var data = k && SHELVES[k];
                if (!data || a.dataset.tlRelinked) return;
                a.href = '/' + data.s + '/#tl-item-' + k.replace(/\.\w+$/, '').replace(/[^a-z0-9]+/g, '-');
                a.dataset.tlRelinked = '1';
            });
        }
        relink();
        setTimeout(relink, 0);
        window.addEventListener('load', relink);
    }

    /* ---- 6. Fía, 2026-09-27: Studio captions as one line —
       Title (italic), year ⇥ materials ⇥ dimensions. ---- */
    if (body.classList.contains('tl-studio-detail')) {
        var wp = document.querySelector('[data-elementor-type="wp-page"]');
        var all = wp ? Array.prototype.slice.call(wp.querySelectorAll('.elementor-widget-image, .elementor-widget-heading')) : [];
        var group = null;
        function flush() {
            if (!group || !group.heads.length) return;
            var parts = [];
            group.heads.forEach(function (h) {
                h.innerHTML.split(/<br\s*\/?>/i).forEach(function (seg) {
                    var tmp = document.createElement('span'); tmp.innerHTML = seg;
                    var plain = clean(tmp.textContent);
                    if (plain) parts.push({ html: seg.trim(), text: plain, italic: !!tmp.querySelector('i, em') });
                });
            });
            if (!parts.length) return;
            var title = '', year = '', rest = [];
            var first = parts[0];
            var m = first.text.match(/^(.*?),\s*((?:c\.\s*)?\d{4}(?:\s*[-–]\s*\d{2,4})?)\s*$/);
            if (m) { title = m[1]; year = m[2]; rest = parts.slice(1); }
            else {
                title = first.text;
                var y = parts[1] && parts[1].text.match(/^((?:c\.\s*)?\d{4}(?:\s*[-–]\s*\d{2,4})?)$/);
                if (y) { year = y[1]; rest = parts.slice(2); } else rest = parts.slice(1);
            }
            var cap = document.createElement('p');
            cap.className = 'tl-cap';
            var t = document.createElement('span');
            t.className = 'tl-cap-title';
            var i = document.createElement('i');
            i.textContent = title;
            t.appendChild(i);
            if (year) t.appendChild(document.createTextNode(', ' + year));
            cap.appendChild(t);
            rest.forEach(function (r) {
                var sp = document.createElement('span');
                sp.className = 'tl-cap-detail';
                sp.textContent = r.text.replace(/\s+[xX]\s+/g, ' × ');
                cap.appendChild(sp);
            });
            /* Inside the image widget, so the caption starts at the picture's edge. */
            var holder = group.image.querySelector('.elementor-widget-container') || group.image;
            holder.appendChild(cap);
            group.heads.forEach(function (h) { h.closest('.elementor-widget').classList.add('tl-cap-source'); });
        }
        all.forEach(function (w) {
            if (w.closest('.elementor-top-section') === wp.querySelector(':scope > .elementor-top-section')) return;
            if (w.classList.contains('elementor-widget-image')) { flush(); group = w.querySelector('.tl-cap') ? null : { heads: [], image: w }; }
            else if (group) {
                var h = w.querySelector('.elementor-heading-title');
                if (h && clean(h.textContent)) group.heads.push(h);
            }
        });
        flush();
    }

    /* ---- 7. Studio materials from Valise (Fía, 2026-09-27). ----
       The plugin prints Tom's Valise vault server-side as window.TL_VALISE
       ([title, year, medium, dimensions]); any caption the page left bare
       gets its materials and size, matched by title (+ year when known). */
    var VALISE = window.TL_VALISE || [];
    if (VALISE.length && body.classList.contains('tl-studio-detail')) {
        var key = function (t) {
            return clean(t).toLowerCase().replace(/[‘’']/g, '').replace(/&/g, 'and')
                .replace(/\(.*?\)/g, '').replace(/[^a-z0-9]+/g, ' ').replace(/^(the|a|an) /, '').trim();
        };
        var byTitle = {};
        VALISE.forEach(function (w) { (byTitle[key(w[0])] = byTitle[key(w[0])] || []).push(w); });
        var matched = 0;
        document.querySelectorAll('.tl-cap').forEach(function (cap) {
            if (cap.querySelector('.tl-cap-detail')) return;
            var t = cap.querySelector('.tl-cap-title i');
            if (!t) return;
            var year = (cap.querySelector('.tl-cap-title').textContent.match(/(\d{4})\s*$/) || [])[1];
            var hits = byTitle[key(t.textContent)] || [];
            /* One work with this title: take it (Valise often has no year, or a
               different one). Several: the year has to agree. */
            var w = hits.length === 1 ? hits[0]
                : hits.filter(function (h) { return year && String(h[1]).indexOf(year) === 0; })[0] || null;
            if (!w) return;
            [w[2], w[3]].forEach(function (v) {
                if (!v) return;
                var sp = document.createElement('span');
                sp.className = 'tl-cap-detail';
                sp.textContent = clean(v).replace(/(\d)\s*[xX×]\s*(?=\d)/g, '$1 × ');
                cap.appendChild(sp);
            });
            matched++;
        });
        body.setAttribute('data-tl-valise', matched + '/' + document.querySelectorAll('.tl-cap').length);
    }

    /* ---- 8. Exhibition pages carry the index card's venue (Fía, 2026-09-27). ----
       "Venue · date" under the title, keeping the page's own (more exact) date. */
    var CONTEXT = {
        '/elementor-1878': ['CalArts, Valencia, California', ''],
        '/art-in-context-dissent': ['LACE, Los Angeles', ''],
        '/art-in-context-hot-coffee': ['REDCAT, Los Angeles', ''],
        '/art-in-context-hot-coffee-2': ['Artists Space, New York', '1997'],   /* page said 1973 — the Douthwaite year */
        '/art-in-context-shimmer': ['Municipal Art Gallery at Barnsdall Park, Los Angeles', ''],
        '/art-in-context-the-british-art-show': ['Manchester · Edinburgh · Cardiff', ''],
        '/art-in-context-nostalgia-as-reference': ['P.S.1 and The Clocktower, New York', ''],
        '/art-in-context-livin-in-the-usa': ['Damon Brandt Gallery, New York', ''],
        '/critical-perspectives-art-in-context': ['P.S.1, New York', ''],
        '/reallife-magazine-presents-whitecolumns-art-in-context': ['White Columns, New York', ''],
        '/art-in-context-reallife-presents': ['Nigel Greenwood Gallery, London', ''],
        '/pat-douthewaite-art-in-context': ['St Andrews Festival, St Andrews', '']
    };
    var ctx = CONTEXT[path];
    if (ctx && firstTitle && !document.querySelector('.tl-context-line')) {
        if (path === '/pat-douthewaite-art-in-context') {
            document.querySelectorAll('.elementor-heading-title').forEach(function (h) {
                h.innerHTML = h.innerHTML.replace(/Douthewaite/g, 'Douthwaite');
            });
            document.title = document.title.replace(/Douthewaite/g, 'Douthwaite');
        }
        var tw = firstTitle.closest('.elementor-widget');
        var next = tw && tw.nextElementSibling;
        var dateNode = next && next.classList.contains('elementor-widget-heading') ? next.querySelector('.elementor-heading-title') : null;
        var date = ctx[1] || (dateNode ? clean(dateNode.textContent) : '');
        date = date.replace(/\s*[-–]\s*/g, '–');
        var line = document.createElement('p');
        line.className = 'tl-context-line';
        line.textContent = [ctx[0], date].filter(Boolean).join(' · ');
        if (dateNode) next.style.display = 'none';
        tw.parentNode.insertBefore(line, tw.nextSibling);
    }
})();
