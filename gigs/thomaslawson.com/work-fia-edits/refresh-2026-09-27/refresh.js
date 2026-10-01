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

    /* ---- 0. Fixes from the 2026-09-30 meeting (Fía). They run first, so the
       sections below see the corrected words and links. ---- */
    var UP = 'https://www.thomaslawson.com/wp-content/uploads/';
    var MEDIA = UP + 'tl-refresh/';
    var main = document.querySelector('main') || document.getElementById('content') || body;
    function on(id) { return body.classList.contains('page-id-' + id); }
    function retext(root, re, to) {
        var walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
        var node;
        while ((node = walker.nextNode())) {
            if (re.test(node.nodeValue)) node.nodeValue = node.nodeValue.replace(re, to);
            re.lastIndex = 0;
        }
    }
    function el(tag, cls, text) {
        var e = document.createElement(tag);
        if (cls) e.className = cls;
        if (text) e.textContent = text;
        return e;
    }
    /* Hedi Slimane's name. */
    retext(main, /Silmane/g, 'Slimane');
    /* The Municipal Building mural is one portrait of the city. */
    /* Fía: "Portrait of New York" is singular wherever it appears. */
    retext(body, /Portraits of New York/g, 'Portrait of New York');
    if (on(1622)) {
        /* Tom's notes: Portrait of New York (1989) comes before Memory Lingers
           Here, which opens "As I was completing the New York project". */
        var tops = Array.prototype.slice.call(main.querySelectorAll('.elementor-top-section'));
        var textOf = function (sec) { var t = sec.querySelector('.elementor-widget-text-editor'); return t ? t.textContent : ''; };
        var memIdx = tops.findIndex(function (sec) { return /As I was completing the New York project/.test(textOf(sec)); });
        var porIdx = tops.findIndex(function (sec) { return /knowing I could rely on Russell Rainbolt/.test(textOf(sec)); });
        if (memIdx > -1 && porIdx > memIdx && !main.querySelector('.tl-moved')) {
            tops.slice(porIdx).forEach(function (sec) {
                sec.classList.add('tl-moved');
                tops[memIdx].parentNode.insertBefore(sec, tops[memIdx]);
            });
        }
    }
    /* Yeats's Deirdre (Tom's notes: "fix spelling"). */
    if (on(1660)) retext(main, /\bDeidre\b/g, 'Deirdre');
    /* REALLIFE 15 pointed at an unrelated East of Borneo article; until its
       scan is uploaded, the cover opens the cover. */
    if (on(1819)) {
        var r15 = window.TL_SHELVES && window.TL_SHELVES['reallife-15-cover.jpg'];
        if (r15) r15.h = UP + '2023/12/REALLIFE-15-cover.jpg';
        main.querySelectorAll('a[href*="the-journey-west"]').forEach(function (a) {
            var col = a.closest('.elementor-column');
            if (col && col.querySelector('img[src*="REALLIFE-15-cover"]')) a.href = UP + '2023/12/REALLIFE-15-cover.jpg';
        });
    }
    /* Deirdre: Tom's opening film of birds, beside the paragraph about it. */
    if (on(1660) && !document.querySelector('.tl-video')) {
        var birds = Array.prototype.find.call(main.querySelectorAll('.elementor-widget-text-editor'), function (t) {
            return /video of birds/i.test(t.textContent);
        });
        if (birds) {
            var vf = el('figure', 'tl-video');
            var v = document.createElement('video');
            v.controls = true; v.preload = 'none'; v.setAttribute('playsinline', '');
            v.poster = MEDIA + 'deirdre-birds-poster.jpg';
            v.setAttribute('aria-label', 'Birds, the opening film for Deirdre');
            var vs = document.createElement('source');
            vs.src = MEDIA + 'deirdre-birds.mp4'; vs.type = 'video/mp4';
            v.appendChild(vs);
            vf.appendChild(v);
            vf.appendChild(el('figcaption', '', 'Birds, the opening film for Deirdre'));
            birds.parentNode.insertBefore(vf, birds.nextSibling);
        }
    }
    /* Art School: the last four items had paragraph captions; give them the
       same label as every other item, and name the Sohrab Mohebbi interview. */
    if (on(1878)) {
        main.querySelectorAll('.elementor-widget-image + .elementor-widget-text-editor').forEach(function (t) {
            var text = clean(t.textContent);
            if (!text || text.length > 200) return;
            if (/Sohrab Mohebbi/.test(text)) text = 'Sohrab Mohebbi Interview';
            var w = el('div', 'elementor-element elementor-widget elementor-widget-heading tl-caption-label');
            var c = el('div', 'elementor-widget-container');
            var h = el('h5', 'elementor-heading-title elementor-size-default', text);
            h.setAttribute('data-tl-role', 'caption');
            c.appendChild(h); w.appendChild(c);
            t.replaceWith(w);
        });
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

    /* ---- 9. Section headers: image left, text right (Fía, 2026-09-28). ----
       Like the Burning Torch About pages: the picture whole, as a square or
       rectangle at its own proportions, beside a narrow column of title +
       intro — instead of a slim full-width band that cropped it (About cut
       Tom's face). The authored section stays in the DOM, hidden; its intro
       widgets move into the new column so nothing is duplicated.
         ar    fallback proportions until the image reports its own
         m     phone crop (portraits only), with its focal point           */
    /* Recent work from Valise (window.TL_RECENT, newest first, printed by the
       plugin on Home and In the Studio). */
    var RECENT = (window.TL_RECENT || []).filter(function (w) { return w && w.u; });
    function valiseSize(u, width) { return u.replace(/\/rs:fit:\d+:\d+\//, '/rs:fit:' + width + ':0/'); }
    function recentLead() {
        /* The newest landscape picture reads best beside the intro. */
        return RECENT.filter(function (w) { return w.w && w.h && w.w >= w.h; })[0] || RECENT[0] || null;
    }
    var SPLIT = {
        'page-id-68':   { sel: '.elementor-element-3d58b5f', title: 'About' },
        'page-id-140':  { sel: '.elementor-element-71fa6aa', ar: '720/556', recent: true, bg: 'https://www.thomaslawson.com/wp-content/uploads/2022/09/2010_Tree_HR.jpg' },
        'page-id-1177': { sel: '.elementor-element-1b54d5a' },
        'page-id-1147': { sel: '.elementor-element-ba54885' },
        'page-id-808':  { sel: '.elementor-element-825b6e9', m: '4/5', focus: '50% 42%' },
        'page-id-1898': { sel: '.elementor-element-1553c2e', title: 'News', m: '4/5', focus: '50% 62%' }
    };
    function bestSrc(img) {
        var set = (img.getAttribute('srcset') || '').split(',').map(function (s) {
            var p = s.trim().split(/\s+/);
            return { url: p[0], w: parseInt(p[1], 10) || 0 };
        }).filter(function (c) { return c.url && c.w; });
        /* The smallest file that fills a half-screen column (≥ 1000px wide),
           else the largest there is — never a thumbnail (News has only
           225w beside its 1920w original). */
        set.sort(function (a, b) { return a.w - b.w; });
        var fit = set.filter(function (c) { return c.w >= 1000; })[0] || set[set.length - 1];
        return (fit && fit.url) || img.getAttribute('src') || img.currentSrc;
    }
    Object.keys(SPLIT).some(function (bodyClass) {
        if (!body.classList.contains(bodyClass) || document.querySelector('.tl-split')) return false;
        var cfg = SPLIT[bodyClass];
        var section = document.querySelector(cfg.sel);
        if (!section) return true;
        var src = '', alt = '', ar = cfg.ar || '';
        var srcImg = section.querySelector('.elementor-widget-image img');
        var lead = cfg.recent && recentLead();
        if (lead) {
            src = valiseSize(lead.u, 1400);
            alt = lead.t + (lead.ys ? ', ' + lead.ys : '');
            if (lead.w && lead.h) ar = lead.w + '/' + lead.h;
        } else if (srcImg) {
            src = bestSrc(srcImg);
            alt = srcImg.getAttribute('alt') || '';
            var w = parseInt(srcImg.getAttribute('width'), 10), h = parseInt(srcImg.getAttribute('height'), 10);
            if (w && h) ar = w + '/' + h;
        } else if (cfg.bg) {
            var bg = getComputedStyle(section, '::before').backgroundImage || '';
            var m = bg.match(/url\(["']?([^"')]+)["']?\)/);
            src = (m && m[1]) || cfg.bg;
        }
        if (!src) return true;
        var heading = section.querySelector('.tl-doorway-source .elementor-heading-title, .elementor-widget-heading:not(.tl-about-heading) .elementor-heading-title');
        var titleText = cfg.title || (heading ? clean(heading.textContent) : '');

        var split = document.createElement('header');
        split.className = 'tl-split';
        split.style.setProperty('--tl-ar', ar || '1/1');
        if (cfg.m) split.style.setProperty('--tl-ar-m', cfg.m);
        if (cfg.focus) split.style.setProperty('--tl-focus', cfg.focus);
        var fig = document.createElement('figure');
        fig.className = 'tl-split-media';
        var img = document.createElement('img');
        img.src = src;
        img.alt = alt || titleText;
        img.decoding = 'async';
        img.addEventListener('load', function () {
            if (img.naturalWidth && img.naturalHeight) split.style.setProperty('--tl-ar', img.naturalWidth + '/' + img.naturalHeight);
        });
        fig.appendChild(img);
        var col = document.createElement('div');
        col.className = 'tl-split-text';
        var h1 = document.createElement('h1');
        h1.className = 'tl-split-title';
        h1.textContent = titleText;
        col.appendChild(h1);
        section.querySelectorAll('.elementor-widget-text-editor').forEach(function (t) { col.appendChild(t); });
        split.appendChild(fig);
        split.appendChild(col);
        section.parentNode.insertBefore(split, section);
        section.classList.add('tl-split-source');
        section.setAttribute('aria-hidden', 'true');
        return true;
    });

    /* ---- 10. Beyond the Studio as an index (Fía, 2026-09-30). ----
       One row per project: title, its opening lines, and a strip of its
       pictures (window.TL_BTS, read from each project page by the plugin).
       Exhibitions leads the list; Curatorial Projects is gone from here,
       since Art in a Broader Context already holds those shows. ---- */
    if (on(1177) && !document.querySelector('.tl-bts-index')) {
        var BTS = window.TL_BTS || {};
        var CVS = window.TL_CV;
        var cards = [], seen = {}, holders = [];
        main.querySelectorAll('a[href*="/beyond-the-studio-"]').forEach(function (a) {
            if (a.closest('.tl-split')) return;
            var m = a.getAttribute('href').match(/\/(beyond-the-studio-[^\/?#]+)/);
            if (!m || seen[m[1]]) return;
            seen[m[1]] = true;
            var col = a.closest('.elementor-column') || a.parentElement;
            var img = col.querySelector('img');
            cards.push({ slug: m[1], href: a.getAttribute('href'), title: clean(col.textContent), img: img && (img.currentSrc || img.src) });
            var sec = a.closest('.elementor-top-section');
            if (sec && holders.indexOf(sec) < 0) holders.push(sec);
        });
        /* Tom's notes: Early New York before Painted Installations, as it happened. */
        var iE = cards.findIndex(function (c) { return /early-new-york/.test(c.slug); });
        var iP = cards.findIndex(function (c) { return /painted-installations/.test(c.slug); });
        if (iE > -1 && iP > -1 && iP < iE) cards.splice(iP, 0, cards.splice(iE, 1)[0]);
        if (cards.length) {
            var list = el('ol', 'tl-bts-index');
            var row = function (href, title, text, imgs, n) {
                var li = el('li', 'tl-bts-row');
                var a = el('a', 'tl-bts-row-link');
                a.href = href;
                var head = el('span', 'tl-bts-row-head');
                head.appendChild(el('span', 'tl-bts-row-n', n));
                head.appendChild(el('span', 'tl-bts-row-title', title));
                a.appendChild(head);
                if (text) a.appendChild(el('span', 'tl-bts-row-text', text));
                if (imgs.length) {
                    var strip = el('span', 'tl-bts-row-strip');
                    imgs.slice(0, 6).forEach(function (u) {
                        var im = document.createElement('img');
                        im.src = u; im.alt = ''; im.loading = 'lazy'; im.decoding = 'async';
                        strip.appendChild(im);
                    });
                    a.appendChild(strip);
                }
                li.appendChild(a);
                return li;
            };
            var n = 0;
            var pad = function (i) { return (i < 10 ? '0' : '') + i; };
            if (CVS && CVS.exhibitions) {
                var ex = CVS.exhibitions;
                list.appendChild(row('/exhibitions/', 'Exhibitions', ex.solo + ' solo and ' + ex.group + ' group exhibitions, ' + ex.from + ' – ' + ex.to + '.', [ex.img], pad(++n)));
            }
            cards.forEach(function (c) {
                var d = BTS[c.slug] || {};
                var imgs = (d.imgs && d.imgs.length) ? d.imgs : (c.img ? [c.img] : []);
                list.appendChild(row(c.href, c.title, d.text || '', imgs, pad(++n)));
            });
            var first = holders[0];
            first.parentNode.insertBefore(list, first);
            holders.forEach(function (h) { h.classList.add('tl-bts-source'); h.setAttribute('aria-hidden', 'true'); });
        }
    }

    /* ---- 11. A small lightbox: About's pictures and the new studio
       periods open full size (Fía, 2026-09-30). ---- */
    function lightbox(src, caption, opener) {
        var ov = el('div', 'tl-lb');
        ov.setAttribute('role', 'dialog');
        ov.setAttribute('aria-modal', 'true');
        ov.setAttribute('aria-label', caption || 'Image');
        var im = document.createElement('img');
        im.src = src; im.alt = caption || '';
        var fig = el('figure', 'tl-lb-figure');
        fig.appendChild(im);
        if (caption) fig.appendChild(el('figcaption', 'tl-lb-caption', caption));
        var close = el('button', 'tl-lb-close', '×');
        close.type = 'button';
        close.setAttribute('aria-label', 'Close');
        ov.appendChild(fig); ov.appendChild(close);
        function shut() {
            ov.remove();
            document.removeEventListener('keydown', key);
            document.documentElement.classList.remove('tl-lb-open');
            if (opener && opener.focus) opener.focus();
        }
        function key(e) { if (e.key === 'Escape') shut(); }
        ov.addEventListener('click', function (e) { if (e.target !== im) shut(); });
        document.addEventListener('keydown', key);
        document.documentElement.classList.add('tl-lb-open');
        body.appendChild(ov);
        close.focus();
    }
    function largest(img) {
        var best = { url: img.currentSrc || img.src, w: 0 };
        (img.getAttribute('srcset') || '').split(',').forEach(function (s) {
            var p = s.trim().split(/\s+/), w = parseInt(p[1], 10) || 0;
            if (p[0] && w > best.w && w <= 2600) best = { url: p[0], w: w };
        });
        return best.url;
    }
    if (on(68)) {
        main.querySelectorAll('.elementor-widget-image img').forEach(function (img) {
            if (img.closest('.tl-split, .tl-split-source, a')) return;
            var col = img.closest('.elementor-column');
            var label = col ? Array.prototype.map.call(col.querySelectorAll('.elementor-heading-title'), function (h) { return clean(h.textContent); }).filter(function (t) { return t && t !== '-' && t !== '–'; }).join(', ') : '';
            img.classList.add('tl-zoomable');
            img.tabIndex = 0;
            img.setAttribute('role', 'button');
            img.setAttribute('aria-label', 'Enlarge' + (label ? ': ' + label : ''));
            var open = function () { lightbox(largest(img), label, img); };
            img.addEventListener('click', open);
            img.addEventListener('keydown', function (e) { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); open(); } });
        });
    }
    document.querySelectorAll('a.tl-zoom').forEach(function (a) {
        a.addEventListener('click', function (e) {
            if (e.metaKey || e.ctrlKey || e.shiftKey) return;
            e.preventDefault();
            lightbox(a.href, a.getAttribute('data-caption') || '', a);
        });
    });

    /* ---- 12. In the Studio: the two newest periods, from Valise
       (window.TL_PERIODS), above 2017 – 2020. ---- */
    var PERIODS = window.TL_PERIODS || [];
    if (on(140) && PERIODS.length && !document.querySelector('.tl-period')) {
        var oldest = main.querySelector('.elementor-top-section img[src*="2019_Head-in-Hands"]');
        var before = oldest && oldest.closest('.elementor-top-section');
        if (before) {
            PERIODS.forEach(function (p) {
                var sec = el('section', 'tl-period');
                var a = el('a', 'tl-period-link');
                a.href = '/' + p.slug + '/';
                a.setAttribute('aria-label', p.title);
                var lab = el('span', 'tl-period-label');
                lab.appendChild(el('span', 'tl-period-range', p.title));
                a.appendChild(lab);
                var works = el('span', 'tl-period-works');
                p.works.forEach(function (w) {
                    var im = document.createElement('img');
                    im.src = valiseSize(w.u, 700); im.alt = w.t; im.loading = 'lazy'; im.decoding = 'async';
                    if (w.w && w.h) { im.width = w.w; im.height = w.h; }
                    works.appendChild(im);
                });
                a.appendChild(works);
                sec.appendChild(a);
                before.parentNode.insertBefore(sec, before);
            });
        }
    }

    /* ---- 13. Home: a carousel of recent work from Valise. ---- */
    function mountCarousel(tries) {
        if (document.querySelector('.tl-carousel')) return;
        /* The polish plugin builds .tl-home-artwork around the painting. */
        var holder = document.querySelector('.tl-home-artwork');
        if (!holder) { if (tries < 30) setTimeout(function () { mountCarousel(tries + 1); }, 100); return; }
        {
            var slides = RECENT.slice(0, 12);
            var car = el('section', 'tl-carousel');
            car.setAttribute('aria-roledescription', 'carousel');
            car.setAttribute('aria-label', 'Recent work');
            var stage = el('div', 'tl-carousel-stage');
            var cap = el('p', 'tl-carousel-caption');
            cap.setAttribute('aria-live', 'polite');
            var figs = slides.map(function (w, i) {
                var a = el('a', 'tl-carousel-slide');
                a.href = '/' + (w.y >= 2022 ? 'inthestudio_2022-present' : 'inthestudio_2020-2022') + '/';
                a.setAttribute('aria-roledescription', 'slide');
                a.setAttribute('aria-label', (i + 1) + ' of ' + slides.length + ': ' + w.t);
                var im = document.createElement('img');
                im.src = valiseSize(w.u, 1200); im.alt = w.t; im.decoding = 'async';
                if (i > 1) im.loading = 'lazy';
                a.appendChild(im);
                stage.appendChild(a);
                return a;
            });
            var prev = el('button', 'tl-carousel-btn tl-carousel-prev', '←');
            var next = el('button', 'tl-carousel-btn tl-carousel-next', '→');
            prev.type = next.type = 'button';
            prev.setAttribute('aria-label', 'Previous work');
            next.setAttribute('aria-label', 'Next work');
            var foot = el('div', 'tl-carousel-foot');
            foot.appendChild(cap);
            var ctl = el('span', 'tl-carousel-controls');
            ctl.appendChild(prev); ctl.appendChild(next);
            foot.appendChild(ctl);
            car.appendChild(stage); car.appendChild(foot);
            var cur = 0, timer = null;
            var still = window.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches;
            var show = function (i) {
                cur = (i + figs.length) % figs.length;
                figs.forEach(function (f, j) {
                    f.classList.toggle('is-on', j === cur);
                    f.tabIndex = j === cur ? 0 : -1;
                    f.setAttribute('aria-hidden', j === cur ? 'false' : 'true');
                });
                var w = slides[cur];
                cap.textContent = w.t + (w.ys ? ', ' + w.ys : '');
            };
            var stop = function () { clearInterval(timer); timer = null; };
            var go = function () { if (!still && !timer) timer = setInterval(function () { show(cur + 1); }, 5000); };
            prev.addEventListener('click', function () { stop(); show(cur - 1); });
            next.addEventListener('click', function () { stop(); show(cur + 1); });
            car.addEventListener('mouseenter', stop);
            car.addEventListener('mouseleave', go);
            car.addEventListener('focusin', stop);
            var x0 = null;
            stage.addEventListener('touchstart', function (e) { x0 = e.touches[0].clientX; }, { passive: true });
            stage.addEventListener('touchend', function (e) {
                if (x0 === null) return;
                var dx = e.changedTouches[0].clientX - x0; x0 = null;
                if (Math.abs(dx) > 40) { stop(); show(cur + (dx < 0 ? 1 : -1)); }
            });
            show(0);
            holder.innerHTML = '';
            holder.appendChild(car);
            go();
        }
    }
    if (on(10) && RECENT.length > 1) mountCarousel(0);

    /* ---- 14. News: one grid, newest first, with Tom's find from a JetBlue
       screen — the Portrait of New York mural behind a Law & Order scene. ---- */
    if (on(1898) && !document.querySelector('.tl-news-grid')) {
        var items = [], secs = [];
        main.querySelectorAll('.elementor-top-section').forEach(function (sec) {
            if (sec.closest('.tl-split') || sec.classList.contains('tl-split-source')) return;
            var cols = sec.querySelectorAll('.elementor-top-column');
            var got = false;
            cols.forEach(function (col) {
                var img = col.querySelector('.elementor-widget-image img');
                var title = col.querySelector('.elementor-heading-title');
                if (!img || !title) return;
                var link = img.closest('a') || col.querySelector('a');
                items.push({ href: link ? link.href : '', ext: link ? link.target === '_blank' : false, src: largest(img), title: clean(title.textContent) });
                got = true;
            });
            if (got) secs.push(sec);
        });
        if (secs.length) {
            items.unshift({ href: '/beyond-the-studio-portraits-of-new-york/', ext: false, src: MEDIA + 'law-and-order-portrait-of-new-york.jpg', title: 'Portrait of New York, spotted behind a scene of Law & Order (early 1990s)' });
            var grid = el('section', 'tl-news-grid');
            items.forEach(function (it) {
                var art = el('article', 'tl-news-item');
                var a = el('a', 'tl-news-cover');
                if (it.href) a.href = it.href;
                if (it.ext) { a.target = '_blank'; a.rel = 'noopener'; }
                var im = document.createElement('img');
                im.src = it.src; im.alt = ''; im.loading = 'lazy'; im.decoding = 'async';
                a.appendChild(im);
                art.appendChild(a);
                var h = el('h2', 'tl-news-title');
                var ha = el('a', '', it.title);
                if (it.href) ha.href = it.href;
                if (it.ext) { ha.target = '_blank'; ha.rel = 'noopener'; ha.className = 'tl-opens'; }
                h.appendChild(ha);
                art.appendChild(h);
                grid.appendChild(art);
            });
            secs[0].parentNode.insertBefore(grid, secs[0]);
            secs.forEach(function (sx) { sx.classList.add('tl-news-source'); sx.setAttribute('aria-hidden', 'true'); });
        }
    }
})();
