<?php
// Bootstrap WordPress to check current user
define('SHORTINIT', false);
$wp_load = dirname(__DIR__) . '/wp-load.php';
if (!file_exists($wp_load)) {
    // Try absolute path as fallback
    $wp_load = $_SERVER['DOCUMENT_ROOT'] . '/wp-load.php';
}
require_once $wp_load;

$wp_user = null;
if (function_exists('is_user_logged_in') && is_user_logged_in()) {
    $current = wp_get_current_user();
    $wp_user = [
        'id'       => $current->ID,
        'login'    => $current->user_login,
        'name'     => $current->display_name,
        'email'    => $current->user_email,
        'isAdmin'  => current_user_can('manage_options'),
        'isEditor' => current_user_can('edit_posts'),
        'roles'    => $current->roles,
    ];
}
?>
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Thomas Lawson — Artwork Index</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Poppins:ital,wght@0,300;0,400;0,500;0,600;1,400&display=swap" rel="stylesheet">
<style>
  :root {
    --bg: #fff9ef;
    --fg: #222222;
    --muted: #6f6f6f;
    --border: #e8e2d6;
    --accent: #0170B9;
    --card-bg: #ffffff;
    --font: "Poppins", "Helvetica Neue", Helvetica, Arial, sans-serif;
  }
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    font-family: var(--font);
    background: var(--bg);
    color: var(--fg);
    font-size: 15px;
    line-height: 1.6;
    -webkit-font-smoothing: antialiased;
  }

  /* Header */
  header {
    padding: 2rem 2rem 1.5rem;
    border-bottom: 1px solid var(--border);
    max-width: 1400px;
    margin: 0 auto;
    display: flex;
    align-items: center;
    gap: 1.5rem;
    flex-wrap: wrap;
  }
  header a {
    display: block;
    line-height: 0;
  }
  header .logo {
    height: 40px;
    width: auto;
  }
  header .page-title {
    font-size: 0.85rem;
    font-weight: 300;
    letter-spacing: 0.06em;
    text-transform: uppercase;
    color: var(--muted);
  }
  header .sub {
    color: var(--muted);
    font-size: 0.78rem;
    margin-left: auto;
    font-weight: 300;
  }

  /* User badge */
  .user-badge {
    font-size: 0.72rem;
    font-weight: 400;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }
  .user-badge .user-name {
    color: var(--fg);
  }
  .user-badge .user-role {
    font-size: 0.62rem;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    padding: 0.15rem 0.5rem;
    border-radius: 2px;
    font-weight: 500;
  }
  .user-badge .role-admin {
    background: #0170B9;
    color: #fff;
  }
  .user-badge .role-editor {
    background: #e8e2d6;
    color: var(--muted);
  }
  .user-badge .role-viewer {
    background: #f5f0e6;
    color: var(--muted);
  }
  .user-badge a {
    color: var(--accent);
    text-decoration: none;
    font-size: 0.72rem;
  }
  .user-badge a:hover { text-decoration: underline; }

  /* Filters */
  .filters {
    max-width: 1400px;
    margin: 0 auto;
    padding: 1rem 2rem;
    display: flex;
    gap: 0.75rem;
    flex-wrap: wrap;
    align-items: center;
    border-bottom: 1px solid var(--border);
  }
  .filters label {
    font-size: 0.75rem;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: var(--muted);
  }
  .filters select, .filters input {
    font-family: var(--font);
    font-size: 0.8rem;
    font-weight: 300;
    padding: 0.4rem 0.7rem;
    border: 1px solid var(--border);
    border-radius: 2px;
    background: var(--card-bg);
    color: var(--fg);
    outline: none;
  }
  .filters select:focus, .filters input:focus {
    border-color: var(--accent);
  }
  .filters input[type="search"] {
    width: 220px;
  }
  .count {
    margin-left: auto;
    font-size: 0.8rem;
    color: var(--muted);
  }

  /* Tabs */
  .tabs {
    max-width: 1400px;
    margin: 0 auto;
    padding: 0 2rem;
    display: flex;
    gap: 0;
    border-bottom: 1px solid var(--border);
  }
  .tabs button {
    font-family: var(--font);
    font-size: 0.78rem;
    font-weight: 400;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    padding: 0.8rem 1.4rem;
    border: none;
    background: none;
    color: var(--muted);
    cursor: pointer;
    border-bottom: 2px solid transparent;
    transition: all 0.2s;
  }
  .tabs button.active {
    color: var(--fg);
    border-bottom-color: var(--accent);
    font-weight: 500;
  }
  .tabs button:hover { color: var(--fg); }

  /* Card */
  .card {
    background: var(--card-bg);
    border: 1px solid var(--border);
    border-radius: 2px;
    overflow: hidden;
    transition: box-shadow 0.2s, transform 0.2s;
    cursor: pointer;
  }
  .card:hover {
    box-shadow: 0 4px 20px rgba(0,0,0,0.07);
    transform: translateY(-2px);
  }
  .card .img-wrap {
    width: 100%;
    aspect-ratio: 1;
    overflow: hidden;
    background: #f5f0e6;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 0.75rem;
  }
  .card .img-wrap img {
    max-width: 100%;
    max-height: 100%;
    object-fit: contain;
  }
  .card .info {
    padding: 0.8rem 1rem 1rem;
    border-top: 1px solid var(--border);
  }
  .card .info h3 {
    font-size: 0.82rem;
    font-weight: 500;
    line-height: 1.35;
    margin-bottom: 0.2rem;
    color: var(--fg);
  }
  .card .info .year {
    font-size: 0.78rem;
    color: var(--muted);
    font-weight: 300;
  }
  .card .info .medium {
    font-size: 0.73rem;
    color: var(--muted);
    margin-top: 0.2rem;
    font-style: italic;
    font-weight: 300;
  }
  .card .info .dims {
    font-size: 0.7rem;
    color: var(--muted);
    margin-top: 0.1rem;
    font-weight: 300;
  }
  .card .info .tags {
    margin-top: 0.4rem;
    display: flex;
    gap: 0.3rem;
    flex-wrap: wrap;
  }
  .card .info .tag {
    font-size: 0.62rem;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    padding: 0.15rem 0.45rem;
    background: #f5f0e6;
    border-radius: 2px;
    color: var(--muted);
    font-weight: 400;
  }

  .card .info .serial {
    font-size: 0.78rem;
    font-family: "SF Mono", "Menlo", "Monaco", monospace;
    color: #b0a99a;
    margin-top: 0.4rem;
    letter-spacing: 0.05em;
  }
  .card .img-wrap {
    position: relative;
  }
  .card .img-count {
    position: absolute;
    bottom: 0.4rem;
    right: 0.4rem;
    font-size: 0.6rem;
    background: rgba(0,0,0,0.55);
    color: #fff;
    padding: 0.1rem 0.4rem;
    border-radius: 2px;
    font-weight: 400;
  }

  /* Section headers */
  .section-header {
    max-width: 1400px;
    margin: 0 auto;
    padding: 2.5rem 2rem 0.5rem;
  }
  .section-header:first-child {
    padding-top: 1.5rem;
  }
  .section-header h2 {
    font-size: 1rem;
    font-weight: 500;
    letter-spacing: 0.04em;
    color: var(--fg);
    margin-bottom: 0.4rem;
  }
  .section-header .section-line {
    width: 40px;
    height: 2px;
    background: var(--accent);
    margin-bottom: 0.8rem;
  }
  .section-intros {
    max-width: 700px;
  }
  .section-intros .intro-block {
    margin-bottom: 1rem;
  }
  .section-intros .intro-heading {
    font-size: 0.82rem;
    font-weight: 500;
    font-style: italic;
    color: var(--fg);
    margin-bottom: 0.25rem;
  }
  .section-intros .intro-text {
    font-size: 0.8rem;
    font-weight: 300;
    color: var(--muted);
    line-height: 1.65;
  }
  .section-grid {
    max-width: 1400px;
    margin: 0 auto;
    padding: 1rem 2rem 1rem;
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(260px, 1fr));
    gap: 1.8rem;
  }

  /* Writings list */
  .writings-list {
    max-width: 1400px;
    margin: 0 auto;
    padding: 1.5rem 2rem 4rem;
  }
  .writing-row {
    display: grid;
    grid-template-columns: 1fr auto auto;
    gap: 1rem;
    padding: 0.6rem 0;
    border-bottom: 1px solid var(--border);
    align-items: baseline;
  }
  .writing-row .w-title {
    font-size: 0.85rem;
    font-weight: 400;
  }
  .writing-row .w-pub {
    font-size: 0.8rem;
    color: var(--muted);
    font-style: italic;
    font-weight: 300;
  }
  .writing-row .w-year {
    font-size: 0.8rem;
    color: var(--muted);
    min-width: 3rem;
    text-align: right;
    font-weight: 300;
  }

  /* Exhibitions */
  .exhibitions-grid {
    max-width: 1400px;
    margin: 0 auto;
    padding: 1.5rem 2rem 4rem;
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
    gap: 1.5rem;
  }
  .exhibition-card {
    background: var(--card-bg);
    border: 1px solid var(--border);
    border-radius: 2px;
    padding: 1.2rem;
  }
  .exhibition-card h3 {
    font-size: 0.88rem;
    font-weight: 500;
    margin-bottom: 0.3rem;
  }
  .exhibition-card .e-year {
    font-size: 0.8rem;
    color: var(--muted);
    font-weight: 300;
  }
  .exhibition-card .e-images {
    display: flex;
    gap: 0.4rem;
    margin-top: 0.6rem;
    overflow-x: auto;
  }
  .exhibition-card .e-images img {
    height: 60px;
    border-radius: 2px;
    object-fit: cover;
  }

  /* Lightbox */
  .lightbox {
    display: none;
    position: fixed;
    inset: 0;
    background: rgba(0,0,0,0.85);
    z-index: 100;
    align-items: center;
    justify-content: center;
    cursor: pointer;
  }
  .lightbox.open { display: flex; }
  .lightbox img {
    max-width: 90vw;
    max-height: 90vh;
    object-fit: contain;
  }
  .lightbox .lb-info {
    position: absolute;
    bottom: 2rem;
    left: 50%;
    transform: translateX(-50%);
    color: #ccc;
    text-align: center;
    font-size: 0.85rem;
    max-width: 600px;
  }
  .lightbox .lb-info h3 {
    color: #fff;
    font-size: 1rem;
    margin-bottom: 0.3rem;
  }

  @media (max-width: 640px) {
    header { padding: 1.2rem 1rem; }
    .filters { padding: 0.8rem 1rem; }
    .tabs { padding: 0 1rem; }
    .grid { padding: 1rem; grid-template-columns: repeat(auto-fill, minmax(160px, 1fr)); gap: 1rem; }
    .writings-list { padding: 1rem; }
    .writing-row { grid-template-columns: 1fr auto; }
    .writing-row .w-pub { display: none; }
  }
</style>
</head>
<body>

<header>
  <a href="https://www.thomaslawson.com/"><img class="logo" src="https://www.thomaslawson.com/wp-content/uploads/2022/07/THOMAS-LAWSON-1-301x99.png" alt="Thomas Lawson"></a>
  <span class="page-title">Artwork Index</span>
  <div class="sub" id="stats"></div>
  <div class="user-badge" id="user-badge"></div>
</header>

<div class="tabs" id="tabs">
  <button class="active" data-tab="artworks">Artworks</button>
  <button data-tab="writings">Writings</button>
  <button data-tab="exhibitions">Exhibitions</button>
</div>

<div id="artworks-view">
  <div class="filters" id="artwork-filters">
    <label>Section</label>
    <select id="filter-section"><option value="">All</option></select>
    <label>Series</label>
    <select id="filter-series"><option value="">All</option></select>
    <label>Search</label>
    <input type="search" id="filter-search" placeholder="Title...">
    <span class="count" id="artwork-count"></span>
  </div>
  <div id="artwork-sections"></div>
</div>

<div id="writings-view" style="display:none">
  <div class="filters">
    <label>Category</label>
    <select id="filter-w-cat"><option value="">All</option></select>
    <label>Search</label>
    <input type="search" id="filter-w-search" placeholder="Title...">
    <span class="count" id="writing-count"></span>
  </div>
  <div class="writings-list" id="writings-list"></div>
</div>

<div id="exhibitions-view" style="display:none">
  <div class="exhibitions-grid" id="exhibitions-grid"></div>
</div>

<div class="lightbox" id="lightbox">
  <img id="lb-img" src="" alt="">
  <div class="lb-info">
    <h3 id="lb-title"></h3>
    <div id="lb-detail"></div>
  </div>
</div>

<script>
const WP_USER = <?php echo $wp_user ? json_encode($wp_user) : 'null'; ?>;
const CATALOG = {
  "artist": {
    "name": "Thomas Lawson",
    "born": 1951,
    "birthplace": "Glasgow, Scotland",
    "website": "https://www.thomaslawson.com",
    "contact": "studio@thomaslawson.com"
  },
  "sections": [
    {
      "pageId": 347,
      "label": "In the Studio: 1977–1979",
      "section": "in-the-studio",
      "period": "1977-1979",
      "tag": null,
      "wpTitle": "InTheStudio_1977-1979",
      "sourceUrl": "https://www.thomaslawson.com/inthestudio_1977-1979/",
      "intros": []
    },
    {
      "pageId": 401,
      "label": "In the Studio: 1980–1982",
      "section": "in-the-studio",
      "period": "1980-1982",
      "tag": null,
      "wpTitle": "InTheStudio_1980-1982",
      "sourceUrl": "https://www.thomaslawson.com/inthestudio_1980-1982/",
      "intros": []
    },
    {
      "pageId": 428,
      "label": "In the Studio: 1983–1987",
      "section": "in-the-studio",
      "period": "1983-1987",
      "tag": null,
      "wpTitle": "InTheStudio_1983-1987",
      "sourceUrl": "https://www.thomaslawson.com/elementor-428/",
      "intros": []
    },
    {
      "pageId": 457,
      "label": "In the Studio: 1987–1990",
      "section": "in-the-studio",
      "period": "1987-1990",
      "tag": null,
      "wpTitle": "InTheStudio_1987-1990",
      "sourceUrl": "https://www.thomaslawson.com/inthestudio_1987-1990/",
      "intros": []
    },
    {
      "pageId": 476,
      "label": "In the Studio: 1991–1993",
      "section": "in-the-studio",
      "period": "1991-1993",
      "tag": null,
      "wpTitle": "InTheStudio_1991-1993",
      "sourceUrl": "https://www.thomaslawson.com/inthestudio_1991-1993/",
      "intros": []
    },
    {
      "pageId": 486,
      "label": "In the Studio: 1994–1998",
      "section": "in-the-studio",
      "period": "1994-1998",
      "tag": null,
      "wpTitle": "InTheStudio_1994-1998",
      "sourceUrl": "https://www.thomaslawson.com/inthestudio_1994-1998/",
      "intros": []
    },
    {
      "pageId": 504,
      "label": "In the Studio: 1999–2006",
      "section": "in-the-studio",
      "period": "1999-2006",
      "tag": null,
      "wpTitle": "InTheStudio_1999-2006",
      "sourceUrl": "https://www.thomaslawson.com/inthestudio_1999-2006/",
      "intros": []
    },
    {
      "pageId": 524,
      "label": "In the Studio: 2006–2010",
      "section": "in-the-studio",
      "period": "2006-2010",
      "tag": null,
      "wpTitle": "InTheStudio_2006-2010",
      "sourceUrl": "https://www.thomaslawson.com/inthestudio_2006-2010/",
      "intros": []
    },
    {
      "pageId": 544,
      "label": "In the Studio: 2015–2016",
      "section": "in-the-studio",
      "period": "2015-2016",
      "tag": null,
      "wpTitle": "InTheStudio_2015-2016",
      "sourceUrl": "https://www.thomaslawson.com/inthestudio_2015-2016/",
      "intros": []
    },
    {
      "pageId": 560,
      "label": "In the Studio: 2017–2020",
      "section": "in-the-studio",
      "period": "2017-2020",
      "tag": null,
      "wpTitle": "InTheStudio_2017-2020",
      "sourceUrl": "https://www.thomaslawson.com/inthestudio_2017-2020/",
      "intros": []
    },
    {
      "pageId": 1552,
      "label": "Painted Installations",
      "section": "beyond-the-studio",
      "period": null,
      "tag": "painted installations",
      "wpTitle": "Beyond the Studio &#8211; Painted Installations",
      "sourceUrl": "https://www.thomaslawson.com/beyond-the-studio-painted-installations/",
      "intros": [
        {
          "heading": "Painted Installations",
          "text": "During the latter half of the 80s I added a contextual layer to my painting exhibitions by painting various color patterns on the wall. The idea was to extend the individual works into a larger whole, to make the entire gallery space a container which brought the individual pieces into conversations with one another."
        },
        {
          "heading": "Anthony Reynolds",
          "text": "This was the first time I tried out the idea. I thought it would be funny to paint a very schematic plaid pattern on the wall, a pattern based on a design I found on a plastic shopping bag, for my first show in a London gallery. But The Guardian reviewer didn’t see the humor and came at me with one of the oldest character smears the English bring to the Scots, that we are grim and ‘dour,’ unable to see the joy in life. Anthony was one of the most supportive gallerists I ever worked with, and I returned several times to paint on his walls."
        },
        {
          "heading": "The Party’s Over",
          "text": "For this show at MetroPictures n early 1987 I built a semi-circular wall to contain an arrangement of paintings in a continuous narrative. I had been doing research on 19th Century panorama paintings for an essay in Artforum, and that informed the project. The panorama here was of life in the later Reagan years in the US, spanning self-congratulatory wealth and power to abject poverty and sickness. In the front part of the gallery I arranged several almost abstract paintings over an elongated triangle painted pale blue."
        },
        {
          "heading": "Freedom of Choice",
          "text": "Some months after The Party’s Over at MetroPictures Ron Onorato invited me to do a show at the La Jolla Museum of Contemporary Art (now expanded to be the San Diego Museum of Contemporary Art, MCASD) in. Here I rein stalled the second part of the MetroPictures show, tying the two walls together with a large graphic with two arrows pointing away from a photographic close-up of the US Capitol building. In the small gallery overlooking the ocean I built a makeshift billboard with photographic images of the Manhattan skyline at night and oversized martini glasses, punctuated by round fluourescent lights. The idea being to continue the discourse on spectacularized consumerism."
        }
      ]
    },
    {
      "pageId": 1580,
      "label": "Dark Installations",
      "section": "beyond-the-studio",
      "period": null,
      "tag": "dark installations",
      "wpTitle": "Beyond the Studio &#8211; Dark Installations",
      "sourceUrl": "https://www.thomaslawson.com/beyond-the-studio-dark-installations/",
      "intros": [
        {
          "heading": "Dark Installations",
          "text": "Between 1989 and 1992 I made a number of room installations that were dark in color and in theme. I had discovered a new kind of iridescent paint that responded in interesting ways under artificial light, and combined this with various dark matte backgrounds to create somber spaces on which to hang paintings, and sometimes add sound."
        },
        {
          "heading": "Forrest of Signs",
          "text": "Forrest of Signs was a comprehensive survey of the “Pictures” phenomenon, organized by MoCA LA for what was then called the Temporary Contemporary, in 1989. Each artist was given space to create a large installation. I painted the walls of my space a dark blue, and lit a selection of old and new work with theatrical spots. I think there was also a sound component, but can’t remember what."
        },
        {
          "heading": "Gallery Too",
          "text": "This installation was done at a distance; I mailed instructions to the gallery staff regarding color (a pale blue I think) and placement of the two B&W photos (outtakes from the Portrait of New York project)."
        },
        {
          "heading": "For Derry",
          "text": "Also in 1989, Declan McGonagle, the director of the Orchard Gallery in Derry, Northern Ireland, invited me to devise an installation that would address the fraught situation there. During the site visit I found a city sharply divided, with areas marked, through murals and painted curbs, as “no go” to the opposing populations. A tense peace was imposed by the constant patrols of the British army through the center and the neighborhoods. In the center, an old market town surrounded by a thick stone wall, I found a massive memorial to the dead of the First World War – a column topped by an angel, and surrounded by soldiers lunging forward with bayonets. A shockingly brutal reminder of British rule (parallel to the imposed British name for the city, Londonderry). I used images from this memorial, along with some illicitly taken photographs of current British army patrols, and included some words from both Yeats and Joyce."
        },
        {
          "heading": "El sueno Imperativo",
          "text": "In 1991 the Spanish curator Mar Villaspesa invited me to participate in a group show at the Circulo de Bellas Artes in central Madrid. Dating from the late 19th century, the Circulo had a complex history as both an important venue for contemporary art and film, but as a one-time hang out for supporters of the Francoist regime. After a site visit I sought to address that history, working with a craftsman at the San Fernando Real Academia de Bellas Artes to create a human-sized replica of the wing of a “fallen angel” in the nearby Parque de el Retiro. This wing, surrounded by a ring of red neon, was then placed at the foot of the grand staircase of the Circulo, to be viewed from the upper floors."
        },
        {
          "heading": "Tiefe Nacht",
          "text": "On moving to Los Angeles I began a series based on photographs on houses on fire. I used the primitive computer lab at CalArts to manipulate the images and then had then printed on canvas. After the riots in April 1992, I decided to drop the project out of concern that it might carry more meaning than intended. Foundation Arts Resources (FAR) had negotiated a short lease of a disused bank building in downtown LA, and made a huge open show later in the year. I commandeered a room, painted it red, with a text culled from Goethe’s Faust running around the ceiling molding, and installed one of the paintings. There was a soundtrack, a female voice reading a longer passage from Goethe’s epic poem, a passage about the burning of a house in the woods."
        },
        {
          "heading": "Vienna Academy",
          "text": "I conducted a workshop on public art at the Academy of Fine Art in Vienna during the summer of 1992. As part of the project I worked with a couple of students on this temporary mural sketch."
        }
      ]
    },
    {
      "pageId": 1608,
      "label": "Early New York",
      "section": "beyond-the-studio",
      "period": null,
      "tag": "early new york",
      "wpTitle": "Beyond the Studio &#8211; Early New York",
      "sourceUrl": "https://www.thomaslawson.com/beyond-the-studio-early-new-york/",
      "intros": [
        {
          "heading": "Early New York",
          "text": "During the latter half of the 80s I added a contextual layer to my painting exhibitions by painting various color patterns on the wall. The idea was to extend the individual works into a larger whole, to make the entire gallery space a container which brought the individual pieces into conversations with one another."
        },
        {
          "heading": "Civic Virtue/Civil Rights",
          "text": "This was the first time I tried out the idea. I thought it would be funny to paint a very schematic plaid pattern on the wall, a pattern based on a design I found on a plastic shopping bag, for my first show in a London gallery. But The Guardian reviewer didn’t see the humor and came at me with one of the oldest character smears the English bring to the Scots, that we are grim and ‘dour,’ unable to see the joy in life. Anthony was one of the most supportive gallerists I ever worked with, and I returned several times to paint on his walls."
        },
        {
          "heading": "City Hall Park",
          "text": "For this show at MetroPictures n early 1987 I built a semi-circular wall to contain an arrangement of paintings in a continuous narrative. I had been doing research on 19th Century panorama paintings for an essay in Artforum, and that informed the project. The panorama here was of life in the later Reagan years in the US, spanning self-congratulatory wealth and power to abject poverty and sickness. In the front part of the gallery I arranged several almost abstract paintings over an elongated triangle painted pale blue."
        }
      ]
    },
    {
      "pageId": 1622,
      "label": "Temporary Murals",
      "section": "beyond-the-studio",
      "period": null,
      "tag": "temporary murals",
      "wpTitle": "Beyond the Studio &#8211; Temporary Murals",
      "sourceUrl": "https://www.thomaslawson.com/beyond-the-studio-portraits-of-new-york/",
      "intros": [
        {
          "heading": "Temporary Murals",
          "text": "I met Russell Rainbolt at some point during 1988. He was in a class I was teaching at SVA, and was an established sign-painter, working for a billboard company in New Haven, CT. At some point during our conversations about painting he told me that he had arranged for me to have access to one of the company’s billboards on I-95, and that if I would give him an 8×10 drawing, he would take care of the rest. Intrigued, I put together a composite image of the art historian Bernard Berenson admiring a sculpture, flanked by rearing horses. A couple of weeks later he invited me up to see the finished project."
        },
        {
          "heading": "Memory Lingers Here",
          "text": "As I was completing the New York project the Irish curator, Declan McGonagle, invited me to participate in the First Tyne International. This was to be an examination of artists’ responses to urban renewal, set in Gateshead, the impoverished twin of Newcastle in the north of England. A site, I zeroed in on the walls of an abandoned soap factory, and drew up plans for another large scale temporary mural based on images of a statue of St George that had been somehow abandoned and overwhelmed by a municipal parking structure. Armed with my drawings, Russell Rainbolt went ahead to paint the panels on site, and I joined him in March 1990 to supervise the installation. It was while this was happening that I got a call from someone at CalArts to say that I was shortlisted for the position of dean of the Art School, and could I come in for an interview with the search committee."
        },
        {
          "heading": "Portraits of New York",
          "text": "The following year, knowing I could rely on Russell Rainbolt’s expertise, I submitted a proposal to New York City’s General Services Administration to install a temporary mural on the scaffolding surrounding the Municipal Building during a major renovation. My proposal was simple – to create a portrait of the city through its public statuary — and I was awarded the commission. I suspect that the committee that selected the work thought the project unpolitical, but my research had shown me that public statuary was only political, a system of recognizing and rewarding the powerful, and the interests of the powerful. At that time, the only non-allegorical woman represented on the streets of New York was Golda Meir. The only African-Americans were supplicants at the feet of white abolitionists, or, in the case of the infamous equestrian statue of Teddy Roosevelt, half-naked retainers supporting the great man. In composing the mural, which wrapped around the entire building, I sought to bring attention to this by alternating close-up faces of those less seen with a seeming endless parade of the politicians striking rhetorical poses as if making public speeches. The work was in situ for approximately three years."
        }
      ]
    },
    {
      "pageId": 1646,
      "label": "Glasgow Projects",
      "section": "beyond-the-studio",
      "period": null,
      "tag": "glasgow projects",
      "wpTitle": "Beyond the Studio &#8211; Glasgow Projects",
      "sourceUrl": "https://www.thomaslawson.com/beyond-the-studio-glasgow-projects/",
      "intros": [
        {
          "heading": "Glasgow Projects",
          "text": "I left my hometown, Glasgow, in 1969, to go to college. During the next few years I returned intermittently, mostly during holidays, but after 1972 I stayed away until 1990, when I was invited to make an exhibition surveying the work I had done during the 80s, in New York. In the wake of that invitation, I made several new works in Glasgow, thinking about Glasgow."
        },
        {
          "heading": "Third Eye Centre",
          "text": "The survey exhibition was curated by Andrew Nairne, and presented at the Third Eye Centre, the contemporary art space on Sauchiehall Street now called CCA. Along with the survey of a decade’s worth of work, I was given some funds to make two new pieces. One was a painting inspired by a memorial sculpture of a materially successful Scottish artist, mounted on a wall painted with a garish approximation of the Black Watch tartan. The top of the painting was crested with a string of lights. In a separate gallery I hung a large photographic detail, printed on canvas, of a ruined fountain that stood in the city’s oldest park, Glasgow Green. In front of this I installed a scaffolding rig to hold theatrical lights."
        },
        {
          "heading": "Here’s Looking at You Kid.",
          "text": "The following year local artist Alan Dunn invited me to participate in his Bellgrove Project, basically a billboard space facing a train station in the east end of Glasgow that he controlled for a while. The station had a certain notoriety because it was the closest stop to Parkhead, the stadium of Glasgow Celtic Football Club, and thus got rowdy on Saturdays. As a result there was a heavy police presence most of the time. To make the piece I took photographs of a different group of police officers standing around the Cenotaph in George Square in the center of the city."
        },
        {
          "heading": "Shawhead Sentinel",
          "text": "In 1995 I was approached by “Art in Partnership,” a consultancy group in Edinburgh, about making a proposal for a monumental structure to function as a kind of sign for a new, interactive museum of the industrial age, planned for the village of Shawhead outside Coatbridge. The area had been at the center of Scotland’s iron and coal industries in the 19th century, but had long fallen on hard times, and the museum was intended as an economic lifeline. The design remit was for a structure that would be visible from the nearby Motorway connecting Edinburgh and Glasgow, and that it should include some sort of light feature. I designed a castellated tower whose side opened to reveal large spinning cogwheels. This area was to be lit inside, and a high intensity beam shot up vertically from the battlements. My proposal was not accepted."
        }
      ]
    },
    {
      "pageId": 1660,
      "label": "Theatre, Dance & Fashion",
      "section": "beyond-the-studio",
      "period": null,
      "tag": "theatre dance fashion",
      "wpTitle": "Beyond the Studio &#8211; Theatre Dance &#038; Fashion",
      "sourceUrl": "https://www.thomaslawson.com/beyond-the-studio-theatre-dance-fashion/",
      "intros": [
        {
          "heading": "Deidre",
          "text": "Working in theater was eye-opening, the way the project unfolded over time as everyone came to understand the text had a different quality than in the art space, perhaps not more collaborative, but there was certainly more discussion about the poet’s meanings. The play was a short piece by W.B. Yeats, his 1906 Deidre, a poetic story of love, power, and betrayal. I designed the set – a massive, curved wall to suggest the heroine’s entrapment, and giant warrior heads to represent the armed forces arraigned against her. These heads, which were modeled on the Lewis chess pieces in the Scottish National Museum, were lowered on to the set at various times in the action. I made a short video of birds rising in flight to serve as an introduction to the action. The production premiered at the MOD Theater at CalArts, and then the producer, Susan Solt, found support from the Dunnard Fund to take it to the Edinburgh Fringe Festival."
        },
        {
          "heading": "Silmane Vogue Project",
          "text": "I got a call one morning in the spring of 2013 from Dodie Kazanjian, contributing editor for Vogue, asking if I would be interested in doing a portrait of the designer Hedi Slimane for the upcoming September issue. She warned me that final approval would sit with Anna Wintour, but that the editorial team was excited to see what I would do. Flattering. I spent a morning with Slimane at his place in Trousdale Estates, chatting and taking photos. I made several paintings, finally settling on a double portrait to submit to the magazine. Anna Wintour approved, and it was published as a two page spread in the September issue, with Jennifer Lawrence on the cover.\n\nDodie Kazanjian was also director of Gallery Met at the Metropolitan Opera, and shortly afterwards she invited me to participate in a show celebrating the restaging of Alexander Borodin’s Prince Igor. I was able to repurpose some of the unpublished Slimane portraits and studies for this."
        },
        {
          "heading": "Till You Drop",
          "text": "Lauri Firstenberg invited me to participate in the LAXART Benefit, to be held at the fabled, possibly haunted, Greystone Mansion in Beverly Hills. I asked the dancer Flora Weigman and the composer Nina Waisman to collaborate on the piece. We chose an octagonal room at one end of the building, I designed a set of painted screens, Waisman constructed an interactive sound work based on various found music from the 1920s, and Wegman and her dancers improvised as they triggered the sound."
        }
      ]
    },
    {
      "pageId": 1689,
      "label": "Los Angeles",
      "section": "beyond-the-studio",
      "period": null,
      "tag": "los angeles",
      "wpTitle": "Beyond the Studio &#8211; Los Angeles",
      "sourceUrl": "https://www.thomaslawson.com/beyond-the-studio-los-angeles/",
      "intros": [
        {
          "heading": "East Rancho",
          "text": "In 2014 the Provost at Calarts, Jeannene Przyblyski, created Some Place Chronicles as a vehicle to seek funding for various community arts projects. The first grant that came through was from the office of the Supervisor of Los Angeles County, District 2, and was to support five community making projects in several unincorporated areas of the district. I became involved, and opted to work with the community of East Rancho Dominguez, a small residential area east of Compton. Throughout the next couple of years I spent a good deal of time with a group of long-term residents, attending meetings and social events, building trust and interviewing people. My assistants, Kate Kendall and Lucia Prancha also took photographs and made videos. In addition I spent hours reading through archived editions of the Compton Herald at the library of CalState Dominguez Hills, learning about the development of the area in the 30s through the 60s. The ultimate goal was to produce five separate volumes, and the designer Anther Kiley was brought on board to provide an overall template, with graduate students working on individual books. In the end I wrote a personal essay accompanied by some of my own photographs and drawings, as well the images made by Kate and Lucia. The series was finally published in collaboration with EastofBorneo Books ."
        },
        {
          "heading": "Metro LA",
          "text": "In the spring of 2021 I was invited to submit a proposal for a mural in the planned Rodeo Drive station on the Metro Purple Line. The space I was to consider was on an upper level of the station, and called for three long segments. The deadline for submission was tight, a matter of months. Walking around Beverly Hills, and riding the existing Metro I came up with a plan that incorporated some local architectural details with pictures of everyday subway riders. I was pleased with the result, but didn’t win the commission."
        }
      ]
    },
    {
      "pageId": 1711,
      "label": "The Scottish Project",
      "section": "beyond-the-studio",
      "period": null,
      "tag": "the scottish project",
      "wpTitle": "Beyond the Studio &#8211; The Scottish Project",
      "sourceUrl": "https://www.thomaslawson.com/beyond-the-studio-the-scottish-project/",
      "intros": [
        {
          "heading": "The Scottish Project",
          "text": "I spent some months in Scotland in 1997, during the months leading up to the vote that led to a devolved Scottish Parliament. A footnote in an essay on Scottish history opened up a new research topic, the life and times of Thomas Muir, a political reformer of the late 18th century who became radicalized by both his encounter with the French Revolution, and the severity of response of the British authorities to his rather mild pleas for change. I spent hours in the Scottish National Library archives, the Edinburgh Room at the Central Library in Edinburgh, and later at the Bancroft Library at UC Berkeley. I also spent a month at the Lewis Walpole Library at Yale researching their extraordinary collection of political caricatures from that period. The results were several small chapbooks, a couple of exhibitions, and a secondary series of paintings that ultimately took me in a different direction."
        },
        {
          "heading": "Escape Towards Liberty",
          "text": "The following year Nothing Moments Press in Los Angeles approached me about making a book, and I offered a third in the series. This one based on the diaries of a French trader who helped Muir escape Australia and cross the Pacific to California. To this I then added excerpts for several letters that Muir wrote to friends and family from Monterey while in captivity of the Spanish authorities. The book was illustrated by a series of drawings by Andrea Bowers, and designed by Penny Pehl and James W. Moore."
        },
        {
          "heading": "Gathered Under the Tree of Liberty",
          "text": "This 2013 book contains 39 black and white line portraits of Muir and some of his fellow reformers and revolutionists. These drawings were based on the painted portraits included in the Suburban show in 2004.  It was funded by a grant from USA Projects (United States Artists) and designed by John Wiese."
        },
        {
          "heading": "Paranoia on the High Seas",
          "text": "The second book in the series was a joint production of Distrito Cu4rto, Madrid, and White Wine Press, Santa Monica, published in 2006. The text here is based on a long letter written by a fellow political prisoner who shared Muir’s transport to the penal colony in Botany Bay."
        },
        {
          "heading": "Pest of Scotland",
          "text": "In 1999 Visual Art Projects in Glasgow awarded me an Artist Research Fellowship to work with the Spanish architect, Enric Miralles, as he designed a new Scottish Parliament Building in Edinburgh. I had several preliminary meetings with him, at his office in Barcelona and in Edinburgh, before he died suddenly, in 2000. Too upset to continue with this, I asked if I could redirect the funds to publish a small chapbook based on my Thomas Muir research. This was the result, a text based on a transcript of Muir’s trial for sedition, edited and embellished. The book was designed by Jessica Fleischman, who would later design the look of EastofBorneo."
        },
        {
          "heading": "Constructing a World View; reconstructing a journey",
          "text": "Sleeper is a small gallery in the basement of an architectural firm in Edinburgh. I was invited to make a show there in late summer of 2001, and chose to paint a map of the world directly on the wall, faced by a listing of Muir’s travels, by longitude and latitude. The map was based on an older projection. The show opened on the first weekend of that September."
        }
      ]
    },
    {
      "pageId": 1998,
      "label": "In the Studio: 2010–2015",
      "section": "in-the-studio",
      "period": "2010-2015",
      "tag": null,
      "wpTitle": "InTheStudio_2010-2015",
      "sourceUrl": "https://www.thomaslawson.com/inthestudio_2010-2015/",
      "intros": []
    }
  ],
  "artworks": [
    {
      "id": "EttDU57r",
      "serial": "TL1",
      "title": "Formanesque",
      "slug": "formanesque-1977",
      "year": 1977,
      "yearEnd": null,
      "type": "painting",
      "medium": "oilstick and graphite on paper",
      "artMedium": "oilstick, graphite",
      "artworkSurface": "paper",
      "dimensions": {
        "width": 110,
        "height": 150,
        "depth": null,
        "unit": "in",
        "text": "110 X 150 in. (25 sheets, 22 X 30 in. each)"
      },
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/001-Tlawson_Forman-esque-25-drawings1977.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Photography©2009 Fredrik Nilsen, All Rights Reserved",
          "wpMediaId": 363
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/003-TlawsonFigures-Foreman-esque1977.jpg",
          "caption": null,
          "primary": false,
          "isDetail": true,
          "detailLabel": "Running Figures",
          "credit": "Photography©2009 Fredrik Nilsen, All Rights Reserved",
          "wpMediaId": 370
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/004-Tlawson_FiguresAeroplanes-Several-21977.jpg",
          "caption": null,
          "primary": false,
          "isDetail": true,
          "detailLabel": "Several Aeroplanes",
          "credit": "Photography©2009 Fredrik Nilsen, All Rights Reserved",
          "wpMediaId": 369
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.852Z",
      "updatedAt": "2026-03-18T07:24:09.852Z"
    },
    {
      "id": "c3rZCpeA",
      "serial": "TL2",
      "title": "Highland Games",
      "slug": "highland-games-1977",
      "year": 1977,
      "yearEnd": null,
      "type": "painting",
      "medium": "oilstick and graphite on paper",
      "artMedium": "oilstick, graphite",
      "artworkSurface": "paper",
      "dimensions": {
        "width": 22,
        "height": 30,
        "depth": null,
        "unit": "in",
        "text": "22 x 30 in."
      },
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/005-Tlawson_highland-games-1977.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 372
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.852Z",
      "updatedAt": "2026-03-18T07:24:09.852Z"
    },
    {
      "id": "dKipRHiO",
      "serial": "TL3",
      "title": "Lone Piper",
      "slug": "lone-piper-1977",
      "year": 1977,
      "yearEnd": null,
      "type": "painting",
      "medium": "oilstick and enamel paint on canvas",
      "artMedium": "oilstick, enamel paint",
      "artworkSurface": "canvas",
      "dimensions": {
        "width": 54,
        "height": 54,
        "depth": null,
        "unit": "in",
        "text": "54 x 54 in."
      },
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/006-Tlawson_Lone-Piper1978.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 371
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.852Z",
      "updatedAt": "2026-03-18T07:24:09.852Z"
    },
    {
      "id": "9d059rfA",
      "serial": "TL4",
      "title": "Single Piper",
      "slug": "single-piper-1977",
      "year": 1977,
      "yearEnd": null,
      "type": "painting",
      "medium": "oilstick and graphite on paper",
      "artMedium": "oilstick, graphite",
      "artworkSurface": "paper",
      "dimensions": {
        "width": 22,
        "height": 30,
        "depth": null,
        "unit": "in",
        "text": "22 x 30  in."
      },
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/002-Tlawson_Scottish-Drawing-One-Piper1977.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 364
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.852Z",
      "updatedAt": "2026-03-18T07:24:09.852Z"
    },
    {
      "id": "CjT1wG2o",
      "serial": "TL5",
      "title": "Gold Dog",
      "slug": "gold-dog-1978",
      "year": 1978,
      "yearEnd": null,
      "type": "painting",
      "medium": "oilstick and enamel paint on canvas, with painted wooden frame",
      "artMedium": "oilstick, enamel paint",
      "artworkSurface": "canvas",
      "dimensions": {
        "width": 53,
        "height": 43,
        "depth": null,
        "unit": "in",
        "text": "53 x 43 in."
      },
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/013-Tlawson_Gold-Dog1978.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 384
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "-evy1eGq",
      "serial": "TL6",
      "title": "Good Dog",
      "slug": "good-dog-1978",
      "year": 1978,
      "yearEnd": null,
      "type": "painting",
      "medium": "oilstick on paper",
      "artMedium": "oilstick",
      "artworkSurface": "paper",
      "dimensions": {
        "width": 30,
        "height": 45,
        "depth": null,
        "unit": "in",
        "text": "diptych, 30 x 45 in."
      },
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/009-Tlawson_Gold-Dog-Black-Orange1978.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 375
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "vKe8_dOy",
      "serial": "TL7",
      "title": "Jumping Baby",
      "slug": "jumping-baby-1978",
      "year": 1978,
      "yearEnd": null,
      "type": "painting",
      "medium": "oilstick and enamel on canvas",
      "artMedium": "oilstick, enamel",
      "artworkSurface": "canvas",
      "dimensions": {
        "width": 52,
        "height": 42,
        "depth": null,
        "unit": "in",
        "text": "52 x 42 in."
      },
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/008-Tlawson_Untitled-jumping-baby-on-aqua1978.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 374
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.852Z",
      "updatedAt": "2026-03-18T07:24:09.852Z"
    },
    {
      "id": "fSxR6e8g",
      "serial": "TL8",
      "title": "Mirror Bassets",
      "slug": "mirror-bassets-1978",
      "year": 1978,
      "yearEnd": null,
      "type": "painting",
      "medium": "oilstick on paper",
      "artMedium": "oilstick",
      "artworkSurface": "paper",
      "dimensions": {
        "width": 30,
        "height": 45,
        "depth": null,
        "unit": "in",
        "text": "diptych, 30 x 45 in."
      },
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/010-Tlawson-Untitled-two-dogs-Burgundy-1979.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 376
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "1JEoBA0h",
      "serial": "TL9",
      "title": "Mirror Boxers",
      "slug": "mirror-boxers-1978",
      "year": 1978,
      "yearEnd": null,
      "type": "painting",
      "medium": "oilstick on paper",
      "artMedium": "oilstick",
      "artworkSurface": "paper",
      "dimensions": {
        "width": 30,
        "height": 45,
        "depth": null,
        "unit": "in",
        "text": "diptych, 30 x 45  in."
      },
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/012-Tlawson_Two-Boxers1978.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 383
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "sqQzaObU",
      "serial": "TL10",
      "title": "Pink Baby on Blue",
      "slug": "pink-baby-on-blue-1978",
      "year": 1978,
      "yearEnd": null,
      "type": "painting",
      "medium": "oilstick and spray enamel on paper",
      "artMedium": "oilstick, spray enamel",
      "artworkSurface": "paper",
      "dimensions": {
        "width": 44,
        "height": 30,
        "depth": null,
        "unit": "in",
        "text": "44 x 30 in."
      },
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/007-Tlawson_Baby-Pink-on-Blue1978.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 373
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.852Z",
      "updatedAt": "2026-03-18T07:24:09.852Z"
    },
    {
      "id": "Ko3OVfRQ",
      "serial": "TL11",
      "title": "Tiny Dog",
      "slug": "tiny-dog-1978",
      "year": 1978,
      "yearEnd": null,
      "type": "painting",
      "medium": "oilstick on paper",
      "artMedium": "oilstick",
      "artworkSurface": "paper",
      "dimensions": {
        "width": 30,
        "height": 45,
        "depth": null,
        "unit": "in",
        "text": "diptych, 30 x 45 in."
      },
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/011-Tlawson_Tiny-Dog-Green-Blue1978.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 382
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "8du3gq1e",
      "serial": "TL12",
      "title": "Red Shoe",
      "slug": "red-shoe-1979",
      "year": 1979,
      "yearEnd": null,
      "type": "painting",
      "medium": "oilstick and enamel paint on canvas",
      "artMedium": "oilstick, enamel paint",
      "artworkSurface": "canvas",
      "dimensions": {
        "width": 36,
        "height": 36,
        "depth": null,
        "unit": "in",
        "text": "36 x 36 in."
      },
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/07/014-Tlawson_Red-Shoe1979.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 381
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "iTwXgu04",
      "serial": "TL13",
      "title": "GREED",
      "slug": "greed-1980",
      "year": 1980,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/020-Tlawson_Greed1980.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 403
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "9i865UNn",
      "serial": "TL14",
      "title": "HE DIED LIKE INNOCENT VICTIMS",
      "slug": "he-died-like-innocent-victims-1980",
      "year": 1980,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/021-Tlawson_He-Died-Like-His-Innocent-Victims1980-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 404
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "i2SLfuBd",
      "serial": "TL15",
      "title": "WILD IN THE STREETS",
      "slug": "wild-in-the-streets-1980",
      "year": 1980,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/022-Tlawson_Wild-in-the-Streets1980.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 405
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "psCAUWm_",
      "serial": "TL16",
      "title": "A NEW BEGGINING",
      "slug": "a-new-beggining-1981",
      "year": 1981,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/023-Tlawson_Untitled-A-New-Beginning1981.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 406
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "S9SMZS-_",
      "serial": "TL17",
      "title": "BATTERED BODY FOUND IN FREEZER",
      "slug": "battered-body-found-in-freezer-1981",
      "year": 1981,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/024-Tlawson_Battered-Body-Found-in-Freezer1981.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 407
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "OmFvpO0O",
      "serial": "TL18",
      "title": "BEATEN TO DEATH",
      "slug": "beaten-to-death-1981",
      "year": 1981,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/026-Tlawson_Beaten-to-Death1981.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "JEFF MCLANE",
          "wpMediaId": 409
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "z3PEm5ga",
      "serial": "TL19",
      "title": "DON'T HIT HER AGAIN",
      "slug": "dont-hit-her-again-1981",
      "year": 1981,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/025-Tlawson_Dont-Hit-Her-Again1981.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 408
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "-iaBBLJS",
      "serial": "TL20",
      "title": "SHOT FOR A BIKE",
      "slug": "shot-for-a-bike-1981",
      "year": 1981,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/027-Tlawson_Shot-for-a-Bike1981.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Fredrik Nilsen",
          "wpMediaId": 410
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "ZDmPy8XR",
      "serial": "TL21",
      "title": "BURN BURN BURN",
      "slug": "burn-burn-burn-1982",
      "year": 1982,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/028-Tlawson_Burn-Burn-Burn1982.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 411
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "-77SneD2",
      "serial": "TL22",
      "title": "GUILTY",
      "slug": "guilty-1982",
      "year": 1982,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/034-Tlawson_Guilty1982.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 415
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "ltJabb0V",
      "serial": "TL23",
      "title": "HE SHOT BEST BUDDY (DETAIL)",
      "slug": "he-shot-best-buddy-detail-1982",
      "year": 1982,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/038-Tlawson_He-Shot-Best-Buddydetail-1982.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Fredrik Nilsen",
          "wpMediaId": 419
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "U6beb4px",
      "serial": "TL24",
      "title": "HE SHOT BEST BUDDY (PENCIL)",
      "slug": "he-shot-best-buddy-pencil-1982",
      "year": 1982,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/036-Tlawson_He-Shot-Best-Buddy-pencil1982.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 417
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "Qjoq6JYQ",
      "serial": "TL25",
      "title": "HE SHOT HIS BEST BUDDY",
      "slug": "he-shot-his-best-buddy-1982",
      "year": 1982,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/035-Tlawson_He-Shot-His-Best-Budy1982.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 416
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "qWEEiXAk",
      "serial": "TL26",
      "title": "HE SHOT HIS BEST BUDDY (SCALED)",
      "slug": "he-shot-his-best-buddy-scaled-1982",
      "year": 1982,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/037-Tlawson_He-Shot-Best-Buddy1982.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": null
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "k5oTeKOZ",
      "serial": "TL27",
      "title": "NEW YORK'S FINEST II",
      "slug": "new-yorks-finest-ii-1982",
      "year": 1982,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/030-Tlawson_New-Yorks-Finest-II-1982.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Fredrik Nilsen",
          "wpMediaId": 412
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "B7aVl7cH",
      "serial": "TL28",
      "title": "REPLACEMENT",
      "slug": "replacement-1982",
      "year": 1982,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/039-Tlawson_Replacement1982.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 420
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "L8uUAvGB",
      "serial": "TL29",
      "title": "SAVED",
      "slug": "saved-1982",
      "year": 1982,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/032-Tlawson_Saved1982.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 414
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "vkCZ8f2N",
      "serial": "TL30",
      "title": "SHE SLEW SCHOOLMATE",
      "slug": "she-slew-schoolmate-1982",
      "year": 1982,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/040-Tlawson_She-Slew-Schoolmate_HR-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 421
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "q02Z_S1Z",
      "serial": "TL31",
      "title": "SWIMMING POOL 2",
      "slug": "swimming-pool-2-1982",
      "year": 1982,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/063-Tlawson_Swimming-Pool-2_1985.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 431
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "6_OdzEFd",
      "serial": "TL32",
      "title": "THEIR BROKEN HEARTS",
      "slug": "their-broken-hearts-1982",
      "year": 1982,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/031-Tlawson_Their-Broken-Hearts1982.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 413
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=401",
      "section": "in-the-studio",
      "period": "1980-1982",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "m56voZiF",
      "serial": "TL33",
      "title": "BETRAYAL",
      "slug": "betrayal-1983",
      "year": 1983,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/052-Tlawson_Betrayal1983.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 441
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "s8_qtRmO",
      "serial": "TL34",
      "title": "METROPOLIS 1",
      "slug": "metropolis-1-1983",
      "year": 1983,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Metropolis",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/050-Tlawson_Metropolis-1_1983.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 439
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "dwHJkyCZ",
      "serial": "TL35",
      "title": "METROPOLIS, THE MUSEUM",
      "slug": "metropolis-the-museum-1983",
      "year": 1983,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Metropolis",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/051-Tlawson_Metropolis-The-Museum1983.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 440
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "Gu5qBbGu",
      "serial": "TL36",
      "title": "THE VIEW FROM BERGHOF",
      "slug": "the-view-from-berghof-1983",
      "year": 1983,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/053-Tlawson_The-View-from-the-Berghof1983.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 442
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "6noU1UXE",
      "serial": "TL37",
      "title": "CATHEDRAL ROCKS",
      "slug": "cathedral-rocks-1984",
      "year": 1984,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/055-Tlawson_Cathedral-Rocks1984.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 444
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "755YwRJO",
      "serial": "TL38",
      "title": "CHRISTMINSTER",
      "slug": "christminster-1984",
      "year": 1984,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/057-Tlawson_Christminster1984.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 446
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "nsND8_DW",
      "serial": "TL39",
      "title": "CHRISTMINSTER (INSTALL)",
      "slug": "christminster-install-1984",
      "year": 1984,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/058-Tlawson_Christminster-install1984.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 447
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "KeRnpYOS",
      "serial": "TL40",
      "title": "DEEP IN FOREST GLADE",
      "slug": "deep-in-forest-glade-1984",
      "year": 1984,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/056-Tlawson_Deep-in-Forest-Glade1984.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 445
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "ha2L7oxR",
      "serial": "TL41",
      "title": "INSTALLATION AT METRO PICTURES",
      "slug": "installation-at-metro-pictures-1984",
      "year": 1984,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/054-Tlawson_-installation-at-MetroPictures-1984.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 443
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "zKWm09wz",
      "serial": "TL42",
      "title": "KULTUR KULTUR",
      "slug": "kultur-kultur-1984",
      "year": 1984,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/059-Tlawson_Kultur-Kultur1984.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 448
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "DE7l_MwH",
      "serial": "TL43",
      "title": "COLD STORAGE",
      "slug": "cold-storage-1985",
      "year": 1985,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/062-Tlawson_Cold-Storage1985.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 430
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "jTXxMGLC",
      "serial": "TL44",
      "title": "SPIRIT OF THE MUSEUM (STUDY)",
      "slug": "spirit-of-the-museum-study-1985",
      "year": 1985,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/061-Tlawson_Spirit-of-the-Museum-Study1985.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 450
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "thXQcrhq",
      "serial": "TL45",
      "title": "TEMPLE OF THE KULTUR KRITIK",
      "slug": "temple-of-the-kultur-kritik-1985",
      "year": 1985,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/060-Tlawson_Temple-of-the-Kultur-Kritik1985.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 449
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "QMbmq1md",
      "serial": "TL46",
      "title": "BURDEN OF HISTORY",
      "slug": "burden-of-history-1986",
      "year": 1986,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/065-Tlawson_Burden-of-History1986.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 433
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "ZwoAMN8w",
      "serial": "TL47",
      "title": "HEAVEN IS A PLACE",
      "slug": "heaven-is-a-place-1986",
      "year": 1986,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/066-Tlawson_Heaven-is-a-Place1986.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Fredrik Nilsen",
          "wpMediaId": 434
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "mEpAJi6f",
      "serial": "TL48",
      "title": "HEAVEN IS A PLACE (STUDY)",
      "slug": "heaven-is-a-place-study-1986",
      "year": 1986,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/064-Tlawson_Heaven-is-a-Place-Study1986.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 432
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "mRigr8DL",
      "serial": "TL49",
      "title": "MOONDANCE 2",
      "slug": "moondance-2-1986",
      "year": 1986,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/067-Tlawson_Moondance-2_1986.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 435
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "RTaTL-Ep",
      "serial": "TL50",
      "title": "MOONDANCE 3",
      "slug": "moondance-3-1986",
      "year": 1986,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/068-Tlawson_Moondance-3_1986.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 436
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=428",
      "section": "in-the-studio",
      "period": "1983-1987",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "_piWJBgc",
      "serial": "TL51",
      "title": "Party's Over",
      "slug": "partys-over-1986",
      "year": 1986,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "painted installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/party_1-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1562
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1986_Partys-Over-1-scaled.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1556
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/party_6-scaled.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1564
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/party_4-scaled.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1563
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1987_Partys-Over-3-Metro-Study_LR.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1561
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1987_Partys-Over-The-blue_HR.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1560
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1986_Partys-Over-3-scaled.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1558
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1986_Partys-Over-2-scaled.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1557
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Glasgow-Green-Flourishing_1-scaled.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1653
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1552",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "FDOdkahe",
      "serial": "TL52",
      "title": "Anthony Reynolds",
      "slug": "anthony-reynolds-1987",
      "year": 1987,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "painted installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1987_Anthony-Reynolds-exhibition-catalogue-drawing_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1545
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1552",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "NQGAjecF",
      "serial": "TL53",
      "title": "California Dreamin'",
      "slug": "california-dreamin-1987",
      "year": 1987,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "painted installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1987_California-Dreamin_HR-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1565
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1552",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "7QrXPEz2",
      "serial": "TL54",
      "title": "FREEDOM OF CHOICE",
      "slug": "freedom-of-choice-1987",
      "year": 1987,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1987_Freedom-of-Choice_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 465
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1987_Freedom-of-Choice_HR-scaled.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1566
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "HXeLbwmT",
      "serial": "TL55",
      "title": "Party's Over 1",
      "slug": "partys-over-1-1987",
      "year": 1987,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "painted installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1987_Partys-Over-2-La-Jolla-Study_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1568
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1552",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "2CZfDlGE",
      "serial": "TL56",
      "title": "Party's Over 2",
      "slug": "partys-over-2-1987",
      "year": 1987,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "painted installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1987_Partys-Over-1-La-Jolla-Study_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1567
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1552",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "HgxO4ZSZ",
      "serial": "TL57",
      "title": "Walking Line an Egyptian",
      "slug": "walking-line-an-egyptian-1987",
      "year": 1987,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "painted installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Walking-Like-an-Egyptian.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1569
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1552",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "vbhGXZ2f",
      "serial": "TL58",
      "title": "For Derry Installation",
      "slug": "for-derry-installation-1989",
      "year": 1989,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "dark installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1989_For-Derry_installation_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1591
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/for-derry-6.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1590
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/for-derry-5.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1589
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/for-derry-4.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1588
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/for-derry-3.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1587
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1580",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "kYTsZap8",
      "serial": "TL59",
      "title": "Forrest of Signs",
      "slug": "forrest-of-signs-1989",
      "year": 1989,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "dark installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Forest-of-Signs-1-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1582
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Forest-of-Signs-2.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1583
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Forest-of-Signs-4.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1585
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Forest-of-Signs-3.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1584
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1580",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "xUepG0an",
      "serial": "TL60",
      "title": "Angel From Above",
      "slug": "angel-from-above-1990",
      "year": 1990,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "dark installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/angel-from-above.jpeg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1596
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1580",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "LwPQFcPF",
      "serial": "TL61",
      "title": "ARM JUGGLING LIGHT",
      "slug": "arm-juggling-light-1990",
      "year": 1990,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1990_Arm-Juggling-Light_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 467
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1990_Arm-Juggling-Light-scaled.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1554
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "SgCGSEvK",
      "serial": "TL62",
      "title": "Fallen Angel",
      "slug": "fallen-angel-1990",
      "year": 1990,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "dark installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1990_Fallen-Angel-El-Sueno-drawing-5.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1594
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1580",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "2orxN7Xq",
      "serial": "TL63",
      "title": "FallenAngel",
      "slug": "fallenangel-1990",
      "year": 1990,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "dark installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1990_Fallen-Angel-El-Sueno-drawing-2_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1593
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/fallen_angel_3.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1597
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/fallen_angel_7.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1592
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1991_Fallen-Angel_LR.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1595
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1580",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "KhptSbTc",
      "serial": "TL64",
      "title": "New Glasgow​",
      "slug": "new-glasgow-1990",
      "year": 1990,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "glasgow projects"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1990_New-Glasgow-The_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1648
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1646",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "U1nuQAwC",
      "serial": "TL65",
      "title": "LENIN FOUNTAIN",
      "slug": "lenin-fountain-1991",
      "year": 1991,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/1992_Lenin-Fountain-1-copy.jpeg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1973
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=476",
      "section": "in-the-studio",
      "period": "1991-1993",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "r_WhJTsO",
      "serial": "TL66",
      "title": "RESPONSIBILITY",
      "slug": "responsibility-1991",
      "year": 1991,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/1991_Responsibility-illuminated.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1977
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=476",
      "section": "in-the-studio",
      "period": "1991-1993",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "DBO93KUf",
      "serial": "TL67",
      "title": "TOP HAT",
      "slug": "top-hat-1991",
      "year": 1991,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/1991_Top-Hat.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1972
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=476",
      "section": "in-the-studio",
      "period": "1991-1993",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "U06s9z0S",
      "serial": "TL68",
      "title": "WRAPPED GLASGOW",
      "slug": "wrapped-glasgow-1991",
      "year": 1991,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/1992_Wrapped-Glasgow.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1974
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=476",
      "section": "in-the-studio",
      "period": "1991-1993",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "rA4QXdDa",
      "serial": "TL69",
      "title": "Vienna Academy Project",
      "slug": "vienna-academy-project-1992",
      "year": 1992,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "dark installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1992_Vienna-Academy-Project_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1601
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1580",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "CvyXaXk6",
      "serial": "TL70",
      "title": "PHRYGIAN CAP",
      "slug": "phrygian-cap-1993",
      "year": 1993,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/1993_Phrygian-cap.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1975
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=476",
      "section": "in-the-studio",
      "period": "1991-1993",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "ccq9rzID",
      "serial": "TL71",
      "title": "PROPOSAL FOR LIBERTY CAP MONUMENT",
      "slug": "proposal-for-liberty-cap-monument-1993",
      "year": 1993,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/1993_proposal-for-liberty-cap-monument.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1976
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=476",
      "section": "in-the-studio",
      "period": "1991-1993",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "YO9bmANL",
      "serial": "TL72",
      "title": "VIENNESE PAINTING 2",
      "slug": "viennese-painting-2-1994",
      "year": 1994,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Viennese Painting",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1994_Viennese-Painting-2_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 489
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "ayxcv8Hd",
      "serial": "TL73",
      "title": "VIENNESE PAINTING 4",
      "slug": "viennese-painting-4-1994",
      "year": 1994,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Viennese Painting",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1994_Viennese-Painting-4_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 490
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "U75wWdcB",
      "serial": "TL74",
      "title": "VIENNESE PAINTING 5",
      "slug": "viennese-painting-5-1994",
      "year": 1994,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Viennese Painting",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1994_Viennese-Painting-5_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 491
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "4zjSZ9uf",
      "serial": "TL75",
      "title": "VIENNESE PAINTING 10",
      "slug": "viennese-painting-10-1995",
      "year": 1995,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Viennese Painting",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1995_Viennese-painting-10_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 494
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "h_Rv9jMj",
      "serial": "TL76",
      "title": "VIENNESE PAINTING 7",
      "slug": "viennese-painting-7-1995",
      "year": 1995,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Viennese Painting",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1995_Viennese-Painting-7_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 492
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "1VFKCBPh",
      "serial": "TL77",
      "title": "VIENNESE PAINTING 9",
      "slug": "viennese-painting-9-1995",
      "year": 1995,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Viennese Painting",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1995_Viennese-Painting-9_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 493
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "HcOPvJXC",
      "serial": "TL78",
      "title": "LOS ANGELES PAINTING 1",
      "slug": "los-angeles-painting-1-1996",
      "year": 1996,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1996_Los-Angeles-Painting-1_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 495
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "MtyPic5h",
      "serial": "TL79",
      "title": "LOS ANGELES PAINTING 2",
      "slug": "los-angeles-painting-2-1996",
      "year": 1996,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1996_Los-Angeles-Painting-2_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 496
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "e8-0d_uQ",
      "serial": "TL80",
      "title": "THE ARCHITECT'S COMFORT",
      "slug": "the-architects-comfort-1996",
      "year": 1996,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1996-7_The-Architects-Comfort_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 497
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "as1QPiY9",
      "serial": "TL81",
      "title": "DEAD CALM",
      "slug": "dead-calm-1998",
      "year": 1998,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1998_Dead-Calm_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 498
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "3Ku8o1pz",
      "serial": "TL82",
      "title": "NIGHT OF THE LIVING DEAD",
      "slug": "night-of-the-living-dead-1998",
      "year": 1998,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1998_Night-of-Living-Dead_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 499
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "r-5TV3IN",
      "serial": "TL83",
      "title": "NIGHTMARE INN",
      "slug": "nightmare-inn-1998",
      "year": 1998,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1998_Nightmare-Inn_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 500
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "He79ccY3",
      "serial": "TL84",
      "title": "THE HILLS HAVE EYES",
      "slug": "the-hills-have-eyes-1999",
      "year": 1999,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1999_The-Hills-Have-Eyes_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 488
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=486",
      "section": "in-the-studio",
      "period": "1994-1998",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "b2jZIfEg",
      "serial": "TL85",
      "title": "DROWNED EARTH",
      "slug": "drowned-earth-2001",
      "year": 2001,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2001_Drowned-Earth-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 507
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "2nWa0Vbe",
      "serial": "TL86",
      "title": "HALFROND",
      "slug": "halfrond-2001",
      "year": 2001,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2001_Halfrond_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 510
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "72ifbK5K",
      "serial": "TL87",
      "title": "OVAL WORLD",
      "slug": "oval-world-2001",
      "year": 2001,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2001_Oval-World__HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 511
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "pWsraAd3",
      "serial": "TL88",
      "title": "PINK OCEAN",
      "slug": "pink-ocean-2001",
      "year": 2001,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2001_Pink-Ocean_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 512
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "QXro7NgR",
      "serial": "TL89",
      "title": "Reformer Revolutions",
      "slug": "reformer-revolutions-2001",
      "year": 2001,
      "yearEnd": null,
      "type": "mixed-media",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "the scottish project"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/2001_02_ReformerRevolutionist.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1713
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1711",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "27tz84Oe",
      "serial": "TL90",
      "title": "TWO YOUNG POETS",
      "slug": "two-young-poets-2001",
      "year": 2001,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/03/2001_03_TwoYoungPoets.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 2041
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "GuvpNJZk",
      "serial": "TL91",
      "title": "WEATHER SYSTEM",
      "slug": "weather-system-2001",
      "year": 2001,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2001_Weather-System-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 514
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "oS5UQer-",
      "serial": "TL92",
      "title": "BIG PINK",
      "slug": "big-pink-2002",
      "year": 2002,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2002_Big-Pink-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 515
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "54OQFrdL",
      "serial": "TL93",
      "title": "EARTH ON FIRE",
      "slug": "earth-on-fire-2002",
      "year": 2002,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2002_Earth-on-Fire-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 516
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "YvoDWtJS",
      "serial": "TL94",
      "title": "RED OCEANS",
      "slug": "red-oceans-2002",
      "year": 2002,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2002_Red-Oceans-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 517
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "qRYsCDC0",
      "serial": "TL95",
      "title": "DOGS OF WAR",
      "slug": "dogs-of-war-2006",
      "year": 2006,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/03/2006_Dogs-of-War_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 2039
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "eMphsMzi",
      "serial": "TL96",
      "title": "DOUBLE WORLD",
      "slug": "double-world-2006",
      "year": 2006,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2006_Double-World_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 518
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "8i-XQeFZ",
      "serial": "TL97",
      "title": "MARTYR",
      "slug": "martyr-2006",
      "year": 2006,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/03/2006_Martyr-1_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 2040
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "wTCEzVr_",
      "serial": "TL98",
      "title": "REFORMER REVOLUTIONIST",
      "slug": "reformer-revolutionist-2006",
      "year": 2006,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/03/2004_Reformer_Revolutionist_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 2036
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "4BQSEM57",
      "serial": "TL99",
      "title": "STUDY FOR JIHAD",
      "slug": "study-for-jihad-2006",
      "year": 2006,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/03/2005_Study-for-Jihad_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 2037
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "heQPIq3a",
      "serial": "TL100",
      "title": "UNTITLED BUE-BROWN ON ORANGE",
      "slug": "untitled-bue-brown-on-orange-2006",
      "year": 2006,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2006_Untitled-blue_brown-on-orange.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 519
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "3k0NOlgM",
      "serial": "TL101",
      "title": "UNTITLED ORANGE-PINK-GREEN",
      "slug": "untitled-orange-pink-green-2006",
      "year": 2006,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2006_Untitled-Orange-Pink-Green_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 520
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "d4uAkd-J",
      "serial": "TL102",
      "title": "UNTITLED RED-GREEN",
      "slug": "untitled-red-green-2006",
      "year": 2006,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2006_Untitled-Red-Green_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 506
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=504",
      "section": "in-the-studio",
      "period": "1999-2006",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "2fZZ_5iI",
      "serial": "TL103",
      "title": "AFTER D. TIEPOLO",
      "slug": "after-d-tiepolo-2007",
      "year": 2007,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2007_After-D.-Tiepolo_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 527
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "lxaDJa_x",
      "serial": "TL104",
      "title": "BOY WITH HAIR ON FIRE",
      "slug": "boy-with-hair-on-fire-2008",
      "year": 2008,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2008_Boy-with-Hair-on-FIre_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 528
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "KjbCDlPN",
      "serial": "TL105",
      "title": "GIRL WITH HAIR ON FIRE",
      "slug": "girl-with-hair-on-fire-2008",
      "year": 2008,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2008_Girl-with-Hair-on-Fire_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 529
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "f-qmYeXj",
      "serial": "TL106",
      "title": "PARASOL",
      "slug": "parasol-2008",
      "year": 2008,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2008_Parasol_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 530
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "r14UdQMj",
      "serial": "TL107",
      "title": "THE NEW WORLD DOUBT",
      "slug": "the-new-world-doubt-2008",
      "year": 2008,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2008_The-New-Word-Doubt_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 531
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "k-UwOQQX",
      "serial": "TL108",
      "title": "THE NEW WORLD REJECTION",
      "slug": "the-new-world-rejection-2008",
      "year": 2008,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2008_The-New-World-Rejection_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 533
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "UUP5qBdz",
      "serial": "TL109",
      "title": "THE NEW WORLD TWINS",
      "slug": "the-new-world-twins-2008",
      "year": 2008,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2008_The-New-World-Twins_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 532
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "l3P5cPgj",
      "serial": "TL110",
      "title": "CONFRONTATION AGGRAVATION",
      "slug": "confrontation-aggravation-2009",
      "year": 2009,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2009_Confrontation-Aggravation_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 534
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "Wl4AD3lJ",
      "serial": "TL111",
      "title": "RED MENACE",
      "slug": "red-menace-2009",
      "year": 2009,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2009_Red-Menace_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "unknown",
          "wpMediaId": 535
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "l4vXO4hY",
      "serial": "TL112",
      "title": "CONFRONTATION HEADBANGERS",
      "slug": "confrontation-headbangers-2010",
      "year": 2010,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2010_Confrontation-Headbangers_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "unknown",
          "wpMediaId": 536
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "yv9xPg4C",
      "serial": "TL113",
      "title": "DARK DAYS",
      "slug": "dark-days-2010",
      "year": 2010,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2010_Dark-Days_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 537
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "FDuuSYkp",
      "serial": "TL114",
      "title": "GREEN MASQUE",
      "slug": "green-masque-2010",
      "year": 2010,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2010_Green-Masque_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "unknown",
          "wpMediaId": 538
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "YTUOmGnH",
      "serial": "TL115",
      "title": "HARLEQUIN",
      "slug": "harlequin-2010",
      "year": 2010,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2010_Harlequin_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "unknown",
          "wpMediaId": 539
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "ZF0ZhirD",
      "serial": "TL116",
      "title": "SOMNAMBULIST",
      "slug": "somnambulist-2010",
      "year": 2010,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2010_Somnambulist-The_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "unknown",
          "wpMediaId": 540
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "VmYLugcU",
      "serial": "TL117",
      "title": "TREE",
      "slug": "tree-2010",
      "year": 2010,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2010_Tree_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "unknown",
          "wpMediaId": 526
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=524",
      "section": "in-the-studio",
      "period": "2006-2010",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "rL6bm5jX",
      "serial": "TL118",
      "title": "CONFRONTATION",
      "slug": "confrontation-2012",
      "year": 2012,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/2011_Confrontation-Double-Exposure_HR-min-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1984
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "fDNiLqkR",
      "serial": "TL119",
      "title": "CONFRONTATION HEADBANGERS",
      "slug": "confrontation-headbangers-2012",
      "year": 2012,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/2010_Confrontation-Headbangers_HR-min-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1983
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "S9VP3nda",
      "serial": "TL120",
      "title": "ENDURANCE",
      "slug": "endurance-2012",
      "year": 2012,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/2012_Endurance_HR-min.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1986
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "-iL6iZJT",
      "serial": "TL121",
      "title": "HANGED MAN",
      "slug": "hanged-man-2012",
      "year": 2012,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/2011_Hanged-Man-The_HR-min-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1985
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "KPk59NtQ",
      "serial": "TL122",
      "title": "INSTALLATION AT DAVID KORDANSKY",
      "slug": "installation-at-david-kordansky-2012",
      "year": 2012,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/Installation-at-David-Kordansky-min.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1982
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "75Eq98Yr",
      "serial": "TL123",
      "title": "INTO THE NIGHT",
      "slug": "into-the-night-2012",
      "year": 2012,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/2012_Into-the-Night_HR-min.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1987
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "cdopZiIm",
      "serial": "TL124",
      "title": "STOP DO NOT GO ON",
      "slug": "stop-do-not-go-on-2012",
      "year": 2012,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/2012_Stop-Do-Not-Go-On_HR-min.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1988
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "02qDfa6g",
      "serial": "TL125",
      "title": "THE BELL",
      "slug": "the-bell-2012",
      "year": 2012,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/2012_The-Bell_HR-min.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1989
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "RaOa40wK",
      "serial": "TL126",
      "title": "THEORETICAL PICTURE",
      "slug": "theoretical-picture-2012",
      "year": 2012,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/2012_Theoretical-Picture_HR-min.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1990
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "smH3pAB0",
      "serial": "TL127",
      "title": "VOLUPTUOUS PANIC",
      "slug": "voluptuous-panic-2012",
      "year": 2012,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/2012_Voluptuous-Panic_HR-min.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1991
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "FnZRWUDw",
      "serial": "TL128",
      "title": "WALKING ON WATER",
      "slug": "walking-on-water-2012",
      "year": 2012,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/2012_Walking-on-Water_HR-min.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1992
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "py3TYdPk",
      "serial": "TL129",
      "title": "WATCHING BEING WATCHED",
      "slug": "watching-being-watched-2013",
      "year": 2013,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/2013_WatchingBeing-Watched_HR-min.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1993
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "yuEIBvUD",
      "serial": "TL130",
      "title": "DANCE THEORY",
      "slug": "dance-theory-2014",
      "year": 2014,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/2014_Dance-Theory-copy-min.jpeg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1994
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1998",
      "section": "in-the-studio",
      "period": "2010-2015",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "jGB0aBXV",
      "serial": "TL131",
      "title": "DISASTROUS",
      "slug": "disastrous-2015",
      "year": 2015,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Dis-",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2015_Disastrous.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Photo Credit: Heather Rasmussen",
          "wpMediaId": 546
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=544",
      "section": "in-the-studio",
      "period": "2015-2016",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "WuIhKi3Q",
      "serial": "TL132",
      "title": "DISCONNECTED",
      "slug": "disconnected-2015",
      "year": 2015,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Dis-",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2015_Disconnected.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Photo Credit: Heather Rasmussen",
          "wpMediaId": 547
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=544",
      "section": "in-the-studio",
      "period": "2015-2016",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "4x31d8i5",
      "serial": "TL133",
      "title": "DISEMBODIMENT",
      "slug": "disembodiment-2015",
      "year": 2015,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Dis-",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2015_Disembodiment.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Photo Credit: Heather Rasmussen",
          "wpMediaId": 548
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=544",
      "section": "in-the-studio",
      "period": "2015-2016",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "u389_Nig",
      "serial": "TL134",
      "title": "DISILLUSIONMENT",
      "slug": "disillusionment-2015",
      "year": 2015,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Dis-",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2015_Disillusionment.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Photo Credit: Heather Rasmussen",
          "wpMediaId": 549
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=544",
      "section": "in-the-studio",
      "period": "2015-2016",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "Cvah8qDk",
      "serial": "TL135",
      "title": "DISLOCATION",
      "slug": "dislocation-2015",
      "year": 2015,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Dis-",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2015_Dislocation.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Photo Credit: Heather Rasmussen",
          "wpMediaId": 550
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=544",
      "section": "in-the-studio",
      "period": "2015-2016",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "nbJQV89N",
      "serial": "TL136",
      "title": "DISORDER",
      "slug": "disorder-2015",
      "year": 2015,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Dis-",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2015_Disorder.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Photo Credit: Heather Rasmussen",
          "wpMediaId": 551
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=544",
      "section": "in-the-studio",
      "period": "2015-2016",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "pt0b-fc8",
      "serial": "TL137",
      "title": "DISPLACEMENT",
      "slug": "displacement-2015",
      "year": 2015,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Dis-",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2015_Displacement.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Photo Credit: Heather Rasmussen",
          "wpMediaId": 552
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=544",
      "section": "in-the-studio",
      "period": "2015-2016",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "Z9tBiOMJ",
      "serial": "TL138",
      "title": "DISPLEASURE",
      "slug": "displeasure-2015",
      "year": 2015,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Dis-",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2015_Displeasure.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 553
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=544",
      "section": "in-the-studio",
      "period": "2015-2016",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "TF5-Ax4t",
      "serial": "TL139",
      "title": "RED HAND",
      "slug": "red-hand-2015",
      "year": 2015,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2015_Red-Hand.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 554
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=544",
      "section": "in-the-studio",
      "period": "2015-2016",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "4KK51499",
      "serial": "TL140",
      "title": "ROMAN HEAD",
      "slug": "roman-head-2016",
      "year": 2016,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Roman Head",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2016_Roman-Head-I.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 555
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=544",
      "section": "in-the-studio",
      "period": "2015-2016",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "gBPZSKB-",
      "serial": "TL141",
      "title": "ROMAN HEAD II",
      "slug": "roman-head-ii-2016",
      "year": 2016,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": "Roman Head",
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2016_Roman-Head-II.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 556
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=544",
      "section": "in-the-studio",
      "period": "2015-2016",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "_H6Mrk_u",
      "serial": "TL142",
      "title": "DREAMS OF THE ARROGANT PRINCE",
      "slug": "dreams-of-the-arrogant-prince-2017",
      "year": 2017,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2017_Dreams-of-the-Arrogant-Prince.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 563
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=560",
      "section": "in-the-studio",
      "period": "2017-2020",
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "efMoj2L-",
      "serial": "TL143",
      "title": "SOULLESS SISTERS",
      "slug": "soulless-sisters-2017",
      "year": 2017,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2017_Soulless-Sisters.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 564
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=560",
      "section": "in-the-studio",
      "period": "2017-2020",
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "E04pdX4G",
      "serial": "TL144",
      "title": "WHITE ON WHITE",
      "slug": "white-on-white-2018",
      "year": 2018,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2018_White-on-White.-png.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 565
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=560",
      "section": "in-the-studio",
      "period": "2017-2020",
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "9nFkbHBP",
      "serial": "TL145",
      "title": "BLOODSUCKER",
      "slug": "bloodsucker-2019",
      "year": 2019,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2019_Bloodsucker.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 566
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=560",
      "section": "in-the-studio",
      "period": "2017-2020",
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "aSOoDVir",
      "serial": "TL146",
      "title": "HEAD IN HANDS",
      "slug": "head-in-hands-2019",
      "year": 2019,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2019_Head-in-Hands.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 567
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=560",
      "section": "in-the-studio",
      "period": "2017-2020",
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "g2rWeUbn",
      "serial": "TL147",
      "title": "STORMY WEATHER",
      "slug": "stormy-weather-2019",
      "year": 2019,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2019_Stormy-Weather.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 568
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=560",
      "section": "in-the-studio",
      "period": "2017-2020",
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "71P1GRKC",
      "serial": "TL148",
      "title": "STORMY WEATHER",
      "slug": "stormy-weather-2020",
      "year": 2020,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/2020_Dreaming.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 562
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=560",
      "section": "in-the-studio",
      "period": "2017-2020",
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "6rUtztf9",
      "serial": "TL149",
      "title": "ANTHONY REYNOLDS INSTALL 1",
      "slug": "anthony-reynolds-install-1",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/anthony-reynolds-install-1.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 468
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "EBaHO-_v",
      "serial": "TL150",
      "title": "ANTHONY REYNOLDS INSTALL 2",
      "slug": "anthony-reynolds-install-2",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/anthony-reynolds-install-2.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 469
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "iXBkH-dj",
      "serial": "TL151",
      "title": "ANTHONY REYNOLDS INSTALL 4",
      "slug": "anthony-reynolds-install-4",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/anthony-reynolds-install-4.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 470
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "2fC7wKPa",
      "serial": "TL152",
      "title": "Bellgrove",
      "slug": "bellgrove",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "glasgow projects"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/bellgrove_2.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1651
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/bellgrove_1.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1650
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1646",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "Rfl8AaT1",
      "serial": "TL153",
      "title": "Bellgrove Proposal",
      "slug": "bellgrove-proposal",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "glasgow projects"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Bellgrove-Proposal_LR-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1649
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1646",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "5ZyzZrTv",
      "serial": "TL154",
      "title": "Billboard on the Studio",
      "slug": "billboard-on-the-studio",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "temporary murals"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/billboard-in-studio.jpeg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1625
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1622",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "jzvxbvmr",
      "serial": "TL155",
      "title": "BURNING DOWN THE HOUSE",
      "slug": "burning-down-the-house",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1992_Burning-Down-the-House_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 479
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=476",
      "section": "in-the-studio",
      "period": "1991-1993",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "1nNXRcsv",
      "serial": "TL156",
      "title": "Civic Virtue",
      "slug": "civic-virtue",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "early new york"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/civic_virtue_1.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1617
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/civic_virtue_3.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1618
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1608",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "XnqqIEnn",
      "serial": "TL157",
      "title": "Civic Virtue Study",
      "slug": "civic-virtue-study",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "early new york"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1987_Civic-Virtue-Study_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1616
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1608",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "JAKPI1zz",
      "serial": "TL158",
      "title": "Civic Virtues",
      "slug": "civic-virtues",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "early new york"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Civic-Virtues-install2.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1610
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/civic-virtues-2-scaled.jpeg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1613
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/civic-virtues-1-scaled.jpeg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1612
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Civic-Virtues-install-3.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1615
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Civic-Virtues-install-1.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1614
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1608",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "tMVxE53A",
      "serial": "TL159",
      "title": "Civic Virtues Poster",
      "slug": "civic-virtues-poster",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "early new york"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Civic-Virtues-poster.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1611
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1608",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "sU-Kx-78",
      "serial": "TL160",
      "title": "CORRUPTION",
      "slug": "corruption",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/Corruption.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1964
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "IsIUO9Gt",
      "serial": "TL161",
      "title": "Dancing at McDonalds",
      "slug": "dancing-at-mcdonalds",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "los angeles"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/dancing-at-mcdonalds.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1692
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1689",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "OZJrdqby",
      "serial": "TL162",
      "title": "Deidre Conchor's Men",
      "slug": "deidre-conchors-men",
      "year": null,
      "yearEnd": null,
      "type": "other",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "theatre dance fashion"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/2_Deirdre-Conochors-Men_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1670
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1660",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "NpNu8_-M",
      "serial": "TL163",
      "title": "Deidre Opening",
      "slug": "deidre-opening",
      "year": null,
      "yearEnd": null,
      "type": "other",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "theatre dance fashion"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/1_Deirdre-Opening_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1669
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1660",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "ENCtSMhd",
      "serial": "TL164",
      "title": "Deidre Poster​",
      "slug": "deidre-poster",
      "year": null,
      "yearEnd": null,
      "type": "other",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "theatre dance fashion"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Deirdre-poster-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1668
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1660",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "EHerDty0",
      "serial": "TL165",
      "title": "Deidre Set",
      "slug": "deidre-set",
      "year": null,
      "yearEnd": null,
      "type": "other",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "theatre dance fashion"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/5_deirdre-set-4.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1663
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/7_Deirdre-set-5.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1665
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/9_Deirdre-set-6.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1667
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1660",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "c7dj9AB0",
      "serial": "TL166",
      "title": "Deidre Set Chess Pieces",
      "slug": "deidre-set-chess-pieces",
      "year": null,
      "yearEnd": null,
      "type": "other",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "theatre dance fashion"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/4_Deirdre-set-chess-peices.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1662
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1660",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "RJbg29YK",
      "serial": "TL167",
      "title": "Deidre Set Heads",
      "slug": "deidre-set-heads",
      "year": null,
      "yearEnd": null,
      "type": "other",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "theatre dance fashion"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/3_Deirdre-set-heads.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1671
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/6_Image-11.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1664
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/8_unnamed.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1666
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1660",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "URSeQiaG",
      "serial": "TL168",
      "title": "DIRE FLAME",
      "slug": "dire-flame",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1992_Dire-Flame_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 480
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=476",
      "section": "in-the-studio",
      "period": "1991-1993",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "EOdm61iV",
      "serial": "TL169",
      "title": "East Rancho Service Center",
      "slug": "east-rancho-service-center",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "los angeles"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/4-EAS-RANCH-DOMINGUEZ-Service-CENTER.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1691
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1689",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "DgTSpVRc",
      "serial": "TL170",
      "title": "Easy E, Dania Birks, Dr Dre",
      "slug": "easy-e-dania-birks-dr-dre",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "los angeles"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Easy-E-Dania-Birks-Dr-Dre.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1694
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1689",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "hNkx_tBU",
      "serial": "TL171",
      "title": "ERD Across the RIver",
      "slug": "erd-across-the-river",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "los angeles"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/ERD-across-the-river-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1695
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1689",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "sNcB0CT4",
      "serial": "TL172",
      "title": "Escape Towards Liberty",
      "slug": "escape-towards-liberty",
      "year": null,
      "yearEnd": null,
      "type": "mixed-media",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "the scottish project"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/710-An-Escape-Towards-Liberty.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1714
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1711",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "rn2HOXeK",
      "serial": "TL173",
      "title": "FIERY SKY",
      "slug": "fiery-sky",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1992-Fiery-Sky_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 482
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=476",
      "section": "in-the-studio",
      "period": "1991-1993",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "DJH5gU7H",
      "serial": "TL174",
      "title": "For Derry",
      "slug": "for-derry",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "dark installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/for-derry-3.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1587
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1580",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "FQPqSRye",
      "serial": "TL175",
      "title": "FREEDOM",
      "slug": "freedom",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/Freedom.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1965
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "Kqk3GcCw",
      "serial": "TL176",
      "title": "Gala Opening Drive In",
      "slug": "gala-opening-drive-in",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "los angeles"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/gala-opening-drive-in.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1696
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1689",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "tenLoWPI",
      "serial": "TL177",
      "title": "Gallery Too",
      "slug": "gallery-too",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "dark installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Gallery-Too-install-scaled.jpeg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1586
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1580",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "wMOv7jKT",
      "serial": "TL178",
      "title": "Gathered Under the Tree of Liberty",
      "slug": "gathered-under-the-tree-of-liberty",
      "year": null,
      "yearEnd": null,
      "type": "mixed-media",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "the scottish project"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/2013_GatheredUnderTreeLiberty.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1715
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1711",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "CkxjY9Xr",
      "serial": "TL179",
      "title": "Glasgow Green &amp; Flourishing",
      "slug": "glasgow-green-amp-flourishing",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "glasgow projects"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Glasgow-Green-Flourishing_2.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1654
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1646",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "6-b1s3m4",
      "serial": "TL180",
      "title": "I-95 Billboard",
      "slug": "i-95-billboard",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "temporary murals"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/i-95-billboard-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1624
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1622",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "a2fG2vzs",
      "serial": "TL181",
      "title": "INSTALLATION KUHLENSCHMIDT 1",
      "slug": "installation-kuhlenschmidt-1",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/installation-kuhlenschmidt-05.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 471
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "iHOja6kq",
      "serial": "TL182",
      "title": "INSTALLATION KUHLENSCHMIDT 2",
      "slug": "installation-kuhlenschmidt-2",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/installation-kuhlenschmidt-08.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 472
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "MMS7lnkd",
      "serial": "TL183",
      "title": "Memory Lingers Here",
      "slug": "memory-lingers-here",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "temporary murals"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/memory_1.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1632
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/memory_2.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1633
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/memory_4.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1634
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/memory_5.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1635
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/memory_7.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1637
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/memory_6.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1636
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/memory_8.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1626
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/memory_13.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1631
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/memory_11.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1629
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1622",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "URQYdrlj",
      "serial": "TL184",
      "title": "Metro LA",
      "slug": "metro-la",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "los angeles"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/IMG_0413-scaled.jpeg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1703
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/IMG_0401-scaled.jpeg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1701
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/IMG_0307-scaled.jpeg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1706
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/DSC04057.jpeg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1705
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/IMG_0409-scaled.jpeg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1702
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/IMG_0414-scaled.jpeg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1704
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/IMG_0325.jpeg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1707
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1689",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "0DeV917U",
      "serial": "TL185",
      "title": "NUCLEAR FAMILY",
      "slug": "nuclear-family",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/Nuclear-Family.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1966
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "gCg29gCP",
      "serial": "TL186",
      "title": "On Atlantic",
      "slug": "on-atlantic",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "los angeles"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/on-Atlantic-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1698
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1689",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "fTDOSwAU",
      "serial": "TL187",
      "title": "Open Gate",
      "slug": "open-gate",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "los angeles"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/open-gate-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1699
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1689",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "r20TGyHV",
      "serial": "TL188",
      "title": "OUT OF THE DARK NIGHT",
      "slug": "out-of-the-dark-night",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1992_Out-of-the-Dark-Night_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 481
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=476",
      "section": "in-the-studio",
      "period": "1991-1993",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "b3NEyuIq",
      "serial": "TL189",
      "title": "Paranoia on the High Seas",
      "slug": "paranoia-on-the-high-seas",
      "year": null,
      "yearEnd": null,
      "type": "mixed-media",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "the scottish project"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/2006_ParanoiaonHighSeas.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1716
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1711",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "_veFviSe",
      "serial": "TL190",
      "title": "Pest of Scotland",
      "slug": "pest-of-scotland",
      "year": null,
      "yearEnd": null,
      "type": "mixed-media",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "the scottish project"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/2001_PestofScotland.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1717
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1711",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "aOc2JeNu",
      "serial": "TL191",
      "title": "Portraits of New York",
      "slug": "portraits-of-new-york",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "temporary murals"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/portrait_9-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1638
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/portrait_43.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1641
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/portrait_54.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1642
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/portrait_16.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1639
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1622",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "UXerk4eP",
      "serial": "TL192",
      "title": "Scottish Rights",
      "slug": "scottish-rights",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "painted installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Scottish-Rites-at-Anthony-Reynolds.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1555
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1552",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.854Z",
      "updatedAt": "2026-03-18T07:24:09.854Z"
    },
    {
      "id": "lT6mZ00I",
      "serial": "TL193",
      "title": "Shawhead Proposal​",
      "slug": "shawhead-proposal",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "glasgow projects"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Shawhead-Proposal.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1656
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1646",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "b9WavlpK",
      "serial": "TL194",
      "title": "Sleeper Installation",
      "slug": "sleeper-installation",
      "year": null,
      "yearEnd": null,
      "type": "mixed-media",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "the scottish project"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/2001_Sleeper_installation_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1718
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1711",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "5gyARIYX",
      "serial": "TL195",
      "title": "STUDY FOR WILD HORSES",
      "slug": "study-for-wild-horses",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/Study-for-wild-Horses.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1967
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "tsmC2l4U",
      "serial": "TL196",
      "title": "SUBURBAN DREAMS",
      "slug": "suburban-dreams",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/1993_Suburban-Dreams-3_HR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 478
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=476",
      "section": "in-the-studio",
      "period": "1991-1993",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "85-6b6uD",
      "serial": "TL197",
      "title": "Suburban Installation",
      "slug": "suburban-installation",
      "year": null,
      "yearEnd": null,
      "type": "mixed-media",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "the scottish project"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Suburban-install3.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1719
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Suburban-install2.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1720
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/14_LawsonT.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1721
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1711",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "_oysQBvj",
      "serial": "TL198",
      "title": "Tiefe Nacht",
      "slug": "tiefe-nacht",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "dark installations"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Tiefe-Nacht-sign-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1599
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Tiefe-Nacht-2.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1598
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Tiefe-Nacht-1-scaled.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1600
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1580",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "ycWnDDgF",
      "serial": "TL199",
      "title": "Till You Drop",
      "slug": "till-you-drop",
      "year": null,
      "yearEnd": null,
      "type": "other",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "theatre dance fashion"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/10_Till-you-Drop-Sept-2014-copy.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1685
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/09_MVI_03512ndPerf_19twkd.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1684
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/08_03512ndPerf_25yatwkd.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1683
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/07_2ndPerfFirstHalfMVI_2062slip04twkd.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1681
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/06_2ndPerfFirstHalfMVI_2062niceJosJumpstwkd.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1680
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/02_2ndPerf2ndHalf_3niceJosNFloraDuoAliOnFloortwkd.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1678
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1660",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "gyrY7X8G",
      "serial": "TL200",
      "title": "Town Hall Ad",
      "slug": "town-hall-ad",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "los angeles"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/town-hall-ad.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1700
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1689",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "kUnQJn7L",
      "serial": "TL201",
      "title": "Untitled",
      "slug": "untitled",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2026/03/1979_Joy-of-Sex-The_LR.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 2143
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2026/03/1979_Joy-of-Sex-III_1.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 2144
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/03/069-Tlawson_Spirit-of-the-Museum1987.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": "Fredrik Nilsen",
          "wpMediaId": 2051
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2022/09/lawson1_1990.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 459
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=347",
      "section": "in-the-studio",
      "period": "1977-1979",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "WSGaS-KQ",
      "serial": "TL202",
      "title": "VICE",
      "slug": "vice",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/Vice.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1968
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "dZe6cM77",
      "serial": "TL203",
      "title": "VIRTUE",
      "slug": "virtue",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/Virtue.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1962
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    },
    {
      "id": "thOqHOwN",
      "serial": "TL204",
      "title": "Vogue",
      "slug": "vogue",
      "year": null,
      "yearEnd": null,
      "type": "other",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "theatre dance fashion"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Vogue-1-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1673
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Vogue-Sept2013-2.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1676
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/2013_Hedi-Slimane-double-portrait.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1672
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Vogue-2-scaled.jpg",
          "caption": null,
          "primary": false,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1674
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1660",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "9pd75V6p",
      "serial": "TL205",
      "title": "Walking Like an Egyptian",
      "slug": "walking-like-an-egyptian",
      "year": null,
      "yearEnd": null,
      "type": "installation",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [
        "los angeles"
      ],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/donuts-scaled.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1693
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1689",
      "section": "beyond-the-studio",
      "period": null,
      "createdAt": "2026-03-18T07:24:09.855Z",
      "updatedAt": "2026-03-18T07:24:09.855Z"
    },
    {
      "id": "Ynd1Bs8w",
      "serial": "TL206",
      "title": "WASHINGTON ILLUMINATED",
      "slug": "washington-illuminated",
      "year": null,
      "yearEnd": null,
      "type": "painting",
      "medium": null,
      "artMedium": null,
      "artworkSurface": null,
      "dimensions": null,
      "artEdition": null,
      "series": null,
      "tags": [],
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/02/Washington-illuminated.jpg",
          "caption": null,
          "primary": true,
          "isDetail": false,
          "detailLabel": null,
          "credit": null,
          "wpMediaId": 1963
        }
      ],
      "description": null,
      "collection": null,
      "provenance": [],
      "exhibitions": [],
      "writingIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=457",
      "section": "in-the-studio",
      "period": "1987-1990",
      "createdAt": "2026-03-18T07:24:09.853Z",
      "updatedAt": "2026-03-18T07:24:09.853Z"
    }
  ],
  "exhibitions": [
    {
      "id": "ctTrE78U",
      "title": "Pat Douthwaite",
      "slug": "pat-douthwaite",
      "year": 1973,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/09/Douthewaite-Woman-One-121x121cm1968.png",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/08/Douthewaite-Woman-in-Red-Gown-120x90cm-1966.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/09/Douthewaite.Woman-with-snake-1.25x1M-1970.webp",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/09/Cordelia-Oliver-The-Guardian-Wed-14-Feb-1973.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/09/John-Steir-Aien-Feb-14-1973.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/09/Pat-Douthewaite-brochure_Page_1-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/09/St-Andrews-Festival-73_Page_1-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/09/TL-Preview-Aien-Feb-1973.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/09/William-Ferguson-The-Times-Educational-Supplement-Feb-16-1973-scaled.jpg",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1205",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "xE5CrWEx",
      "title": "REALLIFE Magazine Presents",
      "slug": "reallife-magazine-presents",
      "year": 1981,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Greenwood-Robinson.jpg",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Greenwood-Lawsonandbosman.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Greenwood-Jenney.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/REALLIFE-7-Too-Good-To-Be-True_Page_1.jpg",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1421",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "MxHQ9TyR",
      "title": "Critical Perspectives",
      "slug": "critical-perspectives",
      "year": 1982,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Jack-Goldstein-Untitled.png",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/SAlome-Blue-Boys-1981-240x200-cm.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Gerry-Morehead-Beanstalk-1984.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Nachume-Miller-Reagan-Assassination-piece.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Jack-Goldstein-Untitled.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Fischl-Father-and-Son-Sleeping.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Critical-Perspectives-cover.jpg",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1233",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "4F_AC1Zm",
      "title": "REALLIFE | Whitecolumns",
      "slug": "reallife-whitecolumns",
      "year": 1982,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Ken-Lum-Sculpture-for-LIving-Room-1980.png",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/John-Miller-Untitled-ink-on-paper-1981.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/David-Robbinsevery-man-an-elvis.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Jennifer-Bolande-White-House-Drawings-c-print-of-colored-pencil-drawing-1982.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Michael-Ross-Untitled-1982-scaled.jpeg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Reallife-Presents-82.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Mark-Innerst-Untitled-Two-Ships-oil-on-board-1984.jpg",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1252",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "AUB2F8k6",
      "title": "Livin' in the USA",
      "slug": "livin-in-the-usa",
      "year": 1984,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Paul-McMahon-Diiving-Catch-Baseball-pastel-on-newspaper-1974.webp",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Richards-Jarden-TV-Fragment-Early-Morning-Coffee-laminated-wax-relief-1983.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Livin-in-the-USA-cover.jpg",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1280",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "LziEXtT5",
      "title": "Nostalgia as Resistance",
      "slug": "nostalgia-as-resistance",
      "year": 1988,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/David-Robbins-Talent-1986.jpg",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Jenny-Holzer-LED-light-installation-Times-Square-New-York-1982.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Jessica-DIAMOND_5-Dicks_-Clocktower-1988-1-1.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Matt-Mullican-cosmological-painting-1988.jpeg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Barbara-Kruger-Untitled-Your-gaze-hits-the-side-of-my-face-1981.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Richard-Prince-Untititled-Monochromatic-Joke-acrylic-silkscreen-on-canvas-1987.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Sherrie-Levine-After-Rodchenko-4-1987.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Cindy-Sherman-Untitled-152-1985.-jpeg.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Nostalgia-as-Resistance-1988_Page_01-rotated.jpg",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1292",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "sOuZnPWc",
      "title": "Familie Beck",
      "slug": "familie-beck",
      "year": 1990,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/FAmilie-Beck-cover.jpg",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/FAmilie-Beck-cover.jpg",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1435",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "YeAu9Cvk",
      "title": "The British Art Show",
      "slug": "the-british-art-show",
      "year": 1994,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Anya-Gallaccio-Stroke-Chocolate-and-Cardboard-1994.png",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Gary-Hume-Tony-Blackburn-Alkyd-on-board1994.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Perminder-Kaur-Cot-Fabric-steel-and-foam-1994.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Jordan-Baseman-Words-WIll-Never-Hurt-Me-1995.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Chris-Offili-at-British-Art-Show.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Lucia-Nogueira-Hide-and-Seek-Refrigerator-packing-material-framed-photograph-1997.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Mark-Wallinger-Half-Brother-oil-on-canvas-1994_95.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Jan-and-Louise-Wilson-Hypnotic-Suggestion-505-video-installation-1993.jpeg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Gillian-Wearing-Signs-c-type-prints-on-aluminium-1993.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Douglas-Gordon-10ms-1-video-back-projection-1994.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Damien-HirstAway-from-the-Flock-Steel-glass-lamb-formaldehyde-solution1994.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Steve-McQueen-Bear-16mm-film-projection-1994.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/British-Art-show-install.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/11/Jane-and-Louise-Wilson.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Ceal-Floyer-Light-Switch-35mm-slide-and-slide-projector-1994.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Artforum-review-of-British-Art-Show-4.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Unrelenting-Jet-Lag_Page_1.jpg",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1312",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "5_c_cCbA",
      "title": "Hot Coffee",
      "slug": "hot-coffee",
      "year": 1997,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/HotCoffe.png",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/97-hotcoffee-exhibitionimage-2560x.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/97-hotcoffee-newsletterimage-owens-2560x.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/97_hotcoffee_projectdescription_Page_1-min-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/97_hotcoffee_columbiaspectator-min-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/97_hotcoffee_checklist_Page_1-min-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/97_hotcoffee_timeoutny-min-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/97_hotcoffee_villagevoice-min-scaled.jpg",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1378",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "y207EXDP",
      "title": "Shimmer",
      "slug": "shimmer",
      "year": 1997,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Steven-Hull-color-corrected.jpg",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/WayTheFuckOutWest.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/VeryDeadEmperors.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/2.-Slide-Projector-detail--scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Carousel-study.jpg",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1458",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "hpJecZfB",
      "title": "Art School",
      "slug": "art-school",
      "year": 1997,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/01/Thomas-Lawson_Essay_Galley-2_Page_01-scaled.jpg",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/01/712-An-Artists-Education_Page_01.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/01/3102-CORE_Page_01-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/01/Thomas-Lawson_Essay_Galley-2_Page_01-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/01/ArtWeek-interview-about-Art-School-Nov-1991_Page_1.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2024/01/3101-Akademie-X_Page_01.jpg",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1878",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "CBcRBYat",
      "title": "The Experimental Impulse",
      "slug": "the-experimental-impulse",
      "year": 2011,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/CIA_20111119_6002-scaled.jpg",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/CIA_20111119_6640-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/CIA_20111119_6414-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/CIA_20111119_6006-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/CIA_20111119_6045-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/CIA_20111119_6263-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/CIA_20111119_6045-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/CIA_20111119_6244-scaled.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Artslant-12_19_12_Page_1.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Daily-Serving-12_16_12_Page_1.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/LA-Times-12_23_11_Page_1.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/TheExperimentalImpulse_CMagazine_Spring2012_Page_1.jpg",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/TheExperimentalImpulse_May_February2012_Page_1.jpg",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1343",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "WFHo9IUK",
      "title": "Dissent",
      "slug": "dissent",
      "year": 2016,
      "yearEnd": null,
      "venue": null,
      "city": null,
      "role": "curator",
      "description": null,
      "images": [
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Install-shot.png",
          "caption": null,
          "primary": true
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Barbara-Ess_.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Sondra-Perry-install.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Sondra-Perry-Install-2.png",
          "caption": null,
          "primary": false
        },
        {
          "url": "https://www.thomaslawson.com/wp-content/uploads/2023/10/Sondra-Perry-Install-2.png",
          "caption": null,
          "primary": false
        }
      ],
      "artworkIds": [],
      "sourceUrl": "https://www.thomaslawson.com/?p=1445",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    }
  ],
  "writings": [
    {
      "id": "uF4JDLrt",
      "title": "Looking for Something to Read",
      "slug": "looking-for-something-to-read",
      "year": 2000,
      "type": "essay",
      "author": null,
      "publication": "AFTERALL 6",
      "issue": "2000",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "afterall",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "tNDX-rPl",
      "title": "Foreword to Issue 8",
      "slug": "foreword-to-issue-8",
      "year": 2003,
      "type": "essay",
      "author": null,
      "publication": "AFTERALL 7",
      "issue": "2003",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "afterall",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "nnWH4HzS",
      "title": "Desperate Dreams",
      "slug": "desperate-dreams",
      "year": 2004,
      "type": "essay",
      "author": null,
      "publication": "AFTERALL 9",
      "issue": "2004",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "afterall",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "ykQnDvoP",
      "title": "Not a Condition but a Process: The Wooster Group",
      "slug": "not-a-condition-but-a-process-the-wooster-group",
      "year": 2005,
      "type": "essay",
      "author": null,
      "publication": "AFTERALL 11",
      "issue": "2005",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "afterall",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "jZ_J8G6g",
      "title": "Foreword to Issue 12.",
      "slug": "foreword-to-issue-12",
      "year": 2005,
      "type": "essay",
      "author": null,
      "publication": "AFTERALL 12",
      "issue": "2005",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "afterall",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "nk9CvIgc",
      "title": "Waiting, Thinking, Drinking: A Conversation about Patrick Caulfield’s Interiors,\nWritten with Katherine Lewis",
      "slug": "waiting-thinking-drinking-a-conversation-about-patrick-caulfield-s-interiors-written-with-katherine-lewis",
      "year": 2005,
      "type": "essay",
      "author": null,
      "publication": "AFTERALL 12",
      "issue": "2005",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "afterall",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "7eCvRHL9",
      "title": "Afterall 13",
      "slug": "afterall-13",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "Afterall",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "afterall",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "cep-UUed",
      "title": "LIVIO SAGANIC and LEO RABKIN; MATT MULLICAN",
      "slug": "livio-saganic-and-leo-rabkin-matt-mullican",
      "year": 1980,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "November 1980",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "477XBBDK",
      "title": "Fashion Moda et al",
      "slug": "fashion-moda-et-al",
      "year": 1981,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "March 1981",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "XNz0XkKD",
      "title": "People's Choice, Serra, King",
      "slug": "peoples-choice-serra-king",
      "year": 1981,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "April 1981",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "kVuWhgOk",
      "title": "Bush, Porter, Haacke, Salle, Fischl, Winters",
      "slug": "bush-porter-haacke-salle-fischl-winters",
      "year": 1981,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "May 1981",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "ZlDlwrpk",
      "title": "September",
      "slug": "september",
      "year": 1981,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "September 1981",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "7IiEf00A",
      "title": "Last Exit- Painting",
      "slug": "last-exit-painting",
      "year": 1981,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "October 1981",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "DimtBsJZ",
      "title": "Bill Woodrow Marc Chaimowicz",
      "slug": "bill-woodrow-marc-chaimowicz",
      "year": 1981,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "December 1981",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "d99aPNtH",
      "title": "Photos, Bess, Liberman",
      "slug": "photos-bess-liberman",
      "year": 1982,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "January 1982",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "v5HnIYWo",
      "title": "Hurson, Clark",
      "slug": "hurson-clark",
      "year": 1982,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "May 1982",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "zuDcncvJ",
      "title": "Smith Koberling",
      "slug": "smith-koberling",
      "year": 1982,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "Summer 1982",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "o5MinVd5",
      "title": "Katz, McMahon, Polke",
      "slug": "katz-mcmahon-polke",
      "year": 1982,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "October 1982",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "Qk3QERVv",
      "title": "The Dark Side of the Bright Light",
      "slug": "the-dark-side-of-the-bright-light",
      "year": 1982,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "November 1982",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "rxrEUMZy",
      "title": "Susan Caldwell Gallery",
      "slug": "susan-caldwell-gallery",
      "year": 1983,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "February 1983",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "OhJJmxiT",
      "title": "The Beast",
      "slug": "the-beast",
      "year": 1983,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "March 1983",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "udusuxba",
      "title": "Middendorf, Bosman, Clemente",
      "slug": "middendorf-bosman-clemente",
      "year": 1983,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "Summer 1983",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "nLWKSrmN",
      "title": "McCollum, Chunn, R.C.Morgan",
      "slug": "mccollum-chunn-r-c-morgan",
      "year": 1983,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "September 1983",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "zdexjnbi",
      "title": "New German Painting and Kiely Jenkins",
      "slug": "new-german-painting-and-kiely-jenkins",
      "year": 1984,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "January 1984",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "PfH_TtvQ",
      "title": "Group Material James Brown May",
      "slug": "group-material-james-brown-may",
      "year": 1984,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "May 1984",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "Y8ANVlZ1",
      "title": "TL review of How New York Stole the Idea of Modern Art",
      "slug": "tl-review-of-how-new-york-stole-the-idea-of-modern-art",
      "year": 1984,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "Summer 1984",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "2YvlCcMT",
      "title": "Generation in vitro",
      "slug": "generation-in-vitro",
      "year": 1984,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "September 1984",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "o11WUg90",
      "title": "Hilton Kramer",
      "slug": "hilton-kramer",
      "year": 1984,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "November 1984",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "xehaVsff",
      "title": "Snake Pit",
      "slug": "snake-pit",
      "year": 1986,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "March 1986",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "7BS_7j9u",
      "title": "Time Bandits",
      "slug": "time-bandits",
      "year": 1988,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "January 1988",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "EwO-edFA",
      "title": "Infotainment review",
      "slug": "infotainment-review",
      "year": 2004,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "October 2004",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "SFstI-Yn",
      "title": "Gretchen Bender",
      "slug": "gretchen-bender",
      "year": 2005,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "April 2005",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "gsBj_qWF",
      "title": "Jean Prouve in New Haven",
      "slug": "jean-prouve-in-new-haven",
      "year": 2006,
      "type": "essay",
      "author": null,
      "publication": "Arforum",
      "issue": "January 2006",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "gNWCvoid",
      "title": "Los Angeles at Pompidou",
      "slug": "los-angeles-at-pompidou",
      "year": 2005,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "April 2005",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "SRrhGsBQ",
      "title": "Lucy McKenzie",
      "slug": "lucy-mckenzie",
      "year": 2006,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "November 2006",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "YAkfg74J",
      "title": "Thomas Lawson Top Ten",
      "slug": "thomas-lawson-top-ten",
      "year": 2006,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "December 2006",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "tf-z7bzE",
      "title": "Michael Hurson",
      "slug": "michael-hurson",
      "year": 2007,
      "type": "essay",
      "author": null,
      "publication": "Arforum",
      "issue": "April 2007",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "xgOxHg3T",
      "title": "Jack Goldstein",
      "slug": "jack-goldstein",
      "year": 2012,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "December 2012",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "NnrU_5Dc",
      "title": "Made in LA",
      "slug": "made-in-la",
      "year": 2014,
      "type": "essay",
      "author": null,
      "publication": "Artforum",
      "issue": "October 2014",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "artforum",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "yfjogpwB",
      "title": "The Journey West",
      "slug": "the-journey-west",
      "year": 2010,
      "type": "essay",
      "author": null,
      "publication": "October 10",
      "issue": "2010",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "t-Jvrn_N",
      "title": "Interview with Andrea Bowers",
      "slug": "interview-with-andrea-bowers",
      "year": 2011,
      "type": "essay",
      "author": null,
      "publication": "January 20",
      "issue": "2011",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "qcmHbEZt",
      "title": "Institutional Whitewash",
      "slug": "institutional-whitewash",
      "year": 2011,
      "type": "essay",
      "author": null,
      "publication": "February 11",
      "issue": "2011",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "ZXnMdV0Z",
      "title": "Interview with Liz Glynn",
      "slug": "interview-with-liz-glynn",
      "year": 2012,
      "type": "essay",
      "author": null,
      "publication": "September 6",
      "issue": "2012",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "bf9X3c-L",
      "title": "Michael Asher, 1943-",
      "slug": "michael-asher-1943",
      "year": 2012,
      "type": "essay",
      "author": null,
      "publication": "October 15",
      "issue": "2012",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "bemu07i4",
      "title": "4 Taxis",
      "slug": "4-taxis",
      "year": 2013,
      "type": "essay",
      "author": null,
      "publication": "February 5",
      "issue": "2013",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "j7_qOO4M",
      "title": "Rhapsody in Pink: Steven Prina Paints",
      "slug": "rhapsody-in-pink-steven-prina-paints",
      "year": 2013,
      "type": "essay",
      "author": null,
      "publication": "April 11",
      "issue": "2013",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "NTfGb6Rt",
      "title": "Allan Sekula, 1951-",
      "slug": "allan-sekula-1951",
      "year": 2013,
      "type": "essay",
      "author": null,
      "publication": "August 11",
      "issue": "2013",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "A9S0kIRR",
      "title": "Interview with Fiona Connor",
      "slug": "interview-with-fiona-connor",
      "year": 2017,
      "type": "essay",
      "author": null,
      "publication": "July 18",
      "issue": "2017",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "qU6ge1Vc",
      "title": "A Schoolboy visits Man Ray",
      "slug": "a-schoolboy-visits-man-ray",
      "year": 2017,
      "type": "essay",
      "author": null,
      "publication": "August 29",
      "issue": "2017",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "wd9ijwLG",
      "title": "Walter Hopps discovers Joseph Cornell",
      "slug": "walter-hopps-discovers-joseph-cornell",
      "year": 2017,
      "type": "essay",
      "author": null,
      "publication": "August 29",
      "issue": "2017",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "yyUabJkt",
      "title": "Marcel Duchamp cover for Beatrice Wood catalogue",
      "slug": "marcel-duchamp-cover-for-beatrice-wood-catalogue",
      "year": 2017,
      "type": "essay",
      "author": null,
      "publication": "August 29",
      "issue": "2017",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "dnjvn8mI",
      "title": "Walter Hopps visits the Arensberg Collection",
      "slug": "walter-hopps-visits-the-arensberg-collection",
      "year": 2017,
      "type": "essay",
      "author": null,
      "publication": "October 1",
      "issue": "2017",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "xZs0Jt7X",
      "title": "Cesar Pelli, 1926-",
      "slug": "cesar-pelli-1926",
      "year": 2019,
      "type": "essay",
      "author": null,
      "publication": "July 21",
      "issue": "2019",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "405sbAiK",
      "title": "Remembering John Baldessari",
      "slug": "remembering-john-baldessari",
      "year": 2020,
      "type": "essay",
      "author": null,
      "publication": "March 2",
      "issue": "2020",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "JrmP8w8w",
      "title": "Betye Saar, The Liberation of Aunt Jemima,",
      "slug": "betye-saar-the-liberation-of-aunt-jemima",
      "year": 2020,
      "type": "essay",
      "author": null,
      "publication": "June 19",
      "issue": "2020",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "gpc5qHvI",
      "title": "Millard Sheets, Albert Stewart: Monument to Freemason, Albert Pike, Scottish Rite Temple,",
      "slug": "millard-sheets-albert-stewart-monument-to-freemason-albert-pike-scottish-rite-temple",
      "year": 2020,
      "type": "essay",
      "author": null,
      "publication": "August 1",
      "issue": "2020",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "ela6eOFV",
      "title": "A CalArts Story",
      "slug": "a-calarts-story",
      "year": 2021,
      "type": "essay",
      "author": null,
      "publication": "September 7",
      "issue": "2021",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "byCeH5zy",
      "title": "Luciano Perna, 1958-",
      "slug": "luciano-perna-1958",
      "year": 2022,
      "type": "essay",
      "author": null,
      "publication": "January 21",
      "issue": "2022",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "smKzRj8a",
      "title": "Michael Asher: There is Never Enough Time to get Everything Said",
      "slug": "michael-asher-there-is-never-enough-time-to-get-everything-said",
      "year": 2022,
      "type": "essay",
      "author": null,
      "publication": "May 24",
      "issue": "2022",
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "east-of-borneo",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "UEPpCIcD",
      "title": "REALLIFE Review of Anthology",
      "slug": "reallife-review-of-anthology",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "GNWlHKtD",
      "title": "REALLIFE Show",
      "slug": "reallife-show",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "xD-oNwKX",
      "title": "Pfiel Magazine",
      "slug": "pfiel-magazine",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "Z6Ll_LSw",
      "title": "REALLIFE 1",
      "slug": "reallife-1",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "RxL2AEpZ",
      "title": "REALLIFE 2",
      "slug": "reallife-2",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "Yx4ObnrA",
      "title": "REALLIFE 3",
      "slug": "reallife-3",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "8lFvEWnx",
      "title": "REALLIFE 4",
      "slug": "reallife-4",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "8eFNAwI5",
      "title": "REALLIFE 5",
      "slug": "reallife-5",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "0i5y-R-n",
      "title": "REALLIFE 6",
      "slug": "reallife-6",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "jhleUeTS",
      "title": "REALLIFE 7",
      "slug": "reallife-7",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "gHUGe-84",
      "title": "REALLIFE 8",
      "slug": "reallife-8",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "kinOa5bw",
      "title": "REALLIFE 9",
      "slug": "reallife-9",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "oozLRpMK",
      "title": "REALLIFE 10",
      "slug": "reallife-10",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "a9-wly36",
      "title": "REALLIFE 11-12",
      "slug": "reallife-11-12",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "KqSa27NV",
      "title": "REALLIFE 13",
      "slug": "reallife-13",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "queNk0Yc",
      "title": "REALLIFE 14",
      "slug": "reallife-14",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "EryaI9Ld",
      "title": "REALLIFE 15",
      "slug": "reallife-15",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "Obu80DxN",
      "title": "REALLIFE 16",
      "slug": "reallife-16",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "58Ugh_fG",
      "title": "REALLIFE 17-18",
      "slug": "reallife-17-18",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "SV4BbEVb",
      "title": "REALLIFE 19",
      "slug": "reallife-19",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "5PK7Pg1r",
      "title": "REALLIFE 20",
      "slug": "reallife-20",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "4ngA1GAF",
      "title": "REALLIFE 21-22",
      "slug": "reallife-21-22",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "G7lf7Pww",
      "title": "REALLIFE 23",
      "slug": "reallife-23",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "76sWWLpB",
      "title": "REALLIFE Anthology",
      "slug": "reallife-anthology",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": "REALLIFE",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "reallife",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "b-0w6VyT",
      "title": "OTHER",
      "slug": "other",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "Dn0wl5Wt",
      "title": "Art After Modernism: Rethinking Representation",
      "slug": "art-after-modernism-rethinking-representation",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "Itzss7kb",
      "title": "Brian Wallis ed., New Museum of Contemporary Art, NY, 1984, reprinted",
      "slug": "brian-wallis-ed-new-museum-of-contemporary-art-ny-1984-reprinted",
      "year": 1984,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "YR0aM6Po",
      "title": "Theories of Contemporary Art",
      "slug": "theories-of-contemporary-art",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "VlPjY_g_",
      "title": "Richard Hertz, ed., Prentice-Hall, NJ,",
      "slug": "richard-hertz-ed-prentice-hall-nj",
      "year": 1985,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "rcDl28XF",
      "title": "Infotainment",
      "slug": "infotainment",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "y_J2ezjN",
      "title": "Peter Nagy and David Robbins, eds., Reichard/Livet and J. Berg Press, NY,",
      "slug": "peter-nagy-and-david-robbins-eds-reichard-livet-and-j-berg-press-ny",
      "year": 1985,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "PCKy4Wmp",
      "title": "Individuals: A Selected History of Contemporary Art 1945-",
      "slug": "individuals-a-selected-history-of-contemporary-art-1945",
      "year": 1986,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "RiZjkljJ",
      "title": "Howard Singerman, ed., MoCA Los Angeles and Abbeville Press, NY,",
      "slug": "howard-singerman-ed-moca-los-angeles-and-abbeville-press-ny",
      "year": 1986,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "H8G7XYZI",
      "title": "Modern Dreams: The Rise and Fall of Pop",
      "slug": "modern-dreams-the-rise-and-fall-of-pop",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "vhmZZ7w_",
      "title": "Ed Leffingwell and Karen Marta, eds., MIT Press, Cambridge, MA",
      "slug": "ed-leffingwell-and-karen-marta-eds-mit-press-cambridge-ma",
      "year": 1988,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "vc1JN8V0",
      "title": "Blasted Allegories: An Anthology of Writing by Contemporary Artists",
      "slug": "blasted-allegories-an-anthology-of-writing-by-contemporary-artists",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "PErkzRM0",
      "title": "Brian Wallis, ed., New Museum of Contemporary Art, NY and MIT Press, Cambridge M,",
      "slug": "brian-wallis-ed-new-museum-of-contemporary-art-ny-and-mit-press-cambridge-m",
      "year": 1989,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "C3ypzvDo",
      "title": "Flash Art: Two Decades of History",
      "slug": "flash-art-two-decades-of-history",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "yUdeh-fW",
      "title": "Giancarlo Politi and Helena Kontova, eds., MIT Press, Cambridge, MA",
      "slug": "giancarlo-politi-and-helena-kontova-eds-mit-press-cambridge-ma",
      "year": 1990,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "xk0l2Ok_",
      "title": "Postmodern Perspectives: Issues in Contemporary Art",
      "slug": "postmodern-perspectives-issues-in-contemporary-art",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "as0OZCjk",
      "title": "Howard Risatti ed., Prentice-Hall., NJ, 1990,",
      "slug": "howard-risatti-ed-prentice-hall-nj-1990",
      "year": 1990,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "_UaVoFnb",
      "title": "Between Artists: Twelve Artists Interview Twelve Artists,",
      "slug": "between-artists-twelve-artists-interview-twelve-artists",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.863Z",
      "updatedAt": "2026-03-18T07:24:09.863Z"
    },
    {
      "id": "BCxGA5Y3",
      "title": "Barnes, Barossh, Bartman, Sappington, eds., A.R.T. Press, Los Angeles,",
      "slug": "barnes-barossh-bartman-sappington-eds-a-r-t-press-los-angeles",
      "year": 1996,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "AIxZTpFI",
      "title": "Thomas Lawson: Mining for Gold, Selected Writings 1979-",
      "slug": "thomas-lawson-mining-for-gold-selected-writings-1979",
      "year": 1979,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "DpKNeA22",
      "title": "Lionel Bovier and Fabrice Stroun, eds., JRP/Ringier, Zurich,",
      "slug": "lionel-bovier-and-fabrice-stroun-eds-jrp-ringier-zurich",
      "year": 2004,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "C931BS3J",
      "title": "REALLIFE Magazine; Selected Writings and Projects 1979-",
      "slug": "reallife-magazine-selected-writings-and-projects-1979",
      "year": 1979,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "H6Q4rvcw",
      "title": "Miriam Katzeff, Thomas Lawson, Susan Morgan, eds., Primary Information, NY,",
      "slug": "miriam-katzeff-thomas-lawson-susan-morgan-eds-primary-information-ny",
      "year": 2006,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "KNhVhtea",
      "title": "12\tThe Beat and the Buzz: Inside the L.A. Art World",
      "slug": "12-the-beat-and-the-buzz-inside-the-l-a-art-world",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "GrxJ0Sqg",
      "title": "Richard Hertz, ed., Mineola Press, CA",
      "slug": "richard-hertz-ed-mineola-press-ca",
      "year": 2009,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "L-LRFvef",
      "title": "Excursus I-IV",
      "slug": "excursus-i-iv",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "oetH3jFl",
      "title": "Alex Klein, ed., ICA, University of Pennsylvania,",
      "slug": "alex-klein-ed-ica-university-of-pennsylvania",
      "year": 2014,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "hopP72yi",
      "title": "Akademie X Lessons in Art and Life",
      "slug": "akademie-x-lessons-in-art-and-life",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "_lAqcwne",
      "title": "Craig Garret, et al, eds., Phaidon, London and NY,",
      "slug": "craig-garret-et-al-eds-phaidon-london-and-ny",
      "year": 2015,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "8dbNt-HY",
      "title": "Remote/Control: astral Projection in Higher Ed",
      "slug": "remote-control-astral-projection-in-higher-ed",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "jaKtm6cI",
      "title": "Emma kemp and Adriana Widdoes, eds., EastofBorneo Books, LA",
      "slug": "emma-kemp-and-adriana-widdoes-eds-eastofborneo-books-la",
      "year": 2020,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "gKY7BXqt",
      "title": "Artists at Work",
      "slug": "artists-at-work",
      "year": null,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "1zJqZ6Kw",
      "title": "Adriana Widdoes, ed., EastofBorneo Books, LA,",
      "slug": "adriana-widdoes-ed-eastofborneo-books-la",
      "year": 2023,
      "type": "anthology",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "anthologies",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "w4yzJFsw",
      "title": "Video Data Bank Profile",
      "slug": "video-data-bank-profile",
      "year": null,
      "type": "interview",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "interviews",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "aJx4gjV4",
      "title": "Robbins Interview",
      "slug": "robbins-interview",
      "year": null,
      "type": "interview",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "interviews",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "o7k7H4oP",
      "title": "Robert Rooney Pool Side",
      "slug": "robert-rooney-pool-side",
      "year": null,
      "type": "interview",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "interviews",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "IAQciG7Y",
      "title": "CAE Art Papers",
      "slug": "cae-art-papers",
      "year": null,
      "type": "interview",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "interviews",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "3dvcLVSW",
      "title": "Christopher Howard",
      "slug": "christopher-howard",
      "year": null,
      "type": "interview",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "interviews",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "XFik3yCJ",
      "title": "McEvilly Sischy",
      "slug": "mcevilly-sischy",
      "year": null,
      "type": "interview",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "interviews",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "SCaG7aM9",
      "title": "Considering Other Artists",
      "slug": "considering-other-artists",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "SQVDRZP7",
      "title": "Spike Art Magazine Issue 71 (Spring",
      "slug": "spike-art-magazine-issue-71-spring",
      "year": 2022,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "bJr5hy0R",
      "title": "Empire",
      "slug": "empire",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "Dr1Z2kfK",
      "title": "Laura Owens",
      "slug": "laura-owens",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "oAP6w1un",
      "title": "Richard Wright",
      "slug": "richard-wright",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "7P7Mxouc",
      "title": "Paint",
      "slug": "paint",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "dTIG2e40",
      "title": "Gary Hume",
      "slug": "gary-hume",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "ZSlHenPD",
      "title": "Some Thoughts on Painting",
      "slug": "some-thoughts-on-painting",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "SIimj_J6",
      "title": "Stills",
      "slug": "stills",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "LGBXI373",
      "title": "Gerard Hemsworth",
      "slug": "gerard-hemsworth",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "8M1bYUlZ",
      "title": "LA at Pompidou",
      "slug": "la-at-pompidou",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "zFdQTQUO",
      "title": "Mario Merz",
      "slug": "mario-merz",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "HnTpi9L6",
      "title": "Georgina Starr",
      "slug": "georgina-starr",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "UB3u8PXg",
      "title": "Chema Cobo",
      "slug": "chema-cobo",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "3Td_ykO8",
      "title": "Don Dudley",
      "slug": "don-dudley",
      "year": null,
      "type": "essay",
      "author": null,
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "considering-other-artists",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "OOfgaxcK",
      "title": "Writings About Thomas Lawson",
      "slug": "writings-about-thomas-lawson",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "-TO6NNse",
      "title": "Thomas Lawson at LAXART",
      "slug": "thomas-lawson-at-laxart",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "CxnZCjIo",
      "title": "Christopher Miles Review",
      "slug": "christopher-miles-review",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "Oz0zaTWZ",
      "title": "Donald Kuspit",
      "slug": "donald-kuspit",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "6LEuLI7h",
      "title": "Ingrid Sischy",
      "slug": "ingrid-sischy",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "7vj4wJxV",
      "title": "Jeanne Silverthorn",
      "slug": "jeanne-silverthorn",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "j_5gQaAL",
      "title": "Joan Casademont",
      "slug": "joan-casademont",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "opt0crqH",
      "title": "Judith Russi",
      "slug": "judith-russi",
      "year": 1983,
      "type": "article",
      "author": "various",
      "publication": "May 1983",
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "AOuRenQ5",
      "title": "Kate Linker",
      "slug": "kate-linker",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "MlghiDLo",
      "title": "Kuspit on Drawing",
      "slug": "kuspit-on-drawing",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "08-q14Gs",
      "title": "Lisa Liebman",
      "slug": "lisa-liebman",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "Q9jdLjCF",
      "title": "Lobel Singerman",
      "slug": "lobel-singerman",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "AMWEgIVG",
      "title": "Nichtsagend",
      "slug": "nichtsagend",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "cwj1bHLl",
      "title": "Ron Jones - Decay of Living",
      "slug": "ron-jones-decay-of-living",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "xySVvaoQ",
      "title": "Ron Jones - Seeing is Believing",
      "slug": "ron-jones-seeing-is-believing",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "509H1ND3",
      "title": "Schjeldahl",
      "slug": "schjeldahl",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "SRAg7B0y",
      "title": "Sydney Biennale",
      "slug": "sydney-biennale",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    },
    {
      "id": "bwOomDZY",
      "title": "Travis Deihl",
      "slug": "travis-deihl",
      "year": null,
      "type": "article",
      "author": "various",
      "publication": null,
      "issue": null,
      "url": null,
      "images": [],
      "description": null,
      "artworkIds": [],
      "sourceUrl": null,
      "category": "writings-about-tl",
      "createdAt": "2026-03-18T07:24:09.864Z",
      "updatedAt": "2026-03-18T07:24:09.864Z"
    }
  ],
  "scrapedAt": "2026-03-18T07:24:42.469Z",
  "version": "5.0.0"
};

// --- State ---
let currentTab = "artworks";
let filters = { section: "", series: "", search: "" };
let wFilters = { category: "", search: "" };

// --- Init ---
function init() {
  const c = CATALOG;
  document.getElementById("stats").textContent =
    c.artworks.length + " artworks · " +
    c.writings.length + " writings · " +
    c.exhibitions.length + " exhibitions · " +
    "b. " + c.artist.born + ", " + c.artist.birthplace;

  // Populate filter dropdowns
  const sectionLabels = (c.sections || []).map(s => s.label);
  const seriesList = [...new Set(c.artworks.map(a => a.series).filter(Boolean))].sort();
  const wCats = [...new Set(c.writings.map(w => w.category))].sort();

  populateSelect("filter-section", sectionLabels);
  populateSelect("filter-series", seriesList);
  populateSelect("filter-w-cat", wCats);

  // Event listeners
  document.getElementById("filter-section").onchange = e => { filters.section = e.target.value; renderArtworks(); };
  document.getElementById("filter-series").onchange = e => { filters.series = e.target.value; renderArtworks(); };
  document.getElementById("filter-search").oninput = e => { filters.search = e.target.value.toLowerCase(); renderArtworks(); };
  document.getElementById("filter-w-cat").onchange = e => { wFilters.category = e.target.value; renderWritings(); };
  document.getElementById("filter-w-search").oninput = e => { wFilters.search = e.target.value.toLowerCase(); renderWritings(); };

  // Tabs
  document.getElementById("tabs").onclick = e => {
    const btn = e.target.closest("button");
    if (!btn) return;
    currentTab = btn.dataset.tab;
    document.querySelectorAll(".tabs button").forEach(b => b.classList.toggle("active", b === btn));
    document.getElementById("artworks-view").style.display = currentTab === "artworks" ? "" : "none";
    document.getElementById("writings-view").style.display = currentTab === "writings" ? "" : "none";
    document.getElementById("exhibitions-view").style.display = currentTab === "exhibitions" ? "" : "none";
  };

  // Lightbox
  const lb = document.getElementById("lightbox");
  lb.onclick = () => lb.classList.remove("open");

  // User badge
  const badge = document.getElementById("user-badge");
  if (WP_USER) {
    const role = WP_USER.isAdmin ? "admin" : WP_USER.isEditor ? "editor" : "viewer";
    const roleClass = WP_USER.isAdmin ? "role-admin" : WP_USER.isEditor ? "role-editor" : "role-viewer";
    badge.innerHTML =
      '<span class="user-name">' + esc(WP_USER.name) + '</span>' +
      '<span class="user-role ' + roleClass + '">' + role + '</span>' +
      '<a href="/wp-admin/">Dashboard</a>';
  } else {
    badge.innerHTML = '<a href="/wp-login.php?redirect_to=' + encodeURIComponent(location.href) + '">Log in</a>';
  }

  renderArtworks();
  renderWritings();
  renderExhibitions();
}

function populateSelect(id, values) {
  const sel = document.getElementById(id);
  for (const v of values) {
    const opt = document.createElement("option");
    opt.value = v;
    opt.textContent = v;
    sel.appendChild(opt);
  }
}

// --- Render Artworks (grouped by section with intros) ---
function renderArtworks() {
  const container = document.getElementById("artwork-sections");
  const sections = CATALOG.sections || [];
  let allArtworks = CATALOG.artworks;
  let totalShown = 0;

  if (filters.series) allArtworks = allArtworks.filter(a => a.series === filters.series);
  if (filters.search) allArtworks = allArtworks.filter(a => a.title.toLowerCase().includes(filters.search));

  let html = "";

  for (const sec of sections) {
    let artworks = allArtworks.filter(a => {
      if (sec.period) return a.period === sec.period;
      if (sec.tag) return a.tags && a.tags.includes(sec.tag);
      return false;
    });

    if (filters.section && filters.section !== sec.label) continue;
    if (artworks.length === 0 && !filters.section) continue;

    totalShown += artworks.length;

    // Section header
    html += '<div class="section-header">';
    html += '<h2>' + esc(sec.label) + '</h2>';
    html += '<div class="section-line"></div>';

    if (sec.intros && sec.intros.length > 0) {
      html += '<div class="section-intros">';
      for (const intro of sec.intros) {
        html += '<div class="intro-block">';
        if (intro.heading) {
          html += '<div class="intro-heading">' + esc(intro.heading) + '</div>';
        }
        const paras = intro.text.split("\n\n");
        for (const p of paras) {
          html += '<div class="intro-text">' + esc(p) + '</div>';
        }
        html += '</div>';
      }
      html += '</div>';
    }

    html += '</div>';

    // Artwork grid
    html += '<div class="section-grid">';
    html += artworks.map(a => renderCard(a)).join("");
    html += '</div>';
  }

  container.innerHTML = html;
  document.getElementById("artwork-count").textContent = totalShown + " works";
}

function renderCard(a) {
  const img = a.images[0];
  const thumbUrl = makeThumb(img.url);
  const yearStr = a.year ? String(a.year) + (a.yearEnd ? "\u2013" + a.yearEnd : "") : "";
  const mediumStr = a.medium ? '<div class="medium">' + esc(a.medium) + '</div>' : "";
  const dimsStr = a.dimensions ? '<div class="dims">' + formatDims(a.dimensions) + '</div>' : "";
  const tagsHtml = a.series ? '<div class="tags"><span class="tag">' + esc(a.series) + '</span></div>' : "";
  const serialStr = a.serial ? '<div class="serial">' + esc(a.serial) + '</div>' : "";
  const imgCount = a.images.length > 1 ? '<span class="img-count">' + a.images.length + ' views</span>' : "";

  return '<div class="card" data-slug="' + a.slug + '" onclick="openLightbox(this)"' +
    ' data-full="' + esc(img.url) + '"' +
    ' data-title="' + esc(a.title) + '"' +
    ' data-serial="' + esc(a.serial || "") + '"' +
    ' data-detail="' + esc([yearStr, a.medium, a.dimensions ? formatDims(a.dimensions) : ""].filter(Boolean).join(" \u00b7 ")) + '">' +
    '<div class="img-wrap"><img src="' + esc(thumbUrl) + '" alt="' + esc(a.title) + '" loading="lazy">' + imgCount + '</div>' +
    '<div class="info">' +
      '<h3><em>' + esc(a.title) + '</em></h3>' +
      (yearStr ? '<div class="year">' + yearStr + '</div>' : '') +
      mediumStr + dimsStr + tagsHtml + serialStr +
    '</div></div>';
}

function makeThumb(url) {
  // Use full-res URL directly — WP doesn't generate square thumbs
  return url;
}

function formatDims(d) {
  if (!d) return "";
  if (d.text) return d.text;
  let s = d.width + " × " + d.height;
  if (d.depth) s += " × " + d.depth;
  s += " " + d.unit + ".";
  return s;
}

function esc(s) {
  if (!s) return "";
  return String(s).replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;");
}

function openLightbox(card) {
  const lb = document.getElementById("lightbox");
  document.getElementById("lb-img").src = card.dataset.full;
  document.getElementById("lb-title").innerHTML = "<em>" + esc(card.dataset.title) + "</em>";
  const serial = card.dataset.serial ? ' <span style="font-size:0.75rem;color:#999;font-family:monospace">' + esc(card.dataset.serial) + '</span>' : '';
  document.getElementById("lb-detail").innerHTML = esc(card.dataset.detail) + serial;
  lb.classList.add("open");
}

// --- Render Writings ---
function renderWritings() {
  const list = document.getElementById("writings-list");
  let writings = CATALOG.writings;

  if (wFilters.category) writings = writings.filter(w => w.category === wFilters.category);
  if (wFilters.search) writings = writings.filter(w => w.title.toLowerCase().includes(wFilters.search));

  document.getElementById("writing-count").textContent = writings.length + " entries";

  list.innerHTML = writings.map(w => {
    const pub = [w.publication, w.issue].filter(Boolean).join(", ");
    return '<div class="writing-row">' +
      '<div class="w-title">' + esc(w.title) + '</div>' +
      '<div class="w-pub">' + esc(pub) + '</div>' +
      '<div class="w-year">' + (w.year || "") + '</div>' +
    '</div>';
  }).join("");
}

// --- Render Exhibitions ---
function renderExhibitions() {
  const grid = document.getElementById("exhibitions-grid");
  grid.innerHTML = CATALOG.exhibitions.map(e => {
    const imgs = e.images.slice(0, 6).map(i =>
      '<img src="' + esc(makeThumb(i.url)) + '" alt="" loading="lazy">'
    ).join("");
    return '<div class="exhibition-card">' +
      '<h3>' + esc(e.title) + '</h3>' +
      '<div class="e-year">' + (e.year || "") + '</div>' +
      (imgs ? '<div class="e-images">' + imgs + '</div>' : '') +
    '</div>';
  }).join("");
}

init();
</script>
</body>
</html>