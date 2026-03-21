// Insta, 2026.02.18
// Browse public Instagram profiles in a compact pixel view.
// Usage: insta:whistlegraph or insta:@whistlegraph

const { min, max, floor, ceil, sin } = Math;

const FONT = "MatrixChunky8";
const LINE_H = 10; // 8px font + 2px gap
const SCROLL_BAR_W = 3;
const LEFT_MARGIN = 9;
const RIGHT_MARGIN = 6;
const TOP_PAD = 4;
const BOTTOM_PAD = 10;

const scheme = {
  dark: {
    bg: [12, 14, 20],
    username: [255, 180, 220],
    fullName: [200, 200, 210],
    bio: [180, 180, 190],
    stats: [140, 180, 220],
    statNum: [220, 230, 255],
    rule: [40, 50, 70],
    postType: [180, 150, 255],
    timestamp: [90, 100, 120],
    caption: [170, 170, 180],
    likes: [255, 120, 120],
    comments: [140, 200, 255],
    rowAlt: [18, 20, 28],
    day: [120, 140, 160],
    error: [255, 80, 80],
    loading: [100, 110, 130],
    footer: [60, 65, 80],
    scrollTrack: [30, 35, 50],
    scrollThumb: [80, 100, 160],
    link: [100, 160, 220],
    private: [255, 200, 100],
  },
  light: {
    bg: [245, 243, 238],
    username: [180, 60, 120],
    fullName: [40, 40, 50],
    bio: [50, 50, 55],
    stats: [60, 80, 120],
    statNum: [30, 30, 40],
    rule: [200, 200, 210],
    postType: [100, 60, 140],
    timestamp: [120, 120, 130],
    caption: [70, 70, 80],
    likes: [200, 60, 60],
    comments: [40, 100, 180],
    rowAlt: [235, 233, 228],
    day: [100, 110, 120],
    error: [200, 40, 40],
    loading: [120, 120, 130],
    footer: [100, 100, 110],
    scrollTrack: [220, 220, 225],
    scrollThumb: [160, 170, 200],
    link: [40, 100, 180],
    private: [160, 120, 40],
  },
};

// State
let profile = null;
let posts = [];
let scroll = 0;
let contentHeight = 0;
let layoutItems = [];
let loading = true;
let error = null;
let lastWidth = 0;
let handle = "";
let pulsePhase = 0;

// Time formatting
const DAYS = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
const MONTHS = [
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
];

function timeAgo(epoch) {
  const seconds = floor((Date.now() / 1000) - epoch);
  const units = [
    { n: "y", s: 31536000 },
    { n: "mo", s: 2592000 },
    { n: "w", s: 604800 },
    { n: "d", s: 86400 },
    { n: "h", s: 3600 },
    { n: "m", s: 60 },
  ];
  for (const u of units) {
    const count = floor(seconds / u.s);
    if (count >= 1) return `${count}${u.n}`;
  }
  return "now";
}

function dayLabel(epoch) {
  const d = new Date(epoch * 1000);
  return `${DAYS[d.getDay()]} ${MONTHS[d.getMonth()]} ${d.getDate()}`;
}

function mediaIcon(type) {
  if (type === 2) return "[vid]";
  if (type === 8) return "[alb]";
  return "[img]";
}

// Layout engine
function computeLayout(textApi, screenWidth) {
  const items = [];
  let y = TOP_PAD;
  const wrapW = screenWidth - LEFT_MARGIN - RIGHT_MARGIN;

  if (!profile) return { items, totalHeight: y + BOTTOM_PAD };

  // Username
  const userText = `@${profile.username}`;
  items.push({ type: "username", text: userText, y, height: LINE_H });
  y += LINE_H;

  // Full name (if different from username)
  if (profile.fullName && profile.fullName.toLowerCase() !== profile.username) {
    items.push({ type: "fullName", text: profile.fullName, y, height: LINE_H });
    y += LINE_H;
  }

  // Bio
  if (profile.bio) {
    const bioLines = profile.bio.split("\n");
    for (const bioLine of bioLines) {
      if (!bioLine.trim()) {
        y += 4;
        continue;
      }
      const tb = textApi.box(
        bioLine, { x: 0, y: 0 }, wrapW, 1, true, FONT,
      );
      const h = tb ? tb.box.height : LINE_H;
      items.push({ type: "bio", text: bioLine, y, height: h });
      y += h;
    }
    y += 2;
  }

  // Stats line
  const statsText =
    `${profile.mediaCountFormatted} posts · ` +
    `${profile.followerCountFormatted} followers · ` +
    `${profile.followingCountFormatted} following`;
  items.push({ type: "stats", text: statsText, y, height: LINE_H });
  y += LINE_H + 2;

  // External URL
  if (profile.externalUrl) {
    const url = profile.externalUrl.replace(/^https?:\/\//, "");
    items.push({ type: "link", text: url, url: profile.externalUrl, y, height: LINE_H });
    y += LINE_H + 2;
  }

  // Private account notice
  if (profile.isPrivate) {
    items.push({ type: "private", text: "This account is private.", y, height: LINE_H });
    y += LINE_H + 2;
  }

  // Rule
  items.push({ type: "rule", y, height: 7 });
  y += 7;

  // Posts feed
  if (posts.length === 0 && !profile.isPrivate) {
    items.push({ type: "bio", text: "No posts.", y, height: LINE_H });
    y += LINE_H;
  }

  let lastDay = null;
  for (let i = 0; i < posts.length; i++) {
    const post = posts[i];
    const thisDay = dayLabel(post.timestamp);

    // Day separator
    if (thisDay !== lastDay) {
      items.push({ type: "day", text: thisDay, y, height: LINE_H + 4 });
      y += LINE_H + 4;
      lastDay = thisDay;
    }

    // Row 1: type icon + time + likes + comments
    const row1 =
      `${mediaIcon(post.mediaType)} ${timeAgo(post.timestamp)}  ` +
      `${post.likeCountFormatted}L ${post.commentCountFormatted}C`;
    items.push({
      type: "postHeader",
      text: row1,
      y,
      height: LINE_H,
      index: i,
      post,
    });
    y += LINE_H;

    // Row 2+: Caption (truncated)
    if (post.caption) {
      const capText = post.caption.split("\n")[0].slice(0, 200);
      const tb = textApi.box(
        capText, { x: 0, y: 0 }, wrapW, 1, true, FONT,
      );
      const h = tb ? min(tb.box.height, LINE_H * 3) : LINE_H;
      items.push({
        type: "caption",
        text: capText,
        y,
        height: h,
        index: i,
        post,
      });
      y += h;
    }

    y += 4; // Gap between posts
  }

  // Footer space
  if (posts.length > 0) {
    const footerText = `${posts.length} posts shown`;
    items.push({ type: "footer", text: footerText, y, height: LINE_H });
    y += LINE_H;
  }

  y += BOTTOM_PAD;
  return { items, totalHeight: y };
}

function clampScroll(screen) {
  const viewH = screen.height;
  const minScroll = -max(0, contentHeight - viewH);
  scroll = max(minScroll, min(0, scroll));
}

const INSTA_API = "https://silo.aesthetic.computer/insta";

async function fetchData() {
  const encodedHandle = encodeURIComponent(handle);
  try {
    const [profileRes, feedRes] = await Promise.all([
      fetch(`${INSTA_API}?action=profile&username=${encodedHandle}`),
      fetch(`${INSTA_API}?action=feed&username=${encodedHandle}`),
    ]);

    if (!profileRes.ok) {
      const err = await profileRes.json().catch(() => ({}));
      throw new Error(err.error || `Profile fetch failed (${profileRes.status})`);
    }

    profile = await profileRes.json();

    if (feedRes.ok) {
      const feedData = await feedRes.json();
      posts = feedData.posts || [];
    }
  } catch (err) {
    error = err.message;
  }
  loading = false;
  lastWidth = 0; // Force layout recomputation
}

// Boot
async function boot({ colon, params, hud }) {
  handle = (colon?.[0] || params?.[0] || "").replace(/^@/, "");

  if (!handle) {
    error = "Usage: insta:username";
    loading = false;
    return;
  }

  hud.label(`insta @${handle}`);
  await fetchData();
}

// Paint
function paint({ wipe, ink, screen, text, line, dark, paintCount }) {
  const pal = dark ? scheme.dark : scheme.light;
  pulsePhase += 0.04;
  wipe(pal.bg);

  if (loading) {
    if (paintCount > 4n) {
      const dots = ".".repeat((floor(Number(paintCount) / 10) % 4));
      ink(pal.loading).write(
        `Loading @${handle}${dots}`,
        { center: "xy" }, undefined, undefined, true, FONT,
      );
    }
    return;
  }

  if (error) {
    ink(pal.error).write(
      error, { center: "xy" }, undefined, undefined, true, FONT,
    );
    return;
  }

  // Recompute layout if screen width changed
  if (screen.width !== lastWidth) {
    const result = computeLayout(text, screen.width);
    layoutItems = result.items;
    contentHeight = result.totalHeight;
    lastWidth = screen.width;
    clampScroll(screen);
  }

  const contentW = screen.width - LEFT_MARGIN - RIGHT_MARGIN;

  for (const item of layoutItems) {
    const y = item.y + scroll;
    if (y + item.height < 0 || y > screen.height) continue;

    switch (item.type) {
      case "username":
        ink(pal.username).write(
          item.text, { x: LEFT_MARGIN, y },
          undefined, contentW, true, FONT,
        );
        if (profile?.isVerified) {
          const vx = LEFT_MARGIN + text.width(item.text, FONT) + 4;
          ink(pal.stats).write("*", { x: vx, y }, undefined, undefined, false, FONT);
        }
        break;

      case "fullName":
        ink(pal.fullName).write(
          item.text, { x: LEFT_MARGIN, y },
          undefined, contentW, true, FONT,
        );
        break;

      case "bio":
        ink(pal.bio).write(
          item.text, { x: LEFT_MARGIN, y },
          undefined, contentW, true, FONT,
        );
        break;

      case "stats":
        ink(pal.stats).write(
          item.text, { x: LEFT_MARGIN, y },
          undefined, contentW, true, FONT,
        );
        break;

      case "link":
        ink(pal.link).write(
          item.text, { x: LEFT_MARGIN, y },
          undefined, contentW, true, FONT,
        );
        break;

      case "private":
        ink(pal.private).write(
          item.text, { x: LEFT_MARGIN, y },
          undefined, contentW, true, FONT,
        );
        break;

      case "rule": {
        const ruleY = y + 3;
        ink(pal.rule).line(
          LEFT_MARGIN, ruleY,
          screen.width - RIGHT_MARGIN, ruleY,
        );
        break;
      }

      case "day": {
        const bgAlpha = 120;
        ink(pal.rule[0], pal.rule[1], pal.rule[2], bgAlpha).box(
          0, y, screen.width, item.height,
        );
        ink(pal.day).write(
          item.text, { x: LEFT_MARGIN, y: y + 2 },
          undefined, undefined, false, FONT,
        );
        break;
      }

      case "postHeader": {
        // Alternating row bg
        if (item.index % 2 === 0) {
          ink(...pal.rowAlt, 100).box(
            0, y - 1, screen.width, item.height + 2,
          );
        }
        // Parse and render parts with different colors
        const icon = mediaIcon(item.post.mediaType);
        const ago = timeAgo(item.post.timestamp);
        let x = LEFT_MARGIN;

        ink(pal.postType).write(icon, { x, y }, undefined, undefined, false, FONT);
        x += text.width(icon + " ", FONT);

        ink(pal.timestamp).write(ago, { x, y }, undefined, undefined, false, FONT);
        x += text.width(ago + "  ", FONT);

        const likeText = `${item.post.likeCountFormatted}L`;
        ink(pal.likes).write(likeText, { x, y }, undefined, undefined, false, FONT);
        x += text.width(likeText + " ", FONT);

        const commentText = `${item.post.commentCountFormatted}C`;
        ink(pal.comments).write(commentText, { x, y }, undefined, undefined, false, FONT);
        break;
      }

      case "caption":
        ink(pal.caption).write(
          item.text, { x: LEFT_MARGIN + 4, y },
          undefined, contentW - 4, true, FONT,
        );
        break;

      case "footer": {
        const glow = 100 + sin(pulsePhase) * 30;
        ink(pal.footer[0], pal.footer[1], pal.footer[2], glow).write(
          item.text, { x: LEFT_MARGIN, y },
          undefined, undefined, false, FONT,
        );
        break;
      }
    }
  }

  // Left-side scrollbar (matches chat.mjs / opinion.mjs)
  const viewH = screen.height;
  if (contentHeight > viewH) {
    ink(pal.scrollTrack).box(0, 0, SCROLL_BAR_W, viewH);
    const segH = max(1, floor((viewH / contentHeight) * viewH) - 1);
    const thumbY = ceil(viewH - segH - (scroll / contentHeight) * viewH) || 0;
    ink(pal.scrollThumb).box(0, thumbY, SCROLL_BAR_W, segH);
  }
}

// Act
function act({ event: e, screen, needsPaint, jump, net }) {
  if (loading) return;

  // Scroll wheel
  if (e.is("scroll")) {
    scroll -= e.y;
    clampScroll(screen);
    needsPaint();
    return;
  }

  // Drag scroll
  if (e.is("draw:1")) {
    scroll += e.delta.y;
    clampScroll(screen);
    needsPaint();
    return;
  }

  // Keyboard scroll
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:j")) {
    scroll -= 20;
    clampScroll(screen);
    needsPaint();
  }
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:k")) {
    scroll += 20;
    clampScroll(screen);
    needsPaint();
  }
  if (e.is("keyboard:down:pagedown")) {
    scroll -= screen.height - 20;
    clampScroll(screen);
    needsPaint();
  }
  if (e.is("keyboard:down:pageup")) {
    scroll += screen.height - 20;
    clampScroll(screen);
    needsPaint();
  }
  if (e.is("keyboard:down:home")) {
    scroll = 0;
    needsPaint();
  }
  if (e.is("keyboard:down:end")) {
    scroll = -max(0, contentHeight - screen.height);
    needsPaint();
  }

  // Refresh
  if (e.is("keyboard:down:r")) {
    loading = true;
    error = null;
    profile = null;
    posts = [];
    lastWidth = 0;
    fetchData();
    needsPaint();
  }

  // Back
  if (e.is("keyboard:down:escape")) {
    jump("prompt");
  }

  // Tap on post → open in browser
  if (e.is("lift")) {
    for (const item of layoutItems) {
      if (item.type !== "postHeader" && item.type !== "caption") continue;
      const y = item.y + scroll;
      if (e.y >= y && e.y < y + item.height && e.x >= LEFT_MARGIN) {
        const code = item.post.shortcode;
        if (code) net.web(`https://www.instagram.com/p/${code}/`);
        return;
      }
    }

    // Tap on link
    for (const item of layoutItems) {
      if (item.type !== "link") continue;
      const y = item.y + scroll;
      if (e.y >= y && e.y < y + item.height && e.x >= LEFT_MARGIN) {
        net.web(item.url);
        return;
      }
    }

    // Tap on username → open profile in browser
    for (const item of layoutItems) {
      if (item.type !== "username") continue;
      const y = item.y + scroll;
      if (e.y >= y && e.y < y + item.height && e.x >= LEFT_MARGIN) {
        net.web(`https://www.instagram.com/${handle}/`);
        return;
      }
    }
  }
}

function meta() {
  return {
    desc: "Browse public Instagram profiles in a compact pixel view.",
  };
}

export { boot, paint, act, meta };
