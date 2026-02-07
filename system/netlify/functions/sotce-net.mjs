// Sotce Net, 24.06.13.06.38
// A paid diary network by Sotce & Aesthetic Computer.

/* #region 🟢 TODO 
  + Now
  - [] Add custom meta descriptions for specific paths like '/chat'.
  - [] Add sound (to chat) and other buttons.
  - [] Pictures in pages.
    - [] Custom pictures or preset clip art from @amelia?
    - [] Pictures in line with text or like an overlay scrapbook style?
  - [] Add notifications to chat.
  + Later
  - [] Add "snippet" endpoint to get @amelia's latest page so it
       can be rendered on the login screen.
  *** 📧 Email Notifications for Pages ***
  - [] email new pages to each subscriber, and include the contents?
    - [] make an 'eblast' endpoint for this
    - [] add the checkbox under the main page for whether to receive them
         or not
  *** 'Think' *** 
  - [] Add meditation timer via AC sound engine, and sounds to buttons.
  *** 🔊 Sounds ***
  - [] Soft sine clicks and beeps.
  *** 📟 Page Feed ***
  - [] Add multi-user page feed. 
  *** 🛂 Page Controls ***
  - [] Automatic Dark Theme
  - [c] Patreon linkage.
  *** Accessibility ***
  - [] Accurate Tab Index in Modes/ different screens.
    - [] Login
    - [] Cookie Menu
    - [] Editor
  - [] Cleaner Ctrl +/- zoom logic / layout fixes.
  - [] Relational scrolling. 
  - [] Better routing / slash urls for the editor and cookie-menu.
    - [] Shouldn't resolve if no access.
  *** User Info Rate Limiting ***
  - [] Try to reduce the authorize() call rate limiting on ac.
  + Done
  - [x] Test on mobile.
  - [x] Deploy production chat.
  - [x] Delete any chat messages owned by a user when their account gets deleted.
  - [x] Test guest chat interface with logged in (unsubscribed) user.
  - [x] Display chatter count in the chat interface.
  - [x] Add an 'editor' route and an unsaved changes confirmation?
  - [x] Add proper url route support for `chat` on both the client title
        and server, in splash screen and in main ui.
  - [x] Finish bottom bar design.
  - [x] Show character limit / enforce on the text input.
  - [x] Limit scrollback to 100 chats.
  - [x] Handle chat disconnection / reconnection UI.
  - [x] Fix gate reload. / Add 'gate' route.
  - [x] Send a new message to chat and auto-scroll.
  - [x] Shouldn't be able to send blank messages.
  - [x] `chat` needs a special URL route that automatically opens to it.
  - [x] Test chat interface with subscribed user.
  - [x] Hide chat until pink spinner dot disappears.
  - [c] Add exporting of PNG images per pages.
  - [x] Test and enable printing on iOS.
#endregion */

// ♻️ Environment
const AUTH0_CLIENT_ID_SPA = "3SvAbUDFLIFZCc1lV7e4fAAGKWXwl2B0";
const AUTH0_DOMAIN = "https://hi.sotce.net";

const dev = process.env.NETLIFY_DEV;

// 💳 Payment
import {
  SOTCE_STRIPE_API_PRIV_KEY,
  SOTCE_STRIPE_API_PUB_KEY,
  SOTCE_STRIPE_API_TEST_PRIV_KEY,
  SOTCE_STRIPE_API_TEST_PUB_KEY,
  SOTCE_STRIPE_ENDPOINT_DEV_SECRET,
  SOTCE_STRIPE_ENDPOINT_SECRET,
  priceId,
  productId,
  prodProductId,
} from "../../backend/sotce-net-constants.mjs";

import { defaultTemplateStringProcessor as html } from "../../public/aesthetic.computer/lib/helpers.mjs";
import { respond } from "../../backend/http.mjs";
import { connect, ObjectId } from "../../backend/database.mjs";
import { shell } from "../../backend/shell.mjs";

import {
  authorize,
  deleteUser,
  hasAdmin,
  handleFor,
  getHandleOrEmail,
  userIDFromEmail,
} from "../../backend/authorization.mjs";
import * as KeyValue from "../../backend/kv.mjs";

import Stripe from "stripe";

const dateOptions = {
  weekday: "long",
  year: "numeric",
  month: "long",
  day: "numeric",
};

export const handler = async (event, context) => {
  // console.log("Event:", event, "Context:", context, "Path:", event.path);
  // 🚙 Router
  const method = event.httpMethod.toLowerCase();
  let path = event.path;
  if (path.startsWith("/sotce-net"))
    path = path.replace("/sotce-net", "/").replace("//", "/");
  if (path.startsWith("/sotce.net"))
    path = path.replace("/sotce.net", "/").replace("//", "/");

  const key = dev ? SOTCE_STRIPE_API_TEST_PRIV_KEY : SOTCE_STRIPE_API_PRIV_KEY;
  const assetPath = dev
    ? "/assets/sotce-net/"
    : "https://assets.aesthetic.computer/sotce-net/";

  const baseHost = event.headers["host"] || "localhost";
  const HOST = dev ? `https://${baseHost}/sotce-net` : `https://${baseHost}`;

  // 👑 Admin emails that get subscriber access without Stripe
  const ADMIN_EMAILS = ["me@jas.life", "sotce.net@gmail.com"];

  // Check to see if a user sub is subscribed.

  async function subscribed(user) {
    // 👑 Admin bypass - grant subscriber access without checking Stripe
    if (user.email && ADMIN_EMAILS.includes(user.email.toLowerCase())) {
      shell.log("👑 Admin bypass granted for:", user.email);
      return {
        status: "active",
        current_period_end: Math.floor(Date.now() / 1000) + 365 * 24 * 60 * 60, // 1 year from now
        admin_bypass: true,
      };
    }

    // TODO: 🟠 Add redis caching in here.
    try {
      // 🩷 First look to see if we have a subscribed entry in the redis cache.
      shell.log(
        "Checking subscription for:",
        user.sub,
        user.email,
        performance.now(),
      );

      await KeyValue.connect();
      const cachedSubscription = await KeyValue.get(
        "sotce-subscribed",
        user.sub,
      );

      if (cachedSubscription) {
        const parsed = JSON.parse(cachedSubscription);
        // Check to see if the time on parses has passed, and if it has
        // then invalidate the cache and continue to checking it with Stripe.
        const currentTime = Math.floor(Date.now() / 1000);
        const subscriptionEndTime = parsed.current_period_end; // Assume unix timestamp.

        if (subscriptionEndTime > currentTime) {
          shell.log("📰 Subscription active from cache!", performance.now());
          await KeyValue.disconnect();
          return parsed;
        } else {
          shell.log(
            "⌛ Subscription expired, invalidating cache and checking...",
          );
          await KeyValue.del("sotce-subscribed", user.sub);
        }
      }

      shell.log("🔍 Checking Stripe for subscription...");
      // 🩷 Then query it from Stripe,

      const stripe = Stripe(key);
      // Fetch customer by user ID (sub) from subscription metadata field.
      const customers = await stripe.customers.search({
        query: "metadata['sub']:'" + user.sub + "'",
      });

      if (!customers.data.length) {
        shell.log("❌ No customer found in Stripe");
        await KeyValue.disconnect();
        return { subscribed: false };
      }
      const customer = customers.data[0];
      shell.log("✅ Customer found:", customer.id);

      // Fetch subscriptions for the customer
      const subscriptions = await stripe.subscriptions.list({
        customer: customer.id,
        status: "active", // Only find the first active subscription.
        limit: 5,
      });

      shell.log("📊 Found subscriptions:", subscriptions.data.length);
      const subscription = subscriptions.data.find((sub) =>
        sub.items.data.some((item) => item.price.product === productId),
      );

      if (subscription) {
        shell.log("✅ Active subscription found:", subscription.status);
        // 🩷 And serialize it into redis.
        await KeyValue.set(
          "sotce-subscribed",
          user.sub,
          JSON.stringify({
            status: subscription.status,
            current_period_end: subscription.current_period_end,
          }),
        );
        await KeyValue.disconnect();
        return subscription;
      } else {
        shell.log("❌ No active subscription found");
        await KeyValue.disconnect();
        return { subscribed: false };
      }
    } catch (err) {
      shell.error("Error fetching subscription status:", err);
      return null;
    }
  }

  // Get the count of all active subscriptions for the given productId
  // TODO: Put this behind a redis cache... 24.10.14.01.23
  async function getActiveSubscriptionCount(productId) {
    try {
      const stripe = Stripe(key);

      // Fetch all subscriptions with the active status
      let hasMore = true;
      let totalSubscriptions = 0;
      let startingAfter = undefined;

      while (hasMore) {
        const subscriptions = await stripe.subscriptions.list({
          status: "active",
          limit: 100, // Maximum allowed per request
          starting_after: startingAfter,
        });

        // Filter subscriptions by the productId
        const matchingSubscriptions = subscriptions.data.filter((sub) =>
          sub.items.data.some((item) => item.price.product === productId),
        );

        totalSubscriptions += matchingSubscriptions.length;

        // Check if more pages of subscriptions exist
        hasMore = subscriptions.has_more;
        if (hasMore) {
          startingAfter = subscriptions.data[subscriptions.data.length - 1].id;
        }
      }

      return totalSubscriptions;
    } catch (err) {
      shell.error("Error fetching subscription count:", err);
    }
  }

  const MAX_LINES = 19;

  // 🏠 Home, Chat, Page Routes
  if (
    (path === "/" ||
      path === "/chat" ||
      path === "/gate" ||
      path === "/write" ||
      path === "/ask" ||
      path === "/respond" ||
      path.match(/^\/page\/\d+$/) ||
      path.match(/^\/q\/\d+$/)) &&
    method === "get"
  ) {
    const miniBreakpoint = 245;

    let title = "Sotce Net";
    if (path !== "/") {
      title = path.replace("/", "") + " · " + title;
    }

    const body = html`
      <html lang="en">
        <head>
          <meta charset="utf-8" />
          <title>${title}</title>
          <meta name="description" content="confessions" />
          <meta name="og:image" content="${assetPath}thumbnail.png" />
          <link rel="icon" type="image/png" href="${assetPath}cookie.png" />
          <link rel="apple-touch-icon" href="${assetPath}cookie.png" />
          <link rel="preload" href="${assetPath}cookie.png" as="image" />
          <link rel="preload" href="${assetPath}cookie-open.png" as="image" />
          <link
            rel="preload"
            href="${assetPath}Wingdings 2.ttf"
            as="font"
            type="font/ttf"
            crossorigin="anonymous"
          />
          <meta
            name="viewport"
            content="width=device-width, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no, viewport-fit=cover, interactive-widget=resizes-content"
          />
          <!--<link
            href="https://fonts.googleapis.com/css2?family=EB+Garamond:ital,wght@0,400..800;1,400..800&display=block"
            rel="stylesheet"
          />-->
          <link rel="preconnect" href="https://fonts.googleapis.com" />
          <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin />
          <link
            href="https://fonts.googleapis.com/css2?family=Carlito:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900&display=swap"
            rel="stylesheet"
          />
          <style>
            @font-face {
                font-family: "Helvetica";
                src: url('${assetPath}helvetica.woff') format('woff'),
                    url('${assetPath}helvetica.ttf') format('truetype');
                font-weight: normal;
                font-style: normal;
            }

            @font-face {
                font-family: "Helvetica";
                src: url('${assetPath}helvetica-bold.woff') format('woff'),
                    url('${assetPath}helvetica-bold.ttf') format('truetype');
                font-weight: bold;
                font-style: normal;
            }

            /*
             * 🎨 SOTCE-NET THEME SYSTEM
             * ========================
             * Light mode: Pink/cream paper aesthetic (original)
             * Dark mode: Warm brown/sepia tones (cozy evening read)
             * 
             * Theme colors are defined in :root and @media (prefers-color-scheme: dark)
             * Canvas colors are passed via CSS custom properties and read in JS
             */
            
            :root {
              /* === Light Mode (Default) === */
              -webkit-locale: "en";
              
              /* Page/Background Colors */
              --background-color: #FFD1DC;
              --garden-background: #FFD1DC;
              --chat-background: rgb(240, 235, 230);
              --chat-input-bar-background: rgb(255, 240, 235);
              --backpage-color: rgb(250, 250, 250);
              --backpage-color-translucent: rgba(250, 250, 250, 0.8);
              --editor-placemat-background: rgba(255, 255, 255, 0.5);
              --editor-placemat-background-opaque: rgb(255, 255, 255);
              
              /* Card/Paper Colors (for canvas) */
              --card-background: #f8f4ec;
              --card-back-background: #f0ebe0;
              --card-border: #d4c8b8;
              --card-ear: #e8e0d0;
              --card-ear-hover: #FFD1DC;
              --card-text: #000000;
              --card-text-muted: #666666;
              --card-text-dim: #999999;
              --card-text-faint: #aaaaaa;
              
              /* Question Card Colors (bluish) */
              --question-card-background: #e8f0f8;
              --question-card-border: #b8c8d8;
              --question-card-ear: #d0e0f0;
              
              /* UI Colors */
              --pink-border: rgb(255, 190, 215);
              --button-background: rgb(255, 235, 183);
              --button-background-highlight: rgb(255, 245, 170);
              --button-text: black;
              --spinner-background: rgb(255, 147, 191);
              --destructive-red: rgb(200, 0, 0);
              
              /* Chat Colors */
              --chat-text: rgb(50, 50, 50);
              --chat-handle: rgb(200, 80, 120);
              --chat-link: rgb(80, 120, 200);
              --chat-link-hover: rgb(60, 100, 180);
              --chat-diary-link: rgb(180, 120, 80);
              --chat-question-link: rgb(80, 140, 200);
              --chat-timestamp: rgba(0, 0, 0, 0.4);
              --chat-message-border: rgba(0, 0, 0, 0.15);
              --chat-input-bg: white;
              --chat-input-text: black;
              --chat-input-border: rgb(130, 100, 100);
              --chat-autocomplete-bg: white;
              --chat-autocomplete-selected: var(--button-background-highlight);
              --chat-shadow: rgb(80, 80, 80);
              
              /* Gate/Text Colors */
              --gate-text: black;
              --button-active-bg: rgb(255, 248, 165);
              --positive-bg: rgb(203, 238, 161);
              --positive-border: rgb(114, 203, 80);
              --positive-hover: rgb(199, 252, 136);
              --positive-active: rgb(210, 252, 146);
              --negative-bg: rgb(255, 154, 168);
              --negative-border: rgb(255, 87, 87);
              --negative-hover: rgb(255, 171, 171);
              --negative-active: rgb(255, 161, 186);
              
              /* Link Color */
              --link-color: rgb(80, 100, 180);
              
              /* Typography */
              --line-height: 1.76em;
              --page-font: "Helvetica";
              --max-lines: ${MAX_LINES};
            }

            /* === Dark Mode === */
            @media (prefers-color-scheme: dark) {
              :root {
                /* Page/Background Colors - deep rose/plum evening */
                --background-color: #2d1f2a;
                --garden-background: #2d1f2a;
                --chat-background: #231a20;
                --chat-input-bar-background: #2a1f26;
                --backpage-color: #1e171b;
                --backpage-color-translucent: rgba(30, 23, 27, 0.8);
                --editor-placemat-background: rgba(35, 26, 32, 0.5);
                --editor-placemat-background-opaque: #231a20;
                
                /* Card/Paper Colors - olive/sage tinted parchment */
                --card-background: #3a3832;
                --card-back-background: #33322c;
                --card-border: #5a5548;
                --card-ear: #4a4840;
                --card-ear-hover: #8a5070;
                --card-text: #ece8de;
                --card-text-muted: #b0a898;
                --card-text-dim: #908878;
                --card-text-faint: #706858;
                
                /* Question Card Colors (darker blue) */
                --question-card-background: #2a3442;
                --question-card-border: #4a5a6a;
                --question-card-ear: #3a4a5a;
                
                /* UI Colors - olive-purple tones */
                --pink-border: #a06080;
                --button-background: #4a4550;
                --button-background-highlight: #5a5560;
                --button-text: #e8e0f0;
                --spinner-background: #7a5068;
                --destructive-red: rgb(200, 70, 80);
                
                /* Chat Colors - warm evening tones */
                --chat-text: #ddd5cc;
                --chat-handle: #d88aa0;
                --chat-link: #7ab0e0;
                --chat-link-hover: #9ac8f0;
                --chat-diary-link: #d0a070;
                --chat-question-link: #70b0d8;
                --chat-timestamp: rgba(255, 255, 255, 0.35);
                --chat-message-border: rgba(255, 255, 255, 0.1);
                --chat-input-bg: #2a2420;
                --chat-input-text: #e8e0d8;
                --chat-input-border: #5a4a50;
                --chat-autocomplete-bg: #3a3030;
                --chat-autocomplete-selected: #5a4a40;
                --chat-shadow: rgba(0, 0, 0, 0.4);
                
                /* Gate/Text Colors */
                --gate-text: #e8e0d8;
                --button-active-bg: #6a6050;
                --positive-bg: #3a5030;
                --positive-border: #4a7040;
                --positive-hover: #4a6040;
                --positive-active: #5a7050;
                --negative-bg: #5a3038;
                --negative-border: #7a4048;
                --negative-hover: #6a3a42;
                --negative-active: #5a3540;
                
                /* Link Color */
                --link-color: #8ab0e0;
              }
            }

            /* Dark mode scrollbars */
            @media (prefers-color-scheme: dark) {
              * {
                scrollbar-color: #5a5060 #2a2028;
              }
              ::-webkit-scrollbar {
                width: 10px;
                height: 10px;
              }
              ::-webkit-scrollbar-track {
                background: #2a2028;
              }
              ::-webkit-scrollbar-thumb {
                background: #5a5060;
                border-radius: 5px;
              }
              ::-webkit-scrollbar-thumb:hover {
                background: #6a6070;
              }
            }

            /* Dark mode: DOM editor overrides (ask, respond, write-a-page) */
            @media (prefers-color-scheme: dark) {
              /* === Write-a-Page Editor === */
              #garden article.page,
              #editor-page,
              #print-page article.page {
                background-color: #3a3832 !important;
                border-color: #5a5548 !important;
              }
              #garden article.page div.page-number,
              #editor-page div.page-number,
              #garden article.page div.page-title,
              #editor-page div.page-title {
                color: #b0a898 !important;
              }
              #garden #editor textarea {
                background: #3a3832 !important;
                caret-color: #d88aa0 !important;
              }
              #garden #editor #words-wrapper::before,
              #garden #editor #words-wrapper::after {
                background: #33322c !important;
              }
              #garden #editor #words-wrapper.invisible.hover {
                background: rgba(90, 85, 72, 0.25) !important;
              }
              #garden #editor #words-wrapper.invisible.active {
                background: rgba(90, 85, 72, 0.15) !important;
              }
              #garden #editor #words-wrapper.invisible.hover::after {
                background: rgba(90, 85, 72, 0.5) !important;
              }
              #garden #editor #words-wrapper.invisible.active::after {
                background: rgba(100, 95, 72, 0.6) !important;
              }
              #editor-lines-left {
                background: linear-gradient(
                  to bottom,
                  rgba(45, 31, 42, 0.85) 25%,
                  transparent 100%
                ) !important;
              }
              #nav-editor {
                background: linear-gradient(
                  to top,
                  rgba(45, 31, 42, 0.8) 25%,
                  transparent 100%
                ) !important;
              }
              .lines-left-loads {
                color: #b0a898 !important;
              }
              /* Backpage */
              #garden .page-wrapper .backpage {
                background: rgba(30, 23, 27, 0.9) !important;
                border-color: #5a5548 !important;
                color: #ece8de !important;
              }
              .crumple-this-page {
                color: #b0a898 !important;
              }
              .share-this-page {
                color: #b0a898 !important;
              }
              #garden .page-wrapper div.ear.hover,
              #garden .page-wrapper div.ear.active {
                border-color: #5a5548 !important;
              }
              #garden .page-wrapper div.ear.active::after {
                background: #3a3832 !important;
              }
              #garden .page-wrapper div.ear.reverse.hover::after,
              #garden .page-wrapper div.ear.reverse.active::after {
                background: #3a3832 !important;
              }

              /* === Ask Editor === */
              #ask-editor-page {
                background-color: #2a3442 !important;
                border-color: #3a4a5a !important;
              }
              #ask-editor-page .ask-title,
              #ask-editor-page .ask-date,
              #ask-editor-page .ask-number {
                color: #98a8b8 !important;
              }
              #ask-editor-page #ask-words-wrapper {
                background: #243340 !important;
              }
              #ask-editor-page #ask-words-wrapper::before,
              #ask-editor-page #ask-words-wrapper::after {
                background: #1e2d3a !important;
              }
              #ask-editor-page #ask-highlights {
                color: #ece8de !important;
              }
              #ask-editor-page textarea {
                caret-color: #d88aa0 !important;
              }
              #ask-answer-space {
                border-top-color: rgba(255, 255, 255, 0.08) !important;
                color: rgba(176, 168, 152, 0.35) !important;
              }
              /* My Questions page (inside ask editor) */
              #asks-list-page {
                color: #ece8de !important;
              }
              #asks-list-page h2 {
                color: #ece8de !important;
              }
              #asks-list-page .ask-item {
                color: #ece8de !important;
                border-bottom-color: rgba(90, 85, 72, 0.4) !important;
              }
              #asks-list-page .ask-status {
                color: #b0a898 !important;
              }
              #asks-list-page .ask-item.answered {
                background: rgba(74, 112, 64, 0.2) !important;
              }
              #asks-list-page p {
                color: #b0a898 !important;
              }
              /* Ask toggle ("my questions") + pending toggle buttons */
              nav button.ask-toggle {
                background: var(--button-background) !important;
                border-color: var(--pink-border) !important;
                color: var(--button-text) !important;
              }
              nav button.ask-toggle:hover {
                background: var(--button-background-highlight) !important;
              }
              nav button.ask-toggle:active {
                background: var(--button-active-bg) !important;
              }
              nav button.pending-toggle {
                background: #4a3a30 !important;
                border-color: #7a5a40 !important;
                color: #e8d0b8 !important;
              }
              nav button.pending-toggle:hover {
                background: #5a4a3a !important;
              }
              nav button.pending-toggle:active {
                background: #6a5a4a !important;
              }
              #ask-chars-left {
                background: linear-gradient(
                  to bottom,
                  rgba(26, 42, 56, 0.85) 25%,
                  transparent 100%
                ) !important;
              }
              #nav-ask-editor {
                background: linear-gradient(
                  to top,
                  rgba(26, 42, 56, 0.8) 25%,
                  transparent 100%
                ) !important;
              }

              /* === Respond Editor === */
              #respond-editor-page {
                background-color: #2a3442 !important;
                border-color: #3a4a5a !important;
              }
              #respond-editor-page .respond-date {
                color: #e0e8f0 !important;
              }
              #respond-editor-page .respond-title {
                color: #b8c8d8 !important;
              }
              #respond-editor-page .respond-question-section .respond-counter {
                color: #b8c8d8 !important;
              }
              #respond-editor-page .respond-question-text {
                color: #e8f0f5 !important;
              }
              #respond-editor-page .respond-separator {
                border-top-color: rgba(255, 255, 255, 0.12) !important;
              }
              #respond-editor-page #respond-words-wrapper {
                background: #243340 !important;
              }
              #respond-editor-page #respond-words-wrapper::before,
              #respond-editor-page #respond-words-wrapper::after {
                background: #1e2d3a !important;
              }
              #respond-editor-page .respond-textarea {
                color: #f5f0e8 !important;
                caret-color: #e8a0b8 !important;
              }
              #respond-editor-page .page-number {
                color: #d8c8b8 !important;
              }
              #respond-lines-left {
                background: linear-gradient(
                  to bottom,
                  rgba(26, 42, 56, 0.85) 25%,
                  transparent 100%
                ) !important;
              }
              #respond-asked-by {
                color: #d88aa0 !important;
              }
              #nav-respond-editor {
                background: linear-gradient(
                  to top,
                  rgba(26, 42, 56, 0.8) 25%,
                  transparent 100%
                ) !important;
              }

              /* === Page placeholder (empty pages in garden) === */
              .page-placeholder {
                background: rgba(58, 56, 50, 0.5);
                border-color: rgba(90, 85, 72, 0.3);
                color: rgba(176, 168, 152, 0.4);
              }

              /* === Ask list items inside ask editor === */
              #asks-list h3 {
                color: #b0a898 !important;
              }
              .ask-item {
                color: #ece8de !important;
              }
              .ask-item.answered {
                background: rgba(74, 112, 64, 0.25) !important;
              }
              .ask-item.answered:hover {
                background: rgba(74, 112, 64, 0.4) !important;
              }
              .ask-item button.take-back {
                background: rgba(200, 80, 80, 0.15) !important;
                border-color: rgba(200, 80, 80, 0.3) !important;
                color: rgb(220, 120, 120) !important;
              }
              .ask-item button.take-back:hover {
                background: rgba(200, 80, 80, 0.3) !important;
              }

              /* === Prompt / back button on editor overlay === */
              #prompt {
                color: #b0a898 !important;
              }
              #prompt:hover {
                color: #d88aa0 !important;
              }

              /* === Garden page text (rendered pages in scroll view) === */
              #garden article.page .words,
              #print-page article.page .words {
                color: #ece8de !important;
              }
              #garden article.page div.page-number:hover {
                color: #d0a070 !important;
              }

              /* === Write-a-page: textarea text + measurement overlay === */
              #garden #editor textarea {
                color: #ece8de !important;
              }
              #editor-measurement {
                color: #ece8de !important;
              }

              /* === Respond view (non-admin user respond) === */
              .respond-view .respond-handle {
                color: #d88aa0 !important;
              }
              .respond-view .respond-question-text {
                background: rgba(51, 50, 44, 0.6) !important;
                border-left-color: #5a5548 !important;
                color: #ece8de !important;
              }
              .respond-view .respond-label {
                color: #7ab0e0 !important;
              }
              .respond-view .respond-textarea {
                background: #33322c !important;
                color: #ece8de !important;
                caret-color: #d88aa0 !important;
              }

              /* === Lines-left counter text colors === */
              .lines-left-lots {
                color: #7ab07a !important;
              }
              .lines-left-little {
                color: #d0a060 !important;
              }
              .lines-left-few {
                color: #d07060 !important;
              }
            }

            /* Using default browser scrollbars */

            html,
            body {
              touch-action: none; /*pan-x pan-y;*/
            }

            /* Preload EB Garamond. */
            html::after {
              content: " ";
              font-family: var(--page-font);//"EB Garamond", serif;
              /* font-weight: 400; */
              font-style: normal;
              visibility: hidden;
              position: absolute;
              height: 0;
              width: 0;
              overflow: hidden;
            }

            html.veiled body {
              background: rgba(0, 0, 0, 0.75) !important;
              transition: 0.5s background;
            }

            html.editing {
              /*background: mix() blue !important;*/ /*var(--garden-background);*/
              background: color-mix(
                in srgb,
                var(--garden-background) 70%,
                var(--editor-placemat-background-opaque) 30%
              ) !important;
            }

            html.editing #garden #top-bar {
              z-index: 4;
              opacity: 0.5;
              pointer-events: none;
            }

            html.editing #garden-canvas {
              display: none;
            }

            /* Disable grab cursor in editing mode */
            html.editing #garden {
              cursor: default;
            }
            html.editing #garden:active {
              cursor: default;
            }

            html.editing body {
              /* background: var(--editor-placemat-background); */
            }

            html.garden {
              background: var(--garden-background);
            }

            html {
              background: var(--background-color);
            }

            html.printing, html.printing #garden {
              background: white !important;
            }

            html.printing #garden {
              display: none;
            }

            body {
              font-family: sans-serif;
              margin: 0;
              width: 100%;
              /* min-height: 100%; */
              /* background: yellow; */
              background: rgba(0, 0, 0, 0);
              -webkit-text-size-adjust: none;
              user-select: none;
              -webkit-user-select: none;
              /* height: 100%; */
              /* overflow: hidden; */
              overscroll-behavior-y: none; /* prevent pull-to-refresh for Chrome 63+ */
              color: var(--gate-text);
            }
            
            a {
              color: var(--link-color);
            }
            
            a:hover {
              opacity: 0.8;
            }

            /* prevent pull-to-refresh for Safari 16+ */

            @media screen and (pointer: coarse) {
              @supports (-webkit-backdrop-filter: blur(1px)) and
                (overscroll-behavior-y: none) {
                html/*:not(.editing)*/ {
                  /* min-height: 100.0%; */
                  overscroll-behavior-y: none;
                }
              }
            }

            /*
            @media screen and (pointer: coarse) {
              @supports (-webkit-backdrop-filter: blur(1px)) and
                (not (overscroll-behavior-y: none)) {
                html {
                  height: 100%;
                  overflow: hidden;
                }
                body {
                  margin: 0px;
                  max-height: 100%;
                  overflow: auto;
                  -webkit-overflow-scrolling: touch;
                }
              }
            }
            */

            /* print.css */
            @media print {
              body {
                width: 8.5in;
                height: 11in;
                /* aspect-ratio: 8.5 / 11; */
                /* max-height: 11in; */
                margin: 0;
                max-height: 100vh;
                overflow-y: hidden;
              }
            }

            #editor-page,
            #garden article.page,
            #print-page article.page,
            #garden #editor textarea,
            #garden .page-wrapper .backpage {
              font-family: var(--page-font), serif;
              font-optical-sizing: auto;
              font-style: normal;
              letter-spacing: 0;
              overflow-x: hidden;
            }

            #garden article.page {
              z-index: 0;
            }

            /* Show a little transparent yellow highlight under each page. */
            /*
            html:not(.veiled) #garden div.page-wrapper::after {
              content: '🩷';
              font-size: 75%;
              opacity: 0.25;
              font-family: var(--page-font), serif;
              position: absolute;
              bottom: 2.5em;
              right: 2.5em;
              z-index: 0;
              transform: scale(1.5);
            }
            */

            #editor-page,
            #print-page article.page,
            #garden article.page {
              /* font-size: calc(3.25px * 8); */ /* Garamond */
              /* font-size: calc(2.97px * 8); */ /* Inter */
              /* font-size: calc(2.96px * 8); */ /* Calibri */
              font-size: calc(2.78px * 8); /* Helvetica */
            }

            /* body.noscroll { */
            /* overflow: hidden; */
            /* } */
            body.pages-hidden {
              /* overflow: hidden; */
            }
            body.pages-hidden #binding {
              display: none;
              /* visibility: hidden; */
              /* height: 100vh; */
              /* overflow: hidden; */
            }
            .asset {
              display: none;
            }
            #wrapper {
              display: flex;
              width: 100%;
              flex-direction: column;
              min-height: 100%;
              height: 100%;
              overflow-y: scroll;
              -webkit-overflow-scrolling: touch;
              touch-action: pan-y;
              scroll-snap-type: y proximity;
            }
            /* When garden is visible, wrapper shouldn't scroll - binding handles it */
            #wrapper:has(#garden:not(.hidden)) {
              overflow: hidden;
            }
            body.reloading::after {
              content: "";
              position: fixed;
              top: 0;
              left: 0;
              background: rgba(0, 0, 0, 0.5);
              width: 100%;
              height: 100%;
              z-index: 100;
              /* filter: blur(2px) saturate(1.25); */
              /* transition: 0.25s filter; */
              /* overflow: hidden; */
            }
            #wrapper.flash::after {
              content: "";
              position: fixed;
              top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              pointer-events: none;
              z-index: 100;
            }
            #wrapper.flash.red::after {
              /* sprinkle matching red */
              background-color: rgb(180, 11, 40, 0.8);
            }
            #spinner,
            #veil div.spinner {
              width: 5vmin;
              height: 5vmin;
              background: var(--spinner-background);
              margin: auto;
              border-radius: 100%;
              filter: blur(4px);
              transition: 0.25s opacity ease-in;
              opacity: 0;
              animation: spinner 1s infinite alternate ease-in-out;
            }
            #veil div.spinner {
              background: white;
            }
            #spinner.showing,
            #veil div.spinner.showing {
              opacity: 1;
              transition: 0.1s opacity ease-in;
            }
            @keyframes spinner {
              0% {
                filter: blur(4px);
                transform: scale(1);
              }
              50% {
                filter: blur(2px);
                transform: scale(1.2);
              }
              100% {
                filter: blur(4px);
                transform: scale(1);
              }
            }
            #email-verified {
              color: #33930b;
            }
            #full-alert {
              margin: auto;
            }
            #gate-curtain {
              /* position: fixed; */
              top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              display: flex;
              margin: auto;
            }
            #gate-curtain.hidden {
              position: fixed;
            }
            #gate {
              margin: auto;
              width: 260px;
              max-width: 80vw;
              /* background: yellow; */
              z-index: 1;
              box-sizing: border-box;
              padding: 0em 0.5em 2.25em 0.5em;
              transition: 0.1s opacity;
              opacity: 1;
            }
            #gate.coming-soon {
              padding: 0;
            }
            #gate.coming-soon :is(h2, h1, nav) {
              display: none;
            }
            #gate #cookie {
              max-width: 70%;
              max-height: 100%;
              margin: auto;
              display: block;
              user-select: none;
              -webkit-user-select: none;
              filter: drop-shadow(-2px 0px 1px rgba(0, 0, 0, 0.35));
              pointer-events: none;
            }
            @media (prefers-color-scheme: dark) {
              #gate #cookie {
                filter: drop-shadow(-2px 0px 1px rgba(0, 0, 0, 0.5)) brightness(0.85);
              }
            }
            #gate #cookie-wrapper {
              position: relative;
              /* z-index: 1000; */
              z-index: 1;
            }
            #gate #cookie-wrapper.interactive {
              cursor: pointer;
              transition: 0.2s ease-out transform;
            }
            #gate #cookie-wrapper.interactive:hover {
              transform: scale(0.99);
            }
            #gate #cookie-wrapper.interactive:active {
              transform: scale(0.96);
              transition: 0.13s ease-out transform;
            }
            #gate h1 {
              font-weight: normal;
              font-size: 100%;
              margin: 0;
              padding-bottom: 1em;
              text-align: center;
              user-select: none;
              -webkit-user-select: none;
              color: var(--gate-text);
            }
            #gate h2 {
              font-weight: normal;
              font-size: 100%;
              margin: 0;
              text-align: center;
              padding-bottom: 1em;
              user-select: none;
              -webkit-user-select: none;
              color: var(--gate-text);
            }
            #gate #nav-high {
              margin-top: -0.5em;
            }
            #gate :is(#nav-low, #nav-high) {
              display: flex;
              justify-content: center;
            }
            #gate
              :is(#nav-low, #nav-high):has(> *:first-child:nth-last-child(2)) {
              justify-content: space-between;
            }
            nav button,
            #write-a-page,
            #pages-button,
            /*#chat-enter,*/
            #chat-button,
            #ask-button,
            #respond-button {
              color: var(--button-text);
              background: var(--button-background);
              padding: 0.35em;
              font-size: 100%;
              border: 0.205em solid var(--pink-border);
              filter: drop-shadow(-0.065em 0.065em 0.065em var(--chat-shadow));
              border-radius: 0.5em;
              cursor: pointer;
              user-select: none;
              -webkit-user-select: none;
              -webkit-text-size-adjust: 100%;
              margin-bottom: 1em;
              -webkit-tap-highlight-color: transparent;
              touch-action: none;
              pointer-events: all;
            }
            a,
            textarea {
              -webkit-tap-highlight-color: transparent;
            }
            #chat-button {
              /* display: none; */
              margin-left: 1em;
            }
            #ask-button,
            #respond-button {
              margin-left: 1em;
            }
            @keyframes chat-unread-pulse {
              0%, 100% { border-color: var(--pink-border); }
              50% { border-color: var(--chat-handle); }
            }
            #chat-button.has-unread {
              animation: chat-unread-pulse 1.5s ease-in-out infinite;
            }
            /* Ask Editor - reuses #editor styles with ask-specific additions */
            #ask-editor {
              position: relative;
              width: 100%;
              min-height: 100.1%;
              top: 0;
              left: 0;
              border: none;
              z-index: 4;
              padding: 0;
              display: flex;
            }
            #ask-editor-form {
              padding-top: 100px;
              padding-bottom: 72px;
              padding-left: 16px;
              padding-right: 16px;
              box-sizing: border-box;
              margin: 0 auto auto auto;
            }
            #ask-editor-page {
              aspect-ratio: 4 / 5;
              background-color: rgb(240, 248, 255);
              border: calc(max(1px, 0.1em)) solid black;
              box-sizing: border-box;
              left: 0;
              padding: 1em;
              position: absolute;
              top: 0;
              transform-origin: top left;
              width: calc(100px * 8);
              font-family: var(--page-font), serif;
              font-size: calc(2.78px * 8);
            }
            #ask-editor-page .ask-title {
              position: absolute;
              top: calc(6.5% + 1.5em);
              left: 0;
              width: 100%;
              text-align: center;
              color: black;
              opacity: 0.6;
              font-size: 90%;
            }
            #ask-editor-page .ask-date {
              position: absolute;
              top: 6.5%;
              left: 0;
              width: 100%;
              text-align: center;
              color: black;
            }
            #ask-editor-page .ask-number {
              position: absolute;
              bottom: 6.5%;
              left: 0;
              width: 100%;
              text-align: center;
              color: black;
            }
            #ask-editor-page #ask-words-wrapper {
              position: relative;
              touch-action: none;
              margin-top: 15%;
              height: calc(var(--line-height) * 5);
              background: rgb(235, 245, 255);
            }
            #ask-editor-page #ask-words-wrapper::before {
              content: "";
              background: rgb(200, 220, 255);
              width: 2em;
              height: 100%;
              display: block;
              position: absolute;
              top: 0;
              left: 0;
              z-index: 101;
            }
            #ask-editor-page #ask-words-wrapper::after {
              content: "";
              background: rgb(200, 220, 255);
              width: 2em;
              height: 100%;
              display: block;
              position: absolute;
              top: 0;
              right: 0;
              z-index: 101;
            }
            #ask-editor-page #ask-highlights {
              position: absolute;
              top: 0;
              left: 0;
              width: 100%;
              padding: 0 2em;
              box-sizing: border-box;
              font-family: var(--page-font), serif;
              font-size: 100%;
              line-height: var(--line-height);
              text-align: justify;
              hyphens: auto;
              -webkit-hyphens: auto;
              overflow-wrap: break-word;
              white-space: pre-wrap;
              pointer-events: none;
              color: black;
              z-index: 99;
            }
            #ask-editor-page #ask-highlights .handle-hl {
              color: var(--chat-handle);
            }
            #ask-answer-space {
              position: relative;
              margin-top: 0.5em;
              border-top: 1px dashed rgba(0, 0, 0, 0.15);
              padding: 0 2em;
              box-sizing: border-box;
              display: flex;
              align-items: center;
              justify-content: center;
              color: rgba(0, 0, 0, 0.2);
              font-style: italic;
              font-size: 85%;
              transition: height 0.2s ease;
              overflow: hidden;
            }
            #ask-editor-page textarea {
              border: none;
              font-family: var(--page-font), serif;
              font-size: 100%;
              resize: none;
              display: block;
              background: transparent;
              color: transparent;
              padding: 0 2em;
              text-indent: 0em;
              text-align: justify;
              line-height: var(--line-height);
              height: calc(var(--line-height) * 5);
              width: 100%;
              overflow: hidden;
              position: relative;
              z-index: 100;
              hyphens: auto;
              -webkit-hyphens: auto;
              overflow-wrap: break-word;
              caret-color: rgb(50, 100, 180);
            }
            #ask-editor-page textarea:focus {
              outline: none;
            }
            #ask-chars-left {
              position: fixed;
              top: 0;
              left: 0;
              width: 100%;
              text-align: center;
              padding-top: 1.5em;
              padding-bottom: 1.5em;
              z-index: 6;
              background: linear-gradient(
                to bottom,
                rgb(220 235 250 / 70%) 25%,
                transparent 100%
              );
            }
            #nav-ask-editor {
              position: fixed;
              bottom: 0;
              left: 0;
              padding-top: 1em;
              width: 100%;
              padding-left: 1em;
              padding-right: 1em;
              box-sizing: border-box;
              display: flex;
              justify-content: space-between;
              z-index: 7;
              background: linear-gradient(
                to top,
                rgb(220 235 250 / 50%) 25%,
                transparent 100%
              );
            }
            #nav-ask-editor .nav-center {
              position: absolute;
              left: 50%;
              transform: translateX(-50%);
            }
            #asks-list {
              margin-top: 20%;
              padding: 0 2em;
              max-height: calc(var(--line-height) * 8);
              overflow-y: auto;
            }
            #asks-list h3 {
              margin: 0 0 0.5em 0;
              font-weight: normal;
              font-size: 90%;
              opacity: 0.6;
            }
            .ask-item {
              padding: 0.75em 0.5em;
              border-bottom: 1px solid var(--pink-border);
              font-size: 90%;
            }
            .ask-item:last-child {
              border-bottom: none;
            }
            .ask-item.answered {
              background: rgba(203, 238, 161, 0.3);
              margin-left: -0.5em;
              margin-right: -0.5em;
              cursor: pointer;
              transition: background 0.15s ease;
            }
            .ask-item.answered:hover {
              background: rgba(203, 238, 161, 0.5);
            }
            .ask-item .ask-status {
              font-size: 75%;
              opacity: 0.45;
              text-transform: lowercase;
              font-style: italic;
            }
            .ask-item button.take-back {
              display: block;
              margin-top: 0.4em;
              font-size: 75%;
              padding: 0.2em 0.7em;
              background: rgba(180, 60, 60, 0.1);
              border: 1px solid rgba(180, 60, 60, 0.3);
              color: rgb(160, 50, 50);
              cursor: pointer;
              transition: background 0.15s ease;
            }
            .ask-item button.take-back:hover {
              background: rgba(180, 60, 60, 0.2);
            }
            #pages-button {
              position: fixed;
              top: 1em;
              margin-right: 1em;
              z-index: 1;
            }
            #write-a-page {
              margin-left: 1em;
              margin-top: 1em;
              /* transition: 0.25s filter; */
            }
            #respond-button.deactivated,
            #write-a-page.deactivated {
              pointer-events: none;
              /* filter: saturate(0.5); */
            }
            #garden #top-bar {
              position: fixed;
              left: 0;
              top: 0;
              /* width: is calculated dynamically based on binding width */
              width: 100%;
              background: linear-gradient(
                to bottom,
                var(--garden-background) 25%,
                transparent 100%
              );
              z-index: 3;
              height: 72px;
              pointer-events: none;
            }

            #garden #top-bar::before {
              content: "";
              position: fixed;
              top: -300%;
              left: 0;
              width: 100%;
              height: 300%;
              background: var(--garden-background);
              z-index: 2;
            }

            #nav-editor {
              /* background: linear-gradient(to top, rgb(207 255 195 / 50%) 25%, transparent 100%); */
              background: linear-gradient(
                to top,
                rgb(207 255 195 / 50%) 25%,
                transparent 100%
              );
            }

            nav button:hover,
            #write-a-page:hover,
            /*#chat-enter:hover,*/
            #pages-button:hover,
            #chat-button:hover,
            #ask-button:hover,
            #respond-button:hover {
              background: var(--button-background-highlight);
            }
            nav button:active,
            #write-a-page:active,
            /*#chat-enter:active,*/
            #pages-button:active,
            #chat-button:active,
            #ask-button:active,
            #respond-button:active {
              filter: none; /* drop-shadow(
                        -0.035em 0.035em 0.035em rgba(40, 40, 40, 0.8)
                      ); */
              background: var(--button-active-bg);
              transform: translate(-2px, 2px);
            }
            #write-a-page {
              /* background: rgb(240, 240, 240); */
              /* border-color: rgb(40, 40, 200); */
            }
            button#publish {
              font-weight: normal;
            }
            nav button.positive {
              background: var(--positive-bg);
              border-color: var(--positive-border);
            }
            nav button.positive:hover {
              background: var(--positive-hover);
            }
            nav button.positive:active {
              background: var(--positive-active);
            }
            nav button.negative {
              background: var(--negative-bg);
              border-color: var(--negative-border);
            }
            nav button.negative:hover {
              background: var(--negative-hover);
            }
            nav button.negative:active {
              background: var(--negative-active);
            }
            nav button.ask-toggle {
              background: rgb(220, 235, 250);
              border-color: rgb(130, 170, 210);
              font-size: 90%;
            }
            nav button.ask-toggle:hover {
              background: rgb(200, 225, 250);
            }
            nav button.ask-toggle:active {
              background: rgb(190, 215, 245);
            }
            nav button.pending-toggle {
              background: rgb(255, 235, 220);
              border-color: rgb(200, 150, 100);
              font-size: 90%;
            }
            nav button.pending-toggle:hover {
              background: rgb(255, 225, 200);
            }
            nav button.pending-toggle:active {
              background: rgb(255, 215, 190);
            }
            /* Respond view within ask editor (admin) */
            .respond-view {
              padding: 1em 2em;
              height: 100%;
              overflow-y: auto;
              box-sizing: border-box;
            }
            .respond-view .respond-counter {
              font-size: 80%;
              opacity: 0.6;
              text-align: center;
              margin-bottom: 0.5em;
            }
            .respond-view .respond-handle {
              font-size: 90%;
              opacity: 0.8;
              margin-bottom: 0.5em;
              color: rgb(180, 72, 135);
            }
            .respond-view .respond-question-text {
              font-size: 100%;
              line-height: var(--line-height);
              text-align: justify;
              hyphens: auto;
              -webkit-hyphens: auto;
              padding: 0.5em;
              background: rgba(255, 240, 220, 0.5);
              border-left: 3px solid rgb(200, 150, 100);
              margin-bottom: 1em;
            }
            .respond-view .respond-label {
              font-size: 90%;
              opacity: 0.8;
              margin-bottom: 0.5em;
              color: rgb(100, 150, 180);
            }
            .respond-view .respond-textarea {
              border: none;
              font-family: var(--page-font), serif;
              font-size: 100%;
              resize: none;
              display: block;
              background: rgb(245, 250, 255);
              padding: 0.5em;
              text-align: justify;
              line-height: var(--line-height);
              height: calc(var(--line-height) * 8);
              width: 100%;
              overflow: hidden;
              hyphens: auto;
              -webkit-hyphens: auto;
              overflow-wrap: break-word;
              caret-color: rgb(50, 100, 180);
              box-sizing: border-box;
            }
            .respond-view .respond-textarea:focus {
              outline: none;
            }
            .respond-nav-btns {
              display: flex;
              justify-content: space-between;
              margin-top: 0.5em;
            }
            .respond-nav-btn {
              font-size: 90%;
              padding: 0.25em 0.5em;
            }
            .respond-nav-btn:disabled {
              opacity: 0.4;
              cursor: not-allowed;
            }

            /* 📝 Respond Editor Page Styles (Admin) */
            #respond-editor {
              position: relative;
              width: 100%;
              min-height: 100.1%;
              top: 0;
              left: 0;
              border: none;
              z-index: 4;
              padding: 0;
              display: flex;
            }
            #respond-editor-form {
              padding-top: 100px;
              padding-bottom: 72px;
              padding-left: 16px;
              padding-right: 16px;
              box-sizing: border-box;
              margin: 0 auto auto auto;
            }
            #respond-editor-page {
              aspect-ratio: 4 / 5;
              background-color: rgb(240, 248, 255);
              border: calc(max(1px, 0.1em)) solid black;
              box-sizing: border-box;
              left: 0;
              padding: 1em;
              position: absolute;
              top: 0;
              transform-origin: top left;
              width: calc(100px * 8);
              font-family: var(--page-font), serif;
              font-size: calc(2.78px * 8);
            }
            #respond-editor-page .respond-date {
              position: absolute;
              top: 6.5%;
              left: 0;
              width: 100%;
              text-align: center;
              color: black;
            }
            #respond-editor-page .respond-title {
              position: absolute;
              top: calc(6.5% + 1.5em);
              left: 0;
              width: 100%;
              text-align: center;
              color: black;
              opacity: 0.6;
              font-size: 90%;
            }
            #respond-editor-page .respond-question-section {
              margin-top: 15%;
              padding: 0 2em;
            }
            #respond-editor-page .respond-counter {
              font-size: 80%;
              opacity: 0.6;
              text-align: center;
              margin-bottom: 0.25em;
            }
            #respond-editor-page .respond-handle {
              font-size: 90%;
              opacity: 0.8;
              margin-bottom: 0.25em;
              color: rgb(180, 72, 135);
            }
            #respond-editor-page .respond-question-text {
              font-size: 100%;
              line-height: var(--line-height);
              text-align: justify;
              hyphens: auto;
              -webkit-hyphens: auto;
              padding: 0;
            }
            #respond-editor-page .respond-separator {
              border: none;
              border-top: 1px dashed rgba(0, 0, 0, 0.2);
              margin: 0.5em 0;
            }
            #respond-editor-page .respond-response-section {
              padding: 0;
            }
            #respond-editor-page #respond-words-wrapper {
              position: relative;
              touch-action: none;
              background: rgb(245, 240, 230);
            }
            #respond-editor-page #respond-words-wrapper::before {
              content: "";
              background: rgb(220, 200, 180);
              width: 2em;
              height: 100%;
              display: block;
              position: absolute;
              top: 0;
              left: 0;
              z-index: 101;
            }
            #respond-editor-page #respond-words-wrapper::after {
              content: "";
              background: rgb(220, 200, 180);
              width: 2em;
              height: 100%;
              display: block;
              position: absolute;
              top: 0;
              right: 0;
              z-index: 101;
            }
            #respond-editor-page .respond-textarea {
              border: none;
              font-family: var(--page-font), serif;
              font-size: 100%;
              resize: none;
              display: block;
              background: transparent;
              padding: 0 2em;
              text-align: justify;
              line-height: var(--line-height);
              height: calc(var(--line-height) * 12); /* Default, overridden dynamically */
              width: 100%;
              overflow: hidden;
              hyphens: auto;
              -webkit-hyphens: auto;
              overflow-wrap: break-word;
              caret-color: rgb(50, 100, 180);
              box-sizing: border-box;
              position: relative;
              z-index: 100;
            }
            #respond-editor-page .respond-textarea:focus {
              outline: none;
            }
            #respond-editor-page .page-number {
              position: absolute;
              bottom: 6.5%;
              left: 0;
              width: 100%;
              text-align: center;
              color: black;
            }
            #respond-lines-left {
              position: fixed;
              top: 0;
              left: 0;
              width: 100%;
              text-align: center;
              padding-top: 1.5em;
              padding-bottom: 1.5em;
              z-index: 6;
              background: linear-gradient(
                to bottom,
                rgb(220 235 250 / 70%) 25%,
                transparent 100%
              );
            }
            #respond-asked-by {
              position: fixed;
              top: 0;
              right: 0;
              padding-top: 1.6em;
              padding-right: 1em;
              z-index: 7;
              font-size: 75%;
              opacity: 0.6;
              color: rgb(180, 72, 135);
              font-family: sans-serif;
            }
            #nav-respond-editor {
              position: fixed;
              bottom: 0;
              left: 0;
              padding-top: 1em;
              justify-content: space-between;
              width: 100%;
              padding-left: 1em;
              padding-right: 1em;
              box-sizing: border-box;
              display: flex;
              z-index: 7;
              background: linear-gradient(
                to top,
                rgb(220 235 250 / 50%) 25%,
                transparent 100%
              );
            }
            #nav-respond-editor button:disabled {
              opacity: 0.4;
              cursor: not-allowed;
            }

            #garden {
              box-sizing: border-box;
              width: 100%;
              /*transition: 0.15s opacity;*/
              opacity: 1;
              background-color: var(--garden-background);
            }
            
            #garden-canvas {
              display: block;
              width: 100%;
              height: calc(100vh - 72px);
              margin-top: 72px;
              background-color: var(--garden-background);
            }
            
            #garden.hidden {
              display: none !important;
            }

            #garden.faded,
            #gate.faded {
              opacity: 0;
            }
            
            .page-placeholder {
              width: 100%;
              height: 100%;
              display: flex;
              justify-content: center;
              align-items: center;
              background: rgba(255,255,255,0.5);
              border: 1px dashed rgba(0,0,0,0.2);
              box-sizing: border-box;
              font-size: 0.8em;
              color: rgba(0,0,0,0.4);
            }

            #nopages {
              position: fixed;
              top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              display: flex;
              justify-content: center;
              align-items: center;
              text-align: center;
            }
            #binding {
              /* FYP-style: binding is the scroll container */
              height: 100vh;
              overflow-y: scroll;
              scroll-snap-type: y mandatory;
              scroll-behavior: smooth; /* Animate snap from current position */
              -webkit-overflow-scrolling: touch;
              overscroll-behavior: contain;
              padding-left: 16px;
              padding-right: 16px;
              margin-left: auto;
              margin-right: auto;
              box-sizing: border-box;
              /* Hide scrollbar but keep scroll functionality */
              scrollbar-width: none; /* Firefox */
              -ms-overflow-style: none; /* IE/Edge */
            }
            #binding::-webkit-scrollbar {
              display: none; /* Chrome/Safari/Opera */
            }
            #editor-form {
              padding-top: 100px;
              padding-bottom: 72px;
              padding-left: 16px;
              padding-right: 16px;
              box-sizing: border-box;
              /* opacity: 0.5; */
              margin: 0 auto auto auto;
            }
            #editor-page-wrapper {
              width: 100%;
              height: 100%;
              aspect-ratio: 4 / 5;
              position: relative;
              /* overflow: hidden; */
            }
            /*
            #editor-page {
              aspect-ratio: 4 / 5;
              background-color: white;
              border: calc(max(1px, 0.1em)) solid black;
              box-sizing: border-box;
              left: 0;
              padding: 1em;
              position: absolute;
              top: 0;
              transform-origin: top left;
              width: calc(100px * 8);
            }*/
            #editor-lines-left {
              position: fixed;
              top: 0;
              left: 0;
              width: 100%;
              text-align: center;
              padding-top: 1.5em;
              padding-bottom: 1.5em;
              z-index: 6;
              background: linear-gradient(
                to bottom,
                rgb(255 245 245 / 70%) 25%,
                transparent 100%
              );
            }
            .lines-left-loads {
              color: black;
            }
            .lines-left-lots {
              color: green;
            }
            .lines-left-little {
              color: orange;
            }
            .lines-left-few {
              color: red;
            }

            #nav-editor {
              position: fixed;
              bottom: 0;
              left: 0;
              padding-top: 1em;
              justify-content: space-between;
              width: 100%;
              padding-left: 1em;
              padding-right: 1em;
              box-sizing: border-box;
              display: flex;
              z-index: 7;
            }

            .page *::selection,
            #chat *::selection {
              background-color: var(--button-background-highlight);
              /* color: black; */
            }

            @font-face {
              font-family: "Wingdings-2";
              src: url("${assetPath}Wingdings 2.ttf") format("truetype");
              font-weight: normal;
              font-style: normal;
            }

            .fleuron {
              font-family: "Wingdings-2";
              font-size: 75%;
              vertical-align: middle;
              margin-bottom: 0.25em;
              display: inline-block;
            }

            #garden div.page-wrapper {
              /* FYP-style: full viewport height, one page at a time */
              width: 100%;
              height: 100vh;
              min-height: 100vh;
              box-sizing: border-box;
              scroll-snap-align: center;
              scroll-snap-stop: always;
              /* Flexbox to center the page-container vertically */
              display: flex;
              align-items: center;
              justify-content: center;
              padding-top: 72px; /* header offset at mobile */
            }
            @media (min-width: ${miniBreakpoint}px) {
              #garden div.page-wrapper {
                padding-top: 100px; /* header offset at desktop */
              }
            }
            
            #garden div.page-wrapper .page-container {
              width: 100%;
              aspect-ratio: 4 / 5;
              position: relative;
            }
            /* Grab cursor for dragging anywhere */
            #garden {
              cursor: grab;
            }
            #garden:active {
              cursor: grabbing;
            }
            /* Keep pointer on interactive elements */
            #garden > #binding .page-number,
            #garden .ear,
            #garden a,
            #garden button {
              cursor: pointer;
            }
            /* Editor page numbers are not interactive */
            #editor-page .page-number,
            #ask-editor-page .ask-number,
            #respond-editor-page .page-number {
              cursor: default !important;
            }
            
            /* Drag direction indicators */
            #garden.drag-up .page-container {
              border-top: 4px solid #4CAF50; /* green = will go up/prev */
            }
            #garden.drag-down .page-container {
              border-bottom: 4px solid #2196F3; /* blue = will go down/next */
            }
            #garden.drag-snap .page-container {
              /* no indicator = will snap back */
            }
            
            /* Smooth scroll behavior */
            #binding {
              scroll-behavior: smooth;
              transition: transform 0.15s ease-out;
            }

            #garden article.page,
            #editor-page,
            #print-page article.page {
              aspect-ratio: 4 / 5;
              background-color: white;
              border: calc(max(1px, 0.1em)) solid black;
              box-sizing: border-box;
              left: 0;
              padding: 1em;
              position: absolute;
              top: 0;
              transform-origin: top left;
              width: calc(100px * 8);
              user-select: text;
              -webkit-user-select: text;
            }

            #garden article.page div.page-number,
            #print-page div.page-number,
            #editor-page div.page-number {
              position: absolute;
              bottom: 5%;
              left: 50%;
              transform: translateX(-50%);
              text-align: center;
              color: black;
            }
            
            #garden article.page div.page-number:hover {
              color: rgb(180, 120, 80);
            }
            
            /* Page number tooltip with scrolling ticker */
            #page-number-tooltip {
              position: fixed;
              background: rgba(40, 30, 25, 0.95);
              color: #f5e6d3;
              padding: 0.5em 1em;
              border-radius: 0.5em;
              font-size: 12px;
              max-width: 200px;
              white-space: nowrap;
              overflow: hidden;
              pointer-events: none;
              z-index: 1000;
              opacity: 0;
              transition: opacity 0.15s ease;
              transform: translate(-50%, -100%);
              margin-top: -8px;
              border: 1px solid rgba(200, 150, 100, 0.3);
              box-shadow: 0 4px 12px rgba(0,0,0,0.3);
            }
            #page-number-tooltip.visible {
              opacity: 1;
            }
            #page-number-tooltip .ticker {
              display: inline-block;
              animation: ticker-scroll 8s linear infinite;
            }
            @keyframes ticker-scroll {
              0% { transform: translateX(100%); }
              100% { transform: translateX(-100%); }
            }

            #garden article.page div.page-title,
            #print-page div.page-title,
            #editor-page div.page-title {
              position: absolute;
              top: 6.5%;
              left: 0;
              width: 100%;
              text-align: center;
              color: black;
              /* background: red; */
            }

            /* The default template for page styling. */
            #garden article.page.page-style-a {
              /* background: yellow; */
            }

            #garden article.page .words,
            #print-page article.page .words {
              margin: 0;
              text-align: justify;
              line-height: var(--line-height);
              margin-top: 15%;
              max-height: calc(var(--line-height) * var(--max-lines));
              overflow: hidden;
              padding: 0 2em;
              hyphens: auto;
              -webkit-hyphens: auto;
              overflow-wrap: break-word;
              white-space: pre-wrap;
            }

            #garden article.page .words.justify-last-line::after,
            #print-page article.page .words.justify-last-line::after {
              content: "";
              display: inline-block;
              width: 100%;
            }

            #editor-page.editor-justify-last-line #editor-measurement::after {
              content: "";
              display: inline-block;
              width: 100%;
            }

            /* ✏️️📄 Page Editor */
            #garden #editor {
              /* position: fixed; */
              position: relative;
              width: 100%;
              /* height: 100%; */
              min-height: 100.1%;
              top: 0;
              left: 0;
              border: none;
              z-index: 4;
              padding: 0;
              display: flex;
            }

            #garden #editor-placemat /* #editor::before */ {
              content: "";
              position: fixed;
              top: 0;
              left: 0;
              width: 100%;
              height: 100vh;
              /* overflow: hidden; */
              background: color-mix(
                in srgb,
                var(--garden-background) 50%,
                var(--editor-placemat-background-opaque) 50%
              ) !important;
              /* background: linear-gradient(
                to top,
                rgba(255, 255, 255, 0.5) 99%,
                transparent 100%
              ); */
              z-index: 3;
              pointer-events: none;
            }

            #garden #editor #words-wrapper::before {
              content: "";
              background: rgb(255, 245, 245, 1);
              width: 2em;
              height: 100%;
              display: block;
              position: absolute;
              top: 0;
              left: 0;
              z-index: 101;
            }

            #garden #editor #words-wrapper::after {
              content: "";
              background: rgb(255, 245, 245, 1);
              width: 2em;
              height: 100%;
              display: block;
              position: absolute;
              top: 0;
              right: 0;
              z-index: 101;
            }

            #garden #editor textarea {
              border: none;
              /* font-family: var(--font-page); */
              font-size: 100%;
              resize: none;
              display: block;
              background: rgb(255, 250, 250);
              margin-top: 15%;
              /* margin-top: 14.5%; */
              padding: 0 2em;
              text-indent: 0em;
              text-align: justify;
              /* text-align-last: justify; */
              line-height: var(--line-height);
              /* transform-origin: top left; */
              height: calc(var(--line-height) * var(--max-lines));
              width: 100%;
              overflow: hidden;
              position: relative;

              /* word-break: break-word; */
              hyphens: auto;
              -webkit-hyphens: auto;
              overflow-wrap: break-word;
              /* z-index: 1; */
            }

            #garden #editor #words-wrapper.invisible textarea {
              opacity: 0;
            }

            #garden #editor #words-wrapper.invisible.hover {
              background: rgb(255, 245, 170, 0.25);
              cursor: text;
            }

            #garden #editor #words-wrapper.invisible.active {
              background: rgb(255, 150, 150, 0.125);
              cursor: text;
            }

            #garden #editor #words-wrapper.invisible:before,
            #garden #editor #words-wrapper.invisible:after {
              opacity: 0;
            }

            #garden #editor #words-wrapper.invisible.hover::before {
              display: none;
            }

            #garden #editor #words-wrapper.invisible.hover::after {
              display: block;
              content: "";
              position: absolute;
              top: 0;
              left: 2em;
              width: calc(100% - 4em);
              height: 100%;
              background: rgb(255, 245, 170, 0.5);
              pointer-events: none;
              z-index: 0;
              opacity: 1;
            }

            #garden #editor #words-wrapper.invisible.active::after {
              display: block;
              content: "";
              position: absolute;
              top: 0;
              left: 2em;
              width: calc(100% - 4em);
              height: 100%;
              background: rgb(255, 255, 150, 0.6);
              pointer-events: none;
              z-index: 0;
              opacity: 1;
            }

            #garden #editor #words-wrapper {
              position: relative;
              touch-action: none;
            }

            #garden #editor textarea {
              pointer-events: none;
              /* caret-color: black; */
              caret-color: var(--spinner-background);
            }

            #garden #editor textarea:focus {
              pointer-events: auto;
              outline: none;
            }

            /* 🐕 Doggy Ear Rendering */
            #garden .page-wrapper div.ear {
              width: 15%; /* rounded by js */
              background: transparent;
              position: absolute;
              box-sizing: border-box;
              z-index: 1;
              touch-action: none;
              -webkit-tap-highlight-color: transparent;
            }

            #garden .page-wrapper div.ear.hover,
            #garden .page-wrapper div.ear.active {
              cursor: pointer;
              background: var(--garden-background);
              border-left: calc(max(1px, 0.1em)) solid black;
              border-top: calc(max(1px, 0.1em)) solid black;
            }

            #garden .page-wrapper div.ear.hover::before,
            #garden .page-wrapper div.ear.active::before {
              content: "";
              position: absolute;
              bottom: 0;
              right: 0;
              width: 100%;
              height: 100%;
              background: black;
              clip-path: polygon(0 0, calc(100%) 0, 0 calc(100%));
              z-index: 1;
              user-select: none;
              -webkit-user-select: none;
              pointer-events: none;
            }

            #garden .page-wrapper div.ear.hover::after,
            #garden .page-wrapper div.ear.active::after {
              content: "";
              position: absolute;
              bottom: 0;
              right: 0;
              width: 100%;
              height: 100%;
              background: var(--backpage-color);
              clip-path: polygon(
                0 0,
                calc(100% - calc(max(1px, 0.1em))) 0,
                0 calc(100% - calc(max(1px, 0.1em)))
              );
              z-index: 2;
              user-select: none;
              -webkit-user-select: none;
              pointer-events: none;
            }

            #garden .page-wrapper div.ear.active::after {
              background: rgb(240, 240, 240) !important;
            }

            #garden .page-wrapper div.ear.reverse.hover::after,
            #garden .page-wrapper div.ear.reverse.active::after {
              background: white;
            }

            #garden .page-wrapper {
              /* overflow: hidden; */
              box-sizing: border-box;
            }

            #garden .page-wrapper.reverse {
              /* border: 0.1em solid black; */
            }

            #garden article.page.reverse {
              filter: blur(2px);
              overflow: hidden;
              border: none;
            }

            #garden article.page.reverse .page-title,
            #garden article.page.reverse .words,
            #garden article.page.reverse .page-number {
              transform: scaleX(-1);
            }

            /* 📃 Backpage */
            #garden .page-wrapper .backpage {
              /* font-family: var(--font-page); */
              width: 100%;
              height: 100%;
              background: var(--backpage-color-translucent);
              position: absolute;
              /* z-index: 0; */
              padding-left: 3em;
              padding-right: 3em;
              padding-top: calc(1em + 15%);
              /* background: yellow; */
              line-height: var(--line-height);
              hyphens: auto;
              -webkit-hyphens: auto;
              text-align: justify;
              overflow-wrap: break-word;
              top: 0;
              left: 0;
              border: 0.1em solid black;
              box-sizing: border-box;
            }
            #garden .page-wrapper .backpage p {
              margin-top: -0.4em;
            }

            .byline {
              /* Same as .page-title - 24.10.11.04.14 */
              position: absolute;
              top: 6%;
              left: 3em;
              width: 100%;
              text-align: left;
              /* line-height: 1.6em; */
              /* color: black; */
            }

            .crumple-this-page {
              position: absolute;
              bottom: 4.45%;
              left: 3em;
              color: black;
            }

            .crumple-this-page:hover {
              color: var(--destructive-red);
            }

            .crumple-this-page:active {
              color: red;
            }

            .share-this-page {
              position: absolute;
              bottom: calc(4.45% + 7.6% + var(--line-height));
              right: calc(3em);
              color: black;
            }

            .print-this-page {
              position: absolute;
              bottom: calc(4.45% + 7.6%);
              right: 3em;
              color: black;
            }

            #print-page-wrapper {
              position: fixed;
              display: flex;
              z-index: 10;
              width: 100vw;
              height: 100vh;
            }

            #print-page {
              width: 100%;
              min-height: 100%;
              background: rgba(255, 255, 0, 0.5);
              margin: auto;
              display: flex;
            }

            #print-page article {
              margin: auto;
            }

            .share-this-page:hover,
            .print-this-page:hover {
              color: rgb(0, 0, 200);
              z-index: 5;
            }

            .share-this-page:active,
            .print-this-page:active {
              color: blue;
            }

            #email {
              position: relative;
              color: var(--link-color);
            }
            #email.admin::after,
            .crumple-this-page::after {
              content: "⬤";
              font-size: 25%;
              position: absolute;
              color: var(--spinner-background);
              top: -0.25em;
              right: -1.5em;
              opacity: 0.75;
            }
            .crumple-this-page::after {
              top: -0.9em;
              right: -1.75em;
            }
            #email:hover {
              color: var(--chat-handle);
            }
            #email:active {
              -webkit-tap-highlight-color: transparent;
              color: var(--positive-border);
            }
            #delete-account,
            #privacy-policy {
              color: var(--link-color);
              position: absolute;
              font-size: 80%;
              bottom: -15%;
              user-select: none;
              left: 50%;
              transform: translateX(-51.5%);
              white-space: nowrap;
            }

            #subscriber-count {
              color: var(--gate-text);
              position: absolute;
              font-size: 80%;
              bottom: -15%;
              user-select: none;
              -webkit-user-select: none;
              left: 50%;
              transform: translateX(-52%);
              /* background: yellow; */
              white-space: nowrap;
              text-align: center;
            }

            #delete-account {
              /* left: calc(-132% / 8); */
              /* width: 132%; */
            }
            #delete-account:hover {
              color: var(--destructive-red);
            }
            #delete-account:active {
              color: red;
            }
            #privacy-policy {
              /* 'width' and 'left' value calculated in js 'genSubscribeButton' */
            }
            #privacy-policy:hover {
              color: var(--chat-link-hover);
            }
            #privacy-policy:active {
              color: var(--chat-link);
            }
            #logout-wrapper,
            #imnew-wrapper,
            #secondary-wrapper {
              position: relative;
            }
            #cookie-menu {
              position: absolute;
              top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              user-select: none;
              -webkit-user-select: none;
              cursor: pointer;
              background-color: var(--pink-border);
              mask-size: 100% 100%;
              mask-position: center;
              mask-repeat: no-repeat;
              -webkit-mask-size: 100% 100%;
              -webkit-mask-position: center;
              -webkit-mask-repeat: no-repeat;
              -webkit-tap-highlight-color: transparent;
              touch-action: none;
              pointer-events: all;
              /* Apply drop shadow here instead of wrapper to avoid transform artifacts */
              filter: drop-shadow(-2px 0px 1px rgba(0, 0, 0, 0.25));
            }
            #cookie-menu-wrapper {
              position: absolute;
              top: 0.225em;
              right: 0.25em;
              width: 90px;
              height: 90px;
              z-index: 2;
              cursor: pointer;
              -webkit-tap-highlight-color: transparent;
              /* Promote to GPU layer */
              transform: translateZ(0);
              backface-visibility: hidden;
            }
            #cookie-menu-wrapper:hover #cookie-menu {
              transform: scale(0.97);
              transition: transform 0.2s ease-out;
            }
            #cookie-menu-wrapper #cookie-menu {
              transition: transform 0.15s ease-in;
            }
            #cookie-menu-wrapper:active #cookie-menu {
              transform: scale(0.94);
              transition: transform 0.13s ease-out;
            }
            #cookie-menu-wrapper.nogarden #cookie-menu {
              /* Different shadow color when not in garden */
              filter: drop-shadow(-2px 0px 1px rgba(0, 0, 0, 0.25));
            }
            #cookie-menu-img {
              /* Used to generate the mask dynamically */
              position: absolute;
              top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              visibility: hidden;
              pointer-events: none;
            }
            @media (max-width: ${miniBreakpoint}px) {
              #cookie-menu-wrapper {
                width: 60px;
                height: 60px;
                top: 0.525em;
                right: 0.5em;
              }
              #garden #top-bar {
                height: 64px;
              }
              #binding {
                padding-top: 72px;
              }
              #gate {
                max-width: none;
                transform: scale(0.75);
              }
            }
            #prompt {
              font-size: 22px;
              font-family: monospace;
              border: none;
              background: none;
              position: absolute;
              top: 16px;
              left: 16px;
              color: black;
              user-select: none;
              -webkit-user-select: none;
              cursor: pointer;
            }
            #prompt:hover {
              color: rgb(180, 72, 135);
            }
            #editor-measurement.invisible {
              opacity: 0;
              /* visibility: hidden; */
            }
            .hidden {
              visibility: hidden;
              pointer-events: none;
              overflow: hidden;
            }
            .obscured {
              display: none !important;
              pointer-events: none;
            }
            .loading-dots::after {
              content: "\\00a0\\00a0\\00a0"; /* Three non-breaking spaces */
              animation: loading-dots 1s steps(3, end) infinite;
            }
            @keyframes loading-dots {
              0% {
                content: "\\00a0\\00a0\\00a0";
              } /* Empty spaces */
              33% {
                content: ".\\00a0\\00a0";
              } /* One dot, two spaces */
              66% {
                content: "..\\00a0";
              } /* Two dots, one space */
              90% {
                content: "...";
              } /* Three dots */
            }
            #veil {
              position: fixed;
              z-index: 300;
              top: -50%;
              left: 0;
              width: 100%;
              height: 200%;
              display: flex;
              /* background: black; */
              background: rgba(0, 0, 0, 0.75);
              opacity: 1;
              /* transition: 0.5s background; */
            }
            #veil.unveiled {
              background: rgba(0, 0, 0, 0);
              /* transition: 0.5s background; */
              pointer-events: none;
            }
            #veil.unveiled-instant {
              transition: none;
              /* opacity: 0; */
              background: rgba(0, 0, 0, 0);
              pointer-events: none;
            }
            #chat {
              /* display: none; */
              top: 0;
              left: 0;
              position: absolute;
              z-index: 2; /* This may be wrong. 24.11.05.22.43 */
              width: 100%;
              height: 100%;
              height: 100dvh;
              --chat-input-height: 2.65em;
              --chat-enter-width: 5em;
              --chat-input-border-color: rgb(130, 100, 100);
              /* --chat-input-border-color: var(--chat-input-bar-background); */
              display: flex;
              flex-direction: column;
              overflow: hidden;
              background: var(--chat-background);
              /* opacity: 0.7; */
            }
            #chatter-count {
              display: none;
              position: fixed;
              color: black;
              top: 0.35em;
              left: 0.35em;
              z-index: 3;
            }
            #chat.inaccessible {
              transition: 1.5s opacity;
            }
            #chat.inaccessible .message {
              pointer-events: none;
            }
            #chat-disconnected {
              width: 100%;
              height: 100%;
              background: rgba(0, 0, 0, 0.25);
              position: fixed;
              top: 0;
              left: 0;
              z-index: 1;
              color: white;
              text-align: center;
              line-height: 100vh;
            }
            #chat.inaccessible #chat-disconnected,
            #chat.inaccessible #chatter-count,
            #chat.inaccessible #chat-message-empty,
            #chat.inaccessible #pages-button {
              display: none;
            }
            #chat.inaccessible #chat-messages {
              flex-direction: column;
              padding-bottom: 0;
              min-height: 100%;
            }
            #chat-messages {
              flex: 1;
              overflow-y: auto;
              overflow-x: hidden;
              display: flex;
              flex-direction: column;
            }
            #chat-messages div.message:first-child {
              margin-top: auto;
            }
            #chat-messages div.message {
              border-bottom: 1.5px solid var(--chat-message-border);
              box-sizing: border-box;
              padding: 0.25em 0.5em;
              line-height: 1.25em;
              /* font-size: 85%; */
            }
            #chat-messages div.message div.message-author {
              font-weight: bold;
              display: inline-block;
              color: var(--chat-handle);
              padding-right: 0.25em;
              user-select: text;
              cursor: pointer;
            }
            #chat-messages div.message div.message-author:hover {
              text-decoration: underline;
            }
            #chat-messages div.message div.message-content {
              display: inline-block;
              color: var(--chat-text);
              user-select: text;
              word-wrap: break-word;
              max-width: calc(100% - 0.5em);
            }
            #chat-messages div.message div.message-content a {
              color: var(--chat-link);
              text-decoration: underline;
            }
            #chat-messages div.message div.message-content a:hover {
              color: var(--chat-link-hover);
            }
            #chat-messages div.message div.message-content .handle-mention {
              color: var(--chat-handle);
              font-weight: bold;
              cursor: pointer;
            }
            #chat-messages div.message div.message-content .handle-mention:hover {
              text-decoration: underline;
            }
            #chat-messages div.message div.message-content .page-link {
              font-weight: bold;
              cursor: pointer;
              text-decoration: none;
              display: inline;
            }
            #chat-messages div.message div.message-content .diary-link {
              color: var(--chat-diary-link);
            }
            #chat-messages div.message div.message-content .question-link {
              color: var(--chat-question-link);
            }
            #chat-messages div.message div.message-content .page-link:hover {
              text-decoration: none;
              opacity: 0.7;
            }
            #page-preview {
              position: fixed;
              width: 120px;
              aspect-ratio: 4 / 5;
              background: var(--card-background);
              color: var(--chat-text);
              border: 1px solid var(--card-border);
              box-shadow: 0 4px 12px rgba(0,0,0,0.3);
              pointer-events: none;
              z-index: 1000;
              padding: 8px;
              box-sizing: border-box;
              font-size: 6px;
              line-height: 1.3;
              overflow: hidden;
              opacity: 0;
              transition: opacity 0.15s ease;
              transform: translate(-50%, -100%);
            }
            #page-preview.visible {
              opacity: 1;
            }
            #page-preview .preview-title {
              font-weight: bold;
              margin-bottom: 4px;
              font-size: 5px;
              opacity: 0.6;
            }
            #page-preview .preview-content {
              overflow: hidden;
              text-overflow: ellipsis;
            }
            #page-preview .preview-number {
              position: absolute;
              bottom: 4px;
              right: 6px;
              font-size: 5px;
              opacity: 0.5;
            }
            #chat-messages div.message div.message-when {
              color: var(--chat-timestamp);
              opacity: 1;
              display: inline-block;
              font-size: 75%;
              padding-left: 0.5em;
              transition: opacity 0.15s ease;
            }
            #chat-messages div.message:hover div.message-when {
              opacity: 1;
              color: var(--chat-text);
            }
            #chat-input-bar {
              /* width: 100%; */ /* Set in JavaScript */
              /* background: var(--chat-background); */
              background: var(--chat-input-bar-background);
              min-height: var(--chat-input-height);
              display: flex;
              align-items: center;
              flex-shrink: 0;
              overflow: hidden;
              border-top: 2px solid rgba(0, 0, 0, 0.1);
              padding-bottom: 1em;
              padding-top: 0.35em;
              padding-left: 0.5em;
              padding-right: 0.5em;
              gap: 0.5em;
            }
            #chat-input-bar.sending * {
              pointer-events: none;
              opacity: 0.5;
            }
            #chat-handle {
              display: flex;
              align-items: center;
              font-weight: bold;
              padding: 0;
              margin-left: 0;
              color: var(--chat-handle);
            }
            #chat-input-container {
              flex: 1;
              height: var(--chat-input-height);
              display: flex;
              align-items: center;
              border-radius: 0.5em;
              border: 0.205em solid var(--pink-border);
              box-sizing: border-box;
              background: var(--chat-input-bg);
              position: relative;
              overflow: hidden;
              filter: drop-shadow(-0.065em 0.065em 0.065em var(--chat-shadow));
            }
            #chat-input-container .monaco-editor {
              position: absolute !important;
              top: 0;
              left: 0;
              right: 0;
              bottom: 0;
            }
            #chat-input-container .monaco-editor .view-lines {
              padding-left: 0.5em !important;
              padding-top: 0.5em !important;
            }
            #chat-input-container .monaco-editor .cursors-layer {
              padding-left: 0.5em !important;
              padding-top: 0.5em !important;
            }
            #chat-input-container .monaco-editor,
            #chat-input-container .monaco-editor .view-line {
              font-family: var(--page-font), sans-serif !important;
            }
            /* Hide Monaco's mobile keyboard toggle button (SVG keyboard icon) */
            #chat-input-container .monaco-editor .iPadShowKeyboard,
            #chat-input-container .monaco-editor .codicon-keyboard,
            #chat-input-container .monaco-editor .monaco-editor-overlaymessage,
            #chat-input-container .monaco-editor .accessibilityHelpWidget {
              display: none !important;
            }
            /* Ensure Monaco's hidden textarea is accessible for mobile keyboard */
            #chat-input-container .monaco-editor .inputarea {
              opacity: 0 !important;
              height: 1em !important;
              font-size: 16px !important; /* Prevents iOS zoom on focus */
            }
            #chat-input {
              flex: 1;
              height: 100%;
              margin: auto 0;
              border-radius: 0;
              border: 2px solid var(--pink-border);
              box-sizing: border-box;
              font-size: 100%;
              padding: 0.35em 0.5em;
              background: var(--chat-input-bg);
              color: var(--chat-input-text);
            }
            #chat-input:focus {
              outline: none;
            }
            #chat-enter {
              display: flex;
              align-items: center;
              justify-content: center;
              min-width: var(--chat-enter-width);
              font-size: 100%;
              padding: 0.35em 0.75em;
              border: 0.205em solid var(--pink-border);
              box-sizing: border-box;
              color: var(--button-text);
              background-color: var(--button-background);
              cursor: pointer;
              border-radius: 0.5em;
              filter: drop-shadow(-0.065em 0.065em 0.065em var(--chat-shadow));
              user-select: none;
              -webkit-user-select: none;
              -webkit-tap-highlight-color: transparent;
              touch-action: manipulation;
            }
            #chat-enter:hover {
              background-color: var(--button-background-highlight);
            }
            #chat-enter:active {
              background-color: var(--button-background-highlight);
              filter: drop-shadow(-0.03em 0.03em 0.03em var(--chat-shadow));
            }
            #chat-autocomplete {
              position: absolute;
              bottom: calc(100% + 0.25em);
              left: 0;
              background: var(--chat-autocomplete-bg);
              border: 0.205em solid var(--pink-border);
              border-radius: 0.5em;
              max-height: 150px;
              overflow-y: auto;
              display: none;
              z-index: 100;
              min-width: 100px;
            }
            #chat-autocomplete.visible {
              display: block;
            }
            #chat-autocomplete .autocomplete-item {
              padding: 0.5em 0.75em;
              cursor: pointer;
              color: var(--chat-handle);
              font-weight: bold;
              -webkit-tap-highlight-color: transparent;
              touch-action: manipulation;
            }
            #chat-autocomplete .autocomplete-item:hover,
            #chat-autocomplete .autocomplete-item.selected {
              background: var(--chat-autocomplete-selected);
            }
            @media (hover: none) {
              #chat-autocomplete .autocomplete-item:active {
                background: var(--button-background-highlight);
              }
            }
            #chat-messages-veil {
              width: 100%;
              display: block;
              top: 0;
              left: 0;
              position: fixed;
              background: linear-gradient(
                to bottom,
                var(--chat-background) 0%,
                transparent 100%
              );
              opacity: 0.9;
              height: 150px;
              z-index: 1;
            }
            #chat.hidden {
              opacity: 0;
            }
            #chat.inaccessible #chat-input-bar {
              display: none;
            }
            #chat.inaccessible #chat-messages {
              padding-bottom: 0;
              opacity: 0.25;
            }
            #chat.inaccessible.splash-chat-open #chat-messages {
              padding-bottom: 0;
              opacity: 1;
            }
            #chat.inaccessible.splash-chat-open #chatter-count {
              display: inline-block;
            }
            #chat.inaccessible.splash-chat-open .message {
              pointer-events: all;
            }
            #chat.inaccessible.splash-chat-open #chat-message-empty {
              display: inline-block;
            }
            #gate {
              z-index: 2;
            }
            #garden {
              z-index: 1;
            }
            #spinner {
              z-index: 3;
            }
          </style>
          ${dev ? reloadScript : ""}
          <script
            crossorigin="anonymous"
            src="/aesthetic.computer/dep/auth0-spa-js.production.js"
          ></script>
          <script src="https://js.stripe.com/v3/"></script>
          <script src="https://cdn.jsdelivr.net/npm/monaco-editor@0.52.0/min/vs/loader.min.js"></script>
          ${!dev ? analyticsScript : ""}
        </head>
        <body>
          <div id="wrapper">
            <div id="spinner"></div>
          </div>
          <div id="veil" class="unveiled"></div>
          <img class="asset" src="${assetPath}cookie-open.png" />
          <!-- suppress asset preload warning 24.10.14.22.21 -->
          <img class="asset" src="${assetPath}cookie.png" />
          <script type="module">
            // 🗺️ Environment
            const dev = ${dev};
            const fromAesthetic =
              (document.referrer.indexOf("aesthetic") > -1 ||
                document.referrer.indexOf("localhost") > -1) &&
              document.referrer.indexOf("sotce-net") === -1;
            const embedded = window.self !== window.top;

            const url = new URL(window.location);
            const path = dev
              ? url.pathname.replace("/sotce-net", "")
              : url.pathname;

            if (path.length > 0) console.log("🏡 Path:", path);

            // Update the URL path without affecting back button history.
            function updatePath(name) {
              name = name === "/" ? "" : name;
              const url = new URL(window.location.href);
              if (dev) {
                url.pathname = "/sotce-net" + name;
              } else {
                url.pathname = name;
              }
              history.replaceState(null, "", url);

              if (name === "") {
                document.title = "Sotce Net";
              } else {
                document.title = name.replace("/", "") + " · Sotce Net";
              }
            }

            const cel = (el) => document.createElement(el); // shorthand
            let fullAlert;
            let waitForSubscriptionSuccessThreeTimes = false;
            const { round, abs, floor, ceil, min, max } = Math;

            // Platform constants.
            // From aesthetic-computer's 'platform.mjs'.
            const nav = navigator;
            export const iOS = /(iPad|iPhone|iPod)/g.test(nav.userAgent);
            export const Safari = /apple/i.test(nav.vendor);
            export const Android = /(Android)/g.test(nav.userAgent);
            export const MetaBrowser = /(OculusBrowser)/g.test(nav.userAgent);
            export const Desktop = !iOS && !Android && !MetaBrowser;
            export const Instagram = /(Instagram)/g.test(nav.userAgent);
            export const TikTok = /BytedanceWebview/i.test(nav.userAgent);

            const wrapper = document.getElementById("wrapper");
            let scrollMemory = wrapper.scrollTop; // Used to retrieve scroll across gate and garden.

            // 🗨️ Chat
            import { Chat } from "/aesthetic.computer/lib/chat.mjs";
            const chat = new Chat(dev, undefined, function disconnect() {
              chatDisconnected.classList.remove("hidden");
            });
            
            // Global chat button reference (set when garden renders)
            let chatButtonRef = null;
            
            // Helper to open chat with optional prefilled message
            function openChatWithMessage(message) {
              if (chatButtonRef) {
                chatButtonRef.click();
                if (message) {
                  setTimeout(() => {
                    chatInput.value = message;
                    chatInput.focus();
                    // Move cursor to end of input
                    const len = message.length;
                    chatInput.setSelectionRange(len, len);
                    // Force focus again after a tick to ensure cursor is visible
                    requestAnimationFrame(() => {
                      chatInput.focus();
                      chatInput.setSelectionRange(len, len);
                    });
                  }, 100);
                }
              }
            }

            chat.connect("sotce"); // Connect to 'sotce' chat.

            const chatInterface = cel("div"); // 🪟 Interface
            chatInterface.id = "chat";
            chatInterface.classList.add("hidden");
            chatInterface.classList.add("inaccessible");

            const chatDisconnected = cel("div");
            const chatDisconnectedMessage = cel("div");
            chatDisconnected.id = "chat-disconnected";
            chatDisconnected.appendChild(chatDisconnectedMessage);
            chatDisconnectedMessage.innerText = "Connecting";
            chatDisconnectedMessage.classList.add("loading-dots");
            chatDisconnected.classList.add("hidden");

            const chatMessages = cel("div"); // Scrolling panel for messages.
            chatMessages.id = "chat-messages";
            
            // Event delegation for clicking @mentions and page links in message content
            chatMessages.addEventListener("click", (e) => {
              if (e.target.classList.contains("handle-mention")) {
                const handle = e.target.innerText;
                chatInput.value = chatInput.value + handle + " ";
                chatInput.focus();
                chatInput.setSelectionRange(chatInput.value.length, chatInput.value.length);
              }
              // Handle page link clicks (navigate within SPA)
              if (e.target.classList.contains("page-link")) {
                e.preventDefault();
                const href = e.target.getAttribute("href");
                // Close chat and navigate to page
                const chatPagesBtn = document.getElementById("pages-button");
                if (chatPagesBtn) chatPagesBtn.click(); // Close chat via pages button
                updatePath(href);
                // Scroll to target page
                const pageMatch = href.match(/^\\/page\\/(\\d+)$/);
                const qMatch = href.match(/^\\/q\\/(\\d+)$/);
                if (pageMatch) {
                  const targetPage = document.getElementById("page-" + pageMatch[1]);
                  if (targetPage) targetPage.scrollIntoView({ block: "start", behavior: "smooth" });
                } else if (qMatch) {
                  const targetQ = document.getElementById("q-" + qMatch[1]);
                  if (targetQ) targetQ.scrollIntoView({ block: "start", behavior: "smooth" });
                }
              }
            });
            
            // Page preview tooltip on hover
            const pagePreview = cel("div");
            pagePreview.id = "page-preview";
            document.body.appendChild(pagePreview);
            
            let previewTimeout;
            chatMessages.addEventListener("mouseover", async (e) => {
              if (e.target.classList.contains("page-link")) {
                const href = e.target.getAttribute("href");
                const pageMatch = href.match(/^\\/page\\/(\\d+)$/);
                if (pageMatch) {
                  const pageNum = parseInt(pageMatch[1], 10);
                  clearTimeout(previewTimeout);
                  
                  // Position preview above the link
                  const rect = e.target.getBoundingClientRect();
                  pagePreview.style.left = (rect.left + rect.width / 2) + "px";
                  pagePreview.style.top = (rect.top - 8) + "px";
                  
                  // Try to get page content from cache or DOM
                  const pageEl = document.getElementById("page-" + pageNum);
                  if (pageEl && pageEl.dataset.loaded === "true") {
                    const words = pageEl.querySelector(".words");
                    const title = pageEl.querySelector(".page-title");
                    if (words && title) {
                      pagePreview.innerHTML = 
                        '<div class="preview-title">' + title.innerText + '</div>' +
                        '<div class="preview-content">' + words.innerText.slice(0, 200) + '</div>' +
                        '<div class="preview-number">-' + pageNum + '-</div>';
                      pagePreview.classList.add("visible");
                    }
                  } else {
                    // Try from cache
                    const cached = await getCachedPage(pageNum);
                    if (cached) {
                      const opts = { weekday: "long", month: "long", day: "numeric" };
                      const previewDate = new Date(cached.when).toLocaleDateString("en-US", opts);
                      pagePreview.innerHTML = 
                        '<div class="preview-title">' + previewDate + '</div>' +
                        '<div class="preview-content">' + cached.words.slice(0, 200) + '</div>' +
                        '<div class="preview-number">-' + pageNum + '-</div>';
                      pagePreview.classList.add("visible");
                    }
                  }
                }
              }
            });
            
            chatMessages.addEventListener("mouseout", (e) => {
              if (e.target.classList.contains("page-link")) {
                previewTimeout = setTimeout(() => {
                  pagePreview.classList.remove("visible");
                }, 100);
              }
            });

            const chatMessagesVeil = cel("div");
            chatMessagesVeil.id = "chat-messages-veil";
            chatInterface.appendChild(chatMessagesVeil);

            chatInterface.appendChild(chatDisconnected);

            const chatPagesButton = cel("button");
            chatPagesButton.id = "pages-button";
            chatPagesButton.innerText = "pages";
            chatInterface.appendChild(chatPagesButton);

            const chatterCount = cel("div");
            chatterCount.id = "chatter-count";
            chatterCount.innerText = "Online: " + 0;
            chatInterface.appendChild(chatterCount);

            // Populate some test messages.
            const chatMessageData = [];

            function chatScrollToBottom(options = { always: false }) {
              const currentScroll = chatMessages.scrollTop;
              const toBottom =
                chatMessages.scrollHeight - chatMessages.clientHeight;
              if (options.always === true || currentScroll !== toBottom) {
                chatMessages.scrollTop = toBottom;
              }
            }

            function clearChatMessages() {
              chatMessages.replaceChildren();
            }

            function chatterCountUpdate() {
              chatterCount.innerText = "Online: " + chat.system.chatterCount;
            }

            const chatWhenFormatter = new Intl.DateTimeFormat("en-US", {
              weekday: "long",
              year: "numeric",
              month: "long",
              day: "numeric",
              hour: "numeric",
              minute: "numeric",
              second: "numeric",
            });

            // Auto-link URLs, highlight @handles, and link page references in text
            function linkifyText(text) {
              const urlRegex = new RegExp('(https?:\\\\/\\\\/[^\\\\s<>"\\']+)', 'gi');
              let result = text.replace(urlRegex, (url) => {
                // Escape HTML in the URL for safety
                const safeUrl = url.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
                return '<a href="' + safeUrl + '" target="_blank" rel="noopener noreferrer">' + safeUrl + '</a>';
              });
              // Highlight @handles
              const handleRegex = new RegExp('(@[a-zA-Z0-9_.-]+)', 'g');
              result = result.replace(handleRegex, '<span class="handle-mention">$1</span>');
              // Link diary page references like -5-
              const diaryPageRegex = new RegExp('-(\\\\d+)-', 'g');
              result = result.replace(diaryPageRegex, '<a href="/page/$1" class="page-link diary-link">-$1-</a>');
              // Link question references like *3*
              const questionRegex = new RegExp('\\\\*(\\\\d+)\\\\*', 'g');
              result = result.replace(questionRegex, '<a href="/q/$1" class="page-link question-link">*$1*</a>');
              return result;
            }

            function chatAddMessage(text, handle, when, count) {
              const msg = cel("div");
              msg.classList.add("message");
              msg.dataset.when = when; // Store timestamp for recency calculations
              
              const by = cel("div");
              by.classList.add("message-author");
              const txt = cel("div");
              txt.classList.add("message-content");
              const date = cel("div");
              date.classList.add("message-when");
              // Add count multiplier if message was repeated
              const displayText = count > 1 ? text + " x" + count : text;
              // Auto-link URLs and escape HTML for non-URL parts
              const escapedText = displayText
                .replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;');
              txt.innerHTML = linkifyText(escapedText);
              by.innerText = handle;
              by.addEventListener("click", () => {
                chatInput.value = chatInput.value + handle + " ";
                chatInput.focus();
              });
              updateSeenHandles(handle); // Track handle for autocomplete
              date.innerText = chatWhenFormatter.format(new Date(when));
              msg.appendChild(by);
              msg.appendChild(txt);
              msg.appendChild(date);
              chatMessages.appendChild(msg);
              // Store message ID for potential updates
              msg.dataset.messageId = chatMessages.children.length - 1;
              // If there are more than 500 children, remove the first (oldest) message.
              while (chatMessages.children.length > 500) {
                chatMessages.removeChild(chatMessages.firstChild);
              }
              // Update position-based fading for all messages
              updateMessageFading();
            }
            
            // Update opacity of borders and timestamps based on position from bottom
            // Most recent 30 messages are visible, then fade out slowly
            function updateMessageFading() {
              const messages = chatMessages.querySelectorAll(".message");
              const total = messages.length;
              const fadeStart = 30; // Start fading after this many messages from bottom
              const fadeLength = 20; // Fade over this many messages
              
              messages.forEach((msg, i) => {
                const posFromBottom = total - 1 - i;
                let opacity;
                if (posFromBottom < fadeStart) {
                  opacity = 0.15; // Full opacity for recent messages
                } else {
                  const fadeProgress = (posFromBottom - fadeStart) / fadeLength;
                  opacity = Math.max(0, 0.15 * (1 - fadeProgress));
                }
                msg.style.setProperty('--msg-border-opacity', opacity.toFixed(3));
                const whenEl = msg.querySelector(".message-when");
                if (whenEl) whenEl.style.opacity = opacity.toFixed(3);
              });
            }

            function chatAddEmpty() {
              const msg = cel("div");
              msg.classList.add("message");
              msg.innerText = "No messages yet.";
              msg.id = "chat-message-empty";
              chatMessages.appendChild(msg);
            }

            // 👷 Dummy messages.
            //for (let i = 0; i <= 30; i++) {
            //  chatAddMessage("Hello", "@user");
            //}

            const chatInputBar = cel("div"); // Input bar.
            chatInputBar.id = "chat-input-bar";

            // Calculate scrollbar width for veil and pages button positioning
            function updateChatLayout() {
              const scrollbarWidth = chatMessages.offsetWidth - chatMessages.clientWidth;
              chatMessagesVeil.style.width = "calc(100% - " + scrollbarWidth + "px)";
              chatPagesButton.style.right = scrollbarWidth + "px";
            }
            // Initial calculation (may be 0 if not visible yet)
            updateChatLayout();
            // Recalculate when chat becomes visible
            const chatLayoutObserver = new MutationObserver(() => {
              if (!chatInterface.classList.contains("hidden")) {
                updateChatLayout();
              }
            });
            chatLayoutObserver.observe(chatInterface, { attributes: true, attributeFilter: ["class"] });

            const chatHandle = cel("div");
            chatHandle.id = "chat-handle";
            chatHandle.innerText = "nohandle";

            // 🎹 Monaco-based chat input for syntax highlighting
            const chatInputContainer = cel("div");
            chatInputContainer.id = "chat-input-container";
            
            let chatEditor = null; // Will be set after Monaco loads
            let chatEditorReady = false;
            
            // Helper to get/set chat input value (works with both Monaco and fallback)
            const chatInput = {
              get value() {
                if (chatEditor) return chatEditor.getValue();
                const fallback = document.querySelector("#chat-input-fallback");
                return fallback ? fallback.value : "";
              },
              set value(val) {
                if (chatEditor) {
                  chatEditor.setValue(val);
                } else {
                  const fallback = document.querySelector("#chat-input-fallback");
                  if (fallback) fallback.value = val;
                }
              },
              focus() {
                if (chatEditor) chatEditor.focus();
                else document.querySelector("#chat-input-fallback")?.focus();
              },
              setSelectionRange(start, end) {
                if (chatEditor) {
                  // Monaco: set cursor position at end
                  const model = chatEditor.getModel();
                  if (model) {
                    const pos = model.getPositionAt(end);
                    chatEditor.setPosition(pos);
                    chatEditor.revealPosition(pos);
                  }
                } else {
                  const fallback = document.querySelector("#chat-input-fallback");
                  if (fallback && fallback.setSelectionRange) {
                    fallback.setSelectionRange(start, end);
                  }
                }
              }
            };
            
            // Create fallback input until Monaco loads
            const chatInputFallback = cel("input");
            chatInputFallback.id = "chat-input-fallback";
            chatInputFallback.type = "text";
            chatInputFallback.autocomplete = "off";
            chatInputFallback.maxLength = 128;
            chatInputFallback.style.cssText = "width:100%;height:100%;border:none;padding:0.35em 0.5em;font-size:100%;box-sizing:border-box;";
            chatInputContainer.appendChild(chatInputFallback);

            const chatEnter = cel("button"); // Enter button.
            chatEnter.innerText = "Enter";
            chatEnter.id = "chat-enter";

            // 🏷️ Handle autocomplete
            const chatAutocomplete = cel("div");
            chatAutocomplete.id = "chat-autocomplete";
            chatInputBar.style.position = "relative";
            
            const seenHandles = new Set(); // Track handles seen in chat
            let autocompleteIndex = -1;
            let autocompleteMatches = [];
            
            function updateSeenHandles(handle) {
              if (handle && handle.startsWith("@") && handle !== "@nohandle") {
                seenHandles.add(handle);
              }
            }
            
            function showAutocomplete(query) {
              const q = query.toLowerCase();
              autocompleteMatches = Array.from(seenHandles)
                .filter(h => h.toLowerCase().includes(q))
                .slice(0, 8);
              
              if (autocompleteMatches.length === 0) {
                hideAutocomplete();
                return;
              }
              
              chatAutocomplete.innerHTML = "";
              autocompleteMatches.forEach((handle, i) => {
                const item = cel("div");
                item.classList.add("autocomplete-item");
                if (i === autocompleteIndex) item.classList.add("selected");
                item.innerText = handle;
                item.addEventListener("click", (e) => { e.preventDefault(); insertHandle(handle); });
                item.addEventListener("touchend", (e) => { e.preventDefault(); insertHandle(handle); });
                chatAutocomplete.appendChild(item);
              });
              chatAutocomplete.classList.add("visible");
            }
            
            function hideAutocomplete() {
              chatAutocomplete.classList.remove("visible");
              autocompleteIndex = -1;
              autocompleteMatches = [];
            }
            
            function insertHandle(handle) {
              const val = chatInput.value;
              const atPos = val.lastIndexOf("@");
              if (atPos !== -1) {
                const newVal = val.slice(0, atPos) + handle + " ";
                chatInput.value = newVal;
                // Set cursor to end of input
                chatInput.setSelectionRange(newVal.length, newVal.length);
              }
              hideAutocomplete();
              chatInput.focus();
            }
            
            // Fallback input event listeners (used before Monaco loads)
            chatInputFallback.addEventListener("input", (e) => {
              const val = chatInputFallback.value;
              const atPos = val.lastIndexOf("@");
              if (atPos !== -1 && atPos === val.length - 1 || 
                  (atPos !== -1 && !val.slice(atPos).includes(" "))) {
                const query = val.slice(atPos + 1);
                showAutocomplete(query);
              } else {
                hideAutocomplete();
              }
            });
            
            chatInputFallback.addEventListener("keydown", (e) => {
              if (!chatAutocomplete.classList.contains("visible")) return;
              
              if (e.key === "ArrowDown") {
                e.preventDefault();
                autocompleteIndex = Math.min(autocompleteIndex + 1, autocompleteMatches.length - 1);
                showAutocomplete(chatInputFallback.value.slice(chatInputFallback.value.lastIndexOf("@") + 1));
              } else if (e.key === "ArrowUp") {
                e.preventDefault();
                autocompleteIndex = Math.max(autocompleteIndex - 1, 0);
                showAutocomplete(chatInputFallback.value.slice(chatInputFallback.value.lastIndexOf("@") + 1));
              } else if (e.key === "Enter" && autocompleteIndex >= 0) {
                e.preventDefault();
                insertHandle(autocompleteMatches[autocompleteIndex]);
              } else if (e.key === "Escape") {
                hideAutocomplete();
              } else if (e.key === "Tab" && autocompleteMatches.length > 0) {
                e.preventDefault();
                insertHandle(autocompleteMatches[autocompleteIndex >= 0 ? autocompleteIndex : 0]);
              }
            });
            
            chatInputFallback.addEventListener("blur", () => {
              setTimeout(hideAutocomplete, 150); // Delay to allow click on autocomplete
            });

            chatInputBar.appendChild(chatHandle);
            chatInputBar.appendChild(chatInputContainer);
            chatInputBar.appendChild(chatAutocomplete);
            chatInputBar.appendChild(chatEnter);

            chatInterface.appendChild(chatMessages);
            chatInterface.appendChild(chatInputBar);
            
            // 🎹 Initialize Monaco Editor for chat input
            require.config({ 
              paths: { 
                vs: 'https://cdn.jsdelivr.net/npm/monaco-editor@0.52.0/min/vs'
              },
              ignoreDuplicateModules: ['vs/editor/editor.main']
            });
            
            require(['vs/editor/editor.main'], function() {
              // Register sotce-chat language with syntax highlighting
              monaco.languages.register({ id: 'sotce-chat' });
              
              monaco.languages.setMonarchTokensProvider('sotce-chat', {
                tokenizer: {
                  root: [
                    [/-\\d+-/, 'page-link'],      // Page references like -1-
                    [/\\*\\d+\\*/, 'question-link'], // Question references like *1*
                    [/@[a-zA-Z0-9_-]+/, 'handle'], // Handles like @user
                    [/./, 'text']
                  ]
                }
              });
              
              // Define sotce-chat theme (light)
              monaco.editor.defineTheme('sotce-chat-light', {
                base: 'vs',
                inherit: true,
                rules: [
                  { token: 'text', foreground: '333333' },
                  { token: 'page-link', foreground: 'ff69b4', fontStyle: 'bold' },      // Hot pink for pages
                  { token: 'question-link', foreground: '9b59b6', fontStyle: 'bold' },  // Purple for questions
                  { token: 'handle', foreground: 'c85078', fontStyle: 'bold' }          // Pink for handles
                ],
                colors: {
                  'editor.background': '#ffffff',
                  'editor.foreground': '#333333',
                  'editorCursor.foreground': '#ff69b4',
                  'editor.lineHighlightBackground': '#ffffff00',
                  'editor.selectionBackground': '#ff69b444',
                }
              });
              
              // Define sotce-chat theme (dark)
              monaco.editor.defineTheme('sotce-chat-dark', {
                base: 'vs-dark',
                inherit: true,
                rules: [
                  { token: 'text', foreground: 'e8e0d8' },
                  { token: 'page-link', foreground: 'd88aa0', fontStyle: 'bold' },      // Dusty pink for pages
                  { token: 'question-link', foreground: 'b08ac0', fontStyle: 'bold' },  // Soft purple for questions
                  { token: 'handle', foreground: 'd88aa0', fontStyle: 'bold' }          // Dusty pink for handles
                ],
                colors: {
                  'editor.background': '#2a2420',
                  'editor.foreground': '#e8e0d8',
                  'editorCursor.foreground': '#d88aa0',
                  'editor.lineHighlightBackground': '#2a242000',
                  'editor.selectionBackground': '#d88aa044',
                }
              });
              
              // Detect system theme preference
              const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
              const initialTheme = prefersDark ? 'sotce-chat-dark' : 'sotce-chat-light';
              
              // Remove the fallback input
              chatInputFallback.remove();
              
              // Create Monaco editor
              chatEditor = monaco.editor.create(chatInputContainer, {
                value: '',
                language: 'sotce-chat',
                theme: initialTheme,
                minimap: { enabled: false },
                scrollBeyondLastLine: false,
                fontSize: 16,
                fontFamily: 'Helvetica, sans-serif',
                lineNumbers: 'off',
                glyphMargin: false,
                folding: false,
                lineDecorationsWidth: 0,
                lineNumbersMinChars: 0,
                renderLineHighlight: 'none',
                overviewRulerLanes: 0,
                scrollbar: {
                  vertical: 'hidden',
                  horizontal: 'hidden',
                  handleMouseWheel: false
                },
                automaticLayout: true,
                wordWrap: 'off',
                cursorStyle: 'line',
                cursorBlinking: 'blink',
                padding: { top: 4, bottom: 4 },
                hover: { enabled: false },
                parameterHints: { enabled: false },
                quickSuggestions: false,
                suggestOnTriggerCharacters: false,
                codeLens: false,
                lightbulb: { enabled: 'off' },
                contextmenu: false,
                accessibilitySupport: 'off',
                renderWhitespace: 'none',
                links: false,
                matchBrackets: 'never',
                occurrencesHighlight: 'off',
                selectionHighlight: false,
                find: { addExtraSpaceOnTop: false, autoFindInSelection: 'never' },
              });
              
              // Mobile: ensure tapping chat input opens native keyboard
              chatInputContainer.addEventListener('touchend', () => {
                if (chatEditor) {
                  chatEditor.focus();
                  // Also explicitly focus the hidden textarea Monaco uses
                  const textarea = chatInputContainer.querySelector('.inputarea');
                  if (textarea) {
                    textarea.style.position = 'absolute';
                    textarea.style.top = '0';
                    textarea.style.left = '0';
                    textarea.focus();
                  }
                }
              });
              
              // Mobile: adjust chat height when virtual keyboard opens/closes
              if (window.visualViewport) {
                const adjustForKeyboard = () => {
                  const vv = window.visualViewport;
                  const chatEl = document.getElementById('chat');
                  if (chatEl) {
                    chatEl.style.height = vv.height + 'px';
                    // Scroll messages to bottom so input stays visible
                    const msgs = document.getElementById('chat-messages');
                    if (msgs) msgs.scrollTop = msgs.scrollHeight;
                  }
                };
                window.visualViewport.addEventListener('resize', adjustForKeyboard);
                window.visualViewport.addEventListener('scroll', adjustForKeyboard);
              }
              
              // Listen for system theme changes and update Monaco
              window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', (e) => {
                const newTheme = e.matches ? 'sotce-chat-dark' : 'sotce-chat-light';
                monaco.editor.setTheme(newTheme);
              });
              
              // Handle Enter key for sending
              chatEditor.addCommand(monaco.KeyCode.Enter, () => {
                chatEnter.click();
              });
              
              // Handle Escape to blur
              chatEditor.addCommand(monaco.KeyCode.Escape, () => {
                chatEditor.getContainerDomNode().blur();
              });
              
              // Character limit (128)
              chatEditor.onDidChangeModelContent(() => {
                const text = chatEditor.getValue();
                if (text.length > 128) {
                  chatEditor.setValue(text.slice(0, 128));
                  // Move cursor to end
                  const model = chatEditor.getModel();
                  chatEditor.setPosition({ lineNumber: 1, column: 129 });
                }
                // Collapse to single line (remove newlines)
                if (text.includes('\\n')) {
                  chatEditor.setValue(text.replace(/\\n/g, ' '));
                }
                
                // Handle autocomplete for @mentions
                const val = text;
                const atPos = val.lastIndexOf("@");
                if (atPos !== -1 && atPos === val.length - 1 || 
                    (atPos !== -1 && !val.slice(atPos).includes(" "))) {
                  const query = val.slice(atPos + 1);
                  showAutocomplete(query);
                } else {
                  hideAutocomplete();
                }
              });
              
              // Arrow key navigation for autocomplete
              chatEditor.addCommand(monaco.KeyCode.DownArrow, () => {
                if (chatAutocomplete.classList.contains("visible")) {
                  autocompleteIndex = Math.min(autocompleteIndex + 1, autocompleteMatches.length - 1);
                  showAutocomplete(chatInput.value.slice(chatInput.value.lastIndexOf("@") + 1));
                } else {
                  // Default behavior - do nothing special for single line
                }
              });
              
              chatEditor.addCommand(monaco.KeyCode.UpArrow, () => {
                if (chatAutocomplete.classList.contains("visible")) {
                  autocompleteIndex = Math.max(autocompleteIndex - 1, 0);
                  showAutocomplete(chatInput.value.slice(chatInput.value.lastIndexOf("@") + 1));
                }
              });
              
              chatEditor.addCommand(monaco.KeyCode.Tab, () => {
                if (chatAutocomplete.classList.contains("visible") && autocompleteMatches.length > 0) {
                  insertHandle(autocompleteMatches[autocompleteIndex >= 0 ? autocompleteIndex : 0]);
                }
              });
              
              chatEditorReady = true;
              console.log("🎹 Monaco chat editor ready");
            });

            // 🥬 Send a message to chat.
            async function chatSend(text) {
              text = text.replace(/s+$/, ""); // Trim trailing whitespace.

              if (!window.sotceHandle) {
                alert("👤 No handle set.");
              } else if (text === "") {
                alert("🪧 Your message cannot empty.");
              } else {
                // TODO: Check for user / user.sub and for a token.

                chatInputBar.classList.add("sending");

                const token =
                  window.sotceTOKEN || (await auth0Client.getTokenSilently());

                // console.log(user);

                // Send the chat message.
                chat.system.server.send("chat:message", {
                  text,
                  token,
                  sub: user.sub,
                });

                setTimeout(() => {
                  chatInputBar.classList.remove("sending");
                }, 250);

                // notice("SENT"); // TODO: 🔔 Play a sent sound. 24.11.30.23.57
              }
            }

            chatEnter.addEventListener("click", async () => {
              const text = chatInput.value;
              chatInput.value = "";
              await chatSend(text);
            });

            function chatEnterKeyListener(e) {
              if (e.key === "Enter") chatEnter.click();
            }

            // Fallback input Enter key handling (before Monaco)
            chatInputFallback.addEventListener("focus", () => {
              window.addEventListener("keydown", chatEnterKeyListener);
            });

            chatInputFallback.addEventListener("blur", () => {
              window.removeEventListener("keydown", chatEnterKeyListener);
            });

            chatPagesButton.addEventListener("click", () => {
              chatInterface.classList.add("hidden");
              chatInterface.classList.add("inaccessible");
              updatePath("/");
              // Mark chat as read on close too
              localStorage.setItem("sotce-chat-last-seen", new Date().toISOString());
            });

            // 🤖 Respond to every chat message...
            chat.system.receiver = (id, type, content) => {
              /*
              console.log(
                "🗨️ Received chat:",
                "Connection ID:",
                id,
                "Type:",
                type,
                "Content:",
                content,
              );
              */

              if (type === "connected") {
                chatDisconnected.classList.add("hidden");
                // Insert messages if necessary...
                clearChatMessages();
                chat.system.messages.forEach((message) => {
                  chatAddMessage(message.text, message.from, message.when, message.count);
                });

                if (chat.system.messages.length === 0) chatAddEmpty();

                // Check for unread messages
                const lastSeen = localStorage.getItem("sotce-chat-last-seen");
                if (lastSeen && chat.system.messages.length > 0) {
                  const lastMsg = chat.system.messages[chat.system.messages.length - 1];
                  if (lastMsg.when && new Date(lastMsg.when) > new Date(lastSeen)) {
                    chatButtonRef?.classList.add("has-unread");
                  }
                } else if (!lastSeen && chat.system.messages.length > 0) {
                  // First visit ever — mark as unread so they discover chat
                  chatButtonRef?.classList.add("has-unread");
                }

                const gateCurtain = document.querySelector("#gate-curtain");
                const garden = document.querySelector("#garden");

                if (
                  gateCurtain &&
                  !gateCurtain.classList.contains("obscured") &&
                  !garden &&
                  (status === "subscribed" || subscription?.admin) // Only show chat for subscribed users and admins
                ) {
                  chatInterface.classList.remove("hidden");
                }
                chatterCountUpdate();
                return;
              }

              if (type === "too-long") {
                // notice("TOO LONG", ["red", "yellow"]);
                alert("⚠️ 🗨️ Your message was too long.");
                return;
              }

              if (type === "unauthorized") {
                // notice("Unauthorized", ["red", "yellow"]);
                alert("⚠️ 🗨️ Subscribe and create a handle to chat.");
                return;
              }

              if (type === "message") {
                const msg = content; // Pre-transformed and stored.
                if (chat.system.messages.length === 1) {
                  document.getElementById("chat-message-empty")?.remove();
                }
                chatAddMessage(msg.text, msg.from, msg.when, msg.count);
                chatScrollToBottom({ always: false });
                // If chat is hidden, mark unread
                if (chatInterface.classList.contains("hidden")) {
                  chatButtonRef?.classList.add("has-unread");
                }
                // sound.play(messageSfx);
                return;
              }

              if (type === "message:update") {
                // Update the count on the most recent message
                const updateData = content;
                const lastMessageEl = chatMessages.lastChild;
                if (lastMessageEl && lastMessageEl.querySelector) {
                  const txtEl = lastMessageEl.querySelector(".message-content");
                  if (txtEl && chat.system.messages[updateData.index]) {
                    const msg = chat.system.messages[updateData.index];
                    txtEl.innerText = msg.text + " x" + updateData.count;
                  }
                }
                return;
              }

              if (type === "handle:update" || type === "handle:strip") {
                console.log("👱️‍ 'handle' edit received:", type, content);
                let layoutChanged;
                chat.system.messages.forEach((message) => {
                  if (message.sub === content.user) {
                    message.from = content.handle;
                    layoutChanged = true;
                  }
                });

                if (layoutChanged) {
                  clearChatMessages();
                  chat.system.messages.forEach((message) => {
                    chatAddMessage(message.text, message.from, message.when, message.count);
                  });
                }

                console.log("👱 'handle' edit completed for:", content.handle);
                return;
              }

              if (type === "joined" || type === "left") {
                console.log("New chatter count:", chat.system.chatterCount);
                chatterCountUpdate();
                return;
              }

              // console.log("🌠 Message received:", id, type, content);
            };

            wrapper.appendChild(chatInterface);

            // Scroll to the bottom of pre-loaded messages.
            chatScrollToBottom();

            window.messages = chatMessages;
            window.wrapper = wrapper;
            window.chat = chatInterface;

            // 🌠 Authorization & Interface

            // Enable ':active' class on iOS Safari.
            document.addEventListener("touchstart", function () {}, false);

            function adjustFontSize() {
              const fontSizeInPx = 16;
              document.body.style.fontSize = fontSizeInPx + "px";
            }

            window.addEventListener("resize", adjustFontSize);
            adjustFontSize();

            function asset(identifier) {
              return "${assetPath}" + identifier;
            }

            function cleanUrlParams(url, params) {
              const queryString = params?.toString();
              history.pushState(
                {},
                "",
                url.pathname + (queryString ? "?" + queryString : ""),
              );
            }

            // Reload the page with the gate open in development.
            let GATE_WAS_UP = false;
            let WRITING_A_PAGE = false;

            if (dev) {
              const params = url.searchParams;
              // Gate
              const gateParam = params.get("gate");
              GATE_WAS_UP = gateParam === "up";
              params.delete("gate");
              // Editor
              const writingParam = params.get("writing");
              WRITING_A_PAGE = writingParam === "page";
              params.delete("writing");
              cleanUrlParams(url, params);
            }

            // Send some messages to the VS Code extension.
            window.parent?.postMessage(
              { type: "url:updated", slug: "sotce-net" },
              "*",
            );
            window.parent?.postMessage({ type: "ready" }, "*");

            // 🗺️🎏 URL Param Flags
            // Check for the '?notice=' parameter, memorize and clear it.
            {
              const urlParams = new URLSearchParams(window.location.search);
              const notice = urlParams.get("notice");

              if (notice) {
                console.log("🪧 Notice:", notice, urlParams);

                if (notice === "success") {
                  alert("🗞️ You're subscribed!");
                  waitForSubscriptionSuccessThreeTimes = true;
                }

                urlParams.delete("notice");
                cleanUrlParams(url, urlParams);
              }
            }

            // 📧 Check to see if the user clicked an 'email' verified link.
            {
              const urlParams = new URLSearchParams(window.location.search);
              if (
                urlParams.get("supportSignUp") === "true" &&
                urlParams.get("success") === "true" &&
                urlParams.get("code") === "success"
              ) {
                urlParams.delete("supportSignUp");
                urlParams.delete("supportForgotPassword");
                urlParams.delete("message");
                urlParams.delete("success");
                urlParams.delete("code");
                cleanUrlParams(url, urlParams);
                fullAlert = "<span id='email-verified'>Email verified!</span>";
              }
            }

            // Clean any url params if the first param is 'iss'.
            if (window.location.search.startsWith("?iss")) cleanUrlParams(url);

            // Reload fading.
            window.addEventListener("beforeunload", (e) => {
              document.body.classList.add("reloading");
            });

            document.addEventListener("visibilitychange", function () {
              if (!document.hidden) document.body.classList.remove("reloading");
            });

            const gateElements = {};
            let gating = false;
            let computePageLayout;
            let SUBSCRIBER_COUNT;
            const maxLines = ${MAX_LINES};

            // #region 🥀 gate&garden
            // Cache gate elements to avoid recreation
            let cachedGateCurtain = null;
            let cachedGateStatus = null;
            
            async function gate(status, user, subscription) {
              console.log("🚪 gate() called with status:", status);
              // If gate already exists with same status, just return it (fast path)
              if (cachedGateCurtain && cachedGateStatus === status && document.body.contains(cachedGateCurtain)) {
                console.log("🚪 Reusing cached gate!");
                return cachedGateCurtain;
              }
              
              if (gating) {
                // Return existing curtain if available
                const existing = document.getElementById("gate-curtain");
                if (existing) return existing;
                return;
              }
              gating = true;
              let message,
                buttons = [],
                buttonsTop = [];

              const g = document.createElement("div");
              const curtain = document.createElement("div");
              curtain.id = "gate-curtain";
              curtain.classList.add("obscured");
              g.id = "gate";
              g.classList.add("faded");

              const cookieWrapper = cel("div");
              cookieWrapper.id = "cookie-wrapper";

              // Only make cookie interactive for subscribed users and admins
              if (status === "subscribed" || subscription?.admin) {
                cookieWrapper.classList.add("interactive");
              }

              const img = document.createElement("img");
              img.id = "cookie";

              // Prevent tap and hold on iOS.
              // img.addEventListener("contextmenu", function (e) {
              //  e.preventDefault();
              // });

              const h1 = document.createElement("h1");
              const h2 = cel("h2");
              const navLow = document.createElement("nav");
              navLow.id = "nav-low";

              if (embedded || fromAesthetic) {
                const prompt = document.createElement("button");
                prompt.id = "prompt";
                prompt.onclick = aesthetic;
                prompt.innerHTML = "sotce-net";
                curtain.appendChild(prompt);
              }

              function genSubscribeButton(type) {
                if (!type) {
                  h2.innerHTML =
                    "<span id='email-verified'>Email verified!</span>";
                  h2.classList.remove("loading-dots");
                }

                // Build button.
                const sb = cel("button");

                if (!type) {
                  sb.id = "subscribe";
                  sb.onclick = subscribe;
                  sb.innerText = "subscribe";
                } else if (type === "unsubscribe") {
                  sb.innerText = "unsubscribe";
                  sb.onclick = cancel;
                } else if (type === "resubscribe") {
                  sb.innerText = "resubscribe";
                  sb.onclick = subscribe;
                }

                let out = sb;

                // if (type) {
                // And privacy-policy link.
                const priv = cel("a");
                priv.id = "privacy-policy";
                priv.innerText = "privacy policy";
                priv.href = "${dev ? "/sotce-net/" : "/"}privacy-policy";

                // if (!type) {
                //   // subscribe
                //   priv.style.left = "4%";
                //   priv.style.width = "93%";
                // } else if (type === "resubscribe") {
                //   priv.style.left = "11%";
                //   priv.style.width = "80%";
                // } else if (type === "unsubscribe") {
                //   priv.style.left = "11.5%";
                //   priv.style.width = "77%";
                // }

                const secondrap = cel("div");
                secondrap.id = "secondary-wrapper";
                secondrap.appendChild(sb);
                secondrap.appendChild(priv);
                out = secondrap;
                //}

                return out;
              }

              function genWelcomeMessage(subscription) {
                return (
                  "Signed in as <a href='' id='email'" +
                  (subscription?.admin ? "class='admin'" : "") +
                  ">" +
                  user.email +
                  "</a>"
                );
              }

              if (status === "logged-out") {
                message = "confessions";

                const lb = cel("button");
                lb.innerText = "log in";
                lb.onclick = login;
                buttons.push(lb);

                if (!embedded) {
                  const imnew = cel("button");
                  imnew.onclick = signup;
                  imnew.innerText = "i'm new";

                  const imnewwrap = cel("div");

                  imnewwrap.id = "imnew-wrapper";
                  imnewwrap.appendChild(imnew);

                  if (SUBSCRIBER_COUNT > 0) {
                    let text;
                    if (SUBSCRIBER_COUNT === 1) {
                      text = "1 reading";
                    } else {
                      text = SUBSCRIBER_COUNT + " reading";
                    }
                    const subscriberCount = cel("div");
                    subscriberCount.id = "subscriber-count";
                    subscriberCount.innerText = text;
                    imnewwrap.appendChild(subscriberCount);
                  }

                  buttons.push(imnewwrap);
                }
              } else if (status !== "coming-soon") {
                const lo = cel("button");
                lo.onclick = () => {
                  if (confirm("🐾 See you later?")) logout();
                };
                lo.innerText = "log out";

                const lowrap = cel("div");
                lowrap.id = "logout-wrapper";
                lowrap.appendChild(lo);

                const del = cel("a");
                del.id = "delete-account";
                del.innerText = "delete account";
                del.href = "";

                del.onclick = function (e) {
                  e.preventDefault();
                  veil();
                  if (
                    confirm(
                      "🚨 Are you sure you want to delete your account?",
                    ) &&
                    confirm("❌ This action cannot be undone.") &&
                    confirm("🔴 Press OK once more to delete your account.")
                  ) {
                    // Run the delete endpoint if confirmed
                    userRequest("POST", "/sotce-net/delete-account")
                      .then(async (res) => {
                        if (res.status === 200) {
                          unveil({ instant: true });
                          flash("red", { hold: true });
                          setTimeout(
                            () => alert("Your account has been deleted."),
                            25,
                          );
                          logout();
                        } else {
                          setTimeout(
                            () =>
                              alert(
                                "😦 Your account could not be fully deleted. Email \`hello@sotce.net\` for help.",
                              ),
                            25,
                          );
                          unveil({ instant: true });
                          logout();
                        }
                      })
                      .catch((err) => {
                        console.error("🔴 Account deletion error...", err);
                        unveil();
                      });
                  } else {
                    unveil();
                  }
                  // Show a confirmation dialog before proceeding
                };

                lowrap.appendChild(del);
                buttons.push(lowrap);
              }

              if (status === "coming-soon") {
                // 🥠 Leave a blank cookie in production pre-launch. 24.08.23.21.35
                g.classList.add("coming-soon");
              }

              if (status === "unverified") {
                message = genWelcomeMessage();
                h2.innerText = "Awaiting email verification";
                h2.classList.add("loading-dots");

                const rs = cel("button");
                rs.onclick = resend;
                rs.innerText = "resend email";
                buttons.push(rs);

                fetchUser = function (email) {
                  // console.log("💚 Fetching user...");
                  fetch(
                    "/user?from=" + encodeURIComponent(email) + "&tenant=sotce",
                  )
                    .then((res) => res.json())
                    .then(async (u) => {
                      if (u.email_verified) {
                        // Check to see if the user has a subscription here, before rendering a subscribe button.
                        user.email_verified = u.email_verified;
                        user.sub = u.sub; // Add sub to user.
                        const entered = await subscribed({ limit: 100 }); // Load more pages for feed
                        if (entered) {
                          status = "subscribed";
                          subscription = entered;
                          await garden(entered, user, true); // Re-open garden but show the gate first.
                        } else {
                          if (!embedded) {
                            rs.remove();
                            navLow.appendChild(genSubscribeButton());
                          } else {
                            h2.innerText = "please subscribe in your browser";
                            h2.classList.remove("loading-dots");
                          }
                        }
                      } else if (u) {
                        h2.innerText = "Awaiting email verification";
                        h2.classList.add("loading-dots");
                        verificationTimeout = setTimeout(() => {
                          fetchUser(email);
                        }, 1000);
                      } else {
                        console.warn("No user retrieved:", u);
                      }
                    })
                    .catch((err) => {
                      console.error("👨‍🦰 Error:", err);
                      verificationTimeout = setTimeout(() => {
                        fetchUser(email);
                      }, 1000);
                    });
                };
                fetchUser(user.email);
              } else if (status === "verified") {
                message = genWelcomeMessage();
                buttons.push(genSubscribeButton());
              } else if (status === "subscribed") {
                message = genWelcomeMessage(subscription);
                // 🗼 Check to see if a handle already exists...
                let handle;

                try {
                  const response = await fetch("/handle?for=sotce-" + user.sub);
                  if (response.status === 200) {
                    const data = await response.json();
                    const newHandle = "@" + data.handle;
                    handle = newHandle;
                    console.log("🫅 Handle found:", newHandle);
                    window.sotceHandle = handle;
                  } else {
                    //console.warn(
                    //  "❌ 🫅 Handle not found:",
                    //  await response.json(),
                    //);
                  }
                } catch (error) {
                  console.error("❌ 🫅 Handle error:", error);
                }

                // 🙆 Add 'create handle' button here.
                const hb = cel("button");
                hb.classList.add("positive");
                hb.innerText = handle || "create handle";

                hb.onclick = async function () {
                  const newHandle = prompt(
                    handle
                      ? "👤 Change your handle to?"
                      : "Set your handle to?",
                  );
                  if (!newHandle) return;
                  veil();
                  try {
                    const response = await fetch("/handle", {
                      method: "POST",
                      headers: {
                        "Content-Type": "application/json",
                        Authorization:
                          "Bearer " +
                          (window.sotceTOKEN ||
                            (await auth0Client.getTokenSilently())),
                      },
                      body: JSON.stringify({
                        handle: newHandle,
                        tenant: "sotce",
                      }),
                    });
                    if (response.ok) {
                      const result = await response.json();
                      // console.log("💁‍♀️ Handle set:", result);
                      hb.innerText = "@" + result.handle;
                      handle = hb.innerText;
                      window.sotceHandle = handle;
                      unveil();
                    } else {
                      const error = await response.json();
                      console.error("❌ Handle error:", error.message);
                      unveil({ instant: true });
                      const message =
                        error.message === "same" ? "the same" : error.message;
                      setTimeout(
                        () =>
                          alert(
                            "❌ Sorry, " +
                              (message !== "error"
                                ? "that handle is " + message + "."
                                : "there was an error setting your handle."),
                          ),
                        100,
                      );
                    }
                  } catch (error) {
                    console.error("🔴 Error:", error);
                    unveil({ instant: true });
                    setTimeout(
                      () =>
                        alert("An error occurred while creating your handle."),
                      100,
                    );
                  }
                };
                buttonsTop.push(hb /*hbwrap*/);

                if (subscription.until === "recurring") {
                  h2.innerText =
                    "Your subscription renews on " + subscription.renews; // + ".";
                  buttons.push(genSubscribeButton("unsubscribe"));
                } else {
                  h2.innerText =
                    "Your subscription ends on " + subscription.until + ".";
                  buttons.push(genSubscribeButton("resubscribe"));
                }

                curtain.classList.add("hidden");
                // if (GATE_WAS_UP) cookieWrapper.classList.add("interactive");
              }

              // if (status === "subscribed") {
              //   curtain.classList.add("hidden");
              //   if (GATE_WAS_UP) cookieWrapper.classList.add("interactive");
              // }

              cookieWrapper.addEventListener(
                "click",
                () => {
                  if (!cookieWrapper.classList.contains("interactive")) return;
                  const enterStart = performance.now();
                  console.log("🌻 Gate cookie click - ENTERING garden");
                  
                  curtain.classList.add("hidden");
                  console.log("  curtain.hidden:", (performance.now() - enterStart).toFixed(2), "ms");
                  
                  cookieWrapper.classList.remove("interactive");

                  const garden = document.querySelector("#garden");

                  if (garden) {
                    document.documentElement.classList.add("garden");
                    console.log("  [SYNC] Before removing hidden from garden");
                    garden.classList.remove("hidden");
                    console.log("  garden.visible:", (performance.now() - enterStart).toFixed(2), "ms");
                    console.log("  [SYNC] After removing hidden, before RAF");
                    
                    // Track when layout/paint actually completes
                    requestAnimationFrame(() => {
                      console.log("  RAF 1:", (performance.now() - enterStart).toFixed(2), "ms");
                      requestAnimationFrame(() => {
                        console.log("  RAF 2 (paint):", (performance.now() - enterStart).toFixed(2), "ms");
                      });
                    });
                    
                    console.log("  [SYNC] After RAF scheduled, before updatePath");
                    updatePath("/");
                    console.log("  [SYNC] After updatePath, handler done:", (performance.now() - enterStart).toFixed(2), "ms");
                  } else {
                    // Only show chat for subscribed users and admins
                    if (status === "subscribed" || subscription?.admin) {
                      chatInterface.classList.add("splash-chat-open");
                      updatePath("/chat");
                    } else {
                      // For non-subscribed users, just go to home
                      updatePath("/");
                    }

                    // Make sure the icon cookie is visible.
                    let cookieMenuWrapper = document.querySelector(
                      "#cookie-menu-wrapper",
                    );

                    if (!cookieMenuWrapper) {
                      // Duplicate in 'garden'.
                      const cookieMenuWrapper = cel("div");
                      const cookieMenu = cel("div");
                      cookieMenuWrapper.id = "cookie-menu-wrapper";
                      cookieMenuWrapper.classList.add("nogarden");
                      cookieMenu.id = "cookie-menu";
                      const cookieImg = cel("img");
                      cookieImg.id = "cookie-menu-img";
                      cookieImg.crossOrigin = "anonymous";

                      cookieMenuWrapper.style.marginRight =
                        scrollbarWidth + "px";

                      cookieMenuWrapper.appendChild(cookieMenu);
                      cookieMenuWrapper.appendChild(cookieImg);

                      cookieImg.onload = function () {
                        // Render a mask image dataurl for the cookieImg.
                        const canvas = document.createElement("canvas");
                        canvas.width = cookieImg.naturalWidth;
                        canvas.height = cookieImg.naturalHeight;
                        const ctx = canvas.getContext("2d");
                        ctx.drawImage(cookieImg, 0, 0);
                        const dataUrl = canvas.toDataURL();
                        cookieMenu.style.maskImage = "url(" + dataUrl + ")";
                        unveil({ instant: true });
                        wrapper.appendChild(cookieMenuWrapper);
                      };

                      veil();

                      cookieImg.src = asset("cookie-open.png");

                      cookieMenu.onclick = function () {
                        curtain.classList.remove("hidden");
                        cookieWrapper.classList.add("interactive");
                        cookieMenuWrapper.classList.add("hidden");
                        chatInterface.classList.remove("splash-chat-open");
                        updatePath("/");
                      };

                      // topBar.appendChild(cookieMenuWrapper);
                      // topBar.appendChild(cookieImg);
                    } else {
                      cookieMenuWrapper.classList.remove("hidden");
                    }
                  }

                  document.body.classList.remove("pages-hidden");
                  wrapper.scrollTop = scrollMemory;
                  computePageLayout?.();
                },
                // { once: true },
              );

              h1.innerHTML = message || "";
              if (buttons.length > 0)
                buttons.forEach((b) => navLow.appendChild(b));
              cookieWrapper.appendChild(img);
              g.appendChild(cookieWrapper);
              if (buttonsTop.length > 0) {
                const navHigh = cel("nav");
                navHigh.id = "nav-high";
                buttonsTop.forEach((b) => navHigh.appendChild(b));
                g.appendChild(navHigh);
              }
              g.appendChild(h1);
              if (h2.innerText.length > 0) g.appendChild(h2);
              // g.appendChild(h2);
              g.appendChild(navLow);

              curtain.appendChild(g);

              const imageSrc = asset(
                status === "subscribed" ? "cookie-open.png" : "cookie.png",
              );
              img.src = imageSrc;

              const imageLoadPromise = new Promise((resolve) => {
                const handleImageReady = () => {
                  document.getElementById("gate-curtain")?.remove(); // Rid old curtain.
                  const checkObscurity = setInterval(() => {
                    if (!curtain.classList.contains("obscured")) {
                      g.classList.remove("faded");
                      clearInterval(checkObscurity);
                    }
                  }, 10);
                  wrapper.appendChild(curtain);
                  const email = document.getElementById("email");
                  if (email) {
                    email.onclick = (e) =>
                      resend(e, status === "unverified" ? undefined : "change");
                  }
                  resolve();
                };
                
                // If image is already cached/complete, fire immediately
                if (img.complete && img.naturalWidth > 0) {
                  handleImageReady();
                } else {
                  img.onload = handleImageReady;
                }
              });

              await imageLoadPromise;
              gating = false;
              
              // Cache the curtain for fast re-entry
              cachedGateCurtain = curtain;
              cachedGateStatus = status;
              
              // Debug: Watch for unexpected hidden class changes
              const hiddenObserver = new MutationObserver((mutations) => {
                for (const mutation of mutations) {
                  if (mutation.attributeName === 'class') {
                    const hadHidden = mutation.oldValue?.includes('hidden');
                    const hasHidden = curtain.classList.contains('hidden');
                    if (hadHidden !== hasHidden) {
                      console.log("🚪 Curtain hidden changed:", hadHidden, "→", hasHidden);
                      console.trace("Stack trace for hidden change:");
                    }
                  }
                }
              });
              hiddenObserver.observe(curtain, { attributes: true, attributeOldValue: true, attributeFilter: ['class'] });
              
              return curtain;
            }

            // Build a layout for the 🌻 'garden'.
            async function garden(subscription, user, showGate = false) {
              const gateCurtain = await gate("subscribed", user, subscription);
              gateCurtain.classList.remove("obscured");

              if (showGate) {
                // gateCurtain.classList.remove("hidden");
                document.body.classList.remove("pages-hidden");
                document.documentElement.classList.remove("garden");
              }

              // Swap the favicon url.
              document.querySelector('link[rel="icon"]').href =
                asset("cookie-open.png");

              const g = cel("div");

              const topBar = cel("div");
              topBar.id = "top-bar";

              g.appendChild(topBar);

              g.id = "garden";
              
              // 🏷️ Page number tooltip with scrolling ticker
              const pageNumberTooltip = cel("div");
              pageNumberTooltip.id = "page-number-tooltip";
              document.body.appendChild(pageNumberTooltip);
              
              let tooltipTimeout = null;
              function showPageNumberTooltip(e, content, pageIndex) {
                if (tooltipTimeout) clearTimeout(tooltipTimeout);
                const rect = e.target.getBoundingClientRect();
                pageNumberTooltip.style.left = (rect.left + rect.width / 2) + "px";
                pageNumberTooltip.style.top = rect.top + "px";
                pageNumberTooltip.innerHTML = '<span class="ticker">' + (content || "Page " + pageIndex) + '</span>';
                pageNumberTooltip.classList.add("visible");
              }
              function hidePageNumberTooltip() {
                tooltipTimeout = setTimeout(() => {
                  pageNumberTooltip.classList.remove("visible");
                }, 100);
              }

              if (!showGate) g.classList.add("obscured");
              if (showGate) {
                g.classList.add("hidden");
                document.body.classList.add("pages-hidden");
              }

              function computeLastLineText(source) {
                const cachedText = source.innerText;
                // let lastHeight = source.clientHeight;
                // let line = 0;
                // let lastLineText = "";
                // source.innerText = "";

                const cs = getComputedStyle(source);
                const lineHeight = parseFloat(cs.lineHeight);
                const totalLines = Math.round(source.clientHeight / lineHeight);

                let lastLineText = ""; // Store the maxLine's text

                if (totalLines >= maxLines) {
                  let currentLine = totalLines;
                  let collecting = false;
                  let maxLineCharIndex = -1; // Track where the 17th line starts

                  // Walk backwards character-by-character
                  for (let c = cachedText.length - 1; c >= 0; c--) {
                    // Update the innerText to measure the line count
                    source.innerText = cachedText.slice(0, c);
                    const newLineCount = Math.round(
                      source.clientHeight / lineHeight,
                    );

                    if (newLineCount < currentLine) {
                      currentLine = newLineCount;
                    }

                    // Start collecting if we are on the 17th line
                    if (currentLine === 17) {
                      collecting = true;
                      if (maxLineCharIndex === -1) maxLineCharIndex = c; // Mark the first char index on line 17
                      lastLineText = cachedText[c] + lastLineText; // Prepend the character
                    }

                    // Stop collecting once we drop below the 17th line
                    if (collecting && currentLine < maxLines) {
                      break;
                    }
                  }

                  // ⚠️ Now handle the TODO: Backtrack through the full word if it was split
                  // const matchIndex = cachedText.indexOf(lastLineText);

                  // if (matchIndex > 0) {
                  //   // Walk backwards from the match index to capture the rest of the word.
                  //   for (let i = matchIndex - 1; i >= 0; i--) {
                  //     const char = cachedText[i];

                  //     // Stop if we hit a word boundary (space, newline, or other separator).
                  //     if (
                  //       /\\s/.test(char) ||
                  //       char === "\\n" ||
                  //       char === "\\r"
                  //     ) {
                  //       break;
                  //     }

                  //     lastLineText = char + lastLineText;
                  //   }
                  // }

                  // Find the starting index of the last line's text in the full cached text.
                  const matchIndex = cachedText.indexOf(lastLineText);

                  if (matchIndex > 0) {
                    // Walk backwards from the match index to capture the rest of the word.
                    for (let i = matchIndex - 1; i >= 0; i--) {
                      const char = cachedText[i];

                      // Stop if we hit a word boundary or any form of line separator.
                      if (
                        /\\s/.test(char) || // Handles spaces, tabs, etc.
                        char === "\\n" ||
                        char === "\\r" ||
                        char === "\\u00A0" // Non-breaking space.
                      ) {
                        break;
                      }

                      lastLineText = char + lastLineText;
                    }
                  }
                }
                // console.log("Last line text:", lastLineText);
                source.innerText = cachedText;
                const cachedWidth = cs.width;
                // source.style.width = "100%";
                source.style.display = "block";
                const maxWidth = source.clientWidth;
                source.style.display = "inline-block";
                source.innerText = "";
                source.style.width = "auto";
                const paddingWidth = source.clientWidth;
                source.innerText = lastLineText;
                // console.log("👱", source.clientWidth, paddingWidth, maxWidth);
                const lastLineProgress =
                  (source.clientWidth - paddingWidth) /
                  (maxWidth - paddingWidth);
                source.style.width = cachedWidth;
                source.style.display = "";
                source.innerText = cachedText;
                return { lastLineProgress, lastLineText };
              }

              // ✍️ Line ending space conversion.
              function needsJustification(text, progress) {
                const endsInPeriod = text.endsWith(".");
                const needsJustification =
                  (!endsInPeriod && progress > 0.8) ||
                  (endsInPeriod && progress > 0.95);
                // if (needsJustification)
                //  console.log("🟠 Line needs justification!");
                return needsJustification;
              }

              // Observe and run a callback once a NodeElement is added
              // to the DOM.
              function observeAdd(nodeToWatch, callback) {
                if (document.body.contains(nodeToWatch)) {
                  callback();
                  return;
                }

                const observer = new MutationObserver((mutationsList) => {
                  mutationsList.forEach((mutation) => {
                    mutation.addedNodes.forEach((node) => {
                      console.log(node);
                      if (node === nodeToWatch) {
                        callback();
                        observer.disconnect();
                      }
                    });
                  });
                });

                observer.observe(document.body, {
                  childList: true,
                  subtree: true,
                });
              }

              // Render a diary page date title in the page feed or the editor.
              function dateTitle(dateString) {
                const opts = { weekday: "long", month: "long", day: "numeric" };
                return new Date(dateString).toLocaleDateString("en-US", opts);
              }

              // 🗨️ Chat chat - Open up the system chat.
              // {
              const chatButton = cel("button");
              chatButton.id = "chat-button";
              chatButton.innerText = "chat";
              chatButtonRef = chatButton; // Set global reference

              chatButton.onclick = function () {
                chatInterface.classList.remove("hidden");
                chatInterface.classList.remove("inaccessible");
                chatScrollToBottom();
                updatePath("/chat");
                if (window.sotceHandle) {
                  chatHandle.innerText = window.sotceHandle;
                }
                // Mark chat as read
                chatButton.classList.remove("has-unread");
                localStorage.setItem("sotce-chat-last-seen", new Date().toISOString());
              };

              topBar.appendChild(chatButton);
              // }

              // ❓ Ask + Respond buttons
              const isJeffrey = window.sotceHandle === "@jeffrey";
              const askButton = null; // Ask button removed for admins

              const respondButton = (subscription?.admin && isJeffrey) ? cel("button") : null;
              if (respondButton) {
                respondButton.id = "respond-button";
                respondButton.innerText = "respond";
              }

              async function openAskEditor() {
                scrollMemory = wrapper.scrollTop;

                veil();
                const asksRes = await userRequest("GET", "/sotce-net/asks");
                unveil({ instant: true });

                const askEditor = cel("div");
                askEditor.id = "ask-editor";

                const editorPlacemat = cel("div");
                editorPlacemat.id = "editor-placemat";

                const form = cel("form");
                form.id = "ask-editor-form";

                const pageWrapper = cel("div");
                pageWrapper.id = "editor-page-wrapper";

                const askPage = cel("div");
                askPage.id = "ask-editor-page";

                // Match the binding style width
                const binding = document.getElementById("binding");
                if (binding) form.style.width = binding.style.width;

                // Date at top (like diary pages)
                const askDate = cel("div");
                askDate.classList.add("ask-date");
                askDate.innerText = dateTitle(new Date());

                const wordsWrapper = cel("div");
                wordsWrapper.id = "ask-words-wrapper";

                // Highlights overlay for @handle coloring
                const highlights = cel("div");
                highlights.id = "ask-highlights";

                const words = cel("textarea");
                // Restore draft if available, otherwise use default
                const savedDraft = localStorage.getItem("sotce-ask-draft");
                words.value = savedDraft || "Dear @amelia, ";
                words.placeholder = "Dear @amelia,";

                function updateHighlights() {
                  highlights.innerHTML = words.value
                    .replace(/&/g, "&amp;")
                    .replace(/</g, "&lt;")
                    .replace(/@[a-zA-Z0-9_-]+/g, '<span class="handle-hl">$&</span>');
                  if (highlights.innerHTML.endsWith("\\n")) highlights.innerHTML += " ";
                }
                updateHighlights();

                const linesLeft = cel("div");
                linesLeft.id = "ask-chars-left";
                const maxAskLines = 5;
                linesLeft.innerText = maxAskLines + " lines left";

                let lastValidValue = words.value;
                words.addEventListener("input", () => {
                  // Line-based limit like diary pages
                  const wordsStyle = window.getComputedStyle(words);
                  const lineHeight = parseFloat(wordsStyle.lineHeight);
                  
                  let measurement = askPage.querySelector("#ask-measurement");
                  if (!measurement) {
                    measurement = document.createElement("div");
                    measurement.id = "ask-measurement";
                    measurement.style.cssText = "position:absolute;z-index:-1;pointer-events:none;visibility:hidden;white-space:pre-wrap;text-align:justify;hyphens:auto;-webkit-hyphens:auto;overflow-wrap:break-word;";
                    measurement.style.width = words.clientWidth + "px";
                    measurement.style.font = wordsStyle.font;
                    measurement.style.fontSize = wordsStyle.fontSize;
                    measurement.style.lineHeight = wordsStyle.lineHeight;
                    measurement.style.padding = wordsStyle.padding;
                    askPage.appendChild(measurement);
                  }
                  
                  measurement.style.width = words.clientWidth + "px";
                  measurement.textContent = words.value || " ";
                  if (words.value.endsWith("\\n")) measurement.textContent += " ";
                  
                  const contentHeight = measurement.scrollHeight;
                  let lineCount = Math.round(contentHeight / lineHeight);
                  if (lineCount === 1 && words.value.length === 0) lineCount = 0;
                  
                  const cursorPosition = words.selectionStart;
                  if (lineCount > maxAskLines) {
                    words.value = lastValidValue;
                    words.setSelectionRange(Math.max(0, cursorPosition - 1), Math.max(0, cursorPosition - 1));
                    lineCount = maxAskLines;
                  } else {
                    lastValidValue = words.value;
                  }
                  
                  const remaining = maxAskLines - Math.min(lineCount, maxAskLines);
                  linesLeft.innerText = remaining + " line" + (remaining !== 1 ? "s" : "") + " left";
                  
                  linesLeft.classList.remove("lines-left-few", "lines-left-little", "lines-left-lots", "lines-left-loads");
                  if (remaining === 0) {
                    linesLeft.classList.add("lines-left-few");
                  } else if (remaining <= 1) {
                    linesLeft.classList.add("lines-left-little");
                  } else if (remaining <= 3) {
                    linesLeft.classList.add("lines-left-lots");
                  } else {
                    linesLeft.classList.add("lines-left-loads");
                  }

                  // Update handle highlighting
                  updateHighlights();
                });

                wordsWrapper.appendChild(highlights);
                wordsWrapper.appendChild(words);
                askPage.appendChild(askDate);
                askPage.appendChild(wordsWrapper);

                // My asks list (shown by swapping page content)
                let asksData = asksRes.status === 200 ? asksRes.asks : [];

                // Question number at bottom with asterisks
                const askNumber = cel("div");
                askNumber.classList.add("ask-number");
                const nextAskNum = asksData.length + 1;
                askNumber.innerText = "*" + nextAskNum + "*";
                askPage.appendChild(askNumber);
                let showingAsks = false;

                // Create asks list container (initially hidden)
                const asksListPage = cel("div");
                asksListPage.id = "asks-list-page";
                asksListPage.style.cssText = "display:none;padding:1em 2em;height:100%;overflow-y:auto;box-sizing:border-box;";

                function updateMyAsksLink() {
                  const count = asksData ? asksData.length : 0;
                  if (showingAsks) {
                    myAsksBtn.innerText = "back";
                    myAsksBtn.style.display = "block";
                  } else if (count > 0) {
                    myAsksBtn.innerText = "my questions (" + count + ")";
                    myAsksBtn.style.display = "block";
                  } else {
                    myAsksBtn.style.display = "none";
                  }
                }

                function renderAsksList() {
                  asksListPage.innerHTML = "";

                  if (!asksData || asksData.length === 0) {
                    const empty = cel("p");
                    empty.innerText = "No questions yet.";
                    empty.style.cssText = "opacity:0.6;text-align:center;margin-top:2em;";
                    asksListPage.appendChild(empty);
                  } else {
                    asksData.forEach((ask) => {
                      const item = cel("div");
                      item.classList.add("ask-item");
                      if (ask.state === "answered") item.classList.add("answered");

                      const whenDate = new Date(ask.when).toLocaleDateString("en-US", {
                        weekday: "long", month: "long", day: "numeric"
                      });

                      const dateLine = cel("div");
                      dateLine.classList.add("ask-status");
                      if (ask.state === "answered") {
                        dateLine.innerText = "Answered · " + whenDate;
                      } else if (ask.draftStartedAt) {
                        dateLine.innerText = "Being answered · " + whenDate;
                      } else {
                        dateLine.innerText = "Asked " + whenDate;
                      }
                      item.appendChild(dateLine);

                      const q = cel("div");
                      q.style.cssText = "margin-top:0.25em;";
                      q.innerText = ask.question;
                      item.appendChild(q);

                      // "take back" button for pending questions without a draft
                      if (ask.state === "pending" && !ask.draftStartedAt) {
                        const deleteBtn = cel("button");
                        deleteBtn.innerText = "take back";
                        deleteBtn.classList.add("take-back");
                        deleteBtn.onclick = async (e) => {
                          e.stopPropagation();
                          if (!confirm("Take back this question?")) return;
                          veil();
                          const res = await userRequest("DELETE", "/sotce-net/ask/" + ask._id);
                          unveil({ instant: true });
                          if (res.status === 200) {
                            asksData = asksData.filter(a => a._id !== ask._id);
                            renderAsksList();
                            updateMyAsksLink();
                          } else {
                            alert(res.message || "Could not delete.");
                          }
                        };
                        item.appendChild(deleteBtn);
                      }
                      
                      // Make answered questions clickable to navigate to them
                      if (ask.state === "answered" && window.feedItems) {
                        const feedItem = window.feedItems.find(fi => fi.type === "question" && fi._id?.toString() === ask._id?.toString());
                        if (feedItem && feedItem.questionNumber) {
                          item.onclick = () => {
                            // Save draft if there's text
                            if (words.value.trim()) {
                              localStorage.setItem("sotce-ask-draft", words.value);
                            }
                            closeAskEditor();
                            // Navigate to the question
                            location.href = "/q/" + feedItem.questionNumber;
                          };
                        }
                      }
                      
                      asksListPage.appendChild(item);
                    });
                  }
                }

                function toggleAsksView() {
                  showingAsks = !showingAsks;
                  if (showingAsks) {
                    renderAsksList();
                    askPage.style.display = "none";
                    asksListPage.style.display = "block";
                    linesLeft.style.display = "none";
                    // Hide ask and nevermind buttons in list view
                    cancelBtn.style.display = "none";
                    submitBtn.style.display = "none";
                  } else {
                    askPage.style.display = "block";
                    asksListPage.style.display = "none";
                    linesLeft.style.display = "block";
                    // Show ask and nevermind buttons
                    cancelBtn.style.display = "";
                    submitBtn.style.display = "";
                  }
                  updateMyAsksLink();
                }

                pageWrapper.appendChild(askPage);
                pageWrapper.appendChild(asksListPage);
                form.appendChild(pageWrapper);

                // Nav buttons (like page editor)
                const nav = cel("nav");
                nav.id = "nav-ask-editor";
                nav.style.width = topBar.style.width;

                const cancelBtn = cel("button");
                cancelBtn.innerText = "nevermind";

                const myAsksBtn = cel("button");
                const initialCount = asksData ? asksData.length : 0;
                myAsksBtn.innerText = "my questions (" + initialCount + ")";
                myAsksBtn.classList.add("ask-toggle", "nav-center");
                if (initialCount === 0) myAsksBtn.style.display = "none";
                myAsksBtn.onclick = (e) => {
                  e.preventDefault();
                  toggleAsksView();
                };

                const submitBtn = cel("button");
                submitBtn.type = "submit";
                submitBtn.setAttribute("form", form.id);
                submitBtn.innerText = "ask";
                submitBtn.classList.add("positive");

                nav.appendChild(cancelBtn);
                nav.appendChild(myAsksBtn);
                nav.appendChild(submitBtn);

                function closeAskEditor() {
                  document.body.classList.remove("pages-hidden");
                  document.documentElement.classList.remove("editing");
                  askEditor.remove();
                  editorPlacemat.remove();
                  nav.remove();
                  linesLeft.remove();
                  askButton?.classList.remove("deactivated");
                  respondButton?.classList.remove("deactivated");
                  wrapper.scrollTop = scrollMemory;
                  computePageLayout?.();
                  updatePath("/");
                }

                cancelBtn.onclick = (e) => {
                  e.preventDefault();
                  if (words.value.trim()) {
                    // Save draft to localStorage
                    localStorage.setItem("sotce-ask-draft", words.value);
                  }
                  closeAskEditor();
                };

                form.addEventListener("submit", async (e) => {
                  e.preventDefault();

                  // Handle ask mode (user submitting a question)
                  const question = words.value.trim();
                  if (!question) {
                    alert("Please enter a question.");
                    return;
                  }
                  veil();
                  const res = await userRequest("POST", "/sotce-net/ask", { question });
                  unveil({ instant: true });
                  if (res.status === 200) {
                    words.value = "";
                    lastValidValue = "";
                    localStorage.removeItem("sotce-ask-draft"); // Clear saved draft
                    linesLeft.innerText = maxAskLines + " lines left";
                    linesLeft.classList.remove("lines-left-few", "lines-left-little");
                    // Refresh the list and jump to my questions view
                    const newAsks = await userRequest("GET", "/sotce-net/asks");
                    if (newAsks.status === 200) {
                      asksData = newAsks.asks;
                    }
                    // Jump to asks list view
                    if (!showingAsks) toggleAsksView();
                    else { renderAsksList(); updateMyAsksLink(); }
                  } else {
                    alert("Error: " + (res.message || "Could not submit question."));
                  }
                });

                const scrollbarWidth = wrapper.offsetWidth - wrapper.clientWidth;
                submitBtn.style.marginRight = scrollbarWidth / 1.5 + "px";

                askEditor.appendChild(form);
                askEditor.appendChild(linesLeft);
                g.appendChild(nav);

                document.documentElement.classList.add("editing");

                g.appendChild(editorPlacemat);
                g.appendChild(askEditor);
                document.body.classList.add("pages-hidden");

                // Scale the page
                const baseWidth = 100 * 8;
                const goalWidth = askPage.parentElement.clientWidth;
                const scale = goalWidth / baseWidth;
                askPage.style.transform = "scale(" + scale + ")";

                askButton?.classList.add("deactivated");
                respondButton?.classList.add("deactivated");
                updatePath("/ask");
                words.focus();
                words.selectionStart = words.selectionEnd = words.value.length;
                words.dispatchEvent(new Event("input"));
              }

              // 📝 Respond Editor - Admin only, page-style editor for responding to questions
              async function openRespondEditor() {
                scrollMemory = wrapper.scrollTop;

                veil();
                const pendingRes = await userRequest("GET", "/sotce-net/asks/pending");
                unveil({ instant: true });

                let pendingData = pendingRes.status === 200 ? pendingRes.asks || [] : [];
                let currentPendingIndex = 0;

                const respondEditor = cel("div");
                respondEditor.id = "respond-editor";

                const editorPlacemat = cel("div");
                editorPlacemat.id = "editor-placemat";

                const form = cel("form");
                form.id = "respond-editor-form";

                const pageWrapper = cel("div");
                pageWrapper.id = "editor-page-wrapper";

                const respondPage = cel("div");
                respondPage.id = "respond-editor-page";

                // Match the binding style width
                const binding = document.getElementById("binding");
                if (binding) form.style.width = binding.style.width;

                // Lines left indicator
                const linesLeft = cel("div");
                linesLeft.id = "respond-lines-left";
                const totalPageLines = 19; // Max lines available for question + answer

                // Asked by indicator (top right)
                const askedBy = cel("div");
                askedBy.id = "respond-asked-by";
                let maxRespondLines = 12; // Will be calculated per question

                let lastValidValue = "";
                let responseWords = null;

                function renderRespondPage() {
                  respondPage.innerHTML = "";

                  if (!pendingData || pendingData.length === 0) {
                    const empty = cel("p");
                    empty.innerText = "No pending questions.";
                    empty.style.cssText = "opacity:0.6;text-align:center;margin-top:40%;";
                    respondPage.appendChild(empty);
                    linesLeft.style.display = "none";
                    return;
                  }

                  const question = pendingData[currentPendingIndex];

                  // Update asked-by indicator
                  if (question.handle) {
                    askedBy.innerText = "asked by @" + question.handle;
                    askedBy.style.display = "block";
                  } else {
                    askedBy.innerText = "asked anonymously";
                    askedBy.style.display = "block";
                  }

                  // Date at top (centered, matching ask page)
                  const pageDate = cel("div");
                  pageDate.classList.add("respond-date");
                  pageDate.innerText = dateTitle(new Date());
                  respondPage.appendChild(pageDate);

                  // Question section
                  const questionSection = cel("div");
                  questionSection.classList.add("respond-question-section");

                  // Counter (only show if multiple questions)
                  const counter = cel("div");
                  counter.classList.add("respond-counter");
                  if (pendingData.length > 1) {
                    counter.innerText = (currentPendingIndex + 1) + " / " + pendingData.length;
                  }
                  questionSection.appendChild(counter);

                  // Question text (with @handles as tappable pink links)
                  const questionText = cel("div");
                  questionText.classList.add("respond-question-text");
                  // Render question with @handles as tappable pink spans
                  questionText.innerHTML = question.question
                    .replace(/&/g, "&amp;")
                    .replace(/</g, "&lt;")
                    .replace(/@[a-zA-Z0-9_-]+/g, '<span class="respond-handle-link" style="color: rgb(200, 80, 120); cursor: pointer; font-style: normal;">$&</span>');
                  // Make @handle spans tappable → save draft and jump to chat
                  questionText.querySelectorAll(".respond-handle-link").forEach(span => {
                    span.addEventListener("click", async (e) => {
                      e.stopPropagation();
                      const handle = span.textContent;
                      await saveDraft();
                      closeRespondEditor();
                      openChatWithMessage(handle + " ");
                    });
                  });
                  questionSection.appendChild(questionText);

                  respondPage.appendChild(questionSection);

                  // Horizontal separator between question and response
                  const separator = cel("hr");
                  separator.classList.add("respond-separator");
                  respondPage.appendChild(separator);

                  // Response section
                  const responseSection = cel("div");
                  responseSection.classList.add("respond-response-section");

                  const wordsWrapper = cel("div");
                  wordsWrapper.id = "respond-words-wrapper";

                  responseWords = cel("textarea");
                  responseWords.classList.add("respond-textarea");
                  responseWords.placeholder = "Your response...";
                  // Pre-populate with saved draft if any
                  if (question.draftAnswer) {
                    responseWords.value = question.draftAnswer;
                  }

                  responseWords.addEventListener("input", () => {
                    const wordsStyle = window.getComputedStyle(responseWords);
                    const lineHeight = parseFloat(wordsStyle.lineHeight);
                    
                    let measurement = respondPage.querySelector("#respond-measurement");
                    if (!measurement) {
                      measurement = document.createElement("div");
                      measurement.id = "respond-measurement";
                      measurement.style.cssText = "position:absolute;z-index:-1;pointer-events:none;visibility:hidden;white-space:pre-wrap;text-align:justify;hyphens:auto;-webkit-hyphens:auto;overflow-wrap:break-word;";
                      measurement.style.width = responseWords.clientWidth + "px";
                      measurement.style.font = wordsStyle.font;
                      measurement.style.fontSize = wordsStyle.fontSize;
                      measurement.style.lineHeight = wordsStyle.lineHeight;
                      measurement.style.padding = wordsStyle.padding;
                      respondPage.appendChild(measurement);
                    }
                    
                    measurement.style.width = responseWords.clientWidth + "px";
                    measurement.textContent = responseWords.value || " ";
                    if (responseWords.value.endsWith("\\n")) measurement.textContent += " ";
                    
                    const contentHeight = measurement.scrollHeight;
                    let lineCount = Math.round(contentHeight / lineHeight);
                    if (lineCount === 1 && responseWords.value.length === 0) lineCount = 0;
                    
                    const cursorPosition = responseWords.selectionStart;
                    if (lineCount > maxRespondLines) {
                      responseWords.value = lastValidValue;
                      responseWords.setSelectionRange(Math.max(0, cursorPosition - 1), Math.max(0, cursorPosition - 1));
                      lineCount = maxRespondLines;
                    } else {
                      lastValidValue = responseWords.value;
                    }
                    
                    const remaining = maxRespondLines - Math.min(lineCount, maxRespondLines);
                    linesLeft.innerText = remaining + " line" + (remaining !== 1 ? "s" : "") + " left";
                    
                    linesLeft.classList.remove("lines-left-few", "lines-left-little", "lines-left-lots", "lines-left-loads");
                    if (remaining === 0) {
                      linesLeft.classList.add("lines-left-few");
                    } else if (remaining <= 3) {
                      linesLeft.classList.add("lines-left-little");
                    } else if (remaining <= 8) {
                      linesLeft.classList.add("lines-left-lots");
                    } else {
                      linesLeft.classList.add("lines-left-loads");
                    }
                  });

                  wordsWrapper.appendChild(responseWords);
                  responseSection.appendChild(wordsWrapper);
                  respondPage.appendChild(responseSection);

                  // Question number at bottom (with asterisks to denote questions)
                  const pageNumber = cel("div");
                  pageNumber.classList.add("page-number");
                  pageNumber.innerText = "*" + (currentPendingIndex + 1) + "*";
                  respondPage.appendChild(pageNumber);

                  // Calculate how many lines the question takes, then set response lines
                  setTimeout(() => {
                    const qStyle = window.getComputedStyle(questionText);
                    const qLineHeight = parseFloat(qStyle.lineHeight);
                    const qHeight = questionText.scrollHeight;
                    const questionLines = Math.ceil(qHeight / qLineHeight);
                    
                    // Available lines = total - question lines (minimum 5 for response)
                    maxRespondLines = Math.max(5, totalPageLines - questionLines);
                    
                    // Update textarea height to match
                    responseWords.style.height = "calc(var(--line-height) * " + maxRespondLines + ")";
                    wordsWrapper.style.height = "calc(var(--line-height) * " + maxRespondLines + ")";
                    
                    linesLeft.innerText = maxRespondLines + " lines left";
                    linesLeft.classList.remove("lines-left-few", "lines-left-little", "lines-left-lots");
                    linesLeft.classList.add("lines-left-loads");
                    
                    responseWords?.focus();
                  }, 50);

                  linesLeft.style.display = "block";
                  linesLeft.innerText = "...";

                  lastValidValue = "";
                }

                renderRespondPage();

                pageWrapper.appendChild(respondPage);
                form.appendChild(pageWrapper);

                // Nav buttons
                const nav = cel("nav");
                nav.id = "nav-respond-editor";
                nav.style.width = topBar.style.width;

                const nevermindBtn = cel("button");
                nevermindBtn.innerText = "nevermind";

                const prevBtn = cel("button");
                prevBtn.innerText = "← prev";
                
                const nextBtn = cel("button");
                nextBtn.innerText = "next →";

                const submitBtn = cel("button");
                submitBtn.type = "submit";
                submitBtn.setAttribute("form", form.id);
                submitBtn.innerText = "respond";
                submitBtn.classList.add("positive");

                function updateNavButtons() {
                  const hasPrev = currentPendingIndex > 0;
                  const hasNext = pendingData && currentPendingIndex < pendingData.length - 1;
                  prevBtn.disabled = !hasPrev;
                  nextBtn.disabled = !hasNext;
                  prevBtn.style.display = (pendingData && pendingData.length > 1) ? "" : "none";
                  nextBtn.style.display = (pendingData && pendingData.length > 1) ? "" : "none";
                  if (!pendingData || pendingData.length === 0) {
                    submitBtn.disabled = true;
                  } else {
                    submitBtn.disabled = false;
                  }
                }

                updateNavButtons();

                // Auto-save current draft to server
                async function saveDraft() {
                  if (!pendingData || pendingData.length === 0) return;
                  const question = pendingData[currentPendingIndex];
                  const draft = responseWords?.value?.trim() || "";
                  if (draft || question.draftAnswer) {
                    await userRequest("POST", "/sotce-net/ask/" + question._id + "/save-draft", { draft });
                    question.draftAnswer = draft; // Update local cache
                  }
                }

                prevBtn.onclick = async (e) => {
                  e.preventDefault();
                  if (currentPendingIndex > 0) {
                    await saveDraft();
                    currentPendingIndex--;
                    renderRespondPage();
                    updateNavButtons();
                  }
                };

                nextBtn.onclick = async (e) => {
                  e.preventDefault();
                  if (currentPendingIndex < pendingData.length - 1) {
                    await saveDraft();
                    currentPendingIndex++;
                    renderRespondPage();
                    updateNavButtons();
                  }
                };

                nevermindBtn.onclick = async (e) => {
                  e.preventDefault();
                  await saveDraft();
                  closeRespondEditor();
                };

                nav.appendChild(nevermindBtn);
                nav.appendChild(prevBtn);
                nav.appendChild(nextBtn);
                nav.appendChild(submitBtn);

                function closeRespondEditor() {
                  document.body.classList.remove("pages-hidden");
                  document.documentElement.classList.remove("editing");
                  respondEditor.remove();
                  editorPlacemat.remove();
                  nav.remove();
                  linesLeft.remove();
                  askedBy.remove();
                  askButton?.classList.remove("deactivated");
                  respondButton?.classList.remove("deactivated");
                  wrapper.scrollTop = scrollMemory;
                  computePageLayout?.();
                  updatePath("/");
                }

                form.addEventListener("submit", async (e) => {
                  e.preventDefault();
                  if (!pendingData || pendingData.length === 0) return;

                  const answer = responseWords?.value?.trim();
                  if (!answer) {
                    alert("Please enter a response.");
                    return;
                  }

                  const question = pendingData[currentPendingIndex];
                  veil();
                  const res = await userRequest("POST", "/sotce-net/ask/" + question._id + "/respond", { answer });
                  unveil({ instant: true });

                  if (res.status === 200) {
                    pendingData.splice(currentPendingIndex, 1);
                    if (currentPendingIndex >= pendingData.length && pendingData.length > 0) {
                      currentPendingIndex = pendingData.length - 1;
                    }
                    renderRespondPage();
                    updateNavButtons();
                    if (pendingData.length === 0) {
                      closeRespondEditor();
                    }
                  } else {
                    alert("Error: " + (res.message || "Could not submit response."));
                  }
                });

                const scrollbarWidth = wrapper.offsetWidth - wrapper.clientWidth;
                submitBtn.style.marginRight = scrollbarWidth / 1.5 + "px";

                respondEditor.appendChild(form);
                respondEditor.appendChild(linesLeft);
                respondEditor.appendChild(askedBy);
                g.appendChild(nav);

                document.documentElement.classList.add("editing");

                g.appendChild(editorPlacemat);
                g.appendChild(respondEditor);
                document.body.classList.add("pages-hidden");

                // Scale the page
                const baseWidth = 100 * 8;
                const goalWidth = respondPage.parentElement.clientWidth;
                const scale = goalWidth / baseWidth;
                respondPage.style.transform = "scale(" + scale + ")";

                askButton?.classList.add("deactivated");
                respondButton?.classList.add("deactivated");
                updatePath("/respond");
              }

              // Set button handlers
              if (askButton) {
                askButton.onclick = openAskEditor;
              }
              if (respondButton) {
                respondButton.onclick = openRespondEditor;
              }

              // Auto-open /ask route
              if (path === "/ask") {
                const observer = new MutationObserver((mutationsList, observer) => {
                  for (const mutation of mutationsList) {
                    if (mutation.type === "childList" && Array.from(mutation.addedNodes).includes(g)) {
                      openAskEditor();
                      observer.disconnect();
                      break;
                    }
                  }
                });
                observer.observe(wrapper, { childList: true, subtree: true });
                if (wrapper.contains(g)) {
                  openAskEditor();
                  observer.disconnect();
                }
              }

              // Auto-open /respond route for admins
              if (subscription?.admin && path === "/respond") {
                const observer = new MutationObserver((mutationsList, observer) => {
                  for (const mutation of mutationsList) {
                    if (mutation.type === "childList" && Array.from(mutation.addedNodes).includes(g)) {
                      openRespondEditor();
                      observer.disconnect();
                      break;
                    }
                  }
                });
                observer.observe(wrapper, { childList: true, subtree: true });
                if (wrapper.contains(g)) {
                  openRespondEditor();
                  observer.disconnect();
                }
              }

              if (askButton) topBar.appendChild(askButton);
              if (respondButton) topBar.appendChild(respondButton);

              // 🪷 write-a-page - Create compose form.
              if (subscription?.admin) {
                const writeButton = cel("button");
                writeButton.id = "write-a-page";
                writeButton.innerText = "write a page"; // "remember"; // "write a page"; // or "page" or "prayer";

                // const purposes = ["page", "poem", "prayer"];
                // let currentPurpose = 0;
                // const writeButtonInterval = setInterval(() => {
                //   if (!document.body.contains(writeButton)) {
                //     clearInterval(writeButtonInterval);
                //     return;
                //   }
                //   currentPurpose = (currentPurpose + 1) % purposes.length;
                //   writeButton.innerText = "write a " + purposes[currentPurpose];
                // }, 2000);

                writeButton.onclick = async function compose() {
                  let page;

                  // Create or retrieve the user's current draft.
                  scrollMemory = wrapper.scrollTop;
                  // TODO: 🟠 Memoize draft in ram.

                  veil();
                  const res = await userRequest(
                    "POST",
                    "/sotce-net/write-a-page",
                    { draft: "retrieve-or-create" },
                  );
                  if (res.status === 200) {
                    page = res.page;
                    // console.log("🪧 Draft:", page);
                    // window.location.reload();
                  } else {
                    console.error("🪧 Draft:", res);
                    unveil({ instant: true });
                    alert("📄 Could not start a page.");
                    return;
                  }

                  if (!page) {
                    unveil({ instant: true });
                    alert("📄 Could not start a page.");
                    return;
                  }

                  const editor = cel("div");
                  editor.id = "editor";
                  // editor.setAttribute("open", "");

                  const editorPlacemat = cel("div");
                  editorPlacemat.id = "editor-placemat";

                  const form = cel("form");
                  form.id = "editor-form";

                  const pageWrapper = cel("div");
                  pageWrapper.id = "editor-page-wrapper";

                  const editorPage = cel("div");
                  editorPage.id = "editor-page";

                  // Match the binding style width, computed from
                  const binding = document.getElementById("binding");
                  if (binding) form.style.width = binding.style.width;

                  const words = cel("textarea");
                  const wordsWrapper = cel("div");

                  // Insert whitespace tab character instead of changing
                  // element focus.
                  words.addEventListener("keydown", function (e) {
                    if (e.key === "Tab") {
                      e.preventDefault(); // Prevent the default tab behavior
                      let start = this.selectionStart;
                      let end = this.selectionEnd;
                      this.value =
                        this.value.substring(0, start) +
                        "	" +
                        this.value.substring(end);
                      this.selectionStart = this.selectionEnd = start + 1;
                    }
                  });

                  words.value = page.words; // Add words from existing draft.

                  wordsWrapper.id = "words-wrapper";

                  const linesLeft = cel("div");
                  linesLeft.id = "editor-lines-left";

                  let lastValidValue = words.value;
                  const updateLineCount = ({ lastLineRender } = {}) => {
                    const wordsStyle = window.getComputedStyle(words);
                    const pageStyle = window.getComputedStyle(editorPage);
                    const lineHeight = parseFloat(wordsStyle.lineHeight);

                    const cursorPosition = words.selectionStart;

                    let edMeasurement = editorPage.querySelector(
                      "#editor-measurement",
                    );
                    if (!edMeasurement) {
                      edMeasurement = document.createElement("div");
                      edMeasurement.id = "editor-measurement";
                    }

                    edMeasurement.style.position = "absolute";
                    edMeasurement.style.zIndex = 50;
                    edMeasurement.style.pointerEvents = "none";
                    edMeasurement.style.left = pageStyle.paddingLeft;
                    edMeasurement.style.top = pageStyle.paddingTop;
                    edMeasurement.style.whiteSpace = "pre-wrap";
                    edMeasurement.style.width = words.clientWidth + "px";
                    edMeasurement.style.font = wordsStyle.font;
                    edMeasurement.style.fontSize = wordsStyle.fontSize;
                    edMeasurement.style.lineHeight = wordsStyle.lineHeight;
                    edMeasurement.style.padding = wordsStyle.padding;
                    edMeasurement.style.margin = wordsStyle.margin;
                    edMeasurement.style.boxSizing = wordsStyle.boxSizing;
                    edMeasurement.style.textAlign = "justify";
                    edMeasurement.style.hyphens = "auto";
                    edMeasurement.style.webkitHyphens = "auto";
                    edMeasurement.style.overflowWrap = "break-word";

                    if (
                      words.value === "" &&
                      document.activeElement === words
                    ) {
                      edMeasurement.textContent = " ";
                    } else {
                      edMeasurement.textContent = words.value;

                      if (words.value.endsWith("\\n"))
                        edMeasurement.textContent += " ";
                    }

                    editorPage.appendChild(edMeasurement);

                    const contentHeight = edMeasurement.clientHeight;
                    let lineCount = round(contentHeight / lineHeight);

                    if (lineCount === 1 && words.value.length === 0)
                      lineCount = 0;

                    // Check if the line count exceeds max lines
                    if (lineCount > maxLines) {
                      words.value = lastValidValue;
                      words.setSelectionRange(
                        max(0, cursorPosition - 1),
                        max(0, cursorPosition - 1),
                      );
                    } else {
                      lastValidValue = words.value;
                    }

                    const remainingLines = maxLines - min(lineCount, maxLines);

                    if (remainingLines === 0 && lastLineRender) {
                      const { lastLineText, lastLineProgress } =
                        computeLastLineText(edMeasurement);
                      //const lastlineText = "",
                      //  lastLineProgress = 0;
                      if (needsJustification(words.value, lastLineProgress)) {
                        editorPage.classList.add("editor-justify-last-line");
                      } else {
                        editorPage.classList.remove("editor-justify-last-line");
                      }
                    } else {
                      editorPage.classList.remove("editor-justify-last-line");
                    }

                    linesLeft.classList = "";
                    if (remainingLines > 12) {
                      linesLeft.classList.add("lines-left-loads");
                    } else if (remainingLines > 8) {
                      linesLeft.classList.add("lines-left-lots");
                    } else if (remainingLines > 3) {
                      linesLeft.classList.add("lines-left-little");
                    } else if (remainingLines >= 0) {
                      linesLeft.classList.add("lines-left-few");
                    }

                    if (remainingLines === 0) {
                      linesLeft.innerText = "no lines left";
                    } else if (remainingLines === 1) {
                      linesLeft.innerText = "1 line left";
                    } else {
                      linesLeft.innerText = remainingLines + " lines left";
                    }

                    return edMeasurement;
                  };

                  words.addEventListener("input", updateLineCount);

                  //wordsWrapper.addEventListener(
                  //  "touchmove",
                  //  (event) => {
                  //    if (wordsWrapper.classList.contains("invisible")) {
                  //      event.preventDefault();
                  //    }
                  //  },
                  //  false,
                  //);

                  let down = false;

                  function checkup() {
                    if (!wordsWrapper.classList.contains("active"))
                      words.blur();
                  }

                  window.addEventListener("pointerup", checkup);

                  wordsWrapper.addEventListener("pointerenter", () => {
                    wordsWrapper.classList.add("hover");
                  });

                  wordsWrapper.addEventListener("pointerleave", () => {
                    wordsWrapper.classList.remove("hover");
                  });

                  wordsWrapper.addEventListener("pointermove", () => {
                    if (down) return;
                    wordsWrapper.classList.add("hover");
                  });

                  wordsWrapper.addEventListener("pointerdown", () => {
                    if (!wordsWrapper.classList.contains("hover")) return;
                    down = true;
                    wordsWrapper.classList.add("active");
                    wordsWrapper.classList.remove("hover");

                    // window.addEventListener(
                    //   "pointermove",
                    //   (e) => {
                    //     wordsWrapper.classList.remove("active");
                    //     wordsWrapper.classList.remove("hover");
                    //     window.removeEventListener("pointerup", release);
                    //   },
                    //   { once: true },
                    // );

                    function release(e) {
                      down = false;
                      if (e.target === wordsWrapper) {
                        words.focus();
                      }
                      wordsWrapper.classList.remove("active");
                    }

                    window.addEventListener("pointerup", release, {
                      once: true,
                    });

                    window.addEventListener("pointerup", () => (down = false), {
                      once: true,
                    });
                  });

                  wordsWrapper.classList.add("invisible");

                  words.addEventListener("focus", (e) => {
                    const edMeasurement = updateLineCount();
                    wordsWrapper.classList.remove("invisible");
                    edMeasurement.classList.add("invisible");
                    // window.scrollTo(0, 0);
                  });

                  words.addEventListener("blur", () => {
                    const edMeasurement = updateLineCount({
                      lastLineRender: true,
                    });
                    wordsWrapper.classList.add("invisible");
                    edMeasurement.classList.remove("invisible");
                    // document.body.focus();
                  });

                  window.addEventListener("resize", updateLineCount);
                  updateLineCount();

                  const nav = cel("nav");
                  nav.id = "nav-editor";
                  nav.style.width = topBar.style.width; // Match width of '#top-bar'

                  const submit = cel("button");
                  submit.type = "submit";
                  submit.setAttribute("form", form.id);
                  submit.innerText = "publish";
                  submit.id = "publish";
                  submit.classList.add("positive");

                  const keep = cel("button");
                  keep.innerText = "keep"; // draft // remember // keep
                  keep.id = "keep";
                  // keep.classList.add("maybe");

                  const crumple = cel("button");
                  crumple.innerText = "crumple"; // discard // crumple
                  crumple.classList.add("negative");
                  crumple.id = "crumple";

                  const pageTitle = cel("div");
                  pageTitle.classList.add("page-title");

                  pageTitle.innerText = dateTitle(page.when);

                  const pageNumber = cel("div");
                  pageNumber.classList.add("page-number");
                  pageNumber.innerText = "- " + (subscription.pages.length + 1) + " -";

                  editorPage.appendChild(pageTitle);
                  editorPage.appendChild(pageNumber);
                  wordsWrapper.appendChild(words);
                  editorPage.appendChild(wordsWrapper);

                  pageWrapper.appendChild(editorPage);

                  form.appendChild(pageWrapper);
                  nav.appendChild(crumple);
                  nav.appendChild(keep);
                  nav.appendChild(submit);

                  // function preventScroll(e) {
                  //   console.log(e);
                  //   e.preventDefault();
                  //   return false;
                  // }

                  function close() {
                    document.body.classList.remove("pages-hidden");
                    document.documentElement.classList.remove("editing");
                    editor.remove();
                    editorPlacemat.remove();
                    nav.remove();
                    writeButton.classList.remove("deactivated");
                    wrapper.scrollTop = scrollMemory;
                    computePageLayout?.();
                    window.removeEventListener("resize", updateLineCount);
                    window.removeEventListener("pointerup", checkup);
                    updatePath("/");
                  }

                  keep.onclick = async (e) => {
                    e.preventDefault();
                    if (words.value !== page.words) {
                      veil();
                      const res = await userRequest(
                        "POST",
                        "/sotce-net/write-a-page",
                        { draft: "keep", words: words.value },
                      );
                      if (res.status === 200) {
                        // console.log("🪧 Draft kept:", res);
                      } else {
                        console.error("🪧 Draft keep:", res);
                      }
                      unveil({ instant: true });
                    }
                    close();
                    // window.removeEventListener("scroll", preventScroll);
                  };

                  crumple.onclick = async (e) => {
                    e.preventDefault();
                    if (
                      words.value.length > 0 &&
                      !confirm("💣 Abandon this page?")
                    ) {
                      return;
                    }
                    veil();
                    const res = await userRequest(
                      "POST",
                      "/sotce-net/write-a-page",
                      { draft: "crumple", words: words.value },
                    );
                    if (res.status === 200) {
                      // console.log("🪧 Draft crumpled:", res);
                    } else {
                      console.error("🪧 Draft crumple:", res);
                    }
                    unveil({ instant: true });
                    close();
                    // window.removeEventListener("scroll", preventScroll);
                  };

                  form.addEventListener("submit", async (e) => {
                    console.log("📤 Publishing page...");
                    e.preventDefault();
                    if (words.value.trim().length === 0) {
                      alert("📃 A page cannot be empty.");
                      return;
                    }
                    if (!confirm("📰 Put this page online?")) return;
                    veil();
                    const res = await userRequest(
                      "POST",
                      "/sotce-net/write-a-page",
                      { words: words.value },
                    );
                    if (res.status === 200) {
                      console.log("🪧 Written:", res);
                      // Clear cache since new page was added
                      await clearPageCache();
                      // close();
                      unveil({ instant: true });
                      window.location.reload();
                    } else {
                      console.error("🪧 Unwritten:", res);
                    }
                  });

                  const scrollbarWidth =
                    wrapper.offsetWidth - wrapper.clientWidth;
                  // if (Safari && Desktop) scrollbarWidth = 16;
                  keep.style.marginLeft = scrollbarWidth / 1.5 + "px";
                  // linesLeft.style.marginLeft = -scrollbarWidth + "px";

                  editor.appendChild(form);
                  editor.appendChild(linesLeft);
                  g.appendChild(nav);

                  document.documentElement.classList.add("editing");

                  unveil({ instant: true });
                  g.appendChild(editorPlacemat);
                  g.appendChild(editor);
                  document.body.classList.add("pages-hidden");

                  const baseWidth = 100 * 8;
                  const goalWidth = editorPage.parentElement.clientWidth;
                  const scale = goalWidth / baseWidth;
                  editorPage.style.transform = "scale(" + scale + ")";

                  // Initialize line count
                  updateLineCount();
                  writeButton.classList.add("deactivated");
                  // window.addEventListener("scroll", preventScroll, {
                  //   passive: false,
                  // });

                  // observeAdd(words, () => {
                  // words.focus(); // Auto-focus on the words element
                  // });

                  updatePath("/write");
                };

                if (WRITING_A_PAGE || path === "/write") {
                  const observer = new MutationObserver(
                    (mutationsList, observer) => {
                      for (const mutation of mutationsList) {
                        if (
                          mutation.type === "childList" &&
                          Array.from(mutation.addedNodes).includes(g)
                        ) {
                          writeButton.click();
                          WRITING_A_PAGE = false;
                          observer.disconnect();
                          break;
                        }
                      }
                    },
                  );

                  observer.observe(wrapper, { childList: true, subtree: true });

                  // Check if 'g' is already in 'wrapper'
                  if (wrapper.contains(g)) {
                    writeButton.click();
                    WRITING_A_PAGE = false;
                    observer.disconnect();
                  }
                }

                topBar.appendChild(writeButton);
              }

              // 📦 Cache management
              const totalPages = subscription.totalPages || 0;
              const lastModified = subscription.lastModified;
              const loadedPagesData = subscription.pages || [];
              const loadedQuestionsData = subscription.questions || [];
              const totalQuestions = subscription.totalQuestions || 0;
              const pageIndex = subscription.pageIndex; // If loading specific page
              
              // Build combined feed: pages + answered questions, sorted by date
              const feedItems = [];
              
              // Add pages with type marker
              const pageStartNum = totalPages - loadedPagesData.length + 1;
              loadedPagesData.forEach((page, idx) => {
                feedItems.push({
                  ...page,
                  type: "page",
                  pageNumber: pageStartNum + idx,
                  sortDate: new Date(page.when || page.updatedAt)
                });
              });
              
              // Add questions with type marker
              let questionNum = 1;
              for (const q of loadedQuestionsData) {
                feedItems.push({
                  ...q,
                  type: "question", 
                  questionNumber: questionNum++,
                  sortDate: new Date(q.answeredAt)
                });
              }
              
              // Sort combined feed by date (newest last for chronological order)
              feedItems.sort((a, b) => a.sortDate - b.sortDate);
              
              // Assign feed indices (1-indexed)
              feedItems.forEach((item, i) => {
                item.feedIndex = i + 1;
              });
              
              const totalFeedItems = totalPages + totalQuestions;
              
              // Expose feedItems globally for ask editor to access
              window.feedItems = feedItems;
              
              console.log("📦 Feed built:", feedItems.length, "items (", totalPages, "pages +", totalQuestions, "questions)");
              console.log("📦 loadedPagesData.length:", loadedPagesData.length, "loadedQuestionsData.length:", loadedQuestionsData.length);
              console.log("📦 First few feed items:", feedItems.slice(0, 5).map(i => ({ type: i.type, feedIndex: i.feedIndex, pageNumber: i.pageNumber, questionNumber: i.questionNumber, sortDate: i.sortDate })));
              
              // Check cache validity
              const cacheMeta = await getCacheMeta();
              const cacheValid = cacheMeta && 
                cacheMeta.totalPages === totalPages && 
                cacheMeta.lastModified === lastModified;
              
              if (!cacheValid && cacheMeta) {
                console.log("📦 Cache invalidated, clearing...");
                await clearPageCache();
              }
              
              // Update cache meta
              if (totalPages > 0) {
                await setCacheMeta(totalPages, lastModified);
              }
              
              // Cache the loaded pages (by page number for backwards compat)
              for (const page of loadedPagesData) {
                const idx = pageIndex || (totalPages - loadedPagesData.length + loadedPagesData.indexOf(page) + 1);
                await setCachedPage(idx, page);
              }

              // 🎨 CANVAS-BASED PAGE RENDERING (single page + transitions)
              const USE_CANVAS_GARDEN = true; // Feature flag
              
              if (USE_CANVAS_GARDEN && (totalFeedItems > 0 || feedItems.length > 0)) {
                console.log("🎨 Using Canvas garden renderer (single page mode)");
                
                const canvas = cel("canvas");
                canvas.id = "garden-canvas";
                const ctx = canvas.getContext("2d");
                
                // Build a feed-index to item map for easy lookup
                const feedItemMap = new Map();
                feedItems.forEach(item => feedItemMap.set(item.feedIndex, item));
                
                // Total items is just the loaded items for now (not totalFeedItems which includes unfetched items)
                const loadedFeedCount = feedItems.length;
                console.log("📊 Feed counts: loaded =", loadedFeedCount, "total (pages + questions) =", totalFeedItems);
                
                // State - using feed indices (1 to loadedFeedCount)
                let currentPageIndex = loadedFeedCount; // Start at most recent loaded item
                let displayedPageIndex = loadedFeedCount;
                let transitionProgress = 0; // 0 = showing current, 1 = showing next
                let transitionDirection = 0; // -1 = prev, 0 = none, 1 = next
                let transitionTarget = null;
                let transitionSlow = false; // true when arrow keys triggered the transition
                let textFadeIn = 1; // 0 to 1, fades in text when page becomes current
                const pageCache = new Map(); // Cache by feed index
                let cardWidth = 0;
                let cardHeight = 0;
                let cardX = 0;
                let cardY = 0;
                let dpr = window.devicePixelRatio || 1;
                
                // Drag state
                let isDragging = false;
                let dragStartY = 0;
                let dragDelta = 0;
                
                // Hover state for debug boxes
                let hoverEar = false;
                let hoverPageNum = false;
                let hoverHandle = null; // Which handle is hovered on the back
                
                // Hit boxes for @handles on card back
                let handleHitBoxes = []; // [{handle, x, y, w, h}]
                // Hit boxes for @handles on question card front (in question/answer text)
                let frontHandleHitBoxes = []; // [{handle, x, y, w, h}]
                
                // Card flip animation state (full 3D card flip)
                let isFlipping = false;
                let flipProgress = 0; // 0 = front showing, 1 = back showing
                let flipDirection = 1; // 1 = flipping to back, -1 = flipping to front
                let showingBack = false; // Whether the back of the card is currently displayed
                
                // Touch data cache for showing who touched each page
                const touchCache = new Map(); // pageId -> { touches: [...], fetching: false }
                
                // Determine starting position from URL
                const pageMatch = path.match(/^\\/page\\/(\\d+)$/);
                const questionMatch = path.match(/^\\/q\\/(\\d+)$/);
                if (pageMatch) {
                  const requestedPageNum = parseInt(pageMatch[1], 10);
                  // Find feed item with this page number
                  const feedItem = feedItems.find(item => item.type === "page" && item.pageNumber === requestedPageNum);
                  if (feedItem) {
                    currentPageIndex = feedItem.feedIndex;
                    displayedPageIndex = feedItem.feedIndex;
                    console.log("📍 URL requested page", requestedPageNum, "-> feed index", feedItem.feedIndex);
                  }
                } else if (questionMatch) {
                  const requestedQNum = parseInt(questionMatch[1], 10);
                  // Find feed item with this question number
                  const feedItem = feedItems.find(item => item.type === "question" && item.questionNumber === requestedQNum);
                  if (feedItem) {
                    currentPageIndex = feedItem.feedIndex;
                    displayedPageIndex = feedItem.feedIndex;
                    console.log("📍 URL requested question", requestedQNum, "-> feed index", feedItem.feedIndex);
                  }
                }
                
                // Cache feed items by feed index
                console.log("🗃️ Caching", feedItems.length, "feed items:");
                feedItems.forEach(item => {
                  console.log("  -", item.feedIndex, ":", item.type, item.type === "question" ? item.question?.slice(0, 30) + "..." : item.when);
                  pageCache.set(item.feedIndex, item);
                });
                console.log("🗃️ pageCache size after init:", pageCache.size);
                
                // 🎨 Theme colors reader - gets CSS custom properties for canvas rendering
                function getThemeColors() {
                  const style = getComputedStyle(document.documentElement);
                  return {
                    gardenBackground: style.getPropertyValue('--garden-background').trim() || '#FFD1DC',
                    cardBackground: style.getPropertyValue('--card-background').trim() || '#f8f4ec',
                    cardBackBackground: style.getPropertyValue('--card-back-background').trim() || '#f0ebe0',
                    cardBorder: style.getPropertyValue('--card-border').trim() || '#d4c8b8',
                    cardEar: style.getPropertyValue('--card-ear').trim() || '#e8e0d0',
                    cardEarHover: style.getPropertyValue('--card-ear-hover').trim() || '#FFD1DC',
                    cardText: style.getPropertyValue('--card-text').trim() || '#000000',
                    cardTextMuted: style.getPropertyValue('--card-text-muted').trim() || '#666666',
                    cardTextDim: style.getPropertyValue('--card-text-dim').trim() || '#999999',
                    cardTextFaint: style.getPropertyValue('--card-text-faint').trim() || '#aaaaaa',
                    questionCardBackground: style.getPropertyValue('--question-card-background').trim() || '#e8f0f8',
                    questionCardBorder: style.getPropertyValue('--question-card-border').trim() || '#b8c8d8',
                    questionCardEar: style.getPropertyValue('--question-card-ear').trim() || '#d0e0f0',
                  };
                }
                
                // Cache theme colors (update on system theme change)
                let themeColors = getThemeColors();
                
                // Listen for system theme changes
                if (window.matchMedia) {
                  window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', () => {
                    themeColors = getThemeColors();
                  });
                }
                
                // Resize canvas to fill container
                function resizeCanvas() {
                  const topBarHeight = 72;
                  const bottomPadding = 32;
                  const w = window.innerWidth;
                  const h = window.innerHeight - topBarHeight;
                  dpr = window.devicePixelRatio || 1;
                  
                  canvas.width = w * dpr;
                  canvas.height = h * dpr;
                  canvas.style.width = w + "px";
                  canvas.style.height = h + "px";
                  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
                  
                  // Compute card dimensions (4:5 aspect ratio, centered)
                  const maxWidth = 600;
                  const sidePadding = 32;
                  const availableWidth = w - sidePadding * 2;
                  const availableHeight = h - bottomPadding;
                  
                  // Calculate size to fit in viewport while maintaining 4:5 ratio
                  cardWidth = Math.min(availableWidth, maxWidth);
                  cardHeight = cardWidth * (5/4);
                  
                  // If too tall, scale down
                  if (cardHeight > availableHeight) {
                    cardHeight = availableHeight;
                    cardWidth = cardHeight * (4/5);
                  }
                  
                  // Center horizontally and vertically
                  cardX = (w - cardWidth) / 2;
                  cardY = (h - cardHeight) / 2;
                  
                  console.log("🎨 Canvas resized:", w, "x", h, "card:", cardWidth, "x", cardHeight);
                }
                
                // Fetch page data (with deduplication)
                const fetchingPages = new Set();
                async function fetchPage(idx) {
                  console.log("📥 fetchPage called for idx:", idx, "inCache:", pageCache.has(idx));
                  if (pageCache.has(idx)) return pageCache.get(idx);
                  if (fetchingPages.has(idx)) return null;
                  
                  fetchingPages.add(idx);
                  try {
                    let pageData = await getCachedPage(idx);
                    console.log("📥 IndexedDB cache result for idx:", idx, "found:", !!pageData);
                    if (!pageData) {
                      console.log("📥 Fetching from server, pageNumber:", idx);
                      const response = await subscribed({ pageNumber: idx, limit: 1 });
                      console.log("📥 Server response:", response?.pages?.length, "pages");
                      if (response?.pages?.[0]) {
                        pageData = response.pages[0];
                        await setCachedPage(idx, pageData);
                      }
                    }
                    if (pageData) pageCache.set(idx, pageData);
                    return pageData;
                  } finally {
                    fetchingPages.delete(idx);
                  }
                }
                
                // Prefetch nearby feed items
                function prefetchPages(centerIdx) {
                  [centerIdx - 1, centerIdx, centerIdx + 1].forEach(idx => {
                    if (idx >= 1 && idx <= loadedFeedCount && !pageCache.has(idx)) {
                      fetchPage(idx);
                    }
                  });
                }
                
                // Text wrapping helper - handles newlines and word wrap
                function wrapText(text, maxWidth, fontSize) {
                  const paragraphs = (text || "").split("\\n");
                  const lines = [];
                  
                  for (const paragraph of paragraphs) {
                    if (paragraph.trim() === "") {
                      // Empty line / paragraph break
                      lines.push("");
                      continue;
                    }
                    
                    const words = paragraph.split(" ");
                    let currentLine = "";
                    
                    for (const word of words) {
                      const testLine = currentLine ? currentLine + " " + word : word;
                      const metrics = ctx.measureText(testLine);
                      if (metrics.width > maxWidth && currentLine) {
                        lines.push(currentLine);
                        currentLine = word;
                      } else {
                        currentLine = testLine;
                      }
                    }
                    if (currentLine) lines.push(currentLine);
                  }
                  return lines;
                }
                
                // Render a single page at position (ghost = blank card, textOpacity for fade)
                function renderPage(pageData, idx, offsetY = 0, ghost = false, textOpacity = 1) {
                  const x = cardX;
                  const y = cardY + offsetY;
                  const w = cardWidth;
                  const h = cardHeight;
                  
                  // Font metrics needed for layout - scale proportionally with no minimum
                  const fontSize = (w / 600) * 17;
                  const em = fontSize;
                  
                  // Determine if this is a question card for different coloring
                  const isQuestion = pageData?.type === "question";
                  
                  // Card background (themed - blue for questions)
                  ctx.fillStyle = isQuestion ? themeColors.questionCardBackground : themeColors.cardBackground;
                  ctx.fillRect(x, y, w, h);
                  
                  // Border (themed - blue for questions)
                  ctx.strokeStyle = isQuestion ? themeColors.questionCardBorder : themeColors.cardBorder;
                  ctx.lineWidth = 1;
                  ctx.strokeRect(x + 0.5, y + 0.5, w - 1, h - 1);
                  
                  // Ear (corner fold) - 8% width - only for diary pages, not Q&A
                  if (!isQuestion) {
                    const earSize = w * 0.08;
                    
                    // Draw ear (themed)
                    ctx.fillStyle = hoverEar && offsetY === 0 ? themeColors.cardEarHover : themeColors.cardEar;
                    ctx.beginPath();
                    ctx.moveTo(x + w - earSize, y + h);
                    ctx.lineTo(x + w, y + h - earSize);
                    ctx.lineTo(x + w, y + h);
                    ctx.closePath();
                    ctx.fill();
                    ctx.strokeStyle = themeColors.cardBorder;
                    ctx.stroke();
                  }
                  
                  // Debug box for ear when hovering (themed)
                  if (hoverEar && offsetY === 0) {
                    ctx.strokeStyle = themeColors.cardEarHover;
                    ctx.lineWidth = 2;
                    ctx.strokeRect(x + w - earSize, y + h - earSize, earSize, earSize);
                  }
                  
                  // Ghost mode = blank card, no text
                  if (ghost) return;
                  
                  if (!pageData) {
                    ctx.fillStyle = themeColors.cardTextDim;
                    ctx.font = "16px Helvetica, sans-serif";
                    ctx.textAlign = "center";
                    ctx.fillText("Loading...", x + w/2, y + h/2);
                    ctx.textAlign = "left";
                    return;
                  }
                  
                  // Font metrics already defined at top of function
                  const lineHeight = fontSize * 1.76; // --line-height: 1.76em
                  const padding = em * 2; // padding: 0 2em
                  const textWidth = w - padding * 2;
                  const maxLines = 19; // --max-lines: 19
                  
                  // Text color with opacity for fade-in (themed)
                  const baseColor = themeColors.cardText;
                  let textColor;
                  if (textOpacity < 1) {
                    // Parse hex color and add alpha
                    const r = parseInt(baseColor.slice(1, 3), 16);
                    const g = parseInt(baseColor.slice(3, 5), 16);
                    const b = parseInt(baseColor.slice(5, 7), 16);
                    textColor = \`rgba(\${r}, \${g}, \${b}, \${textOpacity})\`;
                  } else {
                    textColor = baseColor;
                  }
                  
                  // Different rendering for questions vs pages (isQuestion already defined at top)
                  if (isQuestion) {
                    // Helper: draw a line with @handles in pink
                    function drawLineWithHandles(line, lineX, lineY, baseFont, pinkColor) {
                      const handleRegex = /@[a-zA-Z0-9_-]+/g;
                      let match;
                      let lastIndex = 0;
                      let cursorX = lineX;
                      
                      while ((match = handleRegex.exec(line)) !== null) {
                        // Draw text before handle
                        const before = line.slice(lastIndex, match.index);
                        if (before) {
                          ctx.fillStyle = textColor;
                          ctx.font = baseFont;
                          ctx.fillText(before, cursorX, lineY);
                          cursorX += ctx.measureText(before).width;
                        }
                        // Draw handle in pink
                        const handle = match[0];
                        ctx.fillStyle = pinkColor;
                        ctx.font = baseFont;
                        ctx.fillText(handle, cursorX, lineY);
                        const handleWidth = ctx.measureText(handle).width;
                        // Track hit box for click detection
                        if (offsetY === 0) {
                          frontHandleHitBoxes.push({
                            handle: handle,
                            x: cursorX,
                            y: lineY - fontSize,
                            w: handleWidth,
                            h: fontSize * 1.4
                          });
                        }
                        cursorX += handleWidth;
                        lastIndex = match.index + handle.length;
                      }
                      // Draw remaining text
                      const remaining = line.slice(lastIndex);
                      if (remaining) {
                        ctx.fillStyle = textColor;
                        ctx.font = baseFont;
                        ctx.fillText(remaining, cursorX, lineY);
                      }
                    }
                    
                    // Clear front handle hit boxes (rebuilt each frame)
                    if (offsetY === 0) frontHandleHitBoxes = [];
                    
                    const pinkHandleColor = "rgb(200, 80, 120)";
                    
                    // QUESTION RENDERING
                    // Header: question text (smaller, italic)
                    const headerY = y + h * 0.065 + fontSize;
                    const headerFont = "italic " + (fontSize * 0.9) + "px Helvetica, sans-serif";
                    ctx.textAlign = "left";
                    
                    // Wrap question text for header
                    const questionLines = wrapText(pageData.question, textWidth, fontSize * 0.9);
                    const maxHeaderLines = 3; // Limit header to 3 lines
                    
                    for (let i = 0; i < Math.min(questionLines.length, maxHeaderLines); i++) {
                      drawLineWithHandles(questionLines[i], x + padding, headerY + i * (fontSize * 1.5), headerFont, pinkHandleColor);
                    }
                    
                    // Body: answer text - starts lower to accommodate question header
                    const bodyFont = fontSize + "px Helvetica, sans-serif";
                    ctx.textAlign = "left";
                    
                    const answerStartY = y + h * 0.20 + fontSize; // Start a bit lower
                    const answerLines = wrapText(pageData.answer || "", textWidth, fontSize);
                    
                    for (let i = 0; i < Math.min(answerLines.length, maxLines - 2); i++) {
                      const line = answerLines[i];
                      if (line === "") continue;
                      drawLineWithHandles(line, x + padding, answerStartY + i * lineHeight, bodyFont, pinkHandleColor);
                    }
                    
                    // Page number with asterisk format: *N* (use questionNumber)
                    ctx.fillStyle = textColor;
                    ctx.font = fontSize + "px monospace";
                    ctx.textAlign = "center";
                    const pageNumY = y + h - em * 2;
                    const displayNum = pageData.questionNumber || idx;
                    const pageNumText = "*" + displayNum + "*";
                    ctx.fillText(pageNumText, x + w/2, pageNumY);
                    
                    // Debug box for page number when hovering (themed)
                    if (hoverPageNum && offsetY === 0) {
                      const textMetrics = ctx.measureText(pageNumText);
                      const boxWidth = textMetrics.width + em;
                      const boxHeight = em * 1.5;
                      ctx.strokeStyle = themeColors.cardEarHover;
                      ctx.lineWidth = 2;
                      ctx.strokeRect(x + w/2 - boxWidth/2, pageNumY - em, boxWidth, boxHeight);
                    }
                  } else {
                    // PAGE RENDERING (diary pages)
                    // Date title - CENTERED at top: 6.5%
                    const title = dateTitle(pageData.when);
                    const titleY = y + h * 0.065 + fontSize;
                    ctx.fillStyle = textColor;
                    ctx.font = fontSize + "px Helvetica, sans-serif";
                    ctx.textAlign = "center";
                    ctx.fillText(title, x + w/2, titleY);
                    
                    // Body text - margin-top: 15%
                    ctx.fillStyle = textColor;
                    ctx.font = fontSize + "px Helvetica, sans-serif";
                    ctx.textAlign = "left";
                    
                    const lines = wrapText(pageData.words, textWidth, fontSize);
                    const textStartY = y + h * 0.15 + fontSize;
                    
                    for (let i = 0; i < Math.min(lines.length, maxLines); i++) {
                      const line = lines[i];
                      if (line === "") {
                        // Empty line for paragraph break
                        continue;
                      }
                      ctx.fillText(line, x + padding, textStartY + i * lineHeight);
                    }
                    
                    // Page number - centered at bottom with margin (use pageNumber)
                    ctx.fillStyle = textColor;
                    ctx.font = fontSize + "px monospace";
                    ctx.textAlign = "center";
                    const pageNumY = y + h - em * 2;
                    const displayNum = pageData.pageNumber || idx;
                    ctx.fillText("- " + displayNum + " -", x + w/2, pageNumY);
                    
                    // Debug box for page number when hovering (themed)
                    if (hoverPageNum && offsetY === 0) {
                      const pageNumText = "- " + displayNum + " -";
                      const textMetrics = ctx.measureText(pageNumText);
                      const boxWidth = textMetrics.width + em;
                      const boxHeight = em * 1.5;
                      ctx.strokeStyle = themeColors.cardEarHover;
                      ctx.lineWidth = 2;
                      ctx.strokeRect(x + w/2 - boxWidth/2, pageNumY - em, boxWidth, boxHeight);
                    }
                  }
                  
                  ctx.textAlign = "left";
                }
                
                // Render the back of a card (touch info only, positioned top-left at body text position)
                function renderCardBack(pageData, idx) {
                  const x = cardX;
                  const y = cardY;
                  const w = cardWidth;
                  const h = cardHeight;
                  
                  // Font metrics (same as front)
                  const fontSize = (w / 600) * 17;
                  const em = fontSize;
                  const lineHeight = fontSize * 1.76;
                  const padding = em * 2;
                  
                  // Card back background (themed)
                  ctx.fillStyle = themeColors.cardBackBackground;
                  ctx.fillRect(x, y, w, h);
                  
                  // Border (themed)
                  ctx.strokeStyle = themeColors.cardBorder;
                  ctx.lineWidth = 1;
                  ctx.strokeRect(x + 0.5, y + 0.5, w - 1, h - 1);
                  
                  // Ear on back (bottom-left, mirrored, themed)
                  const earSize = w * 0.08;
                  ctx.fillStyle = hoverEar ? themeColors.cardEarHover : themeColors.cardEar;
                  ctx.beginPath();
                  ctx.moveTo(x + earSize, y + h);
                  ctx.lineTo(x, y + h - earSize);
                  ctx.lineTo(x, y + h);
                  ctx.closePath();
                  ctx.fill();
                  ctx.strokeStyle = themeColors.cardBorder;
                  ctx.stroke();
                  
                  if (!pageData) return;
                  
                  // Body text position (same as front - margin-top: 15%)
                  const textStartY = y + h * 0.15 + fontSize;
                  const textWidth = w - padding * 2;
                  
                  // Touches section - top left, at body text position
                  const pageId = pageData._id;
                  const touchData = touchCache.get(pageId);
                  
                  ctx.font = fontSize + "px Helvetica, sans-serif";
                  ctx.textAlign = "left";
                  
                  let textY = textStartY;
                  
                  if (touchData?.fetching) {
                    ctx.fillStyle = themeColors.cardTextDim;
                    ctx.fillText("Loading...", x + padding, textY);
                  } else if (touchData?.touches && touchData.touches.length > 0) {
                    const touches = touchData.touches;
                    let touchedBy = "";
                    if (touches.length === 1) {
                      touchedBy = touches[0] + " touched this page.";
                    } else if (touches.length === 2) {
                      touchedBy = touches[0] + " and " + touches[1] + " touched this page.";
                    } else {
                      const lastTouch = touches[touches.length - 1];
                      const others = touches.slice(0, -1);
                      touchedBy = others.join(", ") + ", and " + lastTouch + " touched this page.";
                    }
                    
                    // Word wrap touch text, tracking @handle positions for hit testing
                    handleHitBoxes = [];
                    const allWords = touchedBy.split(" ");
                    let line = "";
                    let lineWords = [];
                    
                    function flushLine(lineStr, lineY, wordsInLine) {
                      // Measure each word to find @handle positions
                      let cursorX = x + padding;
                      for (const lw of wordsInLine) {
                        const wordWidth = ctx.measureText(lw).width;
                        const spaceWidth = ctx.measureText(" ").width;
                        if (lw.startsWith("@")) {
                          // Draw handle in pink
                          ctx.fillStyle = "rgb(200, 80, 120)";
                          ctx.fillText(lw, cursorX, lineY);
                          ctx.fillStyle = themeColors.cardTextMuted;
                          handleHitBoxes.push({
                            handle: lw,
                            x: cursorX,
                            y: lineY - fontSize,
                            w: wordWidth,
                            h: fontSize * 1.4
                          });
                        } else {
                          ctx.fillText(lw, cursorX, lineY);
                        }
                        cursorX += wordWidth + spaceWidth;
                      }
                    }
                    
                    ctx.fillStyle = themeColors.cardTextMuted;
                    for (const word of allWords) {
                      const testLine = line ? line + " " + word : word;
                      if (ctx.measureText(testLine).width > textWidth && line) {
                        flushLine(line, textY, lineWords);
                        textY += lineHeight;
                        line = word;
                        lineWords = [word];
                      } else {
                        line = testLine;
                        lineWords.push(word);
                      }
                    }
                    if (line) flushLine(line, textY, lineWords);
                  } else {
                    ctx.fillStyle = themeColors.cardTextFaint;
                    ctx.fillText("No one has touched this page yet.", x + padding, textY);
                  }
                }
                
                // Main render function
                let lastLoggedIdx = -1;
                function render() {
                  const w = canvas.width / dpr;
                  const h = canvas.height / dpr;
                  
                  // Clear with garden background color (themed)
                  ctx.fillStyle = themeColors.gardenBackground;
                  ctx.fillRect(0, 0, w, h);
                  
                  const pageData = pageCache.get(displayedPageIndex);
                  if (displayedPageIndex !== lastLoggedIdx) {
                    console.log("🎨 Rendering idx:", displayedPageIndex, "hasData:", !!pageData, "type:", pageData?.type, "cacheSize:", pageCache.size);
                    lastLoggedIdx = displayedPageIndex;
                  }
                  
                  // Handle card flip animation with 3D perspective (no zoom, just rotation)
                  if (isFlipping || showingBack) {
                    // Calculate rotation angle (0 to PI)
                    const angle = flipProgress * Math.PI;
                    const isFrontVisible = flipProgress < 0.5;
                    
                    const centerX = cardX + cardWidth / 2;
                    
                    // When showing back, render the front first (semi-transparent)
                    if (!isFrontVisible) {
                      ctx.save();
                      ctx.globalAlpha = 0.15; // Semi-transparent front showing through
                      
                      // Front face scale (it's on the "back" side now)
                      const frontAngle = angle - Math.PI;
                      const frontScaleX = Math.abs(Math.cos(frontAngle));
                      
                      ctx.translate(centerX, 0);
                      ctx.scale(frontScaleX, 1);
                      ctx.translate(-centerX, 0);
                      
                      if (frontScaleX > 0.01) {
                        renderPage(pageData, displayedPageIndex, 0, false, 1);
                      }
                      ctx.restore();
                    }
                    
                    // Render the main visible side
                    ctx.save();
                    
                    // Simple horizontal scale to simulate Y-axis rotation (no zoom)
                    const scaleX = Math.abs(Math.cos(angle));
                    
                    ctx.translate(centerX, 0);
                    ctx.scale(scaleX, 1);
                    ctx.translate(-centerX, 0);
                    
                    // Only render if card has some width
                    if (scaleX > 0.01) {
                      if (isFrontVisible) {
                        renderPage(pageData, displayedPageIndex, 0, false, 1);
                      } else {
                        renderCardBack(pageData, displayedPageIndex);
                      }
                    }
                    
                    ctx.restore();
                    
                    // Fetch current page if not cached
                    if (!pageData) fetchPage(displayedPageIndex);
                    return;
                  }
                  
                  if (transitionDirection !== 0 && transitionTarget !== null) {
                    // Animating transition - both pages show text (pre-rendered)
                    const slideDistance = cardHeight + 40;
                    const incomingData = pageCache.get(transitionTarget) || null;
                    
                    if (transitionDirection > 0) {
                      // Going to higher page (next) - current slides up, next comes from below
                      renderPage(pageData, displayedPageIndex, -transitionProgress * slideDistance, false, 1);
                      renderPage(incomingData, transitionTarget, (1 - transitionProgress) * slideDistance, false, 1);
                    } else {
                      // Going to lower page (prev) - current slides down, prev comes from above
                      renderPage(pageData, displayedPageIndex, transitionProgress * slideDistance, false, 1);
                      renderPage(incomingData, transitionTarget, -(1 - transitionProgress) * slideDistance, false, 1);
                    }
                  } else if (isDragging && Math.abs(dragDelta) > 0) {
                    // Dragging - both feed items show text (pre-rendered)
                    const nextIdx = dragDelta > 0 ? displayedPageIndex + 1 : displayedPageIndex - 1;
                    if (nextIdx >= 1 && nextIdx <= loadedFeedCount) {
                      const slideDistance = cardHeight + 40;
                      const progress = Math.min(1, Math.abs(dragDelta) / slideDistance);
                      const nextData = pageCache.get(nextIdx) || null;
                      
                      if (dragDelta > 0) {
                        renderPage(pageData, displayedPageIndex, -progress * slideDistance, false, 1);
                        renderPage(nextData, nextIdx, (1 - progress) * slideDistance, false, 1);
                      } else {
                        renderPage(pageData, displayedPageIndex, progress * slideDistance, false, 1);
                        renderPage(nextData, nextIdx, -(1 - progress) * slideDistance, false, 1);
                      }
                    } else {
                      // At boundary - just offset current page with resistance
                      renderPage(pageData, displayedPageIndex, -dragDelta * 0.3);
                    }
                  } else {
                    // Static - show current page with text (fade in if just arrived)
                    renderPage(pageData, displayedPageIndex, 0, false, textFadeIn);
                  }
                  
                  // Fetch current page if not cached
                  if (!pageData) fetchPage(displayedPageIndex);
                }
                
                // Animation update
                function update() {
                  if (transitionDirection !== 0 && transitionTarget !== null) {
                    transitionProgress += transitionSlow ? 0.045 : 0.12; // Slower for arrow keys
                    
                    if (transitionProgress >= 1) {
                      // Transition complete
                      displayedPageIndex = transitionTarget;
                      currentPageIndex = transitionTarget;
                      transitionProgress = 0;
                      transitionDirection = 0;
                      transitionTarget = null;
                      transitionSlow = false;
                      textFadeIn = 1; // Text already visible, no fade needed
                      // Update URL based on item type (question vs page)
                      const currentItem = pageCache.get(currentPageIndex);
                      if (currentItem?.type === "question") {
                        const qNum = currentItem.questionNumber || currentPageIndex;
                        updatePath("/q/" + qNum);
                      } else {
                        const pNum = currentItem?.pageNumber || currentPageIndex;
                        updatePath("/page/" + pNum);
                      }
                      prefetchPages(currentPageIndex);
                    }
                  }
                  
                  // Fade in text when static
                  if (transitionDirection === 0 && textFadeIn < 1) {
                    textFadeIn = Math.min(1, textFadeIn + 0.08);
                  }
                  
                  // Card flip animation
                  if (isFlipping) {
                    flipProgress += 0.04 * flipDirection; // Smooth flip speed
                    if (flipProgress >= 1) {
                      flipProgress = 1;
                      isFlipping = false;
                      showingBack = true;
                      // Card is now showing back - it stays there until user clicks again
                    } else if (flipProgress <= 0) {
                      flipProgress = 0;
                      isFlipping = false;
                      showingBack = false;
                      flipDirection = 1;
                    }
                  }
                }
                
                // Animation loop
                let running = true;
                function loop() {
                  if (!running) return;
                  update();
                  render();
                  requestAnimationFrame(loop);
                }
                
                // Go to a specific feed item with animation
                function goToPage(targetIdx, startProgress = 0, slow = false) {
                  if (targetIdx < 1 || targetIdx > loadedFeedCount) return;
                  if (targetIdx === displayedPageIndex) return;
                  if (transitionDirection !== 0) return; // Already animating
                  if (isFlipping || showingBack) return; // Don't change pages while flipped
                  
                  transitionSlow = slow;
                  transitionDirection = targetIdx > displayedPageIndex ? 1 : -1;
                  transitionTarget = targetIdx;
                  transitionProgress = startProgress; // Start from where drag left off
                  prefetchPages(targetIdx);
                }
                
                // Input handling
                canvas.addEventListener("pointerdown", (e) => {
                  if (transitionDirection !== 0) return; // Don't drag during animation
                  if (isFlipping || showingBack) return; // Don't drag when flipped
                  
                  isDragging = true;
                  dragStartY = e.clientY;
                  dragDelta = 0;
                  
                  canvas.setPointerCapture(e.pointerId);
                  canvas.style.cursor = "grabbing";
                  e.preventDefault();
                });
                
                canvas.addEventListener("pointermove", (e) => {
                  if (!isDragging) return;
                  dragDelta = dragStartY - e.clientY;
                  
                  // Prefetch the feed item we might be going to
                  const nextIdx = dragDelta > 0 ? displayedPageIndex + 1 : displayedPageIndex - 1;
                  if (nextIdx >= 1 && nextIdx <= loadedFeedCount && !pageCache.has(nextIdx)) {
                    fetchPage(nextIdx);
                  }
                });
                
                canvas.addEventListener("pointerup", (e) => {
                  if (!isDragging) return;
                  isDragging = false;
                  canvas.releasePointerCapture(e.pointerId);
                  canvas.style.cursor = "grab";
                  
                  const threshold = cardHeight * 0.2; // 20% of card height to trigger
                  const slideDistance = cardHeight + 40;
                  const currentProgress = Math.min(1, Math.abs(dragDelta) / slideDistance);
                  
                  if (Math.abs(dragDelta) > threshold) {
                    // Commit to page change - continue from current drag position
                    const nextIdx = dragDelta > 0 ? displayedPageIndex + 1 : displayedPageIndex - 1;
                    if (nextIdx >= 1 && nextIdx <= loadedFeedCount) {
                      goToPage(nextIdx, currentProgress);
                    }
                  }
                  // If threshold not met, render() will snap back automatically
                  dragDelta = 0;
                });
                
                canvas.addEventListener("pointercancel", (e) => {
                  if (!isDragging) return;
                  isDragging = false;
                  canvas.releasePointerCapture(e.pointerId);
                  canvas.style.cursor = "grab";
                  dragDelta = 0;
                });
                
                // Keyboard navigation
                document.addEventListener("keydown", (e) => {
                  if (!document.body.contains(canvas)) return;
                  if (e.target.tagName === "INPUT" || e.target.tagName === "TEXTAREA") return;
                  if (transitionDirection !== 0) return;
                  if (isFlipping || showingBack) return; // Don't navigate when flipped
                  
                  if (e.key === "ArrowUp" || e.key === "ArrowLeft") {
                    e.preventDefault();
                    goToPage(currentPageIndex - 1, 0, true);
                  } else if (e.key === "ArrowDown" || e.key === "ArrowRight") {
                    e.preventDefault();
                    goToPage(currentPageIndex + 1, 0, true);
                  }
                });
                
                // Click detection for ear and page number
                canvas.addEventListener("click", (e) => {
                  if (Math.abs(dragDelta) > 5) return; // Was dragging
                  
                  const rect = canvas.getBoundingClientRect();
                  const x = e.clientX - rect.left;
                  const y = e.clientY - rect.top;
                  
                  // Check if click is within card bounds
                  if (x < cardX || x > cardX + cardWidth) return;
                  if (y < cardY || y > cardY + cardHeight) return;
                  
                  const localX = x - cardX;
                  const localY = y - cardY;
                  
                  // Font metrics for hit detection (must match hover)
                  const baseFontSize = (cardWidth / 600) * 17;
                  const em = Math.max(10, baseFontSize);
                  const earSize = cardWidth * 0.08;
                  
                  // Check ear region - depends on which side of card is showing
                  // Front: bottom-right, Back: bottom-left (mirrored)
                  const earHit = showingBack
                    ? (localX < earSize && localY > cardHeight - earSize)
                    : (localX > cardWidth - earSize && localY > cardHeight - earSize);
                  
                  if (earHit) {
                    console.log("🎨 Ear clicked on page", displayedPageIndex, showingBack ? "(back)" : "(front)");
                    
                    // If showing front, flip to back and touch the page
                    if (!showingBack && !isFlipping) {
                      isFlipping = true;
                      flipProgress = 0;
                      flipDirection = 1;
                      
                      // Touch the page (send to database)
                      const pageData = pageCache.get(displayedPageIndex);
                      if (pageData?._id) {
                        const pageId = pageData._id;
                        // Mark as fetching
                        touchCache.set(pageId, { touches: [], fetching: true });
                        
                        // Make API call to touch the page
                        userRequest("POST", "/sotce-net/touch-a-page", { _id: pageId })
                          .then(res => {
                            if (res.status === 200) {
                              touchCache.set(pageId, { touches: res.touches || [], fetching: false });
                            } else {
                              touchCache.set(pageId, { touches: [], fetching: false });
                            }
                          })
                          .catch(err => {
                            console.error("Touch error:", err);
                            touchCache.set(pageId, { touches: [], fetching: false });
                          });
                      }
                    }
                    // If showing back, flip back to front
                    else if (showingBack && !isFlipping) {
                      isFlipping = true;
                      flipProgress = 1;
                      flipDirection = -1;
                    }
                    return;
                  }
                  
                  // Check @handle hit boxes on question card front
                  if (!showingBack && frontHandleHitBoxes.length > 0) {
                    const absX = e.clientX - rect.left;
                    const absY = e.clientY - rect.top;
                    for (const hb of frontHandleHitBoxes) {
                      if (absX >= hb.x && absX <= hb.x + hb.w && absY >= hb.y && absY <= hb.y + hb.h) {
                        console.log("🎨 Handle clicked on question card:", hb.handle);
                        openChatWithMessage(hb.handle + " ");
                        return;
                      }
                    }
                  }
                  
                  // Check page number region (must match hover detection) - only on front
                  if (!showingBack) {
                    const pageNumTop = cardHeight - em * 3;
                    const pageNumBottom = cardHeight - em * 0.5;
                    if (localY > pageNumTop && localY < pageNumBottom) {
                      const pageData = pageCache.get(displayedPageIndex);
                      const isQuestion = pageData?.type === "question";
                      const displayNum = isQuestion ? (pageData?.questionNumber || displayedPageIndex) : (pageData?.pageNumber || displayedPageIndex);
                      const pageRef = isQuestion 
                        ? "*" + displayNum + "* " 
                        : "-" + displayNum + "- ";
                      console.log("🎨 Page number clicked:", displayNum, "type:", pageData?.type);
                      openChatWithMessage(pageRef);
                      return;
                    }
                  }
                  
                  // Check handle hit boxes on back
                  if (showingBack && handleHitBoxes.length > 0) {
                    const absX = e.clientX - rect.left;
                    const absY = e.clientY - rect.top;
                    for (const hb of handleHitBoxes) {
                      if (absX >= hb.x && absX <= hb.x + hb.w && absY >= hb.y && absY <= hb.y + hb.h) {
                        console.log("🎨 Handle clicked:", hb.handle);
                        openChatWithMessage(hb.handle + " ");
                        return;
                      }
                    }
                  }
                });
                
                // Hover cursor changes for ear and page number
                canvas.addEventListener("mousemove", (e) => {
                  if (isDragging) {
                    hoverEar = false;
                    hoverPageNum = false;
                    return;
                  }
                  
                  const rect = canvas.getBoundingClientRect();
                  const x = e.clientX - rect.left;
                  const y = e.clientY - rect.top;
                  
                  // Check if within card bounds
                  if (x < cardX || x > cardX + cardWidth || y < cardY || y > cardY + cardHeight) {
                    canvas.style.cursor = showingBack ? "default" : "grab";
                    hoverEar = false;
                    hoverPageNum = false;
                    return;
                  }
                  
                  const localX = x - cardX;
                  const localY = y - cardY;
                  
                  // Font metrics for hit detection
                  const baseFontSize = (cardWidth / 600) * 17;
                  const em = Math.max(10, baseFontSize);
                  const earSize = cardWidth * 0.08;
                  
                  // Check ear region - depends on which side is showing
                  const earHit = showingBack
                    ? (localX < earSize && localY > cardHeight - earSize)
                    : (localX > cardWidth - earSize && localY > cardHeight - earSize);
                  
                  if (earHit) {
                    canvas.style.cursor = "pointer";
                    hoverEar = true;
                    hoverPageNum = false;
                    return;
                  }
                  
                  // Check page number region (only on front)
                  if (!showingBack) {
                    const pageNumTop = cardHeight - em * 3;
                    const pageNumBottom = cardHeight - em * 0.5;
                    if (localY > pageNumTop && localY < pageNumBottom) {
                      canvas.style.cursor = "pointer";
                      hoverPageNum = true;
                      hoverEar = false;
                      return;
                    }
                  }
                  
                  hoverEar = false;
                  hoverPageNum = false;
                  hoverHandle = null;
                  
                  // Check handle hit boxes on back
                  if (showingBack && handleHitBoxes.length > 0) {
                    for (const hb of handleHitBoxes) {
                      if (x >= hb.x && x <= hb.x + hb.w && y >= hb.y && y <= hb.y + hb.h) {
                        canvas.style.cursor = "pointer";
                        hoverHandle = hb.handle;
                        return;
                      }
                    }
                  }
                  
                  // Check @handle hit boxes on question card front
                  if (!showingBack && frontHandleHitBoxes.length > 0) {
                    for (const hb of frontHandleHitBoxes) {
                      if (x >= hb.x && x <= hb.x + hb.w && y >= hb.y && y <= hb.y + hb.h) {
                        canvas.style.cursor = "pointer";
                        return;
                      }
                    }
                  }
                  
                  canvas.style.cursor = showingBack ? "default" : "grab";
                });
                
                // Reset hover state when leaving canvas
                canvas.addEventListener("mouseleave", () => {
                  hoverEar = false;
                  hoverPageNum = false;
                  canvas.style.cursor = "grab";
                });
                
                // Touch support for hover highlight (show on touch start, hide on touch end)
                canvas.addEventListener("touchstart", (e) => {
                  if (e.touches.length !== 1) return;
                  const touch = e.touches[0];
                  const rect = canvas.getBoundingClientRect();
                  const x = touch.clientX - rect.left;
                  const y = touch.clientY - rect.top;
                  
                  if (x < cardX || x > cardX + cardWidth || y < cardY || y > cardY + cardHeight) return;
                  
                  const localX = x - cardX;
                  const localY = y - cardY;
                  
                  const baseFontSize = (cardWidth / 600) * 17;
                  const em = Math.max(10, baseFontSize);
                  const earSize = cardWidth * 0.08;
                  
                  // Check ear region - depends on which side is showing
                  const earHit = showingBack
                    ? (localX < earSize && localY > cardHeight - earSize)
                    : (localX > cardWidth - earSize && localY > cardHeight - earSize);
                  
                  if (earHit) {
                    hoverEar = true;
                    hoverPageNum = false;
                  } else if (!showingBack) {
                    const pageNumTop = cardHeight - em * 3;
                    const pageNumBottom = cardHeight - em * 0.5;
                    if (localY > pageNumTop && localY < pageNumBottom) {
                      hoverPageNum = true;
                      hoverEar = false;
                    }
                  }
                }, { passive: true });
                
                canvas.addEventListener("touchend", () => {
                  // Small delay so user sees the highlight before it disappears
                  setTimeout(() => {
                    hoverEar = false;
                    hoverPageNum = false;
                  }, 100);
                }, { passive: true });
                
                // Expose for external use
                g.goToPage = goToPage;
                g.totalPages = totalPages;
                g.getCurrentPage = () => currentPageIndex;
                
                // Cleanup
                const observer = new MutationObserver(() => {
                  if (!document.body.contains(canvas)) {
                    running = false;
                    observer.disconnect();
                  }
                });
                observer.observe(document.body, { childList: true, subtree: true });
                
                // Create hidden binding reference for editors (matches DOM mode structure)
                const binding = cel("div");
                binding.id = "binding";
                binding.style.cssText = "position:absolute;pointer-events:none;opacity:0;";
                g.appendChild(binding);
                
                // Initialize
                g.appendChild(canvas);
                resizeCanvas();
                prefetchPages(currentPageIndex);
                
                // Update binding size when canvas resizes
                function updateBindingSize() {
                  binding.style.width = cardWidth + "px";
                }
                
                window.addEventListener("resize", () => {
                  resizeCanvas();
                  updateBindingSize();
                });
                updateBindingSize();
                loop();
                
                computePageLayout = function() {
                  resizeCanvas();
                  updateBindingSize();
                };
                
                canvas.style.touchAction = "none";
                canvas.style.cursor = "grab";
              } else if (totalPages > 0 || loadedPagesData.length > 0) {
                const binding = cel("div");
                binding.id = "binding";
                binding.classList.add("hidden");
                
                // Track which pages are loaded
                const loadedPages = new Set();
                const pageWrappers = {};
                
                // Helper to render a full page
                function renderFullPage(page, index) {
                  const pageWrapper = pageWrappers[index];
                  if (!pageWrapper || pageWrapper.dataset.loaded === "true") return;
                  
                  pageWrapper.dataset.loaded = "true";
                  pageWrapper.innerHTML = ""; // Clear placeholder
                  loadedPages.add(index);

                  const pageEl = cel("article");
                  pageEl.classList.add("page");
                  pageEl.classList.add("page-style-a");

                  const pageTitle = cel("div");
                  pageTitle.classList.add("page-title");
                  pageTitle.innerText = dateTitle(page.when);

                  const pageNumber = cel("div");
                  pageNumber.classList.add("page-number");
                  pageNumber.innerText = "- " + index + " -";
                  pageNumber.style.cursor = "pointer";
                  pageNumber.dataset.pageIndex = index;
                  pageNumber.dataset.pageContent = page.content?.substring(0, 200) || "";
                  pageNumber.onclick = (e) => {
                    e.stopPropagation();
                    openChatWithMessage("-" + index + "- ");
                  };

                  const ear = cel("div");
                  ear.classList.add("ear");

                  // 📐 Ear / Touch (simplified for now)
                  const leave = () => {
                    ear.classList.remove("hover");
                    ear.classList.remove("active");
                  };

                  ear.addEventListener("pointerenter", () => {
                    if (!ear.classList.contains("hover")) {
                      ear.classList.add("hover");
                      ear.addEventListener("pointerleave", leave, { once: true });
                    }
                  });

                  ear.addEventListener("pointerdown", (e) => {
                    e.preventDefault();
                    ear.classList.remove("hover");
                    ear.classList.add("active");
                    window.addEventListener("pointerup", (e) => {
                      ear.removeEventListener("pointerleave", leave);
                      const elementUnderPointer = document.elementFromPoint(e.clientX, e.clientY);
                      if (elementUnderPointer !== ear) leave();
                    }, { once: true });
                  });

                  ear.onclick = async (e) => {
                    if (ear.classList.contains("reverse")) {
                      pageWrapper.querySelector(".backpage")?.remove();
                      ear.classList.remove("reverse");
                      pageEl.classList.remove("reverse");
                      pageWrapper.classList.remove("reverse");
                      setTimeout(() => ear.classList.remove("active"), 150);
                      return;
                    }

                    const author = page.handle ? "@" + page.handle : "Unknown";
                    const date = new Date(page.when);
                    const backpage = cel("div");
                    backpage.classList.add("backpage");
                    
                    const byline = cel("div");
                    byline.innerText = "Written by " + author;
                    byline.classList.add("byline");
                    backpage.appendChild(byline);

                    // Touches
                    veil();
                    let touches = [];
                    const res = await userRequest("POST", "/sotce-net/touch-a-page", { _id: page._id });
                    if (res.status === 200) touches = res.touches;
                    unveil({ instant: true });

                    let touchedBy = "";
                    if (touches.length === 1) touchedBy = touches[0] + " touched this page.";
                    else if (touches.length === 2) touchedBy = touches[0] + " and " + touches[1] + " touched this page.";
                    else if (touches.length > 2) {
                      const lastTouch = touches.pop();
                      touchedBy = touches.join(", ") + ", and " + lastTouch + " touched this page.";
                    }

                    const touchesEl = cel("p");
                    touchesEl.classList.add("touches");
                    if (touchedBy) touchesEl.innerText = touchedBy;

                    // Allow crumple page action for admin users.
                    if (subscription.admin) {
                      const crumplePage = cel("a");
                      crumplePage.innerText = "crumple this page";
                      crumplePage.href = "";
                      crumplePage.classList.add("crumple-this-page");
                      crumplePage.onclick = async (e) => {
                        e.preventDefault();
                        if (!confirm("💣 Unpublish this page?")) return;
                        veil();
                        const res = await userRequest("POST", "/sotce-net/write-a-page", { draft: "crumple", _id: page._id });
                        if (res.status === 200) {
                          console.log("🪧 Page crumpled:", res);
                          // Clear cache since pages changed
                          await clearPageCache();
                          unveil({ instant: true });
                          window.location.reload();
                        } else {
                          console.error("🪧 Page crumple:", res);
                          alert("☠️ There was a problem crumpling this page.");
                          unveil({ instant: true });
                        }
                      };
                      backpage.appendChild(crumplePage);
                    }

                    const print = cel("button");
                    print.innerText = "Print";
                    print.onclick = () => window.print();
                    backpage.appendChild(print);
                    backpage.appendChild(touchesEl);

                    ear.classList.add("reverse");
                    pageEl.classList.add("reverse");
                    pageWrapper.classList.add("reverse");
                    ear.classList.remove("active");

                    pageWrapper.querySelector(".backpage")?.remove();
                    pageWrapper.appendChild(backpage);
                  };

                  const wordsEl = cel("p");
                  wordsEl.classList.add("words");
                  wordsEl.innerText = page.words;

                  pageEl.appendChild(pageTitle);
                  pageEl.appendChild(wordsEl);
                  pageEl.appendChild(pageNumber);
                  pageWrapper.appendChild(pageEl);
                  pageWrapper.appendChild(ear);
                }
                
                // 📖 VIRTUALIZED PAGE SYSTEM - Only 3 pages in DOM at a time, scroll-based
                let currentPageIndex = totalPages; // Start at most recent page
                const pageCache = new Map(); // Cache page data by index
                const renderedPages = new Map(); // Track which page indices are currently in DOM
                
                // Determine starting page from URL
                const pageMatch = path.match(/^\\/page\\/(\\d+)$/);
                if (pageMatch) {
                  const requestedPage = parseInt(pageMatch[1], 10);
                  if (requestedPage >= 1 && requestedPage <= totalPages) {
                    currentPageIndex = requestedPage;
                  }
                }
                
                // Cache initially loaded pages
                if (pageIndex && loadedPagesData[0]) {
                  pageCache.set(pageIndex, loadedPagesData[0]);
                  currentPageIndex = pageIndex;
                } else {
                  const startIdx = totalPages - loadedPagesData.length + 1;
                  loadedPagesData.forEach((page, i) => {
                    pageCache.set(startIdx + i, page);
                  });
                }
                
                // Create a page wrapper element
                function createPageWrapper(index) {
                  const pageWrapper = cel("div");
                  pageWrapper.classList.add("page-wrapper");
                  pageWrapper.dataset.pageNumber = index;
                  pageWrapper.dataset.pageType = "diary";
                  pageWrapper.dataset.loaded = "false";
                  pageWrapper.id = "page-" + index;
                  return pageWrapper;
                }
                
                // Render page content into a wrapper
                async function renderPageContent(pageWrapper, pageIdx) {
                  if (!pageWrapper || pageIdx < 1 || pageIdx > totalPages) return;
                  if (pageWrapper.dataset.loaded === "true") return;
                  
                  // Show loading state only if wrapper is offscreen
                  const wrapperRect = pageWrapper.getBoundingClientRect();
                  const bindingRect = binding.getBoundingClientRect();
                  const isVisible = wrapperRect.bottom > bindingRect.top && wrapperRect.top < bindingRect.bottom;
                  if (!isVisible && pageWrapper.childElementCount === 0) {
                    pageWrapper.innerHTML = "<div class='page-container'><div class='page-placeholder'><span class='loading-dots'>Loading</span></div></div>";
                  }
                  
                  // Check memory cache first
                  let pageData = pageCache.get(pageIdx);
                  
                  if (!pageData) {
                    // Try IndexedDB cache
                    pageData = await getCachedPage(pageIdx);
                    
                    if (!pageData) {
                      // Fetch from server
                      const response = await subscribed({ pageNumber: pageIdx, limit: 1 });
                      if (response?.pages?.[0]) {
                        pageData = response.pages[0];
                        await setCachedPage(pageIdx, pageData);
                      }
                    }
                    
                    if (pageData) {
                      pageCache.set(pageIdx, pageData);
                    }
                  }
                  
                  if (pageData) {
                    pageWrapper.innerHTML = "";
                    pageWrapper.dataset.loaded = "true";
                    
                    const page = pageData;
                    const pageEl = cel("article");
                    pageEl.classList.add("page");
                    pageEl.classList.add("page-style-a");

                    const pageTitle = cel("div");
                    pageTitle.classList.add("page-title");
                    pageTitle.innerText = dateTitle(page.when);

                    const pageNumber = cel("div");
                    pageNumber.classList.add("page-number");
                    pageNumber.innerText = "- " + pageIdx + " -";
                    pageNumber.style.cursor = "pointer";
                    pageNumber.onclick = (e) => {
                      e.stopPropagation();
                      openChatWithMessage("-" + pageIdx + "- ");
                    };
                    
                    // Page flip ear
                    const ear = cel("div");
                    ear.classList.add("ear");
                    
                    // Ear hover/active states
                    const leave = () => {
                      ear.classList.remove("hover");
                      ear.classList.remove("active");
                    };
                    
                    ear.addEventListener("pointerenter", () => {
                      if (!ear.classList.contains("hover")) {
                        ear.classList.add("hover");
                        ear.addEventListener("pointerleave", leave, { once: true });
                      }
                    });
                    
                    ear.addEventListener("pointerdown", (e) => {
                      e.preventDefault();
                      ear.classList.remove("hover");
                      ear.classList.add("active");
                      window.addEventListener("pointerup", (upE) => {
                        ear.removeEventListener("pointerleave", leave);
                        const elementUnderPointer = document.elementFromPoint(upE.clientX, upE.clientY);
                        if (elementUnderPointer !== ear) leave();
                      }, { once: true });
                    });
                    
                    ear.onclick = async () => {
                      if (ear.classList.contains("reverse")) {
                        ear.classList.remove("reverse");
                        pageEl.classList.remove("reverse");
                        pageWrapper.classList.remove("reverse");
                        return;
                      }
                      
                      // Flip to backpage
                      veil();
                      let touches = [];
                      const res = await userRequest("POST", "/sotce-net/touch", { pageId: page._id });
                      if (res.status === 200) touches = res.touches;
                      unveil({ instant: true });

                      let touchedBy = "";
                      if (touches.length === 1) touchedBy = touches[0] + " touched this page.";
                      else if (touches.length === 2) touchedBy = touches[0] + " and " + touches[1] + " touched this page.";
                      else if (touches.length > 2) {
                        const lastTouch = touches.pop();
                        touchedBy = touches.join(", ") + ", and " + lastTouch + " touched this page.";
                      }

                      const backpage = cel("article");
                      backpage.classList.add("page", "backpage");

                      const touchesEl = cel("p");
                      touchesEl.classList.add("touches");
                      if (touchedBy) touchesEl.innerText = touchedBy;

                      if (subscription.admin) {
                        const crumplePage = cel("a");
                        crumplePage.innerText = "crumple this page";
                        crumplePage.href = "";
                        crumplePage.classList.add("crumple-this-page");
                        crumplePage.onclick = async (e) => {
                          e.preventDefault();
                          if (!confirm("💣 Unpublish this page?")) return;
                          veil();
                          const res = await userRequest("POST", "/sotce-net/write-a-page", { draft: "crumple", _id: page._id });
                          if (res.status === 200) {
                            await clearPageCache();
                            unveil({ instant: true });
                            window.location.reload();
                          } else {
                            alert("☠️ There was a problem crumpling this page.");
                            unveil({ instant: true });
                          }
                        };
                        backpage.appendChild(crumplePage);
                      }

                      const print = cel("button");
                      print.innerText = "Print";
                      print.onclick = () => window.print();
                      backpage.appendChild(print);
                      backpage.appendChild(touchesEl);

                      ear.classList.add("reverse");
                      pageEl.classList.add("reverse");
                      pageWrapper.classList.add("reverse");
                      ear.classList.remove("active");

                      // Add backpage to the page-container
                      const container = pageWrapper.querySelector(".page-container");
                      container.querySelector(".backpage")?.remove();
                      container.appendChild(backpage);
                    };

                    const wordsEl = cel("p");
                    wordsEl.classList.add("words");
                    wordsEl.innerText = page.words;

                    pageEl.appendChild(pageTitle);
                    pageEl.appendChild(wordsEl);
                    pageEl.appendChild(pageNumber);
                    
                    // Wrap page in container for centering
                    const pageContainer = cel("div");
                    pageContainer.classList.add("page-container");
                    pageContainer.appendChild(pageEl);
                    pageContainer.appendChild(ear);
                    pageWrapper.appendChild(pageContainer);
                  }
                }
                
                // Update which pages are in the DOM based on current page
                // Keep 5 pages in DOM for smoother rapid navigation
                async function updateVisiblePages(centerIdx, skipScroll = false) {
                  const pagesToShow = [centerIdx - 2, centerIdx - 1, centerIdx, centerIdx + 1, centerIdx + 2].filter(i => i >= 1 && i <= totalPages);
                  
                  // Get viewport bounds to check what's visible
                  const viewportTop = binding.scrollTop;
                  const viewportBottom = viewportTop + binding.clientHeight;
                  
                  // Remove pages that shouldn't be visible AND are off-screen
                  for (const [idx, wrapper] of renderedPages) {
                    if (!pagesToShow.includes(idx)) {
                      // Only remove if completely off-screen
                      const wrapperTop = wrapper.offsetTop;
                      const wrapperBottom = wrapperTop + wrapper.clientHeight;
                      const isVisible = wrapperBottom > viewportTop && wrapperTop < viewportBottom;
                      
                      if (!isVisible) {
                        wrapper.remove();
                        renderedPages.delete(idx);
                      }
                    }
                  }
                  
                  // Add/update pages that should be visible
                  for (const idx of pagesToShow) {
                    if (!renderedPages.has(idx)) {
                      const wrapper = createPageWrapper(idx);
                      renderedPages.set(idx, wrapper);
                      
                      // Insert in correct order
                      const existingWrappers = Array.from(binding.querySelectorAll(".page-wrapper"));
                      const insertBefore = existingWrappers.find(w => parseInt(w.dataset.pageNumber) > idx);
                      if (insertBefore) {
                        binding.insertBefore(wrapper, insertBefore);
                      } else {
                        binding.appendChild(wrapper);
                      }
                      
                      await renderPageContent(wrapper, idx);
                    }
                  }
                  
                  // IMPORTANT: After DOM changes, scroll to center page (instant, no animation)
                  if (!skipScroll) {
                    const centerWrapper = document.getElementById("page-" + centerIdx);
                    if (centerWrapper) {
                      centerWrapper.scrollIntoView({ block: "center", behavior: "auto" });
                    }
                  }
                  
                  // Prefetch data for pages further ahead (cache only, don't render)
                  const prefetchRange = [centerIdx - 4, centerIdx - 3, centerIdx + 3, centerIdx + 4].filter(i => i >= 1 && i <= totalPages);
                  for (const idx of prefetchRange) {
                    if (!pageCache.has(idx)) {
                      // Async prefetch without awaiting
                      (async () => {
                        let pageData = await getCachedPage(idx);
                        if (!pageData) {
                          const response = await subscribed({ pageNumber: idx, limit: 1 });
                          if (response?.pages?.[0]) {
                            pageData = response.pages[0];
                            await setCachedPage(idx, pageData);
                          }
                        }
                        if (pageData) pageCache.set(idx, pageData);
                      })();
                    }
                  }
                  // Update URL
                  updatePath("/page/" + centerIdx);
                  currentPageIndex = centerIdx;
                }
                
                // Initial render - show 3 pages around current
                console.log("📖 Virtualized scroll view: starting at page", currentPageIndex, "of", totalPages);
                await updateVisiblePages(currentPageIndex, true); // skipScroll=true, we'll do it manually
                
                // Scroll to current page after initial render
                setTimeout(() => {
                  const currentWrapper = document.getElementById("page-" + currentPageIndex);
                  if (currentWrapper) {
                    currentWrapper.scrollIntoView({ block: "center", behavior: "auto" });
                  }
                }, 50);
                
                // Handle scroll to update visible pages
                // #binding is now the scroll container (FYP-style)
                let isAnimating = false;
                let animationTimeout;
                let scrollTimeout;
                let isUpdating = false;
                binding.addEventListener("scroll", () => {
                  if (isUpdating || isDragging || isAnimating) return; // Skip during drag/animation
                  
                  clearTimeout(scrollTimeout);
                  scrollTimeout = setTimeout(async () => {
                    if (isDragging) return; // Double-check
                    
                    // Find which page is centered in viewport
                    const bindingCenter = binding.scrollTop + binding.clientHeight / 2;
                    
                    let closestPage = currentPageIndex;
                    let closestDistance = Infinity;
                    
                    for (const [idx, pageWrapper] of renderedPages) {
                      const elCenter = pageWrapper.offsetTop + pageWrapper.clientHeight / 2;
                      const distance = Math.abs(elCenter - bindingCenter);
                      
                      if (distance < closestDistance) {
                        closestDistance = distance;
                        closestPage = idx;
                      }
                    }
                    
                    if (closestPage !== currentPageIndex) {
                      isUpdating = true;
                      await updateVisiblePages(closestPage, true); // skipScroll - user scrolled here
                      computePageLayout?.();
                      isUpdating = false;
                    }
                  }, 150);
                }, { passive: true });
                
                // Keyboard navigation
                document.addEventListener("keydown", (e) => {
                  if (!document.body.contains(binding)) return;
                  if (e.target.tagName === "INPUT" || e.target.tagName === "TEXTAREA") return;
                  
                  if (e.key === "ArrowUp" || e.key === "ArrowLeft") {
                    e.preventDefault();
                    if (currentPageIndex > 1) {
                      animateToPage(currentPageIndex - 1, "keyboard-prev");
                    }
                  } else if (e.key === "ArrowDown" || e.key === "ArrowRight") {
                    e.preventDefault();
                    if (currentPageIndex < totalPages) {
                      animateToPage(currentPageIndex + 1, "keyboard-next");
                    }
                  }
                });
                
                // Drag-to-scroll anywhere in garden (FYP-like swipe)
                let isDragging = false;
                let dragStartY = 0;
                let dragStartScrollTop = 0;
                let dragStartPageIndex = 0;
                let dragVelocity = 0;
                let lastDragY = 0;
                let lastDragTime = 0;

                async function animateToPage(targetPage, reason = "") {
                  if (!targetPage) return;
                  isAnimating = true;
                  clearTimeout(animationTimeout);

                  const targetWrapper = document.getElementById("page-" + targetPage);
                  if (targetWrapper) {
                    if (reason) console.log("🧭 animateToPage:", reason, "->", targetPage);
                    targetWrapper.scrollIntoView({ block: "center", behavior: "smooth" });
                  } else {
                    console.log("🧭 animateToPage: WARNING missing wrapper for", targetPage);
                  }

                  animationTimeout = setTimeout(async () => {
                    await updateVisiblePages(targetPage, true);
                    computePageLayout?.();
                    isAnimating = false;
                  }, 400);
                }
                
                g.addEventListener("pointerdown", (e) => {
                  // Don't drag on interactive elements
                  if (e.target.closest(".ear, .page-number, a, button, input, textarea")) return;
                  if (isAnimating) return;
                  
                  // Figure out which page we're ACTUALLY on based on scroll position
                  const bindingCenter = binding.scrollTop + binding.clientHeight / 2;
                  let actualPage = currentPageIndex;
                  let closestDistance = Infinity;
                  
                  for (const [idx, pageWrapper] of renderedPages) {
                    const elCenter = pageWrapper.offsetTop + pageWrapper.clientHeight / 2;
                    const distance = Math.abs(elCenter - bindingCenter);
                    if (distance < closestDistance) {
                      closestDistance = distance;
                      actualPage = idx;
                    }
                  }
                  
                  // Use actual scroll position, not potentially stale currentPageIndex
                  isDragging = true;
                  dragStartY = e.clientY;
                  dragStartScrollTop = binding.scrollTop;
                  dragStartPageIndex = actualPage;
                  lastDragY = e.clientY;
                  lastDragTime = Date.now();
                  dragVelocity = 0;
                  
                  console.log("🖐️ Drag START:", {
                    clientY: e.clientY,
                    scrollTop: binding.scrollTop,
                    bindingCenter,
                    currentPageIndex,
                    actualPage,
                    dragStartPageIndex,
                    renderedPages: [...renderedPages.keys()],
                  });
                  
                  // Disable smooth scroll and snap during drag
                  binding.style.scrollBehavior = "auto";
                  binding.style.scrollSnapType = "none";
                  g.setPointerCapture(e.pointerId);
                  e.preventDefault();
                });
                
                g.addEventListener("pointermove", (e) => {
                  if (!isDragging) return;
                  
                  const deltaY = dragStartY - e.clientY;
                  binding.scrollTop = dragStartScrollTop + deltaY;
                  
                  // Calculate velocity for momentum
                  const now = Date.now();
                  const dt = now - lastDragTime;
                  if (dt > 0) {
                    dragVelocity = (lastDragY - e.clientY) / dt;
                  }
                  lastDragY = e.clientY;
                  lastDragTime = now;
                  
                  // Visual feedback - show which direction we'll go
                  const threshold = 100;
                  g.classList.remove("drag-up", "drag-down", "drag-snap");
                  if (deltaY > threshold && dragStartPageIndex < totalPages) {
                    g.classList.add("drag-up"); // Will go to higher page number (drag up)
                  } else if (deltaY < -threshold && dragStartPageIndex > 1) {
                    g.classList.add("drag-down"); // Will go to lower page number (drag down)
                  } else {
                    g.classList.add("drag-snap"); // Will snap back
                  }
                });
                
                g.addEventListener("pointerup", async (e) => {
                  if (!isDragging) return;
                  isDragging = false;
                  
                  g.releasePointerCapture(e.pointerId);
                  // Re-enable smooth scroll and snap
                  binding.style.scrollBehavior = "smooth";
                  binding.style.scrollSnapType = "y mandatory";
                  
                  // Clear visual indicator
                  g.classList.remove("drag-up", "drag-down", "drag-snap");
                  
                  const totalDrag = dragStartY - e.clientY;
                  const threshold = 100; // Minimum drag to trigger page change (increased from 50)
                  
                  console.log("🖐️ Drag release:", {
                    dragStartY,
                    endY: e.clientY,
                    totalDrag,
                    velocity: dragVelocity,
                    dragStartPageIndex,
                    currentPageIndex,
                    totalPages,
                    direction: totalDrag > 0 ? "UP (finger moved up)" : "DOWN (finger moved down)"
                  });
                  
                  // Determine target page based on drag distance/velocity
                  let targetPage = dragStartPageIndex;
                  
                  // Physics: combine distance and velocity
                  // velocity is in px/ms, so scale it up
                  const velocityBoost = dragVelocity * 100; // Convert to more usable scale
                  const effectiveDistance = totalDrag + velocityBoost;
                  
                  console.log("🖐️ Physics:", {
                    totalDrag,
                    velocityRaw: dragVelocity,
                    velocityBoost,
                    effectiveDistance,
                    threshold,
                    willTrigger: Math.abs(effectiveDistance) > threshold
                  });
                  
                  if (Math.abs(effectiveDistance) > threshold) {
                    if (effectiveDistance > 0 && dragStartPageIndex < totalPages) {
                      // Dragged up - go to higher page number
                      targetPage = dragStartPageIndex + 1;
                    } else if (effectiveDistance < 0 && dragStartPageIndex > 1) {
                      // Dragged down - go to lower page number
                      targetPage = dragStartPageIndex - 1;
                    }
                  }
                  
                  console.log("🖐️ -> Target page:", targetPage, "(from", dragStartPageIndex, ")");
                  
                  await animateToPage(targetPage, "drag-release");
                });
                
                g.addEventListener("pointercancel", async (e) => {
                  if (!isDragging) return;
                  isDragging = false;
                  g.releasePointerCapture(e.pointerId);
                  g.classList.remove("drag-up", "drag-down", "drag-snap");
                  binding.style.scrollBehavior = "smooth";
                  binding.style.scrollSnapType = "y mandatory";
                  // Snap back to current
                  await animateToPage(currentPageIndex, "pointer-cancel");
                });
                
                // Expose for external use
                g.goToPage = async (idx) => {
                  await updateVisiblePages(idx, true); // skipScroll - we'll animate
                  await animateToPage(idx, "goToPage");
                };
                g.totalPages = totalPages;
                g.getCurrentPage = () => currentPageIndex;
                
                g.appendChild(binding);

                computePageLayout = function (e) {
                  const layoutStart = performance.now();
                  // Relational scroll wip - 24.09.25.17.43
                  // const bindingRect = binding.getBoundingClientRect();
                  // if (
                  //   abs(round(bindingRect.top - document.body.clientHeight)) ===
                  //   document.body.scrollHeight
                  // ) {
                  //   console.log("🌊 At bottom...");
                  // }

                  const pagesTop =
                    window.visualViewport.width <= ${miniBreakpoint} ? 72 : 100;
                  const pagesBot = 32;

                  const rat =
                    (window.innerHeight - pagesTop) /
                    window.visualViewport.width;

                  const computedWrapper = parseInt(
                    window.getComputedStyle(wrapper).width,
                  );

                  const actualWrapper = wrapper.clientWidth;

                  const maxPageWidth = Infinity;
                  const minPageWidth = 0; // Set your minimum width here.
                  const minPageHeight = 400; // 600;
                  const pageRatio = 4 / 5;

                  let width = max(
                    minPageWidth,
                    min(maxPageWidth, min(1, rat) * computedWrapper),
                  );

                  const pageHeight = width / pageRatio;
                  if (pagesTop + pageHeight > window.innerHeight) {
                    let availableHeight =
                      window.innerHeight - pagesTop - pagesBot;
                    if (availableHeight <= minPageHeight)
                      availableHeight = minPageHeight;
                    width = min(
                      window.visualViewport.width,
                      availableHeight * pageRatio,
                    );
                  }

                  binding.style.width = width + "px";
                  binding.style.fontSize = width * 0.03 + "px";

                  // Set the size of the editor if it's open.
                  const editorForm = document.getElementById("editor-form");
                  const editorPage = document.getElementById("editor-page");

                  if (editorForm) {
                    editorForm.style.width = binding.style.width;
                    const baseWidth = 100 * 8;
                    const goalWidth = editorPage.parentElement.clientWidth;
                    const scale = goalWidth / baseWidth;
                    editorPage.style.transform = "scale(" + scale + ")";
                  }

                  // Set the size of the ask editor if it's open.
                  const askEditorForm = document.getElementById("ask-editor-form");
                  const askEditorPage = document.getElementById("ask-editor-page");

                  if (askEditorForm) {
                    askEditorForm.style.width = binding.style.width;
                    const baseWidth = 100 * 8;
                    const goalWidth = askEditorPage.parentElement.clientWidth;
                    const scale = goalWidth / baseWidth;
                    askEditorPage.style.transform = "scale(" + scale + ")";
                  }

                  const queryStart = performance.now();
                  // Only process VISIBLE pages (+ small buffer) for performance
                  const allPages = document.querySelectorAll(
                    "#garden article.page",
                  );
                  const queryTime = performance.now() - queryStart;
                  
                  const wrapperRect = wrapper.getBoundingClientRect();
                  const viewportBuffer = wrapperRect.height * 1.5; // Process pages within 1.5x viewport

                  const pagesStart = performance.now();
                  let processedPages = 0;
                  let scale;
                  allPages.forEach((page) => {
                    // Skip pages that are far off-screen
                    const pageRect = page.getBoundingClientRect();
                    const isNearViewport = pageRect.bottom > wrapperRect.top - viewportBuffer && 
                                          pageRect.top < wrapperRect.bottom + viewportBuffer;
                    
                    if (!scale) {
                      const baseWidth = 100 * 8;
                      const goalWidth = page.parentElement.clientWidth;
                      scale = goalWidth / baseWidth;
                    }
                    page.style.transform = "scale(" + scale + ")";
                    
                    // Only do expensive text processing for visible pages
                    if (!isNearViewport) return;
                    processedPages++;

                    // Check to see if the last line of the page needs
                    // justification or not.
                    const words = page.querySelector(".words");
                    if (!words) return;
                    const wcs = window.getComputedStyle(words);
                    const lineCount = round(
                      words.clientHeight / parseFloat(wcs.lineHeight),
                    );

                    if (lineCount === 17) {
                      // Compute or read line progress from the cache.
                      if (page.lastLineProgress === undefined) {
                        const { lastLineText, lastLineProgress } =
                          computeLastLineText(words);
                        //  const lastlineText = "",
                        //   lastLineProgress = 0;
                        page.lastLineProgress = lastLineProgress;
                      }

                      if (
                        needsJustification(
                          words.innerText,
                          page.lastLineProgress,
                        )
                      ) {
                        words.classList.add("justify-last-line");
                      }
                    }
                  });
                  const pagesTime = performance.now() - pagesStart;

                  const earsStart = performance.now();
                  const ears = document.querySelectorAll(
                    "#garden .page-wrapper div.ear",
                  );
                  let processedEars = 0;

                  ears.forEach((ear) => {
                    // Skip ears far off-screen
                    const earRect = ear.getBoundingClientRect();
                    const isNearViewport = earRect.bottom > wrapperRect.top - viewportBuffer && 
                                          earRect.top < wrapperRect.bottom + viewportBuffer;
                    if (!isNearViewport) return;
                    processedEars++;
                    
                    ear.style = "";
                    const earStyle = window.getComputedStyle(ear);
                    const computedWidth = parseFloat(earStyle.width);
                    const computedBottom = parseFloat(earStyle.bottom);
                    const computedRight = parseFloat(earStyle.right);
                    const roundedW = round(computedWidth);
                    ear.style.width = roundedW + "px";
                    ear.style.height = roundedW + "px";
                    ear.style.top = "calc(100% - " + roundedW + "px + 0.12em)";
                    ear.style.left = "calc(100% - " + roundedW + "px + 0.12em)";
                  });
                  const earsTime = performance.now() - earsStart;
                  
                  const layoutTime = performance.now() - layoutStart;
                  if (layoutTime > 10) {
                    console.log("📐 computePageLayout:", layoutTime.toFixed(1), "ms",
                      "| query:", queryTime.toFixed(1), "ms",
                      "| pages:", pagesTime.toFixed(1), "ms (" + processedPages + "/" + allPages.length + " processed)",
                      "| ears:", earsTime.toFixed(1), "ms (" + processedEars + "/" + ears.length + ")");
                  }
                };

                // Adjust width of '#top-bar' for scrollbar appearance.
                const scrollbarWidth =
                  wrapper.offsetWidth - wrapper.clientWidth;
                // if (Safari && Desktop) scrollbarWidth = 16;
                topBar.style.width = "calc(100% - " + scrollbarWidth + "px)";
                // binding.style.paddingLeft = "calc(16px + " + scrollbarWidth + "px)";
                // g.style.paddingLeft = scrollbarWidth + "px";

                binding.classList.remove("hidden");

                let previousBodyHeight = document.body.clientHeight;

                function resizeHandler(e) {
                  if (!document.body.contains(binding)) {
                    window.removeEventListener("resize", resizeHandler);
                  } else {
                    computePageLayout(e);
                  }
                }
                window.addEventListener("resize", resizeHandler);
              }

              const cookieMenuWrapper = cel("div");
              const cookieMenu = cel("div");
              cookieMenuWrapper.id = "cookie-menu-wrapper";
              cookieMenu.id = "cookie-menu";
              const cookieImg = cel("img");
              cookieImg.id = "cookie-menu-img";
              cookieImg.src = asset("cookie-open.png");
              cookieImg.crossOrigin = "anonymous";
              cookieMenuWrapper.appendChild(cookieMenu);
              cookieMenuWrapper.appendChild(cookieImg);
              topBar.appendChild(cookieMenuWrapper);

              // if (GATE_WAS_UP) {
              //  console.log("gate was upppppp");
              // g.classList.add("hidden");
              // document.body.classList.add("pages-hidden");
              // }

              const curtainCookie =
                gateCurtain.querySelector("#cookie-wrapper");

              cookieMenu.onclick = function () {
                const perfStart = performance.now();
                console.log("🍪 Garden cookie click - CLOSING garden, OPENING gate");
                
                scrollMemory = wrapper.scrollTop;
                gateCurtain.classList.remove("hidden");
                g.classList.add("hidden");
                document.body.classList.add("pages-hidden");
                document.documentElement.classList.remove("garden");
                curtainCookie.classList.add("interactive");
                updatePath("/gate");
                
                console.log("🍪 Classes updated:", (performance.now() - perfStart).toFixed(2), "ms");
                
                // Track frames to see when paint actually happens
                requestAnimationFrame(() => {
                  console.log("🍪 RAF 1:", (performance.now() - perfStart).toFixed(2), "ms");
                  requestAnimationFrame(() => {
                    console.log("🍪 RAF 2 (paint):", (performance.now() - perfStart).toFixed(2), "ms");
                  });
                });
                
                // Track visibility over time to catch delayed changes
                let checkCount = 0;
                const trackVisibility = () => {
                  checkCount++;
                  const elapsed = (performance.now() - perfStart).toFixed(0);
                  const gc = document.getElementById("gate-curtain");
                  if (gc) {
                    const gcVis = getComputedStyle(gc).visibility;
                    const hasHidden = gc.classList.contains("hidden");
                    // Only log if state changed or at key intervals
                    if (checkCount === 1 || checkCount === 3 || checkCount === 10 || hasHidden) {
                      console.log("🍪 @" + elapsed + "ms - curtain visibility: " + gcVis + ", .hidden: " + hasHidden);
                    }
                    if (hasHidden && checkCount > 1) {
                      console.log("⚠️ Curtain was re-hidden at " + elapsed + "ms - investigate what caused this!");
                    }
                  }
                  if (checkCount < 20 && !gc?.classList.contains("hidden")) setTimeout(trackVisibility, 500);
                };
                setTimeout(trackVisibility, 16); // Check after first frame
              };

              if (showGate) curtainCookie.classList.add("interactive");

              return new Promise((resolve) => {
                cookieImg.onload = function () {
                  // Render a mask image dataurl for the cookieImg.
                  const canvas = document.createElement("canvas");
                  canvas.width = cookieImg.width;
                  canvas.height = cookieImg.height;
                  const ctx = canvas.getContext("2d");
                  ctx.drawImage(cookieImg, 0, 0);
                  const dataUrl = canvas.toDataURL();
                  cookieMenu.style.maskImage = "url(" + dataUrl + ")";

                  document.getElementById("garden")?.remove(); // Remove old gardens.
                  const gardenBuildStart = performance.now();
                  console.log("🌻 Garden build starting...");
                  const observer = new MutationObserver(
                    (mutationsList, observer) => {
                      for (let mutation of mutationsList) {
                        if (
                          mutation.type === "childList" &&
                          mutation.addedNodes.length > 0
                        ) {
                          console.log("🌻 MutationObserver triggered:", (performance.now() - gardenBuildStart).toFixed(2), "ms");
                          const checkWidthSettled = (previousWidth) => {
                            const currentWidth = parseInt(
                              window.getComputedStyle(wrapper).width,
                            );

                            if (
                              currentWidth !== previousWidth ||
                              g.scrollHeight > 0 ||
                              showGate
                            ) {
                              console.log("🌻 Width settled, computing layout:", (performance.now() - gardenBuildStart).toFixed(2), "ms");
                              computePageLayout?.();
                              console.log("🌻 Layout computed:", (performance.now() - gardenBuildStart).toFixed(2), "ms");
                              
                              g.classList.remove("faded");
                              resolve(g);
                            } else {
                              requestAnimationFrame(() =>
                                checkWidthSettled(currentWidth),
                              );
                            }
                          };
                          requestAnimationFrame(() =>
                            checkWidthSettled(
                              parseInt(window.getComputedStyle(wrapper).width),
                            ),
                          );
                          observer.disconnect();
                          break;
                        }
                      }
                    },
                  );
                  observer.observe(wrapper, { childList: true });

                  console.log("🌻 Appending garden to DOM...", (performance.now() - gardenBuildStart).toFixed(2), "ms");
                  g.classList.add("faded");
                  wrapper.appendChild(g);
                  console.log("🌻 Garden appended:", (performance.now() - gardenBuildStart).toFixed(2), "ms");
                  if (!showGate) {
                    document.documentElement.classList.add("garden");
                  }
                  g.classList.remove("obscured");

                  if (path === "/chat") chatButton.click();
                };
              });

              // return g;
            } //);
            // #endregion

            // 🔐 Authorization
            const clientId = "${AUTH0_CLIENT_ID_SPA}";
            let verificationTimeout;
            let isAuthenticated = false;
            let fetchUser;
            let user;
            let status; // Global status variable
            let subscription; // Global subscription variable

            async function retrieveUncachedUser() {
              try {
                if (!window.sotceUSER) {
                  await auth0Client.getTokenSilently({ cacheMode: "off" });
                  user = await auth0Client.getUser();
                } else {
                  try {
                    const response = await fetch(
                      "/user?from=" +
                        encodeURIComponent(user.email) +
                        "&tenant=sotce",
                    );
                    const u = await response.json();
                    if (u.email_verified) {
                      user.email_verified = u.email_verified;
                      user.sub = u.sub;
                    }
                  } catch (err) {
                    console.error("👨‍🦰 Error:", err);
                  }
                }
                // console.log("🎇 New user is...", user);
              } catch (err) {
                console.warn(
                  "Error retrieving uncached user:",
                  err, //,
                  //"Embedded:",
                  //embedded,
                );
                // logout(); // Log the user out automatically, but only if
                //              the token was stale?
              }
            }

            const auth0Client = await window.auth0.createAuth0Client({
              domain: "${AUTH0_DOMAIN}",
              clientId,
              cacheLocation: "localstorage",
              useRefreshTokens: true,
              authorizationParams: { redirect_uri: "${HOST}" },
            });

            if (
              location.search.includes("state=") &&
              (location.search.includes("code=") ||
                location.search.includes("error="))
            ) {
              try {
                // console.log("🔐 Handling auth0 redirect...");
                await auth0Client.handleRedirectCallback();
              } catch (e) {
                console.error("🔐", e);
              }
              window.history.replaceState(
                {},
                document.title,
                window.location.pathname,
              );
            }

            // 😶‍🌫️ Picking up a session from VS Code / the parent frame.
            let pickedUpSession;
            {
              const url = new URL(window.location);
              const params = url.searchParams;
              let param = params.get("session-sotce");

              if (param === "null") {
                localStorage.removeItem("session-sotce");
              } else if (param === "retrieve") {
                param = localStorage.getItem("session-sotce");
              }

              const sessionParams = param;
              let encodedSession = sessionParams;
              if (encodedSession === "null") encodedSession = undefined;
              if (encodedSession) {
                // console.log("🪷 Sotce Session:", encodedSession);
                const sessionJsonString = atob(
                  decodeURIComponent(encodedSession),
                );
                const session = JSON.parse(sessionJsonString);
                // Use the session information to authenticate, if it exists.
                // console.log("🥀 Session data:", session);
                if (session.accessToken && session.account) {
                  window.sotceTOKEN = session.accessToken; // Only set using this flow.
                  window.sotceUSER = {
                    email: session.account.label,
                    sub: session.account.id,
                  };
                  // console.log(

                  //   "🌻 Picked up sotce session!",
                  //   window.sotceTOKEN,
                  //   window.sotceUSER,
                  // );
                  pickedUpSession = true;
                }

                if (sessionParams) {
                  localStorage.setItem("session-sotce", encodedSession);
                  params.delete("session-sotce"); // Remove the 'session' parameter
                  // Update the URL without reloading the page
                  cleanUrlParams(url, params);
                }
              }
            }

            // Logging in normally.
            if (!pickedUpSession) {
              isAuthenticated = await auth0Client.isAuthenticated();

              // Try to fetch a new token on every refresh.
              // TODO: ❤️‍🔥 Is this necessary?
              if (isAuthenticated) {
                try {
                  // await auth0Client.getTokenSilently(/*{ cacheMode: "off" }*/);
                  // console.log("🗝️ Got silent token.");
                } catch (error) {
                  console.log("🔐️ ❌ Unauthorized", error);
                  console.error(
                    "Failed to retrieve token silently. Logging out.",
                    error,
                  );
                  // Redirect the user to logout if the token has failed.

                  auth0Client.logout({
                    logoutParams: { returnTo: "${HOST}" },
                  });
                  isAuthenticated = false;
                }
              }
            } else {
              isAuthenticated = true;
            }

            const spinner = document.getElementById("spinner");
            const spinnerTO = setTimeout(() => {
              spinner.classList.add("showing");
            }, 150);

            async function spinnerPass(callback, type, spinnerRemoved) {
              clearTimeout(spinnerTO);
              let page;
              if (spinner.classList.contains("showing")) {
                page = await callback();
                spinner.classList.remove("showing");

                setTimeout(() => {
                  spinner.remove();
                  page?.classList.remove("obscured"); // Show 'gate' / 'garden' if it wasn't already.
                  spinnerRemoved?.();
                }, 150);
              } else {
                page = await callback();
                spinner.remove();
                page?.classList.remove("obscured");
              }

              if (type === undefined) {
                // Not garden.
                if (path === "/chat") {
                  document.getElementById("cookie-wrapper")?.click();
                }
              }
            }

            if (!fullAlert) {
              if (!isAuthenticated) {
                // console.log("⚠️ Not authenticated...");
                status = "logged-out";
                subscription = null;
                // Wait for a subscriber count. if we are logged out.
                // console.log("Fetching subscribers...");
                try {
                  const res = await fetch("/sotce-net/subscribers");
                  const data = await res.json();
                  if (data.subscribers >= 0) {
                    SUBSCRIBER_COUNT = data.subscribers;
                    // console.log("Subscriber count:", SUBSCRIBER_COUNT);
                  }
                } catch (error) {
                  console.error("Error fetching subscribers:", error);
                }
                await spinnerPass(async () => {
                  return await gate(/* !dev ? "coming-soon" : */ "logged-out");
                });
              } else {
                user = pickedUpSession
                  ? window.sotceUSER
                  : await auth0Client.getUser();

                const userExists = await fetch(
                  "/user?from=" +
                    encodeURIComponent(user.email) +
                    "&tenant=sotce",
                );

                let u;
                try {
                  u = await userExists.json();
                } catch (err) {
                  console.warn(err);
                }
                if (!u?.sub || !user.email_verified)
                  await retrieveUncachedUser();

                //console.log("🟡 Are we here?", user);

                if (!user.email_verified) {
                  status = "unverified";
                  subscription = null;
                  await spinnerPass(async () => await gate("unverified", user));
                } else {
                  // The user's email is verified...

                  // Determine pagination based on path
                  const pageMatchUrl = path.match(/^\\/page\\/(\\d+)$/);
                  const questionMatchUrl = path.match(/^\\/q\\/(\\d+)$/);
                  const subscribeOptions = {};
                  
                  // Always load plenty of pages for the feed - don't set pageNumber
                  // which would limit server to just that one page
                  subscribeOptions.limit = 100;
                  
                  console.log("📄 subscribeOptions:", subscribeOptions, "for path:", path);

                  let entered = await subscribed(subscribeOptions);
                  let times = 0;

                  while (
                    waitForSubscriptionSuccessThreeTimes &&
                    !entered?.subscribed &&
                    times < 3
                  ) {
                    entered = await subscribed(subscribeOptions);
                    times += 1;
                  }

                  if (entered?.subscribed) {
                    status = "subscribed";
                    subscription = entered;
                    const showGate = path === "/gate";

                    let removeGateCurtain;
                    if (showGate)
                      removeGateCurtain = () => {
                        document
                          .querySelector("#gate-curtain")
                          ?.classList.remove("hidden");
                      };

                    await spinnerPass(
                      async () => await garden(entered, user, showGate),
                      "garden",
                      removeGateCurtain,
                    );
                  } else if (entered !== "error") {
                    status = "verified";
                    subscription = null;
                    await spinnerPass(async () => await gate("verified", user));
                  } else {
                    console.log("🔴 Server error.");
                    setTimeout(() => window.location.reload(), 2500);
                    // gate("error");
                  }
                }
              }
            } else {
              // 🚨 Full Alert
              await spinnerPass(() => {
                wrapper.innerHTML =
                  "<div id='full-alert'>" + fullAlert + "</div>";
                setTimeout(() => location.reload(), 2500);
              });
            }

            function veil() {
              const el = document.getElementById("veil");
              el.innerHTML = ""; // Clear any extra spinners in the veil.
              const spinner = cel("div");
              spinner.classList.add("spinner", "showing");
              el.appendChild(spinner);
              el.classList.remove("unveiled");
              el.classList.remove("unveiled-instant");
              document.documentElement.classList.add("veiled");
            }

            function unveil(options) {
              const el = document.getElementById("veil");
              if (options?.instant) {
                document.documentElement.classList.remove("veiled");
                el.classList.add("unveiled-instant");
                el.querySelector(".spinner")?.remove();
              } else {
                el.classList.add("unveiled");
                el.addEventListener(
                  "transitionend",
                  () => {
                    el.querySelector(".spinner")?.remove();
                  },
                  { once: true },
                );
              }
            }

            function flash(color, options) {
              if (options?.hold) wrapper.classList.add("flash", color);
            }

            function login(hint = "login") {
              if (embedded) {
                window.parent.postMessage(
                  { type: "login", tenant: "sotce" },
                  "*",
                );
              } else {
                const opts = { prompt: "login" }; // Never skip the login screen.
                opts.screen_hint = hint;
                auth0Client.loginWithRedirect({ authorizationParams: opts });
              }
            }

            function signup() {
              if (embedded) {
                console.log("🟠 Cannot sign up in an embedded view.");
              } else {
                login("signup");
              }
            }

            // Go to the paywalled subscription page.
            async function subscribe() {
              const stripe = Stripe(
                "${dev
                  ? SOTCE_STRIPE_API_TEST_PUB_KEY
                  : SOTCE_STRIPE_API_PUB_KEY}",
              );
              const response = await fetch("/sotce-net/subscribe", {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify({ email: user.email, sub: user.sub }),
              });
              if (response.ok) {
                const session = await response.json();
                // console.log("💳 Session:", session);
                const result = await stripe.redirectToCheckout({
                  sessionId: session.id,
                });
                if (result.error) console.error(result.error.message);
              } else {
                const error = await response.json();
                if (error.alreadySubscribed) {
                  // User already has an active subscription, refresh to show their status
                  console.log("💳 Already subscribed, refreshing...");
                  window.location.reload();
                } else {
                  console.error("💳", error.message);
                }
              }
            }

            // 📦 IndexedDB Page Cache
            const PAGE_CACHE_DB = "sotce-page-cache";
            const PAGE_CACHE_STORE = "pages";
            const PAGE_META_STORE = "meta";
            
            async function openPageCache() {
              return new Promise((resolve, reject) => {
                const request = indexedDB.open(PAGE_CACHE_DB, 1);
                request.onerror = () => reject(request.error);
                request.onsuccess = () => resolve(request.result);
                request.onupgradeneeded = (e) => {
                  const db = e.target.result;
                  if (!db.objectStoreNames.contains(PAGE_CACHE_STORE)) {
                    db.createObjectStore(PAGE_CACHE_STORE, { keyPath: "pageIndex" });
                  }
                  if (!db.objectStoreNames.contains(PAGE_META_STORE)) {
                    db.createObjectStore(PAGE_META_STORE, { keyPath: "key" });
                  }
                };
              });
            }
            
            async function getCachedPage(pageIndex) {
              try {
                const db = await openPageCache();
                return new Promise((resolve, reject) => {
                  const tx = db.transaction(PAGE_CACHE_STORE, "readonly");
                  const store = tx.objectStore(PAGE_CACHE_STORE);
                  const request = store.get(pageIndex);
                  request.onsuccess = () => resolve(request.result?.data || null);
                  request.onerror = () => resolve(null);
                });
              } catch { return null; }
            }
            
            async function setCachedPage(pageIndex, pageData) {
              try {
                const db = await openPageCache();
                return new Promise((resolve) => {
                  const tx = db.transaction(PAGE_CACHE_STORE, "readwrite");
                  const store = tx.objectStore(PAGE_CACHE_STORE);
                  store.put({ pageIndex, data: pageData });
                  tx.oncomplete = () => resolve(true);
                  tx.onerror = () => resolve(false);
                });
              } catch { return false; }
            }
            
            async function getCachedPages(startIndex, endIndex) {
              try {
                const db = await openPageCache();
                return new Promise((resolve) => {
                  const tx = db.transaction(PAGE_CACHE_STORE, "readonly");
                  const store = tx.objectStore(PAGE_CACHE_STORE);
                  const pages = [];
                  const request = store.openCursor();
                  request.onsuccess = (e) => {
                    const cursor = e.target.result;
                    if (cursor) {
                      if (cursor.value.pageIndex >= startIndex && cursor.value.pageIndex <= endIndex) {
                        pages.push(cursor.value);
                      }
                      cursor.continue();
                    } else {
                      resolve(pages.sort((a, b) => a.pageIndex - b.pageIndex).map(p => p.data));
                    }
                  };
                  request.onerror = () => resolve([]);
                });
              } catch { return []; }
            }
            
            async function getCacheMeta() {
              try {
                const db = await openPageCache();
                return new Promise((resolve) => {
                  const tx = db.transaction(PAGE_META_STORE, "readonly");
                  const store = tx.objectStore(PAGE_META_STORE);
                  const request = store.get("meta");
                  request.onsuccess = () => resolve(request.result || null);
                  request.onerror = () => resolve(null);
                });
              } catch { return null; }
            }
            
            async function setCacheMeta(totalPages, lastModified) {
              try {
                const db = await openPageCache();
                return new Promise((resolve) => {
                  const tx = db.transaction(PAGE_META_STORE, "readwrite");
                  const store = tx.objectStore(PAGE_META_STORE);
                  store.put({ key: "meta", totalPages, lastModified });
                  tx.oncomplete = () => resolve(true);
                  tx.onerror = () => resolve(false);
                });
              } catch { return false; }
            }
            
            async function clearPageCache() {
              try {
                const db = await openPageCache();
                return new Promise((resolve) => {
                  const tx = db.transaction([PAGE_CACHE_STORE, PAGE_META_STORE], "readwrite");
                  tx.objectStore(PAGE_CACHE_STORE).clear();
                  tx.objectStore(PAGE_META_STORE).clear();
                  tx.oncomplete = () => resolve(true);
                  tx.onerror = () => resolve(false);
                });
              } catch { return false; }
            }

            // Check the subscription status of the logged in user.
            async function subscribed(options = {}) {
              if (!user) return false;
              const body = { retrieve: "everything", ...options };
              const response = await userRequest(
                "POST",
                "/sotce-net/subscribed",
                body,
              );

              if (response.status === 200) {
                if (response.subscribed) {
                  return response;
                } else {
                  return false;
                }
              } else {
                console.error("Subscription check failed:", response);
                return "error";
              }
            }

            // Cancel an existing subscription.
            async function cancel() {
              if (!user) return;

              const confirmation = confirm("📆 End your subscription?");
              if (!confirmation) return;
              veil();

              try {
                const response = await fetch("/sotce-net/cancel", {
                  method: "POST",
                  headers: {
                    "Content-Type": "application/json",
                    Authorization:
                      "Bearer " +
                      (window.sotceTOKEN ||
                        (await auth0Client.getTokenSilently())),
                  },
                });
                if (response.ok) {
                  const result = await response.json();
                  console.log("💳❌ Subscription cancelled:", result);
                  const entered = await subscribed();
                  if (entered?.subscribed) {
                    status = "subscribed";
                    subscription = entered;
                    await garden(entered, user, true); // Open garden and show the gate.
                    unveil({ instant: true });
                    setTimeout(() => alert(result.message), 100);
                  } else {
                    status = "verified";
                    subscription = null;
                    // unveil({ instant: true });
                    flash("red", { hold: true });
                    setTimeout(() => window.location.reload(), 150);
                  }
                } else {
                  const error = await response.json();
                  console.error("Cancellation error:", error.message);
                  unveil({ instant: true });
                  setTimeout(
                    () =>
                      alert("Failed to cancel subscription: " + error.message),
                    100,
                  );
                }
              } catch (error) {
                console.error("Error:", error);
                alert(
                  "A network error occurred while cancelling your subscription. Please try again.",
                );
              }
            }

            function logout() {
              if (isAuthenticated) {
                console.log("🔐 Logging out...", window.location.href);
                chat?.system?.server?.send("logout"); // Log out of chat.
                if (embedded) {
                  localStorage.removeItem("session-sotce");
                  window.parent.postMessage(
                    { type: "logout", tenant: "sotce" },
                    "*",
                  );
                } else {
                  auth0Client.logout({
                    logoutParams: { returnTo: "${HOST}" },
                  });
                }
              } else console.log("🔐 Already logged out!");
            }

            // resend verification email
            function resend(e, type) {
              e?.preventDefault();
              clearTimeout(verificationTimeout);

              const promptText =
                type === "change"
                  ? "💌 Update your email to?"
                  : "💌 Resend verification email to?";

              const email = prompt(promptText, user.email);
              if (!email) return;

              veil();
              console.log("📧 Resending...", email);

              userRequest("POST", "/api/email", {
                name: email,
                email,
                tenant: "sotce",
              })
                .then(async (res) => {
                  if (res.status === 200) {
                    await retrieveUncachedUser();
                    console.log("📧 Email verification sent...", res);
                    const gateEl = await gate("unverified", user);
                    unveil({ instant: true });
                    gateEl.classList.remove("obscured");
                    setTimeout(
                      () => alert("📧 Check your \`" + email + "\` inbox."),
                      100,
                    );
                  } else {
                    throw new Error(res.code);
                  }
                })
                .catch((err) => {
                  console.error("🔴 📧 Email change error:", err);
                  unveil({ instant: true });
                  setTimeout(
                    () => alert("❌ Sorry, your email could not be changed."),
                    100,
                  );
                });
            }

            // Jump to Aesthetic Computer.
            function aesthetic() {
              let href = dev
                ? "https://localhost:8888"
                : "https://aesthetic.computer";
              if (embedded) {
                window.parent.postMessage(
                  { type: "url:updated", slug: "prompt" },
                  "*",
                );
                href += "?session-aesthetic=retrieve";
              }
              window.location.href = href;
            }

            async function userRequest(method, endpoint, body) {
              let token;
              try {
                token =
                  window.sotceTOKEN || (await auth0Client.getTokenSilently());
              } catch (error) {
                console.error(error);
                logout();
              }

              try {
                // console.log("🚏 Making user request with token:", token);
                if (!token) throw new Error("🧖 Not logged in.");

                const headers = {
                  Authorization: "Bearer " + token,
                  "Content-Type": "application/json",
                };

                const options = { method, headers };

                if (method.toLowerCase() !== "get" && body)
                  options.body = JSON.stringify(body);

                const response = await fetch(endpoint, options);

                if (response.status === 500) {
                  try {
                    const json = await response.json();
                    return { status: response.status, ...json };
                  } catch (e) {
                    return {
                      status: response.status,
                      message: response.statusText,
                    };
                  }
                } else {
                  const clonedResponse = response.clone();
                  try {
                    return {
                      ...(await clonedResponse.json()),
                      status: response.status,
                    };
                  } catch (error) {
                    console.log("🚫 Error:", error);
                    return {
                      status: response.status,
                      body: await response.text(),
                    };
                  }
                }
              } catch (error) {
                console.error("🚫 Error:", error);
                return { message: "unauthorized" };
              }
            }

            window.login = login;
            window.logout = logout;
            window.cancel = cancel;
            window.signup = signup;
            window.resend = resend;
          </script>
        </body>
      </html>
    `;
    return respond(200, body, { "Content-Type": "text/html; charset=utf-8" });
  } else if (path === "/subscribers" && method === "get") {
    const subscribers = await getActiveSubscriptionCount(productId);

    if (subscribers !== undefined && subscribers !== null) {
      return respond(200, { subscribers });
    } else {
      return respond(500, { message: "Could not get subscriber count." });
    }
  } else if (path === "/subscribe" && method === "post") {
    try {
      const stripe = Stripe(key);
      const redirectPath =
        event.headers.origin === "https://sotce.net" ? "" : "sotce-net";

      shell.log(
        "🗺️ Origin:",
        event.headers.origin,
        "redirectPath:",
        redirectPath,
      );

      const { email, sub } = JSON.parse(event.body);

      // Search for the customer by the metadata field 'sub'

      const customers = await stripe.customers.search({
        query: `metadata['sub']:'${sub}'`,
      });

      let customer;

      if (customers.data.length > 0) {
        // Customer found, optionally update metadata
        // customer = await stripe.customers.update(customers.data[0].id, {
        //  metadata: { sub },
        // });
        customer = customers.data[0];

        // 🛡️ Check if the customer already has an active sotce-net subscription
        // This prevents duplicate subscriptions from being created (bug fix 2025.12.03)
        const existingSubscriptions = await stripe.subscriptions.list({
          customer: customer.id,
          status: "active",
          limit: 10,
        });

        const hasActiveSotceNetSub = existingSubscriptions.data.some((sub) =>
          sub.items.data.some((item) => item.price.product === productId),
        );

        if (hasActiveSotceNetSub) {
          shell.log(
            "⚠️ Customer already has an active sotce-net subscription:",
            customer.id,
          );
          return respond(400, {
            message: "You already have an active subscription.",
            alreadySubscribed: true,
          });
        }
      } else {
        // Customer doesn't exist, create a new one
        customer = await stripe.customers.create({
          email: email,
          metadata: { sub },
        });
      }

      const session = await stripe.checkout.sessions.create({
        payment_method_types: ["card"],
        mode: "subscription",
        line_items: [{ price: priceId, quantity: 1 }],
        customer: customer.id, // Attach the existing or newly created customer
        success_url: `${event.headers.origin}/${redirectPath}?notice=success`,
        cancel_url: `${event.headers.origin}/${redirectPath}?notice=cancel`,
        // metadata: { sub },
      });

      return respond(200, { id: session.id });
    } catch (error) {
      shell.log("⚠️", error);
      return respond(500, { message: `Error: ${error.message}` });
    }
  } else if (path === "/subscribed" && method === "post") {
    // First validate that the user has an active session via auth0.

    // TODO: To properly handle rate limits on the `/userinfo` endpoint
    //       I should probably be sending the user info up in the POST
    //       request here and then the `authorize` function should
    //       be rewritten to use a different endpoint which has
    //       a more apt rate limit?
    // Also it's possible that finding the subscription information here
    // via Stripe should / could be cached in redis for faster
    // retrieval later? 24.08.24.19.22

    const body = JSON.parse(event.body); // Make sure we can parse the body.
    const retrieve = body.retrieve || "subscription";

    const user = await authorize(event.headers, "sotce");
    if (!user) {
      return respond(401, { message: "Unauthorized." });
    }

    const subscription = await subscribed(user);

    if (subscription === null) {
      return respond(500, { error: "Failed to fetch subscription status" });
    }

    if (subscription?.subscribed === false || !subscription) {
      return respond(200, { subscribed: false });
    }

    if (subscription?.status === "active") {
      // What did we need the subscription for?

      const out = { subscribed: true };

      // Include both pages and the subscription state if retrieving 'everything'.
      if (retrieve === "everything") {
        shell.log("🫐 Retrieving pages...", performance.now());

        // 📆 Subscription status. (until, renews)
        out.until = subscription.cancel_at_period_end
          ? new Date(subscription.cancel_at * 1000).toLocaleDateString(
              "en-US",
              dateOptions,
            )
          : "recurring";
        if (out.until === "recurring") {
          out.renews = new Date(subscription.current_period_end * 1000).toLocaleDateString("en-US", dateOptions);
        }

        // 👸 Administrator status.
        // Skip hasAdmin call if using admin_bypass (already verified admin)
        const isAdmin = subscription.admin_bypass ? true : await hasAdmin(user, "sotce");
        if (isAdmin) out.admin = isAdmin;
        shell.log("🔴 Admin:", isAdmin);

        // 📓 Recent Pages (with pagination support)
        const database = await connect();
        const pages = database.db.collection("sotce-pages");
        
        // Pagination parameters
        const requestedPage = body.pageNumber; // Specific page number (1-indexed)
        const limit = Math.min(body.limit || 5, 500); // Default to 5, max 500 pages per request
        const offset = body.offset || 0; // For loading older pages
        const metaOnly = body.metaOnly; // Only return page count and last modified
        
        shell.log("📄 Pagination: requestedPage=", requestedPage, "limit=", limit, "offset=", offset);
        
        // Always get total count and last modified for cache validation
        const totalCount = await pages.countDocuments({ state: "published" });
        const lastModifiedDoc = await pages.findOne(
          { state: "published" },
          { sort: { updatedAt: -1 }, projection: { updatedAt: 1, when: 1 } }
        );
        out.totalPages = totalCount;
        out.lastModified = lastModifiedDoc?.updatedAt || lastModifiedDoc?.when || null;
        
        if (metaOnly) {
          await database.disconnect();
          return respond(200, out);
        }
        
        let retrievedPages;
        
        if (requestedPage !== undefined) {
          // Fetch a specific page by its index (1-indexed)
          retrievedPages = await pages
            .aggregate([
              { $match: { state: "published" } },
              { $sort: { when: 1 } },
              { $skip: requestedPage - 1 },
              { $limit: 1 },
            ])
            .toArray();
          out.pageIndex = requestedPage;
        } else {
          // Fetch latest pages (from the end), with optional offset for loading older
          retrievedPages = await pages
            .aggregate([
              { $match: { state: "published" } },
              { $sort: { when: -1 } }, // Newest first
              { $skip: offset },
              { $limit: limit },
            ])
            .toArray();
          // Reverse to maintain chronological order
          retrievedPages.reverse();
          out.hasMore = offset + limit < totalCount;
        }

        // Add a 'handle' field to each page record.
        const subsToHandles = {}; // Cache handles on this go around.
        for (const [index, page] of retrievedPages.entries()) {
          let handle = subsToHandles[page.user];
          if (!handle) {
            handle = await handleFor(page.user, "sotce");
            if (handle) subsToHandles[page.user] = handle;
          }
          page.handle = handle;
        }

        out.pages = retrievedPages;

        // ❓ Also fetch answered questions to mix into the feed
        const asks = database.db.collection("sotce-asks");
        const answeredQuestions = await asks
          .find({ state: "answered" })
          .sort({ answeredAt: -1 })
          .limit(50) // Reasonable limit for now
          .project({ draftAnswer: 0, draftStartedAt: 0, draftLastEditedAt: 0 })
          .toArray();
        
        // Add handles to questions
        for (const q of answeredQuestions) {
          let handle = subsToHandles[q.user];
          if (!handle) {
            handle = await handleFor(q.user, "sotce");
            if (handle) subsToHandles[q.user] = handle;
          }
          q.handle = handle;
          q.type = "question"; // Mark as question for client-side rendering
        }
        
        out.questions = answeredQuestions;
        out.totalQuestions = await asks.countDocuments({ state: "answered" });

        await database.disconnect();

        // TODO: 👤 'Handled' pages filtered by user..
        shell.log("🫐 Retrieved:", retrievedPages.length, "pages,", answeredQuestions.length, "questions", performance.now());
      }
      return respond(200, out);
    } else {
      return respond(200, { subscribed: false });
    }
  } else if (path === "/cancel" && method === "post") {
    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Unauthorized." });
    shell.log("User authorized. Cancelling subscription...");
    const cancelResult = await cancelSubscription(user, key);
    return respond(cancelResult.status, cancelResult.body);
  } else if (path === "/write-a-page" && method === "post") {
    // 🪧 write-a-page - Submission endpoint.
    // TODO: Make this path RESTful with alternate methods to represent the resource. 24.10.05.23.33

    const user = await authorize(event.headers, "sotce");
    const isAdmin = await hasAdmin(user, "sotce");
    if (!user || !isAdmin) return respond(401, { message: "Unauthorized." });

    // TODO: 🟠 Add support for creating a draft.
    const body = JSON.parse(event.body);

    shell.log("🪧 Page to post:", body);

    if (body.draft === "retrieve-or-create") {
      const database = await connect();
      const pages = database.db.collection("sotce-pages");
      await pages.createIndex({ user: 1, state: 1 }); // Ensure 'user' and 'state' index.

      // Try to get the last page from this user.sub where 'state' is 'draft'.
      let page = await pages.findOne(
        { user: user.sub, state: "draft" },
        { sort: { when: -1 } },
      );

      // If that page does not exist, then insert a new one with the 'draft' state.
      if (!page) {
        const insertion = await pages.insertOne({
          user: user.sub,
          words: "",
          when: new Date(),
          state: "draft",
        });
        page = await pages.findOne({ _id: insertion.insertedId });
      }
      await database.disconnect();
      return respond(200, { page });
    } else if (body.draft === "keep") {
      const database = await connect();
      const pages = database.db.collection("sotce-pages");
      // Try to get the last page from this user.sub where 'state' is 'draft'.
      let page = await pages.findOne(
        { user: user.sub, state: "draft" },
        { sort: { when: -1 } },
      );

      // If the page exists, update the draft with the new content.
      if (page) {
        await pages.updateOne(
          { _id: page._id },
          { $set: { words: body.words } },
        );
        page = await pages.findOne({ _id: page._id });
      }
      // If no draft exists, create a new draft with the content.
      else {
        const insertion = await pages.insertOne({
          user: user.sub,
          words: body.words || "", // Use provided content or default to an empty string
          when: new Date(),
          state: "draft",
        });
        page = await pages.findOne({ _id: insertion.insertedId });
      }
      await database.disconnect();
      return respond(200, { page });
    } else if (body.draft === "crumple") {
      //  🪓️ Delete (crumple) the current draft.
      const database = await connect();
      const pages = database.db.collection("sotce-pages");

      // See if there is a page id attached, otherwise look for the most
      // recent draft...
      let page;

      if (body._id) {
        page = await pages.findOne({ _id: new ObjectId(body._id) });
      } else {
        // Try to get the last page from this user.sub where 'state' is 'draft'.
        page = await pages.findOne(
          { user: user.sub, state: "draft" },
          { sort: { when: -1 } },
        );
      }

      // If the page exists, update its state to 'crumpled' or delete it if body.words is empty/undefined.
      if (page) {
        if (
          page.state === "draft" &&
          (!body.words || body.words.length === 0)
        ) {
          shell.log("❌ Permanently deleting page:", page._id);
          await pages.deleteOne({ _id: page._id });
        } else {
          const updates = { state: "crumpled" };
          if (body.words) updates.words = body.words;
          await pages.updateOne({ _id: page._id }, { $set: updates });
        }

        await database.disconnect();
        return respond(200, { message: "Draft crumpled successfully." });
      } else {
        await database.disconnect();
        return respond(500, { message: "No page found to crumple." });
      }

      // Actually just set the state to 'crumpled' here.
    } else if (body.draft) {
      return respond(500, { message: "Invalid drafting option." });
    }

    // 💡 TODO: Eventually create a 'books' abstraction so users can have
    // multiple books that capture groupings of pages. 24.09.13.01.41
    const words = body.words;
    if (words) {
      const database = await connect();
      const pages = database.db.collection("sotce-pages");

      // Find the existing draft for the user.
      let page = await pages.findOne(
        { user: user.sub, state: "draft" },
        { sort: { when: -1 } },
      );

      // If a draft exists, update it with the new words and set the state to 'published'.
      if (page) {
        await pages.updateOne(
          { _id: page._id },
          { $set: { words, state: "published" } },
        );
      } else {
        // If no draft exists, insert a new page. (Edge case where the
        // date would be updated in a new page on a double save / overwrite)
        const insertion = await pages.insertOne({
          user: user.sub,
          words,
          when: new Date(),
          state: "published",
        });
        page = await pages.findOne({ _id: insertion.insertedId });
      }
      await database.disconnect();
      return respond(200, { page });
    } else {
      return respond(500, { message: "No words written." });
    }
  } else if (path === "/touch-a-page" && method === "post") {
    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Unauthorized." });

    // Make sure the user is subscribed before they can touch a page.
    const subscription = await subscribed(user);

    if (!subscription || subscription.status !== "active") {
      return respond(500, { message: "User not subscribed." });
    }

    const body = JSON.parse(event.body);
    shell.log("💁 Page to touch:", body);

    const id = new ObjectId(body._id);

    const database = await connect();
    const pages = database.db.collection("sotce-pages");
    const page = await pages.findOne({ _id: id });

    // console.log("📃 Page:", page, id);

    if (page) {
      const touches = database.db.collection("sotce-touches");

      // Try to touch the page.
      if (page.user !== user.sub) {
        // Don't let users touch pages they created.
        await touches.createIndex({ user: 1, page: 1 }, { unique: true });

        try {
          // Insert touch, assuming 'user.sub' contains the user's sub identifier
          await touches.insertOne({
            user: user.sub, // User's sub identifier
            page: id, // Page ID from the request body
            when: new Date(), // Current date and time
          });
        } catch (error) {
          if (error.code === 11000) {
            // Duplicate key error, meaning the user has already touched this page
            console.log("User has already touched this page.");
          } else {
            console.error(
              "An error occurred while touching the page:",
              error.message,
            );
          }
        }
      }

      // Fetch all touches for the page, even if an error occurred ot a touch did not happen.
      const pageTouches = await touches.find({ page: id }).toArray();

      // Add a 'handle' field to each touch record.
      const handles = [];
      for (const [index, touch] of pageTouches.entries()) {
        // if (touch.user === user.sub) continue;
        handle = await handleFor(touch.user, "sotce"); // Cross-network handle request.
        if (handle) {
          handles.push("@" + handle);
        } // else {
        // TODO: Eventually track other touches?
        // }
      }

      await database.disconnect();
      return respond(200, { touches: handles });
    } else {
      await database.disconnect();
      return respond(404, { message: "No page found to touch." });
    }
  } else if (path === "/delete-account" && method === "post") {
    // See also the 'delete-erase-and-forget-me.js' function for aesthetic users.
    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Authorization failure..." });
    shell.log("🔴 Deleting user:", user.sub);

    const sub = user.sub;
    const sotceSub = "sotce-" + sub;

    // 1. Unsubscribe the user if they have an active subscription.
    try {
      const cancelResult = await cancelSubscription(user, key);
      shell.log(
        "❌ Cancelled subscription?",
        cancelResult.status,
        cancelResult.body,
      );
    } catch (err) {
      shell.error("🔴 Subscription cancellation error:", err);
    }

    // 2. Delete any user data, like posts.
    const database = await connect();

    // 🗨️ Clear any chat messages owned by the user.
    // Rewrite the "text" field to be null / empty and rewrite the user field to be empty
    // rather than simply deleting the records associated with the user sub.
    await database.db
      .collection("chat-sotce")
      .updateMany({ user: sub }, { $set: { text: "", user: "" } });
    console.log("🧠 Erased chats.");

    // Remove the user's handle cache from redis.
    const handle = await getHandleOrEmail(sotceSub);
    if (handle?.startsWith("@")) {
      await KeyValue.connect();
      await KeyValue.del("@handles", handle);
      await KeyValue.del("userIDs", sotceSub);
      await KeyValue.disconnect();
    }

    // 3. Delete the user's handle if it exists and the user does not have
    //    an aesthetic computer account, otherwise re-associate the key.
    if (handle) {
      shell.log(
        "📚 Checking for any `aesthetic` user with the same email and handle:",
        handle,
      );
      const bareHandle = handle.slice(1); // Remove the "@" from the handle.
      const idRes = await userIDFromEmail(user.email, "aesthetic");
      if (idRes?.userID && idRes?.email_verified) {
        const handles = database.db.collection("@handles");

        const aestheticSub = idRes.userID;
        // Check if an entry with the same _id already exists
        const existingHandle = await handles.findOne({ _id: aestheticSub });

        if (!existingHandle) {
          // If no existing entry, proceed with deletion and insertion
          await handles.deleteOne({ _id: sotceSub });
          await handles.insertOne({ _id: aestheticSub, handle: bareHandle });
          shell.log(
            "🧔 Changed primary handle key of 'sotce' user:",
            sub,
            "to 'aesthetic' user:",
            aestheticSub,
          );
        } else {
          // If an entry already exists, skip deletion and insertion
          shell.log(
            "🩹 Handle already native to `aesthetic`, skipping reassignment.",
          );
        }
      } else {
        await database.db.collection("@handles").deleteOne({ _id: sotceSub });
        shell.log("🧔 Deleted user handle for:", sotceSub);
      }
    }

    shell.log("❌ Deleted database data.");

    await database.disconnect();

    // 3. Delete the user's auth0 account.
    const deleted = await deleteUser(sub, "sotce");
    shell.log("❌ Deleted user registration:", deleted, user.email);
    return respond(200, { result: "Deleted!" }); // Successful account deletion.
  } else if (path === "/ask" && method === "post") {
    // ❓ Submit a question
    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Unauthorized." });

    const subscription = await subscribed(user);
    if (!subscription || subscription.status !== "active") {
      return respond(403, { message: "Subscription required." });
    }

    const body = JSON.parse(event.body);
    const question = body.question?.trim();

    if (!question || question.length === 0) {
      return respond(400, { message: "Question cannot be empty." });
    }

    if (question.length > 500) {
      return respond(400, { message: "Question too long (max 500 chars)." });
    }

    const database = await connect();
    const asks = database.db.collection("sotce-asks");

    // Look up the user's handle to store with the question
    const askerHandle = await handleFor(user.sub, "sotce");

    const insertion = await asks.insertOne({
      user: user.sub,
      handle: askerHandle || null,
      question,
      when: new Date(),
      state: "pending",
    });

    await database.disconnect();
    shell.log("❓ Question submitted:", insertion.insertedId);
    return respond(200, { _id: insertion.insertedId });
  } else if (path === "/asks" && method === "get") {
    // ❓ Get user's own questions
    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Unauthorized." });

    const database = await connect();
    const asks = database.db.collection("sotce-asks");

    const userAsks = await asks.find({ user: user.sub })
      .sort({ when: -1 })
      .limit(50)
      .project({ draftAnswer: 0, answer: 0, answeredBy: 0 }) // Don't expose answers to users yet
      .toArray();

    await database.disconnect();
    return respond(200, { asks: userAsks });
  } else if (path === "/asks/pending" && method === "get") {
    // ❓ Get pending questions (admin only)
    const user = await authorize(event.headers, "sotce");
    const isAdmin = await hasAdmin(user, "sotce");
    if (!user || !isAdmin) return respond(401, { message: "Unauthorized." });

    const database = await connect();
    const asks = database.db.collection("sotce-asks");

    const pending = await asks.find({ state: "pending" })
      .sort({ when: 1 })
      .limit(100)
      .toArray();

    // Resolve handles for questions that don't have one stored
    for (const q of pending) {
      if (!q.handle && q.user) {
        const h = await handleFor(q.user, "sotce");
        if (h) q.handle = h;
      }
    }

    await database.disconnect();
    return respond(200, { asks: pending });
  } else if (path.match(/^\/ask\/[a-f0-9]+\/respond$/) && method === "post") {
    // ❓ Respond to a question (admin only)
    const user = await authorize(event.headers, "sotce");
    const isAdmin = await hasAdmin(user, "sotce");
    if (!user || !isAdmin) return respond(401, { message: "Unauthorized." });

    const askId = path.split("/")[2];
    if (!askId) return respond(400, { message: "Missing question ID." });

    const { answer } = JSON.parse(event.body || "{}");
    if (!answer || !answer.trim()) {
      return respond(400, { message: "Response cannot be empty." });
    }
    if (answer.length > 2000) {
      return respond(400, { message: "Response too long (max 2000 chars)." });
    }

    const database = await connect();
    const asks = database.db.collection("sotce-asks");

    // Find the question
    const question = await asks.findOne({ _id: new ObjectId(askId) });
    if (!question) {
      await database.disconnect();
      return respond(404, { message: "Question not found." });
    }

    // Update the question with the answer
    const result = await asks.updateOne(
      { _id: new ObjectId(askId) },
      {
        $set: {
          state: "answered",
          answer: answer.trim(),
          answeredBy: user.sub,
          answeredAt: new Date().toISOString(),
        },
      }
    );

    if (result.modifiedCount === 0) {
      await database.disconnect();
      return respond(500, { message: "Could not save response." });
    }

    // Create a published page with the Q&A
    const pages = database.db.collection("sotce-pages");
    const askerHandle = question.handle || "@anonymous";
    const pageWords = `${askerHandle} asks @amelia\n\n${question.question}\n\n---\n\n@amelia responds\n\n${answer.trim()}`;
    
    await pages.insertOne({
      user: user.sub,
      words: pageWords,
      when: new Date(),
      state: "published",
      questionId: askId, // Link back to the question
      isQA: true, // Mark as Q&A page
    });

    await database.disconnect();

    shell.log("❓ Question answered:", askId, "by", user.email);
    return respond(200, { success: true, askId });
  } else if (path.match(/^\/ask\/[a-f0-9]+\/save-draft$/) && method === "post") {
    // ❓ Save a draft response (admin only) - also marks draftStartedAt
    const user = await authorize(event.headers, "sotce");
    const isAdmin = await hasAdmin(user, "sotce");
    if (!user || !isAdmin) return respond(401, { message: "Unauthorized." });

    const askId = path.split("/")[2];
    if (!askId) return respond(400, { message: "Missing question ID." });

    const { draft } = JSON.parse(event.body || "{}");

    const database = await connect();
    const asks = database.db.collection("sotce-asks");

    const question = await asks.findOne({ _id: new ObjectId(askId) });
    if (!question) {
      await database.disconnect();
      return respond(404, { message: "Question not found." });
    }

    const updateFields = {
      draftLastEditedAt: new Date().toISOString(),
    };
    if (!question.draftStartedAt) {
      updateFields.draftStartedAt = new Date().toISOString();
    }
    if (draft !== undefined) {
      updateFields.draftAnswer = draft;
    }

    await asks.updateOne(
      { _id: new ObjectId(askId) },
      { $set: updateFields }
    );

    await database.disconnect();
    shell.log("❓ Draft saved for:", askId, "by", user.email);
    return respond(200, { success: true, askId });
  } else if (path.match(/^\/ask\/[a-f0-9]+\/reject$/) && method === "post") {
    // ❓ Reject a question (admin only)
    const user = await authorize(event.headers, "sotce");
    const isAdmin = await hasAdmin(user, "sotce");
    if (!user || !isAdmin) return respond(401, { message: "Unauthorized." });

    const askId = path.split("/")[2];
    if (!askId) return respond(400, { message: "Missing question ID." });

    const database = await connect();
    const asks = database.db.collection("sotce-asks");

    // Find the question
    const question = await asks.findOne({ _id: new ObjectId(askId) });
    if (!question) {
      await database.disconnect();
      return respond(404, { message: "Question not found." });
    }

    // Update the question state to rejected
    const result = await asks.updateOne(
      { _id: new ObjectId(askId) },
      {
        $set: {
          state: "rejected",
          rejectedBy: user.sub,
          rejectedAt: new Date().toISOString(),
        },
      }
    );

    await database.disconnect();

    if (result.modifiedCount === 0) {
      return respond(500, { message: "Could not reject question." });
    }

    shell.log("❓ Question rejected:", askId, "by", user.email);
    return respond(200, { success: true, askId });
  } else if (path === "/asks/clear-all" && method === "delete") {
    // ❓ Clear all questions (admin only) - for development/reset
    const user = await authorize(event.headers, "sotce");
    const isAdmin = await hasAdmin(user, "sotce");
    if (!user || !isAdmin) return respond(401, { message: "Unauthorized." });

    const database = await connect();
    const asks = database.db.collection("sotce-asks");
    
    const result = await asks.deleteMany({});
    
    await database.disconnect();
    shell.log("❓ All questions cleared:", result.deletedCount, "by", user.email);
    return respond(200, { success: true, deletedCount: result.deletedCount });
  } else if (path.match(/^\/ask\/[a-f0-9]+$/) && method === "delete") {
    // ❓ Delete own pending question (only if no draft started by @amelia)
    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Unauthorized." });
    const askId = path.replace("/ask/", "");
    if (!askId) return respond(400, { message: "Missing question ID." });
    const database = await connect();
    const asks = database.db.collection("sotce-asks");
    // Only allow deletion if: owned by user, still pending, and no draft started
    const question = await asks.findOne({ _id: new ObjectId(askId) });
    if (!question) {
      await database.disconnect();
      return respond(404, { message: "Question not found." });
    }
    if (question.user !== user.sub) {
      await database.disconnect();
      return respond(403, { message: "Not your question." });
    }
    if (question.state !== "pending") {
      await database.disconnect();
      return respond(400, { message: "Cannot delete — already answered." });
    }
    if (question.draftStartedAt) {
      await database.disconnect();
      return respond(400, { message: "Cannot delete — @amelia has started drafting a response." });
    }
    const result = await asks.deleteOne({ _id: new ObjectId(askId) });
    await database.disconnect();
    shell.log("❓ Question deleted:", askId, "by", user.sub);
    return respond(200, { deleted: true });
  } else if (path === "/privacy-policy" && method === "get") {
    const subscribers = await getActiveSubscriptionCount(productId);

    const body = html`
      <html>
        <head>
          <title>Sotce Net's Privacy Policy</title>
          <meta name="description" content="The privacy policy for Sotce Net." />
          <link rel="icon" type="image/png" href="${assetPath}cookie.png" />
          <style>
            body {
              font-family: sans-serif;
              background-color: rgb(255, 251, 234);
              -webkit-text-size-adjust: none;
            }
            img {
              filter: grayscale(0.75);
              drop-shadow(-2px 0px 1px rgba(0, 0, 0, 0.35));
            }
            code {
              font-weight: bold;
            }
            a,
            a:visited {
              color: black;
              text-decoration: none;
            }
            sub {
              line-height: 1.25em;
            }
          </style>
          <script>
            window.dataLayer = window.dataLayer || [];
            function gtag() {
              dataLayer.push(arguments);
            }
            gtag("js", new Date());

            gtag("config", "G-8CWWH29LJD");
          </script>
          <meta
            name="viewport"
            content="width=device-width, initial-scale=1.0"
          />
        </head>
        <body>
          <h1>Sotce Net's Privacy Policy</h1>
          <p>
            Sotce Net keeps pages on a server for subscribers to read.
          </p>
          <p>
            You can associate an email with a <code>@handle</code> to represent your identity.
          </p>
          <p>
            We use cookies and third-party services for login, analytics, and payments.
          </p>
          <p>
            We federate handles with <code><a href="https://aesthetic.computer/privacy-policy">Aesthetic Computer</a></code> — same email means shared <code>@handle</code>.
          </p>
          <p>
            We do not sell your data.
          </p>
          <p>
            Delete your account from the settings page. Write to <code>mail@sotce.net</code> with questions.
          </p>
          <p>
            Brought to you by <code><a href="https://sotce.com">Sotce</a></code> and <code><a href="https://aesthetic.computer">Aesthetic Computer</a></code>.
          </p>
          ${subscribers > 0 ? "<p>Sotce Net has <code>" + subscribers + "</code> active subscriber" + (subscribers > 1 ? "s" : "") + ".</p>" : ""}
          <a href="${dev ? "/sotce-net" : "/"}"><img width="128" src="${assetPath + "cookie.png"}" /></a>
          <br />
          <br />
          <sub>February 2026</sub>
        </body>
      </html>
    `;
    return respond(200, body, { "Content-Type": "text/html; charset=utf-8" });
  }
};

async function cancelSubscription(user, key) {
  const email = user.email;
  const result = { status: undefined, body: undefined };
  try {
    const stripe = Stripe(key);
    // Fetch customer by email
    const customers = await stripe.customers.list({ email, limit: 1 });
    if (customers.data.length === 0) {
      result.status = 404;
      result.body = { message: "Customer not found." };
      return result;
    }

    const customer = customers.data[0];

    // Fetch subscriptions for the customer
    const subscriptions = await stripe.subscriptions.list({
      customer: customer.id,
      status: "all",
      limit: 10,
    });

    if (subscriptions.data.length === 0) {
      result.status = 404;
      result.body = { message: "Subscription not found." };
      return result;
    }

    // Find the subscription matching the productId
    const subscription = subscriptions.data.find((sub) =>
      sub.items.data.some((item) => item.price.product === productId),
    );

    if (!subscription) {
      result.status = 404;
      result.body = { message: "Subscription not found." };
      return result;
    }

    // Cancel the subscription
    const cancelled = await stripe.subscriptions.update(subscription.id, {
      cancel_at_period_end: true,
    });

    // Clear the redis cache for this subscriber.
    await KeyValue.connect();
    await KeyValue.del("sotce-subscribed", user.sub);
    await KeyValue.disconnect();

    result.status = 200;
    result.body = {
      message: `Your subscription ends on ${new Date(
        cancelled.cancel_at * 1000,
      ).toLocaleDateString(
        "en-US",
        dateOptions,
      )}. You will not be billed again.`,
      subscription: cancelled,
    };
    return result;
  } catch (error) {
    console.error("Error cancelling subscription:", error);
    result.status = 500;
    result.body = { message: `Error: ${error.message}` };
    return result;
  }
}

const analyticsScript = html`
  <script
    async
    src="https://www.googletagmanager.com/gtag/js?id=G-8CWWH29LJD"
  ></script>
  <script>
    window.dataLayer = window.dataLayer || [];
    function gtag() {
      dataLayer.push(arguments);
    }
    gtag("js", new Date());

    gtag("config", "G-8CWWH29LJD");
  </script>
`;

// Inserted in `dev` mode for live reloading.
const reloadScript = html`
  <script>
    let reconnectInterval = false;
    function connect() {
      clearInterval(reconnectInterval);
      let ws;
      try {
        let connectionUrl = "wss://localhost:8889";
        if (window.location.host === "local.aesthetic.computer") {
          connectionUrl = "wss://session.local.aesthetic.computer";
        }
        ws = new WebSocket(connectionUrl);
      } catch {
        console.warn("🧦 Connection failed.");
        return;
      }

      // ws.onopen = (e) => console.log("🧦 Connected:", e);
      let reloadTimeout;

      ws.onmessage = (e) => {
        const msg = JSON.parse(e.data);
        if (msg.type === "reload" && msg.content.piece === "*refresh*") {
          console.log("🧦 Reloading...");
          clearTimeout(reloadTimeout);
          reloadTimeout = setTimeout(() => {
            const sessionItem = localStorage.getItem("session-sotce");
            const gateUp = document.querySelector("#gate-curtain:not(.hidden)");
            const writingAPage = document.getElementById("#editor");
            if (sessionItem) {
              const url = new URL(window.location.href);
              url.searchParams.set("session-sotce", "retrieve");
              if (gateUp) {
                url.searchParams.set("gate", "up");
              } else if (writingAPage) {
                url.searchParams.set("writing", "page");
              }
              window.location.href = url.toString();
            } else {
              const url = new URL(window.location.href);
              if (gateUp) {
                url.searchParams.set("gate", "up");
              } else if (writingAPage) {
                url.searchParams.set("writing", "page");
              }
              window.location.href = url.toString();
            }
          }, 150); // 📓 Could take a sec for the function to reload... 24.08.08.00.53
        }
      };

      ws.onclose = (e) => {
        // console.log("🧦 🔴 Closed:", e);
        reconnectInterval = setInterval(connect, 1000);
      };
    }
    connect();
  </script>
`.trim();
