// Sotce Net, 24.06.13.06.38
// A paid diary network by Sotce & Aesthetic Computer.

/* #region 🟢 TODO 
  + Now
  - [🩷] Limit scrollback to 100 chats.
  - [] Show character limit / enforce on the text input.

  - [] Add a more accurate chatter count that comes from the server.
  - [] Finish bottom bar design.
  - [] Test guest chat interface with logged in (unsubscribed) user.
  - [] Add sound.

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

  const key = dev ? SOTCE_STRIPE_API_TEST_PRIV_KEY : SOTCE_STRIPE_API_PRIV_KEY;
  const assetPath = dev
    ? "/assets/sotce-net/"
    : "https://assets.aesthetic.computer/sotce-net/";

  // Check to see if a user sub is subscribed.

  async function subscribed(user) {
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
          shell.log("📰 Subscription active!", performance.now());
          await KeyValue.disconnect();
          return parsed;
        } else {
          shell.log(
            "⌛ Subscription expired, invalidating cache and checking...",
          );
          await KeyValue.del("sotce-subscribed", user.sub);
        }
      }

      // 🩷 Then query it from Stripe,

      const stripe = Stripe(key);
      // Fetch customer by user ID (sub) from subscription metadata field.
      const customers = await stripe.customers.search({
        query: "metadata['sub']:'" + user.sub + "'",
      });

      if (!customers.data.length) {
        await KeyValue.disconnect();
        return { subscribed: false };
      }
      const customer = customers.data[0];

      // Fetch subscriptions for the customer
      const subscriptions = await stripe.subscriptions.list({
        customer: customer.id,
        status: "active", // Only find the first active subscription.
        limit: 5,
      });

      const subscription = subscriptions.data.find((sub) =>
        sub.items.data.some((item) => item.price.product === productId),
      );

      if (subscription) {
        // 🩷 And serialize it into redis.
        await KeyValue.set(
          "sotce-subscribed",
          user.sub,
          JSON.stringify({
            status: subscription.status,
            current_period_end: subscription.current_period_end,
          }),
        );
      }

      await KeyValue.disconnect();

      return subscription;
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

  // 🏠 Home, Chat
  if (
    (path === "/" || path === "/chat" || path === "/gate") &&
    method === "get"
  ) {
    const miniBreakpoint = 245;

    const body = html`
      <html lang="en">
        <head>
          <meta charset="utf-8" />
          <title>Sotce Net</title>
          <meta name="description" content="for my worst thoughts" />
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
            content="width=device-width, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no, viewport-fit=cover"
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
                src: url('${assetPath}/helvetica.woff') format('woff'),
                    url('${assetPath}/fonts/arial/helvetica.ttf') format('truetype');
                font-weight: normal;
                font-style: normal;
            }

            @font-face {
                font-family: "Helvetica";
                src: url('${assetPath}/helvetica-bold.woff') format('woff'),
                    url('${assetPath}/fonts/arial/helvetica-bold.ttf') format('truetype');
                font-weight: bold;
                font-style: normal;
            }

            :root {
              -webkit-locale: "en";
              --background-color: rgb(255, 230, 225);
              --pink-border: rgb(255, 190, 215);
              --button-background: rgb(255, 235, 183);
              --button-background-highlight: rgb(255, 245, 170);
              --spinner-background: rgb(255, 147, 191);
              --backpage-color: rgb(250, 250, 250);
              --backpage-color-translucent: rgba(250, 250, 250, 0.8);
              --destructive-red: rgb(200, 0, 0);
              /* --line-height: 1.68em; */
              --line-height: 1.76em;
              /* --garden-background: rgb(187, 251, 254); // #bbfbfe; */
              --garden-background: rgb(204, 231, 255); // #bbfbfe;
              --chat-background: rgb(255, 230, 225); /* rgb(240, 235, 230); */
              --chat-input-bar-background: rgb(255, 240, 235); /* rgb(240, 235, 230); */
              /* --font-page: serif; */
              --editor-placemat-background: rgba(255, 255, 255, 0.5);
              --editor-placemat-background-opaque: rgb(255, 255, 255);
              /* --page-font: "EB Garamond"; */
              --page-font: "Helvetica"; /* "Carlito"; */ /* "Calibri"; */ /* "Inter"; */
              --max-lines: ${MAX_LINES};
            }

            @supports (-webkit-touch-callout: none) and
              (not (overflow: -moz-hidden-unscrollable)) {
              ::-webkit-scrollbar {
                width: 8px;
              }
              ::-webkit-scrollbar-thumb {
                background: rgba(255, 190, 215, 1);
              }
            }

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
            }
            #gate h2 {
              font-weight: normal;
              font-size: 100%;
              margin: 0;
              text-align: center;
              padding-bottom: 1em;
              user-select: none;
              -webkit-user-select: none;
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
            #chat-button {
              color: black;
              background: var(--button-background);
              padding: 0.35em;
              font-size: 100%;
              border: 0.205em solid var(--pink-border);
              filter: drop-shadow(-0.065em 0.065em 0.065em rgb(80, 80, 80));
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
              display: none;
              margin-left: 1em;
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
            #chat-button:hover {
              background: var(--button-background-highlight);
            }
            nav button:active,
            #write-a-page:active,
            /*#chat-enter:active,*/
            #pages-button:active,
            #chat-button:active {
              filter: none; /* drop-shadow(
                        -0.035em 0.035em 0.035em rgba(40, 40, 40, 0.8)
                      ); */
              background: rgb(255, 248, 165);
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
              background: rgb(203, 238, 161);
              border-color: rgb(114, 203, 80);
            }
            nav button.positive:hover {
              background: rgb(199, 252, 136);
            }
            nav button.positive:active {
              background: rgb(210, 252, 146);
            }
            nav button.negative {
              background: rgb(255 154 168);
              border-color: rgb(255, 87, 87);
            }
            nav button.negative:hover {
              background: rgb(255, 171, 171);
            }
            nav button.negative:active {
              background: rgb(255, 161, 186);
            }

            #garden {
              box-sizing: border-box;
              width: 100%;
              /*transition: 0.15s opacity;*/
              opacity: 1;
              background-color: var(--garden-background);
            }

            #garden.faded,
            #gate.faded {
              opacity: 0;
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
              padding-top: 100px;
              padding-left: 16px;
              padding-right: 16px;
              padding-bottom: 16px;
              margin-bottom: 8px;
              margin-left: auto;
              margin-right: auto;
              box-sizing: border-box;
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
              top: 1.5em;
              left: 0;
              width: 100%;
              text-align: center;
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
              z-index: 5;
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
              /* background-color: yellow; */
              width: 100%;
              aspect-ratio: 4 / 5;
              margin-bottom: 1em;
              box-sizing: border-box;
              position: relative;
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
              left: 0;
              width: 100%;
              text-align: center;
              color: black;
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
              width: 100vw;
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
              color: black;
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
              color: maroon;
            }
            #email:active {
              -webkit-tap-highlight-color: transparent;
              color: darkgreen;
            }
            #delete-account,
            #privacy-policy {
              color: black;
              position: absolute;
              font-size: 80%;
              bottom: -15%;
              user-select: none;
              left: 50%;
              transform: translateX(-51.5%);
              white-space: nowrap;
            }

            #subscriber-count {
              color: black;
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
              color: rgb(0, 0, 200);
            }
            #privacy-policy:active {
              color: blue;
            }
            #logout-wrapper,
            #imnew-wrapper,
            #secondary-wrapper {
              position: relative;
            }
            #cookie-menu {
              position: absolute;
              width: 100%;
              height: 100%;
              user-select: none;
              -webkit-user-select: none;
              cursor: pointer;
              transition: 0.2s ease-out transform;
              background-color: var(--pink-border);
              /* background-color: var(--spinner-background); */
              /* mask-image: url("${assetPath}cookie-open.png"); */
              /* filter: drop-shadow(-2px 0px 1px rgba(0, 0, 0, 0.35)); */
              mask-size: cover;
              -webkit-tap-highlight-color: transparent;
              touch-action: none;
              pointer-events: all;
            }
            #cookie-menu-wrapper:hover {
              transform: scale(0.97);
            }
            #cookie-menu-wrapper:active {
              transform: scale(0.94);
              transition: 0.13s ease-out transform;
            }
            #cookie-menu-wrapper.nogarden {
              filter: drop-shadow(0px -6px 6px var(--background-color))
                drop-shadow(4px -14px 0px var(--background-color));
            }
            #cookie-menu-wrapper {
              position: absolute;
              top: 0.225em;
              right: 0.25em;
              width: 90px;
              height: 90px;
              filter: drop-shadow(0px -6px 6px var(--garden-background))
                drop-shadow(4px -14px 0px var(--garden-background));
            }
            #cookie-menu-img {
              /* Used in lieu of a mask for now. */
              visibility: hidden;
              width: 60px;
              height: 60px;
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
              display: none;
              top: 0;
              left: 0;
              position: absolute;
              z-index: 2; /* This may be wrong. 24.11.05.22.43 */
              width: 100%;
              height: 100%;
              --chat-input-height: 2em;
              --chat-enter-width: 5em;
              --chat-input-border-color: rgb(130, 100, 100);
              /* --chat-input-border-color: var(--chat-input-bar-background); */
              overflow-y: scroll;
              background: var(--chat-background);
              /* opacity: 0.7; */
            }
            #chat.inaccessible {
              transition: 1.5s opacity;
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
            #chat.inaccessible #chat-disconnected {
              display: none;
            }
            #chat-messages {
              min-height: calc(100% - var(--chat-input-height));
              padding-bottom: var(--chat-input-height);
              /* padding-top: 1.25em; */
              display: flex;
              flex-direction: column-reverse;
            }
            #chat-messages div.message {
              border-bottom: 1.5px solid rgba(0, 0, 0, 0.15);
              box-sizing: border-box;
              padding: 0.25em;
              line-height: 1.25em;
              /* font-size: 85%; */
            }
            #chat-messages div.message div.message-author {
              font-weight: bold;
              display: inline-block;
              color: black; /* rgb(100, 0, 0); */
            }
            #chat-messages div.message div.message-content {
              display: inline-block;
              padding-left: 0.25em;
              color: rgb(50, 50, 50);
              user-select: text;
              word-wrap: break-word;
              max-width: calc(100% - 0.5em);
            }
            #chat-messages div.message div.message-when {
              opacity: 0.25;
              visibility: hidden;
              display: inline-block;
              font-size: 75%;
              padding-left: 0.5em;

            }
            #chat-messages div.message:hover div.message-when {
              visibility: visible;
            }
            #chat-input-bar {
              /* width: 100%; */ /* Set in JavaScript */
              /* background: var(--chat-background); */
              background: var(--chat-input-bar-background);
              height: var(--chat-input-height);
              display: flex;
              position: fixed;
              bottom: 0;
              overflow: scroll-y;
              border-top: 2px solid rgba(0, 0, 0, 0.1);
            }
            #chat-input-bar.sending * {
              pointer-events: none;
              opacity: 0.5;
            }
            #chat-handle {
              height: 100%;
              line-height: var(--chat-input-height);
              font-weight: bold;
              padding: 0 0.25em;
              vertical-align: center;
            }
            #chat-input {
              width: calc(100% - var(--chat-enter-width));
              height: 100%;
              margin: auto 0;
              /* margin-right: 1em; */
              /* margin-left: 0.5em; */
              /* border-radius: 0.5em; */
              /* border: none; */
              border: 2px solid var(--chat-input-border-color); /* rgb(130, 130, 130); */
              border-right: none;
              box-sizing: border-box;
              font-size: 100%;
              padding: 0.35em;
              /* border: none; */
            }
            #chat-input:focus {
              outline: none;
            }
            #chat-enter {
              width: var(--chat-enter-width);
              height: 100%;
              font-size: 100%;
              padding: 0.35em;
              border: 2px solid black; /*var(--chat-input-border-color);*/
              /* border-left: none; */
              box-sizing: border-box;
              margin: auto 0;
              /* margin-right: 0.35em; */
              background-color: var(--button-background);
              cursor: pointer;
            }
            #chat-enter:hover {
              background-color: var(--button-background-highlight);
            }
            #chat-enter:active {
              background-color: yellow;
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
              height: 100px;
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
              opacity: 0.35;
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

            const chatMessagesVeil = cel("div");
            chatMessagesVeil.id = "chat-messages-veil";
            chatInterface.appendChild(chatMessagesVeil);

            chatInterface.appendChild(chatDisconnected);

            const chatPagesButton = cel("button");
            chatPagesButton.id = "pages-button";
            chatPagesButton.innerText = "pages";
            chatInterface.appendChild(chatPagesButton);

            // Populate some test messages.
            const chatMessageData = [];

            function chatScrollToBottom(options = { always: false }) {
              const currentScroll = chatInterface.scrollTop;
              const toBottom =
                chatInterface.scrollHeight - chatInterface.clientHeight;
              if (options.always === true || currentScroll !== toBottom) {
                chatInterface.scrollTop = toBottom;
              }
            }

            function clearChatMessages() {
              chatMessages.replaceChildren();
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

            function chatAddMessage(text, handle, when) {
              const msg = cel("div");
              msg.classList.add("message");
              const by = cel("div");
              by.classList.add("message-author");
              const txt = cel("div");
              txt.classList.add("message-content");
              const date = cel("div");
              date.classList.add("message-when");
              txt.innerText = text;
              by.innerText = handle;
              date.innerText = chatWhenFormatter.format(new Date(when));
              msg.appendChild(by);
              msg.appendChild(txt);
              msg.appendChild(date);
              chatMessages.prepend(msg);
              // If there are more than 100 children, remove the last message.
              while (chatMessages.children.length > 100) {
                chatMessages.removeChild(chatMessages.lastChild);
              }
            }

            function chatAddEmpty() {
              const msg = cel("div");
              msg.classList.add("message");
              msg.innerText = "No messages yet.";
              msg.id = "chat-message-empty";
              chatMessages.prepend(msg);
            }

            // 👷 Dummy messages.
            //for (let i = 0; i <= 30; i++) {
            //  chatAddMessage("Hello", "@user");
            //}

            const chatInputBar = cel("div"); // Input bar.
            chatInputBar.id = "chat-input-bar";

            const scrollbarWidth = wrapper.offsetWidth - wrapper.clientWidth;
            const styleWidth = "calc(100%  - " + scrollbarWidth + "px)";

            chatInputBar.style.width = styleWidth;
            chatMessagesVeil.style.width = styleWidth;
            chatPagesButton.style.right = scrollbarWidth + "px";

            const chatHandle = cel("div");
            chatHandle.id = "chat-handle";
            chatHandle.innerText = "nohandle";

            const chatInput = cel("input"); // Input element.
            chatInput.id = "chat-input";
            chatInput.type = "text";
            chatInput.autocomplete = "off";

            const chatEnter = cel("button"); // Enter button.
            chatEnter.innerText = "Enter";
            chatEnter.id = "chat-enter";

            chatInputBar.appendChild(chatHandle);
            chatInputBar.appendChild(chatInput);
            chatInputBar.appendChild(chatEnter);

            chatInterface.appendChild(chatMessages);
            chatInterface.appendChild(chatInputBar);

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

            chatInput.addEventListener("focus", () => {
              window.addEventListener("keydown", chatEnterKeyListener);
            });

            chatInput.addEventListener("blur", () => {
              window.removeEventListener("keydown", chatEnterKeyListener);
            });

            chatPagesButton.addEventListener("click", () => {
              chatInterface.classList.add("hidden");
              chatInterface.classList.add("inaccessible");
              updatePath("/");
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
                  chatAddMessage(message.text, message.from, message.when);
                });

                if (chat.system.messages.length === 0) chatAddEmpty();

                const gateCurtain = document.querySelector("#gate-curtain");

                if (
                  gateCurtain &&
                  !gateCurtain.classList.contains("obscured")
                ) {
                  chatInterface.classList.remove("hidden");
                }
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

                // console.log("Got message:", msg);

                if (chat.system.messages.length === 1) {
                  document.getElementById("chat-message-empty")?.remove();
                }
                chatAddMessage(msg.text, msg.from, msg.when);
                chatScrollToBottom({ always: false });

                // messagesToAddToLayout.push(msg);
                // sound.play(messageSfx);
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
                    chatAddMessage(message.text, message.from, message.when);
                  });
                }

                console.log("👱 'handle' edit completed for:", content.handle);
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
            async function gate(status, user, subscription) {
              if (gating) return;
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

              cookieWrapper.classList.add("interactive");

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
                message = "for my worst thoughts";

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
                        const entered = await subscribed();
                        if (entered) {
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
                  curtain.classList.add("hidden");
                  cookieWrapper.classList.remove("interactive");

                  const garden = document.querySelector("#garden");

                  if (garden) {
                    document.documentElement.classList.add("garden");
                    garden.classList.remove("hidden");
                    updatePath("/");
                  } else {
                    // TODO: Make sure the icon cookie is visible.

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

              const imageLoadPromise = new Promise((resolve) => {
                img.onload = function () {
                  document.getElementById("gate-curtain")?.remove(); // Rid old curtain.
                  const checkObscurity = setInterval(() => {
                    if (!curtain.classList.contains("obscured")) {
                      g.classList.remove("faded");
                      // Check to see if the chat is connected.
                      if (!subscription && !chat.system.connecting) {
                        chatInterface.classList.remove("hidden");
                      }
                      clearInterval(checkObscurity);
                    }
                  }, 10);
                  wrapper.appendChild(curtain);
                  const email = document.getElementById("email");
                  if (email) {
                    email.onclick = (e) =>
                      resend(e, status === "unverified" ? undefined : "change");
                  }
                  resolve(); // Resolve the promise once the image is loaded.
                };
              });

              img.src = asset(
                status === "subscribed" ? "cookie-open.png" : "cookie.png",
              );

              await imageLoadPromise; // Wait for the image to load.
              gating = false;
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

              chatButton.onclick = function () {
                chatInterface.classList.remove("hidden");
                chatInterface.classList.remove("inaccessible");
                chatScrollToBottom();
                updatePath("/chat");
                if (window.sotceHandle) {
                  chatHandle.innerText = window.sotceHandle;
                }
              };

              topBar.appendChild(chatButton);
              // }

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
                  pageNumber.innerHTML =
                    "<span class='fleuron'>h</span> " +
                    (subscription.pages.length + 1) +
                    " <span class='fleuron'>g</span>";

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
                };

                if (WRITING_A_PAGE) {
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

              if (subscription.pages) {
                let pages = subscription.pages;
                // console.log("🗞️ Pages retrieved:", pages);

                // const MOCKUP_PAGES = false;
                // if (MOCKUP_PAGES) {
                //   pages = [
                //     {
                //       when: new Date().toString(),
                //       words: "Mockup.",
                //       handle: "amelia",
                //       _id: "mockup-id",
                //     },
                //   ];
                // }

                const binding = cel("div");
                binding.id = "binding";
                binding.classList.add("hidden");

                if (pages.length === 0) {
                  const nopages = cel("div");
                  nopages.id = "nopages";
                  nopages.innerText = "Nothing is written";
                  g.appendChild(nopages);
                }

                pages.forEach((page, index) => {
                  const pageWrapper = cel("div");
                  pageWrapper.classList.add("page-wrapper");

                  const pageEl = cel("article");
                  pageEl.classList.add("page");

                  // 🖌️ Grab design template from the page record or use
                  //    the default.
                  pageEl.classList.add("page-style-a");

                  const pageTitle = cel("div");
                  pageTitle.classList.add("page-title");

                  pageTitle.innerText = dateTitle(page.when);

                  const pageNumber = cel("div");
                  pageNumber.classList.add("page-number");
                  pageNumber.innerHTML =
                    "<span class='fleuron'>h</span> " +
                    (index + 1) +
                    " <span class='fleuron'>g</span>";

                  const ear = cel("div");
                  ear.classList.add("ear");

                  // 📐 Ear / Touch
                  const leave = () => {
                    ear.classList.remove("hover");
                    ear.classList.remove("active");
                    // alert("leave");
                  };

                  ear.addEventListener("pointerenter", () => {
                    if (!ear.classList.contains("hover")) {
                      ear.classList.add("hover");
                      ear.addEventListener("pointerleave", leave, {
                        once: true,
                      });
                    }
                  });

                  ear.addEventListener("pointerdown", (e) => {
                    e.preventDefault();
                    ear.classList.remove("hover");
                    ear.classList.add("active");

                    window.addEventListener(
                      "pointerup",
                      (e) => {
                        ear.removeEventListener("pointerleave", leave);
                        const elementUnderPointer = document.elementFromPoint(
                          e.clientX,
                          e.clientY,
                        );
                        if (elementUnderPointer !== ear) leave();
                      },
                      { once: true },
                    );
                  });

                  ear.addEventListener("pointermove", () => {
                    if (
                      !ear.classList.contains("active") &&
                      !ear.classList.contains("hover")
                    ) {
                      ear.classList.add("hover");
                      ear.addEventListener("pointerleave", leave, {
                        once: true,
                      });
                    }
                  });

                  ear.onclick = async (e) => {
                    if (ear.classList.contains("reverse")) {
                      pageWrapper.querySelector(".backpage")?.remove();
                      ear.classList.remove("reverse");
                      pageEl.classList.remove("reverse");
                      pageWrapper.classList.remove("reverse");
                      setTimeout(function () {
                        ear.classList.remove("active");
                      }, 150);
                      return;
                    }

                    const author = page.handle ? "@" + page.handle : "Unknown";

                    // Parse the timestamp from page.when
                    const date = new Date(page.when);

                    // Format the date
                    const dateOptions = {
                      weekday: "long",
                      year: "numeric",
                      month: "long",
                      day: "numeric",
                    };

                    const formattedDate = date.toLocaleDateString(
                      "en-US",
                      dateOptions,
                    );

                    // Format the time
                    const timeOptions = {
                      hour: "numeric",
                      minute: "numeric",
                      hour12: true,
                    };
                    const formattedTime = date.toLocaleTimeString(
                      "en-US",
                      timeOptions,
                    );

                    // Generate a back section with stats, controls, and exports.

                    const backpage = cel("div");
                    backpage.classList.add("backpage");

                    const byline = cel("div");
                    byline.innerText = "Written by " + author;
                    byline.classList.add("byline");

                    backpage.appendChild(byline);

                    //backpage.innerText =
                    //  "Written by " +
                    //  author +
                    //  "\\n" +
                    //  "From " +
                    //  formattedDate +
                    //  " at " +
                    //  formattedTime;

                    // Touches
                    veil();
                    let touches = [];
                    const res = await userRequest(
                      "POST",
                      "/sotce-net/touch-a-page",
                      { _id: page._id },
                    );
                    if (res.status === 200) {
                      // console.log("💁 Page touched:", res);
                      // console.log("🖐️ Touches for page:", res.body.touches);
                      touches = res.touches;
                    } else {
                      console.error("💁 Page touch:", res);
                    }

                    unveil({ instant: true });

                    let touchedBy;
                    if (touches.length === 0) {
                      touchedBy = "";
                    } else if (touches.length === 1) {
                      touchedBy = touches[0] + " touched this page.";
                    } else if (touches.length === 2) {
                      touchedBy =
                        touches[0] +
                        " and " +
                        touches[1] +
                        " touched this page.";
                    } else if (touches.length > 2) {
                      const lastTouch = touches.pop();
                      touchedBy =
                        touches.join(", ") +
                        ", and " +
                        lastTouch +
                        " touched this page.";
                    }

                    const touchesEl = cel("p");
                    touchesEl.innerHTML = touchedBy;

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
                        const res = await userRequest(
                          "POST",
                          "/sotce-net/write-a-page",
                          { draft: "crumple", _id: page._id },
                        );
                        if (res.status === 200) {
                          console.log("🪧 Page crumpled:", res);
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

                    const share = cel("a");
                    share.innerText = "share this page";
                    share.classList.add("share-this-page");
                    share.href = "";

                    share.onclick = async (e) => {
                      e.preventDefault();
                      alert("😃 Coming soon.");
                    };

                    const print = cel("a");
                    print.innerText = "print this page";
                    print.classList.add("print-this-page");
                    print.href = "";

                    print.onclick = async (e) => {
                      e.preventDefault();
                      // Grab presentational html content from page and insert it
                      // into '#print-page'.
                      const printPageWrapper = cel("div");
                      printPageWrapper.id = "print-page-wrapper";

                      const printPage = cel("div");
                      printPage.id = "print-page";

                      const pageWrapper = cel("div");
                      pageWrapper.classList.add("page-wrapper");

                      const article = cel("article");
                      article.classList.add("page", "page-style-a");

                      const title = cel("div");
                      title.classList.add("page-title");
                      title.innerHTML = pageTitle.innerHTML;

                      const content = cel("p");
                      content.classList.add(...wordsEl.classList);
                      content.innerHTML = wordsEl.innerHTML;
                      content.style = wordsEl.style;

                      const num = cel("div");
                      num.classList.add("page-number");
                      num.innerHTML = pageNumber.innerHTML;

                      article.appendChild(title);
                      article.appendChild(content);
                      article.appendChild(num);
                      printPage.appendChild(article);
                      printPageWrapper.appendChild(printPage);
                      wrapper.appendChild(printPageWrapper);

                      document.documentElement.classList.add("printing");

                      // const scale = 812 / article.clientWidth;
                      const scale = 800 / article.clientWidth;
                      //            ^ Just shy of the 8.5in. CSS pixel value.
                      article.style.transform = "scale(" + scale + ")";
                      // Attach the events
                      //window.addEventListener('beforeprint', () => {
                      //  // console.log("Before print.");
                      //  // wrapper.removeChild(printPage);
                      //}, { once: true });

                      function closePrint() {
                        wrapper.removeChild(printPageWrapper);
                        document.documentElement.classList.remove("printing");
                      }

                      // printPageWrapper.onclick = closePrint;

                      window.addEventListener("afterprint", closePrint, {
                        once: true,
                      });

                      window.print();
                      // setTimeout(() => {
                      // }, 500);
                    };

                    /* if (!iOS) */ backpage.appendChild(print);
                    // backpage.appendChild(share);

                    ear.classList.add("reverse");
                    pageEl.classList.add("reverse");
                    pageWrapper.classList.add("reverse");

                    // ear.classList.remove("hover");
                    ear.classList.remove("active");

                    backpage.appendChild(touchesEl);

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

                  binding.appendChild(pageWrapper);
                });

                // console.log("📚 Rendered pages...");

                g.appendChild(binding);

                computePageLayout = function (e) {
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

                  const allPages = document.querySelectorAll(
                    "#garden article.page",
                  );

                  let scale;
                  allPages.forEach((page) => {
                    if (!scale) {
                      const baseWidth = 100 * 8;
                      const goalWidth = page.parentElement.clientWidth;
                      scale = goalWidth / baseWidth;
                    }
                    page.style.transform = "scale(" + scale + ")";

                    // Check to see if the last line of the page needs
                    // justification or not.
                    const words = page.querySelector(".words");
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

                  const ears = document.querySelectorAll(
                    "#garden .page-wrapper div.ear",
                  );

                  ears.forEach((ear) => {
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

                window.addEventListener("resize", function resizeEvent(e) {
                  if (!document.body.contains(binding)) {
                    window.removeEventListener(resizeEvent);
                  } else {
                    computePageLayout(e);
                  }
                });
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
                scrollMemory = wrapper.scrollTop;
                gateCurtain.classList.remove("hidden");
                g.classList.add("hidden");
                document.body.classList.add("pages-hidden");
                document.documentElement.classList.remove("garden");
                curtainCookie.classList.add("interactive");
                updatePath("/gate");
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
                  const observer = new MutationObserver(
                    (mutationsList, observer) => {
                      for (let mutation of mutationsList) {
                        if (
                          mutation.type === "childList" &&
                          mutation.addedNodes.length > 0
                        ) {
                          const checkWidthSettled = (previousWidth) => {
                            const currentWidth = parseInt(
                              window.getComputedStyle(wrapper).width,
                            );

                            if (
                              currentWidth !== previousWidth ||
                              g.scrollHeight > 0 ||
                              showGate
                            ) {
                              //console.log(
                              //  "🟢 Computing page layout...",
                              //  performance.now(),
                              //);
                              computePageLayout?.();
                              // console.log("🟩 Done", performance.now());
                              // TODO:    ^ This takes awhile and the spinner could hold until the initial
                              //            computation is done. 24.10.16.07.06

                              wrapper.scrollTop =
                                wrapper.scrollHeight - wrapper.clientHeight;
                              g.classList.remove("faded");
                              //g.addEventListener(
                              //  "transitionend",
                              //  () => {
                              resolve(g);

                              // },
                              // { once: true },
                              //);
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

                  g.classList.add("faded");
                  wrapper.appendChild(g);
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
              authorizationParams: { redirect_uri: window.location.href },
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
                    logoutParams: { returnTo: window.location.href },
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
            }

            if (!fullAlert) {
              if (!isAuthenticated) {
                // console.log("⚠️ Not authenticated...");
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
                  await spinnerPass(async () => await gate("unverified", user));
                } else {
                  // The user's email is verified...

                  let entered = await subscribed();
                  let times = 0;

                  while (
                    waitForSubscriptionSuccessThreeTimes &&
                    !entered?.subscribed &&
                    times < 3
                  ) {
                    entered = await subscribed();
                    times += 1;
                  }

                  if (entered?.subscribed) {
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
                console.error("💳", error.message);
              }
            }

            // Check the subscription status of the logged in user.
            async function subscribed() {
              if (!user) return false;
              const response = await userRequest(
                "POST",
                "/sotce-net/subscribed",
                { retrieve: "everything" },
              );

              if (response.status === 200) {
                if (response.subscribed) {
                  // console.log("️📰 Subscribed:", response);
                  return response;
                } else {
                  console.error("🗞️ Unsubscribed:", response);
                  return false;
                }
              } else {
                console.error("🗞️ Unsubscribed:", response);
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
                    await garden(entered, user, true); // Open garden and show the gate.
                    unveil({ instant: true });
                    setTimeout(() => alert(result.message), 100);
                  } else {
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
                    logoutParams: { returnTo: window.location.href },
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
    if (!user) return respond(401, { message: "Unauthorized." });

    const subscription = await subscribed(user);

    if (subscription === null) {
      return respond(500, { error: "Failed to fetch subscription status" });
    }

    if (subscription?.subscribed === false) {
      return respond(200, subscription);
    }

    if (subscription && subscription?.status === "active") {
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
          out.renews = `${new Date(subscription.current_period_end * 1000).toLocaleDateString("en-US", dateOptions)}`;
        }

        // 👸 Administrator status.
        const isAdmin = await hasAdmin(user, "sotce");
        if (isAdmin) out.admin = isAdmin;
        shell.log("🔴 Admin:", isAdmin);

        // 📓 Recent Pages
        const database = await connect();
        const pages = database.db.collection("sotce-pages");
        const retrievedPages = await pages
          .aggregate([
            { $match: { state: "published" } }, // Ensure pages are published
            { $sort: { when: 1 } }, // Sort by the 'when' field
            { $limit: 100 }, // Limit to 100 results
          ])
          .toArray();

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

        out.pages = retrievedPages; //isAdmin ? retrievedPages : [];
        await database.disconnect();

        // TODO: 👤 'Handled' pages filtered by user..
        shell.log("🫐 Retrieved:", performance.now());
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

    // TODO: 2. Delete any user data, like posts.

    const database = await connect();

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
            Sotce Net keeps pages on a remote server so they can be shared
            with and viewed by subscribers.
          </p>
          <p>
            Sotce Net allows you to associate an email with a
            <code>@handle</code> to represent your identity.
          </p>
          <p>
            Sotce Net does not sell or exchange any user data with third
            parties.
          </p>
          <p>
            Sotce Net is brought to you by the partnership of <code><a href="https://sotce.com">Sotce</a></code> and
            <code><a href="https://aesthetic.computer/privacy-policy">Aesthetic Computer</code>.</a>
          </p>
          ${subscribers > 0 ? "<p>Sotce Net has <code>" + subscribers + "</code> active subscriber" + (subscribers > 1 ? "s" : "") + ".</p>" : ""}
          <p>
            For more information write to <code>mail@sotce.net</code> to
            communicate with the author.
          </p>
          <a href="${dev ? "/sotce-net" : "/"}"><img width="128" src="${assetPath + "cookie.png"}" /></a>
          <br />
          <br />
          <sub>Edited on September 25, 2024</sub>
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
