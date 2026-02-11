# Notification System Analysis & Recommendations - 2026

**Date**: February 11, 2026
**Author**: Claude
**Purpose**: Analyze current push notification infrastructure and explore faster, self-hosted alternatives with separate sotce-net notifications

---

## Executive Summary

Aesthetic Computer currently uses **Firebase Cloud Messaging (FCM)** for push notifications with a hybrid web/native iOS architecture. While iOS now supports web push notifications (as of iOS 16.4), **WKWebView does not support Web Push APIs**, meaning your native iOS app wrapper must continue using APNs for notifications. However, you can modernize your infrastructure by self-hosting the notification server and adding separate notification channels for sotce-net subscribers.

**Key Findings**:
- ✅ iOS Safari supports web push for PWAs (home screen apps) since iOS 16.4
- ❌ WKWebView (your native app's webview) does NOT support Web Push APIs
- ✅ You can self-host push notifications using open-source alternatives
- ✅ Separate sotce-net notifications are achievable via topics or dedicated channels
- ⚠️ Your current Firebase setup is functional but not fully utilized (background handlers commented out)

---

## 1. Current Notification Infrastructure

### 1.1 Web Implementation

**Location**: [system/public/aesthetic.computer/boot.mjs](system/public/aesthetic.computer/boot.mjs) (lines 1796-1891)

**Technology Stack**:
- Firebase Cloud Messaging SDK v10.8.0
- VAPID Key: `BDEh3JDD1xZo7eQU00TgsYb_o8ENJlpU-ovbZzWoCOu4AOeFJD8PVbZ3pif_7rMEk65Uj00-lwdXgc3qJVLp4ys`
- Firebase Config loaded at runtime via `/api/firebase-config` endpoint
- GCM Sender ID: `839964586768`

**Features**:
- Initializes Firebase Messaging with `getMessaging()`
- Requests FCM token via `getToken()`
- Subscribes to two topics: **"scream"** and **"mood"**
- Foreground message handler via `onMessage()`
- User commands: `notifs` (enable) and `nonotifs` (disable)

**Service Workers**:
1. **Main Service Worker** ([system/public/sw.js](system/public/sw.js))
   - Module caching with stale-while-revalidate strategy
   - Does NOT handle push notifications

2. **Firebase Messaging Service Worker** ([system/public/firebase-messaging-sw.js](system/public/firebase-messaging-sw.js))
   - Firebase Messaging 8.10.1
   - **Background message handling is commented out/disabled**
   - Configuration via `self.AC_FIREBASE_CONFIG`

### 1.2 iOS Native App Implementation

**Location**: [apple/aesthetic.computer/](apple/aesthetic.computer/)

**Architecture**: Swift + SwiftUI WebView wrapper using WKWebView

**Key Files**:
- `aesthetic_computerApp.swift` - App entry point with Firebase setup
- `ContentView.swift` - WKWebView container with JS bridge
- `GoogleService-Info.plist` - Firebase configuration

**Notification Flow**:
1. Imports: `UserNotifications`, `FirebaseCore`, `FirebaseMessaging`
2. Registers with APNs: `application.registerForRemoteNotifications()`
3. Requests permissions: `.alert`, `.badge`, `.sound`
4. Subscribes to topics: **"scream"** and **"mood"**
5. Handles foreground notifications: `willPresent` (shows banner/sound)
6. Handles background/tapped notifications: `didReceive`
7. Bridges to JavaScript: `iOSAppSwitchPiece()` function

**Bundle IDs**:
- Test: `work.false.SpiderLily`
- Production: `aesthetic.computer`

**Distribution**: https://builds.false.work/ios/

### 1.3 Backend Notification Sending

**Location**: [system/netlify/functions/mood.mjs](system/netlify/functions/mood.mjs) (lines 315-334)

**Example**: Mood notification broadcast
```javascript
Firebase.messaging.send({
  notification: {
    title: `${handle}'s mood is`,
    body: `${mood}`
  },
  apns: {
    payload: {
      aps: { "mutable-content": 1 }
    }
  },
  webpush: {
    headers: { image: "..." }
  },
  topic: "mood"
})
```

**Topic Subscription**: [system/netlify/functions/subscribe-to-topic.js](system/netlify/functions/subscribe-to-topic.js)
- Uses Firebase Admin Messaging API
- Accepts FCM token and topic name
- Subscribes device for broadcast notifications

### 1.4 Sotce-Net Subscription System

**Location**: [system/netlify/functions/sotce-net.mjs](system/netlify/functions/sotce-net.mjs) (~10,986 lines)

**Purpose**: Paid diary/journal network with Stripe subscriptions

**Key Components**:
- **Subscription checking**: Redis cache + Stripe API fallback
- **Admin bypass**: `me@jas.life`, `sotce.net@gmail.com`
- **Stripe integration**: Checkout sessions, cancellations, customer metadata
- **Features**: Chat access, page editing, Q&A system

**Subscription Endpoints**:
- `POST /sotce-net/subscribe` - Checkout session
- `POST /sotce-net/subscribed` - Status check
- `POST /sotce-net/cancel` - Cancel subscription
- `GET /sotce-net/subscribers` - Subscriber count

**TODOs Found in Code**:
- ❌ Email notifications for new pages (not implemented)
- ❌ Push notifications for chat messages
- ❌ Notification preferences UI

---

## 2. iOS Web Push Capabilities in 2026

### 2.1 PWA Support (Home Screen Apps)

**Status**: ✅ Fully supported since iOS 16.4 (March 2023)

**Requirements**:
1. iOS/iPadOS 16.4 or later
2. Website must be added to Home Screen as PWA
3. User opens PWA from Home Screen (not Safari)
4. User grants permission after interaction

**Capabilities**:
- Full Web Push API support
- Service Worker push events
- Background notifications
- Badge updates
- Notification actions

**New in iOS 18.4**: Declarative Web Push (doesn't require Service Worker)

### 2.2 WKWebView Support

**Status**: ❌ **NOT SUPPORTED**

**Critical Finding**: WKWebView (used in your native app) **does not support Service Workers or Web Push APIs**. Apple restricts this to encourage native APNs usage.

**Implication**: Your native iOS app wrapper **MUST continue using APNs** for notifications. You cannot rely solely on web push APIs in the WKWebView.

**Source**: [Apple Developer Documentation](https://developer.apple.com/documentation/usernotifications/sending-web-push-notifications-in-web-apps-and-browsers), [WebKit Blog](https://webkit.org/blog/13878/web-push-for-web-apps-on-ios-and-ipados/)

### 2.3 Your Options

**Option A**: Keep native iOS app wrapper (current approach)
- ✅ Continues to work with APNs
- ✅ No code changes needed
- ✅ Reliable notification delivery
- ❌ Maintains dependency on Firebase/APNs
- ❌ App Store review process for updates

**Option B**: Switch to PWA-only
- ✅ No App Store dependency
- ✅ Direct updates without review
- ✅ Pure web push (self-hostable)
- ❌ Users must manually add to Home Screen
- ❌ Discovery/installation friction
- ❌ Lose native app benefits (icon, splash screen automation)

**Option C**: Hybrid approach
- ✅ Offer both native app AND PWA
- ✅ Users choose their preference
- ✅ Maximize reach
- ❌ Maintain two notification paths
- ❌ Higher complexity

---

## 3. Self-Hosted Notification Server Options

### 3.1 Why Self-Host?

**Benefits**:
- ✅ Full control over infrastructure
- ✅ No vendor lock-in
- ✅ Reduced latency (server closer to you)
- ✅ Privacy (data stays on your servers)
- ✅ Cost savings at scale
- ✅ Custom features and integrations

**Trade-offs**:
- ❌ Increased operational complexity
- ❌ Maintenance burden (updates, security)
- ❌ Still need APNs for iOS (Apple requirement)
- ❌ Still need FCM or WebPush for Android

### 3.2 Self-Hosted Solutions

#### **Gotify** (Recommended for simplicity)
- **Language**: Go
- **Type**: Dedicated push notification server
- **Platforms**: Server + CLI + Android + iOS clients
- **Features**: Rich plugin architecture, Docker support, REST API
- **Use Case**: Internal notifications, admin alerts
- **Limitation**: Does NOT replace APNs/FCM; serves custom apps only

#### **Parse Server** (Recommended for full backend)
- **Type**: Complete Firebase alternative (BaaS)
- **Features**: Push notifications, auth, database, custom backend logic
- **Push Support**: APNs, FCM, web push via service workers
- **Deployment**: Self-hosted or managed (Back4App)
- **Migration**: Can import Firebase data
- **Use Case**: Replace entire Firebase dependency

#### **AirNotifier**
- **Type**: Open-source push notification server
- **Supports**: APNs HTTP/2, FCM, Windows 10 WNS
- **Features**: API access control, unlimited devices, web dashboard, logging
- **Use Case**: Multi-platform push notification hub

#### **Uniqush Push**
- **Type**: Free/open-source push server
- **Supports**: GCM, FCM, APNs, ADM (Amazon)
- **Use Case**: Lightweight notification relay

#### **Appwrite**
- **Type**: Backend-as-a-Service (Firebase alternative)
- **Features**: Push notifications, auth, database, storage, functions
- **Deployment**: Self-hosted (Docker)
- **Use Case**: Modern Firebase replacement

#### **Web Push Protocol (DIY)**
You can also build your own using the Web Push Protocol standard:
- **Standard**: RFC 8030 (Web Push Protocol)
- **Libraries**: `web-push` (Node.js), `pywebpush` (Python)
- **VAPID**: Voluntary Application Server Identification
- **Use Case**: Maximum control, minimal dependencies

**Example** (Node.js):
```javascript
const webpush = require('web-push');

webpush.setVapidDetails(
  'mailto:you@example.com',
  publicKey,
  privateKey
);

webpush.sendNotification(subscription, payload);
```

**Note**: You still need APNs certificates for iOS and FCM for Android, but you control the server logic.

**Sources**:
- [Open-Source Firebase Alternatives](https://medium.com/@nocobase/6-open-source-firebase-alternatives-for-self-hosting-and-data-control-ba4607323258)
- [15 Open-Source Push Notification Projects](https://medevel.com/15-os-push-notification/)
- [Best Firebase FCM Alternatives](https://www.courier.com/integrations/alternatives/firebase-fcm)
- [Firebase Self-Hosted Alternatives](https://blog.back4app.com/firebase-alternative-self-hosted/)

---

## 4. Separate Notifications for Sotce-Net Subscribers

### 4.1 Requirements

- Sotce-net subscribers receive exclusive notifications (e.g., new pages, chat messages, updates)
- Must NOT overlap with general AC notifications ("scream", "mood")
- Should respect subscription status
- Fast delivery

### 4.2 Implementation Approaches

#### **Approach A: Dedicated Topic** (Easiest, works with current setup)

**How It Works**:
1. Create new FCM topic: `"sotce-net"`
2. Subscribe users when they become sotce-net subscribers
3. Send notifications to `topic: "sotce-net"` only

**Implementation**:
```javascript
// In sotce-net.mjs after successful subscription
await Firebase.messaging.subscribeToTopic(fcmToken, 'sotce-net');

// When sending sotce-net notification
await Firebase.messaging.send({
  notification: {
    title: "New Sotce-Net Page",
    body: "Someone posted a new diary entry"
  },
  topic: "sotce-net"
});
```

**Benefits**:
- ✅ Minimal code changes
- ✅ Works with existing Firebase setup
- ✅ Fast delivery
- ✅ Easy to test

**Limitations**:
- ❌ Still depends on Firebase
- ❌ All sotce-net subscribers see same notifications
- ❌ No per-user customization

#### **Approach B: Targeted User Tokens** (More flexible)

**How It Works**:
1. Store FCM tokens in database when users subscribe
2. Query sotce-net subscribers from database
3. Send to specific tokens (multicast)

**Implementation**:
```javascript
// Store token during subscription
await db.collection('users').doc(userId).update({
  fcmToken: token,
  isSotceNetSubscriber: true
});

// Send to all sotce-net subscribers
const subscribers = await db.collection('users')
  .where('isSotceNetSubscriber', '==', true)
  .get();

const tokens = subscribers.docs.map(doc => doc.data().fcmToken);

await Firebase.messaging.sendEachForMulticast({
  tokens: tokens,
  notification: {
    title: "New Sotce-Net Page",
    body: "Check out the latest post"
  }
});
```

**Benefits**:
- ✅ Per-user targeting
- ✅ Can personalize notifications
- ✅ Can exclude specific users
- ✅ Better analytics (who received what)

**Limitations**:
- ❌ Requires database (Redis/Postgres)
- ❌ More complex token management
- ❌ Token refresh handling needed

#### **Approach C: Separate Self-Hosted Channel** (Best for independence)

**How It Works**:
1. Set up self-hosted notification server (e.g., Parse Server)
2. Register sotce-net subscribers with separate service
3. Send notifications via self-hosted server

**Implementation**:
```javascript
// Using Parse Server push
await Parse.Push.send({
  channels: ['sotce-net'],
  data: {
    alert: "New Sotce-Net Page",
    badge: 1
  }
});
```

**Benefits**:
- ✅ Complete independence from Firebase
- ✅ Full control over delivery logic
- ✅ Can add custom features (quiet hours, frequency limits)
- ✅ Privacy (data on your servers)

**Limitations**:
- ❌ Requires new infrastructure
- ❌ Migration effort from Firebase
- ❌ Still needs APNs certificates for iOS
- ❌ Operational overhead

#### **Approach D: Webhook + External Service** (Fastest to implement)

**How It Works**:
1. Use existing notification service (e.g., OneSignal, Pusher)
2. Create webhook in sotce-net.mjs
3. Trigger notifications via API

**Implementation**:
```javascript
// After new sotce-net page created
await fetch('https://onesignal.com/api/v1/notifications', {
  method: 'POST',
  headers: { 'Authorization': `Basic ${apiKey}` },
  body: JSON.stringify({
    app_id: appId,
    filters: [{ field: 'tag', key: 'sotce-net', relation: '=', value: 'true' }],
    contents: { en: "New Sotce-Net Page" }
  })
});
```

**Benefits**:
- ✅ Very fast to implement
- ✅ No infrastructure setup
- ✅ Advanced features (segments, A/B testing, analytics)

**Limitations**:
- ❌ External dependency
- ❌ Cost at scale
- ❌ Another vendor to manage

### 4.3 Recommended Approach

**For immediate implementation**: **Approach A (Dedicated Topic)**
- Fast to build (< 1 hour)
- Works with existing setup
- No new infrastructure

**For long-term**: **Approach C (Self-Hosted)** OR **Approach B (Targeted Tokens)**
- More control
- Better privacy
- Scalable

**Hybrid**: Start with A, migrate to C gradually
- Topic-based now
- Build self-hosted infrastructure in parallel
- Switch when ready

---

## 5. Gaps in Current Implementation

### 5.1 Critical Issues

1. **Background message handling disabled**
   - **File**: [system/public/firebase-messaging-sw.js](system/public/firebase-messaging-sw.js)
   - **Issue**: Background push handler is commented out
   - **Impact**: Notifications only work when app is in foreground
   - **Fix**: Uncomment and implement `messaging.onBackgroundMessage()`

2. **No sotce-net notifications**
   - **File**: [system/netlify/functions/sotce-net.mjs](system/netlify/functions/sotce-net.mjs)
   - **Issue**: TODO comments mention email, but no push notifications
   - **Impact**: Subscribers miss important updates
   - **Fix**: Implement Approach A or B above

3. **No notification preferences**
   - **Issue**: Users can only enable/disable ALL notifications
   - **Impact**: No granular control (e.g., "mood" only, "sotce-net" only)
   - **Fix**: Build preferences UI with per-topic toggles

### 5.2 Missing Features

- ❌ Unsubscribe from specific topics
- ❌ Custom notification sounds (iOS)
- ❌ Notification history/inbox
- ❌ Rich notifications (images, actions, buttons)
- ❌ Notification scheduling
- ❌ Analytics (open rate, click rate)

---

## 6. Recommendations

### 6.1 Short-Term (This Week)

1. **Enable background notifications**
   - Uncomment background message handler in [firebase-messaging-sw.js](system/public/firebase-messaging-sw.js)
   - Test on web and iOS
   - **Effort**: 1-2 hours

2. **Add sotce-net topic**
   - Create `"sotce-net"` topic subscription in [sotce-net.mjs](system/netlify/functions/sotce-net.mjs)
   - Send test notification
   - **Effort**: 2-3 hours

3. **Fix iOS notification toggle**
   - Ensure `notifs`/`nonotifs` commands work reliably
   - Test on physical device
   - **Effort**: 1 hour

### 6.2 Medium-Term (This Month)

1. **Add notification preferences**
   - Build UI for per-topic toggles
   - Store preferences in Redis or database
   - Respect preferences in backend
   - **Effort**: 1-2 days

2. **Implement targeted sotce-net notifications**
   - Use Approach B (targeted tokens)
   - Notify subscribers of new pages, chat messages
   - **Effort**: 2-3 days

3. **Add rich notifications**
   - Images in notifications (webpush)
   - Action buttons ("View", "Dismiss")
   - **Effort**: 1 day

### 6.3 Long-Term (This Quarter)

1. **Migrate to self-hosted solution**
   - Evaluate Parse Server vs. Appwrite
   - Set up self-hosted instance (Docker on existing infra)
   - Migrate FCM tokens
   - Switch notification sending to self-hosted
   - **Effort**: 1-2 weeks

2. **Build notification inbox**
   - Store notification history
   - Allow users to review past notifications
   - Mark as read/unread
   - **Effort**: 1 week

3. **Add analytics**
   - Track notification sends, deliveries, opens
   - A/B test notification copy
   - **Effort**: 3-5 days

### 6.4 Decision: Native App vs PWA

**Recommendation**: **Keep native iOS app** (for now)

**Rationale**:
- WKWebView doesn't support Web Push APIs
- Native app provides better UX (auto-install, app icon, no friction)
- APNs integration is already working
- You can always offer PWA as alternative later

**Future**: Offer both native app AND PWA
- Native app for primary users
- PWA for those who prefer it (no App Store)
- Same notification backend serves both

---

## 7. Architecture Proposal: Self-Hosted Notifications

### 7.1 Recommended Stack

**Backend**: Parse Server (self-hosted on your existing Netlify/VPS)
**Web Push**: `web-push` library (Node.js)
**iOS**: APNs HTTP/2 (via Parse or `apn` library)
**Database**: PostgreSQL (for Parse) or continue using Redis
**Queue**: Bull (Redis-based job queue for reliable delivery)

### 7.2 Architecture Diagram

```
┌─────────────────────┐
│   AC Web App        │
│   (Firebase SDK)    │───┐
└─────────────────────┘   │
                          │ FCM Token
┌─────────────────────┐   │ Registration
│   AC iOS App        │───┤
│   (APNs + FCM)      │   │
└─────────────────────┘   │
                          ▼
                  ┌───────────────────┐
                  │ Your Netlify      │
                  │ Functions         │
                  │                   │
                  │ - sotce-net.mjs   │
                  │ - mood.mjs        │
                  │ - subscribe.js    │
                  └─────────┬─────────┘
                            │
                    ┌───────▼────────┐
                    │ Notification   │
                    │ Router         │
                    │ (New Service)  │
                    └───────┬────────┘
                            │
              ┌─────────────┼─────────────┐
              │             │             │
              ▼             ▼             ▼
      ┌─────────────┐ ┌─────────┐ ┌──────────┐
      │ Parse Push  │ │ APNs    │ │ Web Push │
      │ (Optional)  │ │ HTTP/2  │ │ Protocol │
      └─────────────┘ └─────────┘ └──────────┘
              │             │             │
              └─────────────┼─────────────┘
                            │
                   ┌────────▼────────┐
                   │   Users         │
                   │   (iOS + Web)   │
                   └─────────────────┘
```

### 7.3 Migration Path

**Phase 1**: Parallel infrastructure
- Set up Parse Server alongside Firebase
- Duplicate token registration to both services
- Compare delivery metrics
- **Duration**: 2 weeks

**Phase 2**: Gradual migration
- Route 10% of notifications through Parse
- Monitor errors and latency
- Increase to 50%, then 100%
- **Duration**: 2 weeks

**Phase 3**: Decommission Firebase
- Remove Firebase SDK from web app
- Delete Firebase service workers
- Archive Firebase project
- **Duration**: 1 week

**Total migration time**: ~5 weeks

---

## 8. Cost Analysis

### 8.1 Current Costs (Firebase)

- **Firebase Cloud Messaging**: FREE (unlimited)
- **APNs**: FREE (Apple provides)
- **Hidden costs**:
  - Vendor lock-in
  - Data sent to Google
  - Limited customization

### 8.2 Self-Hosted Costs

**Infrastructure**:
- VPS (2 CPU, 4GB RAM): ~$20-40/month (Hetzner, DigitalOcean)
- PostgreSQL: Included in VPS or ~$15/month (managed)
- Redis: Already have (reuse existing)

**Development**:
- Initial setup: ~40-80 hours @ your rate
- Ongoing maintenance: ~4-8 hours/month

**Operational**:
- APNs certificates: FREE (renewable annually)
- Monitoring (optional): ~$10-20/month (Sentry, Datadog)

**Total**: ~$30-60/month + initial dev time

**Break-even**: Immediate (if you value independence and control)

---

## 9. Next Steps

### Immediate Actions (Today)

1. ✅ Read this report
2. ⬜ Decide on native app vs PWA strategy
3. ⬜ Choose short-term approach for sotce-net notifications (A, B, C, or D)
4. ⬜ Enable background message handling (2-hour task)

### This Week

1. ⬜ Implement sotce-net topic subscription
2. ⬜ Send first test notification to sotce-net subscribers
3. ⬜ Fix any iOS notification bugs

### This Month

1. ⬜ Build notification preferences UI
2. ⬜ Add targeted sotce-net notifications (new pages, chat)
3. ⬜ Evaluate Parse Server vs Appwrite for self-hosting

### This Quarter

1. ⬜ Set up self-hosted notification server (if decided)
2. ⬜ Migrate FCM tokens
3. ⬜ Build notification inbox
4. ⬜ Add analytics

---

## 10. Questions to Resolve

1. **Do you want to keep the native iOS app**, or switch to PWA-only?
   - Native app = better UX, App Store presence
   - PWA = no App Store, easier updates, pure web

2. **How fast do you need sotce-net notifications**?
   - This week = use Topic approach (A)
   - This month = use Targeted approach (B)
   - Long-term = self-hosted (C)

3. **What notifications should sotce-net subscribers receive**?
   - New pages from other subscribers?
   - Chat messages?
   - Subscription reminders?
   - Admin announcements?

4. **Self-hosting priority**?
   - High = start Parse Server setup now
   - Medium = evaluate for Q2 2026
   - Low = stick with Firebase for now

5. **Budget for infrastructure**?
   - $30-60/month for self-hosted OK?
   - Or prefer $0 (stick with Firebase free tier)?

---

## References & Sources

### iOS Web Push Support
- [PWA on iOS - Current Status & Limitations](https://brainhub.eu/library/pwa-on-ios)
- [Apple Developer: Sending web push notifications](https://developer.apple.com/documentation/usernotifications/sending-web-push-notifications-in-web-apps-and-browsers)
- [WebKit: Web Push for Web Apps on iOS](https://webkit.org/blog/13878/web-push-for-web-apps-on-ios-and-ipados/)
- [Do PWAs Work on iPhone?](https://www.mobiloud.com/blog/progressive-web-apps-ios)

### Self-Hosted Notification Servers
- [6 Open-Source Firebase Alternatives](https://medium.com/@nocobase/6-open-source-firebase-alternatives-for-self-hosting-and-data-control-ba4607323258)
- [15 Open-Source Push Notification Projects](https://medevel.com/15-os-push-notification/)
- [Best Firebase FCM Alternatives](https://www.courier.com/integrations/alternatives/firebase-fcm)
- [Firebase Self-Hosted Alternatives](https://blog.back4app.com/firebase-alternative-self-hosted/)

### WKWebView Limitations
- [Setup Web Push Notifications on iOS](https://pushalert.co/documentation/ios-web-push)
- [How To Enable Native Push Notifications On iOS App](https://superpwa.com/docs/article/how-to-enable-native-push-notifications-on-ios-app-generated-using-1-click-app-generator/)

---

**End of Report**
