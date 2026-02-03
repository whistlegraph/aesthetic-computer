# Sotce Net "Ask" Feature - Technical Plan

## Overview
Add an "ask" button next to "chat" that allows visitors to submit questions. Questions create a new content type that can be answered via "pages" with a special design to show Q&A relationships. Two display modes: separate feeds or intermixed.

---

## Current Architecture Summary

### File Structure
- **Main file**: [system/netlify/functions/sotce-net.mjs](../system/netlify/functions/sotce-net.mjs) (~5500 lines)
  - Single Netlify function with internal router
  - Contains all HTML/CSS/JS inline as template literals
  - Handles authentication, Stripe subscriptions, pages, chat, touches

- **Constants**: [system/backend/sotce-net-constants.mjs](../system/backend/sotce-net-constants.mjs)
  - Stripe keys, price/product IDs

### Existing Data Models (MongoDB Collections)
| Collection | Purpose | Key Fields |
|------------|---------|------------|
| `sotce-pages` | Diary entries | `_id`, `user`, `words`, `when`, `state` (draft/published/crumpled) |
| `sotce-touches` | Page interactions | `user`, `page`, `when` |
| `chat-sotce` | Chat messages | `user`, `text`, `when` |
| `@handles` | User handles | `_id` (sub), `handle` |

### Current UI Components
| Component | Location | Access Level |
|-----------|----------|--------------|
| Gate (login/signup) | splash screen | everyone |
| Cookie menu | top-right | logged-in |
| Chat button | top bar | subscribers + admins |
| Write a page | top bar | admins only |
| Page feed | binding | subscribers + admins |

### Key Functions (Client-side)
- `gate(status, user, subscription)` — renders login/subscription UI
- `garden(subscription, user, showGate)` — renders the main subscriber view
- `userRequest(method, endpoint, body)` — authenticated API calls
- `veil()` / `unveil()` — loading states

### Key Functions (Server-side)
- `subscribed(user)` — checks Stripe subscription status
- `authorize(headers, tenant)` — validates auth token
- `hasAdmin(user, tenant)` — checks admin privileges
- `handleFor(sub, tenant)` — gets user handle

---

## New "Ask" Feature Design

### 1. New Data Model: `sotce-asks`

```javascript
{
  _id: ObjectId,
  user: String,          // Auth0 sub of asker
  question: String,      // The question text (max ~500 chars)
  when: Date,            // Submission timestamp
  state: String,         // "pending" | "answered" | "archived"
  answeredBy: ObjectId,  // Reference to sotce-pages._id (if answered)
  answeredWhen: Date,    // When the answer was published
  visibility: String     // "private" (default) | "public" (after answered)
}
```

### 2. Extended Page Model: `sotce-pages`

Add optional fields for answer pages:
```javascript
{
  // ...existing fields...
  answerTo: ObjectId,    // Reference to sotce-asks._id (if this is an answer)
  pageType: String       // "diary" (default) | "answer"
}
```

### 3. New API Endpoints

| Endpoint | Method | Auth | Purpose |
|----------|--------|------|---------|
| `/ask` | POST | subscriber | Submit a new question |
| `/asks` | GET | subscriber | List user's own questions |
| `/asks/pending` | GET | admin | List all unanswered questions |
| `/ask/:id` | DELETE | subscriber/admin | Remove own question |
| `/answer` | POST | admin | Create an answer page linked to a question |

### 4. UI Components

#### A. "Ask" Button (next to "Chat")
```
┌────────────────────────────────────────┐
│  [chat]  [ask]              🍪         │
└────────────────────────────────────────┘
```

- Position: `#top-bar`, after `#chat-button`
- Style: Same as chat button
- Visibility: All subscribers (not just admins)

#### B. Ask Form Modal
```
┌─────────────────────────────────────────┐
│           Ask @amelia                   │
│  ┌───────────────────────────────────┐  │
│  │                                   │  │
│  │   [Your question here...]         │  │
│  │                                   │  │
│  └───────────────────────────────────┘  │
│            chars: 0/500                 │
│                                         │
│        [cancel]        [submit]         │
└─────────────────────────────────────────┘
```

#### C. My Questions View (for askers)
```
┌─────────────────────────────────────────┐
│  My Questions                     [x]   │
├─────────────────────────────────────────┤
│  ⏳ "What inspires you most?"           │
│     Asked Dec 15, 2025                  │
│  ─────────────────────────────────────  │
│  ✓ "How do you start your mornings?"   │
│     Asked Nov 20, 2025                  │
│     → Answered: View Page #42           │
└─────────────────────────────────────────┘
```

#### D. Pending Questions View (for admin)
- Shown in write-a-page flow or separate panel
- Can select a question to answer

#### E. Answer Page Design (special styling)
```
┌─────────────────────────────────────────┐
│                                         │
│   ❝ What inspires you most? ❞          │
│   — @username                           │
│                                         │
│   ─────────────────────────────────────  │
│                                         │
│   [Answer content here, styled as       │
│    a regular page but with Q context]   │
│                                         │
│              ❦ 42 ❧                     │
└─────────────────────────────────────────┘
```

### 5. Feed Display Options

#### Option A: Intermixed Feed (default)
- Pages and answered questions appear chronologically
- Answer pages show question context at top

#### Option B: Separate Feeds (toggle)
```
[all pages]  [Q&A only]  [diary only]
```

---

## Implementation Phases

### Phase 1: Backend Foundation
1. Create `sotce-asks` collection with indexes
2. Add `/ask` POST endpoint
3. Add `/asks` GET endpoint (user's questions)
4. Add `/asks/pending` GET endpoint (admin)

### Phase 2: Ask UI
1. Add "ask" button to top bar
2. Create ask form modal (similar to editor styling)
3. Add "my questions" panel accessible from gate

### Phase 3: Answer Flow
1. Modify `/write-a-page` to accept `answerTo` parameter
2. Add question selector in editor for admins
3. Create answer page special styling

### Phase 4: Feed Integration
1. Modify page retrieval to include Q&A metadata
2. Add answer page rendering with question context
3. Optional: Add feed filter toggles

---

## CSS Additions (to existing `<style>` block)

```css
/* Ask Button */
#ask-button {
  margin-left: 0.5em;
}

/* Ask Form */
#ask-form-overlay {
  position: fixed;
  top: 0; left: 0;
  width: 100%; height: 100%;
  background: rgba(0, 0, 0, 0.5);
  z-index: 200;
  display: flex;
}

#ask-form {
  margin: auto;
  background: var(--backpage-color);
  padding: 2em;
  border-radius: 0.5em;
  max-width: 400px;
  width: 90%;
}

#ask-textarea {
  width: 100%;
  min-height: 100px;
  font-family: var(--page-font);
  resize: vertical;
}

/* Answer Page Question Block */
.page .answer-question {
  font-style: italic;
  margin-bottom: 1em;
  padding: 0.5em;
  border-left: 3px solid var(--pink-border);
  background: rgba(255, 209, 220, 0.3);
}

/* My Questions Panel */
#my-questions {
  max-height: 60vh;
  overflow-y: auto;
}

.question-item {
  padding: 0.75em;
  border-bottom: 1px solid var(--pink-border);
}

.question-item.answered {
  background: rgba(203, 238, 161, 0.3);
}

.question-item.pending::before {
  content: "⏳ ";
}

.question-item.answered::before {
  content: "✓ ";
  color: green;
}
```

---

## Server Code Additions

### New Route: `/ask` (POST)
```javascript
} else if (path === "/ask" && method === "post") {
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
  
  const insertion = await asks.insertOne({
    user: user.sub,
    question,
    when: new Date(),
    state: "pending",
    visibility: "private"
  });
  
  await database.disconnect();
  return respond(200, { _id: insertion.insertedId });
}
```

### New Route: `/asks` (GET)
```javascript
} else if (path === "/asks" && method === "get") {
  const user = await authorize(event.headers, "sotce");
  if (!user) return respond(401, { message: "Unauthorized." });
  
  const database = await connect();
  const asks = database.db.collection("sotce-asks");
  
  const userAsks = await asks.find({ user: user.sub })
    .sort({ when: -1 })
    .limit(50)
    .toArray();
  
  await database.disconnect();
  return respond(200, { asks: userAsks });
}
```

### New Route: `/asks/pending` (GET - admin only)
```javascript
} else if (path === "/asks/pending" && method === "get") {
  const user = await authorize(event.headers, "sotce");
  const isAdmin = await hasAdmin(user, "sotce");
  if (!user || !isAdmin) return respond(401, { message: "Unauthorized." });
  
  const database = await connect();
  const asks = database.db.collection("sotce-asks");
  
  const pending = await asks.aggregate([
    { $match: { state: "pending" } },
    { $sort: { when: 1 } },
    { $limit: 100 }
  ]).toArray();
  
  // Add handles to each ask
  for (const ask of pending) {
    ask.handle = await handleFor(ask.user, "sotce");
  }
  
  await database.disconnect();
  return respond(200, { asks: pending });
}
```

---

## Client Code Additions

### Ask Button (in `garden` function, after chat button)
```javascript
const askButton = cel("button");
askButton.id = "ask-button";
askButton.innerText = "ask";

askButton.onclick = function () {
  showAskForm();
};

topBar.appendChild(askButton);
```

### Ask Form Function
```javascript
function showAskForm() {
  const overlay = cel("div");
  overlay.id = "ask-form-overlay";
  
  const form = cel("div");
  form.id = "ask-form";
  form.innerHTML = `
    <h2>Ask @amelia</h2>
    <textarea id="ask-textarea" maxlength="500" placeholder="Your question..."></textarea>
    <div id="ask-char-count">0/500</div>
    <nav>
      <button id="ask-cancel">cancel</button>
      <button id="ask-submit" class="positive">submit</button>
    </nav>
  `;
  
  overlay.appendChild(form);
  wrapper.appendChild(overlay);
  
  const textarea = form.querySelector("#ask-textarea");
  const charCount = form.querySelector("#ask-char-count");
  
  textarea.addEventListener("input", () => {
    charCount.innerText = `${textarea.value.length}/500`;
  });
  
  form.querySelector("#ask-cancel").onclick = () => overlay.remove();
  
  form.querySelector("#ask-submit").onclick = async () => {
    const question = textarea.value.trim();
    if (!question) return alert("Please enter a question.");
    
    veil();
    const res = await userRequest("POST", "/sotce-net/ask", { question });
    unveil({ instant: true });
    
    if (res.status === 200) {
      overlay.remove();
      alert("Question submitted! You'll be notified when answered.");
    } else {
      alert("Error: " + (res.message || "Could not submit question."));
    }
  };
  
  textarea.focus();
}
```

---

## Migration Notes

1. **Backward Compatible**: All changes are additive; existing pages continue working.
2. **Index Creation**: Add compound index on `sotce-asks`: `{ user: 1, state: 1 }`
3. **Handle Association**: Questions inherit the asker's handle at submission time.

---

## Future Enhancements

- Email notifications when questions are answered
- "Featured questions" section on gate
- Question voting/priority by other subscribers
- Anonymous question mode
- Question categories/tags
- Search through Q&A archive

---

## File Changes Summary

| File | Change Type |
|------|-------------|
| [system/netlify/functions/sotce-net.mjs](../system/netlify/functions/sotce-net.mjs) | Add routes, UI components, CSS |

**Estimated LOC additions**: ~400-500 lines
