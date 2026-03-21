// Gives Feed, 26.02.08
// 🎁 Fetches recent successful gives from Stripe
// Includes both one-time checkout sessions and subscription invoice renewals
// Endpoint: GET /api/gives

import Stripe from "stripe";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT !== "production";

// Cache to avoid hammering Stripe API
let cachedGives = null;
let cacheTimestamp = 0;
const CACHE_TTL = 60000; // 1 minute cache

export async function handler(event, context) {
  // Handle CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, {});
  }

  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  // Always use production Stripe key for gives feed (show real donations)
  const stripeKey = process.env.STRIPE_API_PRIV_KEY;

  if (!stripeKey) {
    // Return empty gives in dev when Stripe isn't configured
    if (dev) {
      return respond(200, { gives: [], total: 0, cached: false, dev: true });
    }
    return respond(500, { error: "Stripe not configured" });
  }

  // Return cached data if fresh
  const now = Date.now();
  if (cachedGives && (now - cacheTimestamp) < CACHE_TTL) {
    return respond(200, cachedGives);
  }

  const stripe = Stripe(stripeKey);

  try {
    const limit = parseInt(event.queryStringParameters?.limit) || 100;
    
    // Only show gives from 2025 onwards (when give page launched)
    const minDate = new Date('2025-01-01').getTime() / 1000; // Unix timestamp
    
    // 1. Fetch checkout sessions for one-time gives and initial subscription signups
    const sessions = await stripe.checkout.sessions.list({
      limit: 100, // Stripe max per page
      status: 'complete',
      created: { gte: minDate },
    });

    // Only include sessions with give-specific metadata
    // (excludes print/mug/news-toll sessions that also use mode:'payment')
    const giftSessions = sessions.data.filter(session => {
      const type = (session.metadata || {}).type;
      return type === 'gift' || type === 'subscription';
    });

    // Track subscription IDs from checkout sessions to avoid duplicates
    const checkoutSubIds = new Set(
      giftSessions
        .filter(s => s.subscription)
        .map(s => typeof s.subscription === 'string' ? s.subscription : s.subscription.id)
    );

    // 2. Fetch recent paid subscription invoices (captures recurring renewals)
    // Subscription renewals don't create new checkout sessions — only invoices.
    let invoiceGives = [];
    try {
      const invoices = await stripe.invoices.list({
        limit: 50,
        status: 'paid',
        created: { gte: minDate },
      });

      invoiceGives = invoices.data
        .filter(inv => {
          // Only subscription invoices
          if (!inv.subscription) return false;
          const subId = typeof inv.subscription === 'string' ? inv.subscription : inv.subscription.id;
          // Skip the initial create invoice if we already have a checkout session for it
          if (inv.billing_reason === 'subscription_create' && checkoutSubIds.has(subId)) {
            return false;
          }
          return true;
        })
        .map(inv => {
          const currency = (inv.currency || 'usd').toUpperCase();
          const amount = inv.amount_paid / 100;
          let amountDisplay;
          if (currency === 'DKK') {
            amountDisplay = `${Math.round(amount)} kr`;
          } else {
            amountDisplay = `$${amount.toFixed(amount % 1 === 0 ? 0 : 2)}`;
          }
          return {
            id: inv.id,
            amount,
            amountDisplay,
            currency,
            isRecurring: true,
            note: '', // Renewal invoices don't carry the original checkout note
            createdAt: (inv.status_transitions?.paid_at || inv.created) * 1000,
          };
        });
    } catch (e) {
      console.log('Could not fetch subscription invoices:', e.message);
    }

    // 3. Convert checkout sessions to gives
    const sessionGives = giftSessions.slice(0, limit).map(session => {
      const metadata = session.metadata || {};
      const currency = (session.currency || 'usd').toUpperCase();
      const amount = session.amount_total / 100;
      const isRecurring = metadata.type === 'subscription';
      // Note comes from custom_fields (checkout form), not metadata
      const customFields = session.custom_fields || [];
      const noteField = customFields.find(f => f.key === 'note');
      const note = noteField?.text?.value || '';
      
      // Format amount based on currency
      let amountDisplay;
      if (currency === 'DKK') {
        amountDisplay = `${Math.round(amount)} kr`;
      } else {
        amountDisplay = `$${amount.toFixed(amount % 1 === 0 ? 0 : 2)}`;
      }

      return {
        id: session.id,
        amount,
        amountDisplay,
        currency,
        isRecurring,
        note,
        createdAt: session.created * 1000, // Convert to JS timestamp
      };
    });

    // 4. Merge, sort by date (newest first), and limit
    const gives = [...sessionGives, ...invoiceGives]
      .sort((a, b) => b.createdAt - a.createdAt)
      .slice(0, limit);

    // Calculate some stats
    const totalAmount = gives.reduce((sum, g) => {
      // Convert to USD for total (approximate)
      const usdAmount = g.currency === 'DKK' ? g.amount / 7 : g.amount;
      return sum + usdAmount;
    }, 0);

    // Calculate this month's total (in USD)
    const now = new Date();
    const startOfMonth = new Date(now.getFullYear(), now.getMonth(), 1).getTime();
    const monthlyGives = gives.filter(g => g.createdAt >= startOfMonth);
    const monthlyTotalUSD = monthlyGives.reduce((sum, g) => {
      const usdAmount = g.currency === 'DKK' ? g.amount / 7 : g.amount;
      return sum + usdAmount;
    }, 0);

    // Count active monthly subscribers
    let activeSubscribers = 0;
    try {
      const subscriptions = await stripe.subscriptions.list({
        status: 'active',
        limit: 100,
      });
      activeSubscribers = subscriptions.data.length;
    } catch (e) {
      console.log('Could not fetch subscriptions:', e.message);
    }

    const result = {
      gives,
      count: gives.length,
      monthlyCount: monthlyGives.length,
      monthlyTotalUSD: Math.round(monthlyTotalUSD),
      totalUSD: Math.round(totalAmount),
      activeSubscribers,
      lastUpdated: new Date().toISOString(),
    };

    // Cache the result
    cachedGives = result;
    cacheTimestamp = now;

    return respond(200, result);
  } catch (error) {
    console.error("Gives feed error:", error);
    return respond(500, { error: error.message });
  }
}
