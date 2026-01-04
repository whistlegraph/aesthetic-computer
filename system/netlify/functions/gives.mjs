// Gives Feed, 26.01.03
// 🎁 Fetches recent successful gives from Stripe
// Endpoint: GET /api/gives

import Stripe from "stripe";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";

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
    const limit = parseInt(event.queryStringParameters?.limit) || 10;
    
    // Only show gives from 2025 onwards (when give page launched)
    const minDate = new Date('2025-01-01').getTime() / 1000; // Unix timestamp
    
    // Fetch recent successful checkout sessions (both one-time and subscriptions)
    const sessions = await stripe.checkout.sessions.list({
      limit: Math.min(limit * 2, 50), // Fetch extra to filter
      status: 'complete',
      expand: ['data.line_items'],
      created: { gte: minDate }, // Only sessions from 2025+
    });

    // Process sessions into a clean format
    // Include all completed sessions - gifts will have metadata, older ones won't
    const gives = sessions.data
      .filter(session => {
        // Include sessions with gift/subscription metadata OR any payment session
        // (to catch older donations before metadata was added)
        const metadata = session.metadata || {};
        const hasGiftMeta = metadata.type === 'gift' || metadata.type === 'subscription';
        const isPayment = session.mode === 'payment' || session.mode === 'subscription';
        return hasGiftMeta || isPayment;
      })
      .slice(0, limit)
      .map(session => {
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
          // Don't expose customer info for privacy
        };
      });

    // Calculate some stats
    const totalAmount = gives.reduce((sum, g) => {
      // Convert to USD for total (approximate)
      const usdAmount = g.currency === 'DKK' ? g.amount / 7 : g.amount;
      return sum + usdAmount;
    }, 0);

    const result = {
      gives,
      count: gives.length,
      totalUSD: Math.round(totalAmount),
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
