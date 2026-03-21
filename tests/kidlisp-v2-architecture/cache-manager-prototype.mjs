// Prototype implementation of the unified KidLisp cache manager
// Tests the network call deduplication and batching strategies

import { mockStore } from './mock-network.mjs';

export class KidLispCacheManager {
  constructor() {
    this.globalCache = new Map();           // Unified RAM cache
    this.requestDeduplication = new Map();  // Prevent duplicate fetches
    this.batchQueue = new Set();            // Queue for batching
    this.batchTimer = null;                 // Timer for batch processing
    this.batchDelay = 10;                   // ms to wait before batching
    this.persistentCache = new Map();       // Mock IndexedDB
    
    // Statistics for testing
    this.stats = {
      cacheHits: 0,
      cacheMisses: 0,
      networkRequests: 0,
      deduplicatedRequests: 0,
      batchRequests: 0
    };
  }
  
  // Main cache access method with deduplication
  async getCachedCode(cacheId) {
    // Check RAM cache first
    if (this.globalCache.has(cacheId)) {
      this.stats.cacheHits++;
      return this.globalCache.get(cacheId);
    }
    
    this.stats.cacheMisses++;
    
    // Check if request already in flight
    if (this.requestDeduplication.has(cacheId)) {
      this.stats.deduplicatedRequests++;
      return this.requestDeduplication.get(cacheId);
    }
    
    // Create new request promise
    const promise = this._fetchWithFallbacks(cacheId);
    this.requestDeduplication.set(cacheId, promise);
    
    try {
      const result = await promise;
      if (result) {
        this.globalCache.set(cacheId, result);
      }
      return result;
    } finally {
      this.requestDeduplication.delete(cacheId);
    }
  }
  
  // Batch multiple cache requests
  async getBatchCachedCodes(cacheIds) {
    const results = {};
    const uncachedIds = [];
    
    // Check what's already cached
    for (const id of cacheIds) {
      if (this.globalCache.has(id)) {
        results[id] = this.globalCache.get(id);
        this.stats.cacheHits++;
      } else {
        uncachedIds.push(id);
        this.stats.cacheMisses++;
      }
    }
    
    // Nothing to fetch
    if (uncachedIds.length === 0) {
      return results;
    }
    
    // Check deduplication for uncached codes
    const stillNeeded = [];
    const pendingPromises = [];
    
    for (const id of uncachedIds) {
      if (this.requestDeduplication.has(id)) {
        pendingPromises.push(
          this.requestDeduplication.get(id).then(code => ({ id, code }))
        );
        this.stats.deduplicatedRequests++;
      } else {
        stillNeeded.push(id);
      }
    }
    
    // Fetch needed codes as batch
    let batchResults = {};
    if (stillNeeded.length > 0) {
      this.stats.batchRequests++;
      batchResults = await this._fetchBatch(stillNeeded);
    }
    
    // Wait for any pending individual requests
    const pendingResults = await Promise.all(pendingPromises);
    for (const { id, code } of pendingResults) {
      if (code) batchResults[id] = code;
    }
    
    // Cache all results
    for (const [id, code] of Object.entries(batchResults)) {
      if (code) {
        this.globalCache.set(id, code);
        results[id] = code;
      }
    }
    
    return results;
  }
  
  // Internal method: fetch with fallback chain
  async _fetchWithFallbacks(cacheId) {
    try {
      // 1. Check persistent cache (IndexedDB simulation)
      if (this.persistentCache.has(cacheId)) {
        const code = this.persistentCache.get(cacheId);
        return code;
      }
      
      // 2. Fetch from network
      this.stats.networkRequests++;
      const code = await mockStore.fetchCode(cacheId);
      
      // 3. Store in persistent cache
      if (code) {
        this.persistentCache.set(cacheId, code);
      }
      
      return code;
    } catch (error) {
      console.warn(`Failed to fetch ${cacheId}:`, error.message);
      return null;
    }
  }
  
  // Internal method: batch fetch
  async _fetchBatch(cacheIds) {
    try {
      this.stats.networkRequests++;
      const results = await mockStore.fetchBatchCodes(cacheIds);
      
      // Store in persistent cache
      for (const [id, code] of Object.entries(results)) {
        if (code) {
          this.persistentCache.set(id, code);
        }
      }
      
      return results;
    } catch (error) {
      console.warn(`Failed to fetch batch [${cacheIds.join(', ')}]:`, error.message);
      return {};
    }
  }
  
  // Extract $code references from source
  extractDollarCodes(source) {
    const dollarCodeRegex = /\$([a-zA-Z0-9_-]+)/g;
    const codes = [];
    let match;
    
    while ((match = dollarCodeRegex.exec(source)) !== null) {
      codes.push(match[1]);
    }
    
    return [...new Set(codes)]; // Remove duplicates
  }
  
  // Preload codes that are likely to be needed
  async preloadCodes(cacheIds) {
    return this.getBatchCachedCodes(cacheIds);
  }
  
  // Clear caches (for testing)
  clearCache() {
    this.globalCache.clear();
    this.requestDeduplication.clear();
    this.persistentCache.clear();
    this.resetStats();
  }
  
  // Reset statistics
  resetStats() {
    this.stats = {
      cacheHits: 0,
      cacheMisses: 0,
      networkRequests: 0,
      deduplicatedRequests: 0,
      batchRequests: 0
    };
  }
  
  // Get cache efficiency metrics
  getCacheEfficiency() {
    const total = this.stats.cacheHits + this.stats.cacheMisses;
    return {
      hitRate: total > 0 ? (this.stats.cacheHits / total) * 100 : 0,
      deduplicationSavings: this.stats.deduplicatedRequests,
      networkRequests: this.stats.networkRequests,
      batchRequests: this.stats.batchRequests,
      totalRequests: total,
      ...this.stats
    };
  }
}
