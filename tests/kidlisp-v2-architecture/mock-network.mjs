// Mock network layer for /api/store-kidlisp testing
// Simulates the actual API responses and network delays

export class MockKidLispStore {
  constructor() {
    this.store = new Map();
    this.requestLog = [];
    this.networkDelay = 50; // ms
    this.failureRate = 0; // 0-1, probability of request failure
    
    // Pre-populate with test data
    this.setupTestData();
  }
  
  setupTestData() {
    // Simulate the $cow example scenario
    this.store.set('cow', `
      (brush 255)
      (paint
        ($39i 10 10 100 100)
        ($r2f 120 10 100 100))
    `);
    
    this.store.set('39i', `
      (brush 255 0 0)
      (paint
        (circle 50 50 40))
    `);
    
    this.store.set('r2f', `
      (brush 0 255 0)
      (paint
        (rect 10 10 80 80))
    `);
    
    // Recursive test cases
    this.store.set('recursive-a', '(paint ($recursive-b 0 0 100 100))');
    this.store.set('recursive-b', '(paint ($recursive-a 0 0 50 50))');
    
    // Deep embedding test
    this.store.set('deep-1', '(paint ($deep-2 0 0 100 100))');
    this.store.set('deep-2', '(paint ($deep-3 0 0 90 90))');
    this.store.set('deep-3', '(paint ($deep-4 0 0 80 80))');
    this.store.set('deep-4', '(paint ($deep-5 0 0 70 70))');
    this.store.set('deep-5', '(paint (circle 35 35 30))');
  }
  
  // Mock single code fetch (GET /api/store-kidlisp?code=xxx)
  async fetchCode(cacheId) {
    await this.simulateNetworkDelay();
    this.logRequest('GET_SINGLE', cacheId);
    
    if (Math.random() < this.failureRate) {
      throw new Error(`Network error fetching ${cacheId}`);
    }
    
    return this.store.get(cacheId) || null;
  }
  
  // Mock batch code fetch (GET /api/store-kidlisp?codes=xxx,yyy,zzz)
  async fetchBatchCodes(cacheIds) {
    await this.simulateNetworkDelay();
    this.logRequest('GET_BATCH', cacheIds);
    
    if (Math.random() < this.failureRate) {
      throw new Error(`Network error fetching batch ${cacheIds.join(',')}`);
    }
    
    const results = {};
    for (const id of cacheIds) {
      const code = this.store.get(id);
      if (code) results[id] = code;
    }
    
    return results;
  }
  
  // Mock code storage (POST /api/store-kidlisp)
  async storeCode(cacheId, source) {
    await this.simulateNetworkDelay();
    this.logRequest('POST', cacheId);
    
    if (Math.random() < this.failureRate) {
      throw new Error(`Network error storing ${cacheId}`);
    }
    
    this.store.set(cacheId, source);
    return { success: true, cacheId };
  }
  
  // Simulate network latency
  async simulateNetworkDelay() {
    const delay = this.networkDelay + (Math.random() * this.networkDelay);
    await new Promise(resolve => setTimeout(resolve, delay));
  }
  
  // Log all requests for analysis
  logRequest(type, data) {
    this.requestLog.push({
      timestamp: Date.now(),
      type,
      data: Array.isArray(data) ? [...data] : data
    });
  }
  
  // Analysis methods for testing
  getRequestStats() {
    const stats = {
      total: this.requestLog.length,
      single: this.requestLog.filter(r => r.type === 'GET_SINGLE').length,
      batch: this.requestLog.filter(r => r.type === 'GET_BATCH').length,
      store: this.requestLog.filter(r => r.type === 'POST').length,
      duplicates: this.findDuplicateRequests()
    };
    
    return stats;
  }
  
  findDuplicateRequests() {
    const seen = new Set();
    const duplicates = [];
    
    for (const request of this.requestLog) {
      if (request.type === 'GET_SINGLE') {
        if (seen.has(request.data)) {
          duplicates.push(request.data);
        }
        seen.add(request.data);
      }
    }
    
    return duplicates;
  }
  
  clearRequestLog() {
    this.requestLog = [];
  }
  
  setNetworkDelay(ms) {
    this.networkDelay = ms;
  }
  
  setFailureRate(rate) {
    this.failureRate = Math.max(0, Math.min(1, rate));
  }
}

// Global mock instance
export const mockStore = new MockKidLispStore();
