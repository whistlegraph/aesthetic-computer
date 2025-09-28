// Prototype implementation of the KidLisp instance manager
// Tests hierarchical instance management and recursion detection

export class KidLispInstanceManager {
  constructor(cacheManager) {
    this.cacheManager = cacheManager;
    this.instances = new Map();      // instanceId -> instance
    this.hierarchy = new Map();      // parentId -> Set<childId>
    this.recursionStack = [];        // [cacheId, ...] current execution stack
    this.recursionDepth = new Map(); // instanceId -> depth
    this.maxRecursionDepth = 10;     // Configurable limit
    
    // Statistics for testing
    this.stats = {
      instancesCreated: 0,
      recursionDetected: 0,
      maxDepthReached: 0,
      cyclesDetected: 0,
      instancesGarbageCollected: 0
    };
  }
  
  // Create or get instance with proper hierarchy
  createInstance(options = {}) {
    const { 
      parentId = null, 
      bounds = { x: 0, y: 0, width: 100, height: 100 }, 
      type = 'embedded',
      cacheId = null
    } = options;
    
    // Generate unique instance ID
    const instanceId = this._generateInstanceId(type, bounds, cacheId);
    
    // Check if instance already exists
    if (this.instances.has(instanceId)) {
      return this.instances.get(instanceId);
    }
    
    // Calculate recursion depth
    const depth = parentId ? (this.recursionDepth.get(parentId) || 0) + 1 : 0;
    
    // Create instance object
    const instance = {
      id: instanceId,
      parentId,
      type,
      bounds,
      cacheId,
      depth,
      children: new Set(),
      created: Date.now(),
      lastUsed: Date.now(),
      state: {
        embeddedLayers: [],
        loadingCodes: new Set(),
        paintingCache: new Map()
      }
    };
    
    // Store instance
    this.instances.set(instanceId, instance);
    this.recursionDepth.set(instanceId, depth);
    this.stats.instancesCreated++;
    
    // Update hierarchy
    if (parentId && this.instances.has(parentId)) {
      const parent = this.instances.get(parentId);
      parent.children.add(instanceId);
      
      if (!this.hierarchy.has(parentId)) {
        this.hierarchy.set(parentId, new Set());
      }
      this.hierarchy.get(parentId).add(instanceId);
    }
    
    // Track max depth
    if (depth > this.stats.maxDepthReached) {
      this.stats.maxDepthReached = depth;
    }
    
    return instance;
  }
  
  // Execute with recursion detection and cycle prevention
  async executeWithRecursionGuard(instanceId, source, mockApi = {}) {
    const instance = this.instances.get(instanceId);
    if (!instance) {
      throw new Error(`Instance not found: ${instanceId}`);
    }
    
    // Update last used time
    instance.lastUsed = Date.now();
    
    // Check recursion depth limit first
    if (instance.depth >= this.maxRecursionDepth) {
      this.stats.recursionDetected++;
      return this._createDepthError(instance);
    }
    
    // Extract $codes from source
    const extractedCodes = this.cacheManager.extractDollarCodes(source);
    
    // Check for immediate cycles in current stack
    for (const code of extractedCodes) {
      if (this.recursionStack.includes(code)) {
        this.stats.cyclesDetected++;
        return this._createCycleError(code, instance);
      }
    }
    
    try {
      // Preload all needed codes
      if (extractedCodes.length > 0) {
        await this.cacheManager.getBatchCachedCodes(extractedCodes);
      }
      
      // Execute with internal recursion handling
      const result = await this._simulateExecution(instance, source, mockApi);
      
      return result;
    } catch (error) {
      console.error('Execution error:', error);
      return {
        type: 'execution-error',
        message: error.message,
        instanceId: instance.id,
        bounds: instance.bounds
      };
    }
  }
  
  // Check if a code would create a cycle
  wouldCreateCycle(cacheId) {
    return this.recursionStack.includes(cacheId);
  }
  
  // Get recursion path to a code
  getRecursionPath(targetCode) {
    const index = this.recursionStack.indexOf(targetCode);
    return index >= 0 ? this.recursionStack.slice(index) : null;
  }
  
  // Garbage collect unused instances
  garbageCollect(maxAge = 60000) { // 1 minute default
    const now = Date.now();
    const toDelete = [];
    
    for (const [id, instance] of this.instances.entries()) {
      if (now - instance.lastUsed > maxAge) {
        toDelete.push(id);
      }
    }
    
    for (const id of toDelete) {
      this._deleteInstance(id);
      this.stats.instancesGarbageCollected++;
    }
    
    return toDelete.length;
  }
  
  // Get instance hierarchy as tree structure
  getHierarchyTree(rootId = null) {
    const buildTree = (instanceId) => {
      const instance = this.instances.get(instanceId);
      if (!instance) return null;
      
      const tree = {
        id: instanceId,
        type: instance.type,
        depth: instance.depth,
        cacheId: instance.cacheId,
        children: []
      };
      
      for (const childId of instance.children) {
        const childTree = buildTree(childId);
        if (childTree) tree.children.push(childTree);
      }
      
      return tree;
    };
    
    if (rootId) {
      return buildTree(rootId);
    }
    
    // Return all root instances
    const roots = [];
    for (const [id, instance] of this.instances.entries()) {
      if (!instance.parentId) {
        roots.push(buildTree(id));
      }
    }
    
    return roots;
  }
  
  // Internal methods
  
  _generateInstanceId(type, bounds, cacheId) {
    const parts = [
      type,
      `${bounds.x}-${bounds.y}-${bounds.width}-${bounds.height}`,
      cacheId || 'inline',
      Date.now().toString(36),
      Math.random().toString(36).substr(2, 5)
    ];
    return parts.join('_');
  }
  
  async _simulateExecution(instance, source, mockApi) {
    // Simulate parsing and execution time
    await new Promise(resolve => setTimeout(resolve, 5 + Math.random() * 10));
    
    // Extract embedded codes and create child instances
    const embeddedCodes = this.cacheManager.extractDollarCodes(source);
    const embeddedResults = [];
    
    for (const code of embeddedCodes) {
      // Check for cycles BEFORE creating child instances
      if (this.recursionStack.includes(code)) {
        return this._createCycleError(code, instance);
      }
      
      // Simulate embedded layer bounds (in real implementation these would be parsed)
      const childBounds = {
        x: instance.bounds.x + Math.random() * 50,
        y: instance.bounds.y + Math.random() * 50,
        width: instance.bounds.width * 0.8,
        height: instance.bounds.height * 0.8
      };
      
      // Create child instance
      const childInstance = this.createInstance({
        parentId: instance.id,
        bounds: childBounds,
        type: 'embedded-layer',
        cacheId: code
      });
      
      // Get cached source for child
      const childSource = await this.cacheManager.getCachedCode(code);
      if (childSource) {
        // Add to recursion stack before recursive call
        this.recursionStack.push(code);
        
        try {
          // Recursively execute child
          const childResult = await this.executeWithRecursionGuard(
            childInstance.id, 
            childSource, 
            mockApi
          );
          embeddedResults.push(childResult);
          
          // If child returned an error, propagate it
          if (childResult.type === 'cycle-error' || childResult.type === 'depth-error') {
            return childResult;
          }
        } finally {
          // Remove from recursion stack after recursive call
          const index = this.recursionStack.lastIndexOf(code);
          if (index > -1) {
            this.recursionStack.splice(index, 1);
          }
        }
      }
    }
    
    return {
      type: 'execution-result',
      instanceId: instance.id,
      source: source.substring(0, 100) + (source.length > 100 ? '...' : ''),
      embeddedCount: embeddedResults.length,
      embeddedResults,
      bounds: instance.bounds,
      depth: instance.depth
    };
  }
  
  _createCycleError(code, instance) {
    const path = this.getRecursionPath(code);
    return {
      type: 'cycle-error',
      message: `Recursion cycle detected: ${code}`,
      cycle: path,
      instanceId: instance.id,
      bounds: instance.bounds
    };
  }
  
  _createDepthError(instance) {
    return {
      type: 'depth-error',
      message: `Maximum recursion depth (${this.maxRecursionDepth}) exceeded`,
      depth: instance.depth,
      instanceId: instance.id,
      bounds: instance.bounds
    };
  }
  
  _deleteInstance(instanceId) {
    const instance = this.instances.get(instanceId);
    if (!instance) return;
    
    // Delete all children first
    for (const childId of instance.children) {
      this._deleteInstance(childId);
    }
    
    // Remove from parent's children
    if (instance.parentId) {
      const parent = this.instances.get(instance.parentId);
      if (parent) {
        parent.children.delete(instanceId);
      }
      
      const siblings = this.hierarchy.get(instance.parentId);
      if (siblings) {
        siblings.delete(instanceId);
        if (siblings.size === 0) {
          this.hierarchy.delete(instance.parentId);
        }
      }
    }
    
    // Remove instance
    this.instances.delete(instanceId);
    this.recursionDepth.delete(instanceId);
  }
  
  // Testing utilities
  
  clearAllInstances() {
    this.instances.clear();
    this.hierarchy.clear();
    this.recursionStack = [];
    this.recursionDepth.clear();
    this.resetStats();
  }
  
  resetStats() {
    this.stats = {
      instancesCreated: 0,
      recursionDetected: 0,
      maxDepthReached: 0,
      cyclesDetected: 0,
      instancesGarbageCollected: 0
    };
  }
  
  getStats() {
    return {
      ...this.stats,
      totalInstances: this.instances.size,
      hierarchyNodes: this.hierarchy.size,
      currentStackDepth: this.recursionStack.length
    };
  }
}
