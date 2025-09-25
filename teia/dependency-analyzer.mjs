/**
 * Dependency Analyzer for TEIA Packaging
 * Analyzes piece code to determine which dependencies are actually needed
 * and which can be safely excluded to reduce package size.
 */

export class DependencyAnalyzer {
  constructor() {
    // Dynamic dependencies that are loaded on-demand and safe to exclude
    this.dynamicDependencies = {
      // wasmboy - Game Boy emulator (19MB)
      'wasmboy': {
        patterns: [
          /sound\.gameboy/i,
          /gameboy\./i, 
          /WasmBoy/i,
          /initGameboy/i,
          /\.gb$/i,
          /\.gbc$/i
        ],
        description: 'Game Boy emulator'
      },
      
      // @mediapipe - Hand tracking (25MB) 
      '@mediapipe': {
        patterns: [
          /hand\./i,
          /mediapipe/i,
          /hand:/i,
          /handtracking/i,
          /gesture/i
        ],
        description: 'MediaPipe hand tracking'
      },
      
      // aframe - WebVR framework (2MB)
      'aframe': {
        patterns: [
          /aframe/i,
          /a-frame/i,
          /WebVR/i,
          /vr\s*mode/i,
          /VRButton/i
        ],
        description: 'A-Frame WebVR'
      },
      
      // web3 - Blockchain (1.4MB)
      'web3': {
        patterns: [
          /web3/i,
          /ethereum/i,
          /blockchain/i,
          /crypto/i,
          /wallet/i,
          /metamask/i
        ],
        description: 'Web3 blockchain libraries'
      },
      
      // gpt3-tokenizer - AI tokenization (3.5MB)
      'gpt3-tokenizer': {
        patterns: [
          /gpt/i,
          /tokenizer/i,
          /openai/i,
          /ai\./i,
          /llm/i,
          /chatgpt/i
        ],
        description: 'GPT tokenizer'
      },
      
      // webpxmux - WebP processing (8.4MB)
      'webpxmux': {
        patterns: [
          /webp/i,
          /\.webp/i,
          /webpxmux/i
        ],
        description: 'WebP muxing'
      },
      
      // three - Three.js 3D graphics (1.5MB)
      'three': {
        patterns: [
          /system.*["']fps["']/i,
          /system.*["']3d["']/i,
          /THREE\./i,
          /Form\(/i,
          /CUBEL/i,
          /form\(/i,
          /3d\s*graphics/i
        ],
        description: 'Three.js 3D graphics (static import)'
      },
      
      // geckos - UDP networking (0MB bundled, loaded dynamically)
      'geckos': {
        patterns: [
          /system.*["']world["']/i,
          /multiplayer/i,
          /udp/i,
          /networking/i,
          /geckos/i,
          /udp:connect/i,
          /udp:send/i,
          /udp:disconnect/i
        ],
        description: 'Geckos UDP networking (static import)'
      }
    };
    
    // Static dependencies that might be conditionally needed
    this.conditionalDependencies = {
    };
  }

  /**
   * Analyze a piece to determine which dependencies can be excluded
   * @param {string} pieceCode - The piece source code
   * @param {string} pieceSystem - The piece system (if any)
   * @param {Object} options - Analysis options
   * @returns {Object} Analysis result with exclusions and recommendations
   */
  analyzePiece(pieceCode, pieceSystem = '', options = {}) {
    const result = {
      exclusions: [],
      required: [],
      savings: 0,
      analysis: {},
      recommendations: []
    };

    // Analyze dynamic dependencies (safe to exclude)
    for (const [depName, config] of Object.entries(this.dynamicDependencies)) {
      const isUsed = this.testDependencyUsage(pieceCode, pieceSystem, config.patterns);
      
      result.analysis[depName] = {
        used: isUsed,
        type: 'dynamic',
        description: config.description,
        safe: true
      };
      
      if (!isUsed) {
        result.exclusions.push(depName);
        result.savings += this.getDependencySize(depName);
      } else {
        result.required.push(depName);
      }
    }

    // Analyze conditional static dependencies (informational for now)
    for (const [depName, config] of Object.entries(this.conditionalDependencies)) {
      const isUsed = this.testDependencyUsage(pieceCode, pieceSystem, config.patterns);
      
      result.analysis[depName] = {
        used: isUsed,
        type: 'static',
        description: config.description,
        safe: false // Not yet implemented for exclusion
      };
      
      if (isUsed) {
        result.required.push(depName);
      }
    }

    // Add recommendations based on analysis
    this.generateRecommendations(result);
    
    return result;
  }

  /**
   * Test if a piece uses a dependency based on patterns
   */
  testDependencyUsage(pieceCode, pieceSystem, patterns) {
    const combinedText = `${pieceCode}\n${pieceSystem}`;
    
    return patterns.some(pattern => {
      if (pattern instanceof RegExp) {
        return pattern.test(combinedText);
      }
      return combinedText.toLowerCase().includes(pattern.toLowerCase());
    });
  }

  /**
   * Get estimated size of a dependency in MB
   */
  getDependencySize(depName) {
    const sizes = {
      '@mediapipe': 25,
      'wasmboy': 19,
      'webpxmux': 8.4,
      'gpt3-tokenizer': 3.5,
      'aframe': 2.0,
      'web3': 1.4,
      'three': 1.5,
      'geckos': 0 // Loaded dynamically, no static bundle size
    };
    
    return sizes[depName] || 0;
  }

  /**
   * Generate exclusion patterns for bundleDepFiles
   */
  generateExclusionPatterns(exclusions) {
    const patterns = [];
    
    for (const dep of exclusions) {
      switch (dep) {
        case 'wasmboy':
          patterns.push('wasmboy/**');
          break;
        case '@mediapipe':
          patterns.push('@mediapipe/**');
          patterns.push('tasks-vision/**');
          break;
        case 'aframe':
          patterns.push('aframe*');
          break;
        case 'web3':
          patterns.push('web3/**');
          break;
        case 'gpt3-tokenizer':
          patterns.push('gpt3-tokenizer/**');
          break;
        case 'webpxmux':
          patterns.push('webpxmux/**');
          break;
        case 'three':
          patterns.push('three/**');
          break;
        case 'geckos':
          patterns.push('geckos.io-client.*.min.js');
          break;
      }
    }
    
    return patterns;
  }

  /**
   * Generate recommendations based on analysis
   */
  generateRecommendations(result) {
    const totalSavings = result.savings;
    
    if (totalSavings > 30) {
      result.recommendations.push(`🎉 Excellent! Can save ${totalSavings.toFixed(1)}MB (~${Math.round(totalSavings/65*100)}% reduction)`);
    } else if (totalSavings > 10) {
      result.recommendations.push(`✅ Good savings: ${totalSavings.toFixed(1)}MB reduction possible`);
    } else if (totalSavings > 0) {
      result.recommendations.push(`📉 Small savings: ${totalSavings.toFixed(1)}MB reduction possible`);
    } else {
      result.recommendations.push(`ℹ️ This piece uses most dependencies - limited savings available`);
    }

    // Specific recommendations
    if (!result.analysis['wasmboy']?.used) {
      result.recommendations.push('🎮 Game Boy emulator not needed - safe to exclude');
    }
    
    if (!result.analysis['@mediapipe']?.used) {
      result.recommendations.push('👋 Hand tracking not needed - safe to exclude');
    }
  }

  /**
   * Generate a summary report
   */
  generateReport(analysis) {
    const lines = [
      `📊 Dependency Analysis Report`,
      `================================`,
      ``,
      `💾 Potential Savings: ${analysis.savings.toFixed(1)}MB`,
      `🚫 Dependencies to Exclude: ${analysis.exclusions.length}`,
      `✅ Dependencies Required: ${analysis.required.length}`,
      ``,
      `📋 Exclusions:`
    ];

    for (const dep of analysis.exclusions) {
      const info = analysis.analysis[dep];
      const size = this.getDependencySize(dep);
      lines.push(`  • ${dep} (${size}MB) - ${info.description}`);
    }

    if (analysis.required.length > 0) {
      lines.push(``, `📋 Required Dependencies:`);
      for (const dep of analysis.required) {
        const info = analysis.analysis[dep];
        const size = this.getDependencySize(dep);
        const type = info.type === 'static' ? ' (static import)' : '';
        lines.push(`  • ${dep} (${size}MB) - ${info.description}${type}`);
      }
    }

    lines.push(``, `💡 Recommendations:`);
    for (const rec of analysis.recommendations) {
      lines.push(`  ${rec}`);
    }

    return lines.join('\n');
  }
}