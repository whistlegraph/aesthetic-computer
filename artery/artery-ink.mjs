#!/usr/bin/env node
/**
 * 🩸 Artery Ink - Modern TUI for Aesthetic Computer
 * Built with Ink (React for CLIs)
 * 
 * Architecture:
 * ┌─────────────────────────────────────────────────────────────┐
 * │ Header: Title + Status Pills                                │
 * ├─────────────────────────────────────────────────────────────┤
 * │ NavBar: Context-aware navigation                            │
 * ├─────────────────────────────────────────────────────────────┤
 * │                                                             │
 * │ MainPanel: Switches between views                           │
 * │   - Menu (default)                                          │
 * │   - Logs (console output)                                   │
 * │   - Pieces (piece browser)                                  │
 * │   - Patterns (parametric runner)                            │
 * │   - REPL (JS console)                                       │
 * │                                                             │
 * ├─────────────────────────────────────────────────────────────┤
 * │ Footer: Status message + hints                              │
 * └─────────────────────────────────────────────────────────────┘
 */

import React from 'react';
import { render, Box, Text, useInput, useApp, useStdout } from 'ink';
import Artery from './artery.mjs';
import https from 'https';
import http from 'http';
import fs from 'fs';
import path from 'path';
import { spawn } from 'child_process';

const { createElement: h, useState, useEffect, useCallback, useRef } = React;

// ═══════════════════════════════════════════════════════════════
// THEME
// ═══════════════════════════════════════════════════════════════

const THEME = {
  connected: 'green',
  ready: 'cyan', 
  remote: 'magenta',
  offline: 'red',
  neutral: 'blue',
  accent: 'yellow',
  muted: 'gray',
  success: 'green',
  warning: 'yellow',
  error: 'red',
  info: 'cyan',
};

const RAINBOW = ['red', 'yellow', 'green', 'cyan', 'blue', 'magenta'];

// ═══════════════════════════════════════════════════════════════
// PATTERN CONFIGS - The heart of parametric patterns!
// ═══════════════════════════════════════════════════════════════

const MELODY_OPTIONS = [
  'twinkle', 'mary', 'old-macdonald', 'yankee-doodle', 'frere-jacques',
  'london-bridge', 'row-row-row', 'skip-to-my-lou', 'camptown-races',
  'oh-susanna', 'amazing-grace', 'auld-lang-syne', 'shenandoah',
  'home-on-the-range', 'red-river-valley', 'scarborough-fair',
  'greensleeves', 'when-the-saints', 'danny-boy'
];

const WALTZ_STYLES = ['classic', 'dark', 'dreamy', 'baroque', 'minimal', 'phonk', 'viennese', 'drill'];
const HIPHOP_STYLES = ['trap', 'boombap', 'lofi', '808', 'halftime', 'phonk', 'drill', 'gfunk'];
const SCALES = ['minor', 'dorian', 'phrygian', 'harmonic', 'major'];
const TEMPOS = ['slow', 'medium', 'viennese'];

// Pattern definitions with full parametric configs
const PATTERN_CONFIGS = {
  'test-notepat.mjs': {
    name: 'composition',
    icon: '🎹',
    desc: 'Full composition [waltz/hiphop]',
    params: [
      { key: 'genre', label: 'Genre', type: 'cycle', options: ['waltz', 'hiphop'], default: 'waltz' },
      { key: 'style', label: 'Style', type: 'cycle', options: [...WALTZ_STYLES, ...HIPHOP_STYLES], default: 'classic' },
      { key: 'bars', label: 'Bars', type: 'number', min: 8, max: 64, step: 4, default: 24 },
      { key: 'bpm', label: 'BPM', type: 'number', min: 80, max: 200, step: 10, default: 140 },
      { key: 'scale', label: 'Scale', type: 'cycle', options: SCALES, default: 'minor' },
      { key: 'room', label: 'Room', type: 'toggle', options: ['', 'room'], default: '' },
      { key: 'waves', label: 'Waves', type: 'toggle', options: ['', 'waves'], default: '' },
    ],
  },
  'test-trapwaltz.mjs': {
    name: 'trapwaltz',
    icon: '🩰',
    desc: 'Trap waltz (3/4 + trap)',
    params: [
      { key: 'style', label: 'Style', type: 'cycle', options: WALTZ_STYLES, default: 'classic' },
      { key: 'bars', label: 'Bars', type: 'number', min: 4, max: 32, step: 4, default: 8 },
      { key: 'bpm', label: 'BPM', type: 'number', min: 100, max: 180, step: 10, default: 140 },
      { key: 'room', label: 'Room', type: 'toggle', options: ['', 'room'], default: '' },
      { key: 'progression', label: 'Progression', type: 'cycle', options: ['', 'harmonic', 'romantic', 'dark'], default: '' },
    ],
  },
  'test-hiphop.mjs': {
    name: 'hiphop',
    icon: '🎤',
    desc: 'Hip-hop beat generator',
    params: [
      { key: 'style', label: 'Style', type: 'cycle', options: HIPHOP_STYLES, default: 'trap' },
      { key: 'bars', label: 'Bars', type: 'number', min: 4, max: 32, step: 4, default: 8 },
      { key: 'bpm', label: 'BPM', type: 'number', min: 80, max: 180, step: 10, default: 90 },
    ],
  },
  'test-melody.mjs': {
    name: 'melody',
    icon: '🎵',
    desc: 'Melody playback',
    params: [
      { key: 'melody', label: 'Melody', type: 'cycle', options: MELODY_OPTIONS, default: 'twinkle' },
    ],
  },
  'test-generative-waltz.mjs': {
    name: 'waltz',
    icon: '💃',
    desc: 'Generative waltz',
    params: [
      { key: 'bars', label: 'Bars', type: 'number', min: 4, max: 32, step: 4, default: 8 },
      { key: 'scale', label: 'Scale', type: 'cycle', options: ['major', 'minor', 'dorian'], default: 'major' },
      { key: 'seed', label: 'Seed', type: 'number', min: 1, max: 999999, step: 111, default: Date.now() % 999999 },
      { key: 'tempo', label: 'Tempo', type: 'cycle', options: TEMPOS, default: 'slow' },
      { key: 'topline', label: 'Top Line', type: 'toggle', options: ['', 'topline'], default: '' },
      { key: 'infinite', label: 'Infinite', type: 'toggle', options: ['', 'infinite'], default: '' },
      { key: 'frolic', label: 'Frolic', type: 'toggle', options: ['', 'frolic'], default: '' },
      { key: 'beat', label: 'Beat', type: 'toggle', options: ['', 'beat'], default: '' },
    ],
  },
  'test-1v1-split.mjs': {
    name: '1v1-split',
    icon: '🎮',
    desc: 'Split view for dueling',
    params: [],
  },
  'test-chords.mjs': {
    name: 'chords',
    icon: '🎸',
    desc: 'Chord progression test',
    params: [],
  },
  'test-line.mjs': {
    name: 'line',
    icon: '📏',
    desc: 'Line drawing test',
    params: [],
  },
  'test-toss.mjs': {
    name: 'toss',
    icon: '🎯',
    desc: 'Comprehensive toss test',
    params: [],
  },
  'test-playlist.mjs': {
    name: 'playlist',
    icon: '📋',
    desc: 'Playlist test',
    params: [],
  },
};

// ═══════════════════════════════════════════════════════════════
// UTILITY COMPONENTS
// ═══════════════════════════════════════════════════════════════

const Pill = ({ status, label, compact = false }) => {
  const config = {
    online: { icon: '●', color: 'green' },
    offline: { icon: '○', color: 'red' },
    pending: { icon: '◐', color: 'yellow' },
    unknown: { icon: '?', color: 'gray' },
  };
  const { icon, color } = config[status] || config.unknown;
  return h(Text, { color }, compact ? `${icon}${label || ''}` : `${icon} ${label || ''}`);
};

const Hotkey = ({ char, label, selected = false }) => {
  if (selected) {
    return h(Text, { backgroundColor: 'cyan', color: 'black' },
      h(Text, { bold: true }, `[${char}]`),
      label
    );
  }
  return h(Text, {},
    h(Text, { color: 'yellow', bold: true }, `[${char}]`),
    h(Text, { color: 'white' }, label)
  );
};

const ScrollList = ({ items, selectedIndex, renderItem, height }) => {
  const start = Math.max(0, selectedIndex - Math.floor(height / 2));
  const visible = items.slice(start, start + height);
  
  return h(Box, { flexDirection: 'column' },
    ...visible.map((item, i) => renderItem(item, start + i, start + i === selectedIndex))
  );
};

// ═══════════════════════════════════════════════════════════════
// HEADER
// ═══════════════════════════════════════════════════════════════

const Header = ({ connected, serverStatus, cdpStatus, piece, compact }) => {
  const title = compact ? 'AC' : 'AESTHETIC COMPUTER';
  const borderColor = connected ? THEME.connected 
    : serverStatus.local ? THEME.ready
    : serverStatus.production ? THEME.remote
    : THEME.offline;
  
  return h(Box, {
    borderStyle: 'round',
    borderColor,
    paddingX: 1,
    justifyContent: 'space-between',
  },
    h(Box, { gap: 1 },
      ...title.split('').map((char, i) =>
        h(Text, { key: i, color: char === ' ' ? undefined : RAINBOW[i % RAINBOW.length], bold: true }, char)
      ),
      piece && h(Text, { color: 'gray' }, ' │ '),
      piece && h(Text, { color: 'cyan' }, piece)
    ),
    h(Box, { gap: 1 },
      h(Pill, { status: connected ? 'online' : 'offline', label: compact ? '' : 'AC', compact }),
      h(Pill, { status: serverStatus.local === true ? 'online' : serverStatus.local === false ? 'pending' : 'unknown', label: 'L', compact: true }),
      h(Pill, { status: serverStatus.production === true ? 'online' : 'offline', label: 'P', compact: true }),
      h(Pill, { status: cdpStatus, label: 'CDP', compact: true })
    )
  );
};

// ═══════════════════════════════════════════════════════════════
// NAVIGATION
// ═══════════════════════════════════════════════════════════════

const NavBar = ({ items, view, compact }) => {
  if (compact) {
    return h(Box, { paddingX: 1 },
      h(Text, { color: 'gray' }, items.map(i => i.key).join(' '))
    );
  }
  
  return h(Box, { paddingX: 1, gap: 2 },
    ...items.map((item, i) =>
      h(Hotkey, { key: i, char: item.key, label: item.label, selected: view === item.view })
    )
  );
};

// ═══════════════════════════════════════════════════════════════
// VIEWS
// ═══════════════════════════════════════════════════════════════

const MenuView = ({ items, selectedIndex, compact, height }) => {
  return h(Box, { flexDirection: 'column', paddingX: 1 },
    h(ScrollList, {
      items,
      selectedIndex,
      height: height - 2,
      renderItem: (item, idx, selected) => {
        if (item.separator) {
          return h(Text, { key: idx, color: 'gray', dimColor: true }, '─'.repeat(30));
        }
        return h(Box, { key: idx },
          h(Text, { backgroundColor: selected ? 'cyan' : undefined, color: selected ? 'black' : 'white' },
            selected ? '▸ ' : '  ',
            h(Text, { color: selected ? 'black' : 'yellow', bold: true }, `[${item.key}] `),
            h(Text, { bold: selected }, item.label),
            !compact && item.desc && h(Text, { color: selected ? 'black' : 'gray' }, ` - ${item.desc}`)
          )
        );
      }
    })
  );
};

const LogsView = ({ logs, height, width }) => {
  const visible = logs.slice(-(height - 2));
  
  return h(Box, {
    flexDirection: 'column',
    borderStyle: 'single',
    borderColor: 'gray',
    height,
    paddingX: 1,
  },
    visible.length === 0
      ? h(Text, { color: 'gray', dimColor: true }, 'No logs yet...')
      : visible.map((log, i) => {
          const time = log.ts ? new Date(log.ts).toLocaleTimeString('en-US', { hour12: false }) : '';
          const colors = { info: 'cyan', success: 'green', warn: 'yellow', error: 'red', input: 'magenta', result: 'white' };
          const maxLen = Math.max(20, width - 14);
          const msg = log.msg?.length > maxLen ? log.msg.slice(0, maxLen - 1) + '…' : log.msg;
          return h(Text, { key: i },
            h(Text, { color: 'gray', dimColor: true }, `${time} `),
            h(Text, { color: colors[log.type] || 'white' }, msg)
          );
        })
  );
};

const PiecesView = ({ pieces, selectedIndex, filter, height }) => {
  const filtered = filter 
    ? pieces.filter(p => p.toLowerCase().includes(filter.toLowerCase()))
    : pieces;
  
  return h(Box, { flexDirection: 'column', paddingX: 1 },
    h(Box, {},
      h(Text, { color: 'yellow' }, 'Filter: '),
      h(Text, { color: 'cyan' }, filter || '(type to search)'),
      h(Text, { color: 'gray' }, ` [${filtered.length}/${pieces.length}]`)
    ),
    h(ScrollList, {
      items: filtered,
      selectedIndex,
      height: height - 3,
      renderItem: (piece, idx, selected) =>
        h(Text, { key: idx, backgroundColor: selected ? 'cyan' : undefined, color: selected ? 'black' : 'white' },
          selected ? '▸ ' : '  ', piece
        )
    })
  );
};

// ═══════════════════════════════════════════════════════════════
// PATTERNS VIEW - The star of the show!
// ═══════════════════════════════════════════════════════════════

const PatternsListView = ({ patterns, selectedIndex, height }) => {
  return h(Box, { flexDirection: 'column', paddingX: 1 },
    h(Text, { color: 'yellow', bold: true }, '🎨 Patterns ', h(Text, { color: 'gray', dimColor: true }, '(Enter to configure)')),
    h(ScrollList, {
      items: patterns,
      selectedIndex,
      height: height - 3,
      renderItem: (pattern, idx, selected) => {
        if (pattern.separator) {
          return h(Text, { key: idx, color: 'gray', dimColor: true }, '─'.repeat(40));
        }
        const hasParams = pattern.params?.length > 0;
        return h(Box, { key: idx },
          h(Text, { backgroundColor: selected ? 'cyan' : undefined, color: selected ? 'black' : 'white' },
            selected ? '▸ ' : '  ',
            h(Text, {}, pattern.icon || '▪', ' '),
            h(Text, { bold: selected }, pattern.name),
            hasParams && h(Text, { color: selected ? 'black' : 'magenta' }, ' ⚙'),
            h(Text, { color: selected ? 'black' : 'gray' }, ` - ${pattern.desc}`)
          )
        );
      }
    })
  );
};

const PatternConfigView = ({ pattern, paramIndex, paramValues, height, width }) => {
  const params = pattern.params || [];
  const isOnRun = paramIndex >= params.length;
  
  // Build command preview
  const buildArgs = () => {
    const parts = [];
    for (const p of params) {
      const val = paramValues[p.key];
      if (val && val !== '' && val !== p.default) {
        if (p.type === 'toggle') {
          if (val) parts.push(val);
        } else if (p.type === 'number') {
          parts.push(`${p.key}=${val}`);
        } else {
          parts.push(val);
        }
      } else if (p.key === 'genre' || p.key === 'style' || p.key === 'melody') {
        // Always include primary params
        parts.push(val || p.default);
      }
    }
    return parts.join(' ');
  };
  
  const cmdPreview = `node artery/${pattern.file} ${buildArgs()}`.trim();
  
  return h(Box, { flexDirection: 'column', paddingX: 1 },
    // Title
    h(Box, { marginBottom: 1 },
      h(Text, { color: 'yellow', bold: true }, `⚙ ${pattern.icon || ''} ${pattern.name} `),
      h(Text, { color: 'gray', dimColor: true }, '(↑↓ move, ←→ change, Enter run)')
    ),
    
    // Parameters
    ...params.map((param, i) => {
      const selected = i === paramIndex;
      const value = paramValues[param.key] ?? param.default;
      
      // Value display with arrows for cycling
      let valueDisplay;
      if (param.type === 'cycle' || param.type === 'toggle') {
        const leftArrow = selected ? '◀ ' : '  ';
        const rightArrow = selected ? ' ▶' : '  ';
        valueDisplay = h(Box, {},
          h(Text, { color: selected ? 'gray' : 'gray' }, leftArrow),
          h(Text, { color: selected ? 'yellow' : 'green', bold: selected }, value || '(none)'),
          h(Text, { color: selected ? 'gray' : 'gray' }, rightArrow)
        );
      } else if (param.type === 'number') {
        const leftArrow = selected ? '◀ ' : '  ';
        const rightArrow = selected ? ' ▶' : '  ';
        valueDisplay = h(Box, {},
          h(Text, { color: selected ? 'gray' : 'gray' }, leftArrow),
          h(Text, { color: selected ? 'yellow' : 'green', bold: selected }, String(value)),
          h(Text, { color: selected ? 'gray' : 'gray' }, rightArrow)
        );
      } else {
        valueDisplay = h(Text, { color: 'green' }, String(value));
      }
      
      return h(Box, { key: i },
        h(Text, { backgroundColor: selected ? 'blue' : undefined, color: selected ? 'white' : 'white' },
          selected ? '▸ ' : '  ',
          h(Text, { bold: true, color: selected ? 'white' : 'cyan' }, `${param.label}: `),
        ),
        valueDisplay
      );
    }),
    
    // Separator
    h(Text, { color: 'gray', dimColor: true }, '─'.repeat(Math.min(40, width - 4))),
    
    // Run button
    h(Box, {},
      h(Text, { backgroundColor: isOnRun ? 'green' : undefined, color: isOnRun ? 'black' : 'green', bold: isOnRun },
        isOnRun ? '▸ 🚀 RUN' : '  🚀 RUN'
      )
    ),
    
    // Command preview
    h(Box, { marginTop: 1 },
      h(Text, { color: 'gray', dimColor: true }, '> ', cmdPreview.slice(0, width - 6))
    )
  );
};

const ReplView = ({ history, input, height }) => {
  const visibleHistory = history.slice(-(height - 4));
  
  return h(Box, { flexDirection: 'column', paddingX: 1 },
    h(Text, { color: 'yellow', bold: true }, '🔮 JavaScript REPL'),
    h(Box, { flexDirection: 'column', height: height - 4 },
      ...visibleHistory.map((entry, i) =>
        h(Text, { key: i, color: entry.type === 'input' ? 'cyan' : entry.type === 'error' ? 'red' : 'white' },
          entry.type === 'input' ? '❯ ' : '  ', entry.text
        )
      )
    ),
    h(Box, {},
      h(Text, { color: 'cyan' }, '❯ '),
      h(Text, {}, input),
      h(Text, { color: 'gray' }, '▋')
    )
  );
};

// ═══════════════════════════════════════════════════════════════
// FOOTER
// ═══════════════════════════════════════════════════════════════

const Footer = ({ message, messageType, hints }) => {
  const colors = { info: 'cyan', success: 'green', warn: 'yellow', error: 'red' };
  
  return h(Box, { paddingX: 1, justifyContent: 'space-between' },
    h(Text, { color: colors[messageType] || 'gray' }, message || 'Ready'),
    h(Text, { color: 'gray', dimColor: true }, hints || '[Q]uit [Esc]Back')
  );
};

// ═══════════════════════════════════════════════════════════════
// MAIN APP
// ═══════════════════════════════════════════════════════════════

const App = () => {
  const { exit } = useApp();
  const { stdout } = useStdout();
  
  const [dims, setDims] = useState({ w: stdout?.columns || 80, h: stdout?.rows || 24 });
  const compact = dims.w < 80;
  
  const [connected, setConnected] = useState(false);
  const [serverStatus, setServerStatus] = useState({ local: null, production: null });
  const [cdpStatus, setCdpStatus] = useState('unknown');
  const [piece, setPiece] = useState('');
  
  const [view, setView] = useState('menu');
  const [selectedIndex, setSelectedIndex] = useState(0);
  const [logs, setLogs] = useState([]);
  const [status, setStatus] = useState({ msg: '', type: 'info' });
  
  const [inputBuffer, setInputBuffer] = useState('');
  const [replHistory, setReplHistory] = useState([]);
  
  const [pieces, setPieces] = useState([]);
  const [patterns, setPatterns] = useState([]);
  
  // Pattern config state
  const [currentPattern, setCurrentPattern] = useState(null);
  const [paramIndex, setParamIndex] = useState(0);
  const [paramValues, setParamValues] = useState({});
  
  const clientRef = useRef(null);
  
  const log = useCallback((msg, type = 'info') => {
    setLogs(prev => [...prev.slice(-200), { msg, type, ts: Date.now() }]);
  }, []);
  
  const showStatus = useCallback((msg, type = 'info') => {
    setStatus({ msg, type });
  }, []);
  
  const menuItems = [
    { key: 'J', label: 'Jump to Piece', desc: 'Navigate to a piece', view: 'pieces' },
    { key: 'L', label: 'View Logs', desc: 'AC console output', view: 'logs' },
    { key: 'N', label: 'Patterns', desc: 'Parametric patterns', view: 'patterns' },
    { key: 'R', label: 'REPL', desc: 'JavaScript console', view: 'repl' },
    { separator: true },
    { key: 'P', label: 'Toggle Panel', desc: 'VS Code AC sidebar', action: 'togglePanel' },
    { key: 'K', label: 'KidLisp.com', desc: 'Open KidLisp editor', action: 'openKidLisp' },
    { key: 'A', label: 'Activate Audio', desc: 'Click to enable audio', action: 'activateAudio' },
    { key: 'X', label: 'Reconnect', desc: 'Reconnect to AC', action: 'reconnect' },
    { separator: true },
    { key: 'Q', label: 'Quit', desc: 'Exit Artery', action: 'quit' },
  ];
  
  const navItems = [
    { key: 'M', label: 'enu', view: 'menu' },
    { key: 'J', label: 'ump', view: 'pieces' },
    { key: 'L', label: 'ogs', view: 'logs' },
    { key: 'N', label: 'atterns', view: 'patterns' },
    { key: 'R', label: 'epl', view: 'repl' },
  ];
  
  const checkServer = useCallback(async (url, requireCert = false) => {
    return new Promise(resolve => {
      const client = url.startsWith('https') ? https : http;
      const req = client.get(url, { timeout: 3000, rejectUnauthorized: requireCert }, res => {
        resolve(res.statusCode >= 200 && res.statusCode < 400);
      });
      req.on('error', () => resolve(false));
      req.on('timeout', () => { req.destroy(); resolve(false); });
    });
  }, []);
  
  // Poll server status
  useEffect(() => {
    const poll = async () => {
      const local = await checkServer('https://localhost:8888', false);
      const prod = await checkServer('https://aesthetic.computer', true);
      setServerStatus({ local, production: prod });
      
      const cdp = await checkServer('http://localhost:9333/json/version', false);
      setCdpStatus(cdp ? 'online' : 'offline');
      
      if (clientRef.current?.connected) {
        setConnected(true);
        try {
          const p = await clientRef.current.getCurrentPiece();
          setPiece(p || '');
        } catch {}
      } else {
        setConnected(local);
      }
    };
    
    poll();
    const interval = setInterval(poll, 5000);
    return () => clearInterval(interval);
  }, [checkServer]);
  
  // Load pieces
  useEffect(() => {
    try {
      const disksDir = path.join(process.cwd(), 'system/public/aesthetic.computer/disks');
      if (fs.existsSync(disksDir)) {
        const files = fs.readdirSync(disksDir)
          .filter(f => f.endsWith('.mjs') || f.endsWith('.lisp'))
          .map(f => f.replace(/\.(mjs|lisp)$/, ''))
          .sort();
        setPieces(files);
        log(`Loaded ${files.length} pieces`, 'info');
      }
    } catch (e) {
      log(`Failed to load pieces: ${e.message}`, 'error');
    }
  }, [log]);
  
  // Load patterns from artery/ and .vscode/tests/
  useEffect(() => {
    try {
      const arteryDir = path.dirname(new URL(import.meta.url).pathname);
      const vscodeTestDir = path.join(process.cwd(), '.vscode/tests');
      
      const patternList = [];
      
      // Artery patterns
      const arteryFiles = fs.readdirSync(arteryDir)
        .filter(f => f.startsWith('test-') && f.endsWith('.mjs'))
        .sort();
      
      for (const file of arteryFiles) {
        const config = PATTERN_CONFIGS[file] || {};
        const baseName = file.replace('test-', '').replace('.mjs', '');
        patternList.push({
          name: config.name || baseName,
          file: file,
          path: `artery/${file}`,
          icon: config.icon || '▪',
          desc: config.desc || `Pattern: ${baseName}`,
          params: config.params || [],
          isArtery: true,
        });
      }
      
      // Separator
      patternList.push({ separator: true, name: '───' });
      
      // Legacy .vscode/tests patterns
      if (fs.existsSync(vscodeTestDir)) {
        const legacyFiles = fs.readdirSync(vscodeTestDir)
          .filter(f => f.startsWith('test-') && f.endsWith('.mjs'))
          .sort();
        
        for (const file of legacyFiles) {
          const config = PATTERN_CONFIGS[file] || {};
          const baseName = file.replace('test-', '').replace('.mjs', '');
          patternList.push({
            name: config.name || baseName,
            file: file,
            path: `.vscode/tests/${file}`,
            icon: config.icon || '○',
            desc: config.desc || `Legacy: ${baseName}`,
            params: config.params || [],
            isArtery: false,
          });
        }
      }
      
      setPatterns(patternList);
      log(`Loaded ${patternList.filter(p => !p.separator).length} patterns`, 'info');
    } catch (e) {
      log(`Failed to load patterns: ${e.message}`, 'error');
    }
  }, [log]);
  
  // Resize handler
  useEffect(() => {
    const handleResize = () => setDims({ w: stdout?.columns || 80, h: stdout?.rows || 24 });
    stdout?.on('resize', handleResize);
    return () => stdout?.off('resize', handleResize);
  }, [stdout]);
  
  // Startup log
  useEffect(() => {
    log('🩸 Artery Ink started', 'success');
    log(`Terminal: ${dims.w}x${dims.h}`, 'info');
  }, []);
  
  // ═══════════════════════════════════════════════════════════════
  // ACTIONS
  // ═══════════════════════════════════════════════════════════════
  
  const actions = {
    quit: () => exit(),
    
    togglePanel: async () => {
      showStatus('Toggling AC panel...', 'info');
      try {
        const result = await Artery.togglePanelStandalone();
        showStatus(result.toggled ? (result.nowExpanded ? 'Panel opened!' : 'Panel closed') : 'Panel not found', result.toggled ? 'success' : 'warn');
      } catch (e) {
        showStatus(`Error: ${e.message}`, 'error');
      }
    },
    
    openKidLisp: async () => {
      showStatus('Opening KidLisp.com...', 'info');
      try {
        await Artery.openKidLispWindow();
        showStatus('KidLisp.com opened!', 'success');
      } catch (e) {
        showStatus(`Error: ${e.message}`, 'error');
      }
    },
    
    activateAudio: async () => {
      showStatus('Activating audio...', 'info');
      try {
        if (clientRef.current) {
          await clientRef.current.click(100, 100);
          showStatus('Audio activated!', 'success');
        } else {
          showStatus('Not connected', 'warn');
        }
      } catch (e) {
        showStatus(`Error: ${e.message}`, 'error');
      }
    },
    
    reconnect: async () => {
      showStatus('Reconnecting...', 'info');
      try {
        clientRef.current = new Artery();
        await clientRef.current.connect();
        setConnected(true);
        showStatus('Connected!', 'success');
        log('Connected to AC', 'success');
      } catch (e) {
        showStatus(`Failed: ${e.message}`, 'error');
        log(`Connection failed: ${e.message}`, 'error');
      }
    },
    
    jumpToPiece: async (pieceName) => {
      if (!clientRef.current?.connected) {
        showStatus('Not connected', 'warn');
        return;
      }
      showStatus(`Jumping to ${pieceName}...`, 'info');
      try {
        await clientRef.current.jump(pieceName);
        setPiece(pieceName);
        showStatus(`Jumped to ${pieceName}`, 'success');
        log(`Jumped to ${pieceName}`, 'success');
      } catch (e) {
        showStatus(`Error: ${e.message}`, 'error');
      }
    },
    
    runPattern: async (pattern, values) => {
      setView('logs');
      
      // Build args from param values
      const args = [];
      for (const p of (pattern.params || [])) {
        const val = values[p.key];
        if (val && val !== '' && val !== p.default) {
          if (p.type === 'toggle') {
            if (val) args.push(val);
          } else if (p.type === 'number') {
            args.push(`${p.key}=${val}`);
          } else {
            args.push(val);
          }
        } else if (p.key === 'genre' || p.key === 'style' || p.key === 'melody') {
          args.push(val || p.default);
        }
      }
      
      const argsStr = args.join(' ');
      log(`🎨 Running ${pattern.name}${argsStr ? ` (${argsStr})` : ''}...`, 'info');
      showStatus(`Running ${pattern.name}...`, 'info');
      
      try {
        const proc = spawn('node', [pattern.path, ...args], {
          cwd: process.cwd(),
          stdio: ['ignore', 'pipe', 'pipe'],
        });
        
        proc.stdout.on('data', data => {
          data.toString().split('\n').filter(l => l.trim()).forEach(line => log(line, 'info'));
        });
        proc.stderr.on('data', data => {
          data.toString().split('\n').filter(l => l.trim()).forEach(line => log(line, 'error'));
        });
        proc.on('close', code => {
          log(`Pattern finished with code ${code}`, code === 0 ? 'success' : 'error');
          showStatus(`${pattern.name} ${code === 0 ? 'completed!' : 'failed'}`, code === 0 ? 'success' : 'error');
        });
      } catch (e) {
        log(`Pattern error: ${e.message}`, 'error');
        showStatus(`Error: ${e.message}`, 'error');
      }
    },
    
    evalRepl: async (code) => {
      if (!code.trim()) return;
      setReplHistory(prev => [...prev, { type: 'input', text: code }]);
      
      if (!clientRef.current?.connected) {
        setReplHistory(prev => [...prev, { type: 'error', text: 'Not connected to AC' }]);
        return;
      }
      
      try {
        const result = await clientRef.current.eval(code);
        setReplHistory(prev => [...prev, { type: 'result', text: String(result ?? 'undefined') }]);
      } catch (e) {
        setReplHistory(prev => [...prev, { type: 'error', text: e.message }]);
      }
    },
  };
  
  // ═══════════════════════════════════════════════════════════════
  // INPUT HANDLING
  // ═══════════════════════════════════════════════════════════════
  
  const cycleParamValue = (param, direction) => {
    const currentVal = paramValues[param.key] ?? param.default;
    
    if (param.type === 'cycle' || param.type === 'toggle') {
      const opts = param.options;
      const idx = opts.indexOf(currentVal);
      const newIdx = direction > 0 
        ? (idx + 1) % opts.length 
        : (idx - 1 + opts.length) % opts.length;
      return opts[newIdx];
    } else if (param.type === 'number') {
      const val = parseInt(currentVal) || param.default;
      const step = param.step || 1;
      const newVal = direction > 0 ? val + step : val - step;
      return Math.max(param.min, Math.min(param.max, newVal));
    }
    return currentVal;
  };
  
  useInput((input, key) => {
    // Global quit
    if (input.toLowerCase() === 'q' && view === 'menu') {
      exit();
      return;
    }
    
    // Escape - go back
    if (key.escape) {
      if (currentPattern) {
        // Exit pattern config
        setCurrentPattern(null);
        setParamIndex(0);
        setParamValues({});
        setView('patterns');
      } else if (view !== 'menu') {
        setView('menu');
        setSelectedIndex(0);
        setInputBuffer('');
        showStatus('', 'info');
      }
      return;
    }
    
    // Pattern config mode
    if (currentPattern) {
      const params = currentPattern.params || [];
      const isOnRun = paramIndex >= params.length;
      
      if (key.upArrow) {
        setParamIndex(i => Math.max(0, i - 1));
      } else if (key.downArrow) {
        setParamIndex(i => Math.min(params.length, i + 1));
      } else if (key.leftArrow && !isOnRun) {
        const param = params[paramIndex];
        if (param) {
          const newVal = cycleParamValue(param, -1);
          setParamValues(prev => ({ ...prev, [param.key]: newVal }));
        }
      } else if (key.rightArrow && !isOnRun) {
        const param = params[paramIndex];
        if (param) {
          const newVal = cycleParamValue(param, 1);
          setParamValues(prev => ({ ...prev, [param.key]: newVal }));
        }
      } else if (key.return) {
        // Run the pattern!
        actions.runPattern(currentPattern, paramValues);
        setCurrentPattern(null);
        setParamIndex(0);
      }
      return;
    }
    
    // View-specific input
    switch (view) {
      case 'menu': {
        if (key.upArrow) {
          setSelectedIndex(i => {
            let next = Math.max(0, i - 1);
            while (menuItems[next]?.separator && next > 0) next--;
            return next;
          });
        } else if (key.downArrow) {
          setSelectedIndex(i => {
            let next = Math.min(menuItems.length - 1, i + 1);
            while (menuItems[next]?.separator && next < menuItems.length - 1) next++;
            return next;
          });
        } else if (key.return) {
          const item = menuItems[selectedIndex];
          if (item?.view) setView(item.view);
          else if (item?.action) actions[item.action]?.();
        } else {
          const item = menuItems.find(m => m.key?.toLowerCase() === input.toLowerCase());
          if (item?.view) setView(item.view);
          else if (item?.action) actions[item.action]?.();
        }
        break;
      }
      
      case 'pieces': {
        if (key.upArrow) {
          setSelectedIndex(i => Math.max(0, i - 1));
        } else if (key.downArrow) {
          const filtered = inputBuffer ? pieces.filter(p => p.toLowerCase().includes(inputBuffer.toLowerCase())) : pieces;
          setSelectedIndex(i => Math.min(filtered.length - 1, i + 1));
        } else if (key.return) {
          const filtered = inputBuffer ? pieces.filter(p => p.toLowerCase().includes(inputBuffer.toLowerCase())) : pieces;
          const pieceName = filtered[selectedIndex];
          if (pieceName) actions.jumpToPiece(pieceName);
        } else if (key.backspace || key.delete) {
          setInputBuffer(b => b.slice(0, -1));
          setSelectedIndex(0);
        } else if (input && !key.ctrl && !key.meta) {
          setInputBuffer(b => b + input);
          setSelectedIndex(0);
        }
        break;
      }
      
      case 'patterns': {
        const nonSeparators = patterns.filter(p => !p.separator);
        if (key.upArrow) {
          setSelectedIndex(i => {
            let next = Math.max(0, i - 1);
            while (patterns[next]?.separator && next > 0) next--;
            return next;
          });
        } else if (key.downArrow) {
          setSelectedIndex(i => {
            let next = Math.min(patterns.length - 1, i + 1);
            while (patterns[next]?.separator && next < patterns.length - 1) next++;
            return next;
          });
        } else if (key.return) {
          const pattern = patterns[selectedIndex];
          if (pattern && !pattern.separator) {
            if (pattern.params?.length > 0) {
              // Enter config mode
              setCurrentPattern(pattern);
              setParamIndex(0);
              // Initialize with defaults
              const defaults = {};
              for (const p of pattern.params) {
                defaults[p.key] = p.default;
              }
              setParamValues(defaults);
            } else {
              // Run immediately (no params)
              actions.runPattern(pattern, {});
            }
          }
        }
        break;
      }
      
      case 'repl': {
        if (key.return) {
          actions.evalRepl(inputBuffer);
          setInputBuffer('');
        } else if (key.backspace || key.delete) {
          setInputBuffer(b => b.slice(0, -1));
        } else if (input && !key.ctrl && !key.meta) {
          setInputBuffer(b => b + input);
        }
        break;
      }
      
      case 'logs':
        // Read-only, just display
        break;
    }
    
    // Nav shortcuts (always active unless in pattern config)
    const navItem = navItems.find(n => n.key.toLowerCase() === input.toLowerCase());
    if (navItem && view !== navItem.view && !currentPattern) {
      setView(navItem.view);
      setSelectedIndex(0);
      setInputBuffer('');
    }
  });
  
  // ═══════════════════════════════════════════════════════════════
  // RENDER
  // ═══════════════════════════════════════════════════════════════
  
  const mainHeight = Math.max(5, dims.h - 5);
  
  const renderView = () => {
    // Pattern config mode
    if (currentPattern) {
      return h(PatternConfigView, { 
        pattern: currentPattern, 
        paramIndex, 
        paramValues, 
        height: mainHeight,
        width: dims.w 
      });
    }
    
    switch (view) {
      case 'menu':
        return h(MenuView, { items: menuItems, selectedIndex, compact, height: mainHeight });
      case 'logs':
        return h(LogsView, { logs, height: mainHeight, width: dims.w });
      case 'pieces':
        return h(PiecesView, { pieces, selectedIndex, filter: inputBuffer, height: mainHeight });
      case 'patterns':
        return h(PatternsListView, { patterns, selectedIndex, height: mainHeight });
      case 'repl':
        return h(ReplView, { history: replHistory, input: inputBuffer, height: mainHeight });
      default:
        return h(Text, { color: 'gray' }, 'Unknown view');
    }
  };
  
  const getHints = () => {
    if (currentPattern) return '[↑↓]Move [←→]Change [Enter]Run [Esc]Back';
    switch (view) {
      case 'patterns': return '[↑↓]Select [Enter]Config [Esc]Menu';
      case 'pieces': return '[↑↓]Select [Enter]Jump [Type]Filter';
      case 'repl': return '[Enter]Eval [Esc]Menu';
      default: return '[Q]uit [Esc]Back';
    }
  };
  
  return h(Box, { flexDirection: 'column', width: dims.w, height: dims.h },
    h(Header, { connected, serverStatus, cdpStatus, piece, compact }),
    h(NavBar, { items: navItems, view: currentPattern ? 'patterns' : view, compact }),
    renderView(),
    h(Footer, { message: status.msg, messageType: status.type, hints: getHints() })
  );
};

// ═══════════════════════════════════════════════════════════════
// RUN
// ═══════════════════════════════════════════════════════════════

render(h(App));
