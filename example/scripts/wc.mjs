#!/usr/bin/env node
// Word count script - counts words in a file
import { readFileSync } from 'fs';

const file = process.argv[2];
if (!file) {
  console.error('Usage: wc.mjs <file>');
  process.exit(1);
}

const content = readFileSync(file, 'utf-8');
const words = content.split(/\s+/).filter(w => w.length > 0);
const lines = content.split('\n').length;
const chars = content.length;

// Simulate some CPU work (like text analysis)
let hash = 0;
for (const word of words) {
  for (let i = 0; i < word.length; i++) {
    hash = ((hash << 5) - hash) + word.charCodeAt(i);
    hash = hash & hash;
  }
}

console.log(JSON.stringify({
  file,
  lines,
  words: words.length,
  chars,
  hash
}));
