#!/usr/bin/env node
// Generate random documents for benchmarking
import { writeFileSync, mkdirSync, existsSync } from 'fs';
import { join } from 'path';

const WORDS = [
  'lorem', 'ipsum', 'dolor', 'sit', 'amet', 'consectetur', 'adipiscing', 'elit',
  'sed', 'do', 'eiusmod', 'tempor', 'incididunt', 'ut', 'labore', 'et', 'dolore',
  'magna', 'aliqua', 'enim', 'ad', 'minim', 'veniam', 'quis', 'nostrud',
  'exercitation', 'ullamco', 'laboris', 'nisi', 'aliquip', 'ex', 'ea', 'commodo',
  'consequat', 'duis', 'aute', 'irure', 'in', 'reprehenderit', 'voluptate',
  'velit', 'esse', 'cillum', 'fugiat', 'nulla', 'pariatur', 'excepteur', 'sint',
  'occaecat', 'cupidatat', 'non', 'proident', 'sunt', 'culpa', 'qui', 'officia',
  'deserunt', 'mollit', 'anim', 'id', 'est', 'laborum', 'process', 'pool',
  'moonbit', 'parallel', 'execution', 'benchmark', 'performance', 'async',
  'worker', 'thread', 'concurrent', 'task', 'job', 'queue', 'semaphore'
];

function randomWord() {
  return WORDS[Math.floor(Math.random() * WORDS.length)];
}

function generateParagraph(wordCount) {
  const words = [];
  for (let i = 0; i < wordCount; i++) {
    words.push(randomWord());
  }
  return words.join(' ') + '.';
}

function generateDocument(paragraphs, wordsPerParagraph) {
  const lines = [];
  lines.push('# Document\n');
  for (let i = 0; i < paragraphs; i++) {
    if (i % 5 === 0) {
      lines.push(`\n## Section ${Math.floor(i / 5) + 1}\n`);
    }
    lines.push(generateParagraph(wordsPerParagraph));
    lines.push('');
  }
  return lines.join('\n');
}

// Configuration
const NUM_DOCS = parseInt(process.argv[2] || '16', 10);
const PARAGRAPHS_PER_DOC = parseInt(process.argv[3] || '100', 10);
const WORDS_PER_PARAGRAPH = parseInt(process.argv[4] || '50', 10);

const outDir = join(import.meta.dirname, 'docs');
if (!existsSync(outDir)) {
  mkdirSync(outDir, { recursive: true });
}

console.log(`Generating ${NUM_DOCS} documents...`);
console.log(`  ${PARAGRAPHS_PER_DOC} paragraphs per doc`);
console.log(`  ${WORDS_PER_PARAGRAPH} words per paragraph`);
console.log(`  ~${PARAGRAPHS_PER_DOC * WORDS_PER_PARAGRAPH} words per doc`);

for (let i = 0; i < NUM_DOCS; i++) {
  const content = generateDocument(PARAGRAPHS_PER_DOC, WORDS_PER_PARAGRAPH);
  const filename = `doc_${String(i).padStart(3, '0')}.md`;
  writeFileSync(join(outDir, filename), content);
}

console.log(`Generated ${NUM_DOCS} documents in ${outDir}`);
