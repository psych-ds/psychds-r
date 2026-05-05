'use strict';

const path = require('path');
const { run } = require(path.join(__dirname, 'node_modules', 'psychds-validator'));

// Pass args directly via process.argv (skip 'node' and script name)
const args = process.argv.slice(2);

run(args).catch(err => {
  console.error("An error occurred:", err);
  process.exit(1);
});