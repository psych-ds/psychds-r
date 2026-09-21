#!/usr/bin/env bash
# tools/build-validator.sh
# ----------------------------------------------------------------------------
# Build the single-file Node validator that ships in inst/node/, replacing the
# vendored node_modules tree. Run from the package root whenever the pinned
# psychds-validator version is bumped. Requires Node >= 18 and npm.
#
# Empirically verified (Sept 2026, psychds-validator 1.5.1):
#   - CJS output fails (top-level await in a dependency) -> ESM is required
#   - ESM needs the require/__dirname banner shims below
#   - loadSchema() reads defaultSchema.json + defaultSchemaOrg.json from the
#     script's own directory, falling back to a network fetch; shipping both
#     JSONs next to the bundle makes validation fully offline
# ----------------------------------------------------------------------------
set -euo pipefail

VALIDATOR_VERSION="1.5.1"   # pin; bump deliberately
OUT_DIR="inst/node"
WORK_DIR="$(mktemp -d)"

cd "$WORK_DIR"
npm init -y > /dev/null
npm install "psychds-validator@${VALIDATOR_VERSION}" esbuild > /dev/null

npx esbuild node_modules/psychds-validator/cli.js \
  --bundle --platform=node --target=node18 --format=esm --minify \
  --legal-comments=inline \
  --banner:js="import { createRequire } from 'module'; const require = createRequire(import.meta.url); import { fileURLToPath as __flp } from 'url'; import { dirname as __dnm } from 'path'; const __filename = __flp(import.meta.url); const __dirname = __dnm(__filename);" \
  --outfile=validate.bundle.mjs

cd - > /dev/null
mkdir -p "$OUT_DIR"
cp "$WORK_DIR/validate.bundle.mjs" "$OUT_DIR/"
cp "$WORK_DIR/node_modules/psychds-validator/script/src/setup/defaultSchema.json" "$OUT_DIR/"
cp "$WORK_DIR/node_modules/psychds-validator/script/src/setup/defaultSchemaOrg.json" "$OUT_DIR/"
rm -rf "$WORK_DIR"

echo "Built:"
ls -la "$OUT_DIR"
echo
echo "Acceptance test (run from any directory OTHER than the package root):"
echo "  node $OUT_DIR/validate.bundle.mjs /path/to/known-valid-dataset"
echo "  node $OUT_DIR/validate.bundle.mjs /path/to/known-broken-dataset"
echo "  # and once with networking blocked, e.g.:"
echo "  HTTPS_PROXY=http://127.0.0.1:9 node $OUT_DIR/validate.bundle.mjs /path/to/known-valid-dataset"

