#!/usr/bin/env bash
# Run once the holoros gh token is available (connect CRSF-Cowork folder, or
# paste token: echo $TOKEN | gh auth login --with-token).
set -e
gh auth status
gh repo fork USDAForestService/ForestVegetationSimulator-Interface --org holoros --clone=false 2>/dev/null || true
git remote add holoros https://github.com/holoros/ForestVegetationSimulator-Interface.git 2>/dev/null || true
git push -u holoros koa-stress-test
echo "Open a PR from holoros:koa-stress-test back to MidgardNaturalResources:main when ready."
