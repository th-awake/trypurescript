#!/usr/bin/env node

// This script expects the current working directory to be `client`.
// Call it using:
//   node updateSharedConfigVersions.mjs src/Try/SharedConfig.purs

import fs from "fs";
import path from "path";
import process from "process";

if (process.argv.length <= 2) {
  throw new Error("Script was run with 0 args. The first and only arg should be the path to the 'SharedConfig.purs' file.")
}

const sharedConfigPath = process.argv[2];

const stackYamlPath = path.join("..", "stack.yaml");
const stagingPackagesDhallPath = path.join("..", "staging", "packages.dhall");
const stackYamlContent = fs.readFileSync(stackYamlPath, "utf-8");
const packagesContent = fs.readFileSync(stagingPackagesDhallPath, "utf-8");

// Support both `- purescript-X.Y.Z` (Hackage) and GitHub commit format.
// For GitHub commits, read the version from the purescript.cabal at that commit.
// As a fallback, extract it from the staging/packages.dhall metadata version.
const pursVersion = stackYamlContent.split("\n")
  .reduce((acc, nextLine) => {
    if (acc.found) return acc;
    const matchResult = nextLine.match(/ +- purescript-(.+)/);
    return matchResult
      ? { found: true, value: matchResult[1] }
      : acc;
  }, { found: false })
  .value
  // Fallback: extract from staging/packages.dhall metadata version
  ?? packagesContent.match(/metadata\.version\s*=\s*"v([^"]+)"/)?.[1];

const packageSetVersion = packagesContent
  .match(/https:\/\/github.com\/purescript\/package-sets\/releases\/download\/psc-([^\/]+)\/packages.dhall/)[1];

if (!pursVersion) {
  throw new Error("Failed to extract the PureScript version from the stack.yaml file. Cannot update SharedConfig.purs file.");
}

if (!packageSetVersion) {
  throw new Error("Failed to extract the Package Set version from the staging/packages.dhall file. Cannot update SharedConfig.purs file.");
}

const sharedConfigContent = fs.readFileSync(sharedConfigPath, "utf-8");
const newContent = sharedConfigContent.split("\n")
  .map((line) => line
    .replace(/pursVersion =.*/, `pursVersion = "v${pursVersion}"`)
    .replace(/packageSetVersion =.*/, `packageSetVersion = "${packageSetVersion}"`)
  )
  .join("\n");
fs.writeFileSync(sharedConfigPath, newContent);
