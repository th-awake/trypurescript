#!/usr/bin/env node

// This script expects the current working directory to be `client`.
// Call it using:
//   node updateSharedConfigVersions.mjs src/Try/SharedConfig.purs

import { execSync } from "child_process";
import fs from "fs";
import path from "path";
import process from "process";

if (process.argv.length <= 2) {
  throw new Error("Script was run with 0 args. The first and only arg should be the path to the 'SharedConfig.purs' file.")
}

const sharedConfigPath = process.argv[2];

const spagoYamlPath = path.join("..", "staging", "spago.yaml");
const spagoYamlContent = fs.readFileSync(spagoYamlPath, "utf-8");

// Extract registry package set version from spago.yaml (e.g. "registry: 73.0.0")
const packageSetVersion = spagoYamlContent
  .match(/registry:\s*(\S+)/)?.[1];

if (!packageSetVersion) {
  throw new Error("Failed to extract the package set version from staging/spago.yaml.");
}

// Fetch the PureScript compiler version from the registry package set JSON.
// The package set has a top-level "compiler" field (e.g. "0.15.15").
const packageSetUrl = `https://raw.githubusercontent.com/purescript/registry/main/package-sets/${packageSetVersion}.json`;
const response = await fetch(packageSetUrl);
if (!response.ok) {
  throw new Error(`Failed to fetch package set ${packageSetVersion} from registry: ${response.status}`);
}
const packageSet = await response.json();
const pursVersion = packageSet.compiler;

if (!pursVersion) {
  throw new Error(`Package set ${packageSetVersion} has no "compiler" field.`);
}

// Verify the local purs binary matches the expected version.
let localPursVersion;
try {
  localPursVersion = execSync("purs --version", { encoding: "utf-8" }).trim().split(" ")[0];
} catch {
  throw new Error("Could not run 'purs --version'. Is purs on your PATH?");
}

if (localPursVersion !== pursVersion) {
  throw new Error(
    `Local purs version (${localPursVersion}) does not match the package set's expected compiler version (${pursVersion}).`
  );
}

const sharedConfigContent = fs.readFileSync(sharedConfigPath, "utf-8");
const newContent = sharedConfigContent.split("\n")
  .map((line) => line
    .replace(/pursVersion =.*/, `pursVersion = "v${pursVersion}"`)
    .replace(/packageSetVersion =.*/, `packageSetVersion = "${packageSetVersion}"`)
  )
  .join("\n");
fs.writeFileSync(sharedConfigPath, newContent);
