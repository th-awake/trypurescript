# Release

## Instructions for Redeploying Try PureScript

After making a new compiler release, do the following to redeploy Try PureScript.

1. Submit a PR with the following changes:
    - In `flake.nix`, update the `purescript-src` to point to the new compiler commit or tag.
    - In `stack.yaml`, update the `resolver` and `purescript` source to match.
    - Update the package set (see next section's instructions).
    - Update the shared config by running `cd client && npm run updateConfigVersions` (requires `purs` on your PATH).
    - Update the changelog to include the next release's date.
2. Once the PR is merged, create a new GitHub tagged release using `vYYYY-MM-DD.X` (where `X` is usually `1` or the release attempt) as the version schema. The release will trigger a GitHub Actions build.
3. Wait for the GitHub Actions build to finish (it builds the assets)
4. Run `./deploy/run.sh vX-X-X.1`, replacing `vX-X-X.1` with the version you created.

## Updating the Package Set

The try.purescript.org server only has a limited amount of memory. If the package set we use in deployment is too large, the server will run out of memory.

Before deploying an updated package set, someone (your reviewer) should check that the memory required to hold the package set's externs files does not exceed that of the try.purescript.org server.

Update the package set by doing the following:

### Summary

```sh
cd staging

# Upgrade to the latest registry package set
spago upgrade

# Reinstall all packages from the set as dependencies
spago ls packages | sed -n 's/| \([^ ]*\) .*/\1/p' | grep -v Package | xargs spago install

# Rebuild
spago build

cd ../client
npm run updateConfigVersions
```

### Step-by-Step Explanation

1. Upgrade the package set version in `staging/spago.yaml`:

        ```
        $ cd staging && spago upgrade
        ```

2. Install all packages from the package set as dependencies. The server needs every package listed as a dependency so that `spago sources` includes them all:

        ```
        $ spago ls packages | sed -n 's/| \([^ ]*\) .*/\1/p' | grep -v Package | xargs spago install
        ```

3. Build the staging packages to verify everything compiles:

        ```
        $ spago build
        ```

4. Update the `client/src/Try/SharedConfig.purs` file (requires `purs` on your PATH):

        ```
        $ cd ../client && npm run updateConfigVersions
        ```

    This script:
    - Extracts the package set version from `staging/spago.yaml`
    - Fetches the expected compiler version from the registry package set
    - Verifies the local `purs` binary matches
    - Updates `SharedConfig.purs` with both versions

5. If any packages need NPM dependencies, you can try adding their shims to the import map in `client/public/frame.html`
    - Open up the `generator.jspm.io` URL in the comment
    - Use the 'Add Dependency' search bar to find the NPM dependency
        - If it exists but doesn't exist in that CDN, you can try another one or [open an issue on `jspm/project`](https://github.com/jspm/project#issue-queue-for-the-jspm-cdn)
    - Update the version to the one you need once added
    - If needed, include other files from that dependency
    - Copy and paste the content into the `client/public/frame.html` file
    - Ensure `es-module-shims` has version `1.5.9` or greater.

6. If `es-module-shims` releases a new version, you can calculate its SHA-384 via

        ```console
        $ ESM_VERSION=1.5.5
        $ curl -L -o es-module-shims.js "https://ga.jspm.io/npm:es-module-shims@$ESM_VERSION/dist/es-module-shims.js"
        $ echo "sha384-$(openssl dgst -sha384 -binary es-module-shims.js | openssl base64 -A)"
        $ rm es-module-shims.js
        ```
