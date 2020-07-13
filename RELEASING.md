# Releasing

Outline for releasing this package to Hackage and Stackage

## Steps

* Bump version in `package.yaml` in this repository. Create and merge a PR with this change.
* `git tag <version> -m "Release version <version>"`
* `git push origin --tags`
* Build and upload to Hackage:
   * Make sure your workspace is clean
   * `stack sdist`
   * Copy to a folder that can be navigated to for upload
   * Upload here: http://hackage.haskell.org/packages/upload
   * `scripts/hackage-docs.sh <user>`
* Create stackage PR if needed
