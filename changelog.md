1.1.0.2
======

- @mossprescott
    - Re-uploading to clean up an extraneous file in the bundle.

1.1.0.1
======

- @mossprescott
    - Improved usability of generated record types by stripping common prefixes when deriving
      type parameter names from field names.

1.1.0
======

- @mossprescott
    - Added Template Haskell splices to generate polymorphic types and "simple" and "Uninterpolated"
      synonyms: `withUninterpolated`, `withPolymorphic`, and `deriveUninterpolated`.
- Note: this release does not contain any breaking changes, and should have been only a minor
  version bump (i.e. 1.0.1).

1.0.0
=======

- @asariley
    - Removed internal symbols `noEnv`, `varNameAllowed`, `maybeGen` from `Data.Interpolation`
    - Added `instance Arbitrary1 Uninterpolated`

0.1.2
=======

- @asariley
    - Updated `TemplateKey` arbitrary instance to only include alpha-numeric and underscore characters.
