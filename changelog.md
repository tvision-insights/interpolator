1.1.0
======

- @mossprescott
    - Add Template Haskell splices to generate polymorphic types and "simple" and "Uninterpolated"
      synonyms: `withUninterpolated`, `withPolymorphic`, and `deriveUninterpolated`.


1.0.0
=======

- @asariley
    - Removed internal symbols `noEnv`, `varNameAllowed`, `maybeGen` from `Data.Interpolation`
    - Added `instance Arbitrary1 Uninterpolated`

0.1.2
=======

- @asariley
    - Updated `TemplateKey` arbitrary instance to only include alpha-numeric and underscore characters.
