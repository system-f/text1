0.0.7.4

* Add operators for appending `Text`

0.0.7.3

* Add operators for `cons1` and `snoc1`

0.0.7.2

* Fix to export `ManyText1`

0.0.7.1

* More type-class instances
* More type parameters to `AsSingle`

0.0.7.0

* Significant refactor

0.0.6

* Remove dependency on papa
* Support GHC versions back to GHC 7.4

0.0.4

* Delete `instance Cons Text1 Text1 Char Char`. Issue #3
* Delete `instance Snoc Text1 Text1 Char Char`. Issue #3
* Introduce `instance Cons (Maybe Text1) (Maybe Text1) Char Char`.
* Introduce `instance Snoc (Maybe Text1) (Maybe Text1) Char Char`.
* `instance IsText (Maybe Text1)`
* `isText1` :: Iso' Text (Maybe Text1)`

0.0.3

* Tests.
* `tpacked1` and `tunpacked1`.
* `AsText1` type-class and instances.
* Fix bug in `compareLength`.
* Fix bug in `last1`.

0.0.2

* Fix issue #1 in `init1`
* Rename functions to have 1 suffix.
* Implement `unsnoc`.
* `_init1` and `_tail1` lenses.
* Remove functions in favour of type-class implementations.
* Reversing instance.
* AsSingle type-class and instances.
* OneAnd type-class and instances.

0.0.1

* Initial version.
