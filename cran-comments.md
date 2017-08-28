## Test environments
* local OS X 10.12.6, R 3.4.1
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 notes

* Update from version 0.3.1 to 0.3.2

## Reverse dependencies

None.

---

## Updates

- In `folds(x, ...)` `x` can now be a positive integer specifying the number of fold IDs to generate
- In `date_factor(dateVec, ...)` the argument `fullyears` has been dropped and replaced by the more flexible pair of arguments `minDate` and `maxDate` for determining resulting vector levels.  Additionally a bug regarding `type=yearquarters` has been fixed.
- In `bin_data()` added some input validation