## Test environments
* local OS X 10.12.2, R 3.3.2
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 1 note

* Update from version 0.1.0 to 0.2.0

## Reverse dependencies

None.

---

## Updates

- Added `date_factor()` for converting dates to a grouped ordered factor (e.g. months, yearmonths, yearquarters, etc.)
- Implemented stratification for numeric `x` in `folds(x, stratified=TRUE, ...)`
