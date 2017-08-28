# News

## mltools 0.3.9000

- In `folds(x, ...)` `x` can now be a positive integer specifying the number of fold IDs to generate
- In `date_factor(dateVec, ...)` the argument `fullyears` has been dropped and replaced by the more flexible pair of arguments `minDate` and `maxDate` for determining resulting vector levels.  Additionally a bug regarding `type=yearquarters` has been fixed.
- In `bin_data()` added some input validation

## mltools 0.3.1

- Fixed bug in `bin_data()` occuring when `x` is integery type and `bins` includes `Inf` or `-Inf`

## mltools 0.3.0

- Fixed bug in `date_factor()` occuring when type = "yearquarter" and fullyears = FALSE
- Fixed bugs involving unsorted or duplicate ubounds in `empirical_cdf()`

## mltools 0.2.0

- Added `date_factor()` for converting dates to a grouped ordered factor (e.g. months, yearmonths, yearquarters, etc.)
- Implemented stratification for numeric `x` in `folds(x, stratified=TRUE, ...)`

## mltools 0.1.0

Initial Release
