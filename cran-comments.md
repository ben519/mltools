## Test environments
* local macOS Big Sur 11.6, R 4.1.1
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 notes

* Update from version 0.3.4 to 0.3.5

## Reverse dependencies

None.

---

## Updates

- Added `drop = FALSE` parameter where needed inside `sparsify()` to force inputted data.table with 1 row to be returned as a sparse matrix
- Fixed bug where `sparsify(dt, sparsifyNAs = FALSE)` would error if dt had a factor column of all NAs (sometimes)
- Added parameters `sparsifyCols` and `memEfficient` to `sparsify()`. `sparsifyCols` let's the user choose a subset of the inputted table's columns to be sparsified and `memEfficient = TRUE` will run a new, memory efficient version of sparsify().
- Added parameter `alpha` to `rmsle()` and `msle()` functions to specify a custom offset instead of the deault (1).
- Added parameter `roundbins` to `bin_data()` to allow bin values to be rounded to a specified decimal place
- Fixed bug in `empirical_cdf()` which occurred when a boundary exactly matched multiple target records