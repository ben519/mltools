## Test environments
* local OS X 10.12.6, R 3.4.2
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 notes

* Update from version 0.3.3 to 0.3.4

## Reverse dependencies

None.

---

## Updates

- Added/organized evaluation metrics, mean square error: mse(), mean square logarithmic error: msle(), root mean square error: rmse(), and root mean square logarithmic error: rmsle().  Each metric now includes an option to pass weights and ignore NA values.
- Added unit tests