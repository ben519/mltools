## Test environments
* local OS X 10.13.4, R 3.5.0
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 notes

* Update from version 0.3.4 to 0.3.5

## Reverse dependencies

None.

---

## Updates

- Added and organize evaluation metrics, mean square error: mse(), mean square logarithmic error: msle(), root mean square error: rmse(), and root mean square logarithmic error: rmsle().  Each metric now includes an option to pass weights and ignore NA values.
- Added unit tests
- Support for multiclass mcc() and user may now provide a confusion matrix as input
- Avoid calling utils::modifyList() on POSIXlt objects