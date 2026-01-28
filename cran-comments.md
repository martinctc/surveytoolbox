## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Windows 11 install, R 4.4.x
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)
* R-hub (linux, windows, macos)

## Downstream dependencies

This is a new package with no downstream dependencies.

## Notes

* This is a first submission to CRAN.
* The package includes Windows-specific clipboard functions (`copy_df()`, `read_df()`) 
  that are documented as Windows-only and fail gracefully on other platforms.
