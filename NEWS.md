# surveytoolbox 0.1.0

* Initial CRAN release
* Core functions for survey data manipulation:
  - `superspread()`, `superspread_count()`, `superspread_fill()` for creating dummy variables
  - `data_dict()` for creating data dictionaries from labelled data
  - `look_up()` for lookup table operations
  - `apply_row()` for rowwise operations with dplyr syntax
  - `box_it()` for creating top/bottom box variables
  - `as_nps()` and `as_nps_cat()` for NPS calculations
  - `test_chisq()` and `ttest_nps()` for statistical tests
  - Label manipulation functions: `set_varl()`, `set_vall()`, `recode_vallab()`, `extract_vallab()`
  - Data conversion functions: `char_to_lab()`, `lab_to_char()`, `chr_to_var()`
  - Utility functions: `clean_strings()`, `wrap_text()`, `timed_fn()`, `categorise()`
  - File operations: `sav_to_rds()`, `copy_df()`, `read_df()`
  - Scale transformations: `likert_convert()`, `likert_reverse()`, `maxmin()`
