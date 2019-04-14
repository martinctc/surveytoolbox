# surveytoolbox
A R package containing useful support functions for survey analysis.
Currently under development! 
(First created 29 Dec 2018)


<img src="icons/surveytoolbox_hex.svg?sanitize=true" align="right" height = 150 width = 150/>

------------------------------------------------------------------------

### About this package

It is from my own experience of analysing survey data in R that whilst more often than not you're able to do what you want to do, there is a sense that the 'essential functions' are scattered across different packages and Stack Overflow answers. This is understandable, as the workflow of every analyst is bound to be unique, and it  isn't necessarily sensible to have 100-odd functions collected in a single package for each user - there is bound to be a need to draw functions from multiple packages.  

This is a curated collection of functions that I've either written or come across over the past years. In my analysis workflow, I often use these functions in conjunction with `tidyverse`, `srvyr` (allows you to work with weighted data), and `janitor`. There are also some miscellaneous functions which work with common survey analysis techniques such as factor analysis (using the `factanal()` function from `stats)

**The aim of this package is to attempt to organise things a bit and add value by curation.**

---

### Summary of Functions

There are broadly several groups of functions that you can find in this package:
- Creating dummy variables, and variations of these which take inputs from multiple categorical variables (`superspread()`)
- Copying data to and from Excel for ad-hoc analysis (`copy_df()`)
- Functions for changing the scale of Likert-scale type questions, including Max-Min Scaling (`likert_reverse`)
- Cleaning variable names
- Converting .sav (SPSS) files to smaller, faster-to-load RDS files (e.g. `sav_to_rds()`)
- Converting one or more categorical variable(s) into other variable types, such as dummy variables (binary), count variables (numeric), or "fill" variables (fills values from a required column if condition is TRUE)
- Functions for recoding variable and value labels (e.g. `recode_vallab()`, `set_varl()`, `set_vall()`)

---

### Installation

surveytoolbox is not released on CRAN (yet). 
You can install the latest development version from GitHub with:

```
install.packages("devtools")
devtools::install_github("martinctc/surveytoolbox")
```

---

### News and Updates

This package is currently still under development, so it does come with a health advice: if you do wish to use them - have a check and run through the examples before assimilating them into your analysis. 

Note: Previously named 'surveytools', but now renamed to 'surveytoolbox' avoid confusion with another similar package of the same name (8th March 2019).

### Contact me
---
Please feel free to submit suggestions and report bugs: <https://github.com/martinctc/surveytoolbox/issues>
Also check out my [website](https://martinctc.github.io)
