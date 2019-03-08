# surveytoolbox
A R package containing useful support functions for survey analysis.
Currently under development! (29 Dec 2018)

It is from my own experience of analysing survey data in R that, whilst more often than not you're able to do what you want to do, there is a sense that the 'essential functions' are scattered across different packages and Stack Overflow answers. This is understandable, as the workflow of every analyst is bound to be unique, and it  isn't necessarily sensible to have 100-odd functions collected in a single package for each user - there is bound to be a need to draw functions from multiple packages.  

This is a curated collection of functions that I've either written or come across over the past years. In my analysis workflow, I often use these functions in conjunction with `tidyverse`, `srvyr` (allows you to work with weighted data), janitor. There are also some miscellaneous functions which work with common survey analysis techniques such as factor analysis (using the `factanal()` function from `stats)

The aim of this package is to attempt to organise things a bit and hopefully add value by curation.

---

There are broadly several groups of functions that you can find in this package:
- Creating dummy variables, and variations of these which take inputs from multiple categorical variables 
- Copying data to and from Excel for ad-hoc analysis
- Functions for changing the scale of Likert-scale type questions, including Max-Min Scaling
- Cleaning variable names
- Converting .sav (SPSS) files to smaller, faster-to-load RDS files


---

This package is currently still under development, so it does come with a health advice: if you do wish to use them - have a check and run through the examples before assimilating them into your analysis. 