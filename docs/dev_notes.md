# Epiweek
In 2026 the first week of June became week 22 not 23. This occurred because 2025 had 53 epi weeks instead of 52. 
Made new function calc_season_week



## Version v2

V2 config file now run separately from qmd pipeline file. This allows all
options to be set once. Then user can run pipeline and make any adjustments
without having to specify the commands each time.\
\
V2 also now runs code as functions instead of scripts. This allows for more
flexibility and reusability of code. For example wnv-s_clean() is designed as a
universal cleaning function to ensure all data follows the same formatting
rules.
