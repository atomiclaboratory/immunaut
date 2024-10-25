# Resubmission of `immunaut` Package

This resubmission addresses all the comments from the CRAN review:

1. **Title Adjustment**  
   The title has been updated to remove the package name from the start and shortened to meet the 65-character limit.

2. **Examples for Unexported Functions**  
   Removed examples for unexported functions, such as `remove_outliers()`, to comply with CRAN guidelines.

3. **Unnecessary `\dontrun{}` Wrapping**  
   Replaced `\dontrun{}` with `\donttest{}` in all cases where the example can be executed within five seconds.

4. **Console Output**  
   Replaced instances of `print()` and `cat()` with `message()` or `warning()` as appropriate, based on CRAN recommendations.  
   Ensured that functions now generate objects for user extraction, where applicable, rather than printing directly to the console.

5. **Random Seed Setting**  
   Removed fixed random seed settings from functions in `R/utils.R`, `R/functions.R`, and `R/immunaut.R` to avoid unintended reproducibility issues, as per CRAN guidelines. We now recommend users set a seed externally if reproducibility is needed.
