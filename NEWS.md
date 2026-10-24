# RCPA4 1.0.0

Initial release of the companion R package for Barry C. Edwards and Philip H.
Pollock III, *An R Companion to Political Analysis, Fourth Edition*.

## Datasets

* Added six classroom-ready datasets used throughout the fourth edition:
  `anes`, `gss`, `states`, `world`, `debate`, and `students`.
* Replaced the earlier `nes` teaching dataset with a curated subset of the 2024
  American National Election Study, including documented recoded variables for
  textbook examples.
* Updated the General Social Survey, state, and world datasets with current
  observations and expanded political, social, demographic, and economic
  measures.
* Established maintained metadata sources that generate package data objects,
  variable documentation, and corresponding textbook appendix material.

## Functions

* Updated the package's teaching-oriented functions for descriptive statistics,
  data transformation, graphics, confidence intervals, hypothesis tests,
  comparisons, cross-tabulation, correlation, and regression.
* Improved support for optional sampling weights, `data` arguments, missing
  values, input validation, and consistent table and plot behavior.
* Added documented, reusable return values to the principal analysis functions.
  Regression functions retain standard model behavior, while auxiliary result
  tables are available through stable names.
* Improved handling of ordered factors and numeric-looking factor labels so that
  calculations use the displayed values where appropriate rather than internal
  factor codes.
* Expanded `getC()` support for common statistical and delimited data formats,
  compressed files, URLs, and native R files containing one or more objects.
* Redesigned `helpC()` to provide interactive access to function help, examples,
  datasets, variable metadata, plotting references, package settings, and
  chapter resources through R's configured help system.
* Preserved familiar fourth-edition classroom workflows while improving error
  messages, P-value formatting, graphics-state restoration, and cross-platform
  plotting behavior.

## Documentation and Testing

* Revised function documentation, argument descriptions, return-value details,
  and examples to agree with current behavior and use the fourth-edition
  datasets, including `anes` in place of `nes`.
* Updated textbook and online-resource references for the fourth edition.
* Added automated tests for core analysis paths, weighted and unweighted
  results, return structures, importing, interactive help, plotting behavior,
  dataset construction, and regression fixes.
* Added a separate checking harness for R code examples used in the textbook so
  package behavior and book instruction can be reviewed together.


