# Contributing to data.fakeR

Thank you for your interest in contributing to data.fakeR! If you wish to get involved, please
read this document.

## Improvements

The best way to contribute to this project is to improve its code. As harsh as it may be to admit, data.fakeR
was hastly written, and may contain many inefficient lines of code. If you wish to improve the code's usability and/or performance, please feel free to open a [pull request](https://github.com/ChronoScrat/data-fakeR/pulls).

If you wish to contribute with an improvement but do not know where to begin, please have a look at the [TODO](https://github.com/ChronoScrat/data-fakeR/blob/main/.github/TODO.md) list.

**Note:** before you submit a pull request, please make sure you have run roxygenize to generate all necessary documentation and that your code passes all tests and checks.

```r
roxygen2::roxygenise()

devtools::test()

devtools::check()
```

If you wish to change the README file, please make all changes to [README.Rmd](https://github.com/ChronoScrat/data-fakeR/blob/main/README.Rmd) and then run `devtools::build_readme()`.

## New Features

If you wish to propose a new feature to the package, but does not want to open a pull request, please open an [issue](https://github.com/ChronoScrat/data-fakeR/issues) for a new feature.

## Bug Report

If you wish to report a bug with this project, please open an [issue](https://github.com/ChronoScrat/data-fakeR/issues) detailing the difficulties you have found.

<br>

# Thank you :heart: