
<!-- README.md is generated from README.Rmd. Please edit that file -->

# data.fakeR: Generate Fake Data in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/ChronoScrat/data-fakeR/workflows/R-CMD-check/badge.svg)](https://github.com/ChronoScrat/data-fakeR/actions)
[![dev-version](https://img.shields.io/badge/dev--version-0.1.0-blue)](https://github.com/ChronoScrat/data-fakeR/releases/tag/0.1.0)

<!-- badges: end -->

data.fakeR is an R library that helps you generate fake datasets in any
format from a given schema. It is an R version of [dunnhumby’s
data-faker](https://github.com/dunnhumby/data-fake), which is originally
built with Scala.

This package is still in its initial release, so it may change over
time.

## Installation

To install data.fakeR, run:

``` r
if(!require(devtools)) install.packages(devtools)
devtools::install_github("ChronoScrat/data-fakeR")
```

## Usage

To use data.fakeR, first you must create a YAML file with the schema for
your tables, and then call `import_schema` to import the list into R.

### Schema example

``` yaml
tables:
    - name: students
      rows: 500
      columns:
      - name: student_id
        data_type: Integer
        column_type: Sequential
        start: 1
        step: 1
      - name: birthday
        data_type: Date
        column_type: Random
        min: 2000-01-01
        max: 2016-12-31
    
    - name: schools
      rows: 10
      columns:
      - name: school_id
        data_type: Integer
        column_type: Sequential
        start: 1
        step: 1
      - name: school_code
        data_type: Character
        column_type: Expression
        expression: paste0("SCHL",school_id)
```

That schema should create two tables: one named `students`, with 500
rows and two columns; and one named `schools`, with 10 rows and two
columns. Both tables will be returned inside a list by the function
`create_tables`.

### Schema Configuration

#### Column Types

-   Fixed:

Supported data types: `Character`, `Integer`, `Double`, `Date`,
`Timestamp` and `Logical`

Required fields:

`value` - The column value

-   Random:

Supported data types: `Integer`, `Double`, `Date`, `Timestamp` and
`Logical`

Required fields:

`min` - The minimum bound of the random data

`max` - The maximum bound of the random data

-   Selection:

Supported data types: `Character`, `Integer`, `Double`, `Date` and
`Timestamp`

Required fields:

`options` - The options from which to select, separated by **comma**
(without a whitespace between values)

`prob` - (Optional) The probabilities for each option, separated by
**comma** (without a whitespace between values.) If provided, the number
of probabilities *must* be equal to the number of options. If not
provided, all options will have the same probability to be chosen.

-   Sequential:

Supported data types: `Integer`, `Double`, `Date` and `Timestamp`

Required fields:

`start` - The starting value `step` - The increment for each row

-   Logical:

Supported data types: `Logical`

Fields:

`prob` - (Optional) The probabilities for both FALSE and TRUE (in that
order), separated by a **comma**. If not provided, both options will
have the same probability of being chosen.

-   Expression:

Supported data types: `Character` (Optional)

Required fields:

`expression` - The R expression to be evaluated.

Obs: Operations are performed row-wise. The `dplyr` package is used to
perform the expression:

``` r
table <- table |>
  dplyr::mutate( {COLUMN_NAME} = {EXPRESSION} )
```

As as consequence, if-else operation must be vectorised, and preferably
be called by the `ifelse()` function from the dplyr package. Very
complex operations may fail.

## Contributing

If you want to contribute to this package, please read the [contribution
guide](https://github.com/ChronoScrat/data-fakeR/blob/main/.github/CONTRIBUTING.md).
Also read the [TODO
list](https://github.com/ChronoScrat/data-fakeR/blob/main/.github/TODO.md)
with planned addition to the package.
