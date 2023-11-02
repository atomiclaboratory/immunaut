immunaut
================
Ivan Tomic <info@ivantomic.com>

<!-- README.md is generated from README.Rmd. Please edit that file -->

R package designed to facilitate machine learning analysis with
multi-dimensional datasets. The codebase provides functions to perform
dimensionality reduction, hierarchical clustering, and predictive
modeling. Within this framework, users can use high-dimensional data,
perform t-distributed stochastic neighbor embedding (t-SNE) for
effective visualization and exploration, and then carry out hierarchical
clustering to identify latent data structures. This analysis chain can
then be extended by further appending the data with cluster results,
allowing for the construction of machine learning models with specific
outcomes. The package is well-suited for exploratory data analysis,
enabling users to uncover patterns, dynamics, and structures within
their complex datasets.

## Installation

You can install the released version of **immunaut** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("immunaut")
```

Or you can install **immunaut** directly from **github** with use of
following commands:

``` r
install.packages("devtools")
devtools::install_github("atomiclaboratory/immunaut", subdir = 'R-package')
```

## Usage Example

``` r
library("immunaut")
## Using provided demo data
data(immunautDemo)
## results <- immunaut(immunautDemo, settings)
```

## Pros and Cons

While this package offers comprehensive functionality, we believe it’s
vital for users to be aware of its strengths and weaknesses.

### Pros:

- Comprehensive workflow: The R package provides an all-in-one solution
  for dimensionality reduction, clustering, and predictive modelling,
  eliminating the need to piece together separate tools.

- Flexible use: This tool has been designed to accommodate a variety of
  high-dimensional dataset types, making it ideal for a broad range of
  applications.

- Improved data understanding: Utilising t-SNE, hierarchical clustering,
  and machine learning within a single pipeline allows for a
  sophisticated and in-depth understanding of a dataset. This is
  particularly useful in identifying patterns and relationships that
  might not be apparent through individual analyses.

- Customizable: With the availability of source code, this package
  allows users to customize and modify its functionality according to
  their specific research needs.

### Cons:

- Required knowledge: A certain level of familiarity with R and the
  respective statistical techniques (t-SNE, clustering, machine
  learning) is necessary to effectively use this package.

- Performance limitations: As with any tool, the performance of the
  package is limited by computational resources. Tasks such as t-SNE and
  machine learning modelling can be resource-intensive.

- Interpretation challenges: The result interpretation particularly from
  the t-SNE and hierarchical clustering may be complex and sometimes
  subjective.

- Possible overfitting: While predictive modelling can be a powerful
  tool for understanding complex datasets, care must be taken to avoid
  overfitting, especially if the number of features is relatively high
  compared to the number of samples.

## License

**immunaut** source-code uses the MIT LICENCE, see our **LICENSE** file.

## Acknowledgements

We are sincerely grateful to our users and contributors who help us
improve the tool and help it grow. Enjoy your data analysis journey with
the **immunaut** package!

## Citation
