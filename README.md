# GarrettRank

<!-- badges: start -->
<!-- badges: end -->

## Overview

**GarrettRank** provides functions for conducting Garrett ranking analysis. The package is designed to calculate Garrett scores from respondent-level ranking data and provide summarized ranking results in a convenient format.

The package also provides functions for generating tables, summaries, visualizations, and Kendall's coefficient of concordance for assessing agreement among respondents. Visualizations are built using `ggplot2` and `pheatmap`.

## Installation

The package can be installed using:

```r
install.packages("GarrettRank")
```

Then load the package:

```r
library(GarrettRank)
```

## Functions

The main functions provided by GarrettRank are:

* `garrett_rank()` — performs Garrett ranking analysis.
* `garrett_table()` — produces the standard Garrett ranking/percent position conversion table used in the scoring calculation.
* `kendall_w()` — calculates Kendall's coefficient of concordance.

The package also provides S3 methods for:

* `summary()`
* `plot()`


## Example Dataset

The package includes an example dataset called `garrett_example`.

The dataset contains:

* 50 respondents
* 22 constraints (`C1` to `C22`)
* respondent names or serial numbers in the first column
* ranks assigned to each constraint
* no tied ranks

The dataset can be loaded using:

```r
data("garrett_example")
head(garrett_example)
```

## Basic Example

```r
library(GarrettRank)
data("garrett_example")

# Perform Garrett ranking
result <- garrett_rank(garrett_example, respondent = "Respondent")

# View the result
result

# Summary
summary(result)

# Plot the results
plot(result)
```

`data` accepts a data.frame, matrix, CSV file path, or Excel file path. Each row should represent one respondent and each column one factor/constraint, with every respondent assigning each rank from 1 to the number of factors exactly once. `respondent` specifies the column name or position containing respondent IDs, so it can be excluded before analysis.

### Plot types

`plot()` supports several `type` options, covering both item-level ranking summaries and respondent-level agreement patterns:

| `type`        | Description                                                        |
|---------------|----------------------------------------------------------------------|
| *(default)*   | Default summary plot of Garrett scores by item.                     |
| `"bar"`       | Bar chart of Garrett scores by item.                                 |
| `"lollipop"`  | Lollipop chart of Garrett scores by item.                            |
| `"dot"`       | Dot plot of Garrett scores by item.                                  |
| `"line"`      | Line plot of Garrett scores by item.                                 |
| `"heatmap"`   | Ranks × item score heatmap.                                     |
| `"cluster"`   | Hierarchical clustering of respondents by ranking pattern.           |
| `"contribution"` | Contribution of each item to the overall Garrett score.          |

Additional arguments for `type = "cluster"`:

* `k` — number of clusters to highlight (e.g., `k = 3`).
* `scale` — scaling applied before clustering (e.g., `scale = "row"`).
* `show_numbers` — whether to display values on the cluster heatmap.

```r
plot(result, type = "bar")
plot(result, type = "lollipop")
plot(result, type = "dot")
plot(result, type = "line")
plot(result, type = "heatmap")
plot(result, type = "cluster")
plot(result, type = "cluster", k = 3)
plot(result, type = "cluster", scale = "row", show_numbers = TRUE)
plot(result, type = "contribution")

```

## Kendall's Coefficient of Concordance

Kendall's coefficient of concordance can be calculated using:

```r
kendall_result <- kendall_w(garrett_example, respondent = "Respondent")
kendall_result
summary(kendall_result)
```

## Citation

If you use GarrettRank in your research, please cite the package as:

> Varghese, B. B., et al. GarrettRank: Garrett Ranking Analysis in R.

A formal citation will be added once the package is published. Once available, it can be retrieved with:

```r
citation("GarrettRank")
```


## Authors

* Blesson B. Varghese
* Adarsh V. S.
* Bhavana Sajeev
* Azhanuo Rutsa
* Joe Shiney M. A.

## License

This package is licensed under the GPL-3 license.
