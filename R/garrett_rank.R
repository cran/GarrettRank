#' Garrett Conversion Table
#' It is a standard Garrett conversion table used by the package.
#'
#' @return A data.frame with two columns:
#' \describe{
#'   \item{Percent_Position}{The percent position values used to determine
#'   Garrett scores from respondent ranks.}
#'   \item{Garrett_Score}{The corresponding Garrett scores assigned to the
#'   percent positions.}
#' }
#' @examples
#' \donttest{
#' table<-garrett_table()
#' print(table)
#' }
#' @export
garrett_table <- function() {
  percent_position <- c(
    0.09, 0.20, 0.32, 0.45, 0.61, 0.78, 0.97, 1.18, 1.42, 1.68,
    1.96, 2.28, 2.69, 3.01, 3.43, 3.89, 4.38, 4.92, 5.51, 6.14,
    6.81, 7.55, 8.33, 9.17, 10.06, 11.03, 12.04, 13.11, 14.25,
    15.44, 16.69, 18.01, 19.39, 20.93, 22.32, 23.88, 25.48,
    27.15, 28.86, 30.61, 32.42, 34.25, 36.15, 38.06, 40.01,
    41.97, 43.97, 45.97, 47.98, 50.00, 52.02, 54.03, 56.03,
    58.03, 59.99, 61.94, 63.85, 65.75, 67.48, 69.39, 71.14,
    72.85, 74.52, 76.12, 77.68, 79.17, 80.61, 81.99, 83.31,
    84.56, 85.75, 86.89, 87.96, 88.97, 89.94, 90.83, 91.67,
    92.45, 93.19, 93.86, 94.49, 95.08, 95.62, 96.11, 96.57,
    96.99, 97.37, 97.72, 98.04, 98.32, 98.58, 98.82, 99.03,
    99.22, 99.39, 99.55, 99.68, 99.80, 99.91, 100
  )

  garrett_score <- 99:0

  data.frame(
    Percent_Position = percent_position,
    Garrett_Score = garrett_score
  )
}


#' Validate Garrett Ranking Data
#'
#' Validates respondent ranking data before Garrett ranking analysis.
#' The function checks the input data structure, ranking values, and
#' completeness of ranks for each respondent. Supported input formats
#' include data frames, matrices, CSV files, and Excel files.
#'
#' @param data A data.frame, matrix, CSV file path, or Excel file path
#'   containing respondent ranking data. Each row represents a respondent
#'   and each column represents a factor or item being ranked.
#' @param respondent Optional respondent ID column name or column position
#'   to exclude from the ranking data before validation.
#'
#' @return A validated data frame containing the ranking data after
#'   checking that the data have valid dimensions, numeric integer ranks,
#'   and that each respondent assigns every rank exactly once.
#'
#' @keywords internal
.validate_ranking_data <- function(data, respondent = NULL) {

  if (is.character(data)) {
    if (length(data) != 1L || is.na(data) || !nzchar(data)) {
      stop("'data' must be a single file path.", call. = FALSE)
    }

    if (!file.exists(data)) {
      stop("The specified file does not exist.", call. = FALSE)
    }

    if (grepl("\\.(xlsx|xls)$", data, ignore.case = TRUE)) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop(
          "Package 'readxl' is required to read Excel files. ",
          "Install it with install.packages('readxl').",
          call. = FALSE
        )
      }
      data <- readxl::read_excel(data)
      data <- as.data.frame(data, check.names = FALSE)
    } else if (grepl("\\.csv$", data, ignore.case = TRUE)) {
      data <- utils::read.csv(
        data,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    } else {
      stop(
        "Unsupported file type. Use a data.frame or a .csv, .xls, or .xlsx file.",
        call. = FALSE
      )
    }
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data, check.names = FALSE)
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, matrix, or supported file path.",
         call. = FALSE)
  }

  if (!is.null(respondent)) {
    if (is.character(respondent)) {
      if (length(respondent) < 1L ||
          anyNA(respondent) ||
          !all(respondent %in% names(data))) {
        stop(
          "Character 'respondent' must contain existing column name(s).",
          call. = FALSE
        )
      }
      data <- data[, !(names(data) %in% respondent), drop = FALSE]
    } else if (is.numeric(respondent)) {
      if (length(respondent) < 1L ||
          anyNA(respondent) ||
          any(respondent < 1) ||
          any(respondent > ncol(data)) ||
          any(respondent != as.integer(respondent))) {
        stop(
          "Numeric 'respondent' must contain valid column position(s).",
          call. = FALSE
        )
      }
      data <- data[, -as.integer(respondent), drop = FALSE]
    } else {
      stop("'respondent' must be NULL, character, or numeric.",
           call. = FALSE)
    }
  }

  if (ncol(data) < 2L) {
    stop("At least two ranking factors are required.", call. = FALSE)
  }

  if (nrow(data) < 2L) {
    stop("At least two respondents are required.", call. = FALSE)
  }

  if (!all(vapply(data, is.numeric, logical(1)))) {
    stop("All ranking columns must be numeric.", call. = FALSE)
  }

  mat <- as.matrix(data)

  if (anyNA(mat)) {
    stop("Missing values are not allowed in ranking data.", call. = FALSE)
  }

  if (any(!is.finite(mat))) {
    stop("Ranking data must contain only finite values.", call. = FALSE)
  }

  if (any(mat != floor(mat))) {
    stop("All ranking values must be integers.", call. = FALSE)
  }

  n_factors <- ncol(mat)

  if (any(mat < 1 | mat > n_factors)) {
    stop(
      sprintf(
        "Ranking values must be integers from 1 to %d.",
        n_factors
      ),
      call. = FALSE
    )
  }

  valid_rows <- apply(
    mat,
    1L,
    function(z) all(sort(z) == seq_len(n_factors))
  )

  if (!all(valid_rows)) {
    bad <- which(!valid_rows)
    stop(
      paste0(
        "Each respondent must assign every rank from 1 to ",
        n_factors,
        " exactly once. Invalid respondent row(s): ",
        paste(bad, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  data
}


#' Garrett Ranking Analysis
#'
#' Performs Garrett ranking analysis on respondent ranking data.
#'
#' Each row must represent one respondent and each column one factor,
#' constraint, or item. Each respondent must assign every rank from 1 to
#' the number of factors exactly once.
#'
#' @param data A data.frame, matrix, CSV file path or Excel file path
#'   containing ranking data.
#' @param respondent Optional respondent ID column name or column position
#'   to exclude before analysis.
#' @return An object of class \code{"garrett"}, which is a list containing:
#' \describe{
#'   \item{ranking}{A data.frame containing the factor names, mean Garrett
#'   scores, and final ranks. \code{Mean_Score} is the average Garrett score
#'   for each factor across all respondents. Higher mean Garrett scores
#'   indicate higher overall priority, and \code{Rank = 1} represents the
#'   highest-ranked factor.}
#'   \item{frequency}{A data.frame containing the number of respondents
#'   assigning each possible rank to each factor, together with the
#'   corresponding percent positions and Garrett scores.}
#'   \item{weighted_scores}{A numeric matrix containing the rank frequencies
#'   multiplied by their corresponding Garrett scores. These values are used
#'   to calculate the mean Garrett score for each factor.}
#'   \item{reference}{A data.frame containing the standard Garrett conversion
#'   table used to convert percent positions into Garrett scores.}
#'   \item{respondents}{An integer giving the number of respondents included
#'   in the analysis.}
#'   \item{factors}{An integer giving the number of factors ranked by the
#'   respondents.}
#'   \item{factor_names}{A character vector containing the names of the
#'   ranked factors.}
#'   \item{call}{The matched function call used to create the object.}
#'   }
#' @examples
#' \donttest{
#' result <- garrett_rank(garrett_example,respondent = "Respondent")
#' summary(result)
#' result$ranking
#' result$frequency
#' result$weighted_scores
#' }
#' @export
garrett_rank <- function(data, respondent = NULL) {

  data <- .validate_ranking_data(data, respondent = respondent)

  rank_number <- ncol(data)
  respondents <- nrow(data)
  factor_names <- names(data)

  frequency_data <- data.frame(
    Rank = seq_len(rank_number),
    check.names = FALSE
  )

  for (i in seq_len(rank_number)) {
    frequency_data[[factor_names[i]]] <-
      as.numeric(
        table(
          factor(
            data[[i]],
            levels = seq_len(rank_number)
          )
        )
      )
  }

  frequency_data$Percent_Position <-
    100 * (frequency_data$Rank - 0.5) / rank_number

  reference <- garrett_table()

  score_match <- vapply(
    frequency_data$Percent_Position,
    function(x) {
      reference$Garrett_Score[
        which.min(abs(reference$Percent_Position - x))
      ]
    },
    numeric(1)
  )

  frequency_data$Garrett_Score <- score_match

  weighted <- as.matrix(
    frequency_data[, seq_len(rank_number) + 1L, drop = FALSE]
  )

  weighted <- sweep(
    weighted,
    MARGIN = 1L,
    STATS = score_match,
    FUN = "*"
  )

  colnames(weighted) <- factor_names
  rownames(weighted) <- paste0("Rank ", seq_len(rank_number))

  results <- data.frame(
    Factor = factor_names,
    Mean_Score = colSums(weighted) / respondents,
    stringsAsFactors = FALSE
  )

  results <- results[order(-results$Mean_Score, results$Factor), ,
                    drop = FALSE]
  results$Rank <- seq_len(nrow(results))
  rownames(results) <- NULL

  out <- list(
    ranking = results,
    frequency = frequency_data,
    weighted_scores = weighted,
    reference = reference,
    respondents = respondents,
    factors = rank_number,
    factor_names = factor_names,
    call = match.call()
  )

  class(out) <- "garrett"
  out
}


#' Plot Garrett Ranking Results
#'
#' Produces graphical representations of Garrett ranking results.
#'
#' @param x An object of class \code{"garrett"} created by
#'   \code{\link{garrett_rank}}.
#' @param type Type of plot. Options are \code{"bar"}, \code{"lollipop"},
#'   \code{"dot"}, \code{"line"}, \code{"heatmap"}, \code{"cluster"}, and
#'   \code{"contribution"}.
#' @param top Number of top-ranked factors to display. Default is \code{NULL},
#'   which displays all factors. Applies to ranking plots.
#' @param color Colour used for ranking plots.
#' @param label Logical. Should Garrett scores be displayed as labels?
#' @param point_size Size of points used in dot, lollipop, and line plots.
#' @param line_size Width of lines used in lollipop and line plots.
#' @param cluster_rows Logical. Should factors be hierarchically clustered
#'   in cluster plots?
#' @param cluster_cols Logical. Should ranks be hierarchically clustered?
#' @param distance Distance measure used for hierarchical clustering.
#' @param clustering_method Method used for hierarchical clustering.
#' @param scale Should heatmap data be scaled by rows, columns, or not
#'   scaled? Options are \code{"none"}, \code{"row"}, or \code{"column"}.
#' @param show_numbers Logical. Should matrix values be displayed inside
#'   heatmap cells?
#' @param ... Additional arguments passed to \code{pheatmap::pheatmap}.
#'
#' @return For \code{"bar"}, \code{"lollipop"}, \code{"dot"}, and
#' \code{"line"} plots, a \code{ggplot} object. For \code{"heatmap"},
#' \code{"cluster"}, and \code{"contribution"} plots, a \code{pheatmap}
#' object returned invisibly. The returned object contains the graphical
#' representation of the Garrett ranking results.
#'
#' @importFrom rlang .data
#' @examples
#' \donttest{
#' data(garrett_example)
#' result <- garrett_rank(garrett_example,respondent = "Respondent")
#' #Select the plot type
#' plot(result, type = "bar")
#' plot(result, type = "lollipop")
#' plot(result, type = "dot")
#' plot(result, type = "line")
#' plot(result, type = "heatmap")
#' plot(result, type = "cluster")
#' plot(result,type="contribution")
#' plot(result, type = "cluster", k = 3)
#' plot(result,type = "cluster",scale = "row",show_numbers = TRUE)
#' }
#' @export
plot.garrett <- function(
    x,
    type = c(
      "bar", "lollipop", "dot", "line",
      "heatmap", "cluster", "contribution"
    ),
    top = NULL,
    color = "#2C7FB8",
    label = TRUE,
    point_size = 3,
    line_size = 1,
    cluster_rows = TRUE,
    cluster_cols = FALSE,
    distance = "euclidean",
    clustering_method = "complete",
    scale = "none",
    show_numbers = FALSE,
    ...
) {

  if (!inherits(x, "garrett")) {
    stop("'x' must be an object of class 'garrett'.", call. = FALSE)
  }

  type <- match.arg(type)

  if (!is.logical(label) || length(label) != 1L || is.na(label)) {
    stop("'label' must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(cluster_rows) ||
      length(cluster_rows) != 1L || is.na(cluster_rows)) {
    stop("'cluster_rows' must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(cluster_cols) ||
      length(cluster_cols) != 1L || is.na(cluster_cols)) {
    stop("'cluster_cols' must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(show_numbers) ||
      length(show_numbers) != 1L || is.na(show_numbers)) {
    stop("'show_numbers' must be TRUE or FALSE.", call. = FALSE)
  }

  scale <- match.arg(scale, c("none", "row", "column"))

  if (length(point_size) != 1L ||
      !is.numeric(point_size) ||
      !is.finite(point_size) ||
      point_size <= 0) {
    stop("'point_size' must be a single positive number.", call. = FALSE)
  }

  if (length(line_size) != 1L ||
      !is.numeric(line_size) ||
      !is.finite(line_size) ||
      line_size <= 0) {
    stop("'line_size' must be a single positive number.", call. = FALSE)
  }

  if (!is.null(top)) {
    if (length(top) != 1L ||
        !is.numeric(top) ||
        !is.finite(top) ||
        top < 1) {
      stop("'top' must be a single positive number.", call. = FALSE)
    }
    top <- min(as.integer(top), nrow(x$ranking))
  }

  if (type %in% c("heatmap", "cluster", "contribution")) {

    if (!requireNamespace("pheatmap", quietly = TRUE)) {
      stop(
        "Package 'pheatmap' is required for this plot type. ",
        "Install it with install.packages('pheatmap').",
        call. = FALSE
      )
    }

    if (type %in% c("heatmap", "cluster")) {
      mat <- t(
        as.matrix(
          x$frequency[
            ,
            seq_len(x$factors) + 1L,
            drop = FALSE
          ]
        )
      )
      main_title <- if (type == "cluster") {
        "Garrett Ranking: Clustered Rank Profiles"
      } else {
        "Garrett Ranking: Rank Frequency Heatmap"
      }
      number_format <- "%.0f"
    } else {
      mat <- t(as.matrix(x$weighted_scores))
      main_title <- "Garrett Ranking: Score Contribution"
      number_format <- "%.1f"
    }

    rownames(mat) <- x$factor_names
    colnames(mat) <- paste0("Rank ", seq_len(ncol(mat)))

    if (any(!is.finite(mat))) {
      stop("Heatmap matrix contains non-finite values.", call. = FALSE)
    }

    cluster_rows_use <- type == "cluster" && cluster_rows
    cluster_cols_use <- type == "cluster" && cluster_cols

    result <- pheatmap::pheatmap(
      mat,
      cluster_rows = cluster_rows_use,
      cluster_cols = cluster_cols_use,
      clustering_distance_rows = distance,
      clustering_distance_cols = distance,
      clustering_method = clustering_method,
      scale = scale,
      display_numbers = show_numbers,
      number_format = number_format,
      border_color = "white",
      fontsize = 11,
      fontsize_row = 10,
      fontsize_col = 10,
      main = main_title,
      ...
    )

    return(invisible(result))
  }

  ranking <- x$ranking

  if (!is.null(top)) {
    ranking <- utils::head(ranking, top)
  }

  ranking$Factor <- factor(
    ranking$Factor,
    levels = rev(ranking$Factor)
  )

  if (type == "bar") {
    p <- ggplot2::ggplot(
      ranking,
      ggplot2::aes(x = .data$Factor, y = .data$Mean_Score)
    ) +
      ggplot2::geom_col(fill = color, width = 0.7) +
      ggplot2::coord_flip()
  } else if (type == "lollipop") {
    p <- ggplot2::ggplot(
      ranking,
      ggplot2::aes(x = .data$Factor, y = .data$Mean_Score)
    ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = .data$Factor, xend = .data$Factor,
          y = 0, yend = .data$Mean_Score
        ),
        linewidth = line_size,
        colour = "grey60"
      ) +
      ggplot2::geom_point(
        size = point_size,
        colour = color
      ) +
      ggplot2::coord_flip()
  } else if (type == "dot") {
    p <- ggplot2::ggplot(
      ranking,
      ggplot2::aes(x = .data$Factor, y = .data$Mean_Score)
    ) +
      ggplot2::geom_point(
        size = point_size + 1,
        colour = color
      ) +
      ggplot2::coord_flip()
  } else {
    ranking$Factor <- factor(
      ranking$Factor,
      levels = rev(levels(ranking$Factor))
    )

    p <- ggplot2::ggplot(
      ranking,
      ggplot2::aes(
        x = .data$Factor, y = .data$Mean_Score,
        group = 1
      )
    ) +
      ggplot2::geom_line(
        linewidth = line_size,
        colour = color
      ) +
      ggplot2::geom_point(
        size = point_size,
        colour = color
      ) +
      ggplot2::coord_flip()
  }

  if (label) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = round(.data$Mean_Score, 2)),
        hjust = -0.15,
        size = 4
      )
  }

  p <- p +
    ggplot2::labs(
      title = "Garrett Ranking",
      subtitle = paste0(x$respondents, " respondents"),
      x = NULL,
      y = "Mean Garrett Score"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(face = "bold")
    )

  if (label) {
    p <- p +
      ggplot2::expand_limits(
        y = max(ranking$Mean_Score, na.rm = TRUE) * 1.10
      )
  }

  p
}


#' Kendall's Coefficient of Concordance
#'
#' Computes Kendall's coefficient of concordance (W) for complete,
#' untied ranking data.
#'
#' Each row must represent one respondent and each column one factor.
#' Each respondent must assign every rank from 1 to the number of factors
#' exactly once. Tied ranks are not supported.
#'
#' @param data A data.frame, matrix, CSV file path, or Excel file path
#'   containing ranking data.
#' @param respondent Optional respondent ID column name or column position
#'   to exclude before analysis.
#'
#' @return An object of class \code{"kendall_w"}, which is a list containing:
#' \describe{
#'   \item{statistic}{Kendall's coefficient of concordance (W), ranging from
#'   0 to 1, where larger values indicate stronger agreement among respondents.}
#'   \item{chisq}{The chi-square statistic used to test the statistical
#'   significance of the concordance.}
#'   \item{df}{Degrees of freedom for the chi-square test.}
#'   \item{p.value}{The p-value associated with the chi-square test.}
#'   \item{respondents}{The number of respondents included in the analysis.}
#'   \item{factors}{The number of ranked factors.}
#'   \item{rank_sum}{A named numeric vector containing the sum of ranks
#'   assigned to each factor across respondents.}
#'   \item{call}{The matched function call.}
#' }
#'
#' @examples
#' \donttest{
#' data(garrett_example)
#' kw <- kendall_w(garrett_example,respondent = "Respondent")
#' kw
#' summary(kw)
#' }
#' @export
kendall_w <- function(data, respondent = NULL) {

  data <- .validate_ranking_data(data, respondent = respondent)
  data <- as.matrix(data)

  n <- nrow(data)
  m <- ncol(data)

  rank_sum <- colSums(data)
  mean_rank_sum <- mean(rank_sum)
  S <- sum((rank_sum - mean_rank_sum)^2)

  W <- 12 * S / (n^2 * (m^3 - m))

  chisq <- n * (m - 1) * W
  df <- m - 1

  p.value <- stats::pchisq(
    chisq,
    df = df,
    lower.tail = FALSE
  )

  out <- list(
    statistic = unname(W),
    chisq = unname(chisq),
    df = df,
    p.value = unname(p.value),
    respondents = n,
    factors = m,
    rank_sum = rank_sum,
    call = match.call()
  )

  class(out) <- "kendall_w"
  out
}





