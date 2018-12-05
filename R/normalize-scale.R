utils::globalVariables(".")

#' Estimate Geometric Means
#'
#' for normalizers
#'
#' @param x A numeric vector
#' @param na.rm should NA be removed?
#'
#' source:
#' https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in

gm_mean <-  function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#' Normalize Ct Values
#'
#' Normalize Ct values in each sample on the geometric mean of user provided
#' reference genes.
#'
#' @importFrom rlang .data
#'
#' @param .data A `data.frame` produced by `read_fluidigm()`. The columns required
#'     are: `sample_name`,`target_name` and `ct_value`.
#'
#' @param normalizers  A `character` vector with the names of the reference normalizers
#'     as they are stored in the `sample_name` column of `data`.
#'
#' @export

normalize <- function(.data, normalizers)
{
  stopifnot(all(normalizers %in% .data$target_name))
  stopifnot(is.data.frame(.data))
  stopifnot(all(c("sample_name","target_name","ct_value") %in% colnames(.data)))

  # The geometric mean of normalizers
  # estimates the amount of cDNA in the sample
  norms <-
    .data %>%
    dplyr::filter(.data$target_name %in% normalizers) %>%
    dplyr::group_by(.data$sample_name) %>%
    dplyr::summarise(norm_geom_mean = gm_mean(.data$ct_value))


  # subtract normalizer and take exponential to estimate expression
  # note!!! Skip calibration, is it legit????
  # So the formula is 2^-(Ct_gene - Ct_norm)
  norm_data <-
    .data %>%
    dplyr::filter(!.data$target_name %in% normalizers) %>%
    dplyr::left_join(norms, by = "sample_name") %>%
    dplyr::mutate(expression = 2^(-(.data$ct_value - .data$norm_geom_mean))) %>%
    # Low expressed genes (Ct 999) to 0
    dplyr::mutate(expression = round(.data$expression, digits = 5))
}

#' Scale Normalized Fluidigm data
#'
#' Scale your fluidigm data as z-scores. Run this function after `normalize()`.
#'
#' @param .data A `data.frame` produced by `read_fluidigm()` and normalized by
#'     `normalize()`. The columns required
#'     are: `sample_name`,`target_name` and `expression`.
#'
#' @param .group A column of `.data` that you want to use to group your
#'     expression values before scaling.

scale_sd_fluidigm <- function(.data,
                              .group)
  {
  .group <- dplyr::enquo(.group)

  scaled_dat <-
    .data %>%
    dplyr::group_by(!!.group) %>%
    dplyr::mutate(scaled_expression = scale(.data$expression))
}

#' Scale Normalized Fluidigm data
#'
#' Scale your fluidigm data in range 0 to 1.
#' Run this function after `normalize()`.
#'
#' @param .data A `data.frame` produced by `read_fluidigm()` and normalized by
#'     `normalize()`. The columns required
#'     are: `sample_name`,`target_name` and `expression`.
#'
#' @param .group A column of `.data` that you want to use to group your
#'     expression values before scaling.


scale_01_fluidigm <- function(.data, .group)
{
  .group <- dplyr::enquo(.group)

  scaled_dat <-
    .data %>%
    dplyr::group_by(!!.group) %>%
    dplyr::mutate(scaled_01_expression = .data$expression %>%
             scales::rescale(from = range(.),
                     to = c(0,1)))
}

#' Scale Normalized Fluidigm data
#'
#' Add these two column to a fluidigm dataset:
#'
#' - `scaled_sd` is a column that stores expression values scaled
#'     by z-scored.
#' - `scaled_01` is a column that stores expression values scaled
#'     in range from 0 to 1.
#'
#' Run this function after `normalize()`.
#'
#' @param .data A `data.frame` produced by `read_fluidigm()` and normalized by
#'     `normalize()`. The columns required
#'     are: `sample_name`,`target_name` and `expression`.
#'
#' @param .group A column of `.data` that you want to use to group your
#'     expression values before scaling.
#'
#' @export

scale_fluidigm <- function(.data, .group)
{
  .group <- dplyr::enquo(.group)
  .data %>%
    scale_sd_fluidigm(.group = !!.group) %>%
    scale_01_fluidigm(.group = !!.group)
}

#' Center Expression on One Single Experimental Condition
#'
#' Add a new column to `.data` named `scaled_ddct`.
#' This new columns stores delta delta Ct
#' scaled expression values.
#'
#' Since large scale qPCR experiments are often multivariated,
#' think clearly which variable you want to use to compare your
#' data and which variable you want to use to split them.
#'
#' The first one goes to the `compare_var` parameter, the others go to the
#' tidy dots.
#'
#' @param .data A `data.frame` produced by `read_fluidigm()` and normalized by
#'     `normalize()`. The columns required
#'     are: `sample_name`,`target_name` and `expression`.
#' @param ... the variables to split `.data` with. Must be a column of `.data`.
#' @param compare_var the variable used to compare expression.
#'     Must be a column of `.data`
#' @param center_on the value of `compare_var` that you want to set to 1 (i.e.
#'     the one that you want to center your data on)
#'
#' @export

scale_ddct <- function(.data,
                       ...,
                       compare_var,
                       center_on)
{
  compare_var <- dplyr::enquo(compare_var)
  stopifnot(center_on %in% (.data %>% dplyr::pull(!!compare_var)))

  # quasiquotation of multiple variables with quos?
  grouping_vars <- ggplot2::quos(...)

  # and then splice them with !!!
  .data %>%
    dplyr::group_by(!!!grouping_vars) %>%
    dplyr::mutate(center_mean = dplyr::case_when(
      !!compare_var == center_on ~ .data$expression) %>%
        mean(na.rm = T),
      ddct_exp = .data$expression/.data$center_mean
      ) %>%
    dplyr::select(-.data$center_mean)
}
