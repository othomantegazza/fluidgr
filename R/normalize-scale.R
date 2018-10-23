#' Estimate Geometric Means
#'
#' for normalizers
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
#' @export

normalize  <- function(.data, normalizers)
{
 stopifnot(all(normalizers %in% .data$target_name))

  # The geometric mean of normalizers
  # estimates the amount of cDNA in the sample
  norms <-
    .data %>%
    dplyr::filter(target_name %in% normalizers) %>%
    dplyr::group_by(sample_name) %>%
    dplyr::summarize(norm_geom_mean = gm_mean(ct_value))


  # subtract normalizer and take exponential to estimate expression
  # note!!! Skip calibration, is it legit????
  # So the formula is 2^-(Ct_gene - Ct_norm)
  norm_data <-
    .data %>%
    dplyr::filter(!target_name %in% normalizers) %>%
    dplyr::left_join(norms) %>%
    dplyr::mutate(expression = 2^(-(ct_value - norm_geom_mean))) %>%
    # Low expressed genes (Ct 999) to 0
    dplyr::mutate(expression = round(expression, digits = 5))
}

#' Scale Normalized Fluidigm data
#'
#' @export

scale_fluidigm <- function(.data, .group)
  {
  .group <- enquo(.group)

  scaled_dat <-
    .data %>%
    group_by(!!.group) %>%
    mutate(scaled_expression = scale(expression))
}
