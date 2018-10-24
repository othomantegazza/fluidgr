#' Parse the CSV Output of Fluidigm
#'
#' Parse the output of the Fluidigm qPCR measurement
#' from CSV format into R.
#' This function uses `read_delim()` from the package
#' `readr` to read the data
#' and functions from the `dplyr` package to perform some
#' basic tidying
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @param path The path to the CSV file
#' @param simplify TRUE or FALSE.
#'
#'   If TRUE, it returns a simplified version of the dataset
#'   that contains only essential variables.
#'
#' @export

read_fluidigm <- function(path,
                          simplify = TRUE) {
  dat <- readr::read_delim(file = path,
                           delim = ";",
                           col_names = c("chamber_id", "sample_name", "sample_type",
                                         "sample_rconc", "target_name", "target_type",
                                         "ct_value", "ct_calibrated_rconc", "ct_quality",
                                         "ct_call", "ct_threshold", "tm_inrange",
                                         "tm_outrange", "tm_peakratio", "comments"),
                           locale = readr::locale(decimal_mark = ","),
                           skip = 12)
  if(simplify) {
    dat %>%
      dplyr::select(.data$sample_name,
                    .data$sample_type,
                    .data$target_name,
                    .data$target_type,
                    .data$ct_value)
  }
  # %>%
  #   select(sampleName, GeneName,
  #          LOC_Name, Value, Type__1) %>%
  #   dplyr::rename(sample_name = sampleName,
  #                 target_name = GeneName,
  #                 locus_id = LOC_Name,
  #                 ct_value = Value,
  #                 type = Type__1) %>%
  #   mutate(ct_value = as.numeric(ct_value)) %>%
  #   # remove standard curves
  #   filter(sample_name != "H20",
  #          !grepl("Mix", sample_name))
  # out
}

# dat <- read_fluidigm("prepare_data/data9696 puce1.csv")
