library(fluidgr)
library(magrittr)
library(dplyr)
library(stringr)

path_to_data <- system.file("extdata", "sample-fluidigm-run.csv",
                            package = "fluidgr",
                            mustWork = TRUE)

normalizers <- c("normalizer1",
                 "normalizer2",
                 "normalizer3")

dat <- read_fluidigm(path = path_to_data,
                     simplify = TRUE) %>%
  filter(!sample_name %>% str_detect("Mix")) %>%
  filter(sample_name != "H20") %>%
  filter(target_name != "normalizer1bis")


normalizers <- c("normalizer1",
                 "normalizer2",
                 "normalizer3")


fluidigm_expr <-
  dat %>%
  normalize(normalizers = normalizers)

use_data(fluidigm_expr)
