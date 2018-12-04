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

use_data(fluidigm_expr, overwrite = T)

# Scale -------------------------------------------------------------------

scaled_fluidigm <-
  fluidigm_expr %>%
  scale_fluidigm(.group = target_name)

# Explicit categorical variables ------------------------------------------

 scaled_fluidigm<-
  scaled_fluidigm %>%
  # make species explicit
  mutate(species = str_split_fixed(string = sample_name,
                                   pattern = "-",
                                   n = 2)[, 1]) %>%
  # make stage explicit
  mutate(stage = str_sub(string = sample_name,
                         start = 3,
                         end = 4)) %>%
  # make replicate explicit
  mutate(replicate = str_sub(string = sample_name,
                             start = 5,
                             end = 6))


# Save --------------------------------------------------------------------

use_data(scaled_fluidigm, overwrite = TRUE)
