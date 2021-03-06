% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize-scale.R
\name{normalize}
\alias{normalize}
\title{Normalize Ct Values}
\usage{
normalize(.data, normalizers)
}
\arguments{
\item{.data}{A `data.frame` produced by `read_fluidigm()`. The columns required
are: `sample_name`,`target_name` and `ct_value`.}

\item{normalizers}{A `character` vector with the names of the reference normalizers
as they are stored in the `sample_name` column of `data`.}
}
\description{
Normalize Ct values in each sample on the geometric mean of user provided
reference genes.
}
