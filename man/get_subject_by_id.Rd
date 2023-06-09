% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_all_subjects.R
\name{get_subject_by_id}
\alias{get_subject_by_id}
\title{Get lower level of subject}
\usage{
get_subject_by_id(id, rate = 60/60)
}
\arguments{
\item{id}{subject id}

\item{rate}{maximum number of requests per second 60/60 by default}
}
\value{
a tibble with high level id (parent_id), name of lower level (name) and
its id
}
\description{
Get lower level of subject
}
\examples{
get_subject_by_id("K8")
}
