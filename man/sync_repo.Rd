% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sync_repo.R
\name{sync_repo}
\alias{sync_repo}
\title{Run the data repository synchronisation}
\usage{
sync_repo(config, create_root = FALSE, verbose = TRUE)
}
\arguments{
\item{config}{data.frame: configuration as returned by \code{\link{read_repo_config}}}

\item{create_root}{logical: should the data root directory be created if it does not exist?}

\item{verbose}{logical: if TRUE, provide additional progress output}
}
\value{
vector of logical values indicating success of each data source in config. NA indicates that the data source was not run (e.g. do_sync set to FALSE)
}
\description{
Run the data repository synchronisation
}

