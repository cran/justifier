% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_workspace.R
\name{save_workspace}
\alias{save_workspace}
\title{Save your workspace}
\usage{
save_workspace(
  file = NULL,
  encoding = "UTF-8",
  append = FALSE,
  preventOverwriting = TRUE,
  silent = justifier::opts$get("silent")
)
}
\arguments{
\item{file}{If specified, the file to export the justification to.}

\item{encoding}{The encoding to use when writing the file.}

\item{append}{Whether to append to the file, or replace its contents.}

\item{preventOverwriting}{Whether to prevent overwriting an existing file.}

\item{silent}{Whether to be silent or chatty.}
}
\value{
The result of a call to \code{\link[=export_justification]{export_justification()}}.
}
\description{
Save your workspace
}
\examples{
clean_workspace(force = TRUE, silent=FALSE);
log_decision("First we start using `justifier`.",
             silent=FALSE);
log_decision(paste0("Then we start documenting our ",
                    "decisions and justifications."),
             silent=FALSE);
log_decision("Then we start learning from ourselves.",
             silent=FALSE);
save_workspace();
}
