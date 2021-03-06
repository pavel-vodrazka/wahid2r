% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/check_web.R
\name{check_web}
\alias{check_web}
\title{Checks if the OIE WAHID website has not changed.}
\usage{
check_web(hash_file = "hashes.rds", write_cache_if_different = FALSE)
}
\arguments{
\item{hash_file}{A character scalar. If the file specified exists, it is used
instead of the dataset supplied with the package.}

\item{write_chache_if_different}{A logical scalar. If \code{TRUE}, current
hashes are written to \code{hash_file}.}
}
\value{
\code{TRUE} If critical features of the web have not changed compared
  to the cached state.

\code{FALSE} If critical features of the the web have changed.
}
\description{
Utility function \code{check_web} downloads the
\href{http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary}{WAHID
--> Disease information --> Immediate notifications and Follow-ups} webpage
and compares computed hashes of some critical features with those stored in
cache. Returns \code{TRUE} if the hashes are identical, otherwise returns
\code{FALSE} with a diagnostic message indicating the features that have
changed. The cache is as a default supplied as a dataset \code{data(hashes)},
but optionally a path to a cache file can be supplied, and also an option to
write such file when the hashes are not identical.
}
\section{Side effects}{
 This function sets the variable \code{web_not_changed}
  in the \code{globals} environment that is utilized by other functions from
  the package. As mentioned above, it also optionally writes a cache file at
  the location specified by the user.
}
\examples{
\dontrun{check_web()}
}
\seealso{
\code{\link{read_cache}}, \code{\link{write_cache}}
}

