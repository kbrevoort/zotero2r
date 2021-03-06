% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build-database.R
\name{create_bibliography}
\alias{create_bibliography}
\title{Create Bibliography}
\usage{
create_bibliography(rmd_file)
}
\arguments{
\item{rmd_file}{The path to the R Markdown file for which a bibliography is to
be generated}
}
\description{
Reads an R Markdown file, identifies the citations contained therein, and
produces a BibLaTeX file. The bibliography file is stored in the same directory
as the R Markdown file.
}
\details{
If any citations are found in the Rmd file that are not found in the existing
zotero2r database, an warning message will appear.
}
