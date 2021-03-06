% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post-update-HSPF-sup-file.R
\name{post.update.HSPF.sup.file}
\alias{post.update.HSPF.sup.file}
\title{updates the sup file (in a character vector) with the loads for each pls given the line info in the sup file}
\usage{
post.update.HSPF.sup.file(df.pls.line.info, chr.hspf.sup, df.sup.table,
  chr.sup.col.num.name)
}
\arguments{
\item{df.pls.line.info}{pls line info that correposnds to the HSPF-sup-file info from HSPF-uci-file}

\item{chr.hspf.sup}{character vector containing the HSPF-sup-file with each each corresponding to a line of the file}

\item{df.sup.table}{the accum or sqolim loads from the sub-models for the update of sup file}

\item{chr.sup.col.num.name}{the column name in df.sup.table that has the sup-file line numbers for the loads}
}
\description{
updates the sup file (in a character vector) with the loads for each pls given the line info in the sup file
}
