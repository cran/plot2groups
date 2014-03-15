\name{plot2}
\alias{plot2}
\title{A function to plot scatter points for two groups of values.}
\usage{
  plot2(df, size = 1, stackdir = "center",
    stackratio = 1.1, color = c("blue", "red"), test = "t",
    pval = p.value, alternative = "two.sided",
    linesize = 1, xlab = xlabel, ylab = ylabel)
}
\arguments{
  \item{alternative}{a character string specifying the
  alternative hypothesis, must be one of "two.sided"
  (default), "greater" or "less".}

  \item{color}{color of the two groups of dots, deault to
  c("blue","red").}

  \item{df}{a two-column data.frame, the first column is
  numeric values, the second column is character or numeric
  elements indicating two groups.}

  \item{linesize}{the size of the average bar, default to
  1.}

  \item{pval}{a P value, default to P value of two-group
  t.test.}

  \item{size}{the dot size, default to 1.}

  \item{stackdir}{which direction to stack the dots. "up",
  "down", "center"(default), "centerwhole" (centered, but
  with dots aligned).}

  \item{stackratio}{how close to stack the dots. Default to
  1.1, where dots just do not touch.}

  \item{test}{statistical test used, "t" for two sample
  t-tests (default), or "wilcox" for two sample Wilcoxon
  rank sum tests (also known as "Mann-Whitney" test).}

  \item{xlab}{the text for the X axis. Default is the name
  of the second column together with a P value.}

  \item{ylab}{the text for the Y axis. Default is the name
  of the first column in the file.}
}
\description{
  A function to plot scatter points for two groups of
  values.
}
\examples{
data(drd3)
 plot2(drd3, test = "wilcox")
}

