\name{plot2f}
\alias{plot2f}
\title{A function to plot scatter points for two groups of values from a data file.}
\usage{
  plot2f(file, size = 1, stackdir = "center",
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

  \item{file}{the name of a tab-separated data file. The
  file needs two columns, the first column is a vector of
  values, the second column is character or numeric
  elements indicating two groups, the first line of the
  file must be the name of the two columns.}

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
  values from a data file.
}

