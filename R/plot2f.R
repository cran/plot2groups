#' A function to plot scatter points for two groups of values from a data file.
#' 
#' @param alternative a character string specifying the alternative hypothesis, 
#' must be one of "two.sided" (default), "greater" or "less".
#' @param color color of the two groups of dots, deault to c("blue","red").
#' @param file the name of a tab-separated data file. The file needs two
#'  columns, the first column is a vector of values, the second column is character
#'  or numeric elements indicating two groups, the first line of the file must be the
#'  name of the two columns.
#' @param linesize the size of the average bar, default to 1.
#' @param pval a P value, default to P value of two-group t.test.
#' @param size the dot size, default to 1.
#' @param stackdir which direction to stack the dots. "up", "down", "center"(default), 
#'  "centerwhole" (centered, but with dots aligned).
#' @param stackratio how close to stack the dots. Default to 1.1, where dots just do not touch. 
#' @param test statistical test used, "t" for two sample t-tests (default), or "wilcox" for two sample  
#'  Wilcoxon rank sum tests (also known as "Mann-Whitney" test).
#' @param xlab the text for the X axis. Default is the name of the second column together with a P value.
#' @param ylab the text for the Y axis. Default is the name of the first column in the file.
#' @export
#'

plot2f <- function (file, size = 1, stackdir = "center", stackratio = 1.1, color = c("blue", "red"), test = "t",
                   pval = p.value, alternative = "two.sided", linesize = 1, xlab = xlabel, ylab = ylabel ) {
  ff = read.table(file = file, header = TRUE, sep = "\t", quote = '"')
  ..y.. = NULL
  #   the name of the first group
  g1 = unique(ff[,2])[1]
  #   the number of rows, group 1, and group 2
  n = length(ff[,1])
  n1 = sum(ff[,2] == g1)  
  n2 = n - n1  
  #   compute p.value of the two groups, truncate p.value into 3 significant digits
  res = eval(parse(text = paste(test,".test(ff[,1]~ff[,2], alternative = alternative)", sep = "") ))
  p.value = signif(res$p.value, 3)
  #   text of x label and y label
  xlabel = paste(names(ff)[2], " (P = ", p.value, ")", sep = "")
  ylabel = names(ff)[1] 
  #   plot the data using ggplot2
  ggplot(ff, aes(x=as.factor(ff[,2]), y=ff[,1] ), environment = environment()) + 
    geom_dotplot(binaxis = "y", binwidth = max(ff[,1])/20*size, stackdir = stackdir, stackratio = stackratio, 
                 fill = I(c(rep(color[1], n1), rep(color[2], n2)))) + 
    geom_errorbar(stat = "hline", yintercept = "mean", size = linesize, aes(ymax=..y.., ymin=..y..)) +
    xlab(xlab) + ylab(ylab)}
