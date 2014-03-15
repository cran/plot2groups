#' A function to plot scatter points for two groups of values.
#' 
#' @import ggplot2
#' @param alternative a character string specifying the alternative hypothesis, 
#' must be one of "two.sided" (default), "greater" or "less".
#' @param color color of the two groups of dots, deault to c("blue","red").
#' @param df a two-column data.frame, the first column is numeric values, 
#'  the second column is character or numeric elements indicating two groups.
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
#' @examples
#' 
#'  data(drd3)
#'  plot2(drd3, test = "wilcox")
#' 
#' @export
#'
plot2 <- function (df, size = 1, stackdir = "center", stackratio = 1.1, color = c("blue", "red"), test = "t",
                   pval = p.value, alternative = "two.sided", linesize = 1, xlab = xlabel, ylab = ylabel ) {
  ..y.. = NULL
  #   the name of the first group
  g1 = unique(df[,2])[1]
  #   the number of rows, group 1, and group 2
  n = length(df[,1])  
  n1 = sum(df[,2] == g1)  
  n2 = n - n1  
  #   compute p.value of the two groups, truncate p.value into 3 significant digits
  res = eval(parse(text = paste(test,".test(df[,1]~df[,2], alternative = alternative)", sep = "") ))
  p.value = signif(res$p.value, 3)
  #   text of x label and y label
  xlabel = paste(names(df)[2], " (P = ", p.value, ")", sep = "")
  ylabel = names(df)[1] 
  #   plot the data using ggplot2
  ggplot(df, aes(x=as.factor(df[,2]), y=df[,1] ), environment = environment()) + 
    geom_dotplot(binaxis = "y", binwidth = max(df[,1])/20*size, stackdir = stackdir, stackratio = stackratio, 
                 fill = I(c(rep(color[1], n1), rep(color[2], n2)))) + 
    geom_errorbar(stat = "hline", yintercept = "mean", size = linesize, aes(ymax=..y.., ymin=..y..)) +
    xlab(xlab) + ylab(ylab)}

#' 
#' @name drd3
#' @alias drd3
#' @docType data
#' @title an example data.frame.
#' @usage data(drd3)
#' @format A data.frame containing 74 observations from two groups.
#' @description relative DRD3 mRNA levels in blood of 37 schizophrenia cases and 37 healthy controls.
#' @usage data(drd3)
#' @source Converging evidence implicates the dopamine D3 receptor gene in vulnerability to schizophrenia. 
#' Am J Med Genet Part B. 2011, 156(5):613-9
#' @keywords datasets
#' 