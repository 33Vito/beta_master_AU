library(dygraphs)
library(xts)
require(PerformanceAnalytics)

chart.RollingRegression2 <- function (Ra, Rb, width = 12, Rf = 0, attribute = c("Beta", "Alpha", 
                                                                                "R-Squared"), main = NULL, na.pad = TRUE, ...) 
{
  Ra = checkData(Ra)
  Rb = checkData(Rb)
  attribute = attribute[1]
  columns.a = ncol(Ra)
  columns.b = ncol(Rb)
  columnnames.a = colnames(Ra)
  columnnames.b = colnames(Rb)
  Ra.excess = Return.excess(Ra, Rf)
  Rb.excess = Return.excess(Rb, Rf)
  for (column.a in 1:columns.a) {
    for (column.b in 1:columns.b) {
      merged.assets = merge(Ra.excess[, column.a, drop = FALSE], 
                            Rb.excess[, column.b, drop = FALSE])
      if (attribute == "Alpha") 
        column.result = rollapply(na.omit(merged.assets), 
                                  width = width, FUN = function(x) lm(x[, 1, 
                                                                        drop = FALSE] ~ x[, 2, drop = FALSE])$coefficients[1], 
                                  by = 1, by.column = FALSE, fill = na.pad, align = "right")
      if (attribute == "Beta") 
        column.result = rollapply(na.omit(merged.assets), 
                                  width = width, FUN = function(x) lm(x[, 1, 
                                                                        drop = FALSE] ~ x[, 2, drop = FALSE])$coefficients[2], 
                                  by = 1, by.column = FALSE, fill = na.pad, align = "right")
      if (attribute == "R-Squared") 
        column.result = rollapply(na.omit(merged.assets), 
                                  width = width, FUN = function(x) summary(lm(x[, 
                                                                                1, drop = FALSE] ~ x[, 2, drop = FALSE]))$r.squared, 
                                  by = 1, by.column = FALSE, align = "right")
      column.result.tmp = xts(column.result)
      colnames(column.result.tmp) = paste(columnnames.a[column.a], 
                                          columnnames.b[column.b], sep = " to ")
      column.result = xts(column.result.tmp, order.by = time(column.result))
      if (column.a == 1 & column.b == 1) 
        Result.calc = column.result
      else Result.calc = merge(Result.calc, column.result)
    }
  }
  if (is.null(main)) {
    freq = periodicity(Ra)
    switch(freq$scale, minute = {
      freq.lab = "minute"
    }, hourly = {
      freq.lab = "hour"
    }, daily = {
      freq.lab = "day"
    }, weekly = {
      freq.lab = "week"
    }, monthly = {
      freq.lab = "month"
    }, quarterly = {
      freq.lab = "quarter"
    }, yearly = {
      freq.lab = "year"
    })
    main = paste("Rolling ", width, "-", freq.lab, " ", attribute, 
                 sep = "")
  }
#  tc <- chart.TimeSeries(Result.calc, main = main, ...)
#   dy <- dygraph(Result.calc, main = main, ...)
#   beta <- Result.calc
  return(Result.calc)
}

