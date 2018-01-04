
CCbeta <- function (index, stock1, mcap, ndebt) {
  function(sd, ed, freq, cb=0, fullsize=FALSE) {

  require(xts)
  data1 <- merge(index, stock1)
  
  ##Indexing
  indx <- names(stock1)
  nn <- as.character(names(stock1))
  
  for (i in 1:length(indx)) {
    indx[i] <- unlist(strsplit(nn[i], ".", fixed = TRUE))[2]
  }
  
  ##Gearing
  mc <- mcap[time(mcap) > sd & time(mcap) < ed]
  nd <- ndebt[time(ndebt) > sd & time(ndebt) < ed ]
  nd[nd < 0] <- 0
  
  gg <- nd/(nd+mc)
  gearing <- colMeans(gg, na.rm = T)
  
  
  ##Return
  require(quantmod)
  endpoints.nna <- function (data, on = freq, cb=0) {
    ep <- endpoints(data, on) - cb
    ep <- ep[c(-1, -2, -length(ep))]
    
    min <- min(which(!is.na(data)))
    max <- max(which(!is.na(data)))
    
    if (on!="days" & !is.na(min+max)) {
      
      range <- seq(min, max,1)
      
      ep[is.na(data[ep]) & ep %in% range] <- ep[is.na(data[ep]) & ep %in% range]-1
      
      ep[is.na(data[ep]) & ep %in% range] <- ep[is.na(data[ep]) & ep %in% range]-1
      
      ep[is.na(data[ep]) & ep %in% range] <- ep[is.na(data[ep]) & ep %in% range]-1
      
    }
    
    ep <- c(0, ep)
    
    return(ep)
  }
  
  ep <- endpoints.nna(data1[,1], on=freq, cb=cb)
  ep2 <- data.frame(sapply(data1, FUN = endpoints.nna, on=freq, cb=cb))
  
  dd <- data1[ep,]
  for (i in 1:ncol(data1)) {
    dd[,i] <- data1[ep2[,i],i]
  }
  
  rr <- lapply(dd, Delt)
  Return <- Reduce(merge, rr)
  names(Return) <- names(data1)
  
  Return <- Return[time(Return) > sd & time(Return) < ed]
  Return1 <- Return  #[time(Return) >= as.Date("2010-10-16")]
   require(PerformanceAnalytics)
  
  #looping
  blist <- list()
  
  for (i in 1:ncol(index)) {
    sub <- c(names(index)[i], names(stock1)[indx == names(index)[i]])
    Return2 <- Return1[,names(Return1) %in% sub]
    
    if (ncol(Return2) > 1) {
      beta <- CAPM.beta(Return2[,-1], Return2[,1], Rf=0)
              #tail(chart.RollingRegression2(Ra=Return2[,-1], Rb=Return2[,1], Rf=0, width=width, na.pad=NULL),1)
              #CAPM.beta(Return2[,-1], Return2[,1], Rf=0)
    
    blist[[names(index)[i]]] <- beta
    
    } else next
  }
  
  blist1 <- list()
  for (i in 1:length(blist)) {
    temp <- data.frame(beta = t(blist[[names(blist)[i]]]))
    names(temp) <- "beta"
    blist1[[names(blist)[i]]] <- temp
  }
  
  ##deal with single stocks
  
  xx2 <- do.call(rbind, blist1)
  indx2 <- nchar(rownames(xx2)) < 3
  single <- names(stock1)[indx %in% rownames(xx2)[indx2]]
  
  indx3 <- single
  for (i in 1:length(indx3)) {
    indx3[i] <- unlist(strsplit(single[i], ".", fixed = TRUE))[2]
  }
  
  single <- paste(indx3, single, sep=".")
  rownames(xx2)[indx2] <- single[match(rownames(xx2)[indx2], indx3)]
  
  ##Add gearing
  xx2$stock <- substring(rownames(xx2), 4)
  gearing <- data.frame(gearing)
  gearing$stock <- rownames(gearing)
  
  require(dplyr)
  xx3 <- full_join(gearing, xx2, by="stock")
  xx3$abeta <- with(xx3, beta*(1-gearing))
  
  ##Add index in front of stock tags
  indx <- xx3$stock
  for (i in 1:length(indx)) {
    indx[i] <- unlist(strsplit(xx3$stock[i], ".", fixed = TRUE))[2]
  }
  xx3$stock <- paste(indx, xx3$stock, sep=".")
  
  indx <- xx3$stock
  for (i in 1:length(indx)) {
    indx[i] <- unlist(strsplit(xx3$stock[i], ".", fixed = TRUE))[1]
  }
  
  xx3$indx <- indx
  xx3 <- xx3[,c(5,2,3,1,4)]
  xx3$stock <- substring(xx3$stock,4)
  
  ##Add sample size
  count <- data.frame(sapply(Return1, function (x) sum(!is.na(x))))
  count$stock <- rownames(count)
  
  xx4 <- full_join(xx3,count, by="stock")
  names(xx4)[6] <- "sample.size"
  
  #if (fullsize) xx4[xx4$sample.size < as.numeric(names(sort(table(xx4$sample.size),decreasing=TRUE)[1])), 3:5] <- NA
  if (fullsize) xx4[xx4$sample.size < 10, 3:5] <- NA
  return(xx4)

  }
}

