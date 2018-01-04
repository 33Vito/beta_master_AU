
library(shiny)
library(shinydashboard)
library(xts)
library(ggplot2)
library(stringr)
library(lubridate)
library(dygraphs)
library(plotly)
library(reshape2)
library(DT)
library(RColorBrewer)

#load("henry data AGK added.RData")
load("Comparable firm data March 2016.RData")

source("CRR2.R")
source("CCbeta.R")
CCbeta.filled <- CCbeta(index, stock1, mcap, ndebt)

add.months <-  function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]

#input <- data.frame(mkt = "AU", stringsAsFactors=F)

data1 <- merge(index, stock1)

##Indexing
indx <- names(stock1)
nn <- as.character(names(stock1))

for (i in 1:length(indx)) {
  indx[i] <- unlist(strsplit(nn[i], ".", fixed = TRUE))[2]
}

require(quantmod)
endpoints.nna <- function (data, on = freq, cb=0) {
  ep <- endpoints(data, on) + cb
  ep <- ep[c(-1, -(length(ep)-1), -length(ep))]
  
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


#---------------------------------------Series, Return and gearing-------------------------------------------------------------------------

shinyServer(function(input, output) {

  sub <- reactive(c(input$mkt, names(stock1)[indx == input$mkt]))
  sub2 <- reactive(c(input$mkt, input$var))
  
  data2 <- reactive({
    data1[,names(data1) %in% sub()]
  })
  
  output$p.series <- renderDygraph({
    data <- data2()[time(data2()) > input$sd & time(data2()) < input$ed,]
    d1 <- dygraph(data) %>%
      dySeries(input$mkt, axis = 'y2')  %>%
      dyLegend(show = "auto", width = 600) %>%
      dyRangeSelector()
    if (ncol(data2()) > 10) d1 <- d1 %>% dyLegend(show = "onmouseover", width = 600, hideOnMouseOut = TRUE)    
    return(d1)      
  })
  
  Return1 <- reactive({
    ep <- endpoints.nna(data1[,1], on=input$freq, cb=input$cb)
    ep2 <- data.frame(sapply(data1, FUN = endpoints.nna, on=input$freq, cb=input$cb))
    
    dd <- data1[ep,]
    for (i in 1:ncol(data1)) {
      dd[,i] <- data1[ep2[,i],i]
    }
    
    rr <- lapply(dd, Delt)
    Return <- Reduce(merge, rr)
    names(Return) <- names(data1)
    
    Return <- Return[time(Return) > input$sd & time(Return) < input$ed]
    Return[abs(Return) > 30] <- 0
    Return
  })
  
  Return2 <- reactive({
    re <- Return1()[,sub2()]
    
    re <- cbind(re, rowMeans(re[,-1], na.rm = TRUE))
    names(re)[ncol(re)] <- paste0("EW.",input$mkt,".Equity")
    #re$EW.AU.Equity <- rowMeans(re[,-1], na.rm = TRUE)
    
    mc <- mcap[time(re), sub2()[-1]]
    tt <- rowMeans(mc, na.rm=T)
    weight <- mc/tt
    re <- cbind(re, rowMeans(re[,c(-1,-ncol(re))]*weight, na.rm=T))
    names(re)[ncol(re)] <- paste0("VW.",input$mkt,".Equity")
    #re$VW.AU.Equity <- rowMeans(re[,c(-1,-ncol(re))]*weight, na.rm=T)
    
    return(re)
    })
  
  output$download.re <- downloadHandler(
    filename = function() { paste0("Return from ", 
                                   input$sd, " to ", input$ed, ".csv")},
    content = function(file) {
      write.csv(data.frame(Return2()), file)
    }
  )
  
  output$t.mcap <- renderDataTable({
    tt <- mcap[time(mcap) %in% time(Return2()), names(mcap) %in% names(Return2())]
    data.frame(tt)
  })
  
  output$p.return <- renderDygraph({
    d1 <- dygraph(100*Return2()) %>%
      dySeries(input$mkt, axis = 'y')  %>%
      dyLegend(show = "auto", width = 600) %>%
      dyRangeSelector()
    if (ncol(data2()) > 10) d1 <- d1 %>% dyLegend(show = "onmouseover", width = 600, hideOnMouseOut = TRUE)    
    return(d1)      
  })
  
  gearing1 <- reactive({
    mc <- mcap[index(mcap) >= input$sd &
                 index(mcap) <= input$ed]
    nd <- ndebt[index(mcap) >= input$sd & index(mcap) <= input$ed]
    nd[nd < 0] <- 0
    
    gg <- nd/(nd+mc)
    gg <- gg[, colnames(gg) %in% colnames(Return2()), drop=F]
    
#    if (input$gzero) gg[] <- 0
    return(gg)
  })
    
  output$p.gearing <- renderDygraph({
    d1 <- dygraph(100*gearing1()) %>%
      dyLegend(show = "auto", width = 600) %>%
      dyRangeSelector()
    if (ncol(gearing1()) > 10) d1 <- d1 %>% dyLegend(show = "onmouseover", width = 600, hideOnMouseOut = TRUE)    
    return(d1) 
  })
  
  output$p.rbs <- renderPlotly({
    rr <- 100*data.frame(Return2())
    data1 <- data.frame(stock=rep(names(rr), each=nrow(rr)),
                        return=melt(rr)$value)
    
    plot_ly(data=data1, x=return, color=stock, type = "box", showlegend=T) %>%
      layout(yaxis=list(showticklabels=F))
    
  })
  
#----------------------------------------UI output-------------------------------------------------------------------
    
  output$var <- renderUI({
    checkboxGroupInput(inputId="var", label="Firms in Portlio:",
                       sub()[-1], inline=F, selected=sub()[-1])
  })
  
  output$var2 <- renderUI({
    selectInput("var2", "Which firms?",
                       c(names(Return2())[-1]), selected = names(Return2())[2])
  })
  
#-----------------------------------------2D and 3D  return scatter and density-----------------------------------------------------------------
  
#   myColorRamp <- function(colors, values) {
#     v <- (values - min(values))/diff(range(values))
#     x <- colorRamp(colors)(v)
#     rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
#   }
#   
#   output$myWebGL <- renderWebGL({
# 
#     data <- data.frame(date=time(Return2()), 100*data.frame(Return2()))
#     
#     cols <- myColorRamp(c("red","blue"), as.numeric(data$date))
#     
#     eval(parse(text=paste0(
#       "with(data, plot3d(",input$mkt,", date, ",input$var2, ", col = cols, size = 0.8, type = 's', axes=FALSE,
#                       main='',
#       xlab='indx %',
#       ylab='date',
#       zlab='stock %')
#     )"
#     )
#                  ))
# 
# #     with(data, plot3d(AU, date, DUE.AU.Equity, col = cols, axes=FALSE,
# #                       main="DUK.US.Equity daily return vs index",
# #                       xlab="indx %",
# #                       ylab="date",
# #                       zlab="stock %")
# #     )
#     
#     axes3d(edges=c("x", "z"))
#     axis3d("y", at=as.numeric(seq.Date(input$sd, input$ed, by='years')), 
#            labels=seq(year(input$sd),year(input$ed),1))
#   })
  
  output$plotly3d <- renderPlotly({
    req(input$var2)
    
    xx <- 100*data.frame(Return2()[,c(input$mkt,input$var2)])
    xx$date <- (time(Return2()))
    xx$date.num <- as.numeric(xx$date)
    
    mktvar <- as.formula(paste0("~", input$mkt))
    stockvar <- as.formula(paste0("~", input$var2))

    
    p1 <- plot_ly(xx, y=mktvar, x=~date, z=stockvar, type ='scatter3d' ,color=~date, 
            mode = 'markers', marker = list(size=4, showscale=F)) %>% 
      layout(scene=list(aspectmode="cube"))
    
    # p1 <- eval(parse(text=paste0("plot_ly(xx, y=~",input$mkt, ", x=date, z=~", input$var2, ", type ='scatter3d', 
    #         mode = 'markers', marker = list(size=4), color = as.numeric(date))")))
    
    p1 <- plotly_build(p1)
    p1$x$data[[1]]$marker$colorbar = NULL
    
    p1
  })
  
  output$scat <- renderPlotly({
    req(input$var2)
    
    xx <- 100*data.frame(Return2()[,c(input$mkt,input$var2)])
    xx$date <- as.Date(time(Return2()))
    
    lm_eqn <- function(df){
      m <- lm(df[,2] ~ df[,1]);
      eq <- str_replace_all("y = a + b * x, R^2 = r2, n = num", 
                       list(a = as.character(round(coef(m)[1], digits = 4)), 
                            b = as.character(round(coef(m)[2], digits = 4)), 
                            r2 = paste0(as.character(100*round(summary(m)$r.squared, digits = 2)),"%"),
                            num = min(sum(!is.na(df[,1])), sum(!is.na(df[,2])))))
      as.character(as.expression(eq));                 
    }
    
    n <- min(sum(!is.na(xx[,1])), sum(!is.na(xx[,2])))

    g1 <- ggplot(environment=environment(), data=xx, aes_string(x=input$mkt, y=input$var2)) + 
      geom_point(aes(text=paste0("Date ",date)), size=2, shape=1) + theme_minimal()+ theme(legend.position="none") +
      stat_smooth(method='lm', se=F)
#       xlab(names(xx)[1]) + #scale_x_continuous(limits = c(-14,14)) + 
#       ylab(names(xx)[2]) + #scale_y_continuous(limits = c(-16,16)) + 
      # ggtitle(lm_eqn(xx))

#       geom_text(x = 5, y = -14, label = lm_eqn(xx), parse = TRUE, size=6.5) +
#       ggtitle(paste0(n,"-",input$freq," beta ending on the ",input$cb,"th trading day"))
    
    gg1 <- ggplotly(g1)
    gg1
  })
  
  output$den <- renderPlotly({
    req(input$var2)
    
    xx <- 100*data.frame(Return2()[,c(input$mkt,input$var2)])
    
    plot_ly(x = xx[,2], opacity = 0.6, type = "histogram", name = input$var2) %>%
      add_trace(x = xx[,1], opacity = 0.6, type = "histogram", name = input$mkt) %>%
      layout(barmode="overlay",
             xaxis = list(title = "Return (%)"),
             legend=list(x=.9,y=1),
             bargap=0.05)
  })
  
  
#------------------------------------------------data table and download----------------------------------------------
  
  output$t.return <- renderDataTable({
    round(100*data.frame(Return2()),2)
  })
  
  output$t.series <- renderDataTable({
    data <- data2()[time(data2()) > input$sd & time(data2()) < input$ed,]
    round(data.frame(data),2)
  })
  
  output$t.result <- renderDataTable({
    datatable(result()) %>% formatRound(names(result())[3:6])
  })
  
  output$download.Result <- downloadHandler(
    filename = function() { paste("Beta result ", 
                                  as.character(time(Return1())[1]), ' to ',
                                  as.character(time(Return1())[nrow(Return1())]),
                                  '.csv', sep='') },
    content = function(file) {
      write.csv(result(), file)
    }
  )
  
#-------------------------------------------------CCbeta---------------------------------------------------------------
  
  result <- reactive({
    withProgress(message="Desperately working on it!", value=0, {
    CCbeta.filled(input$sd, input$ed, input$freq, fullsize = input$fs)
    })
  })
  
  output$bvg2 <- renderPlotly({
    avg.b <- as.character(round(mean(result()[,3], na.rm = T),2))
    plot_ly(data=result()[1:(nrow(result())-ncol(index)),], x = ~beta, y = ~stock, type= "bar", orientation = "h",
            text = paste("stock: ", stock), color = indx, 
            marker = list(colors = brewer.pal(6, "Set2")), opacity=.6) %>%
#      add_trace(x=gearing, y=fitted(lm((beta~gearing))) ,name="fitted") %>%
      layout(title=paste0("Average = ", avg.b), 
             margin=list(l=120), legend=list(x=.9,y=.1), xaxis = list(title = ""))
  })
  
  output$avg2 <- renderPlotly({
    avg.ab <- as.character(round(mean(result()[,5], na.rm = T),2))
    plot_ly(data=result()[1:(nrow(result())-ncol(index)),], x = ~beta, y = ~stock, type= "bar", orientation = "h",
            text = paste("stock: ", stock), color = indx, 
            marker = list(colors = brewer.pal(6, "Set2")), opacity=.6) %>%
#      add_trace(x=gearing, y=fitted(lm(abeta~gearing)) ,name="fitted") %>%
      layout(title=paste0("Average = ", avg.ab), 
             margin=list(l=120), legend=list(x=.9,y=.1), xaxis = list(title = ""))

  })
  
#-----------------------------------------------Portfolio analysis-----------------------------------------------------

  wbeta <- reactive({
    n <- ncol(Return2())
    withProgress(message="Desperately working on it!", value=0, {
    chart.RollingRegression2(Ra=Return2()[,c(n-1,n)], Rb=Return2()[,1], Rf=0, width=input$wd, na.pad=NULL)
    })
  })
  
  output$p.wbeta <- renderDygraph({
    dd <- wbeta()
    names(dd) <- c("Equal weighted portfolio","Value weighted portfolio")
    dygraph(dd, main= paste0("Weighted portfolio rolling ", 
                                 as.character(input$wd), "-", input$freq, " equity beta")) %>%
      dyLegend(show = "auto", width = 600) %>%
      dyOptions(digitsAfterDecimal = 4) %>%
      dyRangeSelector()
  })
  
  rollingg <- reactive({
    gg <- gearing1()[time(Return2()),sub2()[-1]]
    gg$EW <- rowMeans(gg[,-1], na.rm = TRUE)
    
    mc <- mcap[time(Return2()), sub2()[-1]]
    tt <- rowMeans(mc, na.rm=T)
    weight <- mc/tt
    gg$VW <- rowMeans(gg[,-ncol(gg)]*weight, na.rm=T)
    
    rtt <- rollapply(gg, input$wd, function(x) mean(x, na.rm=T))
    rtt1 <- rtt[time(wbeta()),]
    
    n <- ncol(rtt1)
    rtt1[,c(n-1,n)]
  })
  
  wabeta <- reactive({
    wbeta()*(1-rollingg())
  })
  
  output$p.wabeta <- renderDygraph({
    dd <- wabeta()
    names(dd) <- c("Equal weighted portfolio","Value weighted portfolio")
    dygraph(dd, main= paste0("Weighted portfolio rolling ", 
                             as.character(input$wd), "-", input$freq, " asset beta")) %>%
      dyLegend(show = "auto", width = 600) %>%
      dyOptions(digitsAfterDecimal = 4) %>%
      dyRangeSelector()
  })
  
  output$p.wgearing <- renderDygraph({
    dd <- rollingg()
    names(dd) <- c("Equal weighted portfolio","Value weighted portfolio")
    dygraph(dd, main= paste0("Weighted portfolio rolling ", 
                             as.character(input$wd), "-", input$freq, " gearing ratio")) %>%
      dyLegend(show = "auto", width = 600) %>%
      dyOptions(digitsAfterDecimal = 4) %>%
      dyRangeSelector()
  })
  
  rw.data <- reactive({
    dd <- data.frame(wbeta(),rollingg(),wabeta())
    names(dd) <- c("EW.beta","VW.beta","EW.gearing","VW.gearing","EW.abeta","VW.abeta")
    dd
    })
  
  output$download.rw <- downloadHandler(
    filename = function() { paste0("Portfolio rolling results from ", 
                                   input$sd, " to ", input$ed, ".csv")},
    content = function(file) {
      write.csv(rw.data(), file)
    }
  )

  
#----------------------------------------------Dynamic analysis--------------------------------------------------------
  
  rollingMatrix <- reactive({
    
    sd.o <- add.months(input$sd, -input$rp*12)
    ed.o <- add.months(input$ed, -input$rp*12)
    n <- (year(input$sd) - year(sd.o) + 1)*12/input$rg
    
    withProgress(message="Desperately working on it!", value=0, {
    mylist <- list()
    for (i in seq(0, by = input$rg, length.out = n)) {
      sd <- add.months(sd.o, i)
      ed <- add.months(ed.o, i)
      
      mylist[[as.character(ed)]] <- melt(CCbeta.filled(sd, ed, input$freq, input$cb, input$fs)[,c(-1,-2)])$value
    }
    
    rr <- data.frame(do.call(cbind,mylist))
    name <- c(paste0(result()[["stock"]],""), #beta
              paste0(result()[["stock"]],""), #gearing
              paste0(result()[["stock"]],""), #abeta
              paste0(result()[["stock"]],"")) #sample.size
    dd <- data.frame(indx=result()[['indx']],name,rr)
    dd
    # names(category) <- c('name','type')
    # left_join(dd,category,by='name')[,c(ncol(dd)+1,1:ncol(dd))]
  })
  })
  
  output$download.rollingMatrix <- downloadHandler(
    filename = function() { paste0(as.character(year(input$ed)-year(input$sd))," year ", input$rg,"-month Rolling matrix ", 
                                  input$freq," ending ", as.character(input$ed),
                                  '.csv') },
    content = function(file) {
      write.csv(rollingMatrix(), file)
    }
  )
  
  nr <- reactive(nrow(result()))
  
  output$r.beta <- renderDataTable(rollingMatrix()[1:nr(),])
  output$r.gearing <- renderDataTable(rollingMatrix()[(nr()+1):(2*nr()),])
  output$r.abeta <- renderDataTable(rollingMatrix()[(2*nr()+1):(3*nr()),])
  output$r.sample.size <- renderDataTable(rollingMatrix()[(3*nr()+1):(4*nr()),])
  
  d.beta <- reactive({
    dd <- rollingMatrix()[1:nr(),]
    if (input$fbi != "All") dd <- filter(dd,indx==input$fbi)
    if (input$fbs != "All") dd <- filter(dd,name==input$fbs)
    #if (input$fbt != "All") dd <- filter(dd,type==input$fbt)
    dd #[,c(ncol(dd),1:(ncol(dd)-1))]
  })
    
  d.gearing <- reactive({
    dd <- rollingMatrix()[(nr()+1):(2*nr()),]
    if (input$fbi != "All") dd <- filter(dd,indx==input$fbi)
    if (input$fbs != "All") dd <- filter(dd,name==input$fbs)
    #if (input$fbt != "All") dd <- filter(dd,type==input$fbt)
    dd #[,c(ncol(dd),1:(ncol(dd)-1))]
  })
  
  d.abeta <- reactive({
    dd <- rollingMatrix()[(2*nr()+1):(3*nr()),]
   if (input$fbi != "All") dd <- filter(dd,indx==input$fbi)
   if (input$fbs != "All") dd <- filter(dd,name==input$fbs)
   #if (input$fbt != "All") dd <- filter(dd,type==input$fbt)
   dd #[,c(ncol(dd),1:(ncol(dd)-1))]
  })
  
  output$p.bar <- renderPlotly({
    data <- data.frame(beta = colMeans(filter(d.beta())[,c(-1,-2)], na.rm = T),
                       gearing = colMeans(filter(d.gearing())[,c(-1,-2)], na.rm = T),
                       abeta = colMeans(filter(d.abeta())[,c(-1,-2)], na.rm = T))
    
    n <- ifelse(input$fbi != "All" | input$fbs != "All",
                nrow(d.beta()), nrow(d.beta()) - ncol(index))
    
    plot_ly(data, x=names(rollingMatrix())[c(-1,-2)], y=~beta, type='line', name="Equity beta") %>%
      add_trace(x=names(rollingMatrix())[c(-1,-2)], y=~gearing, type='line', name="Gearing") %>%
      add_trace(x=names(rollingMatrix())[c(-1,-2)], y=~abeta, type='line', name="Asset beta") %>%
      layout(xaxis=list(title="Rolling avg. period end date",nticks=10),
             yaxis=list(title="value", rangemode = "tozero"),
             title=paste0("# of firm: ",n))
  })
  
})






