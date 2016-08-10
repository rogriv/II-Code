# author Roman Rivera
library(lubridate)
library(plyr)

ac <- read.csv('june2016_all.csv', stringsAsFactors = FALSE)
ac$Incident_Month <-  months(as.POSIXlt(as.Date(as.character(ac$Incident_Datetime), '%Y-%m-%d')))
ac$Closed_Month <-  months(as.POSIXlt(as.Date(as.character(ac$Closed_Date), '%Y-%m-%d')))
ac$Complaint_Month <-  months(as.POSIXlt(as.Date(as.character(ac$Complaint_Date), '%Y-%m-%d')))
ac$Incident_Year <-  sapply(ac$Incident_Datetime, function(d) ifelse(is.na(d), d, substr(d,1,4)))
ac$Closed_Year <-  sapply(ac$Closed_Date, function(d) ifelse(is.na(d), d, substr(d,1,4)))
ac$Complaint_Year <-  sapply(ac$Complaint_Date, function(d) ifelse(is.na(d), d, substr(d,1,4)))

make.xtab.dataframe <- function(year, year.column, month.column, pc = 'Nope', sort = TRUE, year.columns = FALSE){
  ac. <- unique(ac[ac[[year.column]] == year, c('Complaint_Number', month.column, 'Accused_Complaint_Category')])
  ac.$Accused_Complaint_Category <- sapply(ac.$Accused_Complaint_Category, function(acc) ifelse(acc == '' | acc == ' ', 'Unknown', acc))
  d <- as.data.frame.matrix(table(ac.[['Accused_Complaint_Category']], ac.[[month.column]]), stringsAsFactors = FALSE)
  rn <- as.character(rownames(d))
  d <- rbind(d, colSums(d))
  rownames(d) <- c(rn,'Total')
  d$Total <- apply(d, 1, function(x) sum(x))
  if (sort){
  d <- d[order(d$Total, decreasing = TRUE),]
  }
  cn <- colnames(d)
  rn <- rownames(d)
  if (pc == 'Month'){
    d <- do.call(cbind, lapply(colnames(d), function(n) d[, n] / sum(d[, n])/2))
  }
  else if (pc == 'Category'){
    d <- do.call(rbind, lapply(rownames(d),  function(n) d[n ,] / sum(d[n , ])/2))
  }
  else if (pc == 'Total'){
    d <- do.call(cbind, lapply(colnames(d), function(n) d[, n] / d['Total', 'Total']))
  }
  rownames(d) <- rn
  colnames(d) <- cn
  cn <- c(as.character(sort(factor(cn, levels = month.name))), 'Total')
  d <- d[, cn]
  if (year.columns){
    colnames(d) <- paste(year, colnames(d), sep = '.')
  }
  cn <- colnames(d)
  d <- data.frame(round(d, 3), stringsAsFactors = FALSE)
  colnames(d) <- cn
  d$acc <- rownames(d)
  rownames(d) <- NULL
  return(d)
}

lapply(c('2012','2013','2014','2015','2016'), function(year) {
  types <- list(c('Incident_Year', 'Incident_Month'), c('Complaint_Year', 'Complaint_Month'), c('Closed_Year', 'Closed_Month'))
  lapply(types, function(type){
    pcs <- c('None', 'Total', 'Category', 'Month')
    lapply(pcs, function(pc){
      write.csv(make.xtab.dataframe(year, type[1], type[2], pc, sort = TRUE, year.columns = FALSE), paste('All Yearly Complaints', paste(year, substr(type[1], 1, nchar(type[1])-5), pc, 'csv', sep = '.'), sep = '/'))
    })
  })
})

lapply(list(c('Incident_Year', 'Incident_Month'), c('Complaint_Year', 'Complaint_Month'), c('Closed_Year', 'Closed_Month')), function(type){
  df.list <- lapply(c('2012','2013','2014','2015','2016'), function(year) {
    return(make.xtab.dataframe(year, type[1], type[2], 'None', sort = FALSE, year.columns = TRUE))
  })
  s <- Reduce(function(...) merge(...,
                                 by = "acc",
                                 all = T),
             df.list)
  rownames(s) <- s$acc
  s$acc <- NULL
  s[is.na(s)] <- 0
  write.csv(s, paste('All Yearly Complaints',paste('all','years',substr(type[1], 1, nchar(type[1])-5), 'csv', sep = '.'), sep = '/'))
  })