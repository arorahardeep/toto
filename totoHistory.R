downloadResult <- function(fileUrl) {
  doc <- htmlTreeParse(fileUrl, useInternalNodes=TRUE)
  aus <- xpathSApply(doc, "//td[@class='auspfont']", xmlValue)
  inaus <- xpathSApply(doc, "//td[@class='inauspfont']", xmlValue)
  if (length(aus) && length(inaus)){
    return(c(aus,inaus))
  } else if (length(aus)) {
    return(aus)
  } else if (length(inaus)) {
    return(inaus)
  }
  
}

buildResults <- function(startDate) {
  library(XML)
  library(tidyr)
  library(dplyr)
  result <- data.frame(Date=as.Date(character()),
                       N1=as.integer(character()),
                       N2=as.integer(character()),
                       N3=as.integer(character()),
                       N4=as.integer(character()),
                       N5=as.integer(character()),
                       N6=as.integer(character()),
                       N7=as.integer(character()),
                       stringsAsFactors=FALSE)
  dStart <- as.Date(startDate,format="%d-%m-%Y")
  dEnd <- Sys.Date()
  allDates <- seq(dStart,to=dEnd, by='days')
  for (i in seq_along(allDates)) {
    if(weekdays(allDates[[i]]) == 'Thursday' || weekdays(allDates[[i]]) == 'Monday') {
      dt <- as.character(allDates[[i]],format="%d-%b-%Y(%a)")
      sUrl <- paste("http://sg.myfreepost.com/sgTOTO_get.php?drawdate=",dt,sep="")
      message(sUrl)
      nos <- downloadResult(sUrl)
      message(paste("[",nos,"]",sep=""))
      if (length(nos)) {
        result[nrow(result)+1,] <- data.frame(as.Date(dt,format="%d-%b-%Y(%a)"),
                                            as.integer(nos[1]),
                                            as.integer(nos[2]),
                                            as.integer(nos[3]),
                                            as.integer(nos[4]),
                                            as.integer(nos[5]),
                                            as.integer(nos[6]),
                                            as.integer(nos[7]))
      }
    }
  }
  message("Writing File...")
  th <- tbl_df(result)
  th_tidy <- gather(th, numID, numDrawn, N1:N7, na.rm=TRUE) %>% 
             mutate(numID = extract_numeric(numID)) %>% 
             arrange(as.Date(Date,format="%Y-%m-%d"),numID)
  write.table(result, file="totoHistory.csv",sep=",",append=TRUE,col.names=FALSE,row.names=FALSE)
  write.table(th_tidy,file="totoHistory_tidy.csv",sep=",",append=TRUE, col.names=FALSE, row.names=FALSE)
  return(result)
}