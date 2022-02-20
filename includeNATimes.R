includeNATimes <- function(x, dropTS = FALSE){
  xdf <- data.frame(datetime = index(x), value = coredata(x))
  emptydf <- data.frame(datetime = seq(index(x)[1], 
                                       last(index(x)), 
                                       by = as.numeric(index(x)[2])-as.numeric(index(x)[1])),
                        value = NA)
  # Left Join
  newdf <- merge(emptydf, xdf, all.x = TRUE)
  # Don't include just NA column
  xreturn <- newdf[,-2]
  
  if (dropTS == TRUE){
    return(xreturn) 
  } else{
    xtsreturn <- xts(zoo(xreturn[,2]), order.by = xreturn[,1])
    return(xtsreturn)
  }
}
