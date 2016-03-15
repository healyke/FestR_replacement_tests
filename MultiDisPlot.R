MultiDisPlot<-function(data.list, probs=c(95, 50), col="black", xlim="auto", ...) {
  #Sanitizing
  
  #require
  require(hdrcde)
  
  #data.list
  if(class(data.list) != "list") {
    stop("'data.list' must be a list of numerical values.")
  }
  
  #probs
  if(class(probs) != "numeric") {
    stop("'probs' must be a numeric.")
  }
  
  #col
  if(class(col) != "character") {
    stop("'col' must be a character string.")
  } 
  
  #Calculate the y limits (optional
  if(xlim == "auto") {
    xlim<-c(min(unlist(data.list), na.rm=TRUE) - 0.01*min(unlist(data.list), na.rm=TRUE), max(unlist(data.list), na.rm=TRUE) + 0.01*(max(unlist(data.list), na.rm=TRUE)))
  }
  
  #Calculating the hdr
  hdr.results<-lapply(data.list, hdr, prob=probs)
  
  #Empty plot
  plot(1,1, ylim= c(1,length(data.list)), xlim=xlim, col="white", ...)
  
  #Adding the lines
  for (j in 1:length(data.list)) {
    temp <- hdr.results[[j]]
    shift=0
    #border="black"
    
    for (k in 1:length(probs)) {
      #Plot the probabilities distribution
      temp2 <- temp$hdr[k, ]
      
      #Lines options
      lines(c(j+shift,j+shift)~ c(min(temp2[!is.na(temp2)]), max(temp2[!is.na(temp2)])), lwd=1+(k*2-2), lty=(length(probs)-(k-1)), col=col)  
      points(j+shift~temp$mode,pch=19, col=col)
    }
  }
  
}
