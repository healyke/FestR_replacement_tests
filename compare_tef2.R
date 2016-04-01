compare_tef <- function(Tef.data = c(),
                        isotope = c(carbon, nitrogen),
                        Tef.output = c()){

obv_delta <- unlist(plot_list(Tef.data, isotope = isotope)$delta.plot.list)

######Carbon = 1, Nitrogen = 3.5 this doesnt work
#standard_diff <- vector()
#if(isotope == "carbon"){
#  standard_diff <- obv_delta - 1
#} else{ 
#  standard_diff <- diff(obv_delta, 3.5)}

#######Use Caut
###need to fill in

#######


#####
diff_mean <- list()
diff_int <- list()
for(i in 1:(length(obv_delta))){
  diff_mean[[i]] <- hdr(Tef.output[[i]])$mode - obv_delta[i]
  
  diff_int[[i]] <- diff(hdr(Tef.output[[i]])$hdr["95%",])
  
}





return(list(diff_mean = diff_mean, diff_int = diff_int))
}







