compare_tef <- function(Tef.data = c(),
                        isotope = c(carbon, nitrogen))

obv_delta <- unlist(plot_list(Tef.data)$delta.plot.list)

######Carbon = 1, Nitrogen = 3.5
if(isotope = "carbon"){
  standard_diff <- diff(obv_delta, 1)
} else{ 
  standard_diff <- diff(obv_delta, 3.5)}

#######Use Caut
###need to fill in
#######


#####
diff_mean <- list()
diff_int <- list()
for(i in 1:(length(plot_delta))){
  diff_mean[[i]] <- hdr(t_Liabs[[i]][[1]][,1])$mode - plot_delta[i]
  
  diff_int[[i]] <- hdr(t_Liabs[[i]][[1]][,1])$hdr - plot_delta[i]
  
}





return(list(t_Liabs = t_Liabs,diff_mean = diff_mean, diff_int = diff_int))
}







