#' replaces species tef with a NA and runs tefMcmcglmm


individual_replace <- function(tef_data,
							isotope = c("carbon","nitrogen"),
							formula = ~delta15N ~ source.iso.15N + diet.type + habitat,
							random = ~ animal + sp.col + tissue,
							prior) {


#####decide on the isotope###
  ## AJ - the column names in these dataset are a mess!!
	if((isotope == "carbon") == T){
		
	iso_term <-	c("delta13C")
		
	} else{ 
		if((isotope == "nitrogen") == T){

		iso_term <- c("delta15N")
			
			}}
		

teff_diff <- matrix(0, nrow = length(tef_data$data[,1])) 
teff_full <- list()


for(i in 1:(length(tef_data$data[,1]))){
	
	data_na <- tef_data
	data_na$data[i, iso_term] <- NA
	
	output <- paste("teff",i,tef_mam_data_n$data[i,1], sep = "_")

	 mod  <- tefMcmcglmm(mulTree.data = data_na, formula = formula.n, random.terms = random, prior = prior_tef, output = output)
 
	 teff_diff[i] <- diff(c(mean(mod$tef_global), tef_data$data[i, iso_term]))
	 teff_full[[i]] <-mod$tef_global
	 
}




return(list(teff_diff = teff_diff, teff_full = teff_full))

}
