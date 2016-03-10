#' replaces species tef with a NA and runs tefMcmcglmm


species_replace <- function(tef_data,
							isotope = c("carbon","nitrogen"),
							formula = ~delta15N ~ source.iso.15N + diet.type + habitat,
							random = ~ animal + sp.col + tissue,
							prior) {


#####decide on the isotope###
	if((isotope == "carbon") == T){
		
	iso_term <-	c("delta13C")
		
	} else{ 
		if((isotope == "nitrogen") == T){

		iso_term <- c("delta15N")
			
			}}
	

taxa_list <- unique(tef_data$data$animal)

mod_full <- list()


for(i in 1:length(taxa_list)){

	
	spec_list <- taxa_list
	
	#remove the entire species
	tef_data_na      <- tef_data
	tef_data_na$data <- tef_data_na$data[tef_data_na$data$animal != spec_list[i],]
	tef_data_dropped <- tef_data$data[tef_data$data$animal == spec_list[i],]


	for(j in 1:(length(tef_data_dropped[,1]))){
	tef_data_comb_na <- rbind(tef_data_dropped[j,] , tef_data_na$data)
	
	tef_data_run <- tef_data_na
	tef_data_run$data <- tef_data_comb_na
	tef_data_run$data[1, iso_term] <- NA

	 

	output <- paste("spe_teff",j, tef_data_dropped[j,"animal"], sep = "_")
 
	 mod  <- tefMcmcglmm(mulTree.data = tef_data_run, formula = formula, random.terms = random, prior = prior, output = output)
	
	
		mod_full[[spec_list[i]]][[j]]<- mod$tef_global

	}	
}

return(list(mod_full  = mod_full))

}