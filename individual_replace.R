#' replaces species tef with a NA and runs tefMcmcglmm


individual_replace <- function(tef_data,
							isotope = c("carbon","nitrogen"),
							formula = ~delta15N ~ source.iso.15N + diet.type + habitat,
							random = ~ animal + sp.col + tissue,
							prior,
							output.label,
							nitt = c(120000),
							thin = c(50),  
							burnin = c(20000), 
							no.chains = c(2), 
							convergence = c(1.1), 
							ESS = c(1000)) {


#####decide on the isotope###
	if((isotope == "carbon") == T){
		
	iso_term <-	c("delta13C")
		
	} else{ 
		if((isotope == "nitrogen") == T){

		iso_term <- c("delta15N")
			
			}}
		

teff_diff <- matrix(0, nrow = length(tef_data$data[,1])) 
teff_full <- list()


for(i in 1:(length(tef_data$data[,1]))){
	
	temp_mul <-	tef_data
	replaced_indavidual <- temp_mul$data[i,] 
	replaced_indavidual[, iso_term] <- NA
	temp_mul$data <- temp_mul$data[-i,]
	temp_mul$data <- rbind(replaced_indavidual, temp_mul$data)
	temp_run <- temp_mul
	
	output <- paste(output.label,"ind_teff",i, replaced_indavidual[1], sep = "_")

	 mod  <- tefMcmcglmm(mulTree.data = temp_run, formula = formula.n, random.terms = random, prior = prior, output = output, nitt = nitt,  thin = thin,  burnin = burnin, no.chains = no.chains, convergence = convergence, ESS = ESS)
 
	 teff_diff[i] <- diff(c(mean(mod$tef_global), tef_data$data[i, iso_term]))
	 teff_full[[i]] <-mod$tef_global
	 
}




return(list(teff_diff = teff_diff, teff_full = teff_full))

}
