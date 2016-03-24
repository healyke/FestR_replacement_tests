plot_list <- function(tef_data,
                      isotope = c("carbon", "nitrogen")){

species.plot.list <- list()
diet.plot.list <- list()
tissue.plot.list <- list()
delta.plot.list <- list()
habitat.plot.list <- list()

taxa_list <- unique(tef_data$data$animal)


for(i in 1:length(levels(as.factor(taxa_list)))){

	spec_list <- taxa_list
	
	#remove the entire species
	tef_data_na      <- tef_data
	tef_data_na$data <- tef_data_na$data[tef_data_na$data$animal != spec_list[i],]
	tef_data_dropped <- tef_data$data[tef_data$data$animal == spec_list[i],]

	for(j in 1:(length(tef_data_dropped[,1]))){
	tef_data_comb_na <- rbind(tef_data_dropped[j,] , tef_data_na$data)
		
	
	species.plot.list[[spec_list[i]]][j] <-list(tef_data_comb_na[1,"animal"])
	diet.plot.list[[spec_list[i]]][j] <-list(tef_data_comb_na[1,"diet.type"])
	tissue.plot.list[[spec_list[i]]][j] <-list(tef_data_comb_na[1,"tissue"])
	if(isotope == "carbon"){
	  delta.plot.list[[spec_list[i]]][j] <-list(tef_data_comb_na[1,"delta13C"])
	} else {
	delta.plot.list[[spec_list[i]]][j] <-list(tef_data_comb_na[1,"delta15N"])}
	habitat.plot.list[[spec_list[i]]][j] <-list(tef_data_comb_na[1,"habitat"])

	}	
}

return(list(species.plot.list = species.plot.list, diet.plot.list = diet.plot.list, 
			tissue.plot.list = tissue.plot.list, delta.plot.list = delta.plot.list, habitat.plot.list = habitat.plot.list))
}
