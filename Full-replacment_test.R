########################################################################################################
########################################### all #############################################
########################################################################################################
rm(list=ls())

library(FestR)
if(!require(devtools)) install.packages("devtools")
install_github("TGuillerme/mulTree", ref = "master")
library(mulTree)


#read in the data
mydata<-read.csv(file=system.file("extdata", "FestR_data.csv", package = "FestR"),
                 header=T, stringsAsFactors = F)

# re-order it into species
mydata <- mydata[order(mydata$species),]

###maybe I need to fix the data up.


# ----------------------------------------------------------------
#read in the two trees
# These two are the original as bundled with FestR
mammal.trees <- read.tree(system.file("extdata", "3firstFritzTrees.tre", package = "FestR"))
bird.trees   <- read.tree(system.file("extdata", "3firstJetzTrees.tre", package = "FestR"))

# TG: I just created a subsample of the BIG trees (all sp) to make it faster
#combine them together
combined.trees <- tree.bind (x = mammal.trees, y = bird.trees, sample = 10, root.age = 250)

prior_tef <- list(R = list(V = 1/4, nu=0.002), G = list(G1=list(V = 1/4, nu=0.002),
					G2=list(V = 1/4, nu=0.002), G3=list(V = 1/4, nu=0.002)))


#clean up data 
tef.aves.data.n <- tefMulClean(data = mydata, species_col_name = "species", trees =  combined.trees, taxonomic.class = "aves", isotope = "nitrogen")
tef.aves.data.c <- tefMulClean(data = mydata, species_col_name = "species", trees =  combined.trees, taxonomic.class = "aves", isotope = "carbon")

tef.mam.data.n <- tefMulClean(data = mydata, species_col_name = "species", trees =  combined.trees, taxonomic.class = "mammalia", isotope = "nitrogen")
tef.mam.data.c <- tefMulClean(data = mydata, species_col_name = "species", trees =  combined.trees, taxonomic.class = "mammalia", isotope = "carbon")




#set the forumula
formula.n <- delta15N ~ source.iso.15N + diet.type + habitat
formula.c <- delta13C ~ source.iso.13C + diet.type + habitat

formula.n_noiso <- delta15N ~  diet.type + habitat
formula.c_noiso <- delta13C ~  diet.type + habitat

random <- ~ animal + sp.col + tissue

######################################## calculate the indavidual removal #############################################

aves.nitrogen.ind <- individual_replace(tef_data = tef.aves.data.n, isotope = "nitrogen", formula = formula.n, random = random, prior = prior_tef, output.label = "aves_n")
aves.nitrogen.ind.noiso <- individual_replace(tef_data = tef.aves.data.n, isotope = "nitrogen", formula = formula.n_noiso, random = random, prior = prior_tef, output.label = "aves_n_noiso", nitt = c(1200000),  thin = c(500),  burnin = c(200000), no.chains = c(2), convergence =  c(1.1), ESS = c(1000))


aves.carbon.ind <- individual_replace(tef_data = tef.aves.data.c, isotope = "carbon", formula = formula.n, random = random, prior = prior_tef, output.label = "aves_c")
aves.carbon.ind.noiso <- individual_replace(tef_data = tef.aves.data.c, isotope = "carbon", formula = formula.c_noiso, random = random, prior = prior_tef, output.label = "aves_c_noiso", nitt = c(1200000),  thin = c(500),  burnin = c(200000), no.chains = c(2), convergence =  c(1.1), ESS = c(1000))



mammal.nitrogen.ind <- individual_replace(tef_data = tef.mam.data.n, isotope = "nitrogen", formula = formula.n, random = random, prior = prior_tef, output.label = "mam_n" )
mammal.nitrogen.ind.noiso <- individual_replace(tef_data = tef.mam.data.n, isotope = "nitrogen", formula = formula.n_noiso, random = random, prior = prior_tef, output.label = "mam_n_noiso" , nitt = c(1200000),  thin = c(500),  burnin = c(200000), no.chains = c(2), convergence =  c(1.1), ESS = c(1000))



mammal.carbon.ind <- individual_replace(tef_data = tef.mam.data.c, isotope = "carbon", formula = formula.n, random = random, prior = prior_tef, output.label = "mam_c")
mammal.carbon.ind.noiso <- individual_replace(tef_data = tef.mam.data.c, isotope = "nitrogen", formula = formula.c_noiso, random = random, prior = prior_tef, output.label = "mam_c_noiso", nitt = c(1200000),  thin = c(500),  burnin = c(200000), no.chains = c(2), convergence =  c(1.1), ESS = c(1000) )



######################################## calculate the species removal #############################################



aves.nitrogen.species <- species_replace(tef_data = tef.aves.data.n, isotope = "nitrogen", formula = formula.n, random = random, prior = prior_tef, output.label = "aves_n")
aves.nitrogen.species.noiso <- species_replace(tef_data = tef.aves.data.n, isotope = "nitrogen", formula = formula.n_noiso, random = random, prior = prior_tef, output.label = "aves_n_noiso", nitt = c(2400000),  thin = c(1000),  burnin = c(400000), no.chains = c(2), convergence =  c(1.1), ESS = c(1000))


aves.carbon.species <- species_replace(tef_data = tef.aves.data.c, isotope = "carbon", formula = formula.n, random = random, prior = prior_tef, output.label = "aves_c")
aves.carbon.species.noiso <- species_replace(tef_data = tef.aves.data.c, isotope = "carbon", formula = formula.c_noiso, random = random, prior = prior_tef, output.label = "aves_c_noiso", nitt = c(2400000),  thin = c(1000),  burnin = c(400000), no.chains = c(2), convergence =  c(1.1), ESS = c(1000))



mammal.nitrogen.species <- species_replace(tef_data = tef.mam.data.n, isotope = "nitrogen", formula = formula.n, random = random, prior = prior_tef, output.label = "mam_n")
mammal.nitrogen.species.noiso <- species_replace(tef_data = tef.mam.data.n, isotope = "nitrogen", formula = formula.n_noiso, random = random, prior = prior_tef, output.label = "mam_n_noiso", nitt = c(2400000),  thin = c(1000),  burnin = c(400000), no.chains = c(2), convergence =  c(1.1), ESS = c(1000))



mammal.carbon.species <- species_replace(tef_data = tef.mam.data.c, isotope = "carbon", formula = formula.n, random = random, prior = prior_tef, output.label = "mam_c")
mammal.carbon.species.noiso <- species_replace(tef_data = tef.mam.data.c, isotope = "carbon", formula = formula.c_noiso, random = random, prior = prior_tef, output.label = "mam_c_noiso", nitt = c(2400000),  thin = c(1000),  burnin = c(400000), no.chains = c(2), convergence =  c(1.1), ESS = c(1000))



###Plots
#set up a list with the rigth order of factors.
plot.lists.aves.n <- plot_list(tef.aves.data.n)
plot.lists.aves.c <- plot_list(tef.aves.data.c)






###needs to finished
dev.new()
par(mfrow=c(2,3))


par(mfrow=c(3,1))
par(mar=c(0.1, 7, 0.1, 0.1) + 0.1)
MultiDisPlot(mam_c_full, ylab = "Single observation estimate", xaxt = "n", xlab = "", bty = "n", ylim = c(-4,7))
#add actual estimates
points(seq(1:(length(mam_c_data $delta13C))), mam_c_data $delta13C, col = "red", pch = 16)

par(mar=c(0.1, 7, 0.1, 0.1) + 0.1)
MultiDisPlot(plot_mcd_full, ylab = "Species removal estimate", xaxt = "n", xlab = "", bty = "n", ylim = c(-4,7))
#add actual estimates
points(seq(1:(length(plot_mcd_species))), plot_mcd_delta, col = "red", pch = 16)

mtext("Mammal Carbon TEF", side = c(2), at = c(8), las = 3, cex =1, line=5, font=2)


mtext(mam_c_data$envirnment, side = c(1), at = c(seq(1:(length(plot_mcd_tissue)))), las = 3, cex =0.5, line=0.3)
mtext("Env", side = c(1), at = c(-1), las = 3, cex =0.5, line=0.3, font=2)

mtext(plot_mcd_tissue, side = c(1), at = c(seq(1:(length(plot_mcd_tissue)))), las = 3, cex =0.5, line=2)
mtext("Tissue", side = c(1), at = c(-1), las = 3, cex =0.5, line=2, font=2)

mtext(plot_mcd_diet, side = c(1), at = c(seq(1:(length(plot_mcd_diet)))), las = 3, cex =0.5, line= 4.5)
mtext("Diet", side = c(1), at = c(-1), las = 3, cex =0.5, line=4.5, font=2)

mtext(plot_mcd_species, side = c(1), at = c(seq(1:(length(plot_mcd_species)))), las = 3, cex =0.5, line=7.5)
mtext("Species", side = c(1), at = c(-1), las = 3, cex =0.5, line=7.5, font=2)



















