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
combined.trees <- tree.bind (x = mammal.trees, y = bird.trees, sample = 3, root.age = 250)

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
formula.c_coiso <- delta13C ~  diet.type + habitat

random <- ~ animal + sp.col + tissue

######################################## calculate the indavidual removal #############################################

aves.nitrogen.ind <- individual_replace(tef_data = tef.aves.data.n, isotope = "nitrogen", formula = formula.n, random = random, prior = prior_tef, output.label = "aves_n")
aves.nitrogen.ind.noiso <- individual_replace(tef_data = tef.aves.data.n, isotope = "nitrogen", formula = formula.n_noiso, random = random, prior = prior_tef, output.label = "aves_n_noiso", nitt = c(1200000),  thin = c(500),  burnin = c(200000), no.chains = c(2), convergence =  c(1.1), ESS = c(1000))


aves.carbon.ind <- individual_replace(tef_data = tef.aves.data.c, isotope = "carbon", formula = formula.n, random = random, prior = prior_tef, output.label = "aves_c")
aves.carbon.ind.noiso <- individual_replace(tef_data = tef.aves.data.c, isotope = "carbon", formula = formula.c_noiso, random = random, prior = prior_tef, output.label = "aves_c_noiso", nitt = c(1200000),  thin = c(500),  burnin = c(200000), no.chains = c(2), convergence =  c(1.1), ESS = c(1000))



mammal.nitrogen.ind <- individual_replace(tef_data = tef.mam.data.n, isotope = "nitrogen", formula = formula.n, random = random, prior = prior_tef, output.label = "mam_n" )
mammal.nitrogen.ind.noiso <- individual_replace(tef_data = tef.mam.data.n, isotope = "nitrogen", formula = formula.n_noiso, random = random, prior = prior_tef, output.label = "mam_n_noiso" , nitt = c(1200000),  thin = c(500),  burnin = c(200000), no.chains = c(2), convergence =  c(1.1), ESS = c(1000))



mammal.carbon.ind <- individual_replace(tef_data = tef.mam.data.c, isotope = "carbon", formula = formula.c, random = random, prior = prior_tef, output.label = "mam_c")
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


###if data needs to be read back in.

aves_c_spc <- read_tef(chain.name = "aves_c_spe", Tef.data = c(tef.aves.data.c), no.trees = c(10), no.chains = c(2))
aves_c_ind <- read_tef(chain.name = "aves_c_ind", Tef.data = c(tef.aves.data.c), no.trees = c(3), no.chains = c(2))


plot_delta <- unlist(plot_list(tef.aves.data.n)$delta.plot.list)


diff_mean <- list()
diff_int <- list()
for(i in 1:(length(plot_delta))){
diff_mean[[i]] <- hdr(t_Liabs[[i]][[1]][,1])$mode - plot_delta[i]

diff_int[[i]] <- hdr(t_Liabs[[i]][[1]][,1])$hdr - plot_delta[i]
  
}





###Plots
Tef_plot(tef_data = tef.aves.data.n, ind.output = aves_c_ind, spc.output = aves_c_spc, title.main = c('Aves Nitrogen TEF'), xlim = c(0,8))








