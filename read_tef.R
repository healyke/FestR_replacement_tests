
read_tef <- function(chain.name = c(),
         Tef.data = c(),
         no.chains = c(2),
         no.trees = c(10)){

t_Liabs <- list()

model_grab <- list.files()
model_gsub <- list()
model_grab2 <- gsub("-tree.*\\.*","",model_grab,perl = TRUE)
model_grab_chain_name <- grep(c(chain.name),model_grab2,perl = TRUE)
model_grab_chain_name <- model_grab2[model_grab_chain_name]
un_gsub <- unique(model_grab_chain_name)

#reorder the
order_sub <- list()
for(t in 1:(length(unique(Tef.data$data$animal)))){
  
  order_sub[[t]] <- un_gsub[grep(unique(Tef.data$data$animal)[t],un_gsub)]
  }
order_sub <- unlist(order_sub)


t_Liabs <- list()
t_chains <- list()
for(i in 1:(length(order_sub))){
  
  temp <-	read.mulTree(mulTree.chain= order_sub[i], extract = "Liab")
  for(j in 1:(length(no.chains*no.trees))){
    t_chains[[j]] <- temp[[j]][,1]
  }
  t_Liabs[[i]] <- as.mcmc(unlist(t_chains))
}

return(t_Liabs = t_Liabs)
}
