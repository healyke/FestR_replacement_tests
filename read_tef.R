
read_tef <- function(chain.name = c(),
         Tef.data = c("tef.aves.data.n")){

t_Liabs <- list()

model_grab <- list.files()
model_gsub <- list()
model_grab2 <- gsub("-tree.*\\.*","",model_grab,perl = TRUE)
model_grab_chain_name <- grep(c(chain.name),model_grab2,perl = TRUE)
model_grab_chain_name <- model_grab2[model_grab_chain_name]
un_gsub <- unique(model_grab_chain_name)



t_Liabs <- list()
for(i in 1:(length(un_gsub))){
  
  t_Liabs[[i]] <-	read.mulTree(mulTree.chain= un_gsub[i], extract = "Liab")
  
}


plot_delta <- unlist(plot_list(Tef.data)$delta.plot.list)


diff_mean <- list()
diff_int <- list()
for(i in 1:(length(plot_delta))){
  diff_mean[[i]] <- hdr(t_Liabs[[i]][[1]][,1])$mode - plot_delta[i]
  
  diff_int[[i]] <- hdr(t_Liabs[[i]][[1]][,1])$hdr - plot_delta[i]
  
}
}







