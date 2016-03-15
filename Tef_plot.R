###Plots
#set up a list with the rigth order of factors.

Tef_plot <- function(tef_data,
                     ind.output, 
                     spc.output,
                     title.main = c('TEF'),
                     xlim = c(-4,8)){
  
plot.lists <- plot_list(tef_data)

ind.plot <- ind.output$teff_full
spc.plot <- as.list(as.data.frame(spc.output$mod_full))

###needs to finished
dev.new()
par(mfrow=c(3,2))
par(mfrow=c(1,3), oma = c(0.1,0.1,2,0.1) )
par(mar=c(0.1, 0.1, 5, 0.1) + 0.1)
plot.new()
par(mar=c(0.1, 3, 5, 0.1) + 0.1)

MultiDisPlot(ind.plot,  axes=FALSE ,ylab = "", bty = "n", xlim = c(-4,7))
axis(side = 3)
mtext(c("individual replacment"), side = c(3),cex =0.7, line=2.3)
#add actual estimates
points(seq(1:(length(unlist(plot.lists$delta.plot.list))))~ unlist(plot.lists$delta.plot.list), col = "red", pch = 16)

mtext(as.vector(unlist(plot.lists$species.plot.list)), side = c(2), at = c(seq(1:(length(unlist(plot.lists$species.plot.list))))),cex =0.5, line=12, las  = 1)
mtext("Species", side = c(2), at = c(length(unlist(plot.lists$habitat.plot.list)) +1), cex =0.5, line=12, font=2, las =1)

mtext(as.vector(unlist(plot.lists$diet.plot.list)), side = c(2), at = c(seq(1:(length(unlist(plot.lists$diet.plot.list))))),cex =0.5, line=8, las  = 1)
mtext("Diet", side = c(2), at = c(length(unlist(plot.lists$diet.plot.list)) +0.5), cex =1, line=8, font=2, las =1)

mtext(as.vector(unlist(plot.lists$tissue.plot.list)), side = c(2), at = c(seq(1:(length(unlist(plot.lists$tissue.plot.list))))),cex =0.5, line=5, las  = 1)
mtext("Tissue", side = c(2), at = c(length(unlist(plot.lists$tissue.plot.list)) +0.5), cex =1, line=5, font=2, las =1)

mtext(as.vector(unlist(plot.lists$habitat.plot.list)), side = c(2), at = c(seq(1:(length(unlist(plot.lists$habitat.plot.list))))),cex =0.5, line=1, las  = 1)
mtext("Habitat", side = c(2), at = c(length(unlist(plot.lists$habitat.plot.list)) +0.5), cex =1, line=1, font=2, las =1)

par(mar=c(0.1, 2, 5, 1) + 0.1)
#(bottem, left, top, rigth)
MultiDisPlot(spc.plot,  axes=FALSE ,ylab = "", bty = "n", xlim = c(-4,7))
axis(side = 3)
mtext(c("species replacment"), side = c(3),cex =0.7, line=2.3)
points(seq(1:(length(unlist(plot.lists$delta.plot.list))))~ unlist(plot.lists$delta.plot.list), col = "red", pch = 16)

mtext(title.main, side = c(3), cex =1, font=2, outer = TRUE)

}

