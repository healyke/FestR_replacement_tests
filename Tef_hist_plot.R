Tef.hist.plot <- function(diff.data.ind = c(),
                          diff.data.spc = c(),
                          diff.mam.ind.c = c(),
                          diff.mam.ind.n = c(),
                          diff.aves.ind.c = c(),
                          diff.aves.ind.n = c(),
                          diff.mam.spc.c = c(),
                          diff.mam.spc.n = c(),
                          diff.aves.spc.c = c(),
                          diff.aves.spc.n = c(),
                          xlim = c(-5,5),
                          taxa = c(Aves, Mammalia),
                          isotope = c(carbon, nitrogen),
                          type = c(isotope, full)){
  if(type == "isotope"){
dev.new()
par(mfrow=c(2,2))

hdr.den(as.vector(unlist(diff.data.ind$diff_mean)), main = cat("Estimate:observed difference", taxa, isotope), xlab = "individual replacement", cex = 0.5, xlim= c(-5,5))
hdr.den(as.vector(unlist(diff.data.ind$diff_int)/2), cex = 0.5,main = cat("95% interval range", taxa, isotope), xlab = "individual replacement")
#hdr.den(as.vector(unlist(nmam_diff_ind_int50[])), cex = 0.5,main = "50% interval range Mammalia Nitrogen", xlab = "individual replacement")

hdr.den(as.vector(unlist(diff.data.spc$diff_mean)), main = cat("Estimate:observed difference", taxa, isotope),  xlab = "species replacement",cex = 0.5,  xlim= c(-5,5))
hdr.den(as.vector(unlist(diff.data.spc$diff_int)/2),cex = 0.5,main = cat("95% interval range", taxa, isotope), xlab = "species replacement")
#hdr.den(as.vector(unlist(nmam_diff_sp_int50[])),cex = 0.5,main = "50% interval range Mammalia Nitrogen", xlab = "species replacement")
  } else{
    dev.new()
    par(mfrow=c(2,4))

#####indavidual level
    hdr.den(as.vector(unlist(diff.aves.ind.c$diff_mean)), xlab = "aves carbon", ylab = "individual replacement", cex = 0.5, xlim= c(-5,5))
    hdr.den(as.vector(unlist(diff.aves.ind.n$diff_mean)), xlab = "aves nitrogen", cex = 0.5, xlim= c(-5,5))
    hdr.den(as.vector(unlist(diff.mam.ind.c$diff_mean)), xlab =  "mammalia carbon", cex = 0.5, xlim= c(-5,5))
    hdr.den(as.vector(unlist(diff.mam.ind.n$diff_mean)), xlab = "mammalia nitrogen", cex = 0.5, xlim= c(-5,5))

#####species level
    hdr.den(as.vector(unlist(diff.aves.spc.c$diff_mean)), xlab = "aves carbon",  ylab = "species replacement",cex = 0.5,  xlim= c(-5,5))
    hdr.den(as.vector(unlist(diff.aves.spc.c$diff_mean)), xlab = "aves nitrogen",cex = 0.5,  xlim= c(-5,5))
    hdr.den(as.vector(unlist(diff.mam.spc.c$diff_mean)), xlab = "mammalia carbon",cex = 0.5,  xlim= c(-5,5))
    hdr.den(as.vector(unlist(diff.mam.spc.c$diff_mean)), xlab = "mammalia nitrogen",cex = 0.5,  xlim= c(-5,5))
    
  }

}
