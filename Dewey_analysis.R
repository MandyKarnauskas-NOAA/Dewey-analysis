#########################################################
#  M. Karnauskas - 30 June 2020                         #
#  Code for running the "Dewey analysis"                #
#  Processing of charter boat social media              #
#########################################################

# Set directory 
rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/Dewey_analysis")

# Install libraries ------------------------------------
# if ("RColorBrewer" %in% available.packages() == FALSE) { install.packages("RColorBrewer") } 
# if ("fields" %in% available.packages() == FALSE) { install.packages("fields") } 
# if ("vegan" %in% available.packages() == FALSE) { install.packages("vegan") } 
# if ("viridis" %in% available.packages() == FALSE) { install.packages("viridis") } 
# if ("stringr" %in% available.packages() == FALSE) { install.packages("stringr") }
# if ("pals" %in% available.packages() == FALSE) { install.packages("pals") } 
# if ("yarrr" %in% available.packages() == FALSE) { install.packages("yarrr") } 

library(yarrr)
library(pals)
library(RColorBrewer)
library(fields)
library(vegan)
library(MASS)
library(viridis)
library(stringr)
source("line2user.R")

# Load data --------------------------------------------
#d <- read.table("Charter_SouthFloridaKeys_9_22_20_906Count.csv", header = T, sep = ",", check.names = F, quote = ""); st <- "FL"
d <- read.table("Charter_NCVA_100520.csv", header = T, sep = ",", check.names = F, quote = ""); st <- "NC"

names(d)[grep("mahi", tolower(names(d)))]  
# check to make sure only columns are: "Unk_Mahi" "M_mahi_gaffers" "F_mahi_gaffers" "mahi_bailers"
        
if (st == "FL") { reglab <- "South Florida" } else { reglab <- "Morehead to Virginia Beach" }     
st
head(d)
tail(d)
names(d)

table(d$region, useNA = "always")
table(d$marina, useNA = "always")
table(d$company, useNA = "always")
table(d$photo_ID, useNA = "always")
table(d$year, useNA = "always")
table(d$month, useNA = "always")
table(d$day, useNA = "always")
table(d$photo_type, useNA = "always")
dim(d)
names(d)

if (sum(grepl("notes", tolower(names(d)))) > 0) { d <- d[,-grep("notes", tolower(names(d)))] }

table(d[9], useNA = "always")
for (i in 9:(ncol(d)))  {  d[which(is.na(d[i])), i] <- 0  }
table(d[9], useNA = "always")

d$Mahi_gaffers <- d$Unk_Mahi + d$M_mahi_gaffers + d$F_mahi_gaffers
head(cbind(d$Unk_Mahi, d$M_mahi_gaffers, d$F_mahi_gaffers, d$Mahi_gaffers))

table(rowSums(d[12:(ncol(d))], na.rm = T) == 0)  # check that all rows are filled out

names(d)[grep("ilefish", names(d))] <- "Tilefish"
names(d)[grep("Cero", names(d))] <- "Cero / Spanish mackerel"
names(d)

# Format dates -----------------------------------------
d$date <- as.Date(paste0(d$day, "/", sprintf("%02d", d$month)), "%d/%m")
d$doy <- as.numeric(strftime(d$date, format = "%j"))                        # convert to day of year

names(d)
names(d)[9:(ncol(d)-2)]         # check that this is all the species columns

# Plot number of mahi caught per trip ------------------

#bailers <- d$mahi_bailers 
#gaffers <- d$Mahi_gaffers
#bailers[bailers == 0] <- NA
#gaffers[gaffers == 0] <- NA
#b <- hist(bailers, breaks = seq(0, 75, 5))
#g <- hist(gaffers, breaks = seq(0, 75, 5))

png(filename=paste0(st, "_hist_combined.png"), units="in", width=7, height=3, pointsize=12, res=72*4)

m <- d$mahi_bailers + d$Mahi_gaffers
m[which(m == 0)] <- NA

hist(m, breaks = seq(0, 75, 5), freq = T,  xlab = "total number of dolphin per trip", 
     axes = F, col = 8, main = paste("distribution of mahi catch per trip -", reglab))
axis(1, at = seq(0, 75, 5), line = 0)
axis(2, las = 2)

#ba <- barplot(rbind(g$counts, b$counts), beside = F, axes = F, legend.text = c("gaffers", "bailers"),
#        xlab = "total number of dolphin per trip", ylab = "frequency", 
#    main = paste("distribution of catch per trip -", reglab), cex.main = 1)
#axis(1, at = c(ba - mean(diff(ba))/2, max(ba) + mean(diff(ba))/2), lab = seq(0, 75, 5), line = 0)
#axis(2, las = 2)

dev.off()

mean(m, na.rm = T); median(m, na.rm = T); max(m, na.rm = T)

# Create list of top species ---------------------------

sort(colSums(d[12:(ncol(d)-2)], na.rm=T))
length(sort(colSums(d[12:(ncol(d)-2)], na.rm=T)))
par(mar = c(6,5,1,1), mfrow = c(2,1))
b <- barplot(sort(colSums(d[12:(ncol(d)-2)], na.rm=T)), las=2, ylab = "total #")

nn <- 10   # define number of spp to be considered
abline(v = min(tail(b, nn))-0.5, col = 2)

colSums(d[12:(ncol(d)-2)] != 0)/nrow(d)
sort(colSums(d[12:(ncol(d)-2)] != 0)/nrow(d))*100
barplot(sort(colSums(d[12:(ncol(d)-2)] != 0)/nrow(d)), las = 2, ylab = "occurrence")

if (st == "NC")  { occ_rate <- 0.014 } else { occ_rate <- 0.05 }

which(sort(colSums(d[12:(ncol(d)-2)] != 0)/nrow(d)) > 0.02)
nn2 <- length(which(sort(colSums(d[12:(ncol(d)-2)] != 0)/nrow(d)) > occ_rate))  # define min occurance rate here
abline(v = min(tail(b, nn2))-0.5, col = 2)                                    # use 0.05 for FL   

splis <- names(sort(colSums(d[12:(ncol(d)-2)], na.rm = T), decreasing = T))[1:nn]    
spliso <- names(which(sort(colSums(d[12:(ncol(d)-2)] != 0)/nrow(d), decreasing = T) > occ_rate)) # 0.05 for FL, 0.01 for NC
splis
spliso

# Plot formatting specs ---------------------------------
dev.off()

leg <- read.csv("colors.csv", sep = ",", header = T, quote = "\"", stringsAsFactors = F)
leg$cols[3:24] <- transparent(leg$cols[3:24], trans.val = 0.3)

plot(rep(1, nn), 1:nn, col = leg$cols, pch = leg$pchs, cex = leg$cexs*3, axes=F)
text(rep(0.8, nn), 1:nn, leg$spleg[1:nn])

dlis <- c("15jan", "15mar", "15may", "15jul", "15sep", "15nov")
dy <- as.numeric(strftime(as.Date(dlis, "%d%b"), format = "%j"))

# Plot raw data -----------------------------------------

png(filename=paste0(st, "_raw.png"), units="in", width=7, height=5.5, pointsize=12, res=72*4)

mm <- unlist(lapply(splis, function(x) which(x == leg$spnam)))
cbind(splis, leg$spnam[mm])

plot(d$doy, d[,splis[1]],  xlab = "time of year", ylab = "number caught", 
     axes = F, ylim = c(0, (max(d[9:(ncol(d)-2)] + 0.1, na.rm = T))), col = 0, 
     main = paste("raw catch data -", reglab), cex.main = 1)
for (i in 1:nn) {  
  f <- d[,which(names(d) == splis[i])]
  f[f == 0] <- NA
  points(d$doy, f, col = leg$cols[mm][i], pch = leg$pchs[mm][i], cex = leg$cexs[mm][i])                        
  } 
axis(1, at = dy, lab = month.abb[c(1,3,5,7,9,11)])
la <- c(1:5, 10, 15, 20, 25)
#axis(2, las = 2, at = log(la), lab = la); box()
axis(2, las = 2); box()
legend("topleft", leg$spleg[mm], col = leg$cols[mm], pch = leg$pchs[mm], 
       pt.cex = leg$cexs[mm], ncol = 1, cex = 1, bty = "n")

dev.off()

# Barplot of usage across time of year ------------------

bimon <- c(paste0(1, month.abb), paste0(16, month.abb), "31Dec")
bimondoy <- sort(as.numeric(strftime(as.Date(bimon, "%d%b"), format = "%j")))
bimondoy[1] <- 0
table(diff(bimondoy))

d$monbin <- cut(d$doy, bimondoy)
table(d$monbin, useNA = "always")

png(filename=paste0(st, "_seasonal.png"), units="in", width=10.5, height=7, pointsize=12, res=72*4)

par(mfrow = c(2, 2), mar = c(2, 4, 3, 1), mgp = c(3, 1, 0))

# plot proportion of catch 
nummon <- apply(d[,splis], 2, function(x) tapply(x, d$monbin, sum, na.rm = T))
permon <- apply(nummon, 1, function(x) (x / sum(x)))

mm <- unlist(lapply(splis, function(x) which(x == leg$spnam)))
splis == leg$spnam[mm]

b <- barplot(permon, col = leg$cols[mm], xlim = c(1, 35), axes = F, space = 0, border = NA, names.arg = rep("", ncol(permon)),
             legend.text = leg$spleg[mm], args.legend = list(x = 24.3, y = 0.8, bty = "n", xjust = 0), ylab = "proportion of catch")
axis(1, at = c(b - mean(diff(b))/2, max(b) + mean(diff(b))/2)[seq(1, length(b)+2, 2)], 
     lab = c(paste(month.abb, 1), "Dec 31"), las = 2)
axis(2, las = 2)

# plot occurrence of catch
occmon <- apply(d[,spliso] > 0, 2, function(x) tapply(x, d$monbin, sum, na.rm = T))
promon <- apply(occmon, 1, function(x) (x / sum(x)))

mm <- unlist(lapply(rownames(promon), function(x) which(x == leg$spnam)))
cbind(rownames(promon), leg$spnam[mm])

b <- barplot(promon, col = leg$cols[mm], xlim = c(1, 35), axes = F, space = 0, border = NA, names.arg = rep("", ncol(promon)),
             legend.text = leg$spleg[mm], args.legend = list(x = 24.3, y = 0.8, bty = "n", xjust = 0), ylab = "proportion of occurrences")
axis(1, at = c(b - mean(diff(b))/2, max(b) + mean(diff(b))/2)[seq(1, length(b)+2, 2)], 
        lab = c(paste(month.abb, 1), "Dec 31"), las = 2)
axis(2, las = 2)

text(line2user(line=mean(par('mar')[c(2, 4)]), side=2), xpd=NA, cex=1.2, font=2,
     line2user(line=2, side=3), paste('average distribution of catch over year -', reglab))

# Plot smoothed data ------------------------------------
par(mgp = c(0.8, 1, 0), mar = c(2, 3.0, 1, 1))
if (st == "FL") { ylim1 <- 10; ylim2 <- 0.9 }  else { ylim1 <- 16; ylim2 <- 1.03 }

plot(d$doy, d[,splis[1]], ylim = c(0, ylim1), col = 0, xlim = c(1, 530),   # ylim 10 for FL, 18 for NC
     xlab = "", axes = F, ylab = "relative importance\n(total number)")
axis(1, at = dy, lab = month.abb[c(1,3,5,7,9,11)])
axis(1, at = c(0, 365), lab = c("", ""), tck = 0)

mm <- unlist(lapply(splis, function(x) which(x == leg$spnam)))
legend(x = 380, y = ylim1/1.4, xjust = 0, 
       leg$spleg[mm], pch = 15, pt.cex = 1.5, col = leg$cols[mm], ncol=1, bty= "n", cex = 1)
for (i in 1:nn)  { 
  y <- d[,which(names(d) == splis[i])]
  y[is.na(y)] <- 0  
  ks <- ksmooth(d$doy, y, "normal", bandwidth=50, range.x = c(1, 365), n.points = 365) 
  lines(ks$x, ks$y, col = leg$cols[mm][i], lwd = 4)  
}

plot(d$doy, d[,spliso[1]], ylim = c(0, ylim2), col = 0, xlim = c(1, 530),        # 0.9 for FL, 1.3 for NC
     xlab = "", axes = F, ylab = "relative importance\n(presence/absence)")
axis(1, at = dy, lab = month.abb[c(1,3,5,7,9,11)])
axis(1, at = c(0, 365), lab = c("", ""), tck = 0)

mm <- unlist(lapply(spliso, function(x) which(x == leg$spnam)))
legend(x = 380, y = ylim2/1.4, xjust = 0,
              leg$spleg[mm], pch = 15, pt.cex = 1.5, col = leg$cols[mm], ncol=1, bty= "n", cex = 1)
for (i in 1:length(spliso))  { 
  y <- d[,which(names(d) == spliso[i])]
  #y <- log(y)
  y[which(y > 1)] <- 1
  y[is.na(y)] <- 0  
  ks <- ksmooth(d$doy, y, "normal", bandwidth=50, range.x = c(1, 365), n.points = 365) 
  lines(ks$x, ks$y, col = leg$cols[mm][i], lwd = 4)  
}

dev.off()

# Plot correlations ----------------------------------

colSums(d[12:(ncol(d)-3)] != 0)/nrow(d)
sort(colSums(d[12:(ncol(d)-3)] != 0)/nrow(d))
par(mar=c(12, 4, 1, 1))
barplot(sort(colSums(d[12:(ncol(d)-3)] != 0)/nrow(d)), las = 2)
which(sort(colSums(d[12:(ncol(d)-3)] != 0)/nrow(d)) > 0.02)

splis <- names(which(colSums(d[12:(ncol(d)-3)] != 0)/nrow(d) > 0.01)) # define occ rate here
splis

spleg <- splis

for (i in 1:length(splis))  {
  if (grepl("_", splis[i]) == TRUE)  {
    spleg[i] <- paste(unlist(strsplit(splis[i], "_"))[1], unlist(strsplit(splis[i], "_"))[2])
  }
}
spleg <- str_to_sentence(spleg)
cbind(splis, spleg)
spleg[which(spleg == "Black sea")] <- "Black sea bass"

d1 <- d[,which(names(d) %in% splis)]
d1[is.na(d1)] <- 0
#plot(d1)

dcor <- cor(d1, method="spearman")
dcor[which(dcor==1)] <- NA
#dcor[lower.tri(dcor)] <- NA

par(mar=c(8,8,1,3))
image.plot(dcor, axes=F, 
           col = brewer.pal(n = 11, name = "RdBu"), 
           breaks=seq(-0.6, 0.6, length.out = 12))
axis(1, at = seq(0, 1, length.out = length(splis)), lab = spleg, las = 2)
axis(2, at = seq(0, 1, length.out = length(splis)), lab = spleg, las = 2)
mtext(side = 4, line = 1, "correlation coefficient")
box()

d2 <- as.matrix(d1)
d2[which(d2 >= 1)] <- 1
dcor2 <- cor(d2, method="spearman")
dcor2[which(dcor2==1)] <- NA

par(mar=c(8,8,1,3))
image.plot(dcor2, axes=F, 
           col = brewer.pal(n = 11, name = "RdBu"), 
           breaks=seq(-0.6, 0.6, length.out = 12))
axis(1, at = seq(0, 1, length.out = length(splis)), lab = spleg, las = 2)
axis(2, at = seq(0, 1, length.out = length(splis)), lab = spleg, las = 2)
mtext(side = 4, line = 1, "correlation coefficient")
box()

png(filename=paste0(st, "_cluster.png"), units="in", width=7, height=5, pointsize=12, res=72*4)

d3 <- t(d1)
rownames(d3) 
hc <- hclust(dist(d3, method = "binary"), method = "ward.D2")
#plot(hc)
par(mar = c(2, 5, 1, 1))
plot(hc, labels = spleg, xlab = "",
    main = paste("cluster analysis of catch composition -", reglab), cex.main = 1)

dev.off()

# The end ------------------------------