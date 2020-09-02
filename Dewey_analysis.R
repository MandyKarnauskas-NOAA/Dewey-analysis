#########################################################
#  M. Karnauskas - 30 June 2020                         #
#  Code for running the "Dewey analysis"                #
#  Processing of charter boat social media              #
#########################################################

# Set directory 
rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/Dewey_analysis")

# Install libraries ------------------------------------
if ("RColorBrewer" %in% available.packages() == FALSE) { install.packages("RColorBrewer") } 
if ("fields" %in% available.packages() == FALSE) { install.packages("fields") } 
if ("vegan" %in% available.packages() == FALSE) { install.packages("vegan") } 
if ("viridis" %in% available.packages() == FALSE) { install.packages("viridis") } 
if ("stringr" %in% available.packages() == FALSE) { install.packages("stringr") }
if ("pals" %in% available.packages() == FALSE) { install.packages("pals") } 
if ("yarrr" %in% available.packages() == FALSE) { install.packages("yarrr") } 

library(yarrr)
library(pals)
library(RColorBrewer)
library(fields)
library(vegan)
library(MASS)
library(viridis)
library(stringr)

# Load data --------------------------------------------
d <- read.table("Charter_SouthFloridaKeys_9_2_20_906Count.csv", 
#d <- read.table("Charter_PhotoCount_NC_VA.csv",  
             header = T, sep = ",", check.names = F, quote = "")

head(d)
tail(d)
names(d)

d$photo_type <- tolower(d$photo_type)

table(d$region, useNA = "always")
table(d$marina, useNA = "always")
table(d$company, useNA = "always")
table(d$photo_ID, useNA = "always")
table(d$year, useNA = "always")

table(d$photo_type, useNA = "always")
d <- d[which(d$photo_type == "layout"),]
dim(d)

totalMahiGaffers <- rowSums(d[,grep("mahi_gaffers", tolower(names(d)))], na.rm=T)
d <- d[,-grep("mahi_gaffers", tolower(names(d)))]
d <- d[,-grep("notes", tolower(names(d)))]

totalMahiGaffers[which(totalMahiGaffers == 0)] <- NA
d$mahi_gaffers <- totalMahiGaffers
names(d)

# Format dates -----------------------------------------
d$date <- as.Date(paste0(d$day, "/", sprintf("%02d", d$month)), "%d/%m")
d$doy <- as.numeric(strftime(d$date, format = "%j"))                        # convert to day of year

names(d)
names(d)[9:(ncol(d)-2)]         # check that this is all the species columns

# Create list of top species ---------------------------

sort(colSums(d[9:(ncol(d)-2)], na.rm=T))
length(sort(colSums(d[9:(ncol(d)-2)], na.rm=T)))
par(mar = c(6,5,1,1), mfrow = c(2,1))
b <- barplot(sort(colSums(d[9:(ncol(d)-2)], na.rm=T)), las=2, ylab = "total #")

nn <- 10   # define number of spp to be considered
abline(v = min(tail(b, nn))-0.5, col = 2)

colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d)
sort(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d))
barplot(sort(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d)), las = 2, ylab = "occurrence")
which(sort(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d)) > 0.01)

abline(v = min(tail(b, 10))-0.5, col = 2)

splis <- names(sort(colSums(d[9:(ncol(d)-2)], na.rm = T), decreasing = T))[1:nn]    

splis1 <- names(sort(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d), decreasing = T))[1:10]
splis
splis1
spleg <- splis
spleg1 <- splis1

for (i in 1:length(splis))  {
  if (grepl("_", splis[i]) == TRUE)  {
    spleg[i] <- paste(unlist(strsplit(splis[i], "_"))[1], unlist(strsplit(splis[i], "_"))[2])
  } }
for (i in 1:length(splis1))  {
  if (grepl("_", splis1[i]) == TRUE)  {
    spleg1[i] <- paste(unlist(strsplit(splis1[i], "_"))[1], unlist(strsplit(splis1[i], "_"))[2])
  } }

spleg <- str_to_sentence(spleg)
spleg1 <- str_to_sentence(spleg1)

spleg[grep("Amberjack", spleg)] <- "Amberjacks"
spleg[grep("ilefish", spleg)] <- "Tilefish"
spleg1[grep("Amberjack", spleg1)] <- "Amberjacks"
spleg1[grep("ilefish", spleg1)] <- "Tilefish"

cbind(splis, spleg)
cbind(splis1, spleg1)

# Plot formatting specs ---------------------------------
dev.off()
tab <- glasbey(26)[5:17]
cols <- c("#00000030", "#00000090", yarrr::transparent(tab[1:(nn-2)], trans.val = 0.5))
pchs <- c(17, 17, rep(19, nn-2))
cexs <- c(1.2, 1.4, rep(1, nn-2))
cexs[which(splis == "wahoo")] <- 1.3
pchs[which(splis == "wahoo")] <- 18

cbind(splis, spleg, cols, pchs, cexs)
plot(rep(1, nn), 1:nn, col = cols, pch = pchs, cex = cexs, axes=F)
text(rep(0.9, nn), 1:nn, spleg)

dlis <- c("15jan", "15mar", "15may", "15jul", "15sep", "15nov")
dy <- as.numeric(strftime(as.Date(dlis, "%d%b"), format = "%j"))

# Plot raw data -----------------------------------------
plot(d$doy, d[,splis[1]],  xlab = "time of year", ylab = "number caught", 
     axes = F, ylim = c(0, (max(d[9:(ncol(d)-2)] + 1, na.rm = T))), col = 0)
for (i in 1:nn) { 
  points(d$doy, (d[,which(names(d) == splis[i])]), col = cols[i], pch = pchs[i], cex = cexs[i])                        
  } 
axis(1, at = dy, lab = month.abb[c(1,3,5,7,9,11)])
la <- c(1:5, 10, 15, 20, 25)
#axis(2, las = 2, at = log(la), lab = la); box()
axis(2, las = 2); box()
legend("topleft", spleg, col = cols, pch = pchs, pt.cex = cexs, ncol = 1)


# Plot smoothed data ------------------------------------
par(mfrow = c(1,2), mar = c(3,3,1,1))
plot(d$doy, d[,splis[1]], ylim = c(0, 11), col = 0, 
     xlab = "", ylab = "", axes = F)
mtext(side = 2, line = 0.5, "relative importance\n(total number)")
axis(1, at = dy, lab = month.abb[c(1,3,5,7,9,11)]); box()
legend("topleft", spleg, lwd=3, col=cols, ncol=1, bty= "n")
for (i in 1:nn)  { 
  y <- d[,which(names(d) == splis[i])]
  y[is.na(y)] <- 0  
  ks <- ksmooth(d$doy, y, "normal", bandwidth=50, range.x = c(1, 365), n.points = 365) 
  lines(ks$x, ks$y, col=cols[i], lwd=3)  
}

cols1 <- rep(NA, length(splis1))
for (i in 1:length(splis1))  { 
  if (splis1[i] %in% splis) {
  cols1[i] <- cols[which(splis == splis1[i])] }  else {
  cols1[i] <- yarrr::transparent(alphabet(20), trans.val = 0.5)[i] } }
cols1

plot(d$doy, d[,splis1[1]], ylim = c(0, 1), col = 0, 
     xlab = "", ylab = "", axes = F)
mtext(side = 2, line = 0.5, "relative importance\n(presence/absence)")
axis(1, at = dy, lab = month.abb[c(1,3,5,7,9,11)]); box()
legend("topleft", spleg1, lwd=3, col=cols1, ncol = 1, bty="n")
for (i in 1:length(splis1))  { 
  y <- d[,which(names(d) == splis1[i])]
  #y <- log(y)
  y[which(y > 1)] <- 1
  y[is.na(y)] <- 0  
  ks <- ksmooth(d$doy, y, "normal", bandwidth=50, range.x = c(1, 365), n.points = 365) 
  lines(ks$x, ks$y, col=cols1[i], lwd=3)  
}

dev.off()

# Plot monthly barplot ----------------------------------
m1 <- c()
for (i in 1:nn)  { 
  y <- d[,which(names(d) == splis[i])]
  y[is.na(y)] <- 0
  m <- tapply(y, d$month, sum, na.rm=T)
  mp <- m / sum(m)
  m1 <- cbind(m1, mp)
}

barplot(t(m1), beside=T, col=cols, names.arg = month.abb[1:12], 
        axes=F, #ylim=c(0, 1000), 
        ylab = "proportion of catch",
        main = "relative distribution of catch by month")
axis(2, las = 2)
#abline(h=0)
legend("topleft", spleg, col = cols, pch = 15, cex = 1.2)

# Plot correlations ----------------------------------

colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d)
sort(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d))
par(mar=c(12, 4, 1, 1))
barplot(sort(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d)), las = 2)
which(sort(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d)) > 0.01)

splis <- names(which(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d) > 0.01))
splis
spleg <- splis

for (i in 1:length(splis))  {
  if (grepl("_", splis[i]) == TRUE)  {
    spleg[i] <- paste(unlist(strsplit(splis[i], "_"))[1], unlist(strsplit(splis[i], "_"))[2])
  }
}
spleg <- str_to_sentence(spleg)

spleg[grep("Amberjack", spleg)] <- "Amberjacks"
spleg[grep("ilefish", spleg)] <- "Tilefish"

cbind(splis, spleg)

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

d3 <- t(d1)
rownames(d3) 
hc <- hclust(dist(d3, method = "binary"), method = "ward.D2")
#plot(hc)
plot(hc, labels = spleg, xlab = "")

# The end ------------------------------