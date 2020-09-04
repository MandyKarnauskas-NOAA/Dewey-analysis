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

# Load data --------------------------------------------
# d <- read.table("Charter_SouthFloridaKeys_9_2_20_906Count.csv", 
 d <- read.table("Charter__NCVA_8_12_20.csv",  
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
dim(d)
names(d)

d <- d[-which(names(d) == "Mahi_Gaffers")]
names(d)[grep("Unk_Mahi", names(d))] <- "mahi_gaffers"
totalMahiGaffers <- rowSums(d[,grep("mahi_gaffers", tolower(names(d)))], na.rm=T)
d <- d[,-grep("mahi_gaffers", tolower(names(d)))]
if (sum(grepl("notes", tolower(names(d)))) > 0) {d <- d[,-grep("notes", tolower(names(d)))]}

totalMahiGaffers[which(totalMahiGaffers == 0)] <- NA
d$mahi_gaffers <- totalMahiGaffers
names(d)
table(rowSums(d[9:(ncol(d))], na.rm = T) == 0)

names(d)[grep("ilefish", names(d))] <- "Tilefish"
names(d)[grep("Amberjack", names(d))] <- "Amberjacks"
names(d)

# Format dates -----------------------------------------
d$date <- as.Date(paste0(d$day, "/", sprintf("%02d", d$month)), "%d/%m")
d$doy <- as.numeric(strftime(d$date, format = "%j"))                        # convert to day of year

names(d)
names(d)[9:(ncol(d)-2)]         # check that this is all the species columns

# Plot number of mahi caught per trip ------------------

totMahi <- d$mahi_bailers + d$mahi_gaffers
hist(totMahi, xlab = "total number of dolphin per trip", breaks = seq(0, 75, 5), axes = F, main = "")
axis(1, at = seq(0, 75, 5), line = 0)
axis(2, las = 2)

# Create list of top species ---------------------------

sort(colSums(d[9:(ncol(d)-2)], na.rm=T))
length(sort(colSums(d[9:(ncol(d)-2)], na.rm=T)))
par(mar = c(6,5,1,1), mfrow = c(2,1))
b <- barplot(sort(colSums(d[9:(ncol(d)-2)], na.rm=T)), las=2, ylab = "total #")

nn <- 10   # define number of spp to be considered
abline(v = min(tail(b, nn))-0.5, col = 2)

colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d)
sort(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d))*100
barplot(sort(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d)), las = 2, ylab = "occurrence")
which(sort(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d)) > 0.01)

nn2 <- length(which(sort(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d)) > 0.01))  # define min occurance rate here
abline(v = min(tail(b, nn2))-0.5, col = 2)                                    # use 0.05 for FL   

splis <- names(sort(colSums(d[9:(ncol(d)-2)], na.rm = T), decreasing = T))[1:nn]    
spliso <- names(which(sort(colSums(!is.na(d[9:(ncol(d)-2)]))/nrow(d), decreasing = T) > 0.01)) # 0.05 for FL, 0.01 for NC
splis
spliso

# Plot formatting specs ---------------------------------
dev.off()

leg <- read.csv("colors.csv", sep = ",", header = T, quote = "\"", stringsAsFactors = F)
leg$cols[3:25] <- transparent(leg$cols[3:25], trans.val = 0.3)

plot(rep(1, nn), 1:nn, col = leg$cols, pch = leg$pchs, cex = leg$cexs*3, axes=F)
text(rep(0.8, nn), 1:nn, leg$spleg[1:nn])

dlis <- c("15jan", "15mar", "15may", "15jul", "15sep", "15nov")
dy <- as.numeric(strftime(as.Date(dlis, "%d%b"), format = "%j"))

# Plot raw data -----------------------------------------
plot(d$doy, d[,splis[1]],  xlab = "time of year", ylab = "number caught", 
     axes = F, ylim = c(0, (max(d[9:(ncol(d)-2)] + 0.1, na.rm = T))), col = 0)
mm <- unlist(lapply(splis, function(x) which(x == leg$spnam)))
for (i in 1:nn) {  
  points(d$doy, d[,which(names(d) == splis[i])], col = leg$cols[mm][i], pch = leg$pchs[mm][i], cex = leg$cexs[mm][i])                        
  } 
axis(1, at = dy, lab = month.abb[c(1,3,5,7,9,11)])
la <- c(1:5, 10, 15, 20, 25)
#axis(2, las = 2, at = log(la), lab = la); box()
axis(2, las = 2); box()
legend("topleft", leg$spleg[mm], col = leg$cols[mm], pch = leg$pchs[mm], pt.cex = leg$cexs[mm], ncol = 1)

#dev.print(png,"NC_raw.png", width = 700, height = 500)
dev.off()

# Barplot of usage across time of year ------------------

bimon <- c(paste0(1, month.abb), paste0(16, month.abb), "31Dec")
bimondoy <- sort(as.numeric(strftime(as.Date(bimon, "%d%b"), format = "%j")))
bimondoy[1] <- 0
table(diff(bimondoy))

d$monbin <- cut(d$doy, bimondoy)
table(d$monbin, useNA = "always")

par(mfrow = c(2, 2), mar = c(4, 4, 1, 1), mgp = c(3, 1, 0))

# plot proportion of catch 
nummon <- apply(d[,splis], 2, function(x) tapply(x, d$monbin, sum, na.rm = T))
permon <- apply(nummon, 1, function(x) (x / sum(x)))
mm <- unlist(lapply(splis, function(x) which(x == leg$spnam)))

b <- barplot(permon, col = leg$cols[mm], xlim = c(1, 35), axes = F, space = 0, border = NA, names.arg = rep("", ncol(permon)),
             legend.text = leg$spleg[mm], args.legend = list(x = "right", bty = "n"), ylab = "proportion of catch")
axis(1, at = c(b - mean(diff(b))/2, max(b) + mean(diff(b))/2)[seq(1, length(b)+2, 2)], 
     lab = c(paste(month.abb, 1), "Dec 31"), las = 2)
axis(2, las = 2)

# plot occurrence of catch
occmon <- apply(d[,spliso] > 0, 2, function(x) tapply(x, d$monbin, sum, na.rm = T))
promon <- apply(occmon, 1, function(x) (x / sum(x)))

mm <- unlist(lapply(rownames(promon), function(x) which(x == leg$spnam)))
b <- barplot(promon, col = leg$cols[mm], xlim = c(1, 35), axes = F, space = 0, border = NA, names.arg = rep("", ncol(promon)),
             legend.text = leg$spleg[mm], args.legend = list(x = "right", bty = "n"), ylab = "proportion of occurrences")
axis(1, at = c(b - mean(diff(b))/2, max(b) + mean(diff(b))/2)[seq(1, length(b)+2, 2)], 
        lab = c(paste(month.abb, 1), "Dec 31"), las = 2)
axis(2, las = 2)

# Plot smoothed data ------------------------------------
par(mgp = c(1.5, 1, 0))
plot(d$doy, d[,splis[1]], ylim = c(0, 18), col = 0, xlim = c(1, 550),   # ylim 10 for FL, 18 for NC
     xlab = "", axes = F, ylab = "relative importance\n(total number)")
axis(1, at = dy, lab = month.abb[c(1,3,5,7,9,11)])
axis(1, at = c(0, 365), lab = c("", ""), tck = 0)

mm <- unlist(lapply(splis, function(x) which(x == leg$spnam)))
legend("right", leg$spleg[mm], pch = 15, pt.cex = 1.5, col = leg$cols[mm], ncol=1, bty= "n", cex = 0.9)
for (i in 1:nn)  { 
  y <- d[,which(names(d) == splis[i])]
  y[is.na(y)] <- 0  
  ks <- ksmooth(d$doy, y, "normal", bandwidth=50, range.x = c(1, 365), n.points = 365) 
  lines(ks$x, ks$y, col = leg$cols[mm][i], lwd = 3)  
}

plot(d$doy, d[,spliso[1]], ylim = c(0, 1.3), col = 0, xlim = c(1, 550),        # 0.9 for FL, 1.3 for NC
     xlab = "", axes = F, ylab = "relative importance\n(presence/absence)")
axis(1, at = dy, lab = month.abb[c(1,3,5,7,9,11)])
axis(1, at = c(0, 365), lab = c("", ""), tck = 0)

mm <- unlist(lapply(spliso, function(x) which(x == leg$spnam)))
legend("right", leg$spleg[mm], pch = 15, pt.cex = 1.5, col = leg$cols[mm], ncol=1, bty= "n", cex = 0.9)
for (i in 1:length(spliso))  { 
  y <- d[,which(names(d) == spliso[i])]
  #y <- log(y)
  y[which(y > 1)] <- 1
  y[is.na(y)] <- 0  
  ks <- ksmooth(d$doy, y, "normal", bandwidth=50, range.x = c(1, 365), n.points = 365) 
  lines(ks$x, ks$y, col = leg$cols[mm][i], lwd = 3)  
}

dev.print(png,"NC_seasonal.png", width = 780, height = 600)
dev.off()

# Plot correlations ----------------------------------

colSums(!is.na(d[9:(ncol(d)-3)]))/nrow(d)
sort(colSums(!is.na(d[9:(ncol(d)-3)]))/nrow(d))
par(mar=c(12, 4, 1, 1))
barplot(sort(colSums(!is.na(d[9:(ncol(d)-3)]))/nrow(d)), las = 2)
which(sort(colSums(!is.na(d[9:(ncol(d)-3)]))/nrow(d)) > 0.02)

splis <- names(which(colSums(!is.na(d[9:(ncol(d)-3)]))/nrow(d) > 0.01)) # define occ rate here
splis

spleg <- splis

for (i in 1:length(splis))  {
  if (grepl("_", splis[i]) == TRUE)  {
    spleg[i] <- paste(unlist(strsplit(splis[i], "_"))[1], unlist(strsplit(splis[i], "_"))[2])
  }
}
spleg <- str_to_sentence(spleg)

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
par(mar = c(2, 5, 1, 1))
plot(hc, labels = spleg, xlab = "", main = "")

#dev.print(png,"FL_cluster.png", width = 780, height = 600)

# The end ------------------------------