

#************************
#
#  Graphics and Visualization
#
#***********************

# Packages

library(lattice)
library(GGally)
library(ggplot2)
library(MASS)
library(tcltk)
library(aplpack)
library(reshape2)
library(vcd)
library(mgcv)




#***********************
#
# Get the Train Data
#
#***********************

source("AccidentInput2.R")

totacts <- files2DF(".", "txt")

totactsClean <- traindataclean(totacts)

#***********************
#
# Built in Graphics
#
#***********************

#**********************************************************
#
#			Histograms
#
#*********************************************************

# default: Sturges

hist(totactsClean$TEMP, main = "Accident Temperature", xlab = "Temperature (Fahrenheit)", col = "steelblue" )


# FD

hist(totactsClean$TEMP, main = "Accident Temperature", xlab = "Temperature (Fahrenheit)", col = "steelblue", breaks = "FD" )

# Scott

hist(totactsClean$TEMP, main = "Accident Temperature", xlab = "Temperature (Fahrenheit)", col = "steelblue", breaks = "Scott" )

# 100


hist(totactsClean$TEMP, main = "Accident Temperature", xlab = "Temperature (Fahrenheit)", col = "steelblue", breaks = 10 )


par(mfrow = c(2,2))

hist(totactsClean$TEMP, breaks = "scott", main = "Accident Temperatures (Scott)", xlab = "Temp (F)", col = "steelblue")

hist(totactsClean$TEMP, breaks = "fd", main = "Accident Temperatures (FD)", xlab = "Temp (F)", col = "steelblue")

hist(totactsClean$TEMP, main = "Accident Temperatures (Sturges)", xlab = "Temp (F)", col = "steelblue")

hist(totactsClean$TEMP, breaks = 100, main = "Accident Temperatures (100)", xlab = "Temp (F)", col = "steelblue")

par(mfrow = c(1,1))


# Small multiples of histograms


par(mfrow = c(4,4))
sapply(1:14, function(i)hist(totactsClean[totactsClean$YEAR == i, "ACCDMG"], main = 2000+i, xlab = "Dollars ($)", col = "steelblue"))
#sapply(1:14, function(i)hist(totactsClean[totactsClean$Year == i, "ACCDMG"], main = paste("Damage in 20",i), xlab = "Dollars(S)", col = "steelblue"))
par(mfrow = c(1,1))


# Log
par(mfrow = c(4,4))
xmin=min(log(totactsClean[ "ACCDMG"]+1))
xmax=max(log(totactsClean[ "ACCDMG"]+1))
sapply(1:14, function(i)hist(log(totactsClean[totactsClean$YEAR == i, "ACCDMG"]+1), main = 2000+i, xlab = "Dollars ($)", col = "steelblue",xlim = c(xmin,xmax),ylim = c(0,950)))
#sapply(1:14, function(i)hist(totactsClean[totactsClean$Year == i, "ACCDMG"], main = paste("Damage in 20",i), xlab = "Dollars(S)", col = "steelblue"))
par(mfrow = c(1,1))


# Time series with symbols

par(mfrow = c(1,1))
symbols(min(totactsClean$YEAR):max(totactsClean$YEAR), tapply(totactsClean$ACCDMG, totactsClean$YEAR, sum), circles=tapply(totactsClean$ACCDMG, totactsClean$YEAR, max), inches=0.35, fg="white", bg="red", xlab="Year", ylab="Cost ($)", main = "Total and Maximum Accident Damage", add = F)
lines(min(totactsClean$YEAR):max(totactsClean$YEAR), tapply(totactsClean$ACCDMG, totactsClean$YEAR, sum))
text(x=13, y=3.4*10^8, labels='$18 m', col='black')
arrows(x0=13, y0=3.375*10^8, x1=13, y1=3.25*10^8, col='black', length=0.1, lwd=3)

text(x=5, y=2.375*10^8, labels='$4.6 m', col='black')
arrows(x0=6, y0=2.375*10^8, x1=8.5, y1=2.375*10^8, col='black', length=0.1, lwd=3)

# With legend

symbols(2001:2014, tapply(totactsClean$ACCDMG, totactsClean$YEAR, sum), circles=tapply(totactsClean$ACCDMG, totactsClean$YEAR, max), inches=0.35, fg="white", bg="red", xlab="Year", ylab="Cost ($)", main = "Total and Maximum Accident Damage", add = F)
lines(2001:2014, tapply(totactsClean$ACCDMG, totactsClean$YEAR, sum))

lines(x=c(2000.5, 2000.5, 2005.5, 2005.5, 2000.5), y = c(2.25e8, 2.7e8, 2.7e8, 2.25e8, 2.25e8), lwd = 1)
symbols(x = c(2004,2004), y=c(2.55e8, 2.35e8),  circles = c(1.8e7, 8e6), inches = 0.35, add = TRUE, fg = "white", bg = "red")
text(c(2002, 2002), c(2.55e8, 2.35e8), labels = c("$18M", "$8M"))

for (i in seq_len(nrow(mtcars))) {
  m <- mtcars[i, ]
  a[[i]] <- list(
    x = m$wt,
    y = m$mpg,
    text = rownames(m),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = 20,
    ay = -40
  )
}

# With annotation




#**********************************************************
#
#			Scatter Plot Matrix Functions
#
#*********************************************************



panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(abs(cor(x, y, use = "complete.obs")), 2)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 2)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y,
       col="steelblue2", ...)
}


uva.pairs <- function(vars, ...)
{
  args <- list(...)

  if(is.matrix(vars) | is.data.frame(vars)){
    if(is.null(args$labels))pairs(vars, lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = args$main)
    else(pairs(vars, lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = args$main, labels = args$labels))
  }
  else(if(is.character(vars)){
    if(is.null(args$labels))pairs(formula(vars), lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = args$main, data = args$data)
    else(pairs(formula(vars), lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = args$main, data = args$data, labels = args$labels))}
    else(cat("You must enter a matrix, dataframe or formula")))
}


# SPM approaches

pairs(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG", "TOTKLD", "TOTINJ")])

# uva.pairs

uva.pairs(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG", "TOTKLD", "TOTINJ")])


#lattice

splom(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG", "TOTKLD", "TOTINJ")])


# GGally

ggpairs(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG", "TOTKLD", "TOTINJ")])

# an approach providing correlation and histograms

uva.pairs(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG", "TOTKLD", "TOTINJ")])

# printing

setwd("path")

png("spmpairs.png")
pairs(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG", "TOTKLD", "TOTINJ")])
dev.off()



#**************************************
#
#		Parallel Coordinates
#
#*************************************

parallelplot(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG", "TOTINJ")])


parcoord(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG", "TOTINJ")], col = rainbow(30), var.label = T)


parallelplot(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG", "TOTINJ")], groups = totactsClean$CAUSE, key = simpleKey(levels(totactsClean$CAUSE)), space = "top")


# GGally

ggparcoord(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG", "TOTINJ")], columns = 1:5)

ggparcoord(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG", "TOTINJ", "CAUSE")], columns = 1:5, groupColumn = 6 )

ggparcoord(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG", "TOTINJ", "CAUSE")], columns = 1:5, groupColumn = 6, scale = "robust" )


#*************************************************
#
#   Glyphs
#
#*************************************************


accidents <- sample(1:nrow(totactsClean), 8)

accidents <- c(accidents, which(totactsClean$ACCDMG > 1e7)[1])

dates <- rep(NA, length(accidents))
dates <- sapply(1:length(accidents), function(i){
  dates[i] <- paste(totactsClean[i, "MONTH"], totactsClean[i, "DAY"],totactsClean[i, "YEAR"], sep = "/")
})

dates <- sapply(accidents, function(i){
  dates[i] <- paste(totactsClean[i, "MONTH"], totactsClean[i, "DAY"],totactsClean[i, "YEAR"], sep = "/")
})

faces(totacts[accidents, c("ACCDMG",  "TOTKLD", "TOTINJ", "CARS", "TRKDMG", "TONS", "TRNSPD")], labels = dates)

stars(totacts[accidents, c("ACCDMG",  "TOTKLD", "TOTINJ", "CARS", "TRKDMG", "TONS", "TRNSPD")], labels = dates, col.stars = rep("steelblue", 9))


# Cause

TotalCost <- tapply(totactsClean$ACCDMG, totactsClean$CAUSE, sum)
Killed <- tapply( totactsClean$TOTKLD,totactsClean$CAUSE, sum)
Injured <- tapply( totactsClean$TOTINJ,totactsClean$CAUSE, sum)
Hazmat <- tapply( totactsClean$CARS, totactsClean$CAUSE,sum)
Weight <- tapply(totactsClean$TONS, totactsClean$CAUSE,sum)
Speed <- tapply(totactsClean$TRNSPD, totactsClean$CAUSE,sum)
CauseDF <- data.frame(TotalCost, Killed, Injured, Hazmat, Weight,  Speed)

faces(CauseDF )


#*************************************************
#
#   Graphics for Categorical Variables
#
#*************************************************

#*****************
# New Variables
#*****************

Type <- rep("Other", nrow(totactsClean))
Type[totactsClean$TYPE == "Derailment"] <- "Derail"
Type[totactsClean$TYPE == "Hwy-Rail"] <- "Hwy-Rail"
Type[totactsClean$TYPE == "HeadOn"|totactsClean$TYPE == "Readend"|totactsClean$TYPE == "Side"|totactsClean$TYPE == "Raking"] <- "Impact"

TrainType <- rep("Other", nrow(totactsClean))
TrainType[totactsClean$TYPEQ == "Freight"] <- "Freight"
TrainType[totactsClean$TYPEQ == "Passenger"|totactsClean$TYPEQ == "Commuter"] <- "Passenger"

table(Type)
table(TrainType)
#*****************
# Type of accident
#*****************

# Basic bar plot
barplot(table(totactsClean$TYPE)) #compare with the totacts plot


barplot(table(totactsClean$TYPE), horiz = T, col = "steelblue")

barplot(table(totactsClean$TYPE), xlab = "Type of Accident", main = "Number of Accidents by Type", horiz = F, col = rainbow(20), names.arg = as.factor(1:length(levels(totactsClean$TYPE))), legend.text = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))

barplot(table(totactsClean$TYPE), xlab = "Type of Accident", main = "Number of Accidents by Type", horiz = F, col = "steelblue", names.arg = as.factor(1:length(levels(totactsClean$TYPE))), , legend.text = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))

legend(10, 2e4, legend = c(" 1 = Derailment", " 2 = HeadOn", " 3 = Rearend", " 4 = Side", " 5 = Raking", " 6 = BrokenTrain", " 7 = Hwy-Rail", " 8 = GradeX", " 9 = Obstruction", "10 = Explosive", "11 = Fire","12 = Other","13 = SeeNarrative"))


#*****************
# Type of train
#*****************

barplot(table(totactsClean$TYPEQ), xlab = "Type of Train", main = "Number of Accidents by Train", horiz = F, col = rainbow(20), names.arg = as.factor(1:length(levels(totactsClean$TYPEQ))), legend.text = c("Freight", "Passenger", "Commuter", "Work", "Single", "CutofCars", "Yard", "Light", "Maint", "Maint of Way", "B","C","D","E"))

#*****************
# Cause
#*****************

barplot(table(totactsClean$CAUSE), xlab = "Type of Train", main = "Number of Accidents by Train", horiz = F, col = rainbow(5), names.arg = as.factor(1:length(levels(totactsClean$CAUSE))), legend.text = c("El & Mech","Human Fact.","Misc.","Signal","Hack"))

#*********************************
# Distributions for categorial variables
#*********************************

boxplot(ACCDMG~CAUSE, data = totactsClean)

boxplot(log(ACCDMG+1)~TYPE, data = totactsClean, col = rainbow(13), xlim = c(1,20))
legend(14, 17, legend = levels(totactsClean$TYPE), lwd = 2,col = rainbow(13))

qplot( TYPE, TRNSPD, data = totactsClean, geom = "boxplot", right)

qplot( TYPE, ACCDMG, data = totactsClean, geom = "boxplot", right)

qplot( TYPE, log(ACCDMG +1), data = totactsClean, geom = "boxplot", right)

qplot( TYPE, log(ACCDMG +1), data = totactsClean, geom = "boxplot", right)

# Accident TYPE

ggplot( totactsClean, aes(TYPE, TRNSPD, fill = TYPE)) + layer(geom = "boxplot" )

# Do this for Type and Train Type with titles
library(plyr)
totactsCleanClone = totactsClean
totactsCleanClone$TYPE=revalue(totactsCleanClone$TYPE , c("HeadOn"="Impact", "Rearend"="Impact", "Side"="Impact", "Raking"="Other", "Fire"="Other", "Explosive"="Other", "BrokenTrain"="Other", "Obstruction"="Other", "SeeNarrative"="Other","GradeX"="Other"))

ggplot( totactsCleanClone, aes(TYPE, TRNSPD, fill = TYPE)) + layer(geom = "boxplot" )


#*****************
# Type of accident
#*****************

# Basic bar plot
barplot(table(totactsClean$TYPE)) #compare with the totacts plot



#*****************
# Type of train
#*****************

barplot(table(totactsClean$TYPEQ)) #compare with the totacts plot

#*****************
# Cause
#*****************

barplot(table(totactsClean$Cause)) #compare with the totacts plot

# Put all bar plots together

totactsCleanClone = totactsClean
totactsCleanClone$TYPEQ = revalue(totactsCleanClone$TYPEQ, c("Yard"="Other","Commuter"="Other","Work"="Other","Single"="Other","CutofCars"="Other","Light"="Other","Maint"="Other","Maint of Way"="Other","B"="Other","C"="Other","D"="Other","E"="Other"))

ggplot( subset(totactsCleanClone, TYPEQ == "Passenger" | TYPEQ == "Freight" | TYPEQ == "Other"), aes(TYPEQ, TRNSPD, fill = TYPEQ)) + layer(geom = "boxplot" )

#*********************************
# Scatter Plots for categorial variables
#*********************************

# scatter plots

xyplot(ACCDMG~TRNSPD | CAUSE, data = totactsClean, type = c("p", "r"))

xyplot(ACCDMG~TRNSPD | CAUSE*Type, data = totactsClean, type = c("p", "r"))


# Repeat for TYPE and TYPEQ
xyplot(ACCDMG~TRNSPD | TYPE, data = totactsClean, type = c("p", "r"))



#*********************************
# Interaction with Speed
#*********************************

xyplot(ACCDMG~TRNSPD | cut(TONS, breaks = 5), data = totactsClean, type = c("p", "r"))
xyplot(ACCDMG~TRNSPD | cut(TONS, breaks = 3), data = totactsClean, type = c("p", "r"))


#  Extreme Damage

Speed <- cut(totactsClean$TRNSPD, c(min(totactsClean$TRNSPD),median(totactsClean$TRNSPD),max(totactsClean$TRNSPD)), include.lowest = T, labels = c("low speed", "high speed"))

Weight <- cut(totactsClean$TONS, c(min(totactsClean$TONS),median(totactsClean$TONS),max(totactsClean$TONS)), include.lowest = T, labels = c("light", "heavy"))

osize <- par()$pin # default size

par(pin = c(5.2,7.9))
interaction.plot(Speed, Weight, totactsClean$ACCDMG, ylab = "Cost ($)")
par(pin = osize)


#**************************************
#
#		Heatmap
#
#*************************************


# Matrix or density or heatmap of categorical variables

table(totactsClean$TYPE, totactsClean$TYPEQ)

heatmap(table(totactsClean$TYPE, totactsClean$TYPEQ))

heatmap(table(totactsClean$TYPE, totactsClean$TYPEQ), Colv = NA, Rowv = NA )


#		Chris Seidel's Matrix Plot
#		see http://www.phaget4.org/R/image_matrix.html
# 		also source("http://www.phaget4.org/R/myImagePlot.R")

source("http://www.phaget4.org/R/myImagePlot.R")

myImagePlot(table(totactsClean$CAUSE, totactsClean$TYPE), title = "No. of Accidents by Cause and Type of Accident")


myImagePlot(table(totactsClean$TYPE, totactsClean$TYPEQ), title = "No. of Accidents by Cause and Type of Accident")

# heat map gglot

qplot(x=Var1, y=Var2, data=melt(table(totactsClean$TYPE, totactsClean$TYPEQ), use="p"), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(0, 1.2e4))

qplot(x=Var1, y=Var2, data=melt(table(totactsClean$CAUSE, totactsClean$TYPE), use="p"), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(0, 1e4))


qplot(x=Var1, y=Var2, data=melt(cor(totactsClean[,c("ACCDMG", "TRNSPD", "TONS", "EQPDMG")], use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))


qplot(x=Var1, y=Var2, data=melt(table(totactsClean$TYPE, totactsClean$TYPEQ)), fill=value, geom="tile")+
  scale_fill_gradient2(limits=c(0, 1.5e4))



#**************************************
#
#		Mosaic plots
#
#*************************************

mosaic(table(TrainType, Type), shade = T, legend = T)


mosaic(table(TrainType, Speed), shade = T, legend = T)


mosaic(table(Weight, Speed), shade = T, legend = T)


mosaic(~CAUSE + Type  , data = totactsClean, shade = T, legend = T)

mosaic(~CAUSE + TrainType  , data = totactsClean, shade = T, legend = T)


#***********************
#
#   Smoothers and regressions
#
#***********************

#***************************
qplot(log(TRNSPD+1), log(TONS+1), data = totactsClean, color = CAUSE, xlab= "Log(Speed)", ylab = "Log(Tons)")



# Plots with regression lines & smoothers

qplot(TRNSPD, TONS, data = totactsClean, geom = c("point", "smooth"), method = "rlm")

qplot(TRNSPD, TONS, data = totactsClean, geom = c("point", "smooth"), method = "gam", forumula = y~s(x))

qplot(TRNSPD, TONS, data = totactsClean, geom = c("point", "smooth"), method = "gam", forumula = y~s(x, bs = "cs"))

# plots with groups

qplot(log(TRNSPD+1), log(TONS+1), data = totactsClean, color = Type, geom = c("point", "smooth"), method = "rlm")


qplot(log(TRNSPD+1), log(TONS+1), data = totactsClean, color = Type,geom = c("point", "smooth"), method = "gam", formula =  y ~ s(x, bs = "cs"), alpha = I(1/20))

qplot(log(TRNSPD+1), log(TONS+1), data = totactsClean, color = TYPE, alpha = I(1/5))




#################################
################################

# buiding a plot

###########
p <- ggplot(totactsClean, aes(x = TRNSPD, y = TONS, color = CAUSE), xlab = "Speed", ylab = "Tons")

p <- p + geom_point() + geom_smooth(method = lm)

p + xlab("Speed (mph)") + ylab("Weight (tons)") + ggtitle("Weight vs. Speed")

#########

# build a plot with GAM

p <- ggplot(totactsClean, aes(x = TRNSPD, y = TONS, color = CAUSE), xlab = "Speed", ylab = "Tons")

p <- p + layer(geom = "point") + layer(geom = "smooth",   method = "gam", forumula = y~s(x, bs = "cs"))

p + xlab("Speed (mph)") + ylab("Weight (tons)") + ggtitle("Weight vs. Speed")

##############

# build a plot with GAM with ACCDMG

p <- ggplot(totactsClean, aes(x = TRNSPD, y = log(ACCDMG+1), color = CAUSE), xlab = "Speed", ylab = "DMG")

p <- p + layer(geom = "point") + layer(geom = "smooth", method = "gam", forumula = y~s(x, bs = "cs"))

p + xlab("Speed (mph)") + ylab("Log(ACCDMG)") + ggtitle("DMG vs. Speed")

#############
# facets

p <- p + geom_point() + geom_smooth(method = lm)

p <- p+ facet_wrap(~CAUSE, ncol = 2)

p + xlab("Speed (mph)") + ylab("Weight (tons)") + ggtitle("Weight vs. Speed")
