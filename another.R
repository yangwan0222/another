
library("readxl")
data <- data.frame(read_excel("C:\\Users\\wanya\\Desktop\\123.xlsx"))


which(!(unique(data$Global.Spec.ID.Louie.E) %in% unique(data$Global.Spec.ID...2))==TRUE)
tail(!(unique(data$Global.Spec.ID.Louie.E) %in% unique(data$Global.Spec.ID...2)))

# http://www.sthda.com/english/wiki/cox-proportional-hazards-model
# https://www.datacamp.com/community/tutorials/survival-analysis-R#fifth

setwd("O:\\BATEMAN SHIPMENT 80 PLA replacement FEBRUARY 2022")
data <- read_excel("123.xlsx")
data <- data.frame(data)
data$GlobalSpecID <- substr(data$StorageLabel,1,nchar(data$StorageLabel)-3)
write.xlsx(data,"C:\\Users\\wanya\\Desktop\\123.xlsx")



#########################################################################

t <- data.frame(read_excel("C:\\Users\\wanya\\Desktop\\copy of Unblinidng list for the Abeta round robin study sent to Mike D.xls"))
hist(t$REMAIN, main = "Title of the histogram", sub = "Subtitle", xlab = "Aliquot Left", ylab = "Samples")

t1 <- data.frame(matrix(NA, nrow = 7, ncol = 2))
names(t1) <- c("range","count")
t1[,2] <- c(1,5,23,26,41,26,8)
t1[,1] <- c("4 - 6","6 - 8","8 - 10","10 - 12","12 - 14","14 - 16","16 - 18")

t1$range <- factor(t1$range, levels = t1$range)
ggplot(t1,aes(x=range,y=count))+
  geom_bar(stat = "identity") + 
  geom_col(color='black',fill='cyan3')+
  xlab('Aliquot Remain') + ylab("Samples Count") + ggtitle("Samples count and their remaining aliquot") + ylim(0,50)

#########################################################################

library("readxl")
data <- data.frame(read_excel("C:\\Users\\wanya\\Desktop\\Diadem Samples Freezer Location2.xlsx"))
data$Global.Spec.ID <- substr(data$Global.Spec.ID,1,nchar(data$Global.Spec.ID)-3)

for (i in 1:dim(data)[1])
{
  data$Global.Spec.ID[i] <- paste(data$Global.Spec.ID[i],'-',toString(sprintf("%02d",data$Aliquot)[i]),sep = "")
}

write.csv(data, file = "C:\\Users\\wanya\\Desktop\\456.csv")

#########################################################################

data1 <- read.delim("C:\\Users\\wanya\\Desktop\\Samples in order.txt")
data2 <- read.delim("C:\\Users\\wanya\\Desktop\\Samples missordered.txt")
data2$ID <- substr(data2$ID,2,nchar(data2$ID))

t <- which(data1[1,] == data2)
for (i in 2:593)
{
  t <- c(t,which(data1[i,] == data2))
}



#############################   emory   ############################################

d <- data.frame(read_excel("C:\\Users\\wanya\\Desktop\\emory1.xlsx"))

d1 <- data.frame(matrix(NA,nrow = nrow(d),ncol=3))

names(d1) <- c("PID.ID1","Specimen.Date","Global.Specimen.ID")

d_PID.ID1 <- unlist(lapply(strsplit(d$PID.ID1.Specimen.Date.Global.Specimen.ID, split=","), "[", 1))
d_Specimen.Date <- unlist(lapply(strsplit(d$PID.ID1.Specimen.Date.Global.Specimen.ID, split=","), "[", 2))
d_Global.Specimen.ID <- unlist(lapply(strsplit(d$PID.ID1.Specimen.Date.Global.Specimen.ID, split=","), "[", 3))

d1$PID.ID1 <- d_PID.ID1
d1$Specimen.Date <- d_Specimen.Date
d1$Global.Specimen.ID <- d_Global.Specimen.ID
d <- d1

emory <- data.frame(read_excel("C:\\Users\\wanya\\Desktop\\Copy of ADNI CSF shipment to EMORY UNIV. unblinded manifest 04122021.xlsx"))
d1 <- as.integer(unique(d$PID.ID1))
d2 <- d1[!d1 %in% emory$RID]

d1 <- data.frame(matrix(NA,nrow = length(d2),ncol=3))
names(d1) <- c("PID.ID1","Specimen.Date","Global.Specimen.ID")
d1$PID.ID1 <- d2

d2 <- d
d2$Global.Specimen.ID <- substr(d2$Global.Specimen.ID,1,nchar(d2$Global.Specimen.ID)-3)
d2 <- unique(d2)
d2$Specimen.Date <- as.integer(d2$Specimen.Date)
d2$PID.ID1 <- as.integer(d2$PID.ID1)

x <- c()

for(i in seq(nrow(d1)))
{
  t <- which(d2$PID.ID1 == d1$PID.ID1[i])
  d1[i,2] <- min(d2[t,2])
  t1 <- subset(d2,PID.ID1 == d1[i,1] & Specimen.Date == d1[i,2])[3]
  if(nrow(t1)>1)
  {
    x<-c(x,which(d2$PID.ID1 == d1[i,1] & d2$Specimen.Date == d1[i,2]))
    d1[i,3] <- subset(d2,PID.ID1 == d1[i,1] & Specimen.Date == d1[i,2])[3][1,]
  }
  else
  {
    d1[i,3] <- subset(d2,PID.ID1 == d1[i,1] & Specimen.Date == d1[i,2])[3][1,] 
  }
}
# check x
t <- paste(d1$Global.Specimen.ID,"-01",sep="")

#############################   ptau and qantarix and roche   ############################################
d <- data.frame(read_excel("C:\\Users\\wanya\\Desktop\\roc131\\UPENN 130 spl plasma pTau181 report 2022 04 15 - Copy.xls"))
d <- d[,-c(1:5,7,9,10)]
d$FBP[d$FBP<1.11] <- 0
d$FBP[d$FBP>=1.11] <- 1
names(d)[1] <- "pT181P"

longtest <- melt_roc(d, "FBP", c("pT181P","pT181P.QUANTERIX","pT181P.ROCHE"))

p <- ggplot(longtest, aes(d = D, m = M, color = name)) + geom_roc() + style_roc() +
  ggtitle(gsub("  ", 130, "pT181P and pT181P.QUANTERIX and pT181P.ROCHE (n=  )")) + 
  geom_abline(slope=1, intercept = 0, linetype = "dashed") + scale_color_manual(values=c("#CC6666","#9999CC","#1d91c0")) + 
  scale_x_continuous("1 - Specificity",breaks = seq(0, 1, by = .1)) +
  scale_y_continuous("Sensitivity",breaks = seq(0, 1, by = .1)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))

results1 <- optimal.cutpoints(X = "pT181P", status = "FBP",tag.healthy = 0, method = "Youden", data = d)
results2 <- optimal.cutpoints(X = "pT181P.QUANTERIX", status = "FBP",tag.healthy = 0, method = "Youden", data = d)
results3 <- optimal.cutpoints(X = "pT181P.ROCHE", status = "FBP",tag.healthy = 0, method = "Youden", data = d)

p+geom_point(aes(x=1-data.frame(summary(results1)[5])[3,1], y=data.frame(summary(results1)[5])[2,1]),shape = 18,size = 3,color="slateblue1") + # manually change
  geom_point(aes(x=1-data.frame(summary(results1)[5])[3,2], y=data.frame(summary(results1)[5])[2,2]),shape = 18,size = 3,color="slateblue1") + # manually change
  geom_point(aes(x=1-data.frame(summary(results1)[5])[3,3], y=data.frame(summary(results1)[5])[2,3]),shape = 18,size = 3,color="slateblue1") + # manually change
  geom_point(aes(x=1-data.frame(summary(results1)[5])[3,4], y=data.frame(summary(results1)[5])[2,4]),shape = 18,size = 3,color="slateblue1") +
  geom_point(aes(x=1-data.frame(summary(results1)[5])[3,5], y=data.frame(summary(results1)[5])[2,5]),shape = 18,size = 3,color="slateblue1") +
  geom_point(aes(x=1-data.frame(summary(results2)[5])[3,1], y=data.frame(summary(results2)[5])[2,1]),shape = 18,size = 3,color="tomato") +
  geom_point(aes(x=1-data.frame(summary(results3)[5])[3,1], y=data.frame(summary(results3)[5])[2,1]),shape = 18,size = 3,color="darkblue") +
  geom_point(aes(x=1-data.frame(summary(results3)[5])[3,2], y=data.frame(summary(results3)[5])[2,2]),shape = 18,size = 3,color="darkblue") +
  geom_point(aes(x=1-data.frame(summary(results3)[5])[3,3], y=data.frame(summary(results3)[5])[2,3]),shape = 18,size = 3,color="darkblue") +# manually change
  geom_label(label=paste("95%CI pT181P: ",data.frame(summary(results1)[5])[1,6],
                         "\n95%CI pT181P.QUANTERIX: ",data.frame(summary(results2)[5])[1,2],
                         "\n95%CI pT181P.ROCHE: ",data.frame(summary(results3)[5])[1,4]),
             x=0.45,
             y=0.15,
             label.padding = unit(0.5, "lines"), # Rectangle size around 
             label.size = 0.3,
             color = "black",
             fill="white",
             hjust = 0
  )

#############################   emory 1   ############################################

emory <- data.frame(read_excel("C:\\Users\\wanya\\Desktop\\Copy of ADNI CSF shipment to EMORY UNIV. unblinded manifest 04122021.xlsx"))
emory <- emory[,c(2,4,7)]

bl <- read.csv("C:\\Users\\wanya\\Desktop\\MasterTable\\data\\REGISTRY (1).csv")
bl <- bl[bl$VISCODE == "bl",]
bl <- bl[,c(3,12)]

all <- data.frame(read_excel("C:\\Users\\wanya\\Desktop\\emory1.xlsx"))
all$Global.Specimen.ID <- substr(all$Global.Specimen.ID,1,nchar(all$Global.Specimen.ID)-3)
all <- unique(all)

for(i in 1:nrow(all))
{
  t <- which(all$PID.ID1 == all$PID.ID1[i])
  t1 <- which(all[t,2] == min(all[t,2]))
  all[t,2] <- min(all[t,2])
  all[t,3] <- all[t[t1],3] # same sample, sample sp date, may have different G.S ID
}

all <- unique(all)
names(all)[1] <- "RID"
t1 <- bl[!bl$RID %in% emory$RID,]
t2 <- merge(t1,all,by = "RID")
t2$Global.Specimen.ID <- paste(t2$Global.Specimen.ID,"-01",sep="")
write.csv(t2,"C:\\Users\\wanya\\Desktop\\emory freezer location.csv")

##############################  Passing Bablok Regression  ###########################################

# https://www.r-bloggers.com/2015/09/deming-and-passing-bablok-regression-in-r/
# https://rowannicholls.github.io/python/statistics/agreement/passing_bablok.html
# At any specific point, CI = Slope's[UCI,LCI] + Intercept's[UCI,LCI]

d1 <- read.csv("C://Users//wanya//Desktop//adrc//p-Tau181 PLA ADRC study final results_2021 06 15 - Copy.csv")
d2 <- data.frame(read_excel("C://Users//wanya//Desktop//adrc//Copy ADRCSupplementalPullUPDATED BoxKey 2.11.2022 Results - Copy.xlsx"))
d1 <- d1[,c(2,6)]
d2 <- d2[,c(2,7)]

t <- merge(d2,d1,by = "INDDID")
names(t)[2] <- "r2022";names(t)[3] <- "r2021"
t$r2021 <- as.double(t$r2021)

library(mcr)
PB.reg <- mcreg(t$r2021,t$r2022, method.reg = "PaBa")
PB.reg@para
MCResult.plot(PB.reg, equal.axis = TRUE, 
              x.lab = "Result in 2021", y.lab = "Result in 2022", 
              points.col = "tan2", points.pch = 19, 
              ci.area = TRUE, ci.area.col = "#0000FF50", 
              main = "p-Tau181 pg/mL", sub = "", 
              add.grid = FALSE, points.cex = 1)

legend(7.75, 0.75, legend=c("R-squared = 0.953"),box.col="white")

##############################  Baseline plasma samples  ###########################################

r <- read.csv("C:\\Users\\wanya\\Desktop\\MasterTable\\data\\REGISTRY (1).csv")
r <- r[,c(3,6,12)]
r1 <- r[r$VISCODE2=="bl",]
r1$EXAMDATE <- as.Date(r1$EXAMDATE,"%m/%d/%Y")
r1$EXAMDATE <- gsub("-", "", r1$EXAMDATE)


library(readxl)
r <- data.frame(read_excel("C:\\Users\\wanya\\Desktop\\123.xlsx"))
r <- unique(r)
for(i in seq(nrow(r)))
{
  t <- r[r$RID == r$RID[i],]
  t <- t[!is.na(t$EXAMDATE),]
  r$EXAMDATE[i] <- min(t$EXAMDATE)
}
r <- unique(r)

r2 <- merge(r1,r,by=c("RID","EXAMDATE"))

r3 <- unique(r2)
write.csv(r3, file = "C:\\Users\\wanya\\Desktop\\baseline_samples.csv")
write.csv(r4[r4$RID %in% r3$RID,], file = "C:\\Users\\wanya\\Desktop\\456.csv")

r4 <- read.csv("C:\\Users\\wanya\\Desktop\\miRNA ADNI plasma additional samples.csv")

##############################  Read LDMS storage search results  ###########################################

library(readxl)

d <- data.frame(read_excel("C://Users//wanya//Desktop//StorageSearchResults_emory1.xls"))

d1 <- d[,8][!is.na(d[,8])]
d2 <- d1[! d1 %in% "Global \nSpecimenID"]

##############################  Pair Plasma to CSF  ###########################################

d1 <- data.frame(read_excel("C://Users//wanya//Desktop//Julie//pla.xlsx"))
d2 <- data.frame(read_excel("C://Users//wanya//Desktop//Julie//csf.xlsx"))

pla <- data.frame(read_excel("C://Users//wanya//Desktop//Julie//OHSU-J.Saugstad Lab. ADNI Plasma shipment manifest 11092020.xlsx"))
csf <- data.frame(read_excel("C://Users//wanya//Desktop//Julie//OHSU-J.Saugstad Lab. ADNI CSF shipment manifest 11092020.xlsx"))
pla <- pla[,c(3,4)]
csf <- csf[,c(3,4)]

colnames(pla)[1] <- "Global.Specimen.ID"
colnames(csf)[1] <- "Global.Specimen.ID"

pla <- merge(pla,d1,by="Global.Specimen.ID")
csf <- merge(csf,d2,by="Global.Specimen.ID")

t <- merge(pla,csf,by='ID')
write.csv(t,"C://Users//wanya//Desktop//Julie//matched results.csv")

library(dplyr)
library(tidyr)

before <- data.frame(
  attr = c(1, 30 ,4 ,6 ), 
  type = c('foo_and_bar', 'foo_and_bar_2')
)

before %>%
  separate(type, c("foo", "bar"), "_and_")

##############################  Read LDMS storage search results  ###########################################

library(readxl)
library(plotROC)
library(OptimalCutpoints)

d1 <- data.frame(read_excel("C://Users//wanya//Desktop//ptau//Bulldog pTau-181 analysis.xlsx"))
d2 <- data.frame(read_excel("C://Users//wanya//Desktop//ptau//Greyhound pTau-181 analysis.xlsx"))
d3 <- data.frame(read_excel("C://Users//wanya//Desktop//ptau//Poodle pTau-181 analysis.xlsx"))
d4 <- data.frame(read_excel("C://Users//wanya//Desktop//ptau//Terrier pTau-181 analysis.xlsx"))

d1 <-d1[,c(1,4)]
d2 <-d2[,c(1,4)]
d3 <-d3[,c(1,3)]
d4 <-d4[,c(1,5)]

d <- cbind(d1,d2,d3,d4)
d <-d[,c(1,2,4,6,8)]

t <- data.frame(read_excel("C://Users//wanya//Desktop//2021//Decemeber//03//FNIH//Copy of Unblinded Abeta round robin study Mike D with added columns 06_15_2021.xlsx"))
t <- t[,c(1,9,10)]
t <- t[t$DX!="NL",]

d$GUSPECID <- substr(d$GUSPECID,1,nchar(d$GUSPECID)-3)
t$GUSPECID <- substr(t$GUSPECID,1,nchar(t$GUSPECID)-3)

dt <- merge(t,d,by = "GUSPECID")
dt$FBP[dt$FBP<1.11] <- 0
dt$FBP[dt$FBP>=1.11] <- 1

longtest <- melt_roc(dt, "FBP", c("pTau.Bulldog","pTau.Greyhound","pTau.Poodle","pTau.Terrier"))

p <- ggplot(longtest, aes(d = D, m = M, color = name)) + geom_roc() + style_roc() +
  ggtitle(gsub("  ", 78, "pTau-181 (n=  )")) + 
  geom_abline(slope=1, intercept = 0, linetype = "dashed") + scale_color_manual(values=c("#CC6666","#9999CC","#1d91c0","cyan")) + 
  scale_x_continuous("1 - Specificity",breaks = seq(0, 1, by = .1)) +
  scale_y_continuous("Sensitivity",breaks = seq(0, 1, by = .1)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))

results1 <- optimal.cutpoints(X = "pTau.Bulldog", status = "FBP",tag.healthy = 0, method = "Youden", data = dt)
results2 <- optimal.cutpoints(X = "pTau.Greyhound", status = "FBP",tag.healthy = 0, method = "Youden", data = dt)
results3 <- optimal.cutpoints(X = "pTau.Poodle", status = "FBP",tag.healthy = 0, method = "Youden", data = dt)
results4 <- optimal.cutpoints(X = "pTau.Terrier", status = "FBP",tag.healthy = 0, method = "Youden", data = dt)

p+geom_point(aes(x=1-data.frame(summary(results1)[5])[3,1], y=data.frame(summary(results1)[5])[2,1]),shape = 18,size = 3,color="tomato") + # manually change
  geom_point(aes(x=1-data.frame(summary(results2)[5])[3,1], y=data.frame(summary(results2)[5])[2,1]),shape = 18,size = 3,color="slateblue1") + # manually change
  geom_point(aes(x=1-data.frame(summary(results3)[5])[3,1], y=data.frame(summary(results3)[5])[2,1]),shape = 18,size = 3,color="darkblue") + # manually change
  geom_point(aes(x=1-data.frame(summary(results4)[5])[3,1], y=data.frame(summary(results4)[5])[2,1]),shape = 18,size = 3,color="green") +
  geom_label(label=paste("95%CI Bulldog: ",data.frame(summary(results1)[5])[1,2],
                         "\n95%CI Greyhound: ",data.frame(summary(results2)[5])[1,2],
                         "\n95%CI Poodle: ",data.frame(summary(results3)[5])[1,2],
                         "\n95%CI Terrier: ",data.frame(summary(results4)[5])[1,2]),
             x=0.45,
             y=0.15,
             label.padding = unit(0.5, "lines"), # Rectangle size around 
             label.size = 0.3,
             color = "black",
             fill="white",
             hjust = 0
  )

##############################  Merge keep original order  ###########################################

df.1<-data.frame(class=c(1,2,3), prob=c(0.5,0.7,0.3))
df.2<-data.frame(object=c('A','B','D','F','C'), class=c(2,1,2,3,1))
df.2$id  <- 1:nrow(df.2)
out  <- merge(df.2,df.1, by = "class")
out[order(out$id), ]

##############################  Use table function to find element appear frequency  ###########################################

d <- data.frame(read_excel("C://Users//wanya//Desktop//pla csf GSID&PID1//pla.xlsx"))
d$Global.Specimen.ID <- substr(d$Global.Specimen.ID,1,nchar(d$Global.Specimen.ID)-3)

d1 <- as.data.frame(table(d$Global.Specimen.ID))

d2 <- data.frame(read_excel("C://Users//wanya//Desktop//roc131//UPENN 130 spl plasma pTau181 report 2022 04 15.xls"))
d2$ADNI.ID <- substr(d2$ADNI.ID,1,nchar(d2$ADNI.ID)-3)
d2 <- d2[,c(2,4)]

colnames(d2)[1] <- "Var1"

d3 <- merge(d2,d1,by="Var1")

##############################  Curve Fitting Example With Nonlinear Least Squares  ###########################################

p = function(x) x^3+2*x^2+5

x = seq(-0.99, 1, by = .01)
y = p(x) + runif(200)
df = data.frame(x = x, y = y)

fit = nls(y~a*x^2+b*x, data = df, start = list(a=0, b=0))

fit1 = nls(y~a*x^2+b*x+c, data=df, start=list(a=.5, b=0, c=1))
fit2 = nls(y~a*x^3+b*x^2+c, data=df, start=list(a=.1, b=.1, c=0))
fit3 = nls(y~a*exp(b*x^2)+c, data=df, start=list(a=1, b=1, c=0))

plot(x=df$x, y=df$y, pch=20, col="darkgray", main = "NLS fitting Example")
lines(df$x, predict(fit1, df), type="l", col="red", lwd=2)
lines(df$x, predict(fit2, df), type="l", col="green", lwd=2)
lines(df$x, predict(fit3, df), type="l", col="blue", lwd=2)

legend("topleft",  legend = c("y~ax^2+bx+c", "y~ax^3+bx^2+c", "y~a*exp(bx^2)+c"), 
       fill = c("red", "green","blue"), col = 2:3,  adj = c(0, 0.6))
grid()

##############################  Key word sort LDMS  ###########################################

d1 <- read.csv("C://Users//wanya//Desktop//3 vendor//ADNI plasma samples_5 or more_FBP_3 or more 04_01_21.csv")
d2 <- data.frame(read_excel("C://Users//wanya//Desktop//3 vendor//test2.xlsx"))

d1 <- d1[,c(1,2,14)]
library(stringr)
d1[c('month','day','year')] <- str_split_fixed(d1$EXAMDATE, '/', 3)
d1$day <- as.double(d1$day)
d1$month <- as.double(d1$month)
d1$day <- unlist(strsplit(toString(sprintf("%02d",d1$day)),", "))
d1$month <- unlist(strsplit(toString(sprintf("%02d",d1$month)),", "))
d1$RID <- unlist(strsplit(toString(sprintf("%04d",d1$RID)),", "))
d1$Specimen.Date <- paste(d1$year,d1$month,d1$day,sep = "")
d1 <- d1[,c(1,3,7)]
names(d2)[1] <- "RID"

d2$RID <- as.double(d2$RID)
d2$RID <- unlist(strsplit(toString(sprintf("%04d",d2$RID)),", "))
d2$Specimen.Date <- unlist(strsplit(toString(d2$Specimen.Date),", ")) # or use --> as.character(d2$Specimen.Date)

# find the last comment appearance 
for(i in seq(893))
{
  t <- subset(d2 , d2$RID == d1$RID[i] & d2$Specimen.Date == d1$Specimen.Date[i])
  if(sum(!is.na(t$Specimen.Comments))!=0)
  {
    d1$pla[i] <- nrow(t) - max(which(!is.na(t$Specimen.Comments))) 
  }
  else
  {
    d1$pla[i] <- nrow(t)
  }
}

# find the "sent" keyword
for(i in seq(893))
{
  t <- subset(d2 , d2$RID == d1$RID[i] & d2$Specimen.Date == d1$Specimen.Date[i])
  if(sum(!is.na(t$Specimen.Comments))!=0)
  {
    d1$pla[i] <- nrow(t) - nrow(t[grep("sent|pools|181|pTau|p-Tau", t$Specimen.Comments,ignore.case=TRUE), ]) # for multiple keyword use "a|b" instead
  }
  else
  {
    d1$pla[i] <- nrow(t)
  }
}

##############################  Mixture Model Gaussian find cut off point  ###########################################

d <- data.frame(read_excel("C://Users//wanya//Desktop//plot//NTK.xlsx"))
mixmdl <- mixtools::normalmixEM(d[,14], k = 2)
library(plotGMM)
plot_cut_point(mixmdl, plot = TRUE, color = "wesanderson")
plot_cut_point(mixmdl, plot = FALSE)

##
p1 <- ggplot(d_path, aes(x=group, y=ratio, color = group)) + geom_boxplot() +
  scale_color_manual(values=c("cornsilk3", "darkseagreen3", "gold3", "lightblue3", "mistyrose3")) + 
  #  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  labs(title="AB42/40 Ratio (Pathologic)\nN = 74",x="Clinical Group", y = "AB42/40 Ratio") +
  scale_x_discrete(labels=c("AD"="AD\nn=24", "FTLD"="FTLD\nn=13","HC"="HC\nn=23","MCI"="MCI\nn=6","PD"="PD\nn=8"))
##
boxplot(ratio~group,data=d_path, col="white", 
        main = "AB42/40 Ratio (Pathologic)\nN = 74", border=c("cornsilk3", "darkseagreen3", "gold3", "lightblue3", "mistyrose3"), 
        names = c("AD (n=24)","FTLD (n=13)","HC (n=23)","MCI (n=6)","PD (n=8)"),
        xlab="Clinical Group", ylab="AB42/40 Ratio")
stripchart(ratio~group,data = d_path,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 1:5,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)        # Add it over
grid()

################# tTau/AB42 ratio #################

d <- data.frame(read_excel("C://Users//wanya//Desktop//plot//NTK2.xlsx"))
d1 <- data.frame(read_excel("C://Users//wanya//Desktop//plot//tTauAB42.xlsx"))
d1 <- na.omit(d1)

d <- d[,c(5,6,8,9)]
d1 <- d1[,c(3,4,6,7)]
t <- merge(d,d1,by=c("Tube..","Cobas.ID","Dx"))
names(t)[3] <- "dig";names(t)[4] <- "ratio"

t$abeta[t$ratio<=0.0583] <- "Abeta +"
t$abeta[t$ratio>0.0583] <- "Abeta -"
t_ad <- t[t$dig == "AD",]
t_ftld <- t[t$dig == "FTLD",]
t_hc <- t[t$dig == "HC",]
t_mci <- t[t$dig == "MCI",]
t_pd <- t[t$dig == "PD",]

w <- paste("p = ",round(wilcox.test(t_ad$RATIO ~ t_ad$abeta)[[3]],digits = 4),sep = "")
p1 <- ggplot(t_ad, aes(x=abeta, y=RATIO, color = abeta)) + geom_boxplot() +
  scale_color_manual(values=c("cornsilk3", "darkseagreen3")) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  labs(title="AD",x=w, y = "Ratio")

w <- paste("p = ",round(wilcox.test(t_ftld$RATIO ~ t_ftld$abeta)[[3]],digits = 4),sep = "")
p2 <- ggplot(t_ftld, aes(x=abeta, y=RATIO, color = abeta)) + geom_boxplot() +
  scale_color_manual(values=c("cornsilk3", "darkseagreen3")) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  labs(title="FTLD",x=w, y = "Ratio")

w <- paste("p = ",round(wilcox.test(t_hc$RATIO ~ t_hc$abeta)[[3]],digits = 4),sep = "")
p3 <- ggplot(t_hc, aes(x=abeta, y=RATIO, color = abeta)) + geom_boxplot() +
  scale_color_manual(values=c("cornsilk3", "darkseagreen3")) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  labs(title="HC",x=w, y = "Ratio")

w <- paste("p = ",round(wilcox.test(t_mci$RATIO ~ t_mci$abeta)[[3]],digits = 4),sep = "")
p4 <- ggplot(t_mci, aes(x=abeta, y=RATIO, color = abeta)) + geom_boxplot() +
  scale_color_manual(values=c("cornsilk3", "darkseagreen3")) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  labs(title="MCI",x=w, y = "Ratio")

w <- paste("p = ",round(wilcox.test(t_pd$RATIO ~ t_pd$abeta)[[3]],digits = 4),sep = "")
p5 <- ggplot(t_pd, aes(x=abeta, y=RATIO, color = abeta)) + geom_boxplot() +
  scale_color_manual(values=c("cornsilk3", "darkseagreen3")) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  labs(title="PD",x=w, y = "Ratio")

m <- data.frame(matrix(nrow = 10,ncol = 3))
names(m)<-c("Mean","Std","95% CI")
rownames(m) <- c("AD a-","AD a+","FTLD a-","FTLD a+","HC a-","HC a+","MCI a-","MCI a+","PD a-","PD a+")

m[1,1] <- mean(t_ad$RATIO[t_ad$abeta == "Abeta -"])
m[2,1] <- mean(t_ad$RATIO[t_ad$abeta == "Abeta +"])
m[3,1] <- mean(t_ftld$RATIO[t_ftld$abeta == "Abeta -"])
m[4,1] <- mean(t_ftld$RATIO[t_ftld$abeta == "Abeta +"])
m[5,1] <- mean(t_hc$RATIO[t_hc$abeta == "Abeta -"])
m[6,1] <- mean(t_hc$RATIO[t_hc$abeta == "Abeta +"])
m[7,1] <- mean(t_mci$RATIO[t_mci$abeta == "Abeta -"])
m[8,1] <- mean(t_mci$RATIO[t_mci$abeta == "Abeta +"])
m[9,1] <- mean(t_pd$RATIO[t_pd$abeta == "Abeta -"])
m[10,1] <- mean(t_pd$RATIO[t_pd$abeta == "Abeta +"])

m[1,2] <- sd(t_ad$RATIO[t_ad$abeta == "Abeta -"])
m[2,2] <- sd(t_ad$RATIO[t_ad$abeta == "Abeta +"])
m[3,2] <- sd(t_ftld$RATIO[t_ftld$abeta == "Abeta -"])
m[4,2] <- sd(t_ftld$RATIO[t_ftld$abeta == "Abeta +"])
m[5,2] <- sd(t_hc$RATIO[t_hc$abeta == "Abeta -"])
m[6,2] <- sd(t_hc$RATIO[t_hc$abeta == "Abeta +"])
m[7,2] <- sd(t_mci$RATIO[t_mci$abeta == "Abeta -"])
m[8,2] <- sd(t_mci$RATIO[t_mci$abeta == "Abeta +"])
m[9,2] <- sd(t_pd$RATIO[t_pd$abeta == "Abeta -"])
m[10,2] <- sd(t_pd$RATIO[t_pd$abeta == "Abeta +"])

m$n <- 0
m[1,4] <- sum(t_ad$abeta == "Abeta -")
m[2,4] <- sum(t_ad$abeta == "Abeta +")
m[3,4] <- sum(t_ftld$abeta == "Abeta -")
m[4,4] <- sum(t_ftld$abeta == "Abeta +")
m[5,4] <- sum(t_hc$abeta == "Abeta -")
m[6,4] <- sum(t_hc$abeta == "Abeta +")
m[7,4] <- sum(t_mci$abeta == "Abeta -")
m[8,4] <- sum(t_mci$abeta == "Abeta +")
m[9,4] <- sum(t_pd$abeta == "Abeta -")
m[10,4] <- sum(t_pd$abeta == "Abeta +")

m$se <- m$Std/sqrt(m$n)
m$t_score <- qt(p=0.05/2, df=m$n-1,lower.tail=F)
m$margin_error <- m$t_score * m$se
m$lower_bound <- m$Mean - m$margin_error
m$upper_bound <- m$Mean + m$margin_error
m$`95% CI` <- paste("[",round(m$lower_bound,digits = 2)," ,",round(m$upper_bound,digits = 2),"]")

grid.arrange(p1,p2,p3,p4,p5,
             tableGrob(m[1:10, 1:4]),
             top = "tTau/AB42 Ratio",
             nrow = 2,
             ncol = 3,
             widths = c(1, 1, 1),
             clip = FALSE
)

################# unblind #################
d <- data.frame(read_excel("C://Users//wanya//Desktop//unblind//DIADEM_V2.xlsx"))
d$id  <- 1:nrow(d)
d1 <- data.frame(read_excel("C://Users//wanya//Desktop//unblind//unblind.xlsx"))
names(d)[2] <- "Global.Specimen.ID"

t <- merge(d,d1,by = "Global.Specimen.ID")
t <- t[order(t$id), ]
t1$Specimen.Date <- as.Date(t1[["Specimen.Date"]], "%Y%m%d") # convert to data format
# https://epirhandbook.com/en/working-with-dates.html

################# Diadem unblind #################

d <- read.csv("C://Users//wanya//Desktop//unblind//Unblinded_DIADEM_V2.csv")
d1 <- read.csv("C://Users//wanya//Desktop//unblind//adni_inventory.csv")
d1 <- d1[,c(3:6)]

library(lubridate)
d$Specimen.Date <- mdy(d$Specimen.Date)
d$ANALYSISDATE <- mdy(d$ANALYSISDATE)
names(d)[6] <- "EXAMDATE"
names(d)[4] <- "RID"
d1$EXAMDATE <- mdy(d1$EXAMDATE)
d1 <- unique(d1)

t <- merge(d,d1,by=c("RID","EXAMDATE"))

################# Shipment4 to Fischer #################

d1 <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//v2.xlsx"))
d2 <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//456.xlsx"))

d2$Specimen.Date <- as.character(d2$Specimen.Date)
d2$Specimen.Date <- as.Date(d2[["Specimen.Date"]], "%Y%m%d")
names(d2) <- c("RID","EXAMDATE","Global.Specimen.ID")
d2$Global.Specimen.ID <- substr(d2$Global.Specimen.ID,1,nchar(d2$Global.Specimen.ID)-3)
d2 <- unique(d2)

t <- merge(d1,d2,by = c("RID","EXAMDATE"))
t <- t[-c(117),]



library(readxl)

d <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//StorageSearchResults.xls"))

d1 <- d[,8][!is.na(d[,8])]
d2 <- d1[! d1 %in% "Global \nSpecimenID"]


################# Diadem #################

d1 <- data.frame(read_excel("C://Users//wanya//Desktop//diadem//DIADEM.xls"))
d2 <- read.csv("C://Users//wanya//Desktop//diadem//CDR.csv")
d3 <- data.frame(read_excel("C://Users//wanya//Desktop//unblind//unblind.xlsx"))
d4 <- read.csv("C://Users//wanya//Desktop//unblind//adni_inventory.csv")

names(d3) <- c("RID","Clinic","EXAMDATE","SAMPLEID")
d3 <- d3[,-c(2)]
d1$id <- 1:nrow(d1)
t <- merge(d1,d3,by="SAMPLEID")

d2 <- d2[,c(3,5,6,19,20)]
d4 <- d4[,c(3:6)]
d4 <- unique(d4)
t$EXAMDATE <- as.character(t$EXAMDATE)
t$EXAMDATE <- ymd(t$EXAMDATE)
d4$EXAMDATE <- mdy(d4$EXAMDATE)

t1 <- merge(t,d4,by=c("RID","EXAMDATE"))
t2 <- merge(t1,d2,by=c("RID","VISCODE","VISCODE2"))

###
d1 <- data.frame(read_excel("C://Users//wanya//Desktop//diadem//DIADEM.xls"))
d2 <- read.csv("C://Users//wanya//Desktop//diadem//MMSE.csv")
d3 <- data.frame(read_excel("C://Users//wanya//Desktop//unblind//unblind.xlsx"))
d4 <- read.csv("C://Users//wanya//Desktop//unblind//adni_inventory.csv")
d3 <- d3[,-c(2)]
names(d3) <- c("RID","EXAMDATE","SAMPLEID")

d1$id <- 1:nrow(d1)
t <- merge(d1,d3,by="SAMPLEID")

d2 <- d2[,c(3,5,6,10:75)]
d4 <- d4[,c(3:6)]
d4 <- unique(d4)
t$EXAMDATE <- as.character(t$EXAMDATE)
t$EXAMDATE <- ymd(t$EXAMDATE)
d4$EXAMDATE <- mdy(d4$EXAMDATE)
t1 <- merge(t,d4,by=c("RID","EXAMDATE"))
t2 <- merge(t1,d2,by=c("RID","VISCODE","VISCODE2"))

###
d1 <- data.frame(read_excel("C://Users//wanya//Desktop//diadem//DIADEM.xls"))
d1$id <- 1:nrow(d1)
names(d1)[6] <- "PTID"

d2 <- read.csv("C://Users//wanya//Desktop//MasterTable//Adni_B_2020-11-09.csv")
d2 <- d2[,c(2,9,33,36)]
d2 <- unique(d2)

d3 <- data.frame(read_excel("C://Users//wanya//Desktop//unblind//unblind.xlsx"))
d3 <- d3[,-c(2)]
names(d3) <- c("RID","EXAMDATE","SAMPLEID")


t <- merge(d1,d3,by="SAMPLEID")
t$EXAMDATE <- ymd(as.character(t$EXAMDATE))
names(d2)[2] <- "EXAMDATE"
t1 <- merge(t,d2,by = c("PTID","EXAMDATE"))

t2 <- t[!t$id %in% t1$id,]
write.csv(t1,"C://Users//wanya//Desktop//diadem//DIADEM_MMSE_CDRsob_part1.csv")


tt <- t2
tt$MMSE <- NA;tt$CDRsob <- NA;tt$DOV <- NA
for(i in seq(nrow(tt)))
{
  x <- d2[d2$PTID==tt[i,6],]
  tt$MMSE[i] <- x[1,3]
  tt$CDRsob[i] <- x[1,4]
  tt$DOV[i] <- x[1,2]
}


# When process time, use the below links:
# https://epirhandbook.com/en/working-with-dates.html
# https://www.geeksforgeeks.org/calculate-time-difference-between-dates-in-r-programming-difftime-function/#:~:text=Calculate%20Time%20Difference%20between%20Dates%20in%20R%20Programming%20%E2%80%93%20difftime()%20Function,-View%20Discussion&text=difftime()%20function%20in%20R,%2C%20months%2C%20years%2C%20etc.&text=units%3A%20Days%2C%20weeks%2C%20months%2C%20etc.

# When search key word using grep or grepl, use below link:
# https://statisticsglobe.com/grep-grepl-r-function-example

# when using github, use below link:
# https://codehorizons.com/making-your-first-github-r-project/