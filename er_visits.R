#Lib
library(naniar)
library(caret)
library(lubridate)
library(pastecs)
library(corrplot)
library(mgcv)
library(dlnm)
library(splines)
library(stats)
library(zoo)

#data import
dat <- read.csv ("C:/Users/jagad/Desktop/douglas_cnty_asth_htn_ER/analytic_dataset.csv", 
                 header = T,fileEncoding="UTF-8-BOM")

str(dat)

#data formating
dat$asthma_all<- as.numeric(dat$asthma_all)
dat$htn_all<- as.numeric(dat$htn_all)
dat$asthma_ped<- as.numeric(dat$asthma_ped)
dat$Pm1025Lc.Max<- as.numeric(dat$Pm1025Lc.Max)
dat$acres<- as.numeric(dat$acres)
dat$pres_burn<- as.numeric(dat$pres_burn)
dat$pm25_1hr_avg.1<- as.numeric(dat$pm25_1hr_avg.1)
dat$rh_max<- as.numeric(dat$rh_max)

#missing data
vis_miss(dat)

#temporal variables
dat$date <- as.Date(dat$date, format = "%m/%d/%Y")
dat$month<- as.factor(month(dat$date, label = T))
dat$year<- as.factor(format(dat$date, '%Y'))
dat$dow <- wday(as.Date(dat$date, format = "%m/%d/%Y"))
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
dat$wDay <- factor((weekdays(dat$date) %in% weekdays1), 
                       levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
dat$week_num<- week(ymd(dat$date))  
#year to quarter conversion use zoo library for as.nearmon function
yq <- as.yearqtr(as.yearmon(dat$date, "%Y %m/%d/") + 1/12)
dat$season <- factor(format(yq, "%q"), levels = 1:4, 
                    labels = c("winter", "spring", "summer", "fall"))


#filter missing values
dat_n <- na.omit(dat)

#descriptive stats
desc_stat<- stat.desc(dat_n, basic = F)

#REGRESSION COEFFICIENT
################## Continous variables ######################
ls(dat_n)

myvars <- c("asthma_all","asthma_ped","htn_all","Co.1Hr.Avg","Co.1Hr.Max","Co.8Hr.Avg",
            "Co.8Hr.Max","Max.Sol.Rad","Mean.Sol.Rad","No.Avg", "No.Max","Noy.Avg",
            "Noy.Max","Noyno.Avg","Noyno.Max","Ozone.1Hr.Avg","Ozone.1Hr.Max",
            "Ozone.8Hr.Avg","Ozone.8Hr.Max","Pm1025Lc.Avg","Pm1025Lc.Max","Pm10Tot.1Hr.Avg", 
            "pm25_1hr_avg","pm25_1hr_avg.1","pres_burn","rh_max","rh_mean",
            "so2_1hr_avg","so2_1hr_max","so2_5min_avg","so2_5min_max","tavg","tmax",
            "wind_sp_max","X_so2_3hr_blk_avg","X_so2_3hr_blk_max")
dat_cor<- dat_n[myvars]

M<-cor(dat_cor)

##################### CORR MATRIX- MATRIX FUNCTION #############
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(dat_cor)
head(p.mat[, 1:15])

#Correlation matrix
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="original", addrect = 2,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "pch", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)


