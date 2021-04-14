#Lib
library(naniar)
library(caret)
library(lubridate)
library(pastecs)
library(corrplot)
library(mgcv)
library(dlnm)
library(splines)
library(zoo)
library(dplyr)
#data import
dat <- read.csv ("C:/Users/jagad/Desktop/douglas_cnty_asth_htn_ER/analytic_dat.csv", 
                 header = T,fileEncoding="UTF-8-BOM")

#temporal variables
dat$date <- as.Date(dat$date, format = "%m/%d/%Y")
dat$month<- as.factor(month(dat$date))
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

#convert all numeric variables to numeric
con.names = dat %>% select_if(is.numeric) %>% colnames()
dat[,con.names] = data.frame(apply(dat[con.names], 2, as.numeric))

dat$dow<- as.factor(dat$dow)
dat$dow<- as.factor(dat$dow)

str(dat)

#missing data
vis_miss(dat)

#filter missing values
dat_n <- na.omit(dat)
dat_n<- subset(dat_n, select = -c(date))

#descriptive stats
desc_stat<- stat.desc(dat_n, basic = F)

#correlation matrix
# generating large feature matrix (cols=features, rows=samples)
num_features <- 79 # how many features
num_samples <- 526 # how many samples
dat_cor <- matrix(runif(num_features * num_samples),
                  nrow = num_samples, ncol = num_features)

# setting some dummy names for the features e.g. f23
colnames(dat_n) <- paste0("f", 1:ncol(dat_n))

# let's make 30% of all features to be correlated with feature "f1"
num_feat_corr <- num_features * .3
idx_correlated_features <- as.integer(seq(from = 1,
                                          to = num_features,
                                          length.out = num_feat_corr))[-1]
for (i in idx_correlated_features) {
  dat_n[,i] <- dat_n[,1] + runif(num_samples) # adding some noise
}

corrplot(cor(dat_n), diag = FALSE, order = "FPC",
         tl.pos = "td", tl.cex = 1, method = "color", 
         type = "upper", sig.level = 0.05, insig = "blank")

