#Lib
#library(naniar)
#library(caret)
library(lubridate)
#library(corrplot)
library(zoo)
library(dplyr)
library(mgcv)
library(stats)
library(splines)
library(tsModel)
library(mda)
library(lattice)
#data import
dat <- read.csv ("C:/Users/jagad/Desktop/douglas_cnty_asth_htn_ER/criteria_poll/analytic_dataset.csv", 
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

#imputing missing values


#filter missing values
dat_nmis <- na.omit(dat)
dat_n<- subset(dat_nmis, select = -c(date, month, year, dow, wDay, week_num, season))

# TS variables: month,year, dow, wDay, week_num, season, pres_burn

dat_max_var <- subset(dat, select = c(asthma_ped, asthma_all, htn_all,
                                           Co.1Hr.Max, No.Max,Noy.Max, Noyno.Max, 
                                           Ozone.1Hr.Max, Pm1025Lc.Max,
                                           pm25_Max, so2_1hr_max,so2_5min_max, month,
                                           year, dow, wDay, week_num, season, pres_burn, 
                                           date, tmax))


dat_avg_var<- subset(dat_nmis, select = c(asthma_ped, asthma_all, htn_all,
                                          Co.1Hr.Avg, No.Avg, Noy.Avg, 
                                          Noyno.Avg, Ozone.1Hr.Avg, Pm1025Lc.Avg, 
                                          Pm10Tot.1Hr.Avg, pm25_1hr_avg, so2_5min_avg))

dat_pollen<- subset(dat_nmis, select = c(asthma_ped, asthma_all, htn_all,Alder, Birch,
                                         Chenopods.Pigweed, Ragweed..Ambrosia.,
                                         Trees.total, Grass.total, Weeds.total,Other.total, 
                                         Cat.tail, Cocklebur, Daisy.Group, Dandelion, 
                                         Dill, Dandelion, Dock, Elm, Hackberry, 
                                         Hemp, Hickory.Pecan, Horse.Chestnut, Juniper, 
                                         Linden, Locust, Maple, Mulberry, Nettle, Oak, 
                                         Osage.Orange, Pine, Plantain, Poplar.Cottonwood, 
                                         Prairie.Sage..Artemisia., Sedge, Sycamore, Walnut,
                                         Willow ))
                                          
dat_pollen_select<- subset(dat_nmis, select = c(asthma_ped, asthma_all, htn_all,
                                                Birch,Hackberry, Maple,
                                                Chenopods.Pigweed, Ragweed..Ambrosia.,
                                                Other.total))


M<-cor(dat_max_var)

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
p.mat <- cor.mtest(dat_max_var)
head(p.mat[, 1:15])

#Correlation matrix
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="alphabet", addrect = 2,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

######################################3
###########Models
f0<- glm(dat_max_var$asthma_ped ~ dat_max_var$Co.1Hr.Max+ dat_max_var$dow+
           dat_max_var$season+dat_max_var$pres_burn+ns(dat_max_var$tmax, 6)+
           ns(dat_max_var$date, 4*14),family = poisson)

#parametric model - mannual lag input
f0<- glm(dat_max_var$asthma_ped ~ dat_max_var$Co.1Hr.Max,family = poisson)
f1<- glm(dat_max_var$asthma_ped ~ Lag(dat_max_var$Co.1Hr.Max, 1),family = poisson)
f2<- glm(dat_max_var$asthma_ped ~ Lag(dat_max_var$Co.1Hr.Max, 2),family = poisson)
f3<- glm(dat_max_var$asthma_ped ~ Lag(dat_max_var$Co.1Hr.Max, 3),family = poisson)
f4<- glm(dat_max_var$asthma_ped ~ Lag(dat_max_var$Co.1Hr.Max, 4),family = poisson)
f5<- glm(dat_max_var$asthma_ped ~ Lag(dat_max_var$Co.1Hr.Max, 5),family = poisson)
f6<- glm(dat_max_var$asthma_ped ~ Lag(dat_max_var$Co.1Hr.Max, 6),family = poisson)
ss<- list(summary(f0), summary(f1), summary(f2), summary(f3), summary(f4),
          summary(f5), summary(f6))

models<-lapply(ss, function(x) x$coefficients[2, c("Estimate", "Std. Error")])
models

#parametric models - use lag range - distributed lag model (cumulative effect) ref pg:76
fit<- glm(dat_max_var$asthma_ped ~ Lag(dat_max_var$Co.1Hr.Max, 0:6)+
            dat_max_var$tmax, family = poisson)
summ<- summary(fit)

rn <- rownames(summ$coefficients)
i <- grep("Co.1Hr.Max", rn, fixed = TRUE)
coefs <- summ$coefficients[i, "Estimate"]
total <- sum(coefs)

############### Outcome displacement

maxlag<- 0:5

models<- sapply(maxlag, function(mlag) {
  fit<- glm(asthma_ped ~ Co.1Hr.Max+Lag(tmax, seq(0, mlag)),
            data=dat_max_var,family = poisson)
  summ<-summary(fit)
  summ.coef<- summ$coefficients["Co.1Hr.Max", 2]
  c(coef(fit)["Co.1Hr.Max"], summ.coef)
})

#producing a figure from above model
rng <- range(models[1, ] - 1.96 * models[2,], models[1, ] + 1.96 * models[2,], 0)
par(mar = c(4, 5, 1, 1))
plot(maxlag, models[1, ], type = "b",pch = 20, ylim = rng, 
     xlab = "Maximum temperature lag",
     ylab = expression(hat(beta) * " for " *PM[10] * " at lag 1"))
lines(maxlag, models[1, ] + 1.96 * models[2,], lty = 2)
lines(maxlag, models[1, ] - 1.96 * models[2,], lty = 2)
abline(h = 0, lty = 3)

#prescribed burn and non burn season analysis
maxlag <- 0:7

#burn and non-burn datasets
data.burn <- subset(dat_max_var, pres_burn %in% c("1"))
data.nburn <- subset(dat_max_var, pres_burn %in% c("0"))

#individual model for burn and non-burn pg 81
models.burn<- sapply(maxlag, function(mlag) {
  fit<- glm(asthma_ped ~ Co.1Hr.Max+Lag(tmax, seq(0, mlag)),
            data=data.burn,family = poisson)
  summ<-summary(fit)
  summ.coef<- summ$coefficients["Co.1Hr.Max", 2]
  c(coef(fit)["Co.1Hr.Max"], summ.coef)
})

models.nburn<- sapply(maxlag, function(mlag) {
  fit<- glm(asthma_ped ~ Co.1Hr.Max+Lag(tmax, seq(0, mlag)),
            data=data.nburn,family = poisson)
  summ<-summary(fit)
  summ.coef<- summ$coefficients["Co.1Hr.Max", 2]
  c(coef(fit)["Co.1Hr.Max"], summ.coef)
})

#figure from above model
trellis.par.set(theme = canonical.theme("pdf",FALSE))
y <- c(models.nburn[1, ], models.burn[1,])
xpts <- rep(maxlag, 2)
f<- gl(2, length(maxlag), labels = c("burn season","non-burn season"))

std <- c(models.nburn[2, ], models.burn[2,])
rng <- range(y - 1.96 * std, y + 1.96 *std, 0)
rng <- rng + c(-1, 1) * 0.05 * diff(rng)
ylab <- expression(hat(beta) * " for " *CO_1hr_avg * " at lag 1")

p <- xyplot(y ~ xpts | f, as.table = TRUE,
            ylim = rng, subscripts = TRUE, panel = function(x,y, subscripts, ...) {
              panel.xyplot(x, y, ...)
              llines(x, y - 1.96 * std[subscripts],lty = 2)
              llines(x, y + 1.96 * std[subscripts],lty = 2)
              panel.abline(h = 0, lty = 3)}, 
            xlab = "Maximum temperature lag",layout = c(1, 2), 
            type = "b", ylab = ylab,pch = 20)
            
print(p)



#####################################################3
######################################################
#GAM MODEL

#splines = 2 - 12 times number of study years
library(gam)
dfValues <- c(2, 4, 6, 8, 10, 12, 14)
control <- gam.control(epsilon = 0.00000001,bf.epsilon = 0.00000001)

modelsGAM <- sapply(dfValues, function(dfVal){
  total.df <- dfVal * 4
  fit <- gam(asthma_ped ~ Co.1Hr.Max + tmax + 
             s(date, total.df), data = dat_max_var,
             family = poisson, control = control)
  gamex <- gam.exact(fit)
  gamex.coef <- gamex$coefficients["Co.1Hr.Max","A-exact SE"]
  c(coef(fit)["Co.1Hr.Max"], gamex.coef)
               
})

######################################################
CO <- dat_max_var$Co.1Hr.Max
x<- unclass(dat_max_var$date)
use <- complete.cases(CO, x)
br.fit <- bruto(x[use], CO[use])
df.CO <- br.fit$df

asthma <- dat_max_var$asthma_ped
use <- complete.cases(asthma, x)
br.fit <- bruto(x[use], asthma[use])
df.asthma <- br.fit$df

fit1 <- gam(asthma_ped ~ Co.1Hr.Max + s(date,df.CO), 
            data = dat_max_var, family = quasipoisson)

fit2 <- gam(asthma_ped ~ Co.1Hr.Max + s(date,df.asthma), 
            data = dat_max_var, family = quasipoisson)

v1 <- gam.exact(fit1)
v2 <- gam.exact(fit2)







