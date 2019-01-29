# source("http://myweb.uiowa.edu/pbreheny/7210/f15/notes/fun.R")
Data <- read.delim("http://myweb.uiowa.edu/pbreheny/data/gvhd.txt")
dat <- Data[Data$Group == 'MTX',]
require(survival)
fit <- survfit(Surv(dat$Time, dat$Status)~1)


life_table <- function(time, status){
  n0 = length(time)
  time_points = unique(time)
  failure <- censor <- at_risk<-  integer(length(time_points))
  for (i in 1:length(time_points)){
    failure[i] = sum(status[time == time_points[i]] )
    censor[i] = sum(status[time == time_points[i]]==0 )
    if (i == 1) {
      at_risk[i] = n0
    }
    else{
      at_risk[i] = at_risk[i-1] - failure[i-1] - censor[i-1]
    }
  }
  data.frame(time_points = time_points, 
             at_risk = at_risk,
             failure = failure,
             censor = censor)
}

# test: compare with survival pacakge
life_table(dat$Time, dat$Status)
cbind(fit$time,fit$n.risk, fit$n.event, fit$n.censor)


# t: the time the function evaluate at
# table: life-table
# Output: The Kaplan-Meier estimate for the survival function
surv_KM <- function(t, lifeTable){
  hazard = (lifeTable$at_risk-lifeTable$failure)/lifeTable$at_risk
  prod(hazard[lifeTable$time_points<=t])
}

lifeTable =life_table(dat$Time, dat$Status)
surv_KM(31, lifeTable)
summary(fit, time = 31)

x = 1:100
y = numeric(100)
for (i in 1:100){
  y[i] = surv_KM(x[i], lifeTable)
}
plot(y~x, ylim = c(0, 1), type = 's', lwd = 3, col = 'red')
