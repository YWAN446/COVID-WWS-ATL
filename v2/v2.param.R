#parameters and settings;
#seed
ran.seed <- 1
#Number of new infections per day within the whole ward 58 & 59 is Poisson distributed;
#lambda <- 1 #lambda for the peak season, now assume no seasonality;

n.sim <- 100
n.pop <- 160000
n.household <-30000 #ask the student for details;
n.person.per.house <- 16/3
n.house.per.latrine <- 10
n.latrine <- n.pop/n.house.per.latrine/n.person.per.house

#parameters of simulation sewage network and latrine points
num.lines <- 1000 #The sewage lines will be set to 1000, 2500, 5000.
num.points <- n.latrine
total.vol <- 2e+10 #mL from Peter and Andrew's rough calculation;
#LLOD.test <- 2 #number 10^3 of DNA per 500 mL;
#LLOD.uf.test <- 0.025 #number 10^3 of DNA per 40L ultrafiltration samples;
#low sensitivity;
LLOD.test.low <- 2
LLOD.uf.test.low <- 0.025
#high sensitivity;
LLOD.test.high <- 0.02
LLOD.uf.test.high <- 0.00025

dir.store <- paste0("./",version,".SimIterative",Sys.Date(),".ssn")

#parameters of transmission;
mu.shed <- 10^8
sigma.shed <- 1
n.days.shed <- 14

#decay and lost parameters;
#low decay rate;
gamma.shape <- 1
gamma.rate <- 0.25

#high decay rate;
# gamma.shape <- 1
# gamma.rate <- 2

#pooling samples;
# n.row <- 4
# n.col <- 5
n.row <- 3
n.col <- 3
n.sample <- 9
n.sample.pooling <- 5
percent.x <- 0.02
percent.y <- 0.02
n.minimum.latrine <- 10

#adaptive sampling settings;
n.days.per.sample <- 7
n.week.per.update <- 12
n.drop <- 1
n.drop.add <- 2
n.update <- 20
n.site <- n.latrine
n.days <- n.update*n.week.per.update*n.days.per.sample + 100
#n.days <- 1000

#outbreak
#outbreak geographic size;
outbreak.range <- 0.01
#outbreak area number;
n.outbreak.area <- 5
#outbreak amplification factor;
outbreak.factor <- 10
#outbreak zone changes every 10000 days;
outbreak.days <- 10000
