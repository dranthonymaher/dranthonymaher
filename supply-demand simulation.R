
rm(list=ls())


# Parameters
initial_value <- 100
years <- 0:10
num_plots <- 10  # Number of scenarios to generate
mean_growth_rate <- 0.02
sd_growth_rate <- 0.05

# demand probs
dp<-runif(num_plots)
dprob<-dp/sum(dp)
# supply probs
sp<-runif(num_plots)
sprob<-sp/sum(sp)


nsim<-100

demand_probs<-c(0.25,0.15,0.6)
supply_probs<-c(0.8,0.15,0.5)



# Initialise data frame for storing results
data <- data.frame(year = rep(years, num_plots),
                   beddays = numeric(length(years) * num_plots),
                   group = rep(1:num_plots, each=length(years)))

# Function to calculate growth
calculate_growth <- function(initial_value, growth_rate, years) {
  return(initial_value * (1 + growth_rate) ^ years)
}

data$dprob<-rep(dprob,each = length(years))
data$sprob<-rep(sprob,each = length(years))


# Generate growth rates from normal distribution
# set.seed(123)  # For reproducibility
growth_rates <- rnorm(num_plots, mean = mean_growth_rate, sd = sd_growth_rate)

# Create the data for each growth rate
for (i in 1:num_plots) {
  data$beddays[ data$group == i ] <- calculate_growth(initial_value, growth_rates[ i ], years)
}


# Initialise plot
plot(years, calculate_growth(initial_value, growth_rates[ 1 ], years), type="n", 
     xlab="Years", ylab="Patient bed days", main="Bed day growth over 10 Years", ylim=c(initial_value, initial_value * (1 + mean_growth_rate + 3*sd_growth_rate) ^ max(years)))

# Add lines to the plot
for (i in 1:num_plots) {
  beddays <- calculate_growth(initial_value, growth_rates[ i ], years)
  lines(years, beddays, type="l"
#        , col=rainbow(num_plots)[ i ]
        , col = 'gray'
        , pch=16)  # Different colour for each line
}

# Adding a legend
#legend("topright", legend=paste("Growth rate:", round(growth_rates, 4)), col=rainbow(num_plots), lty=1, pch=16)

####### Calcualte demand for staff based on demand driver
#  assume we need 0.3 nurses and 0.1 doctors per patient bed day
# define nurse to patient bed day
n2pbd<-0.3
d2pbd<-0.1

data$d.nurses<-n2pbd*data$beddays
data$d.doctors<-d2pbd*data$beddays





##### Supply
# current nursing workforce
cn<-27
cd<-9

# attrition rates
a.n<--0.05

# Generate growth rates from normal distribution
# set.seed(123)  # For reproducibility
att_rates <- rnorm(num_plots, mean = a.n, sd = 0.02)

# quantify the remaining workforce for each attrition rate
for (i in 1:num_plots) {
  data$nsupply[ data$group == i ] <- calculate_growth(cn, att_rates[ i ], years)
}
for (i in 1:num_plots) {
  data$ndoc[ data$group == i ] <- calculate_growth(cd, att_rates[ i ], years)
}

par(mfrow=c(1,2))
# Initialise plot
plot(years,data$d.nurses[data$group==1], type = 'l'
    ,ylim = c(min(c(data$d.nurses,data$nsupply))
,max(c(data$d.nurses,data$nsupply)))
,ylab = "FTE"
,xlab = "years"
,main = "Supply and demand curves for FTE \n (thickness prop to prob)"
)


g<-gray.colors(num_plots)


# Add demand curves to the plot
for (i in 1:num_plots) {
  
  lines(years, data$d.nurses[data$group==i], type="l"
        #        , col=rainbow(num_plots)[ i ]
        , col = g[i]
        # ,lwd = 20*dprob[i]
        , pch=16)  # 
  }

# Add supply projections to the plot
for (i in 1:num_plots) {
    lines(years, data$nsupply[data$group==i], type="l"
        #        , col=rainbow(num_plots)[ i ]
        , col = 'blue'
        ,lwd = 20*sprob[i]
        , pch=16)  # 
}


# build the simulations - e.g. 100 simulations of the diff combinations of each possible scenario
# calculate gaps

# define the selected demand scenario
dscen<-sample(1:num_plots,size = nsim,replace=TRUE,prob = dprob)
# define the selected demand scenario
sscen<-sample(1:num_plots,size = nsim,replace=TRUE,prob = sprob)

# construct the supply and denand curves under that scenario adn comput the gaps

gy10<-0

# i<-1
for (i in 1:nsim){
datascen<-as.data.frame(cbind(years,data$d.nurses[data$group==dscen[i]],data$nsupply[data$group==sscen[i]]))
colnames(datascen)<-c("years","demand","supply")
datascen$gap<-datascen$demand-datascen$supply
# gap at year 10
# print(datascen)
gy10[i]<-datascen$gap[datascen$years==10]
}
# barplot(table(round(gy10)))

hist(gy10,20
     ,main = paste0("Gap after 10 years over ", num_plots, " simulations")
     # ,main = paste0('attrit = ', round(att_rates,2), '; demand = ', round(growth_rates,2))
)

