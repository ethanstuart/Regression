url <- 'http://www.statsci.org/data/general/hills.txt'
hills <- read.table(url, sep = '\t',header = TRUE, quote='', comment='')

#EDA
tail(hills)
plot(hills$Distance,hills$Time)
identify(hills$Distance,hills$Time)
hills[18,] #high outlier (vertical)
hills[7,] # A little off from the otherwise linear relationship

plot(hills$Climb,hills$Time)
identify(hills$Climb,hills$Time)
hills[11,]

#Measurements to determine unusual observations:
  #1 Leverage: weight an observation has in predicting itself
  #2 Cook's Distance: Compare Bhat (all coefficients) to what Bhat without the unusual observation
      # so like one model with all of them, then a model without each point individually.
  #3 Studentizing 
    # (yi-yihat)/(sqrt(vhat(yi-yihat)))
      # remove the ith observation (like it is the test dataset) 35 of them.
      # R-Studetnized residuals. 

# Regression Diagnostics

out.hills <- lm(Time~Distance+Climb, data=hills)

# compute leverage: weight an observation has in predicting itself
leverage.hills <- lm.influence(out.hills)$hat
subset(leverage.hills,hills$Race=="BenNevis")
# 1216 is the weight it has in predicting itself

#compute cook's d: The difference this observation has in the computed coefficients. 
cd.hills <- cooks.distance(out.hills)
subset(cd.hills,hills$Race=="MoffatChase")  
# 0.0524 its standardized so we don't really know.

# compute R-Studentized Residuals: 
R.hills <-rstudent(out.hills)
subset(R.hills,hills$Race=="CairnTable")
# .715 this is like a z-score, so this value isn't really unusual

#histogram/dotplot/boxplot/qqqplot

#Ho: E are normally distributed
#Ha: E are not normally distrubted (skewed, multi-modal, outliers)

#KS test compares cdf of E to empircal cdf of R-studentized residuals
  # p-value <.05 means E's not normal
  # p-value >.05 means E's aren't non-normal (this doesn't prove it, it just shows that the assumption is valid)

# validate normality assumption (1 or 2 ouliers)
hist(R.hills)

#compute ks test
ks.test(R.hills,"pnorm")
# p-value is .0366, less than 0.05. Not normal. 

# Rule of Thumb
  #Leverage: (2*p+1)/n. p=# explanatory variables. +1 is for the column of intercepts. 2*3/35=.171
    # if it's bigger than .1714, then it's influential
  #Cook's Distance: 4/(n-p-1). 4/(35-3)
    #So if it's bigger than .125, then it's a problem.

#is Kildcon Hill an outlier: off by three minutes. We need to take into account the variance of prediction error though.
# use R studentized residuals to see if it's an outlier. They are approximately normal. 
# >2 or <-2. more for bigger datasets. 
subset(R.hills,hills$Race=="KildconHill")

##MoffatChase
subset(leverage.hills,hills$Race=="MoffatChase")
subset(cd.hills,hills$Race=="MoffatChase")  
#influential, but it's good because it's in line.

##LairigGhru
subset(leverage.hills,hills$Race=="LairigGhru")
subset(cd.hills,hills$Race=="LairigGhru")  
#bad influential because of the second plot being such a high outlier

##BensofJura plot to see if it's bad. 
subset(leverage.hills,hills$Race=="BensofJura")
subset(cd.hills,hills$Race=="BensofJura")  
subset(R.hills,hills$Race=="BensofJura")

#It is bigger on both leverage and Cook's d. Bad influential. 

#Is Cow hill an oulier?
subset(R.hills,hills$Race=="CowHill")
# to get a p-value. because it could be .314 or -.314. or 2*pval for one
2*(1-pnorm(.314))
#Not an outlier, the p-value is not <.05

#pvalue for KnockHill
subset(R.hills,hills$Race=="KnockHill")
2*(1-pnorm(7.61))
# <0.00001. It is likely that this value was misrecorded because of the low p-value.




