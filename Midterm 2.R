##Midterm 2###

#1 Michael Fulmer, Justin Apton

#2
tv <- read.table(header=TRUE,text="
Year	Tvaud	Tigers	AtBreak	DNP	Bullpen
2003	200.9	1	0.272	6	11
2004	235.6	2	0.483	1	10
2005	436.6	1	0.488	4	10
2006	290.9	3	0.67	7	10
2007	305.1	5	0.605	2	10
2008	223.3	1	0.5	4	11
2009	302.1	5	0.552	19	13
2010	260.8	3	0.558	10	16
2011	201.9	5	0.533	15	13
2012	296.2	3	0.512	6	9
2013	374.7	6	0.553	8	14
2014	279.9	4	0.582	17	12
2015	190.4	4	0.5	8	12
2016	122.1	1	0.517	11	14
2017	111.2	2	0.448	7	11
")
?aggregate

Mean <- c(mean(tv$Tigers),mean(tv$AtBreak),mean(tv$DNP),mean(tv$Bullpen),mean(tv$Tvaud))
StandardDeviation<- c(sd(tv$Tigers),sd(tv$AtBreak),sd(tv$DNP),sd(tv$Bullpen),sd(tv$Tvaud))
Correlation <- c(cor(tv$Tigers,tv$Tvaud),cor(tv$AtBreak,tv$Tvaud),cor(tv$DNP,tv$Tvaud),cor(tv$Bullpen,tv$Tvaud),cor(tv$Tvaud,tv$Tvaud))
tab <- cbind(Mean,StandardDeviation,Correlation)
rownames(tab) <- c("Tigers","AtBreak","DNP","Bullpen","Tvaud")
tab
out.tv <- lm(Tvaud~Tigers+AtBreak+DNP+Bullpen,data=tv)

n <- 15
p <- 4

#3
cutoff.lev <- (2*(p+1))/n
leverage.tv <- lm.influence(out.tv)$hat
subset(leverage.tv,tv$Year==2010)
#4
cutoff.cd <-4/(n-p-1)
cd.tv <- cooks.distance(out.tv)
subset(cd.tv,tv$Year==2003)



#5
R.tv <-rstudent(out.tv)
subset(R.tv,tv$Year==2013) 
#6
hist(R.tv)
ks.test(R.tv,"pnorm") #approximately normal. p-val=.9918
this <- subset(R.tv,tv$Year==2005)
2*(1-pnorm(this)) #0.000392608 
# The game was played in Detroit at Comerica park, which could explain why the viewership was so high

#7
vif(out.tv)[2] 
#1.401069 

#8
which(leverage.tv>cutoff.lev)
which(cd.tv>cutoff.cd)
which(R.tv>3) #third observation, 2005
plot(tv$Year,tv$Tvaud) #it is clear from the plot that 2005 is a high outlier. 


tv1 <- tv[-3,]
dim(tv1)
names(tv1)

#9
out1.tv <- lm(Tvaud~Tigers+AtBreak+DNP+Bullpen,data=tv1)
summary(out1.tv)

#10 
confint(out1.tv)
#For every added Tigers player to the team there is an estimated increase of viewership of 28.446 (thousand households)
#with a 95% confidence interval for that estimate being between (-0.4222851  57.31499)

#11
reduced.tv <- lm(Tvaud~Tigers+AtBreak,data=tv1,x=TRUE)
# TVaud = 79.64 + 24.00Tigers + 164.74AtBreak 
reduced.tv$x  
anova(reduced.tv,out1.tv) #test stat=0.5636, pval= 0.588

#12
newobs <- data.frame(Tigers = 5, AtBreak=.6, DNP=7, Bullpen=11)
predict(out1.tv,newdata=newobs,interval="confidence")
newobs2 <- data.frame(Tigers=1,AtBreak=.4,DNP=7,Bullpen=11)
predict(out1.tv,newdata=newobs2,interval="confidence")

