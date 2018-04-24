### Regression 4

library(XML)
url <- "http://www.espn.com/mlb/standings/_/season/2017"
webpage <- htmlParse(url)
mlb <- readHTMLTable(webpage,header=FALSE,which=1,
                     skip.rows=c(1:21,27,33),colClasses = rep("numeric",11))
mlbr <- mlb[,c(1,9)]

names(mlbr) <- c("wins","run.diff")
mlbr$division <- c(rep("East",5),rep("Central",5),rep("West",5))

east <- mlbr[mlbr$division=="East",]
central <- mlbr[mlbr$division=="Central",]
west <- mlbr[mlbr$division=="West",]

#summary statistics by division
by(mlbr$wins,mlbr$division,mean)
by(mlbr$run.diff,mlbr$division,mean)
by(mlbr$wins,mlbr$division,sd)
by(mlbr$run.diff,mlbr$division,sd)


plot(mlbr$run.diff,mlbr$win,xlab="Run Differential", ylab="Wins")
points(mlbr$run.diff[mlbr$division=="East"],mlbr$wins[mlbr$division=="East"],col="darkgreen",pch=19)
points(mlbr$run.diff[mlbr$division=="Central"],mlbr$wins[mlbr$division=="Central"],col="blue",pch=19)
points(mlbr$run.diff[mlbr$division=="West"],mlbr$wins[mlbr$division=="West"],col="orange",pch=19)
legend("topleft",legend=c("East","Central","West"),col=c("darkgreen","blue","orange"),pch=c(rep(19,3)))


# fit the model
# division is a categorical variable
mlbr$division <- factor(mlbr$division)
out1.mlb <- lm(wins~division + run.diff,data=mlbr,x=TRUE,y=TRUE)
out1.mlb$y
out1.mlb$x
#to specify comparison case (Y-int) as east. We are comparing everyone else to east in this case.
mlbr$division <- relevel(mlbr$division,"East")

out2.mlb <- lm(wins~division + run.diff,data=mlbr,x=TRUE,y=TRUE)
out2.mlb$x

# Bo is y intercept. 

# recognizing there are three levels of division, we have three models
# 1 if East: Bo + B1*0 + B2*0 + B3*rundiff   = (Bo)+ B3 rundiff 
#2 if Central: Bo + B1*1 + B2*0 + B3*rundiff = (Bo + B1) + B3rundiff (parallel) the only diference is B1 (mean diff between central and east for all rundiff)
#3 if West: Bo + B1*0 + B2*0 + B3rundiff     = (Bo + B2) + B3rundiff. B2 is difference between East and west, B1-B2 is diff.

summary(out2.mlb)

#interpret Bhat3 = .08.
# For one additional run scored or prevented scored, we estimate an expected increase in wins of .08, 
# keeping division the same. 

#if East winshat = 81.1 + .08*rundiff
# if Central wins = 81.1 -.16 (80.9) + .08*rundiff
#if West winshat = 81.1 + .13 (81.2) + .08*rundiff

#graphic showing predicted model (3 lines)

plot(mlbr$run.diff,mlbr$win,xlab="Run Differential", ylab="Wins")
points(mlbr$run.diff[mlbr$division=="East"],mlbr$wins[mlbr$division=="East"],col="darkgreen",pch=19)
points(mlbr$run.diff[mlbr$division=="Central"],mlbr$wins[mlbr$division=="Central"],col="blue",pch=19)
points(mlbr$run.diff[mlbr$division=="West"],mlbr$wins[mlbr$division=="West"],col="orange",pch=19)

#If the slopes are different then there is an interaction
#east
abline(81.096918,0.082029,col="darkgreen")
#Central
abline(81.096918-0.168041,0.082029,col="blue")
# West
abline(81.096918+0.138265,0.082029,col="orange")
legend("bottomright",legend=c("East","Central","West"),col=c("darkgreen","blue","orange"),pch=c(rep(19,3)))

# Ho: no diff between divisions after adjusting for run.diff

# Test Ho
reduced.mlb2 <- lm(wins~run.diff,data=mlbr)
anova(reduced.mlb2,out2.mlb)

#Research task, ect.
# Having the data from all of the leagues matched perfectly with the task. We used a linear regression on wins and run.diff
# to show difference between leagues. Run.diff and number of wins provided
# the evidence to suggest that there was not, in fact, a significant difference between each league.  

#Weakness:
# Maybe using another variable other than run.diff would have showed a different result. Is this the best way
# to see if leagues are more or less difficult than each other? Leaves some unanswered questions along
# these lines. 

#Challenge:
# use data from http://stats.nba.com/teams/traditional/?sort=W_PCT&dir=-1 to see if there is a significant 
# between NBA leagues. 



