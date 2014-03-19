require(ggplot2)
require(dplyr)
require(rstan)
require(randomForest)
require(gbm)
require(car)
require(msm)
require(PlayerRatings)

# define our loss function. epsilon chosen to match public numbers from kaggle forums
llf <- function(submission, solution) {
    epsilon <- .000000000000001
    yhat <- pmin(pmax(submission, rep(epsilon)), 1-rep(epsilon))
    logloss <- -mean(solution*log(yhat)
                     + (1-solution)*log(1 - yhat))
    return(logloss)
}

#and a handy replace function:
replace.df <- function(x,y,by,cols=NULL
){
    nx <- nrow(x)
    ny <- nrow(y)
    
    bx <- x[,by,drop=FALSE]
    by <- y[,by,drop=FALSE]
    bz <- do.call("paste", c(rbind(bx, by), sep = "\r"))
    
    bx <- bz[seq_len(nx)]
    by <- bz[nx + seq_len(ny)]
    
    idx <- match(by,bx)
    idy <- match(bx,by)
    idy <- idy[!is.na(idy)]
    
    if(is.null(cols)) {
        cols <- intersect(names(x),names(y))
        cols <- cols[!cols %in% by]
    }
    
    x[idx,cols] <- y[idy,cols]
    x
}

# load data
data_folder <- './data/'
regular_season_results <- read.csv(paste(data_folder,"regular_season_results.csv",sep=""))
sample_submission <- read.csv(paste(data_folder,"sample_submission.csv",sep=""))
seasons <- read.csv(paste(data_folder,"seasons.csv",sep=""))
teams <- read.csv(paste(data_folder,"teams.csv",sep=""))
tourney_results <- read.csv(paste(data_folder,"tourney_results.csv",sep=""))
tourney_seeds <- read.csv(paste(data_folder,"tourney_seeds.csv",sep=""))
tourney_slots <- read.csv(paste(data_folder,"tourney_slots.csv",sep=""))
solution <- read.csv(paste(data_folder,"solution.csv",sep=""))
oranks_orig <- read.csv(paste(data_folder,"ordinal_ranks_core_33.csv",sep=""))
oranks_orig_noncore <- read.csv(paste(data_folder,"ordinal_ranks_non_core.csv",sep=""))
oranks_s <- read.csv(paste(data_folder,"ordinal_ranks_season_S_thru_17-MAR.csv",sep=""))



#clean up a few things
seasons$dayzerodt <- as.Date(seasons$dayzero, "%m/%d/%Y")
solution$id.1 <- as.numeric(substr(solution$id, 3, 5))
solution$id.2 <- as.numeric(substr(solution$id, 7, 9))
solution$season <- factor(substr(solution$id, 1, 1), levels=levels(seasons$season))

sample_submission$id.1 <- as.numeric(substr(sample_submission$id, 3, 5))
sample_submission$id.2 <- as.numeric(substr(sample_submission$id, 7, 9))
sample_submission$season <- factor(substr(sample_submission$id, 1, 1), levels=levels(seasons$season))

oranks <- rbind(oranks_orig, oranks_orig_noncore)
oranks <- rbind(oranks, oranks_s)
oranks$season <- factor(oranks$season, levels=levels(seasons$season))



# test out our llf and data load
sample_submission_old <- solution
sample_submission_old$Usage <- NULL
sample_submission_old$pred <- 0
tourney_seeds$seedn <- as.numeric(substr(tourney_seeds$seed,2,3))


#resuts should be 17.3241 for all 0s, and  ~.69 for random ~N(.5, .04) trimmed
llf(sample_submission_old$pred[solution$Usage != "Ignored"], solution$pred[solution$Usage != "Ignored"])
llf(rtnorm(sum(solution$Usage != "Ignored"), mean=.5, sd=.04, lower=0, upper=1), solution$pred[solution$Usage != "Ignored"])




#build up our season summary information
season_summary <- merge(seasons[,1], teams)
names(season_summary)[1] <- "season"

regular_wins <- summarise(group_by(regular_season_results, season, wteam), wscore_tot=sum(wscore), wmargin_avg=mean(wscore - lscore), wgames=length(wscore))
season_summary <- merge(season_summary, regular_wins, by.x=c("season", "id"), by.y=c("season", "wteam"), all.x=TRUE)
regular_losses <- summarise(group_by(regular_season_results, season, lteam), lscore_tot=sum(lscore), lmargin_avg=mean(lscore - wscore), lgames=length(lscore))
season_summary <- merge(season_summary, regular_losses, by.x=c("season", "id"), by.y=c("season", "lteam"), all.x=TRUE)

season_summary$winpct <- season_summary$wgames / (season_summary$wgames + season_summary$lgames)
season_summary$margin_avg <- season_summary$wmargin_avg*season_summary$wgames / (season_summary$wmargin_avg*season_summary$wgames + season_summary$lmargin_avg*season_summary$lgames)
season_summary <- merge(season_summary, tourney_seeds, by.x=c("season", "id"), by.y=c("season", "team"), all.x=TRUE)

#add ordinal ranks
season_summary <- merge(season_summary, oranks[oranks$sys_name == "SAG" & oranks$rating_day_num == 133, c(1,4,5)], by.x=c("season", "id"), by.y=c("season", "team"), all.x=TRUE)
colnames(season_summary)[length(season_summary)] <- "SAG"
season_summary <- merge(season_summary, oranks[oranks$sys_name == "POM" & oranks$rating_day_num == 133, c(1,4,5)], by.x=c("season", "id"), by.y=c("season", "team"), all.x=TRUE)
colnames(season_summary)[length(season_summary)] <- "POM"
season_summary <- merge(season_summary, oranks[oranks$sys_name == "LMC" & oranks$rating_day_num == 133, c(1,4,5)], by.x=c("season", "id"), by.y=c("season", "team"), all.x=TRUE)
colnames(season_summary)[length(season_summary)] <- "LMC"
season_summary <- merge(season_summary, oranks[oranks$sys_name == "MOR" & oranks$rating_day_num == 133, c(1,4,5)], by.x=c("season", "id"), by.y=c("season", "team"), all.x=TRUE)
colnames(season_summary)[length(season_summary)] <- "MOR"

#build up glicko ratings
results <- rbind(data.frame(regular_season_results, tourney=FALSE), data.frame(tourney_results, wloc=NA, tourney=TRUE))
results$period <- as.numeric(results$season)*100 + results$daynum/10
results$wloc <- recode(results$wloc, "c('H') = 2; c('A') = -2; else = 0")
glickodata <- data.frame(results[,c(10, 3, 5)], winner = log(results$wscore-results$lscore+1)/4.55)
#glickodata <- data.frame(results[,c(10, 3, 5)], winner = 1)



ratings <- NULL
season_summary$glicko <- NA
season_summary$glickord <- NA
for (i in 1:19) {
    letter = rawToChar(as.raw(as.numeric(charToRaw("A")) + i - 1))
    print(paste(i, letter))
    
    thisSeason = as.numeric(results$season) == i & results$tourney == FALSE
    ratings <- glicko(glickodata[thisSeason,], status=ratings$ratings, history=TRUE, gamma=results$wloc[thisSeason], sort=FALSE)
    for (j in 1:nrow(ratings$rating)) {
        season_summary[as.numeric(season_summary$season)==i & season_summary$id == ratings$ratings[j,]$Player,]$glicko <- ratings$ratings[j,]$Rating
        season_summary[as.numeric(season_summary$season)==i & season_summary$id == ratings$ratings[j,]$Player,]$glickord <- ratings$ratings[j,]$Deviation
    }

    postSeason = as.numeric(results$season) == i & results$tourney == TRUE
    ratings <- glicko(glickodata[postSeason,], status=ratings$ratings, history=TRUE, gamma=results$wloc[postSeason])
}
season_summary[sample(nrow(season_summary), 25),]


g <- function(r) {
    a <- 1/sqrt(1+3*(log(10)/400)^2*r^2/pi^2)
    return(a)
}

prepareDf <- function(x) {
    x <- merge(x, season_summary, by.x=c("season", "id.1"), by.y=c("season", "id"), suffixes=c("", ".1"), all.x=TRUE)
    x <- merge(x, season_summary, by.x=c("season", "id.2"), by.y=c("season", "id"), suffixes=c(".1", ".2"), all.x=TRUE)
    
    x$glicko.diff <- x$glicko.1 - x$glicko.2
    x$margin_avg.diff <- x$margin_avg.1 - x$margin_avg.2
    x$wmargin_avg.diff <- x$wmargin_avg.1 - x$wmargin_avg.2
    x$winpct.diff <- x$winpct.1 - x$winpct.2
    x$seedn.diff <- x$seedn.2 - x$seedn.1
    x$seedpred <- .5 + .03*(x$seedn.2 - x$seedn.1)
    x$seed1v16 <- (x$seedn.2 - x$seedn.1 >= 15) + 0 - (x$seedn.2 - x$seedn.1 <= -15)
    
    x$winpred <- 1/(1+10^(-x$glicko.diff/15))
    x$glickopred = 1/(1 + 10^(-g(sqrt(x$glickord.1^2 + x$glickord.2^2))*(x$glicko.diff)/400))
    x$SAG.diff <- x$SAG.1 - x$SAG.2
    x$POM.diff <- x$POM.1 - x$POM.2
    x$LMC.diff <- x$LMC.1 - x$LMC.2
    x$MOR.diff <- x$MOR.1 - x$MOR.2
    
    return(x)
}



t1 <- data.frame(tourney_results[as.numeric(tourney_results$wteam) < as.numeric(tourney_results$lteam),c(1,2, 3,4, 5,6)], won=1)
names(t1) <- c("season", "daynum", "id.1", "score.1", "id.2", "score.2", "won")

t2 <- data.frame(tourney_results[as.numeric(tourney_results$wteam) >= as.numeric(tourney_results$lteam),c(1,2, 5,6,3,4)], won=0)
names(t2) <- c("season", "daynum", "id.1", "score.1", "id.2", "score.2", "won")

t <- rbind(t1, t2)
t$id <- paste(t$season, t$id.1, t$id.2, sep="_")
t <- prepareDf(t)
tail(t, 10)
asdfasdf

#HOLDOUT <- "Q"
#t <- subset(t, season != HOLDOUT)

m <- gbm(won ~ glickopred + seedn.diff + seedpred + glicko.diff + wmargin_avg.1 + wmargin_avg.2 + wmargin_avg.diff + margin_avg.diff + winpct.diff + SAG.diff + POM.diff + LMC.diff + MOR.diff,
    data=t,
    n.trees=50000,
    interaction.depth=3,
    n.cores=4,
    n.minobsinnode = 1,
    shrinkage=.0001,
    distribution="bernoulli")

summary(m, n.trees=best.iter)
best.iter <- gbm.perf(m)

    pred.gbm <- predict(m, p, n.trees=best.iter, type="response")
    llf(pred.gbm[solution$Usage != "Ignored"], solution$pred[solution$Usage != "Ignored"])
    llf(predict(m, pr, n.trees=best.iter, type="response")[solutionr$Usage != "Ignored"], solutionr$pred[solutionr$Usage != "Ignored"])


rf <- randomForest(won ~ winpred + glickopred + seedn.diff + seedpred + wmargin_avg.1 + wmargin_avg.2 + wmargin_avg.diff + margin_avg.diff + winpct.diff,
         data=t,
         ntree=10000,
         importance=TRUE,
         nodesize=10
        )

lm <- glm(won ~ winpred + glickopred + seedn.diff +  glicko.diff + wmargin_avg.diff + margin_avg.diff +  winpct.diff + SAG.diff + POM.diff + LMC.diff + MOR.diff,
                   data=t, family="binomial")
summary(lm)






#prepare the dataset for testing
p <- sample_submission_old
p <- prepareDf(p)
p <- p[with(p, order(id)),]

pr <- subset(p, season == HOLDOUT)
solutionr <- subset(solution, season == HOLDOUT)

pred.gbm <- predict(m, p, n.trees=best.iter, type="response")
llf(pred.gbm[solution$Usage != "Ignored"], solution$pred[solution$Usage != "Ignored"])
llf(predict(m, pr, n.trees=best.iter, type="response")[solutionr$Usage != "Ignored"], solutionr$pred[solutionr$Usage != "Ignored"])


pred.rf <- predict(rf, p) 
llf(pred.rf[solution$Usage != "Ignored"], solution$pred[solution$Usage != "Ignored"])
llf(predict(rf, pr)[solutionr$Usage != "Ignored"], solutionr$pred[solutionr$Usage != "Ignored"])

pred.lm <- predict(lm, p, type="response")
llf(pred.lm[solution$Usage != "Ignored"], solution$pred[solution$Usage != "Ignored"])
llf(predict(lm, pr, type="response")[solutionr$Usage != "Ignored"], solutionr$pred[solutionr$Usage != "Ignored"])



pred.seed <- .5 + .03*(p$seedn.diff)
llf(pred.seed[solution$Usage != "Ignored"], solution$pred[solution$Usage != "Ignored"])
llf((.5 + .03*(pr$seedn.diff))[solutionr$Usage != "Ignored"], solutionr$pred[solutionr$Usage != "Ignored"])


llf(p$glickopred[solution$Usage != "Ignored"], solution$pred[solution$Usage != "Ignored"])
llf(pr$glickopred[solutionr$Usage != "Ignored"], solutionr$pred[solutionr$Usage != "Ignored"])


##bracket season R with gbm
sample_submission_old$pred <- pred.gbm
write.csv(sample_submission_old[sample_submission_old$season ==HOLDOUT,1:2], "submission_R.csv", row.names=FALSE)

##actually predict the season S with gbm
curr <- sample_submission
curr <- prepareDf(curr)
curr <- curr[with(curr, order(id)),]

sample_submission$pred <- predict(m, curr, n.trees=best.iter, type="response")
write.csv( sample_submission[,1:2], "submission_gbm.csv", row.names=FALSE)


sample_submission$pred <- predict(lm, curr, type="response")
sample_submission$pred[is.na(sample_submission$pred)] <- .5
write.csv( sample_submission[,1:2], "submission_lm.csv", row.names=FALSE)
