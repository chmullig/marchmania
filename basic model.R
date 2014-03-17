require(ggplot2)
require(dplyr)
require(rstan)
require(randomForest)
require(gbm)

# define our loss function. epsilon chosen to match public numbers from kaggle forums
llf <- function(submission, solution) {
    epsilon <- .000000000000001
    yhat <- min(max(submission, epsilon), 1-epsilon)
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

#clean up a few things
seasons$dayzerodt <- as.Date(seasons$dayzero, "%m/%d/%Y")

# test out our llf and data load
sample_submission_old <- solution
sample_submission_old$Usage <- NULL
sample_submission_old$pred <- 0

llf(sample_submission_old$pred[solution$Usage != "Ignored"], solution$pred[solution$Usage != "Ignored"])
#resut should be 17.3241




#build up our season summary information
season_summary <- merge(seasons[,1], teams)
names(season_summary)[1] <- "season"


#build up glicko ratings
results <- rbind(data.frame(regular_season_results, tourney=FALSE), data.frame(tourney_results, wloc=NA, tourney=TRUE))
results$period <- as.numeric(results$season)*100 + results$daynum/10
results$wloc <- recode(results$wloc, "c('H') = 2; c('A') = -2; else = 0")
glickodata <- data.frame(results[,c(10, 3, 5)], winner = rep(1, nrow(results)))



ratings <- NULL
season_summary$glicko <- NA
for (i in 1:19) {
    letter = rawToChar(as.raw(as.numeric(charToRaw("A")) + i - 1))
    print(paste(i, letter))
    
    thisSeason = as.numeric(results$season) == i & results$tourney == FALSE
    ratings <- glicko(glickodata[thisSeason,], status=ratings$ratings, history=TRUE, gamma=results$wloc[thisSeason], sort=FALSE)
    for (j in 1:nrow(ratings$rating)) {
        season_summary[as.numeric(season_summary$season)==i & season_summary$id == ratings$ratings[j,]$Player,]$glicko <- ratings$ratings[j,]$Rating
    }

    postSeason = as.numeric(results$season) == i & results$tourney == TRUE
    ratings <- glicko(glickodata[postSeason,], status=ratings$ratings, history=TRUE, gamma=results$wloc[postSeason])
}
season_summary[sample(nrow(season_summary), 25),]

                
gbm(distribution="bernoulli")