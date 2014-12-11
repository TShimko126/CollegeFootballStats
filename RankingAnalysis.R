library(dplyr)
library(ggplot2)

options(stringsAsFactors = FALSE)
rankings <- read.csv("~/CollegeFootballStats/WeeklyRankings.csv", head=F)
colnames(rankings) <- c("rank", "team", "record", "week")
rankings$win <- sapply(strsplit(rankings$record, "-"), function(x){x[1]})
rankings$loss <- sapply(strsplit(rankings$record, "-"), function(x){x[2]})

rankings$rank <- as.numeric(rankings$rank)
rankings$week <- as.numeric(rankings$week)
rankings$win <- as.numeric(rankings$win)
rankings$loss <- as.numeric(rankings$loss)
rankings$team <- sapply(rankings$team, function(x){ifelse(x=="Texas A&amp;M", "Texas A&M", x)})

changes <- data.frame(week=numeric(), team=character(), startRank=numeric(), endRank=numeric(), change=numeric())
for (weekOne in (seq(10,15))){
    print(weekOne)
    weekRank <- filter(rankings, week==weekOne)
    for (teamOne in unique(weekRank$team)){
        print(teamOne)
        week1 <- filter(weekRank, team==teamOne)
        losses <- unlist(week1$loss) + 1
        week2 <- filter(rankings, team==teamOne, week==(weekOne+1), loss==losses)
        if (dim(week1)[1]==dim(week2)[1]){
            startRank <- week1$rank
            endRank <- week2$rank
            team <- week1$team
            week <- week2$week
            change <- startRank - endRank
            df <- data.frame(week, team, startRank, endRank, change)
            changes <- data.frame(rbind(changes, df))
        }
    }
}

ggplot(changes, aes(x=change)) + geom_histogram(breaks=seq(-12.5, 2), fill="#F78F1E", colour = "black", lwd = 1.5, position="dodge") + theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face="bold"),
    plot.title = element_text(size = 18, face="bold"),
    panel.grid.minor = element_line(linetype="dotted", colour="white", size=.75)
  ) + xlab("Ranking Change") + ylab("Count") + ggtitle("College Football Playoff Ranking Change After Loss") + annotate("text", x = 0, y = 4.75, label = "@tylershimko", colour="#808080", size=6, alpha = .25)


ggplot(rankings, aes(x=week, y=rank, colour=team, label=team)) + geom_line() + geom_text(fontface=2, size = 5) + scale_y_reverse(breaks=seq(1, 25, 2), limits=c(25, 1)) + scale_x_continuous(breaks=10:16, limits=c(9.5,16.5)) + theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face="bold"),
    plot.title = element_text(size = 18, face="bold"),
    panel.grid.minor = element_line(linetype="dotted", colour="white", size=.75),
    legend.position = "none"
  ) + xlab("Ranking Week") + ylab("Rank") + ggtitle("Weekly College Football Playoff Ranking") 
