library(dplyr)
library(ggplot2)
options(stringsAsFactors = FALSE)
records <- read.csv("~/CollegeFootballStats/MiamiRecords.csv")
records$Year <- as.numeric(records$Year)
records$Won <- as.numeric(records$Won)
records$Lost <- as.numeric(records$Lost)
records <- mutate(records, winpct=Won/(Won+Lost))
records$NC <- sapply(records$Year, function(x){ifelse(x %in% c(1983,1987,1989,1991,2001), "Yes", "No")})

coaches <- read.csv("~/CollegeFootballStats/MiamiCoaches.csv")
coaches$Coach <- factor(coaches$Coach, levels=c("Schnellenberger","Johnson","Erickson","Davis","Coker","Shannon","Golden"))

ggplot(records, aes(x=Year, y=winpct)) + geom_line() + geom_point() + ylim(0,1.5) + theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face="bold"),
    plot.title = element_text(size = 18, face="bold"),
    panel.grid.minor = element_line(linetype="dotted", colour="white", size=.75),
    legend.position = "none"
  )

pal=colorRampPalette(c("#E75C2D", "#FFFFFF", "#003E24"))
palCols <- pal(7)

ggplot(coaches) + geom_line(data=records, aes(x=Year, y=winpct), size=2, colour="#E75C2D") + geom_point(data=records, aes(x=Year, y=winpct, alpha=NC), size=8, colour="#FFD700") + geom_point(data=records, aes(x=Year, y=winpct), size=4, colour="#003E24") + geom_segment(aes(x=Start-.25, xend=End+.25, y=0, yend=0, colour=Coach), size=10) + ylim(0,1) + xlim(1978.5, 2014.5) + scale_alpha_discrete(name="National\nChampions", range = c(0, 1)) + scale_colour_manual(values=palCols) + theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face="bold"),
    plot.title = element_text(size = 18, face="bold"),
    panel.grid.minor = element_line(linetype="dotted", colour="white", size=.75)
    #panel.background = element_rect(fill="#999999")
  ) + xlab("Year") + ylab("Win Percentage") + ggtitle("Miami Hurricanes Football Timeline")
