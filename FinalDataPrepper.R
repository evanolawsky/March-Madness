##### make sure to comment out lines 10 and 53

setwd("~/Documents/PUBH Data Mining/Final project")
library(dplyr)

data <- read.csv("RegularSeasonDetailedResults.csv")
ranks <- read.csv("MasseyOrdinals.csv")

##### Step 1 - use AggregatedDetailedResults.R to produce agg.data
# data <- data[1:500,]
data$NCAA <- 0

agg.data <- data.frame(matrix(NA, nrow = nrow(data), ncol = 61))

for (i in 1:nrow(data)) {
  # games that the ith winner already won
  WW <- which(data$Season == data$Season[i] &
                data$DayNum < data$DayNum[i] &
                data$WTeamID == data$WTeamID[i])
  # games that the ith winner already lost
  WL <- which(data$Season == data$Season[i] &
                data$DayNum < data$DayNum[i] &
                data$LTeamID == data$WTeamID[i])
  # games that the ith loser already won
  LW <- which(data$Season == data$Season[i] &
                data$DayNum < data$DayNum[i] &
                data$WTeamID == data$LTeamID[i])
  # games that the ith loser already lost
  LL <- which(data$Season == data$Season[i] &
                data$DayNum < data$DayNum[i] &
                data$LTeamID == data$LTeamID[i])
  
  agg.data[i,1:8] <- data[i,1:8] 
  agg.data[i,9:21] <- colSums(data[WW, 9:21]) + colSums(data[WL, 22:34]) # winning team FOR stats
  agg.data[i,22:34] <- colSums(data[LW, 9:21]) + colSums(data[LL, 22:34]) # losing team FOR stats
  agg.data[i,35:47] <- colSums(data[WW, 22:34]) + colSums(data[WL, 9:21]) # winning team AGAINST stats 
  agg.data[i,48:60] <- colSums(data[LW, 22:34]) + colSums(data[LL, 9:21]) # losing team AGAINST stats
  agg.data[i,61] <- data[i,35]
}

names(agg.data) <- c("Season", "DayNum", "WTeamID", "WScore", "LTeamID", "LScore",
                     "WLoc", "NumOT", "WFGM.for", "WFGA.for", "WFGM3.for", "WFGA3.for",
                     "WFTM.for", "WFTA.for", "WOR.for", "WDR.for", "WAst.for", "WTO.for", 
                     "WStl.for", "WBlk.for", "WPF.for", "LFGM.for", "LFGA.for", "LFGM3.for", "LFGA3.for",
                     "LFTM.for", "LFTA.for", "LOR.for", "LDR.for", "LAst.for", "LTO.for", 
                     "LStl.for", "LBlk.for", "LPF.for", "WFGM.ag", "WFGA.ag", "WFGM3.ag", "WFGA3.ag",
                     "WFTM.ag", "WFTA.ag", "WOR.ag", "WDR.ag", "WAst.ag", "WTO.ag", 
                     "WStl.ag", "WBlk.ag", "WPF.ag", "LFGM.ag", "LFGA.ag", "LFGM3.ag", "LFGA3.ag",
                     "LFTM.ag", "LFTA.ag", "LOR.ag", "LDR.ag", "LAst.ag", "LTO.ag", 
                     "LStl.ag", "LBlk.ag", "LPF.ag", "Tourney")

##### Step 2 - use RankingsData.R to produce final
# ranks <- ranks[1:1000,]
final <- agg.data

for (i in 1:nrow(final)){
  w.candidates <- ranks[which(ranks$Season == final$Season[i] &
                                ranks$TeamID == final$WTeamID[i] & 
                                ranks$RankingDayNum > (final$DayNum[i] - 6) & 
                                ranks$RankingDayNum <= final$DayNum[i]),5]
  final[i,62] <- median(w.candidates)
  
  l.candidates <- ranks[which(ranks$Season == final$Season[i] &
                                ranks$TeamID == final$LTeamID[i] & 
                                ranks$RankingDayNum > (final$DayNum[i] - 6) & 
                                ranks$RankingDayNum <= final$DayNum[i]),5]
  final[i,63] <- median(l.candidates)
}

names(final)[62:63] <- c("WRk", "LRk")

##### Step 3 - use FinalDataAggregator.R to produce pergame
pergame <- final

# Calculate number of games each team has played so far that season
for (i in 1:nrow(pergame)){
  # games that the ith winner already won
  WW <- which(pergame$Season == pergame$Season[i] &
                pergame$DayNum < pergame$DayNum[i] &
                pergame$WTeamID == pergame$WTeamID[i])
  # games that the ith winner already lost
  WL <- which(pergame$Season == pergame$Season[i] &
                pergame$DayNum < pergame$DayNum[i] &
                pergame$LTeamID == pergame$WTeamID[i])
  # games that the ith loser already won
  LW <- which(pergame$Season == pergame$Season[i] &
                pergame$DayNum < pergame$DayNum[i] &
                pergame$WTeamID == pergame$LTeamID[i])
  # games that the ith loser already lost
  LL <- which(pergame$Season == pergame$Season[i] &
                pergame$DayNum < pergame$DayNum[i] &
                pergame$LTeamID == pergame$LTeamID[i])
  
  WGames <- length(WW) + length(WL) # games already played by the winner
  LGames <- length(LW) + length(LL) # games already played by the loser
  
  pergame[i,64] <- WGames
  pergame[i,65] <- LGames
  
  if (pergame[i,64] != 0) {
    pergame[i,9:21] <- pergame[i,9:21] / pergame[i,64]
    pergame[i,35:47] <- pergame[i,35:47] / pergame[i,64]
  }
  if (pergame[i,65] != 0) {
    pergame[i,22:34] <- pergame[i,22:34] / pergame[i,65]
    pergame[i,48:60] <- pergame[i,48:60] / pergame[i,65]
  }
}

##### Step 4 - use March8Work.R to produce rate
names(pergame)[64:65] <- c("WGames","LGames")

### Create rate stats
rate <- pergame[,1:8]

# Winning team
rate$WPoss.for <- 0.96*(pergame$WFGA.for + pergame$WTO.for + 0.475*pergame$WFTA.for - pergame$WOR.for) 
rate$WPts.for <- pergame$WFTM.for + 2*pergame$WFGM.for + pergame$WFGM3.for # because 3-pointers ARE included in the FGM column
rate$WORate <- 100 * rate$WPts.for / rate$WPoss.for
rate$WAstRatio.for <- 100*pergame$WAst.for / (pergame$WFGA.for + 0.475*pergame$WFTA.for + pergame$WAst.for + pergame$WTO.for)
rate$WTORatio.for <- 100*pergame$WTO.for / (pergame$WFGA.for + 0.475*pergame$WFTA.for + pergame$WAst.for + pergame$WTO.for)
rate$WTrue.for <- 100 * rate$WPts.for / (2 * (pergame$WFGA.for + 0.475*pergame$WFTA.for))
rate$WeFG.for <- (pergame$WFGM.for + 0.5 * pergame$WFGM3.for) / pergame$WFGA.for
rate$WFTRate.for <- pergame$WFTA.for / pergame$WFGA.for

rate$WPoss.ag <- 0.96*(pergame$WFGA.ag + pergame$WTO.ag + 0.475*pergame$WFTA.ag - pergame$WOR.ag) 
rate$WPts.ag <- pergame$WFTM.ag + 2*pergame$WFGM.ag + pergame$WFGM3.ag
rate$WDRate <- 100 * rate$WPts.ag / rate$WPoss.ag
rate$WAstRatio.ag <- 100*pergame$WAst.ag / (pergame$WFGA.ag + 0.475*pergame$WFTA.ag + pergame$WAst.ag + pergame$WTO.ag)
rate$WTORatio.ag <- 100*pergame$WTO.ag / (pergame$WFGA.ag + 0.475*pergame$WFTA.ag + pergame$WAst.ag + pergame$WTO.ag)
rate$WTrue.ag <- 100 * rate$WPts.ag / (2 * (pergame$WFGA.ag + 0.475*pergame$WFTA.ag))
rate$WeFG.ag <- (pergame$WFGM.ag + 0.5 * pergame$WFGM3.ag) / pergame$WFGA.ag
rate$WFTRate.ag <- pergame$WFTA.ag / pergame$WFGA.ag

rate$WNet <- rate$WORate - rate$WDRate
rate$WORebRate <- pergame$WOR.for / (pergame$WOR.for + pergame$WDR.ag)
rate$WDRebRate <- pergame$WDR.for / (pergame$WDR.for + pergame$WOR.ag)

# Losing team
rate$LPoss.for <- 0.96*(pergame$LFGA.for + pergame$LTO.for + 0.475*pergame$LFTA.for - pergame$LOR.for) 
rate$LPts.for <- pergame$LFTM.for + 2*pergame$LFGM.for + pergame$LFGM3.for # because 3-pointers ARE included in the FGM column
rate$LORate <- 100 * rate$LPts.for / rate$LPoss.for
rate$LAstRatio.for <- 100*pergame$LAst.for / (pergame$LFGA.for + 0.475*pergame$LFTA.for + pergame$LAst.for + pergame$LTO.for)
rate$LTORatio.for <- 100*pergame$LTO.for / (pergame$LFGA.for + 0.475*pergame$LFTA.for + pergame$LAst.for + pergame$LTO.for)
rate$LTrue.for <- 100 * rate$LPts.for / (2 * (pergame$LFGA.for + 0.475*pergame$LFTA.for))
rate$LeFG.for <- (pergame$LFGM.for + 0.5 * pergame$LFGM3.for) / pergame$LFGA.for
rate$LFTRate.for <- pergame$LFTA.for / pergame$LFGA.for

rate$LPoss.ag <- 0.96*(pergame$LFGA.ag + pergame$LTO.ag + 0.475*pergame$LFTA.ag - pergame$LOR.ag) 
rate$LPts.ag <- pergame$LFTM.ag + 2*pergame$LFGM.ag + pergame$LFGM3.ag
rate$LDRate <- 100 * rate$LPts.ag / rate$LPoss.ag
rate$LAstRatio.ag <- 100*pergame$LAst.ag / (pergame$LFGA.ag + 0.475*pergame$LFTA.ag + pergame$LAst.ag + pergame$LTO.ag)
rate$LTORatio.ag <- 100*pergame$LTO.ag / (pergame$LFGA.ag + 0.475*pergame$LFTA.ag + pergame$LAst.ag + pergame$LTO.ag)
rate$LTrue.ag <- 100 * rate$LPts.ag / (2 * (pergame$LFGA.ag + 0.475*pergame$LFTA.ag))
rate$LeFG.ag <- (pergame$LFGM.ag + 0.5 * pergame$LFGM3.ag) / pergame$LFGA.ag
rate$LFTRate.ag <- pergame$LFTA.ag / pergame$LFGA.ag

rate$LNet <- rate$LORate - rate$LDRate
rate$LORebRate <- pergame$LOR.for / (pergame$LOR.for + pergame$LDR.ag)
rate$LDRebRate <- pergame$LDR.for / (pergame$LDR.for + pergame$LOR.ag)

rate$WLoc <- pergame$WLoc
rate$Tourney <- pergame$Tourney
rate$WRk <- pergame$WRk
rate$LRk <- pergame$LRk

##### Step 5 - use Line_Scrape.R to produce designResponse
set.seed(1994)

rate$MOV <- rate$WScore - rate$LScore
rate$refInd <- (rate$MOV %% 2 == 0) + 1

rate1 <- rate %>% filter(refInd == 1)
rate1 <- rate1[, sort(colnames(rate1))]
colnames(rate1) <- gsub(gsub(x = gsub(colnames(rate1), pattern = "WLoc", 
                                          replacement = "Aloc"), pattern = "W", replacement = "A"),
                            pattern = "L", replacement = "B")

rate2 <- rate %>% filter(refInd == 2) %>% mutate(MOV = -MOV,
                                                         WLoc = ifelse(WLoc == 1, 2, ifelse(WLoc == 2, 1, 3)))
rate2 <- rate2[, sort(colnames(rate2))]
colnames(rate2) <- gsub(gsub(x = gsub(colnames(rate2), pattern = "WLoc", 
                                          replacement = "Aloc"), pattern = "L", replacement = "A"),
                            pattern = "W", replacement = "B")

finalrate <- rbind(rate1, rate2) %>% mutate(Awin = (sign(MOV) + 1) / 2)

# Saving design matrix and response
gameInfo <- finalrate %>% dplyr::select(ATeamID, BTeamID, DayNum)
designMat <- finalrate %>% dplyr::select(-c("AScore", "BScore", "ATeamID",
                                                "BTeamID",
                                                "refInd", "NumOT", "DayNum", "MOV"))
response <- finalrate %>% dplyr::select(MOV, Awin)

saveRDS(list(designMat, response), "designResponse2019.rds")

##### Create a test data file with one row for every possible tournament matchup
sample <- read.csv("SampleSubmissionStage2.csv")
# sample <- sample[1:250,]
tourney2019 <- data.frame(matrix(NA, nrow = nrow(data), ncol = 44))
names(tourney2019) <- names(designMat)

for (i in 1:nrow(sample)) {
  team1 <- as.numeric(substr(sample[i,1],6,9))
  day1A <- max(gameInfo[which(gameInfo$ATeamID == team1),]$DayNum)
  day1B <- max(gameInfo[which(gameInfo$BTeamID == team1),]$DayNum)
  if (day1A > day1B) {
    tourney2019[i,1:20] <- designMat[which(gameInfo$ATeamID == team1 & gameInfo$DayNum == day1A),c(23:30,32:43)]
  }
  if (day1A < day1B) {
    tourney2019[i,1:20] <- designMat[which(gameInfo$BTeamID == team1 & gameInfo$DayNum == day1B),1:20]
  }
  
  team2 <- as.numeric(substr(sample[i,1],11,14))
  day2A <- max(gameInfo[which(gameInfo$ATeamID == team2),]$DayNum)
  day2B <- max(gameInfo[which(gameInfo$BTeamID == team2),]$DayNum)
  if (day2A > day2B) {
    tourney2019[i,c(23:30,32:43)] <- designMat[which(gameInfo$ATeamID == team2 & gameInfo$DayNum == day2A),c(23:30,32:43)]
  }
  if (day2A < day2B) {
    tourney2019[i,c(23:30,32:43)] <- designMat[which(gameInfo$BTeamID == team2 & gameInfo$DayNum == day2B),1:20]
  }
  
  tourney2019[i,21] <- 2019
  tourney2019[i,22] <- 1
  tourney2019[i,31] <- 3
}

saveRDS(tourney2019, "tournamentGames2019.rds")

