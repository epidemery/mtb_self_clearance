library("here")

population = read.csv(file = here::here("data", "WPP2019_PopulationByAgeSex_Medium.csv"), header = TRUE, sep = ",")

population$AgeGrpEnd = population$AgeGrpStart + 5

population$cohort = population$Time - population$AgeGrpEnd

### JAPAN ###

population_JPN = subset(population, Location %in% c("Japan") & Time == 2019)

births_JPN = array(,dim=c(2,dim(as.array(population_JPN$PopTotal))))
births_JPN[1,] = population_JPN$cohort
births_JPN[2,] = population_JPN$PopTotal

write.table(births_JPN,here::here("data","births_JPN.csv"),row.names = FALSE, col.names = FALSE, sep = ",")

### CHINA ###

population_CHN = subset(population, Location %in% c("China") & Time == 2019)

births_CHN = array(,dim=c(2,dim(as.array(population_CHN$PopTotal))))
births_CHN[1,] = population_CHN$cohort
births_CHN[2,] = population_CHN$PopTotal

write.table(births_CHN,here::here("data","births_CHN.csv"),row.names = FALSE, col.names = FALSE, sep = ",")

### INDIA ###

population_IND = subset(population, Location %in% c("India") & Time == 2019)

births_IND = array(,dim=c(2,dim(as.array(population_IND$PopTotal))))
births_IND[1,] = population_IND$cohort
births_IND[2,] = population_IND$PopTotal

write.table(births_IND,here::here("data","births_IND.csv"),row.names = FALSE, col.names = FALSE, sep = ",")