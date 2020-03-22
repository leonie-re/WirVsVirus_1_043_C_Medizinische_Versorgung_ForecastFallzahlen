rm(list = ls())

## Load and potentially install all the packages for this lab
#p_needed <- c("tidyverse", "tidyr", "dplyr", "matchingMarkets", "lubridate", "stringr")
#packages <- rownames(installed.packages())
#p_to_install <- p_needed[!(p_needed %in% packages)]
#if (length(p_to_install) > 0) {
#  install.packages(p_to_install)
#}
#lapply(p_needed, require, character.only = TRUE)

## Load datasets
#wd <- "C:/Users/Leoni/Google Drive/WirvsVirus/WirVsVirus"
#setwd(dir = wd)

#dat <- read.csv2(file = paste0(wd, "/RKI_COVID19.csv"), sep = ",", stringsAsFactors = FALSE)
dat <- read.csv2(file = "raw-data/RKI_COVID19.csv", sep = ",", stringsAsFactors = FALSE)

dat$date <- strsplit(dat$Meldedatum, split = "T") %>% lapply(., function(z){
  z[1]
}) %>% unlist(.)

### format data in order to extract daily information

dat_formated <- dat %>% split(., .$Landkreis) %>% lapply(., function(z){
  z[order(z$date, decreasing = FALSE),] %>% split(., .$date) %>% lapply(., function(x){
    AnzahlFallTag       <- sum(x$AnzahlFall)
    AnzahlTodesfall     <- sum(x$AnzahlTodesfall)
    AnzahlFallweiblich  <- ifelse("W" %in% x$Geschlecht,sum(x$AnzahlFall[x$Geschlecht == "W"]), 0)
    Anzahl35bis59       <- ifelse("A35-A59" %in% x$Altersgruppe, sum(x$AnzahlFall[x$Altersgruppe == "A35-A59"]), 0)
    Anzahl60plus        <- ifelse("A60-A79" %in% x$Altersgruppe, sum(x$AnzahlFall[x$Altersgruppe == "A60-A79"]), 0) + ifelse("A80+" %in% x$Altersgruppe, sum(x$AnzahlFall[x$Altersgruppe == "A80+"]), 0)
    data.frame(Bundesland = x$Bundesland[1], Landkreis = x$Landkreis[1], IdLandkreis = x$IdLandkreis[1], date = x$date[1], AnzahlFallTag = AnzahlFallTag, AnzahlTodesfall = AnzahlTodesfall,  AnzahlFallweiblich = AnzahlFallweiblich, Anzahl35bis59 = Anzahl35bis59, Anzahl60plus = Anzahl60plus, stringsAsFactors = FALSE)
  }) %>% do.call("rbind",.)
}) %>% do.call("rbind",.)

### add missing observations to the data assumption no new cases at missing dates (justified if there are no missing cases )
dat_formated$date <- as.Date(dat_formated$date)
dat_formated <- dat_formated %>% split(., .$Landkreis) %>% lapply(., function(z){
  z[order(z$date),]
}) %>% do.call("rbind", .)

dat_formated <- full_join(dat_formated, dat_formated %>% split(., .$Landkreis) %>% lapply(., function(z){
  possible_days <- seq(min(z$date), max(z$date), by="days")
  missing_days <- possible_days[!possible_days %in% z$date]
  if (length(missing_days) > 0){
    res <- data.frame(Bundesland = z$Bundesland[1], Landkreis = z$Landkreis[1],IdLandkreis = z$IdLandkreis[1], date = missing_days, AnzahlFallTag = 0, AnzahlTodesfall = 0, AnzahlFallweiblich = 0, Anzahl35bis59 = 0, Anzahl60plus = 0, stringsAsFactors = FALSE)
  }else{
    res <- NULL
  }
  return(res)
}) %>% do.call("rbind",.), by = c("Bundesland", "Landkreis", "IdLandkreis", "date", "AnzahlFallTag", "AnzahlTodesfall", "AnzahlFallweiblich", "Anzahl35bis59", "Anzahl60plus"))

dat_formated <- dat_formated %>% split(., .$Landkreis) %>% lapply(., function(z){
  z[order(z$date),]
}) %>% do.call("rbind", .)


### add weather information

weather_data                  <- read.csv2(file = "raw-data/Wetterdaten.csv", sep = ";", stringsAsFactors = FALSE )
weather_data$date             <- as.character(weather_data$date)
date_year                     <- str_sub(weather_data$date,1,4)
date_month                    <- str_sub(weather_data$date,5,6)
date_day                      <- str_sub(weather_data$date,7,8)
weather_data$date             <- paste(date_year, date_month, date_day, sep ="-")
weather_data$date             <- as.Date(as.character(weather_data$date))
weather_data$Niederschlag     <- as.numeric(weather_data$Niederschlag)
weather_data$Sonnenstunden    <- as.numeric(weather_data$Sonnenstunden)
names(weather_data)[names(weather_data) ==  "TMK.daily.mean.of.temperature..C"] <- "DMOT" 
weather_data$DMOT             <- as.numeric(weather_data$DMOT)
weather_data$TempMax          <- as.numeric(weather_data$TempMax)
weather_data$TempMin          <- as.numeric(weather_data$TempMin)

### create lagged values of weather data
for ( z in c("Niederschlag","Sonnenstunden","DMOT","TempMax","TempMin")){
  for (i in 1:14){
    weather_data[[paste0(z,"_lag_",i)]] <- lag(x = weather_data[[z]], n = i)
  }
}

weather_data <- weather_data[-c(1:14),]



dat_formated$Bundesland[dat_formated$Bundesland == "Baden-Württemberg"] <- "Baden-W?rttemberg"
dat_formated$Bundesland[dat_formated$Bundesland == "Thüringen"] <- "Th?ringen"

dat_formated <- left_join(dat_formated, weather_data, by = c("Bundesland", "date"))

load("raw-data/measures_state.RData")
names(measures.state)[names(measures.state) == "Startdatum"] <- "date"

measures.state$date <- as.Date(as.character(measures.state$date))
variables_to_replace <- c("<=1000","<=100","<=50","<=10","<=5","Schulschließung","Geschäftsschließung", "Reiserückkehrer", "Lockdown")
for ( i in variables_to_replace ){
  measures.state[[i]] <- as.numeric(measures.state[[i]])
}

dat_formated <- left_join(dat_formated, measures.state, by = c("date","Bundesland")) 


dat_formated <- dat_formated %>% split(., .$Landkreis) %>% lapply(., function(z){
  z <- z[order(z$date),]
  replacement <- rep(0,length(variables_to_replace))
  
  for(i in 1:dim(z)[1]){
    if (sum(is.na(z[i,variables_to_replace])) == 0){
      replacement <- z[i,variables_to_replace]
    }
    z[i,variables_to_replace] <- replacement
    
  }
  return(z)
}) %>% do.call("rbind",.)

## estimate model
# dat_formated <- dat_formated[!duplicated(dat_formated),]
res <- dat_formated %>% split(.,.$Landkreis) %>% lapply(., function(z){
  if ( dim(z)[1] > 1){  
    z <- z[order(z$date),]
    growth <- z$AnzahlFallTag %>% cumsum(.) %>% log(.) %>% diff(.)
    proportionwomen <- cumsum(z$AnzahlFallweiblich)[1:(dim(z)[1]-1)]/cumsum(z$AnzahlFallTag)[1:(dim(z)[1]-1)]
    proportion35    <- cumsum(z$Anzahl35bis59)[1:(dim(z)[1]-1)]/cumsum(z$AnzahlFallTag)[1:(dim(z)[1]-1)]
    proportion60    <- cumsum(z$Anzahl60plus)[1:(dim(z)[1]-1)]/cumsum(z$AnzahlFallTag)[1:(dim(z)[1]-1)]
    res <- data.frame(growth = growth, proportionwomen = proportionwomen, proportion35 = proportion35, proportion60 = proportion60)
    for (i in names(z)[17:length(z)]) {
      res[[i]] <- z[[i]][1:(dim(z)[1]-1)]
    }
    res$date <- z$date[1:(dim(z)[1]-1)]
    res$IdLandkreis   <- z$IdLandkreis[1]
    return(res)
  } else {
    return(NULL)
  }
}) %>% do.call("rbind",.)# %>% mean(.)

names(res)[names(res) == "<=1000"]  <- "thousand"
names(res)[names(res) == "<=100"]   <- "hundred"
names(res)[names(res) == "<=50"]    <- "fifty"
names(res)[names(res) == "<=10"]    <- "ten"
names(res)[names(res) == "<=5"]     <- "five"


attach(res)
reslm <- lm(formula = formula(res[,!names(res) %in% c("date", "IdLandkreis")]) )
lmAIC <- MASS::stepAIC(object = reslm,scope=list(upper= ~ proportionwomen + proportion35 + proportion60 + Niederschlag_lag_1 + Niederschlag_lag_2 + Niederschlag_lag_3 + Niederschlag_lag_4 + Niederschlag_lag_5 + Niederschlag_lag_6 + Niederschlag_lag_7 + Niederschlag_lag_8 + Niederschlag_lag_9 + Niederschlag_lag_10 + Niederschlag_lag_11 + Niederschlag_lag_12 + Niederschlag_lag_13 + Niederschlag_lag_14 + Sonnenstunden_lag_1 + Sonnenstunden_lag_2 + Sonnenstunden_lag_3 + Sonnenstunden_lag_4 + Sonnenstunden_lag_5 + Sonnenstunden_lag_6 + Sonnenstunden_lag_7 + Sonnenstunden_lag_8 + Sonnenstunden_lag_9 + Sonnenstunden_lag_10 + Sonnenstunden_lag_11 + Sonnenstunden_lag_12 + Sonnenstunden_lag_13 + Sonnenstunden_lag_14 + DMOT_lag_1 + DMOT_lag_2 + DMOT_lag_3 + DMOT_lag_4 + DMOT_lag_5 + DMOT_lag_6 + DMOT_lag_7 + DMOT_lag_8 + DMOT_lag_9 + DMOT_lag_10 + DMOT_lag_11 + DMOT_lag_12 + DMOT_lag_13 + DMOT_lag_14 + TempMax_lag_1 + TempMax_lag_2 + TempMax_lag_3 + TempMax_lag_4 + TempMax_lag_5 + TempMax_lag_6 + TempMax_lag_7 + TempMax_lag_8 + TempMax_lag_9 + TempMax_lag_10 + TempMax_lag_11 + TempMax_lag_12 + TempMax_lag_13 + TempMax_lag_14 + TempMin_lag_1 + TempMin_lag_2 + TempMin_lag_3 + TempMin_lag_4 + TempMin_lag_5 + TempMin_lag_6 + TempMin_lag_7 + TempMin_lag_8 + TempMin_lag_9 + TempMin_lag_10 + TempMin_lag_11 + TempMin_lag_12 + TempMin_lag_13 + TempMin_lag_14 + thousand + hundred + fifty + ten + five + Schulschlie?ung + Gesch?ftsschlie?ung + Reiser?ckkehrer + Lockdown, lower=~1))
res$estimated_growth_rate <- exp(lmAIC$fitted.values)
summary(lmAIC)
detach(res)
endresult <- left_join(dat_formated,res, by = c("IdLandkreis", "date", "Niederschlag_lag_1", "Niederschlag_lag_2", "Niederschlag_lag_3", "Niederschlag_lag_4", "Niederschlag_lag_5", "Niederschlag_lag_6", "Niederschlag_lag_7", "Niederschlag_lag_8", "Niederschlag_lag_9", "Niederschlag_lag_10", "Niederschlag_lag_11", "Niederschlag_lag_12", "Niederschlag_lag_13", "Niederschlag_lag_14", "Sonnenstunden_lag_1", "Sonnenstunden_lag_2", "Sonnenstunden_lag_3", "Sonnenstunden_lag_4", "Sonnenstunden_lag_5", "Sonnenstunden_lag_6", "Sonnenstunden_lag_7", "Sonnenstunden_lag_8", "Sonnenstunden_lag_9", "Sonnenstunden_lag_10", "Sonnenstunden_lag_11", "Sonnenstunden_lag_12", "Sonnenstunden_lag_13", "Sonnenstunden_lag_14", "DMOT_lag_1", "DMOT_lag_2", "DMOT_lag_3", "DMOT_lag_4", "DMOT_lag_5", "DMOT_lag_6", "DMOT_lag_7", "DMOT_lag_8", "DMOT_lag_9", "DMOT_lag_10", "DMOT_lag_11", "DMOT_lag_12", "DMOT_lag_13", "DMOT_lag_14", "TempMax_lag_1", "TempMax_lag_2", "TempMax_lag_3", "TempMax_lag_4", "TempMax_lag_5", "TempMax_lag_6", "TempMax_lag_7", "TempMax_lag_8", "TempMax_lag_9", "TempMax_lag_10", "TempMax_lag_11", "TempMax_lag_12", "TempMax_lag_13", "TempMax_lag_14", "TempMin_lag_1", "TempMin_lag_2", "TempMin_lag_3", "TempMin_lag_4", "TempMin_lag_5", "TempMin_lag_6", "TempMin_lag_7", "TempMin_lag_8", "TempMin_lag_9", "TempMin_lag_10", "TempMin_lag_11", "TempMin_lag_12", "TempMin_lag_13", "TempMin_lag_14", "Schulschlie?ung", "Gesch?ftsschlie?ung", "Reiser?ckkehrer", "Lockdown"))

endresult <- endresult %>% split(., .$Landkreis) %>% lapply(., function(z){
  z$Fallgesamt <- cumsum(z$AnzahlFallTag)
  return(z)
}) %>% do.call("rbind", . )

Landkreis_liste <- endresult[,c("Bundesland", "Landkreis", "date", "Fallgesamt", "AnzahlFallTag", "estimated_growth_rate")] %>% split(., .$Landkreis) %>% lapply(., function(z){
  z <- z[order(z$date),]
  z$predicted_values <- 0
  for ( i in 1:dim(z)[1]){
    if ( i == 1){
      z$predicted_values[i] <- z$Fallgesamt[i]
    } else {
      z$predicted_values[i] <- z$Fallgesamt[i-1] * z$estimated_growth_rate[i-1]
    }
  }
  return(z)
})

overview <- lapply(1:length(Landkreis_liste), function(z){
  data.frame(num = z, leng = dim(Landkreis_liste[[z]])[1])
}) %>% do.call("rbind", .)
overview <- overview[order(overview$leng, decreasing = TRUE),]
#setwd(paste0(wd,"/plots/"))

for ( i in 1:20){

k <- Landkreis_liste[[overview$num[i]]]

plot(1:dim(k)[1], k$Fallgesamt, type = "l", col = "blue", ylim = c(0,max(c(k$predicted_values,k$Fallgesamt))+5), ylab = paste("F?lle",names(Landkreis_liste)[i], sep = " "), xlab = "Tage seit Erstinfektion")
lines(1:dim(k)[1], k$predicted_values, lty = 2 )
png(filename = paste0(i,".png"),)
}


