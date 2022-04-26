# Program: Calculate the presidential margin of victories for 2012, 2016, and 2020

library(tidyverse)

# read in presidential margins data (sorce: MIT)
pres <- read.csv("data/1976-2020-president.csv") %>% 
  select( c("year", "state","candidate", "party_detailed",
            "candidatevotes", "totalvotes", "party_simplified"))
pres <- pres %>% subset(year >= 2012 & (party_detailed == "REPUBLICAN" | party_detailed == "DEMOCRAT"))
pres_2012 <- pres %>% subset(year == 2012)
pres_2016 <- pres %>% subset(year == 2016) %>% subset(candidatevotes > 500)
pres_2020 <- pres %>% subset(year == 2020)

# read in senatorial margins data (sorce: MIT)
senate <- read.csv("data/1976-2020-senate.csv") %>% 
  subset(!writein) %>%
  select( c("year", "state","candidate", "party_detailed",
            "candidatevotes", "totalvotes", "party_simplified"))
senate <- senate %>% subset(year >= 2012 ) 
senate <- senate %>% subset(!candidate %in% c("BLANK VOTE","OVER VOTE", "SCATTER", "BLANK VOTE/SCATTERING", "OTHER")) %>%
  group_by(year, state) %>% 
  arrange(year, state, desc(candidatevotes), by_group = TRUE) %>%
       slice(1:2) 
     
senate_2012 <- senate %>% subset(year == 2012)
senate_2014 <- senate %>% subset(year == 2014)
senate_2016 <- senate %>% subset(year == 2016) %>% subset(candidatevotes > 500)
senate_2018 <- senate %>% subset(year == 2018)
senate_2020 <- senate %>% subset(year == 2020)

# =================================================
# PRESIDENTIAL MARGINS
# =================================================
##### 2012 ######
# 2012 election missing minnesota democrat count, add missing row
minnesota_2012 <- data.frame("year" = 2012, "state" = "MINNESOTA", "candidate" = "OBAMA, BARACK H.",
                             "party_detailed" = "DEMOCRAT", "candidatevotes" = 1546167, 
                             "totalvotes" = 2936561, "party_simplified" = "DEMOCRAT")
pres_2012 <- rbind(pres_2012, minnesota_2012)

pres_2012_summary <- pres_2012 %>% group_by(state) %>% 
  summarise(margin = (max(candidatevotes) - min(candidatevotes))/max(totalvotes),
    margincount = max(candidatevotes) - min(candidatevotes)
    ) 


##### 2016 ######
pres_2016_summary <- pres_2016 %>% group_by(state) %>%
  summarise(margin = (max(candidatevotes) - min(candidatevotes))/max(totalvotes),
            margincount = max(candidatevotes) - min(candidatevotes))

##### 2016 ######
pres_2020_summary <- pres_2020 %>% group_by(state) %>%
  summarise(margin = (max(candidatevotes) - min(candidatevotes))/max(totalvotes),
            margincount = max(candidatevotes) - min(candidatevotes))

##### combine all three dataframes ###########
margins <- data.frame(
           state = pres_2020_summary$state,
           margin_2020 = pres_2020_summary$margin,
           margincount_2020 = pres_2020_summary$margincount,
           margin_2016 = pres_2016_summary$margin,
           margincount_2016 = pres_2016_summary$margincount,
           margin_2012 = pres_2012_summary$margin,
           margincount_2012 = pres_2012_summary$margincount)

pres_margins <- margins %>% mutate(
  average = (margin_2020 + margin_2016 + margin_2012)/3,
  wpa = log10(1/average)
  )


# =================================================
# SENATORIAL MARGINS
# =================================================
senate_2012_summary <- senate_2012 %>% group_by(state) %>% 
  summarise(margin = (max(candidatevotes) - min(candidatevotes))/max(totalvotes),
            margincount = max(candidatevotes) - min(candidatevotes)
  ) 

senate_2014_summary <- senate_2014 %>% group_by(state) %>% 
  summarise(margin = (max(candidatevotes) - min(candidatevotes))/max(totalvotes),
            margincount = max(candidatevotes) - min(candidatevotes)
  ) 

senate_2016_summary <- senate_2016 %>% group_by(state) %>% 
  summarise(margin = (max(candidatevotes) - min(candidatevotes))/max(totalvotes),
            margincount = max(candidatevotes) - min(candidatevotes)
  ) 

senate_2018_summary <- senate_2018 %>% group_by(state) %>% 
  summarise(margin = (max(candidatevotes) - min(candidatevotes))/max(totalvotes),
            margincount = max(candidatevotes) - min(candidatevotes)
  ) 

senate_2020_summary <- senate_2020 %>% group_by(state) %>% 
  summarise(margin = (max(candidatevotes) - min(candidatevotes))/max(totalvotes),
            margincount = max(candidatevotes) - min(candidatevotes)
  ) 


states<- sort(unique(c(senate_2012$state, senate_2014$state, senate_2016$state, senate_2018$state)))
senate_margins = data.frame(state = states)
senate_margins <- senate_margins %>% 
  left_join(senate_2020_summary%>% select("state", "margin", "margincount")) %>% 
  rename(margin_2020 = margin, 
         margincount_2020 = margincount) %>%
  left_join(senate_2018_summary %>% select("state", "margin","margincount")) %>% 
  rename(margin_2018 = margin,
         margincount_2018 = margincount)  %>%
  left_join(senate_2016_summary%>% select("state",  "margin","margincount")) %>% 
  rename(margin_2016 = margin,
         margincount_2016 = margincount) %>% 
  left_join(senate_2014_summary%>% select("state",  "margin","margincount")) %>% 
  rename(margin_2014 = margin,
         margincount_2014 = margincount) %>%
  left_join(senate_2012_summary%>% select("state",  "margin","margincount")) %>% 
  rename(margin_2012 = margin,
         margincount_2012 = margincount) 

# =================================================
# EXPORT
# =================================================
write.csv(pres_margins, "out/pres_margins.csv")
write.csv(senate_margins, "out/senate_margins.csv")
