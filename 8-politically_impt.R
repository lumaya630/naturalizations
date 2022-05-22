# read in pres margins from last three elections
pres_margins <- read.csv("out/pres_margins.csv")
pres_margins <- pres_margins %>% mutate(state = str_to_title(state))%>% 
  mutate(state = recode(state, "District Of Columbia" = "District of Columbia" )) %>%
  select(c(state, margin_2020, margin_2016, margin_2012, average, wpa)) 

# 2020 acs
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("data/usa_00012.xml")
data <- read_ipums_micro(ddi)

# subset to only foreign born
foreign_born <- data %>% subset(BPL > 120)

# group_by state
statefip_labels <- data.frame(STATEFIP = attributes(foreign_born$STATEFIP)$labels) %>% 
  rownames_to_column("state")
foreign_by_state <- foreign_born %>% group_by(STATEFIP) %>% 
  ummarise(n_foreign_born = sum(PERWT)) %>% 
  left_join(statefip_labels, by = "STATEFIP")

# merge into pres margin data
politically_impt <- pres_margins %>% 
  left_join(foreign_by_state %>% 
              select(c(state, n_foreign_born)), by = "state")

# merge in battleground scores from cook report
cook_report <- read.csv("data/politically_impt_states.csv")
politically_impt <-politically_impt %>% left_join(cook_report, by = "state")

# export
write.csv(politically_impt, "out/politically_impt.csv", row.names = F)
