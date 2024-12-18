## CLIMATE AND PEACE PROJECTS ##

# 1. Do climate shocks increase rebel demands?
# 2. Are these demands met in peace agreements?

rm(list = ls())

# Check split with resource rents from wb?


## DATA SOURCES ##

# Climate (EMDAT): https://www.emdat.be/
# Conflict Demands (UCDP ISSUES DATA): https://ucdp.uu.se/downloads/index.html#cid
## See codebook: https://ucdp.uu.se/downloads/cid/UCDP_CID_Codebook_231.pdf
# Peace Agreements (PAX & PAAX): https://pax.peaceagreements.org/agreements/search/
# -- PAX Codebook: https://pax.peaceagreements.org/media/documents/PA_X_codebook_v8.pdf
# -- PAAX Codebook: https://github.com/peacerep/paax_data/blob/main/All%20Signatory%20and%20Actor%20Datasets%20Overview%20v8.pdf
# Cliamte laws and policies: https://climate-laws.org/?c=Legislation&sf=date&so=asc 

############################################################
### First look: climate demands met in peace agreements? ###
############################################################

#### Variables of Interest #### 

# -- UCDP Actor ID (Rebels' demands)
# ---- UCDP: side_b_id
# ---- PAAX: ucdp_id
# -- Year
# ---- UCDP: year
# ---- PAX: Dat (must convert to year)
# -- PAX Agreement ID (AgtId)
# -- Water resources (5202)
# -- PAX Water provision (Wat)
# -- Land reforms (5201)
# -- PAX Land provision (LaRefMan)
# -- Revenues from natural resources (5203) + Protection of natural resources/the environment (5204)
# -- PAX Natural Resources (NatRes)

library(readr)
library(readr)
library(dplyr)
library(lubridate)
library(corrplot)
library(data.table)
library(ggplot2)
library(tidyr)
library(stringr)
library(plm)
library(countrycode)
library(did)
library(readxl)

# 1989-2017

# Step 1 - Import UCDP ISSUES; PAX; PAAX

paxv8 <- read_csv("paxv8.csv") %>%
  rename(conflict_id = UcdpCon)

ucdp_issues <- read_excel("ucdp_issues.xlsx")


# Step 3 - Clean UCDP 

ucdp_issues <- ucdp_issues %>%
  mutate(NatRes_demand = `5203` + `5204`)

ucdp_issues <- ucdp_issues %>%
  rename(
    Wat_demand = `5202`,
    LaRefMan_demand = `5201`, 
    ucdp_id = side_b_id
    
    )

ucdp_issues <- ucdp_issues %>% 
  select(conflict_id, ucdp_id, year, LaRefMan_demand, Wat_demand, NatRes_demand)


# Step 4 - only keep observations with shared ucdp_ids

unique_ucdp_issues <- unique(ucdp_issues$conflict_id)
unique_pax_paax <- unique(paxv8$conflict_id)
common_ucdp_ids <- intersect(unique_ucdp_issues, unique_pax_paax)
ucdp_issues_subset <- ucdp_issues %>%
  filter(conflict_id %in% common_ucdp_ids)
paxv8 <- paxv8 %>%
  filter(conflict_id %in% common_ucdp_ids)



# Step 5 - Merge PAX/PAAX with UCDP

paxv8$Dat <- mdy(paxv8$Dat)
paxv8$year <- year(paxv8$Dat)

collapsed_pax <- paxv8 %>%
  group_by(conflict_id, year) %>%
  summarize(
    count = n(),                              # Count of occurrences
    Wat = sum(Wat, na.rm = TRUE),            # Sum of Wat
    LaRefMan = sum(LaRefMan, na.rm = TRUE),  # Sum of LaRefMan
    NatRes = sum(NatRes, na.rm = TRUE),      # Sum of NatRes
    .groups = 'drop'
  )


ucdp_issues_with_agreement <- ucdp_issues_subset %>%
  left_join(paxv8 %>% select(conflict_id, year), by = "conflict_id")

collapsed_ucdp <- ucdp_issues_subset %>%
  group_by(conflict_id, year) %>%
  summarize(
    count = n(),                                # Count of occurrences
    LaRefMan_demand = sum(LaRefMan_demand, na.rm = TRUE),  # Sum of LaRefMan_demand
    Wat_demand = sum(Wat_demand, na.rm = TRUE),            # Sum of Wat_demand
    NatRes_demand = sum(NatRes_demand, na.rm = TRUE),      # Sum of NatRes_demand
    .groups = 'drop'
  )

# Merge the two filtered data frames by conflict_id and year
# Step 1: Create a shell with all unique conflict_ids and all years from 1989 to 2018
unique_conflict_ids <- unique(c(collapsed_pax$conflict_id, collapsed_ucdp$conflict_id))
years <- 1989:2018

# Create the shell data frame
shell_df <- expand.grid(conflict_id = unique_conflict_ids, year = years)

# Step 2: Merge the shell with the collapsed data
merged_data <- shell_df %>%
  left_join(collapsed_pax, by = c("conflict_id", "year")) %>%
  left_join(collapsed_ucdp, by = c("conflict_id", "year"))

merged_data <- merged_data %>%
  rename(
    total_agmt = count.x,  # Rename count.x to total_agmt
    total_demand = count.y  # Rename count.y to total_demand
  )

aggregated_data <- merged_data %>%
  group_by(year) %>%
  summarize(
    total_agmt = sum(total_agmt, na.rm = TRUE),
    total_demand = sum(total_demand, na.rm = TRUE),
    LaRefMan_total = sum(LaRefMan, na.rm = TRUE),
    Wat_total = sum(Wat, na.rm = TRUE),
    NatRes_total = sum(NatRes, na.rm = TRUE),
    LaRefMan_demand_total = sum(LaRefMan_demand, na.rm = TRUE),
    Wat_demand_total = sum(Wat_demand, na.rm = TRUE),
    NatRes_demand_total = sum(NatRes_demand, na.rm = TRUE)
  )
# Step 1: Rename the variables for clarity
aggregated_data <- aggregated_data %>%
  rename(
    Land_Reform_Provision_Total = LaRefMan_total,
    Land_Reform_Demand_Total = LaRefMan_demand_total,
    Water_Provision_Total = Wat_total,
    Water_Demand_Total = Wat_demand_total,
    Natural_Provision_Resources_Total = NatRes_total,
    Natural_Resources_Demand_Total = NatRes_demand_total
  )


# Plot 1: Land Reform Provision Total vs Land Reform Demand Total
# Load the necessary library
library(ggplot2)

# Assuming `aggregated_data` is your data frame
# Load the necessary library
library(ggplot2)

# Assuming `aggregated_data` is your data frame
# Plot 1: Land Reform Provision Total vs Land Reform Demand Total
p1 <- ggplot(aggregated_data, aes(x = year)) +
  geom_line(aes(y = Land_Reform_Provision_Total, color = "Land Reform Provision Total")) +
  geom_line(aes(y = Land_Reform_Demand_Total, color = "Land Reform Demand Total")) +
  geom_point(aes(y = Land_Reform_Provision_Total, color = "Land Reform Provision Total")) +
  geom_point(aes(y = Land_Reform_Demand_Total, color = "Land Reform Demand Total")) +
  labs(title = "Land Reform Provision Total vs Land Reform Demand Total (1989-2018)",
       x = "Year",
       y = "Total Count",
       color = "Variable") +
  scale_color_manual(values = c("Land Reform Provision Total" = "saddlebrown",
                                "Land Reform Demand Total" = "goldenrod")) +
  theme_minimal() +
  theme(text = element_text(size = 12, family = "Times New Roman"))

# Print the plot
print(p1)

# Plot 2: Water Provision Total vs Water Demand Total
p2 <- ggplot(aggregated_data, aes(x = year)) +
  geom_line(aes(y = Water_Provision_Total, color = "Water Provision Total")) +
  geom_line(aes(y = Water_Demand_Total, color = "Water Demand Total")) +
  geom_point(aes(y = Water_Provision_Total, color = "Water Provision Total")) +
  geom_point(aes(y = Water_Demand_Total, color = "Water Demand Total")) +
  labs(title = "Water Provision Total vs Water Demand Total (1989-2018)",
       x = "Year",
       y = "Total Count",
       color = "Variable") +
  scale_color_manual(values = c("Water Provision Total" = "navy",
                                "Water Demand Total" = "deepskyblue")) +
  theme_minimal() +
  theme(text = element_text(size = 12, family = "Times New Roman"))

# Print the plot
print(p2)

# Plot 3: Natural Resources Provision Total vs Natural Resources Demand Total
p3 <- ggplot(aggregated_data, aes(x = year)) +
  geom_line(aes(y = Natural_Provision_Resources_Total, color = "Natural Resources Provision Total")) +
  geom_line(aes(y = Natural_Resources_Demand_Total, color = "Natural Resources Demand Total")) +
  geom_point(aes(y = Natural_Provision_Resources_Total, color = "Natural Resources Provision Total")) +
  geom_point(aes(y = Natural_Resources_Demand_Total, color = "Natural Resources Demand Total")) +
  labs(title = "Natural Resources Provision Total vs Natural Resources Demand Total (1989-2018)",
       x = "Year",
       y = "Total Count",
       color = "Variable") +
  scale_color_manual(values = c("Natural Resources Provision Total" = "forestgreen",
                                "Natural Resources Demand Total" = "lightgreen")) +
  theme_minimal() +
  theme(text = element_text(size = 12, family = "Times New Roman"))

# Print the plot
print(p3)



### Relationships

merged_data <- merged_data %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# Set the data frame as a panel data frame
pdata <- pdata.frame(merged_data, index = c("conflict_id", "year"))

# Create lagged variables for the dependent variables
# Create lagged variables for Wat, LaRefMan, and NatRes
pdata$Wat_lag <- ave(pdata$Wat, pdata$conflict_id, FUN = function(x) c(NA, head(x, -1)))
pdata$LaRefMan_lag <- ave(pdata$LaRefMan, pdata$conflict_id, FUN = function(x) c(NA, head(x, -1)))
pdata$NatRes_lag <- ave(pdata$NatRes, pdata$conflict_id, FUN = function(x) c(NA, head(x, -1)))

# Model for Water Demand leading to Water Provision
water_model <- plm(Wat_lag ~ Wat_demand, data = pdata, model = "within")

# Model for Land Reform Demand leading to Land Reform Provision
land_model <- plm(LaRefMan_lag ~ LaRefMan_demand, data = pdata, model = "within")

# Model for Natural Resources Demand leading to Natural Resources Provision
natural_model <- plm(NatRes_lag ~ NatRes_demand, data = pdata, model = "within")

# Display the summaries of the models
summary(water_model) # null
summary(land_model) # lower land higher provisions
summary(natural_model) # higher nat higher provisions











###########################################################################
### First look: climate shocks lead to climate demands? ###
###########################################################################

emdat <- read_excel("emdat.xlsx")
demands <- ucdp_issues

ucdp_actor <- read_csv("ucdp_actor.csv")
ucdp_actor_long <- ucdp_actor %>%
  select(ActorId, GWNOLoc) %>%
  rename(ucdp_id = ActorId) %>%
  separate_rows(GWNOLoc, sep = ",")
years <- tibble(year = 1989:2017)
ucdp_actor_expanded <- ucdp_actor_long %>%
  crossing(years)

UCDP <- ucdp_actor_expanded %>%
  left_join(demands, by = c("ucdp_id", "year"), relationship = "many-to-many")

UCDP_summary <- UCDP %>%
  group_by(GWNOLoc, year) %>%
  summarize(
    Wat_demand_sum = sum(Wat_demand, na.rm = TRUE),
    LaRefMan_demand_sum = sum(LaRefMan_demand, na.rm = TRUE),
    NatRes_demand_sum = sum(NatRes_demand, na.rm = TRUE),
    .groups = "drop" # Drop grouping after summarizing
  ) %>%
  # Step 3: Arrange by GWNOLoc and year
  arrange(GWNOLoc, year) %>%
  # Step 4: Add cumulative sums
  group_by(GWNOLoc) %>%
  mutate(
    Wat_demand_cumsum = cumsum(Wat_demand_sum),
    LaRefMan_demand_cumsum = cumsum(LaRefMan_demand_sum),
    NatRes_demand_cumsum = cumsum(NatRes_demand_sum)
  ) %>%
  # Ungroup to finalize the data frame
  ungroup()

UCDP_summary <- UCDP_summary %>%
  mutate(GWNOLoc = as.numeric(GWNOLoc))
UCDP_summary <- UCDP_summary %>%
  mutate(ISO = countrycode(GWNOLoc, "gwn", "iso3c"))


# Climate Types

unique_disaster_types <- emdat %>%
  pull('Disaster Type') %>%
  unique()
print(unique_disaster_types)

## Water
#     Storm, Flood, Mass movement (wet), Drought
## Land Reform
#     All
## Natural Resources
#     All 

emdat_wet <- emdat %>%
  filter(`Disaster Type` %in% c("Storm", "Flood", "Mass movement (wet)", "Drought")) %>%
  select(ISO, `Start Year`, `Disaster Type`) %>%
  arrange(ISO, `Start Year`) %>%
  group_by(ISO) %>%
  slice(1) %>%
  ungroup() %>%
  rename(treat_wet = `Start Year`) %>%
  mutate(GWNOLoc = countrycode(ISO, "iso3c", "gwn")) %>%
  select(GWNOLoc, ISO, treat_wet) 

emdat_all <- emdat %>%
  select(ISO, `Start Year`, `Disaster Type`) %>%
  arrange(ISO, `Start Year`) %>%
  group_by(ISO) %>%
  slice(1) %>%
  ungroup() %>%
  rename(treat_all = `Start Year`) %>%
  mutate(GWNOLoc = countrycode(ISO, "iso3c", "gwn")) %>%
  select(GWNOLoc,ISO, treat_all) 



# Perform the left joins
clim_to_demand <- UCDP_summary %>%
  left_join(emdat_wet, by = "ISO") %>%
  left_join(emdat_all, by = "ISO")


clim_to_demand <- clim_to_demand %>%
  mutate(
    iso3n = countrycode(ISO, origin = "iso3c", destination = "iso3n")
  )

clim_to_demand <- clim_to_demand %>%
  mutate(treat_wet = replace_na(treat_wet, 0),
         treat_all = replace_na(treat_all, 0))


### ANALYSIS
print(colnames(clim_to_demand))

# Wat_demand_cumsum

result <- att_gt(yname = "Wat_demand_cumsum",
                 tname = "year",
                 idname = "iso3n",
                 gname = "treat_wet",
                 data = clim_to_demand, anticipation = 1,
                 control_group="notyettreated", 
                 bstrap = TRUE,
                 clustervars = "iso3n",
                 panel = FALSE)


stats_event <- aggte(result, type = "dynamic", na.rm = TRUE)
print(summary(stats_event))
stats_calendar <- aggte(result, type = "calendar", na.rm = TRUE)
print(summary(stats_calendar))

# Merge time effects onto pdata by year
# Create the dataframe
time_effects_df <- data.frame(
  Year = c(1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 
           2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
           2012, 2013, 2014, 2015, 2016, 2017),
  att_wat = c(0.0461, 0.0220, 0.0026, 0.0031, 0.0002, -0.0144, -0.0055, 
               0.0141, 0.0607, 0.0841, 0.1009, 0.1041, 0.1312, 0.1448, 
               0.1486, 0.1757, 0.1875, 0.1991, 0.2124, 0.2699, 0.3053, 
               0.3717, 0.4248, 0.4381, 0.4956, 0.5044)
)

pdata <- merge(pdata, time_effects_df, by.x = "year", by.y = "Year", all.x = TRUE)
wat_model_att <- plm(Wat_lag ~ Wat_demand + att_wat , data = pdata, model = "random")



ggplot <- ggdid(stats_event) +
  labs(
    title = "DID Event Plot: Water-related climate shocks and water resource demands",
    x = "Time to Treatment (Years)",
    y = "Average Treatment Effect (ATT)"
  ) +
  scale_color_manual(values = c("navy", "deepskyblue")) +  # Set the custom colors

  theme(
    plot.title = element_text(hjust = 0.5, family = "Times", color = "black"), # Center the plot title
    axis.text = element_text(family = "Times", color = "black"), # Change axis text font and color
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Times", color = "black"), # Adjust size, angle, and font for x-axis text
    axis.text.y = element_text(family = "Times", color = "black"), # Change y-axis text font and color
    axis.title.x = element_text(margin = margin(t = 10), family = "Times", color = "black"), # Adjust margin and font for x-axis title
    axis.title.y = element_text(margin = margin(r = 10), family = "Times", color = "black"), # Adjust margin and font for y-axis title
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )


print(ggplot)
# NatRes_demand_cumsum
result <- att_gt(yname = "NatRes_demand_cumsum",
                 tname = "year",
                 idname = "iso3n",
                 gname = "treat_all",
                 data = clim_to_demand, anticipation = 1,
                 control_group="notyettreated", 
                 bstrap = TRUE,
                 clustervars = "iso3n",
                 panel = FALSE)


stats_event <- aggte(result, type = "dynamic", na.rm = TRUE)
print(summary(stats_event))
stats_calendar <- aggte(result, type = "calendar", na.rm = TRUE)
print(summary(stats_calendar))



time_effects_df2 <- data.frame(
  Year = c(1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 
           2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
           2012, 2013, 2014, 2015, 2016, 2017),
  att_natres = c(0.0578, 0.1140, 0.1791, 0.1762, 0.2110, 0.2095, 0.2328, 
               0.2072, 0.1952, 0.2276, -0.1263, -0.2063, -0.2922, -0.2205, 
               -0.3437, -0.3920, 1.4248, 1.4737, 1.5702, 1.7281, 1.9035, 
               2.0833, 2.2105, 2.3202, 2.3904, 2.4737)
)
pdata <- merge(pdata, time_effects_df2, by.x = "year", by.y = "Year", all.x = TRUE)
natres_model_att <- plm(NatRes_lag ~ NatRes_demand + att_natres , data = pdata, model = "random")


ggplot <- ggdid(stats_event) +
  labs(
    title = "DID Event Plot: Climate shocks and natural resource demands",
    x = "Time to Treatment (Years)",
    y = "Average Treatment Effect (ATT)"
  ) +
  scale_color_manual(values = c("lightgreen", "forestgreen")) +  # Set the custom colors
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times", color = "black"), # Center the plot title
    axis.text = element_text(family = "Times", color = "black"), # Change axis text font and color
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Times", color = "black"), # Adjust size, angle, and font for x-axis text
    axis.text.y = element_text(family = "Times", color = "black"), # Change y-axis text font and color
    axis.title.x = element_text(margin = margin(t = 10), family = "Times", color = "black"), # Adjust margin and font for x-axis title
    axis.title.y = element_text(margin = margin(r = 10), family = "Times", color = "black"), # Adjust margin and font for y-axis title
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )

print(ggplot)


# LaRefMan_demand_cumsum

result <- att_gt(yname = "LaRefMan_demand_cumsum",
                 tname = "year",
                 idname = "iso3n",
                 gname = "treat_all",
                 data = clim_to_demand, anticipation = 1,
                 control_group="notyettreated", 
                 bstrap = TRUE,
                 clustervars = "iso3n",
                 panel = FALSE)


stats_event <- aggte(result, type = "dynamic", na.rm = TRUE)
print(summary(stats_event))

stats_calendar <- aggte(result, type = "calendar", na.rm = TRUE)
print(summary(stats_calendar))



# Manually create a data frame for the time effects
time_effects <- data.frame(
  Year = c(1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 
           2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
           2012, 2013, 2014, 2015, 2016, 2017),
  att_laref = c(0.0497, 0.0903, 0.0877, 0.1022, 0.1128, 0.1307, 0.1546, 
               0.1682, 0.1713, 0.1806, 0.2182, 0.2242, 0.2601, 0.2960, 
               0.1482, 0.0196, 0.4735, 0.5307, 0.5921, 0.6316, 0.6974, 
               0.7632, 0.8289, 0.8904, 0.9649, 1.0263)
)

pdata <- merge(pdata, time_effects, by.x = "year", by.y = "Year", all.x = TRUE)
land_model_att <- plm(LaRefMan_lag ~ LaRefMan_demand + att_laref , data = pdata, model = "random")


ggplot <- ggdid(stats_event) +
  labs(
    title = "DID Event Plot: Climate shocks and land reform demands",
    x = "Time to Treatment (Years)",
    y = "Average Treatment Effect (ATT)"
  ) +
  scale_color_manual(values = c("goldenrod", "saddlebrown")) +  # Set the custom colors
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times", color = "black"), # Center the plot title
    axis.text = element_text(family = "Times", color = "black"), # Change axis text font and color
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Times", color = "black"), # Adjust size, angle, and font for x-axis text
    axis.text.y = element_text(family = "Times", color = "black"), # Change y-axis text font and color
    axis.title.x = element_text(margin = margin(t = 10), family = "Times", color = "black"), # Adjust margin and font for x-axis title
    axis.title.y = element_text(margin = margin(r = 10), family = "Times", color = "black"), # Adjust margin and font for y-axis title
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )

print(ggplot)

# / total agmts
pdata$Wat_divided <- ifelse(pdata$total_agmt != 0, pdata$Wat / pdata$total_agmt, 0)
pdata$NatRes_divided <- ifelse(pdata$total_agmt != 0, pdata$NatRes / pdata$total_agmt, 0)
pdata$LaRefMan_divided <- ifelse(pdata$total_agmt != 0, pdata$LaRefMan / pdata$total_agmt, 0)

pdata$Wat_lag <- ave(pdata$Wat_divided, pdata$conflict_id, FUN = function(x) c(NA, head(x, -7)))
pdata$LaRefMan_lag <- ave(pdata$LaRefMan_divided, pdata$conflict_id, FUN = function(x) c(NA, head(x, -1)))
pdata$NatRes_lag <- ave(pdata$NatRes_divided, pdata$conflict_id, FUN = function(x) c(NA, head(x, -5)))


land_model_att <- plm(LaRefMan_lag ~ LaRefMan_demand + att_laref  , data = pdata, model = "random")
wat_model_att <- plm(Wat_lag ~ Wat_demand + att_wat , data = pdata, model = "random")
natres_model_att <- plm(NatRes_lag ~ NatRes_demand + att_natres , data = pdata, model = "random")

land_model <- plm(LaRefMan_lag ~ LaRefMan_demand   , data = pdata, model = "within")
wat_model <- plm(Wat_lag ~ Wat_demand  , data = pdata, model = "within")
natres_model <- plm(NatRes_lag ~ NatRes_demand  , data = pdata, model = "within")
summary(land_model)
summary(wat_model)
summary(natres_model)


summary(land_model_att)
summary(wat_model_att)
summary(natres_model_att)


# Create the dataframe with the coefficients
coef_df <- data.frame(
  Demand = c("Land Reform Demands", "Water Demands", "Natural Resource Demands"),
  Estimate = c(0.0138435, -0.0062699, 0.0452369),
  StdError = c(0.0092252, 0.0191320, 0.0082007)
)

# Create the plot with specified colors
coef_plot <- ggplot(coef_df, aes(x = Demand, y = Estimate, color = Demand)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * StdError, ymax = Estimate + 1.96 * StdError), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  scale_color_manual(values = c("Land Reform Demands" = "saddlebrown", "Water Demands" = "navy", "Natural Resource Demands" = "forestgreen")) +
  theme_minimal(base_family = "Times New Roman") +
  labs(title = "Coefficient Plot: Increase in Peace Agreement Provisions",
       x = "Demand Type",
       y = "Estimate",
       color = "Demand Type")

# Print the plot
print(coef_plot)


############
############
############
############
############
############


# RUN TOTAL climate and demand and provisions maps by country 
# climate maybe point overlayed


## Split DID sample with renty vs non renty states
# wb data












prio <- read_excel("UcdpPrioConflict_v24_1.xlsx")

pdata$conflict_id <- as.character(pdata$conflict_id)

pdata_dt <- as.data.table(pdata)

pdata_dt <- pdata_dt %>%
  left_join(prio %>% select(conflict_id, year, gwno_loc), 
            by = c("conflict_id"))

# Calculate the percentage of missing gwno_loc after the join
missing_percentage <- sum(is.na(pdata_dt$gwno_loc)) / nrow(pdata_dt) * 100

# Print the percentage of missing gwno_loc
cat("Percentage of missing gwno_loc after join:", missing_percentage, "%\n")


demand_map <- pdata_dt[, .(
  NatRes_demand = sum(NatRes_demand, na.rm = TRUE),
  LaRefMan_demand = sum(LaRefMan_demand, na.rm = TRUE),
  Wat_demand = sum(Wat_demand, na.rm = TRUE)
), by = gwno_loc]

demand_map[, total_clim := NatRes_demand + LaRefMan_demand + Wat_demand]

sf_map <- ne_countries(scale = "medium", returnclass = "sf")

sf_map <- sf_map %>%
  mutate(gwno_loc = countrycode::countrycode(iso_a3, 
                                             origin = "iso3c", 
                                             destination = "gwn"))

sf_map$gwno_loc <- as.character(sf_map$gwno_loc)
demand_map$gwno_loc <- as.character(demand_map$gwno_loc)

sf_map <- sf_map %>%
  left_join(demand_map[, .(gwno_loc, total_clim)], by = c("gwno_loc"))

sf_map <- sf_map %>%
  mutate(total_clim_log = log(total_clim + 1e-5))  # Add a small constant to avoid log(0)

sf_map <- sf_map %>%
  mutate(total_clim_scaled = (total_clim_log - min(total_clim_log, na.rm = TRUE)) / 
           (max(total_clim_log, na.rm = TRUE) - min(total_clim_log, na.rm = TRUE)) * 5)


# Define the custom color palette (shades of blue, green, and brown)
custom_colors <- c(  # Very light blue
  # Light blue
  "#9ecae1",   # Medium light blue
  "#3182bd",   # Strong blue
  "#0868ac",   # Dark blue
  "#e6d4b8",   # Light tan/brown
  "#d2b48c",   # Tan/brown
  "#c59768",   # Medium brown
  "#8b4513")   # Saddle brown

# Step 4: Plot levels of total_clim by country
dem_map <- ggplot(data = sf_map) +
  geom_sf(aes(fill = total_clim_scaled)) +
  scale_fill_gradientn(colors = custom_colors, name = "Total Climate Demand") +
  labs(title = "Total Climate Demand by Country (logged)",
       subtitle = "Sum of Natural Resource, Land Management, and Water Demands",
       caption = "Data Source: UCDP PRIO") +
  theme_minimal() +
  theme(legend.position = "right")
print(dem_map)

prov_map <- pdata_dt[, .(
  NatRes = sum(NatRes, na.rm = TRUE),
  LaRefMan = sum(LaRefMan, na.rm = TRUE),
  Wat = sum(Wat, na.rm = TRUE)
), by = gwno_loc]

prov_map[, total_prov := NatRes + LaRefMan  + Wat]

sf_map <- sf_map %>%
  left_join(prov_map[, .(gwno_loc, total_prov)], by = c("gwno_loc"))

sf_map <- sf_map %>%
  mutate(total_prov_log = log(total_prov + 1e-5))  # Add a small constant to avoid log(0)

sf_map <- sf_map %>%
  mutate(total_prov_scaled = (total_prov_log - min(total_prov_log, na.rm = TRUE)) / 
           (max(total_prov_log, na.rm = TRUE) - min(total_prov_log, na.rm = TRUE)) * 5)


library(extrafont)   # for font management



# Define the custom color palette (shades of blue and brown)
custom_colors <- c("#9ecae1",   # Medium light blue
                   "#3182bd",   # Strong blue
                   "#0868ac",   # Dark blue
                   "#e6d4b8",   # Light tan/brown
                   "#d2b48c",   # Tan/brown
                   "#c59768",   # Medium brown
                   "#8b4513")   # Saddle brown

# Create the map plot for total_prov_scaled with custom color scale
prov_map <- ggplot(data = sf_map) +
  geom_sf(aes(fill = total_prov_scaled)) +
  scale_fill_gradientn(colors = custom_colors, name = "Total Climate Provisions") +
  labs(title = "Total Climate Provisions by Country (logged)",
       subtitle = "Sum of Natural Resource, Land Management, and Water Provisions",
       caption = "Data Source: UCDP PRIO") +
  theme_minimal() +
  theme(legend.position = "right",
        text = element_text(family = "Times New Roman"))  # Set font to Times New Roman

# Create the map plot for total_clim_scaled with custom color scale
dem_map <- ggplot(data = sf_map) +
  geom_sf(aes(fill = total_clim_scaled)) +
  scale_fill_gradientn(colors = custom_colors, name = "Total Climate Demand") +
  labs(title = "Total Climate Demand by Country (logged)",
       subtitle = "Sum of Natural Resource, Land Management, and Water Demands",
       caption = "Data Source: UCDP PRIO") +
  theme_minimal() +
  theme(legend.position = "right",
        text = element_text(family = "Times New Roman"))  # Set font to Times New Roman

# Print the plots
print(dem_map)
print(prov_map)

# Combine the two plots into one
combined_plot <- grid.arrange(dem_map, prov_map, ncol = 1)
print(combined_plot)