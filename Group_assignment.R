rm(list = ls()) # wipe all objects from the workspace for a clean run

library(readxl)
library(dplyr)
library(stringr)

# Read raw daily station-level price data and compute weekly averages per station

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read_excel("2025.09.02-09.08.xlsx", sheet = "Sheet1")

data <- data %>%
  group_by(Settlement, Company, Address) %>%
  summarise(
    avg_petrol = mean(`Diesel (HUF/l)`, na.rm = TRUE),
    avg_diesel = mean(`Gasoline (HUF/l)`, na.rm = TRUE),
    .groups = "drop"
  )


# Read Localities data
# 'skip = 2' skips the top rows so row 3 becomes the header

localities <- read_excel("dgh_download_2025.xlsx", sheet = "Localities 01.01.2025.",skip=2)

# Convert "Budapest 01. ker." → "Budapest I. kerület" in 'Locality name'
# In the Localities dataset, Budapest districts are written with Arabic numerals (e.g. "Budapest 01. ker." or "Budapest 11. ker.").
# In the main fuel price data, however, these are identified with Roman numerals (e.g. "Budapest I. kerület", "Budapest XI. kerület").
# To ensure a successful match between the 'Settlement' column in the main data and the 'Locality name' column in the Localities file, all Budapest district names are converted from Arabic to Roman numerals, following the format used in the main dataset.
# Without this harmonization, Budapest districts would fail to join correctly and their population or dwelling data would be missing (NA) after the merge.

# Collapse any "Budapest NN. ker." in 'District name' to "Budapest"
# The 'District name' column is later used to connect district-level statistics (e.g. tourism nights, average earnings) to each settlement.
# However, these datasets contain only one aggregated record for the whole of Budapest.
# Therefore, every Budapest district is replaced by a single common label "Budapest" in the 'District name' field.

localities <- localities %>%
  mutate(
    # Detect Budapest + 1–2 digit district with dot and 'ker.' then convert the digit to Roman
    `Locality name` = if_else(
      str_detect(`Locality name`, "^Budapest\\s+\\d{1,2}\\.\\s*ker\\.$"),
      paste0(
        "Budapest ",
        as.character(utils::as.roman(as.integer(str_extract(`Locality name`, "\\d{1,2}")))),
        ". kerület"
      ),
      `Locality name`
    ),
    
    # If 'District name' looks like "Budapest NN. ker.", replace with "Budapest"
    
    `District name` = if_else(
      !is.na(`District name`) &
        str_detect(`District name`, "^Budapest\\s+\\d{1,2}\\.\\s*ker\\.?$"),
      "Budapest",
      `District name`
    )
  )

# Keep only the columns that will be joined later

localities <- localities %>%
  dplyr::select(
    `Locality name`,
    `Locality legal status`,
    `Name of county`,
    `District name`,
    `Resident population`,
    `Number of dwellings`
  ) %>%
rename(
  `Locality`       = `Locality name`,
  `Status`         = `Locality legal status`,
  `County`         = `Name of county`,
  `District`       = `District name`,
  `Resident`       = `Resident population`,
  `Dwellings`      = `Number of dwellings`,
)


# Join Localities → add legal status / county / district / population by Settlement / dwellings by Settlement
# The merge is performed by matching the 'Settlement' column in the fuel price dataset with the 'Locality name' column in the Localities file, ensuring that each station record gets the demographic and administrative attributes of the settlement it belongs to.

data <- left_join(
  data,
  localities,
  by = c("Settlement" = "Locality")
)

# Join Tourism nights per thousand capita (per districts)

tourism_nights <- read_excel("Number of tourism nights spent at commercial accommodation establishments per thousand capita_2024.xlsx", sheet = "Number of tourism nights spent")

tourism_nights <- tourism_nights %>%
  dplyr::select(
    `JARAS_NEV`,
    `VALUE`
  ) %>%
  rename(
    `District`        = `JARAS_NEV`,
    `Tourism_nights`  = `VALUE`,
  )

# District-level left join (only district level data is available)

data <- left_join(
  data,
  tourism_nights,
  by = "District"
)

# Join public roads per hundred km2 at settlement level (handle Budapest districts by mapping them to "Budapest")
# In the main dataset, Budapest districts are listed separately, while in the Public Roads dataset there is only a single aggregated record for the entire city of Budapest.
# For all other settlements, the data is provided at the settlement level. To ensure a consistent merge, Budapest districts are temporarily mapped to the common label "Budapest" during the join process, so that each district receives the same public road density value as the city as a whole, while other settlements are matched directly by their own names.


public_roads <- read_excel("Public roads per hundred km2_2023.xlsx", sheet = "Public roads per hundred km2_20")

public_roads <- public_roads %>%
  dplyr::select(
    `TELEP_NEV`,
    `VALUE`
  ) %>%
  rename(
    `Settlement`    = `TELEP_NEV`,
    `Roads`         = `VALUE`,
  )

# if Settlement is a Budapest district (Roman or Arabic), use "Budapest", else use the original Settlement

data <- data %>%
  mutate(
    settlement_join = if_else(
      str_detect(
        Settlement,
        "^Budapest\\s+((?:[IVXLCDM]+)|(?:0?\\d|1\\d|2[0-3]))\\.\\s*ker(?:\\.|ület)?$"
      ),
      "Budapest",
      Settlement
    )
  ) %>%
  left_join(
    public_roads,
    by = c("settlement_join" = "Settlement")
  ) %>%
  dplyr::select(-settlement_join) # remove helper column

# Join Average monthly gross earnings per district

earnings <- read_excel("Average monthly gross earnings of full-time employees (residence of the employees)_2024.xlsx", sheet = "Average monthly gross earnings")

earnings <- earnings %>%
  dplyr::select(
    `JARAS_NEV`,
    `VALUE`
  ) %>%
  rename(
    `District`             = `JARAS_NEV`,
    `Earnings`             = `VALUE`,
  )

data <- left_join(
  data,
  earnings,
  by = "District"
)

# Join Passenger cars per thousand capita at settlement level

car <- read_excel("Passenger cars per thousand capita_2024.xlsx", sheet = "Passenger cars per thousand cap")

car <- car %>%
  dplyr::select(
    `TELEP_NEV`,
    `VALUE`
  ) %>%
  rename(
    `Settlement`    = `TELEP_NEV`,
    `Cars`          = `VALUE`,
  )

data <- data %>%
  mutate(
    settlement_join = if_else(
      str_detect(
        Settlement,
        "^Budapest\\s+((?:[IVXLCDM]+)|(?:0?\\d|1\\d|2[0-3]))\\.\\s*ker(?:\\.|ület)?$"
      ),
      "Budapest",
      Settlement
    )
  ) %>%
  left_join(
    car,
    by = c("settlement_join" = "Settlement")
  ) %>%
  dplyr::select(-settlement_join) # remove helper column

##### For the company dummy
# Trim extra spaces and uppercases to help matching
data <- data %>%
  mutate(company_1=str_squish(toupper(Company)))

#Frequency of the variable Company - extract the biggest companies
table <- table(data$Company)
table_df <- as.data.frame(table)
table_df <- table_df[order(-table), ]
table_df

# Define 'big' as including 3 biggest companies
big <- c("MOL", "SHELL","OMV")

# big_companies = 1 if company ∈ big_companies; 0 otherwise
data <- data %>%
  mutate(big_companies = if_else(company_1 %in% big, 1L, 0L))

#Dummy variable for the capital city
data <- data %>%
  mutate(Status_capital = if_else(Status == "fővárosi kerület", 1, 0))

#Dummy variable for the cities
data <- data %>%
  mutate(Status_city = if_else(Status == "város", 1, 0))

#Dummy variable for the villages
data <- data %>%
  mutate(Status_village = if_else(Status == "község" | Status == "nagyközség", 1, 0))

#Dummy variable for the main cities
data <- data %>%
  mutate(Status_maincities = if_else(Status == "megyei jogú város" | Status == "megyeszékhely, megyei jogú város", 1, 0))

# Number of observations in each group
print(table(data$big_companies))
# 0 = other companies (459 obs); 1 = big companies (734)

#linear regression for gasoline
model_gasoline <- lm(avg_petrol ~ Earnings + big_companies ,data = data)
summary(model_gasoline)

#linear regression for diesel
model_diesel <- lm(avg_diesel ~ Earnings + big_companies ,data = data)
summary(model_diesel)

#stargazer
library(stargazer)
stargazer(model_gasoline, model_diesel, type = "text")

#linear regression for gasoline
model_gasoline_2 <- lm(avg_petrol ~ log(Earnings) + big_companies ,data = data)
summary(model_gasoline_2)

#linear regression for diesel
model_diesel_2 <- lm(avg_diesel ~ log(Earnings) + big_companies ,data = data)
summary(model_diesel_2)

#linear regression for gasoline
model_gasoline_3 <- lm(avg_petrol ~ Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_gasoline_3)

#linear regression for diesel
model_diesel_3 <- lm(avg_diesel ~ Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_diesel_3)

#linear regression for gasoline
model_gasoline_4 <- lm(avg_petrol ~ log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_gasoline_4)

#linear regression for diesel
model_diesel_4 <- lm(avg_diesel ~ log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_diesel_4)

#linear regression for gasoline
model_gasoline_5 <- lm(log(avg_petrol) ~ log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_gasoline_5)

#linear regression for diesel
model_diesel_5 <- lm(log(avg_diesel) ~ log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_diesel_5)

#linear regression for gasoline
model_gasoline_6 <- lm(log(avg_petrol) ~ Resident + log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_gasoline_6)

#linear regression for diesel
model_diesel_6 <- lm(log(avg_diesel) ~ Resident + log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_diesel_6)

#linear regression for gasoline
model_gasoline_7 <- lm(log(avg_petrol) ~ log(Resident) + log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_gasoline_7)

#linear regression for diesel
model_diesel_7 <- lm(log(avg_diesel) ~ log(Resident) + log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_diesel_7)

motorway <- read_excel("new2025.09.02-09.08.xlsx", sheet = "Sheet1")

motorway <- motorway %>%
  dplyr::select(
    `Address`,
    `motorway`,
    `bp_sections_plus_motorways`,
    `vignette`,
    `only_from_motorway`,
    `min_quick`,
    `min_short`,
    `km_short`,
    `km_quick`,
    `settlement_year_salary`,
  )

data <- left_join(
  data,
  motorway,
  by = "Address"
)

#linear regression for gasoline
model_gasoline_8 <- lm(log(avg_petrol) ~ motorway + Resident + log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_gasoline_8)

#linear regression for diesel
model_diesel_8 <- lm(log(avg_diesel) ~ motorway + Resident + log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_diesel_8)

#linear regression for gasoline
model_gasoline_9 <- lm(log(avg_petrol) ~ only_from_motorway + motorway + Resident + log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_gasoline_9)

#linear regression for diesel
model_diesel_9 <- lm(log(avg_diesel) ~ only_from_motorway + motorway + Resident + log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_diesel_9)

#linear regression for gasoline
model_gasoline_10 <- lm(log(avg_petrol) ~ min_quick +only_from_motorway + motorway + Resident + log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_gasoline_10)

#linear regression for diesel
model_diesel_10 <- lm(log(avg_diesel) ~ min_quick + only_from_motorway + motorway + Resident + log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_diesel_10)


#See which variables are numeric in our data
sapply(data, is.numeric)

# Correlation Matrix (Pearson)
num_vars <- data[, c("avg_petrol","avg_diesel","Resident", "Tourism_nights", "Dwellings", "Earnings", "big_companies", "motorway", "min_quick", "only_from_motorway",  "Status_capital", "Status_city", "Status_village", "Status_maincities")]

cor(num_vars, use = "complete.obs")

#linear regression for gasoline
model_gasoline_11 <- lm(log(avg_petrol) ~ km_quick +only_from_motorway + motorway + Resident + log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_gasoline_11)

#linear regression for diesel
model_diesel_11 <- lm(log(avg_diesel) ~ km_quick + only_from_motorway + motorway + Resident + log(Earnings) + Status_capital + Status_city + Status_village + Status_maincities + big_companies ,data = data)
summary(model_diesel_11)

#linear regression for gasoline
model_gasoline_12 <- lm(log(avg_petrol) ~ min_quick +only_from_motorway + motorway + Resident + big_companies ,data = data)
summary(model_gasoline_12)

#linear regression for diesel
model_diesel_12 <- lm(log(avg_diesel) ~ min_quick + only_from_motorway + motorway + Resident + big_companies ,data = data)
summary(model_diesel_12)

#linear regression for gasoline
model_gasoline_13 <- lm(log(avg_petrol) ~ min_quick + only_from_motorway + motorway + log(Earnings) + Resident + big_companies ,data = data)
summary(model_gasoline_13)

#linear regression for diesel
model_diesel_13 <- lm(log(avg_diesel) ~ min_quick + only_from_motorway + motorway + log(Earnings) + Resident + big_companies ,data = data)
summary(model_diesel_13)

#linear regression for gasoline
model_gasoline_14 <- lm(log(avg_petrol) ~ min_quick + only_from_motorway + motorway + Status_capital + Status_city + Status_village + Status_maincities + Resident + big_companies ,data = data)
summary(model_gasoline_14)

#linear regression for diesel
model_diesel_14 <- lm(log(avg_diesel) ~ min_quick + only_from_motorway + motorway +  Status_capital + Status_city + Status_village + Status_maincities + Resident + big_companies ,data = data)
summary(model_diesel_14)

# Create the stargazer diesel
stargazer(model_diesel_14, model_diesel_13, model_diesel_12,type="text")
stargazer(model_diesel_10, model_diesel_4, model_diesel_8,type="text")

# Create the stargazer gasoline
stargazer(model_gasoline_14, model_gasoline_13, model_gasoline_12,type="text")
stargazer(model_gasoline_10, model_gasoline_4, model_gasoline_8,type="text")

# Diesel AIC and BIC
AIC(model_diesel_14, model_diesel_13, model_diesel_12, model_diesel_10, model_diesel_4, model_diesel_8)
BIC(model_diesel_14, model_diesel_13, model_diesel_12, model_diesel_10, model_diesel_4, model_diesel_8)

# Gasoline AIC and BIC
AIC(model_gasoline_14, model_gasoline_13, model_gasoline_12, model_gasoline_10, model_gasoline_4, model_gasoline_8)
BIC(model_gasoline_14, model_gasoline_13, model_gasoline_12, model_gasoline_10, model_gasoline_4, model_gasoline_8)
