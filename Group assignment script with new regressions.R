############## New regressions idea ######################

rm(list = ls()) # wipe all objects from the workspace for a clean run

library(readxl)
library(dplyr)
library(stringr)
library(stargazer)
library(lmtest)     
library(estimatr)    

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

# Number of observations in each group
print(table(data$big_companies))
# 0 = other companies (459 obs); 1 = big companies (734)


#############################################
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

#############################
# Create Motorway dummy
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


######################## // ######################################

# Create a clean output folder
dir.create("outputs", showWarnings = FALSE)

# Small helper to append text lines to a file (keeps code DRY)
append_line <- function(path, ...) cat(..., "\n", file = path, append = TRUE)

############################################################
## 1) DIESEL: OLS stepwise chain and Heteroscedasticity test
##    start simple and then add motorway, Resident,
##    settlement type (Status dummies), min_quick variables.
############################################################

model1_diesel <- lm(avg_diesel ~ log(Earnings) + big_companies, data = data)
summary(model1_diesel)

model2_diesel <- lm(avg_diesel ~ log(Earnings) + big_companies + motorway, data = data)
summary(model2_diesel)

model3_diesel <- lm(avg_diesel ~ log(Earnings) + big_companies + motorway + Resident, data = data)
summary(model3_diesel)

model4_diesel <- lm(avg_diesel ~ log(Earnings) + big_companies + motorway + Resident +
                      Status_capital + Status_city + Status_village, data = data)
summary(model4_diesel)

model5_diesel <- lm(avg_diesel ~ log(Earnings) + big_companies + motorway + Resident +
                                          Status_capital + Status_city + Status_village + min_quick, data = data)
summary(model5_diesel)

bptest(model1_diesel)
bptest(model2_diesel)
bptest(model3_diesel)
bptest(model4_diesel)
bptest(model5_diesel)


diesel_models <- list(
  "D1: log(Earnings)+big_companies" = model1_diesel,
  "D2: +motorway"                    = model2_diesel,
  "D3: +Resident"                    = model3_diesel,
  "D4: +Status dummies"              = model4_diesel,
  "D5: +min_quick"                   = model5_diesel
)

############################################################
## 2) GASOLINE: OLS stepwise chain and Heteroscedasticity test
############################################################

model1_gas <- lm(avg_petrol ~ log(Earnings) + big_companies, data = data)
summary(model1_gas)

model2_gas <- lm(avg_petrol ~ log(Earnings) + big_companies + motorway, data = data)
summary(model2_gas)

model3_gas <- lm(avg_petrol ~ log(Earnings) + big_companies + motorway + Resident, data = data)
summary(model3_gas)

model4_gas <- lm(avg_petrol ~ log(Earnings) + big_companies + motorway + Resident +
                   Status_capital + Status_city + Status_village, data = data)
summary(model4_gas)


model5_gas <- lm(avg_petrol ~ log(Earnings) + big_companies + motorway + Resident +
                   Status_capital + Status_city + Status_village + min_quick, data = data)
summary(model5_gas)


bptest(model1_gas) 
bptest(model2_gas)
bptest(model3_gas)
bptest(model4_gas)
bptest(model5_gas)

gas_models <- list(
  "G1: log(Earnings)+big_companies" = model1_gas,
  "G2: +motorway"                    = model2_gas,
  "G3: +Resident"                    = model3_gas,
  "G4: +Status dummies"              = model4_gas,
  "G5: +min_quick"                   = model5_gas
)


#since only model 1 is homocedastic, we will use robust SE when
#reporting our results


# Write one DOCX containing BOTH tables with all our regressions and
# robust SE  
msummary(
  gas_models,
  vcov = 'HC1',
  title = "Gasoline regressions",
  stars = TRUE,
  output = "outputs/regression_tableGasoline.docx"
)

msummary(
  diesel_models,
  vcov = 'HC1',
  title = "Diesel regressions",
  stars = TRUE,
   output = "outputs/regression_tableDiesel.docx"
  )

########## Information Criteria AIC and BIC
AIC_BIC_table <- data.frame(
  Model = c("(1)log(Earnings)+big_companies", "(2)+motorway",
  "(3)+Resident", "(4)+Status dummies", "(5)+min_quick"),
  Diesel= AIC(model1_diesel, model2_diesel,model3_diesel, model4_diesel, model5_diesel),
  Diesel= BIC(model1_diesel, model2_diesel,model3_diesel, model4_diesel, model5_diesel),
 Gasoline=AIC(model1_gas, model2_gas,model3_gas, model4_gas, model5_gas),
 Gasoline=BIC(model1_gas, model2_gas,model3_gas, model4_gas, model5_gas))

#Drop Df
AIC_BIC_table_clean <- subset(AIC_BIC_table, select = -c(Diesel.df, Diesel.df.1, Gasoline.df, Gasoline.df.1))

#Export
datasummary_df(AIC_BIC_table_clean, output ="outputs/aic_bic_Table.docx" )
