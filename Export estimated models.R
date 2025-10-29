################## For the models we include on stargazer (main Script) ######## 

# 1) Define the headers and rename the models for better understanding
gas_models <- list(
  "Gasoline (15)"  = model_gasoline_15,
  "Gasoline (14)" = model_gasoline_14,
  "Gasoline (13)" = model_gasoline_13,
  "Gasoline (12)" = model_gasoline_12,
  "Gasoline (10)" = model_gasoline_10,
  "Gasoline (4)"  = model_gasoline_4,
  "Gasoline (8)"  = model_gasoline_8
)

diesel_models <- list(
  "Diesel (15)" = model_diesel_15,
  "Diesel (14)" = model_diesel_14,
  "Diesel (13)" = model_diesel_13,
  "Diesel (12)" = model_diesel_12,
  "Diesel (10)" = model_diesel_10,
  "Diesel (4)"  = model_diesel_4,
  "Diesel (8)"  = model_diesel_8
)

# 2) Create an outputs folder inside the repository
dir.create("outputs", showWarnings = FALSE)

# 3) Write one DOCX containing BOTH tables 
msummary(
  gas_models,
  vcov = 'HC1',
  title = "Gasoline regressions",
  stars = TRUE,
  output = "outputs/regression_tables0.docx"
)
msummary(
  diesel_models,
  vcov = 'HC1',
  title = "Diesel regressions",
  stars = TRUE,
   output = "outputs/regression_tables.docx"
  )



