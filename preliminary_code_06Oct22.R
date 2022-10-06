library(tidyverse)
source("scripts/col_id.R")
source("scripts/coerce_cols.R")

# import red-legged earth mite data
# add species, source and year columns, convert resistance column to binary
RLEM_data <- read_csv("data/RLEM resistance surveillance database.csv") %>%
  mutate(species = "Halotydeus destructor",
         Resistant = case_when(Resistant == "Y" ~ 1,
                               Resistant == "N" ~ 0),
         source = "RLEM database",
         year = as.numeric(str_split(`Date collected`, "/", 3, simplify = T)[,3]))

# import resistance dataset
res_data <- read_csv("data/resistance_data/resistance.csv")

# import column comparison and filter out irrelevant columns/datasets
res_cols <- read_csv("data/Resistance_database_columns.csv") %>%
  drop_na(`Database (Roozbeh)`) %>%
  rename(RLEM = `RLEM (Aston)`) %>%
  select(Columns, RLEM) %>%
  mutate(RLEM = str_extract(string = RLEM, pattern = "(?<=\\().*(?=\\))")) # find column names in RLEM dataset

res_cols[19,1] <- "source"


# rename RLEM columns to corresponding names in resistance dataset
RLEM_data <- as_tibble(col_id(res_data, RLEM_data, res_cols))
# if any columns in second dataset have different class to first dataset, coerce to same
RLEM_data <- as_tibble(coerce_cols(res_data, RLEM_data))

# combine the two datasets
joined_data <- res_data %>%
  select(names(res_data))

