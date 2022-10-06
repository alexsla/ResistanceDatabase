library(tidyverse)

# import red-legged earth mite data
# add species, source and year columns, convert resistance column to binary
RLEM_data <- read_csv("data/RLEM resistance surveillance database.csv") %>%
  mutate(SPECIES = "Halotydeus destructor",
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

# create index of which RLEM columns are need to be renamed
rlem_ind <- which(names(RLEM_data) %in% res_cols$RLEM)
# create index of which rows contain "new" column names
res_ind <- which(res_cols$RLEM %in% names(RLEM_data))

# rename RLEM columns to corresponding names in resistance dataset
names(RLEM_data)[rlem_ind] <- res_cols[res_ind,] %>%
  arrange(RLEM, names(RLEM_data)[rlem_ind]) %>%
  select(Columns) %>%
  as_vector()

# combine the two datasets
joined_data <- res_data %>%
  bind_rows(RLEM_data %>% mutate(long = as.numeric(long),
                                 lat = as.numeric(lat),
                                 resistance = as.numeric(resistance))) %>%
  select(names(res_data))

