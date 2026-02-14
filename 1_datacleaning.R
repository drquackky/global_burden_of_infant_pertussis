# Load Library
library(dplyr)

# ---- Load and Merge data ----

# read the dataset about the burden rate of pertussis in infant under 1 
# all countries and territories from GBD data
df <- read.csv ("dataset/gbd_allregion_burden.csv")

# read the dataset of Socio-demographic index (SDI) of each countries for each year from 1990-2023
sdi_value <- read.csv("dataset/sdi_value.csv")


# merging two dataset on key variables "location_id" and "year"
df <- df %>% left_join(
   sdi_value, by = c("location_id" = "location_id",
                     "year" = "year_id")) %>%
   
   # rename the variable for easier data manipulation
   rename(sdi_value = mean_value,
          burden_rate = val) %>%
   
   # select only variables needed for analysis
   select(-c(1, 5:11, 13, 15:16)) %>%
   
   # create a category of SDI according to the file ...
   mutate (
      sdi_cat = case_when(
         sdi_value < 0.531563818 ~ "low",
         sdi_value >= 0.531563818 & sdi_value < 0.630484502 ~ "low_middle",
         sdi_value >= 0.630484502 & sdi_value < 0.676662495 ~ "middle",
         sdi_value >= 0.676662495 & sdi_value < 0.718826599 ~ "high_middle",
         sdi_value >= 0.718826599 & sdi_value <= 1 ~ "high"
      )) %>%
   
   # encoding SDI categories as a factor with specified levels
   mutate(
      sdi_cat = factor(
         sdi_cat,
         levels = c("low", "low_middle", "middle", "high_middle", "high")))


# ---- Creating a dataframe object for each burden indicator ----
# pertussis in infant under 1 year of age rate expressed per 100.000 population

death_df <- df %>%
   filter(measure_name == "Deaths",
          metric_name == "Rate")

incidence_df <- df %>%
   filter(measure_name == "Incidence",
          metric_name == "Rate")

dalys_df <- df %>%
   filter(measure_name == "DALYs (Disability-Adjusted Life Years)",
          metric_name == "Rate")