# a pertussis burden such as incidence, mortality and disability-adjusted life years rate
# will be compared across all 5 SDI groups

# ---- Load Library ----
library(dplyr)
library(rstatix)

# ---- write a function to compute a descriptive analysis ---- 
summarise_burden <- function(data, group_var, rate_var = burden_rate) {
   data %>%
      group_by({{ group_var }}) %>%
      summarise(
         n = n(),
         mean = mean({{ rate_var }}, na.rm = TRUE),
         median = median({{ rate_var }}, na.rm = TRUE),
         sd = sd({{ rate_var }}, na.rm = TRUE),
         min = min({{ rate_var }}, na.rm = TRUE),
         max = max({{ rate_var }}, na.rm = TRUE),
         q1 = quantile({{ rate_var }}, 0.25, na.rm = TRUE),
         q3 = quantile({{ rate_var }}, 0.75, na.rm = TRUE),
         iqr = IQR({{ rate_var }}, na.rm = TRUE)
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2)))
}

# ---- Assumption check on the data ----
# Normality test by Shapiro Wilk Test
# the function in R can handle maximum 5000 datapoints
# Data sample is needed

# Mortality
death_df %>%
   group_by(sdi_cat) %>%
   summarise( 
      p_value = {
         sample_data <- sample(burden_rate, size = min(5000, n()))
         shapiro.test(sample_data)$p.value})
hist(death_df$burden_rate)

# Incidence
incidence_df %>%
   group_by(sdi_cat) %>%
   summarise( 
      p_value = {
         sample_data <- sample(burden_rate, size = min(5000, n()))
         shapiro.test(sample_data)$p.value})
hist(incidence_df$burden_rate)

# Disability-adjusted Life Years
dalys_df %>%
   group_by(sdi_cat) %>%
   summarise( 
      p_value = {
         sample_data <- sample(burden_rate, size = min(5000, n()))
         shapiro.test(sample_data)$p.value})
hist(dalys_df$burden_rate)

# p-value from shapiro-wilk test of all SDI groups in each indicator is less than 0.05
# histogram plot indicated that data distribution is right-skewed
# suggesting that data is not normally distributed


# ---- Comparison of burden indicator across SDI groups ----
# We have to use non-parametric Kruskal-Wallis test
# in following with Duun Post-hoc test with False Discorvery Rate (fdr) adjustment

# Mortality
summarise_burden(death_df, sdi_cat)
kruskal.test(burden_rate ~ sdi_cat, data = death_df) # p < 0.05 confirming group difference
dunn_test(data = death_df, burden_rate ~ sdi_cat, p.adjust.method = "BH")
boxplot(burden_rate ~ sdi_cat, data = death_df)
        
# Incidence
summarise_burden(incidence_df, sdi_cat)
kruskal.test(burden_rate ~ sdi_cat, data = incidence_df) # p < 0.05 
dunn_test(data = death_df, burden_rate ~ sdi_cat, p.adjust.method = "BH")
boxplot(burden_rate ~ sdi_cat, data = incidence_df)

# Dalys
summarise_burden(dalys_df, sdi_cat)
kruskal.test(burden_rate ~ sdi_cat, data = dalys_df) # p < 0.05 
dunn_test(data = death_df, burden_rate ~ sdi_cat, p.adjust.method = "BH")
boxplot(burden_rate ~ sdi_cat, data = dalys_df)

