# ---- Fitting a Linear regression model to find relationship between pertussis burden and SDI ----

# ---- Mortality ----
# fit a model
lm_death <- lm(burden_rate ~ sdi_value, data = death_df)
# summarize a model, read model parameters ex. coefficient, R square, p-value ...
summary(lm_death)
# simple scatter plot of the model
plot(death_df$sdi_value, death_df$burden_rate)
abline(lm_death, col = "red", lwd = 2)


# ---- Incidence ----
lm_incidence <- lm(burden_rate ~ sdi_value, data = incidence_df)
summary(lm_incidence)

plot(incidence_df$sdi_value, incidence_df$burden_rate)
abline(lm_incidence, col = "red", lwd = 2)


# ---- Disability-adjusted Life Years
lm_dalys <- lm(burden_rate ~ sdi_value, data = dalys_df)
summary(lm_dalys)

plot(dalys_df$sdi_value, dalys_df$burden_rate)
abline(lm_dalys, col = "red", lwd = 2)
