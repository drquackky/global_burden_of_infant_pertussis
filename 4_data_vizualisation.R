# load library
library(ggplot2)
library(ggstatsplot) # for visualizing the box plot

# ---- Group mean rank comparison across SDI groups - box plot ----
# The same code applied to mortality and Dalys data frame
ggbetweenstats(
   data = incidence_df,
   x = sdi_cat,
   y = burden_rate,
   plot.type = "box", type = "np",
   pairwise.display = "ns", p.adjust.method = "fdr",
   ylab = "Under 1-year Incidence rate per 100.000",
   xlab = "Socio Demographic Index Category",
   notch = TRUE) +
   theme_classic() +
   theme(legend.position = "top") +
   
   # add the mean and its confidence interval
   stat_summary(
      fun.data = mean_cl_boot,
      geom = "errorbar",
      width = 0.1,
      color = 'blue'
   ) +
   stat_summary(
      fun = mean,
      geom = "point",
      shape = 18,
      size = 3,
      color = "blue"
   ) +
   ggtitle("Pertussis Incidence Rate Comparision Across SDI category") +
   theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

ggsave(
   filename = "figures/incidence_pairwise_comparison.jpg",
   plot = last_plot(),
   width = 8,
   height = 6,
   dpi = 300
)



# ---- Trend visualization of pertussis burden ----
# The same code applied to mortality and Dalys data frame
trenddf <- read.csv(file = "dataset/gbd_trend_allsdi.csv")
incidence_trenddf <- trenddf %>% 
   filter(measure_name == "Incidence") %>%
   mutate(location_name = factor(location_name,
                             levels = c(
                                "Global",
                                "High SDI",
                                "High-middle SDI",
                                "Middle SDI",
                                "Low-middle SDI",
                                "Low SDI"
                             )))

# set up color code for each SDI groups
sdi_colors <- c(
   "Global" = "#ff8c00",
   "High SDI" = "#483d8b",
   "High-middle SDI" = "#8b008b",
   "Middle SDI" = "#808080",
   "Low-middle SDI" = "#ffd707",
   "Low SDI" = "#586d32"
)

# data vizualisation using ggplot2
ggplot(incidence_trenddf, aes(x = year, group = location_name)) +
   geom_line(aes(y = val, color = location_name), linewidth = 1) +
   geom_line(aes(y = lower, color = location_name),
             linetype = "dotted", alpha = 0.7, linewidth = 0.8) +
   geom_line(aes(y = upper, color = location_name),
             linetype = "dotted", alpha = 0.7, linewidth = 0.8) +
   scale_color_manual(values = sdi_colors) +
   scale_x_continuous(
      breaks = seq(min(incidence_trenddf$year),
                   max(incidence_trenddf$year),
                   by = 1)) +
   scale_y_continuous(n.breaks = 10 ) +
   labs(
      title = "Trend in Pertussis Incidence Among Infants Under 1 Year of Age (1990–2023)",
      y = "Under-1 infant incidence rate per 100.000 "
   ) +
   theme_minimal() +
   theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.title = element_blank(),
      legend.position = "top",
      plot.title = element_text(
         hjust = 0.5,
         face = "bold"
      )
   ) 

ggsave(
   filename = "figures/incidence_trend_1990_2023.jpg",
   plot = last_plot(),
   width = 10,
   height = 6
)



# ---- Scatter plot for Linear regression visualization ----
# The same code applied to mortality and Dalys data frame
ggplot(incidence_df, aes(x = sdi_value, y = burden_rate)) +
   geom_point(color = "darkblue", alpha = 0.2) +
   geom_smooth(method = "lm", se = TRUE, color = "red") +
   labs(
      title = "Linear Association Between SDI and Pertussis under-1 Incidence Rate",
      x = NULL,
      y = "Age-standardized rate (per 100,000)")+
   theme_minimal() +
   theme(
      axis.title.y = element_text(face = "bold"),
      plot.title = element_text (hjust = 0.5, face = "bold"))

ggsave(
   filename = "figures/relationship_between_sdi_and_burden.jpg",
   width = 8,
   height = 6
)

