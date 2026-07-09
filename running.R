
# Clear workspace and load libraries
rm(list = ls())

# Libraries and packages
library(tidyverse)
library(lubridate)
library(hms)
library(mgcv)
library(googlesheets4)
library(googledrive)
library(zoo)
library(modelsummary)

# Authentication and Drive info (for workflow run, comment out if running on my computer)
gs4_auth(path = "google_auth.json")
drive_auth(path = "google_auth.json")

# # Authentication and Drive info (for local run, comment out if running the workflow)
# gs4_auth()
# drive_auth()

# The ID of the Google Drive folder
target_folder <- as_id("1SS550vx5XmxcQI5byIZ_SRVLbKm-z98F")

# A function to handle outliers in both directions for a variable
transform_num <- function(x) {
  result <- rep(NA_real_, length(x))
  result[which(x == 0)] <- 0
  result[which(x > 0)] <- log1p(x[which(x > 0)])
  result[which(x < 0)] <- -log1p(-x[which(x < 0)])
  return(result)
}

# gg theme
theme_set(theme_minimal())

# Read in data
running <- read_sheet("https://docs.google.com/spreadsheets/d/1O_r313XFN5TJK8Edr80UpJcLwDXKNep2gI0Ga8LmnHE/edit?usp=sharing",
                      skip = 2)

# Clean data a bit and create new variables
running <- running %>%
  mutate(date = as.numeric(ymd(date)),
         start_time = as.numeric(as_hms(start_time)) / 3600,
         afternoon = if_else(start_time > 12.5, 1, 0),
         dist_fac = cut(dist, breaks = c(0, 2.5, 7.5, 15, 25, Inf),
                        labels = c("0-2.5", "2.5-7.5", "7.5-15", "15-25", "25+")),
         dist_log = log1p(dist),
         duration = ymd_hms(duration) - ymd_hms("1899-12-30 00:00:00"),
         pace = as.numeric(ymd_hms(pace) - ymd_hms("1899-12-30 00:00:00")),
         type = case_when(
           type %in% "Race" ~ "Race Effort",
           type %in% c("Fast", "Hill", "Interval") ~ "Hard Effort",
           type %in% c("Base", "Tempo", "Three-One") ~ "Medium Effort",
           type %in% c("Marathon Race Pace", "Easy") ~ "Easy Effort",
           TRUE ~ type
         ) %>% factor(levels = c("Easy Effort", "Medium Effort", "Hard Effort", "Race Effort")),
         race = fct_other(type, keep = "Race Effort", other_level = "Non-Race") %>%
           fct_relevel("Non-Race"),
         surface = factor(surface, levels = c("Road", "Treadmill", "Track"))) %>%
  # Now create a variable for shoe mileage, after first making sure the data is
  # in order by date (though that should already be the case)
  arrange(as.Date(date)) %>%
  group_by(shoe) %>%
  mutate(shoe_dist = cumsum(dist)) %>%
  ungroup() %>%
  relocate(date, start_time, afternoon, dist, dist_log, dist_fac, duration, pace,
           gain, change, hills, net_change, inj_ill, sleep, vigorous, peak,
           type, race, surface, elevation, temp, dew, sun, weather, shoe, sh_type,
           notes)

# Model
my_mod <- gam(pace ~ s(date, k = 15) + # change over time, one of main predictors
                s(dist, k = 4) + # distance effect, other main predictor
                weather + sun + te(temp, dew, k = 4) + # run conditions
                type + s(hills, k = 4) + surface + afternoon + # run characteristics
                # NOTE: Add s(net_change) if I get enough data to have it be accurate
                inj_ill + sleep, # personal and equipment conditions
              data = running,
              method = "REML",
              family = Gamma(link = "log"))
coef_names <- c("Intercept", "Weather (0 = Good, 0.5 = Okay, 1 = Bad)",
                "Sun (0 = Cloudy/No Sun, 0.5 = Partly Cloudy, 1 = Sunny)",
                "Medium Effort Run (vs. Easy)", "Hard Effort Run (vs. Easy)",
                "Race Effort Run (vs. Easy)", "Treadmill (vs. Road)",
                "Track (vs. Road)", "Time of Run (0/1, 1 = After Noon)",
                "Health Status (0/1, 1 = Recently Injured or Ill)",
                "Sleep Score")
# Using Gamma function because, based on the response vs. fitted values plot,
# it seems like the variance increases with the mean
# Using log link so that predictions are always positive
# Note that interpretation of coefficients is on the log scale, so interpret
# them as percent changes (e.g. 0.02 = 2% increase in pace)
# Once I have enough race data, I might want to do s(dist, by = race) as a
# term to allow for different distance effects based on whether it
# was a race

# Google Drive files to be able to address the model
# Overall look at variables
datasummary_skim(running, output = "model_1skim.html")
# Coefficients Table
modelsummary(my_mod,
             output = "model_1summary.html",
             title = "Running Pace GAM Model, Gamma Function with Log Link",
             coef_rename = coef_names,
             exponentiate = TRUE,
             statistic = "conf.int",
             conf_level = 0.95)
# GAM Diagnostics on the Smooths and Overall Model
gam_diagnostics <- capture.output(gam.check(my_mod))
writeLines(gam_diagnostics, "model_2gamcheck.txt")
png("model_2gamcheck.png", width = 800, height = 800, res = 100)
par(mfrow = c(2, 2)) # Forces the 4 plots into a 2x2 grid
gam.check(my_mod)
dev.off() # Closes the canvas and saves the file
# Smooths
png("model_3smoothsA.png", width = 1200, height = 800, res = 100)
plot(my_mod, 
     pages = 1, 
     scheme = 1, 
     shade = TRUE, 
     shade.col = "lightblue",
     main = "GAM Smooth Effects") 
dev.off()
# Temperature and Dew Tensor Product Smooth
png("model_3smoothsB.png", width = 800, height = 600, res = 100)
vis.gam(my_mod, 
        view = c("temp", "dew"), 
        plot.type = "contour", # or "persp" for 3D
        color = "topo", 
        main = "GAM Surface: Temp vs Dew Point")
dev.off()
# Autocorrelation Function
png("model_4acf.png", width = 800, height = 600, res = 100)
acf(resid(my_mod), main = "ACF of Model Residuals")
dev.off()
# Save all of these to Google Drive
drive_put(media = "model_1skim.html", path = target_folder, name = "model_1skim.html")
drive_put(media = "model_1summary.html", path = target_folder, name = "model_1summary.html")
drive_put(media = "model_2gamcheck.txt", path = target_folder, name = "model_2gamcheck.txt")
drive_put(media = "model_2gamcheck.png", path = target_folder, name = "model_2gamcheck.png")
drive_put(media = "model_3smoothsA.png", path = target_folder, name = "model_3smoothsA.png")
drive_put(media = "model_3smoothsB.png", path = target_folder, name = "model_3smoothsB.png")
drive_put(media = "model_4acf.png", path = target_folder, name = "model_4acf.png")

# Just a note for one of the things put on the graph: the Boston Qualifying Time
# for my age group for 2026 is 3:00:00, which is a pace of 4:16 min/km

# Predicted Pace
# New Data
# Paces to predict
fig_width <- 9
fig_height <- 5
my_dists <- c(1.60934, 5, 10, 21.0975, 42.195)
n_dists <- length(my_dists)
my_daterange <- seq(min(running$date, na.rm = TRUE),
                    as.numeric(Sys.Date()))
n_dates <- length(my_daterange)
new_data <- tibble(
  date = rep(my_daterange, n_dists),
  dist = rep(my_dists, each = n_dates), # distances above,
  weather = 0, # not raining or bad weather
  sun = 0.5, # Partially sunny
  temp = mean(running$temp, na.rm = TRUE), # average temp
  dew = mean(running$dew, na.rm = TRUE), # average dew point
  type = factor("Medium Effort", levels = levels(running$type)), # A base run
  hills = mean(running$hills, na.rm = TRUE), # hilliness metric
  # net_change = mean(running$net_change, na.rm = TRUE), # net elevation change
  surface = factor("Road", levels = c("Road", "Treadmill", "Track")),
  afternoon = 0, # not afternoon
  inj_ill = 0, # no injury or illness
  sleep = mean(running$sleep, na.rm = TRUE) # average sleep score
)
preds <- predict(my_mod, newdata = new_data, se.fit = TRUE)
# Note: Using exponentiation below rather than type = "response" in the predict
# function because I want to get standard errors on the log scale and then
# transform those to the response scale
new_data <- new_data %>%
  mutate(date = as.Date(date),
         dist = factor(dist),
         # Step 1: Calculate limits on the Log Scale
         log_fit   = preds$fit,
         log_lower = preds$fit - 1.96 * preds$se.fit,
         log_upper = preds$fit + 1.96 * preds$se.fit,
         # Step 2: Exponentiate to get back to "Minutes per km"
         # Then multiply by 60 to get seconds for the hms() function
         pred       = lubridate::hms(hms::hms(exp(log_fit) * 60)),
         pred_lower = lubridate::hms(hms(exp(log_lower) * 60)),
         pred_upper = lubridate::hms(hms(exp(log_upper) * 60)))
shapes <- c(20, 17, 15, 6, 11)
g_out <- ggplot() +
  geom_hline(yintercept = period(minutes = 4, seconds = 16),
             linetype = "dashed") +
  geom_point(data = running %>% filter(type == "Race Effort"),
             aes(x = as.Date(date), y = hms(pace * 60)),
             size = 4, shape = 1, color = "red") +
  geom_point(data = running, aes(x = as.Date(date), y = hms(pace * 60),
                                 shape = dist_fac),
             alpha = 0.5, size = 2) +
  geom_line(data = new_data, aes(x = date, y = pred, color = dist, group = dist),
            linewidth = 1) +
  scale_shape_manual(values = shapes,
                     labels = c("<2.5k", "2.5-7.5k", "7.5-15k", "15-25k", "25k+")) +
  scale_color_discrete(labels = c("1 Mile", "5k", "10k", "Half-\nMarathon", "Marathon")) +
  labs(title = "Pace by Distance and Over Time",
       subtitle = "Base Run",
       x = "Date",
       y = "Pace, min/km\n(Dashed Line = Boston Qualifying Pace)",
       shape = "Distance,\nRecorded Run\nRed = Race",
       color = "Distance,\nPredicted Run")
g_out
ggsave("running_1.png", g_out, width = fig_width, height = fig_height, dpi = 300,
       bg = "white")
drive_put(media = "running_1.png", path = target_folder, name = "running_1.png")

# Racing predicted values
race_data <- tibble(
  date = rep(my_daterange, n_dists),
  dist = rep(my_dists, each = n_dates), # distances above,
  weather = 0, # not raining or bad weather
  sun = 0.5, # Partially sunny
  temp = mean(running$temp, na.rm = TRUE), # average temp
  dew = mean(running$dew, na.rm = TRUE), # average dew point
  type = factor("Race Effort", levels = levels(running$type)), # A race
  hills = mean(running$hills, na.rm = TRUE), # hilliness metric
  # net_change = mean(running$net_change, na.rm = TRUE), # net elevation change
  surface = factor("Road", levels = c("Road", "Treadmill", "Track")),
  afternoon = 0, # not afternoon
  inj_ill = 0, # no injury or illness
  sleep = mean(running$sleep, na.rm = TRUE) # average sleep score
)
preds_race <- predict(my_mod, newdata = race_data, se.fit = TRUE)
race_data <- race_data %>%
  mutate(date = as.Date(date),
         dist = factor(dist),
         # Step 1: Calculate limits on the Log Scale
         log_fit   = preds_race$fit,
         log_lower = preds_race$fit - 1.96 * preds_race$se.fit,
         log_upper = preds_race$fit + 1.96 * preds_race$se.fit,
         # Step 2: Exponentiate to get back to "Minutes per km"
         # Then multiply by 60 to get seconds for the hms() function
         pred       = lubridate::hms(hms::hms(exp(log_fit) * 60)),
         pred_lower = lubridate::hms(hms(exp(log_lower) * 60)),
         pred_upper = lubridate::hms(hms(exp(log_upper) * 60)))
# Racing plots
g_out_2 <- ggplot(race_data %>% filter(dist == "42.195"),
                  aes(x = date)) +
  geom_hline(yintercept = period(minutes = 4, seconds = 16),
             linewidth = 1, linetype = "dashed") +
  geom_point(data = running %>% filter(type == "Race Effort"),
             aes(x = as.Date(date), y = hms(pace * 60)),
             size = 4, shape = 1, color = "red") +
  geom_point(data = running, aes(x = as.Date(date), y = hms(pace * 60),
                                 shape = dist_fac),
             alpha = 0.5, size = 2) +
  geom_line(aes(y = pred), linewidth = 1) +
  geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), alpha = 0.1) +
  scale_shape_manual(values = shapes,
                     labels = c("<2.5k", "2.5-7.5k", "7.5-15k", "15-25k", "25k+")) +
  labs(title = "Pace by Distance and Over Time",
       subtitle = "Marathon Racing Event",
       x = "Date",
       y = "Pace, min/km\n(Dashed Line = Boston Qualifying Pace)",
       shape = "Distance,\nRecorded Run\nRed = Race",
       color = "Distance,\nPredicted Run")
g_out_2
ggsave("running_2.png", g_out_2, width = fig_width, height = fig_height, dpi = 300,
       bg = "white")
drive_put(media = "running_2.png", path = target_folder, name = "running_2.png")
g_out_3 <- ggplot() +
  geom_hline(yintercept = period(minutes = 4, seconds = 16),
             linewidth = 1, linetype = "dashed") +
  geom_point(data = running %>% filter(type == "Race Effort"),
             aes(x = as.Date(date), y = hms(pace * 60)),
             size = 4, shape = 1, color = "red") +
  geom_point(data = running, aes(x = as.Date(date), y = hms(pace * 60),
                                 shape = dist_fac),
             alpha = 0.5, size = 2) +
  geom_line(data = race_data, aes(x = date, y = pred, color = dist, group = dist),
            linewidth = 1) +
  scale_shape_manual(values = shapes,
                     labels = c("<2.5k", "2.5-7.5k", "7.5-15k", "15-25k", "25k+")) +
  scale_color_discrete(labels = c("1 Mile", "5k", "10k", "Half-\nMarathon", "Marathon")) +
  labs(title = "Pace by Distance and Over Time",
       subtitle = "Racing Event",
       x = "Date",
       y = "Pace, min/km\n(Dashed Line = Boston Qualifying Pace)",
       shape = "Distance,\nRecorded Run\nRed = Race",
       color = "Distance,\nPredicted Run")
g_out_3
ggsave("running_3.png", g_out_3, width = fig_width, height = fig_height, dpi = 300,
       bg = "white")
drive_put(media = "running_3.png", path = target_folder, name = "running_3.png")

# Make a plot of my running distance over time, but with weekly (pink) and
# monthly (blue) moving averages
# First I need to fill in the zero days
running_full <- running %>%
  mutate(date = as.Date(date)) %>%
  complete(date = as.Date(my_daterange)) %>%
  mutate(dist = if_else(is.na(dist), 0, dist),
         week_start = floor_date(date, "week", week_start = 1),
         month_start = floor_date(date, "month"),
         yr = year(date)) %>%
  group_by(week_start) %>%
  mutate(dist_weekly = sum(dist)) %>%
  ungroup() %>%
  group_by(month_start) %>%
  mutate(dist_monthly = sum(dist)) %>%
  ungroup()
running_summary <- running_full %>%
  group_by(yr) %>%
  summarize(
    weekave_peryr = sum(dist) / (n() / 7),
    monthave_peryr = sum(dist) / (n() / 30),
    yr_start = min(date),
    yr_end = max(date),
    .groups = "drop"
  )
g_out_4 <- running_full %>%
  ggplot(aes(x = date, y = dist)) +
  geom_segment(data = running_summary,
               aes(y = monthave_peryr, yend = monthave_peryr,
                   x = yr_start, xend = yr_end),
               color = "blue", linewidth = 3, alpha = 0.5) +
  geom_segment(data = running_summary,
               aes(y = monthave_peryr, yend = monthave_peryr,
                   x = yr_start, xend = yr_end),
               color = "black", linewidth = 0.25) +
  geom_segment(data = running_summary,
               aes(y = weekave_peryr, yend = weekave_peryr,
                   x = yr_start, xend = yr_end),
               color = "pink", linewidth = 3, alpha = 0.5) +
  geom_segment(data = running_summary,
               aes(y = weekave_peryr, yend = weekave_peryr,
                   x = yr_start, xend = yr_end),
               color = "black", linewidth = 0.25) +
  geom_step(aes(y = dist_monthly), color = "blue", linewidth = 1) +
  geom_step(aes(y = dist_weekly), color = "pink", linewidth = 1) +
  geom_line(linewidth = 0.25) +
  labs(title = "Running Distance",
       subtitle = "Black = Daily Totals, Pink = Weekly, Blue = Monthly, Horizontal Lines = Average Wk/Mth Totals Each Year",
       x = "Date",
       y = "Distance, km")
g_out_4
ggsave("running_4.png", g_out_4, width = fig_width, height = fig_height, dpi = 300,
       bg = "white")
drive_put(media = "running_4.png", path = target_folder, name = "running_4.png")
datasummary_df(running_summary, "running_4.html")
drive_put(media = "running_4.html", path = target_folder, name = "running_4.html")

# Make a plot showing the proportion of my time running that is spent in the
# peak heart rate zone
# The main variable of interest is peak, which is the percentage of a given run
# spent in the peak heart rate zone, from 0 to 100
# For a given period, I should take (100 - peak) and multiply it, for each run,
# by the duration of the run — then, divide that by the total duration of all
# runs in that period, to get the proportion of time not spent in the peak zone
# Note that the duration variable is already in minutes
# Note that, in contrast to the running distance graph produced above, I want
# this to be a moving calculation, so I will do it with a rolling window
# I want to do this just for a moving 30-day period
# Lastly note that I want this to be a moving 30-day period, and not all days
# are included in the data, so I will first create a full sequence of dates
running_zones <- running %>%
  mutate(date = as.Date(date)) %>%
  complete(date = as.Date(my_daterange)) %>%
  mutate(week_start = floor_date(date, "week"),
         month_start = floor_date(date, "month"),
         yr = year(date)) %>%
  arrange(date) %>%
  select(date, week_start, month_start, yr, peak, vigorous, duration) %>%
  mutate(peak = if_else(is.na(duration), 0, peak / 100),
         vigorous = if_else(is.na(duration), 0, vigorous / 100),
         light_moderate = if_else(is.na(duration), 0, 1 - peak - vigorous),
         peak_time = peak * duration,
         vigorous_time = vigorous * duration,
         light_moderate_time = light_moderate * duration,
         duration = if_else(is.na(duration), as.difftime(0, units = "mins"), duration),
         peak_time = as.numeric(peak_time),
         vigorous_time = as.numeric(vigorous_time),
         light_moderate_time = as.numeric(light_moderate_time),
         duration = as.numeric(duration)) %>%
  group_by(week_start) %>%
  mutate(prop_peak_week = sum(peak_time, na.rm = TRUE) / sum(duration, na.rm = TRUE),
         prop_vigorous_week = sum(vigorous_time, na.rm = TRUE) / sum(duration, na.rm = TRUE),
         prop_light_moderate_week = sum(light_moderate_time, na.rm = TRUE) / sum(duration, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(month_start) %>%
  mutate(prop_peak_month = sum(peak_time, na.rm = TRUE) / sum(duration, na.rm = TRUE),
         prop_vigorous_month = sum(vigorous_time, na.rm = TRUE) / sum(duration, na.rm = TRUE),
         prop_light_moderate_month = sum(light_moderate_time, na.rm = TRUE) / sum(duration, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(date) %>%
  select(date, peak, vigorous, light_moderate, starts_with("prop")) %>%
  rename(prop_peak_day = peak, prop_vigorous_day = vigorous, prop_light_moderate_day = light_moderate) %>%
  pivot_longer(cols = starts_with("prop"),
               names_to = c("zone", ".value"),
               names_pattern = "prop_(.*)_(day|week|month)") %>%
  rename(prop_daily = day, prop_weekly = week, prop_monthly = month) %>%
  # rename values in the zone variable to "Peak Zone", "Vigorous Zone", "Light/Moderate Zone"
  mutate(zone = case_when(
    zone == "peak" ~ "Peak Zone",
    zone == "vigorous" ~ "Vigorous Zone",
    zone == "light_moderate" ~ "Light/Moderate Zone"),
    zone = factor(zone, levels = c("Peak Zone", "Vigorous Zone", "Light/Moderate Zone")))
# Now make the plot
g_out_5 <- running_zones %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = prop_daily), size = 2, alpha = 0.1) +
  geom_line(aes(y = prop_weekly), color = "pink", linewidth = 1) +
  geom_line(aes(y = prop_monthly), color = "blue", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proportion of Running Time Spent in Heart Rate Zones",
       subtitle = "Points = Individual Runs, Pink = Weekly Proportions, Blue = Monthly Proportions",
       x = "Date",
       y = "Proportion") +
  facet_wrap(~zone, ncol = 1, scales = "free_y")
g_out_5
ggsave("running_5.png", g_out_5, width = fig_width, height = fig_height * 2, dpi = 300,
       bg = "white")
drive_put(media = "running_5.png", path = target_folder, name = "running_5.png")

# Histogram of distances with the ones I'm predicting marked
g_out_6 <- ggplot(running, aes(x = dist)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = my_dists, color = "red", linewidth = 1,
             linetype = "dashed") +
  labs(title = "Histogram of Running Distances",
       subtitle = "Red Dashed Lines Indicate Distances Being Predicted",
       x = "Distance, km",
       y = "Count")
g_out_6
ggsave("running_6.png", g_out_6, width = fig_width, height = fig_height, dpi = 300,
       bg = "white")
drive_put(media = "running_6.png", path = target_folder, name = "running_6.png")

# # For the Thanksgiving Half Marathon
# tibble(
#   date = as.numeric(ymd("2025-11-27")), dist = 21.0975,
#   hills = (120 + (120 - (0)))/21.0975,
#   inj_ill = 0, sleep = mean(running$sleep),
#     type = factor("Race", levels = levels(running$type)),
#   surface = factor("Road", levels = c("Road", "Treadmill", "Track")),
#   temp = 3, dew = -3, sun = 0, afternoon = 0, weather = 0
# ) %>%
#   predict(my_mod, newdata = .) %>%
#   `[`(1) %>%
#   exp() %>%
#   `*`(c("Predicted Pace" = 60, "Predicted Half-Marathon Time" = 60 * 21.0975)) %>%
#   hms::hms() %>%
#   lubridate::hms()

# # For the Frigid 5k
# tibble(
#   date = as.numeric(ymd("2026-01-24")), dist = 5,
#   hills = (4 + (4 - (0)))/5,
#   inj_ill = 0, sleep = mean(running$sleep),
#   type = factor("Race", levels = levels(running$type)),
#   surface = factor("Road", levels = c("Road", "Treadmill", "Track")),
#   temp = 0, dew = -4, sun = 0, afternoon = 0, weather = 0
# ) %>%
#   predict(my_mod, newdata = .) %>%
#   `[`(1) %>%
#   exp() %>%
#   `*`(c("Predicted Pace" = 60, "Predicted 5k Time" = 60 * 5)) %>%
#   hms::hms() %>%
#   lubridate::hms()
# 
# # For the Salt Lake City Marathon
# tibble(
#   date = as.numeric(ymd("2026-04-25")), dist = 42.194988,
#   hills = (278 + (278 - (-164)))/42.194988,
#   inj_ill = 0, sleep = mean(running$sleep),
#   type = factor("Race", levels = levels(running$type)),
#   surface = factor("Road", levels = c("Road", "Treadmill", "Track")),
#   temp = 12, dew = 5, sun = 0.5, afternoon = 0, weather = 0
# ) %>%
#   predict(my_mod, newdata = .) %>%
#   `[`(1) %>%
#   exp() %>%
#   `*`(c("Predicted Pace" = 60, "Predicted Marathon Time" = 60 * 42.194988)) %>%
#   hms::hms() %>%
#   lubridate::hms()

# # For the Utah Valley Marathon
# tibble(
#   date = as.numeric(ymd("2026-06-06")), dist = 42.194988,
#   hills = (124 + (124 - (-489.204)))/42.194988,
#   inj_ill = 0, sleep = mean(running$sleep),
#   type = factor("Race", levels = levels(running$type)),
#   surface = factor("Road", levels = c("Road", "Treadmill", "Track")),
#   temp = 15, dew = 11, sun = 0.5, afternoon = 0, weather = 0
# ) %>%
#   predict(my_mod, newdata = .) %>%
#   `[`(1) %>%
#   exp() %>%
#   `*`(c("Predicted Pace" = 60, "Predicted Marathon Time" = 60 * 42.194988)) %>%
#   hms::hms() %>%
#   lubridate::hms()

# # For the Runtastic Timp 5k
# tibble(
#   date = as.numeric(ymd("2026-06-20")), dist = 5,
#   hills = (54 + (54 - (4)))/5,
#   inj_ill = 0, sleep = mean(running$sleep),
#   type = factor("Race", levels = levels(running$type)),
#   surface = factor("Road", levels = c("Road", "Treadmill", "Track")),
#   temp = 25, dew = 3, sun = 0, afternoon = 0, weather = 0
# ) %>%
#   predict(my_mod, newdata = .) %>%
#   `[`(1) %>%
#   exp() %>%
#   `*`(c("Predicted Pace" = 60, "Predicted 5k Time" = 60 * 5)) %>%
#   hms::hms() %>%
#   lubridate::hms()

# For Antelope Island Half Marathon
my_predict <- tibble(
  date = as.numeric(ymd("2026-10-09")), dist = 21.0975,
  hills = (150 + (150 - (0)))/21.0975,
  inj_ill = 0, sleep = mean(running$sleep),
  type = factor("Race Effort", levels = levels(running$type)),
  surface = factor("Road", levels = c("Road", "Treadmill", "Track")),
  temp = 17, dew = 3, sun = 0.5, afternoon = 0, weather = 0
) %>%
  predict(my_mod, newdata = .) %>%
  `[`(1) %>%
  exp() %>%
  `*`(c("Predicted Pace" = 60, "Predicted Marathon Time" = 60 * 21.0975)) %>%
  hms::hms() %>%
  lubridate::hms()

# Total distance run
total_dist <- sum(running$dist)
# When I get to 4,880km I want to celebrate (that is the equivalent of running
# across the US)

# Total distance for each pair of shoes
shoes <- running %>%
  group_by(shoe, sh_type) %>%
  summarize(total_dist = sum(dist))
# Should change for new shoes at 500-800km, but since I am a larger guy maybe
# more like 400-600km

sink("running_7.txt")
cat("Upcoming Race: Antelope Island Half Marathon\n")
cat("Date: Oct 9, 2026\n")
print(my_predict)
cat("\n\n")
cat("Total Distance Tracker\n")
cat("Celebrate at 4,880 km and multiples of that\n")
print(total_dist)
cat("\n\n")
cat("Shoe Tracker\n")
cat("Retire daily trainers at ≥600km, Nylon plate shoes at ≥500km, and CF plate shoes at ≥400km\n")
print(as.data.frame(shoes))
cat("\n")
sink()
drive_put(media = "running_7.txt", path = target_folder, name = "running_7.txt")

