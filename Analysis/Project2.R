## STEP 1: Load and clean APR Table A (cities only)

## If you don't have these installed yet, run once:
## install.packages("dplyr")

library(dplyr)

# 1. Choose the CSV file (RStudio will open a file dialog)
file_path <- file.choose()   # <- you click on your tablea.csv file

# 2. Read the CSV
apr_raw <- read.csv(file_path, stringsAsFactors = FALSE)

# Optional: quick look at the raw data
head(apr_raw)
str(apr_raw)

# 3. Define numeric columns where NA should be treated as 0
numeric_cols <- c(
  "VLOW_INCOME_DR", "VLOW_INCOME_NDR",
  "LOW_INCOME_DR",  "LOW_INCOME_NDR",
  "MOD_INCOME_DR",  "MOD_INCOME_NDR",
  "ABOVE_MOD_INCOME",
  "TOT_PROPOSED_UNITS", "TOT_APPROVED_UNITS", "TOT_DISAPPROVED_UNITS"
)

# Make sure these columns exist (just in case)
numeric_cols <- numeric_cols[numeric_cols %in% names(apr_raw)]

# 4. Basic cleaning:
#    - replace NA in numeric unit columns with 0
#    - replace missing SB35 / density bonus flags with "No"
#    - drop rows with missing city or year
apr_clean <- apr_raw %>%
  mutate(
    # numeric NAs -> 0
    across(all_of(numeric_cols), ~ ifelse(is.na(.), 0, .)),
    
    # SB35 and density bonus: missing -> "No"
    APP_SUBMITTED_SB35 = ifelse(
      is.na(APP_SUBMITTED_SB35) | APP_SUBMITTED_SB35 == "",
      "No",
      APP_SUBMITTED_SB35
    ),
    DENSITY_BONUS_RECEIVED = ifelse(
      is.na(DENSITY_BONUS_RECEIVED) | DENSITY_BONUS_RECEIVED == "",
      "No",
      DENSITY_BONUS_RECEIVED
    )
  ) %>%
  # drop rows with missing city name or year
  filter(!is.na(JURIS_NAME), !is.na(YEAR))

# 5. Keep cities only (drop counties)
apr_cities <- apr_clean %>%
  filter(!grepl("COUNTY", JURIS_NAME, ignore.case = TRUE))

# 6. Inspect the cleaned city-level application data
nrow(apr_raw)      # original number of rows
nrow(apr_cities)   # rows left after keeping only cities
head(apr_cities)
str(apr_cities)

#################################

library(dplyr)

# Columns that should be numeric
numeric_cols <- c(
  "VLOW_INCOME_DR", "VLOW_INCOME_NDR",
  "LOW_INCOME_DR",  "LOW_INCOME_NDR",
  "MOD_INCOME_DR",  "MOD_INCOME_NDR",
  "ABOVE_MOD_INCOME",
  "TOT_PROPOSED_UNITS", "TOT_APPROVED_UNITS", "TOT_DISAPPROVED_UNITS"
)

# Convert character -> numeric, treating "" as 0
apr_cities <- apr_cities %>%
  mutate(
    across(
      all_of(numeric_cols),
      ~ as.numeric(ifelse(. == "" | is.na(.), 0, .))
    ),
    YEAR = as.integer(YEAR)  # make YEAR numeric (useful later)
  )

# Check structure again
str(apr_cities)


############# Step 2 #######################################################
############################################################################
############# Step 2 (updated) ###############################################
library(dplyr)

# 1. Aggregate application-level data to city-year level
city_year <- apr_cities %>%
  group_by(JURIS_NAME, YEAR) %>%
  summarise(
    # total approved units
    TotalApprovedUnits = sum(TOT_APPROVED_UNITS, na.rm = TRUE),
    
    # affordable units: very low + low + moderate (DR + NDR)
    VeryLowUnits = sum(VLOW_INCOME_DR + VLOW_INCOME_NDR, na.rm = TRUE),
    LowUnits     = sum(LOW_INCOME_DR  + LOW_INCOME_NDR,  na.rm = TRUE),
    ModUnits     = sum(MOD_INCOME_DR  + MOD_INCOME_NDR,  na.rm = TRUE),
    
    # above-moderate units
    AboveModUnits = sum(ABOVE_MOD_INCOME, na.rm = TRUE),
    
    # SB 35 and density bonus counts
    SB35Count = sum(APP_SUBMITTED_SB35 == "Yes", na.rm = TRUE),
    DensityBonusCount = sum(DENSITY_BONUS_RECEIVED == "Yes", na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  # 2. Compute total affordable units and *bounded* share
  mutate(
    TotalAffordableUnits = VeryLowUnits + LowUnits + ModUnits,
    IncomeTotal = TotalAffordableUnits + AboveModUnits,
    LowIncomeShare = ifelse(
      IncomeTotal > 0,
      TotalAffordableUnits / IncomeTotal,
      NA_real_
    )
  )

# 3. Add PriorApprovedUnits (lag by 1 year within each city)
city_year <- city_year %>%
  arrange(JURIS_NAME, YEAR) %>%
  group_by(JURIS_NAME) %>%
  mutate(
    PriorApprovedUnits = dplyr::lag(TotalApprovedUnits, n = 1)
  ) %>%
  ungroup()

# 4. Drop rows where PriorApprovedUnits is NA (first year for each city)
city_year <- city_year %>%
  filter(!is.na(PriorApprovedUnits))

# 5. Check result
nrow(city_year)
head(city_year)
str(city_year)

# Extra: check LowIncomeShare range
summary(city_year$LowIncomeShare)


############# Step 3A: Baseline regression ###############################
library(dplyr)

# Make sure Year is treated as categorical in the model
city_year_reg <- city_year %>%
  mutate(
    YearFactor = factor(YEAR)   # categorical year variable
  )

# Baseline model: no city fixed effects yet
m1 <- lm(
  TotalApprovedUnits ~ PriorApprovedUnits + LowIncomeShare +
    SB35Count + DensityBonusCount + YearFactor,
  data = city_year_reg
)

# See results
summary(m1)


############# Step 3B ######################## ###############################


install.packages("fixest")   # run once
library(fixest)

#######

city_year_reg <- city_year_reg %>%
  mutate(
    City = factor(JURIS_NAME)   # city ID for fixed effects + clustering
  )

#######

############# Step 3B: Fixed-effects model ###############################

# FE model: city fixed effects, year as factor, cluster by city
m2 <- feols(
  TotalApprovedUnits ~ PriorApprovedUnits + LowIncomeShare +
    SB35Count + DensityBonusCount + YearFactor | City,
  data = city_year_reg,
  cluster = ~ City
)

summary(m2)



############## Check for SB35 ###########

summary(city_year$SB35Count)
table(city_year$SB35Count)



#################### fixed #################

library(dplyr)
library(fixest)

# Prepare data for regression
city_year_reg <- city_year %>%
  mutate(
    YearFactor = factor(YEAR),
    City = factor(JURIS_NAME)
  )

# Baseline regression (without SB35Count)
m1 <- lm(
  TotalApprovedUnits ~ PriorApprovedUnits + LowIncomeShare +
    DensityBonusCount + YearFactor,
  data = city_year_reg
)
summary(m1)

# Fixed-effects regression (your main model)
m2 <- feols(
  TotalApprovedUnits ~ PriorApprovedUnits + LowIncomeShare +
    DensityBonusCount + YearFactor | City,
  data = city_year_reg,
  cluster = ~ City
)
summary(m2)

######################### Step 5 #######################################

library(fixest)

# Joint significance of all year dummies
wald(m2, "YearFactor2020 = 0, YearFactor2021 = 0, YearFactor2022 = 0, YearFactor2023 = 0, YearFactor2024 = 0")

######################### Step 6 #######################################


etable(m1, m2)


etable(m2)



library(fixest)

# Baseline model as a fixest model (no fixed effects, just OLS)
m1_fe <- feols(
  TotalApprovedUnits ~ PriorApprovedUnits + LowIncomeShare +
    DensityBonusCount + YearFactor,
  data = city_year_reg
)

# Fixed-effects model (same as before)
m2 <- feols(
  TotalApprovedUnits ~ PriorApprovedUnits + LowIncomeShare +
    DensityBonusCount + YearFactor | City,
  data = city_year_reg,
  cluster = ~ City
)

# Now this works:
etable(m1_fe, m2)

######################### Visuals ##############################
################################################################
################################################################


install.packages("ggplot2")  # if not installed yet
library(ggplot2)



ggplot(city_year, aes(x = TotalApprovedUnits)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of Total Approved Units per City-Year",
    x = "Total Approved Units",
    y = "Number of City-Years"
  )



ggplot(city_year, aes(x = PriorApprovedUnits, y = TotalApprovedUnits)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Prior and Current Year Approvals",
    x = "Prior Approved Units (Previous Year)",
    y = "Current Year Approved Units"
  )



ggplot(city_year, aes(x = LowIncomeShare, y = TotalApprovedUnits)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Total Approvals vs. Share of Affordable Units",
    x = "LowIncomeShare (Share of Affordable Units)",
    y = "Total Approved Units"
  )



avg_by_year <- city_year %>%
  group_by(YEAR) %>%
  summarise(
    MeanApproved = mean(TotalApprovedUnits, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(avg_by_year, aes(x = YEAR, y = MeanApproved)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Approved Units per City-Year Over Time",
    x = "Year",
    y = "Average Total Approved Units"
  )



ggplot(city_year, aes(x = TotalApprovedUnits)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of Total Approved Units per City-Year",
    x = "Total Approved Units",
    y = "Number of City-Years"
  )

# Save last plot:
ggsave("total_approved_hist.png", width = 7, height = 5, dpi = 300)




library(dplyr)
library(ggplot2)

# Create summary data by year
avg_by_year <- city_year %>%
  group_by(YEAR) %>%
  summarise(
    MeanApproved = mean(TotalApprovedUnits, na.rm = TRUE),
    .groups = "drop"
  )

# Check the summary table
print(avg_by_year)

# Plot average approvals by year
ggplot(avg_by_year, aes(x = YEAR, y = MeanApproved)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Approved Units per City-Year Over Time",
    x = "Year",
    y = "Average Total Approved Units"
  )


install.packages("fixest")


library(fixest)
etable(m1_fe, m2)



######## figures 

install.packages("ggplot2")
library(ggplot2)


# Create the plot
p1 <- ggplot(city_year, aes(x = TotalApprovedUnits)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribution of Total Approved Units per City-Year",
       x = "Total Approved Units",
       y = "Number of City-Years")

# Save the plot
ggsave(
  filename = "/Users/spartan/Documents/Math 261A/Project 2/Paper/total_approved_hist.png",
  plot = p1,
  width = 8,
  height = 5,
  dpi = 300
)


#######


# Create the plot
p2 <- ggplot(city_year, aes(x = PriorApprovedUnits, y = TotalApprovedUnits)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relationship Between Prior and Current Year Approvals",
       x = "Prior Approved Units (Previous Year)",
       y = "Current Year Approved Units")

# Save the plot
ggsave(
  filename = "/Users/spartan/Documents/Math 261A/Project 2/Paper/prior_vs_current_approvals.png",
  plot = p2,
  width = 8,
  height = 5,
  dpi = 300
)

#######

# Create the plot
p3 <- ggplot(city_year, aes(x = LowIncomeShare, y = TotalApprovedUnits)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Total Approvals vs. Share of Affordable Units",
       x = "LowIncomeShare",
       y = "Total Approved Units")

# Save the plot
ggsave(
  filename = "/Users/spartan/Documents/Math 261A/Project 2/Paper/lowincome_share_vs_total.png",
  plot = p3,
  width = 8,
  height = 5,
  dpi = 300
)


########

install.packages("patchwork")
library(patchwork)


#######

library(ggplot2)

# Plot 1: Histogram
p1 <- ggplot(city_year, aes(x = TotalApprovedUnits)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribution of Total Approved Units per City-Year",
       x = "Total Approved Units",
       y = "Number of City-Years")

# Plot 2: Prior vs Current Approvals
p2 <- ggplot(city_year, aes(x = PriorApprovedUnits, y = TotalApprovedUnits)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Prior vs Current Year Approvals",
       x = "Prior Approved Units",
       y = "Current Year Approved Units")

# Plot 3: LowIncomeShare vs TotalApprovedUnits
p3 <- ggplot(city_year, aes(x = LowIncomeShare, y = TotalApprovedUnits)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Affordable Share vs Total Approvals",
       x = "LowIncomeShare",
       y = "Total Approved Units")
combined_plot <- p1 / p2 / p3

ggsave(
  filename = "/Users/spartan/Documents/Math 261A/Project 2/Paper/approvals_3panel.png",
  plot = combined_plot,
  width = 8,
  height = 12,
  dpi = 300
)


#####

library(dplyr)
library(ggplot2)

# Compute average approved units by year
avg_by_year <- city_year %>%
  group_by(YEAR) %>%
  summarise(
    MeanApproved = mean(TotalApprovedUnits, na.rm = TRUE),
    .groups = "drop"
  )

# Create the plot
p_year <- ggplot(avg_by_year, aes(x = YEAR, y = MeanApproved)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Approved Units per City-Year Over Time",
    x = "Year",
    y = "Average Total Approved Units"
  )

ggsave(
  filename = "/Users/spartan/Documents/Math 261A/Project 2/Paper/avg_approved_units_over_time.png",
  plot = p_year,
  width = 8,
  height = 5,
  dpi = 300
)


############

## Plot 4: Average approvals over time
avg_by_year <- city_year %>%
  group_by(YEAR) %>%
  summarise(
    MeanApproved = mean(TotalApprovedUnits, na.rm = TRUE),
    .groups = "drop"
  )

p4 <- ggplot(avg_by_year, aes(x = YEAR, y = MeanApproved)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Approved Units per City-Year Over Time",
    x = "Year",
    y = "Average Total Approved Units"
  )


############ Combine 

# 2x2 panel: (p1 p2)
#            (p3 p4)
four_panel <- (p1 | p2) /
  (p3 | p4)

ggsave(
  filename = "/Users/spartan/Documents/Math 261A/Project 2/Paper/four_panel.png",
  plot = four_panel,
  width = 12,
  height = 8,
  dpi = 300
)


############# 3 combine graph

combined_data_fig <- p1 / p2 / p3


ggsave(
  "/Users/spartan/Documents/Math 261A/Project 2/Paper/data_three_panel_small.png",
  combined_data_fig,
  width = 6,   # smaller width
  height = 6,  # smaller height
  dpi = 300
)


ggsave("/Users/spartan/Documents/Math 261A/Project 2/Paper/total_small.png", p1, width = 5, height = 3, dpi = 300)
ggsave("/Users/spartan/Documents/Math 261A/Project 2/Paper/prior_small.png", p2, width = 4, height = 3, dpi = 300)
ggsave("/Users/spartan/Documents/Math 261A/Project 2/Paper/lowincome_small.png", p3, width = 4, height = 3, dpi = 300)


getwd()
list.files()


library(fixest)
etable(m1_fe, m2)

etable(m1_fe, m2, file = "regression_results_table.csv")

write.csv(city_year, "city_year_clean.csv", row.names = FALSE)


