library(dplyr)

file_path <- file.choose()   
apr_raw <- read.csv(file_path, stringsAsFactors = FALSE)

numeric_cols <- c(
  "VLOW_INCOME_DR", "VLOW_INCOME_NDR",
  "LOW_INCOME_DR",  "LOW_INCOME_NDR",
  "MOD_INCOME_DR",  "MOD_INCOME_NDR",
  "ABOVE_MOD_INCOME",
  "TOT_PROPOSEED_UNITS", "TOT_APPROVED_UNITS", "TOT_DISAPPROVED_UNITS"
)

numeric_cols <- numeric_cols[numeric_cols %in% names(apr_raw)]

apr_clean <- apr_raw %>%
  mutate(
    
    across(all_of(numeric_cols),
           ~ as.numeric(ifelse(. == "" | is.na(.), 0, .))),
    
    # Clean SB35 flags
    APP_SUBMITTED_SB35 = ifelse(
      is.na(APP_SUBMITTED_SB35) | APP_SUBMITTED_SB35 == "",
      "No", APP_SUBMITTED_SB35
    ),
    
    # Clean density bonus flags
    DENSITY_BONUS_RECEIVED = ifelse(
      is.na(DENSITY_BONUS_RECEIVED) | DENSITY_BONUS_RECEIVED == "",
      "No", DENSITY_BONUS_RECEIVED
    ),
    
    YEAR = as.integer(YEAR)
  ) %>%
  filter(!is.na(JURIS_NAME), !is.na(YEAR))   # remove missing city/year rows

# Keep only city records (drop counties)
apr_cities <- apr_clean %>%
  filter(!grepl("COUNTY", JURIS_NAME, ignore.case = TRUE))

city_year <- apr_cities %>%
  group_by(JURIS_NAME, YEAR) %>%
  summarise(
    TotalApprovedUnits = sum(TOT_APPROVED_UNITS, na.rm = TRUE),
    
    VeryLowUnits = sum(VLOW_INCOME_DR + VLOW_INCOME_NDR, na.rm = TRUE),
    LowUnits     = sum(LOW_INCOME_DR  + LOW_INCOME_NDR,  na.rm = TRUE),
    ModUnits     = sum(MOD_INCOME_DR  + MOD_INCOME_NDR,  na.rm = TRUE),
    
    AboveModUnits = sum(ABOVE_MOD_INCOME, na.rm = TRUE),
    
    SB35Count = sum(APP_SUBMITTED_SB35 == "Yes", na.rm = TRUE),
    DensityBonusCount = sum(DENSITY_BONUS_RECEIVED == "Yes", na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    TotalAffordableUnits = VeryLowUnits + LowUnits + ModUnits,
    IncomeTotal = TotalAffordableUnits + AboveModUnits,
    LowIncomeShare = ifelse(
      IncomeTotal > 0,
      TotalAffordableUnits / IncomeTotal,
      NA_real_
    )
  )

city_year <- city_year %>%
  arrange(JURIS_NAME, YEAR) %>%
  group_by(JURIS_NAME) %>%
  mutate(
    PriorApprovedUnits = lag(TotalApprovedUnits)
  ) %>%
  ungroup() %>%
  filter(!is.na(PriorApprovedUnits))   # drop first-year rows

write.csv(city_year, "city_year_clean.csv", row.names = FALSE)

cat("Cleaning complete! File saved as city_year_clean.csv\n")

