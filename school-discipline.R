install.packages("psych")

library(formattable)
library(tidyverse)
library(psych)
library(scales)

# Import Department of Education data

corporal_punishment <- read_csv("data/Corporal_Punishment.csv")

enrollment <- read_csv("data/Enrollment.csv")

expulsions <- read_csv("data/Expulsions.csv")

referrals_arrests <- read_csv("data/Referrals_and_Arrests.csv")

restraint_seclusion <- read_csv("data/Restraint_and_Seclusion.csv")

suspensions <- read_csv("data/Suspensions.csv")

free_reduced_lunch <- read_csv("data/free_reduced_lunch.csv",
                               skip = 6,
                               n_max = 100740,
                               na = c("†", "–", "‡"),
                               col_types = cols(.default = "c",
                                                `Total Students All Grades (Excludes AE) [Public School] 2017-18` = "n",
                                                `Direct Certification [Public School] 2017-18` = "n",
                                                `Free Lunch Eligible [Public School] 2017-18` = "n",
                                                `Reduced-price Lunch Eligible Students [Public School] 2017-18` = "n",
                                                `Free and Reduced Lunch Students [Public School] 2017-18` = "n")) %>% 
  rename("LEA_STATE_NAME" = 3,
         "SCH_NAME" = 4,
         "COMBOKEY" = 5,
         "LEA_NAME" = 6,
         "LEAID" = 7,
         "ENROLLMENT_TOTAL_NCES" = 8,
         "FREE_LUNCH" = 9,
         "DIRECT_CERT_LUNCH" = 10,
         "REDUCED_PRICE_LUNCH" = 11,
         "FREE_AND_REDUCED_PRICE_LUNCH" = 12) %>% 
  mutate(across(where(is.character), ~str_replace_all(.x, '^="|"$', "")),
         across(where(is.numeric), formattable::comma, digits = 0)) %>% 
  select(3, 7, 6, 4, 5, 8:12)

# Join the data frames
discipline <- list(enrollment, corporal_punishment, expulsions, referrals_arrests,
                   restraint_seclusion, suspensions, free_reduced_lunch) %>% 
  reduce(full_join, by = "COMBOKEY") %>% 
  # Drop duplicate columns
  select(c(-contains(".y") & -contains(".x.x") & -contains(".x.x.x") & -matches("NAME$|ID$"))) %>% 
  # Rename columns
  rename_with(~str_replace(.x, ".x$", "")) %>% 
  # Convert reserved values to 0 (I MAY WANT TO CHANGE THIS TO "NA" LATER)
  mutate(across(where(is.numeric), ~ifelse(. %in% c(-3, -5, -6, -8, -9, -11), 0, .)))

# Test DC schools
discipline %>% 
  filter(LEA_STATE_NAME == "DISTRICT OF COLUMBIA") %>% 
  View()

# Calculate discipline by school
discipline_by_school <- discipline %>% 
  mutate(
    # Create direct certification flag
    DIRECT_CERT_FLAG = case_when(
      !is.na(DIRECT_CERT_LUNCH) & is.na(FREE_AND_REDUCED_PRICE_LUNCH) ~ TRUE,
      TRUE ~ FALSE),
    # Set the free and reduced lunch column equal to direct certification where necessary
    FREE_AND_REDUCED_PRICE_LUNCH = case_when(
      !is.na(FREE_AND_REDUCED_PRICE_LUNCH)  ~ FREE_AND_REDUCED_PRICE_LUNCH,
      is.na(FREE_AND_REDUCED_PRICE_LUNCH) ~ DIRECT_CERT_LUNCH),
    # Calculate free or reduced lunch as a percent of total enrollment
    PCT_DIRECT_CERT_LUNCH = DIRECT_CERT_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_FREE_LUNCH = FREE_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_REDUCED_PRICE_LUNCH = REDUCED_PRICE_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_FREE_AND_REDUCED_PRICE_LUNCH = case_when(
      !is.na(FREE_AND_REDUCED_PRICE_LUNCH)  ~ FREE_AND_REDUCED_PRICE_LUNCH / ENROLLMENT_TOTAL_NCES,
      is.na(FREE_AND_REDUCED_PRICE_LUNCH) ~ DIRECT_CERT_LUNCH / ENROLLMENT_TOTAL_NCES),
    # Total enrollment including all races and all disability statuses (not disabled + IDEA + 504)
    ENROLLMENT_TOTAL = rowSums(across(contains("TOT_ENR_"))),
    # Total referrals including all races and all disability statuses (not disabled + IDEA + 504)
    REFERRALS_TOTAL = rowSums(across(contains("TOT_DISCWODIS_REF_") | contains("TOT_DISCWDIS_REF_IDEA_") | contains("SCH_DISCWDIS_REF_504_"))),
    # Total disabled referrals (IDEA + 504) regardless of race
    REFERRALS_DISABLED = rowSums(across(contains("TOT_DISCWDIS_REF_IDEA_") | contains("SCH_DISCWDIS_REF_504_"))),
    # Referrals by race excluding 504 students (not disabled + IDEA)
    REFERRALS_WHITE = rowSums(across(contains("REF_WH_") | contains("REF_IDEA_WH_"))),
    REFERRALS_HISPANIC = rowSums(across(contains("REF_HI_") | contains("REF_IDEA_HI_"))),
    REFERRALS_BLACK = rowSums(across(contains("REF_BL_") | contains("REF_IDEA_BL_"))),
    REFERRALS_ASIAN = rowSums(across(contains("REF_AS_") | contains("REF_IDEA_AS_"))),
    REFERRALS_NATIVE = rowSums(across(contains("REF_AM_") | contains("REF_IDEA_AM_"))),
    REFERRALS_PACIFIC = rowSums(across(contains("REF_HP_") | contains("REF_IDEA_HP_"))),
    REFERRALS_TWO_OR_MORE = rowSums(across(contains("REF_TR_") | contains("REF_IDEA_TR_"))),
    # Total disabled referrals (IDEA + 504) regardless of race as a percent of total referrals
    REFERRALS_DISABLED_PCT = REFERRALS_DISABLED / REFERRALS_TOTAL,
    # Referrals by race excluding 504 students (not disabled + IDEA) as a percent of total referrals
    REFERRALS_WHITE_PCT = REFERRALS_WHITE / REFERRALS_TOTAL,
    REFERRALS_HISPANIC_PCT = REFERRALS_HISPANIC / REFERRALS_TOTAL,
    REFERRALS_BLACK_PCT = REFERRALS_BLACK / REFERRALS_TOTAL,
    REFERRALS_ASIAN_PCT = REFERRALS_ASIAN / REFERRALS_TOTAL,
    REFERRALS_NATIVE_PCT = REFERRALS_NATIVE / REFERRALS_TOTAL,
    REFERRALS_PACIFIC_PCT = REFERRALS_PACIFIC / REFERRALS_TOTAL,
    REFERRALS_TWO_OR_MORE_PCT = REFERRALS_TWO_OR_MORE / REFERRALS_TOTAL,
    # Total disabled enrollment (IDEA + 504) regardless of race
    ENROLLMENT_DISABLED = rowSums(across(contains("TOT_IDEAENR_") | contains("TOT_504ENR_"))),
    # Total enrollment by race excluding 504 students (not disabled + IDEA) NOTE: THESE WILL DIFFER SLIGHTLY FROM THE COUNTS ON CRDC WEBSITE!
    ENROLLMENT_WHITE = rowSums(across(contains("SCH_ENR_WH_"))) - rowSums(across(contains("SCH_504ENR_WH_"))),
    ENROLLMENT_HISPANIC = rowSums(across(contains("SCH_ENR_HI_"))) - rowSums(across(contains("SCH_504ENR_HI_"))),
    ENROLLMENT_BLACK = rowSums(across(contains("SCH_ENR_BL_"))) - rowSums(across(contains("SCH_504ENR_BL_"))),
    ENROLLMENT_ASIAN = rowSums(across(contains("SCH_ENR_AS_"))) - rowSums(across(contains("SCH_504ENR_AS_"))),
    ENROLLMENT_NATIVE = rowSums(across(contains("SCH_ENR_AM_"))) - rowSums(across(contains("SCH_504ENR_AM_"))),
    ENROLLMENT_PACIFIC = rowSums(across(contains("SCH_ENR_HP_"))) - rowSums(across(contains("SCH_504ENR_HP_"))),
    ENROLLMENT_TWO_OR_MORE = rowSums(across(contains("SCH_ENR_TR_"))) - rowSums(across(contains("SCH_504ENR_TR_"))),
    # Calculate total refferals per thousand
    REFERRALS_TOTAL_PER_THOUSAND = REFERRALS_TOTAL / ENROLLMENT_TOTAL * 1000,
    # Calculate disabled referrals per thousand
    REFERRALS_DISABLED_PER_THOUSAND = REFERRALS_DISABLED / ENROLLMENT_DISABLED * 1000,
    # Calculate referrals by race per thousand
    REFERRALS_WHITE_PER_THOUSAND = REFERRALS_WHITE / ENROLLMENT_WHITE * 1000,
    REFERRALS_HISPANIC_PER_THOUSAND = REFERRALS_HISPANIC / ENROLLMENT_HISPANIC * 1000,
    REFERRALS_BLACK_PER_THOUSAND = REFERRALS_BLACK / ENROLLMENT_BLACK * 1000,
    REFERRALS_ASIAN_PER_THOUSAND = REFERRALS_ASIAN / ENROLLMENT_ASIAN * 1000,
    REFERRALS_NATIVE_PER_THOUSAND = REFERRALS_NATIVE / ENROLLMENT_NATIVE * 1000,
    REFERRALS_PACIFIC_PER_THOUSAND = REFERRALS_PACIFIC / ENROLLMENT_PACIFIC * 1000,
    REFERRALS_TWO_OR_MORE_PER_THOUSAND = REFERRALS_TWO_OR_MORE / ENROLLMENT_TWO_OR_MORE * 1000,
    # Calculate the disparity between total refferals per thousand and each category
    PCT_DIFF_REFERRALS_DISABLED_PER_THOUSAND = (REFERRALS_DISABLED_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_WHITE_PER_THOUSAND = (REFERRALS_WHITE_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_HISPANIC_PER_THOUSAND = (REFERRALS_HISPANIC_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_BLACK_PER_THOUSAND = (REFERRALS_BLACK_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_ASIAN_PER_THOUSAND = (REFERRALS_ASIAN_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_NATIVE_PER_THOUSAND = (REFERRALS_NATIVE_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_PACIFIC_PER_THOUSAND = (REFERRALS_PACIFIC_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_TWO_OR_MORE_PER_THOUSAND = (REFERRALS_TWO_OR_MORE_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    # Calculate enrollment per thousand
    across(starts_with("ENROLLMENT") & -ENROLLMENT_TOTAL, ~ . / ENROLLMENT_TOTAL * 1000, .names = "{.col}_PER_THOUSAND"),
    # Format the columns
    across(where(is.numeric), formattable::comma, digits = 0),
    across(-starts_with("PCT") & ends_with("PER_THOUSAND"), ~ formattable::digits(.x, 1)),
    across(contains("PCT"), ~ formattable::percent(.x, 1))) %>% 
  # Reorder the columns
  select(1:8, 706, 701, 703, 702, 704, 705, 707:710,
         starts_with("REFERRALS_") & ends_with("_PER_THOUSAND"),
         starts_with("REFERRALS_") & ends_with("_PCT"),
         starts_with("ENROLLMENT_") & ends_with("_PER_THOUSAND"),
         starts_with("PCT_"),
         starts_with("REFERRALS_") & -ends_with("_PER_THOUSAND"),
         starts_with("ENROLLMENT_") & -ends_with("_PER_THOUSAND"))

# Test how many schools have free and reduced lunch data
discipline_by_school %>% 
  filter(!is.na(FREE_AND_REDUCED_PRICE_LUNCH)) %>% 
  summarize(num = n())

# Test how many schools have more than 100% of students qualified for free and reduced price lunch (should presumably be zero))
discipline_by_school %>% 
  filter(PCT_FREE_AND_REDUCED_PRICE_LUNCH > 1) %>% 
  summarize(num = n())

# Test on Del Valle HS
View(discipline_by_school %>% 
       filter(COMBOKEY == "481662001424"))

# Calculate discipline nationally
discipline_nationally <- discipline_by_school %>% 
  summarize(across(c(-starts_with("PCT_") & ends_with("_LUNCH"),
                     starts_with("REFERRALS_") & -ends_with("_PER_THOUSAND"),
                     starts_with("ENROLLMENT_") & -ends_with("_PER_THOUSAND")), sum, na.rm = TRUE)) %>% 
  mutate(
    # Calculate free or reduced lunch as a percent of total enrollment
    PCT_DIRECT_CERT_LUNCH = DIRECT_CERT_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_FREE_LUNCH = FREE_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_REDUCED_PRICE_LUNCH = REDUCED_PRICE_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_FREE_AND_REDUCED_PRICE_LUNCH = case_when(
      !is.na(FREE_AND_REDUCED_PRICE_LUNCH)  ~ FREE_AND_REDUCED_PRICE_LUNCH / ENROLLMENT_TOTAL_NCES,
      is.na(FREE_AND_REDUCED_PRICE_LUNCH) ~ DIRECT_CERT_LUNCH / ENROLLMENT_TOTAL_NCES),
    # Calculate total refferals per thousand
    REFERRALS_TOTAL_PER_THOUSAND = REFERRALS_TOTAL / ENROLLMENT_TOTAL * 1000,
    # Calculate disabled referrals per thousand
    REFERRALS_DISABLED_PER_THOUSAND = REFERRALS_DISABLED / ENROLLMENT_DISABLED * 1000,
    # Calculate referrals by race per thousand
    REFERRALS_WHITE_PER_THOUSAND = REFERRALS_WHITE / ENROLLMENT_WHITE * 1000,
    REFERRALS_HISPANIC_PER_THOUSAND = REFERRALS_HISPANIC / ENROLLMENT_HISPANIC * 1000,
    REFERRALS_BLACK_PER_THOUSAND = REFERRALS_BLACK / ENROLLMENT_BLACK * 1000,
    REFERRALS_ASIAN_PER_THOUSAND = REFERRALS_ASIAN / ENROLLMENT_ASIAN * 1000,
    REFERRALS_NATIVE_PER_THOUSAND = REFERRALS_NATIVE / ENROLLMENT_NATIVE * 1000,
    REFERRALS_PACIFIC_PER_THOUSAND = REFERRALS_PACIFIC / ENROLLMENT_PACIFIC * 1000,
    REFERRALS_TWO_OR_MORE_PER_THOUSAND = REFERRALS_TWO_OR_MORE / ENROLLMENT_TWO_OR_MORE * 1000,
    # Total disabled referrals (IDEA + 504) regardless of race as a percent of total referrals
    REFERRALS_DISABLED_PCT = REFERRALS_DISABLED / REFERRALS_TOTAL,
    # Referrals by race excluding 504 students (not disabled + IDEA) as a percent of total referrals
    REFERRALS_WHITE_PCT = REFERRALS_WHITE / REFERRALS_TOTAL,
    REFERRALS_HISPANIC_PCT = REFERRALS_HISPANIC / REFERRALS_TOTAL,
    REFERRALS_BLACK_PCT = REFERRALS_BLACK / REFERRALS_TOTAL,
    REFERRALS_ASIAN_PCT = REFERRALS_ASIAN / REFERRALS_TOTAL,
    REFERRALS_NATIVE_PCT = REFERRALS_NATIVE / REFERRALS_TOTAL,
    REFERRALS_PACIFIC_PCT = REFERRALS_PACIFIC / REFERRALS_TOTAL,
    REFERRALS_TWO_OR_MORE_PCT = REFERRALS_TWO_OR_MORE / REFERRALS_TOTAL,
    # Calculate the disparity between total refferals per thousand and each category
    PCT_DIFF_REFERRALS_DISABLED_PER_THOUSAND = (REFERRALS_DISABLED_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_WHITE_PER_THOUSAND = (REFERRALS_WHITE_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_HISPANIC_PER_THOUSAND = (REFERRALS_HISPANIC_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_BLACK_PER_THOUSAND = (REFERRALS_BLACK_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_ASIAN_PER_THOUSAND = (REFERRALS_ASIAN_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_NATIVE_PER_THOUSAND = (REFERRALS_NATIVE_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_PACIFIC_PER_THOUSAND = (REFERRALS_PACIFIC_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_TWO_OR_MORE_PER_THOUSAND = (REFERRALS_TWO_OR_MORE_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    # Calculate enrollment per thousand
    across(starts_with("ENROLLMENT") & -ENROLLMENT_TOTAL, ~ . / ENROLLMENT_TOTAL * 1000, .names = "{.col}_PER_THOUSAND"),
    # Format the columns
    across(where(is.numeric), formattable::comma, digits = 0),
    across(-starts_with("PCT") & ends_with("PER_THOUSAND"), ~ formattable::digits(.x, 1)),
    across(contains("PCT"), ~ formattable::percent(.x, 1))) %>% 
  # Reorder the columns
  select(starts_with("REFERRALS_") & ends_with("_PER_THOUSAND"),
         starts_with("REFERRALS_") & ends_with("_PCT"),
         starts_with("ENROLLMENT_") & ends_with("_PER_THOUSAND"),
         starts_with("PCT_") & -ends_with("_LUNCH"),
         starts_with("REFERRALS_") & -ends_with("_PER_THOUSAND"),
         starts_with("ENROLLMENT_") & -ends_with("_PER_THOUSAND"),
         ends_with("_LUNCH"))

# Calculate discipline by state
discipline_by_state <- discipline_by_school %>% 
  group_by(LEA_STATE_NAME) %>% 
  summarize(across(c(-starts_with("PCT_") & ends_with("_LUNCH"),
                     starts_with("REFERRALS_") & -ends_with("_PER_THOUSAND"),
                     starts_with("ENROLLMENT_") & -ends_with("_PER_THOUSAND")), sum, na.rm = TRUE)) %>% 
  mutate(
    # Calculate free or reduced lunch as a percent of total enrollment
    PCT_DIRECT_CERT_LUNCH = DIRECT_CERT_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_FREE_LUNCH = FREE_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_REDUCED_PRICE_LUNCH = REDUCED_PRICE_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_FREE_AND_REDUCED_PRICE_LUNCH = case_when(
      !is.na(FREE_AND_REDUCED_PRICE_LUNCH)  ~ FREE_AND_REDUCED_PRICE_LUNCH / ENROLLMENT_TOTAL_NCES,
      is.na(FREE_AND_REDUCED_PRICE_LUNCH) ~ DIRECT_CERT_LUNCH / ENROLLMENT_TOTAL_NCES),
    # Calculate total refferals per thousand
    REFERRALS_TOTAL_PER_THOUSAND = REFERRALS_TOTAL / ENROLLMENT_TOTAL * 1000,
    # Calculate disabled referrals per thousand
    REFERRALS_DISABLED_PER_THOUSAND = REFERRALS_DISABLED / ENROLLMENT_DISABLED * 1000,
    # Calculate referrals by race per thousand
    REFERRALS_WHITE_PER_THOUSAND = REFERRALS_WHITE / ENROLLMENT_WHITE * 1000,
    REFERRALS_HISPANIC_PER_THOUSAND = REFERRALS_HISPANIC / ENROLLMENT_HISPANIC * 1000,
    REFERRALS_BLACK_PER_THOUSAND = REFERRALS_BLACK / ENROLLMENT_BLACK * 1000,
    REFERRALS_ASIAN_PER_THOUSAND = REFERRALS_ASIAN / ENROLLMENT_ASIAN * 1000,
    REFERRALS_NATIVE_PER_THOUSAND = REFERRALS_NATIVE / ENROLLMENT_NATIVE * 1000,
    REFERRALS_PACIFIC_PER_THOUSAND = REFERRALS_PACIFIC / ENROLLMENT_PACIFIC * 1000,
    REFERRALS_TWO_OR_MORE_PER_THOUSAND = REFERRALS_TWO_OR_MORE / ENROLLMENT_TWO_OR_MORE * 1000,
    # Total disabled referrals (IDEA + 504) regardless of race as a percent of total referrals
    REFERRALS_DISABLED_PCT = REFERRALS_DISABLED / REFERRALS_TOTAL,
    # Referrals by race excluding 504 students (not disabled + IDEA) as a percent of total referrals
    REFERRALS_WHITE_PCT = REFERRALS_WHITE / REFERRALS_TOTAL,
    REFERRALS_HISPANIC_PCT = REFERRALS_HISPANIC / REFERRALS_TOTAL,
    REFERRALS_BLACK_PCT = REFERRALS_BLACK / REFERRALS_TOTAL,
    REFERRALS_ASIAN_PCT = REFERRALS_ASIAN / REFERRALS_TOTAL,
    REFERRALS_NATIVE_PCT = REFERRALS_NATIVE / REFERRALS_TOTAL,
    REFERRALS_PACIFIC_PCT = REFERRALS_PACIFIC / REFERRALS_TOTAL,
    REFERRALS_TWO_OR_MORE_PCT = REFERRALS_TWO_OR_MORE / REFERRALS_TOTAL,
    # Calculate the disparity between total refferals per thousand and each category
    PCT_DIFF_REFERRALS_DISABLED_PER_THOUSAND = (REFERRALS_DISABLED_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_WHITE_PER_THOUSAND = (REFERRALS_WHITE_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_HISPANIC_PER_THOUSAND = (REFERRALS_HISPANIC_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_BLACK_PER_THOUSAND = (REFERRALS_BLACK_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_ASIAN_PER_THOUSAND = (REFERRALS_ASIAN_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_NATIVE_PER_THOUSAND = (REFERRALS_NATIVE_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_PACIFIC_PER_THOUSAND = (REFERRALS_PACIFIC_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_TWO_OR_MORE_PER_THOUSAND = (REFERRALS_TWO_OR_MORE_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    # Calculate enrollment per thousand
    across(starts_with("ENROLLMENT_") & -ENROLLMENT_TOTAL, ~ . / ENROLLMENT_TOTAL * 1000, .names = "{.col}_PER_THOUSAND"),
    # Format the columns
    across(where(is.numeric), formattable::comma, digits = 0),
    across(-starts_with("PCT") & ends_with("PER_THOUSAND"), ~ formattable::digits(.x, 1)),
    across(contains("PCT"), ~ formattable::percent(.x, 1))) %>% 
  # Reorder the columns
  select(LEA_STATE_NAME,
         starts_with("REFERRALS_") & ends_with("_PER_THOUSAND"),
         starts_with("REFERRALS_") & ends_with("_PCT"),
         starts_with("ENROLLMENT_") & ends_with("_PER_THOUSAND"),
         starts_with("PCT_") & -ends_with("_LUNCH"),
         starts_with("REFERRALS_") & -ends_with("_PER_THOUSAND"),
         starts_with("ENROLLMENT_") & -ends_with("_PER_THOUSAND"),
         ends_with("_LUNCH"))

# Test a state that uses direct certification
discipline_by_state %>% 
  filter(LEA_STATE_NAME == "DISTRICT OF COLUMBIA") %>% 
  View()

# Calculate discipline by district
discipline_by_district <- discipline_by_school %>% 
  group_by(LEAID) %>% 
  summarize(across(c(-starts_with("PCT_") & ends_with("_LUNCH"),
                     starts_with("REFERRALS_") & -ends_with("_PER_THOUSAND"),
                     starts_with("ENROLLMENT_") & -ends_with("_PER_THOUSAND")), sum, na.rm = TRUE)) %>% 
  mutate(
    # Calculate free or reduced lunch as a percent of total enrollment
    PCT_DIRECT_CERT_LUNCH = DIRECT_CERT_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_FREE_LUNCH = FREE_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_REDUCED_PRICE_LUNCH = REDUCED_PRICE_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_FREE_AND_REDUCED_PRICE_LUNCH = case_when(
      !is.na(FREE_AND_REDUCED_PRICE_LUNCH)  ~ FREE_AND_REDUCED_PRICE_LUNCH / ENROLLMENT_TOTAL_NCES,
      is.na(FREE_AND_REDUCED_PRICE_LUNCH) ~ DIRECT_CERT_LUNCH / ENROLLMENT_TOTAL_NCES),
    # Calculate total refferals per thousand
    REFERRALS_TOTAL_PER_THOUSAND = REFERRALS_TOTAL / ENROLLMENT_TOTAL * 1000,
    # Calculate disabled referrals per thousand
    REFERRALS_DISABLED_PER_THOUSAND = REFERRALS_DISABLED / ENROLLMENT_DISABLED * 1000,
    # Calculate referrals by race per thousand
    REFERRALS_WHITE_PER_THOUSAND = REFERRALS_WHITE / ENROLLMENT_WHITE * 1000,
    REFERRALS_HISPANIC_PER_THOUSAND = REFERRALS_HISPANIC / ENROLLMENT_HISPANIC * 1000,
    REFERRALS_BLACK_PER_THOUSAND = REFERRALS_BLACK / ENROLLMENT_BLACK * 1000,
    REFERRALS_ASIAN_PER_THOUSAND = REFERRALS_ASIAN / ENROLLMENT_ASIAN * 1000,
    REFERRALS_NATIVE_PER_THOUSAND = REFERRALS_NATIVE / ENROLLMENT_NATIVE * 1000,
    REFERRALS_PACIFIC_PER_THOUSAND = REFERRALS_PACIFIC / ENROLLMENT_PACIFIC * 1000,
    REFERRALS_TWO_OR_MORE_PER_THOUSAND = REFERRALS_TWO_OR_MORE / ENROLLMENT_TWO_OR_MORE * 1000,
    # Total disabled referrals (IDEA + 504) regardless of race as a percent of total referrals
    REFERRALS_DISABLED_PCT = REFERRALS_DISABLED / REFERRALS_TOTAL,
    # Referrals by race excluding 504 students (not disabled + IDEA) as a percent of total referrals
    REFERRALS_WHITE_PCT = REFERRALS_WHITE / REFERRALS_TOTAL,
    REFERRALS_HISPANIC_PCT = REFERRALS_HISPANIC / REFERRALS_TOTAL,
    REFERRALS_BLACK_PCT = REFERRALS_BLACK / REFERRALS_TOTAL,
    REFERRALS_ASIAN_PCT = REFERRALS_ASIAN / REFERRALS_TOTAL,
    REFERRALS_NATIVE_PCT = REFERRALS_NATIVE / REFERRALS_TOTAL,
    REFERRALS_PACIFIC_PCT = REFERRALS_PACIFIC / REFERRALS_TOTAL,
    REFERRALS_TWO_OR_MORE_PCT = REFERRALS_TWO_OR_MORE / REFERRALS_TOTAL,
    # Calculate the disparity between total refferals per thousand and each category
    PCT_DIFF_REFERRALS_DISABLED_PER_THOUSAND = (REFERRALS_DISABLED_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_WHITE_PER_THOUSAND = (REFERRALS_WHITE_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_HISPANIC_PER_THOUSAND = (REFERRALS_HISPANIC_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_BLACK_PER_THOUSAND = (REFERRALS_BLACK_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_ASIAN_PER_THOUSAND = (REFERRALS_ASIAN_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_NATIVE_PER_THOUSAND = (REFERRALS_NATIVE_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_PACIFIC_PER_THOUSAND = (REFERRALS_PACIFIC_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_TWO_OR_MORE_PER_THOUSAND = (REFERRALS_TWO_OR_MORE_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    # Calculate enrollment per thousand
    across(starts_with("ENROLLMENT_") & -ENROLLMENT_TOTAL, ~ . / ENROLLMENT_TOTAL * 1000, .names = "{.col}_PER_THOUSAND"),
    # Format the columns
    across(where(is.numeric), formattable::comma, digits = 0),
    across(-starts_with("PCT") & ends_with("PER_THOUSAND"), ~ formattable::digits(.x, 1)),
    across(contains("PCT"), ~ formattable::percent(.x, 1))) %>% 
  # Add state and district names
  left_join(discipline_by_school %>% 
              select(LEAID,
                     LEA_STATE_NAME,
                     LEA_NAME),
            by = "LEAID") %>%
  # Remove duplicates
  distinct(LEAID, .keep_all = TRUE) %>% 
  # Reorder the columns
  select(LEAID,
         LEA_STATE_NAME,
         LEA_NAME,
         starts_with("REFERRALS_") & ends_with("_PER_THOUSAND"),
         starts_with("REFERRALS_") & ends_with("_PCT"),
         starts_with("ENROLLMENT_") & ends_with("_PER_THOUSAND"),
         starts_with("PCT_") & -ends_with("_LUNCH"),
         starts_with("REFERRALS_") & -ends_with("_PER_THOUSAND"),
         starts_with("ENROLLMENT_") & -ends_with("_PER_THOUSAND"),
         ends_with("_LUNCH"))

# Test Boston, MA
discipline_by_district %>% 
  filter(LEAID == "2502790") %>% 
  View()

### ANALYSIS

# Correlations

# Remove INF values prior to running the correlation analysis
discipline_by_district_inf_removed <- discipline_by_district %>% 
  mutate(across(everything(), ~na_if(., Inf))) %>% 
  select(1:3, 64, 4:12)

# What's the correlation between free and reduced lunch and referrals to law enforcement?
cor(discipline_by_district_inf_removed[4:13],
    use = "pairwise.complete.obs") %>% 
  View()

# Plot this
pairs(discipline_by_district_inf_removed[4:13],
      use = "pairwise.complete.obs") %>% 
  View()

# Regressions
lm(formula = 
     REFERRALS_TOTAL_PER_THOUSAND ~ PCT_FREE_AND_REDUCED_PRICE_LUNCH,
   data = discipline_by_district_inf_removed)

discipline_by_district_inf_removed_model <- lm(formula = REFERRALS_TOTAL_PER_THOUSAND ~ PCT_FREE_AND_REDUCED_PRICE_LUNCH, data = discipline_by_district_inf_removed)

summary.data.frame(discipline_by_district_inf_removed_model)

# What's the picture nationally?
View(discipline_nationally)

# Which states disproportionately refer disabled students?
count(discipline_by_state %>% 
        filter(REFERRALS_DISABLED_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND))

# Which states disproportionately refer Black students?
count(discipline_by_state %>% 
        filter(REFERRALS_BLACK_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND))

# Which states disproportionately refer Native American students?
count(discipline_by_state %>% 
        filter(REFERRALS_NATIVE_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND))

# Which states disproportionately refer Hispanic students?
count(discipline_by_state %>% 
        filter(REFERRALS_HISPANIC_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND))

# Which states disproportionately refer White students?
count(discipline_by_state %>% 
        filter(REFERRALS_WHITE_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND))

# What proportion of schools refer a disproportionate number of disabled students to law enforcement?
count(discipline_by_school %>% 
        filter(ENROLLMENT_DISABLED >= 10,
               REFERRALS_DISABLED_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND)) /
  count(discipline_by_school %>% 
          filter(ENROLLMENT_DISABLED >= 10))

# What proportion of schools refer a disproportionate number of Black students to law enforcement?
count(discipline_by_school %>% 
        filter(ENROLLMENT_BLACK >= 10,
               REFERRALS_BLACK_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND)) /
  count(discipline_by_school %>% 
          filter(ENROLLMENT_BLACK >= 10))

# What proportion of schools refer a disproportionate number of Native American students to law enforcement?
count(discipline_by_school %>% 
        filter(ENROLLMENT_NATIVE >= 10,
               REFERRALS_NATIVE_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND)) /
  count(discipline_by_school %>% 
          filter(ENROLLMENT_NATIVE >= 10))

# Check out those high-profile assault schools.
high_profile_assaults <- c("120147007417",
                           "450339000975",
                           "210299000659",
                           "270729000292",
                           "370472003276",
                           "240009001693",
                           "120156001620",
                           "470369001528")

View(discipline_by_school %>% 
       filter(COMBOKEY %in% high_profile_assaults))

# Export the data
write_csv(discipline_by_school,
          "data/exported/discipline_by_school.csv")

write_csv(discipline_nationally,
          "data/exported/discipline_nationally.csv")

write_csv(discipline_by_state,
          "data/exported/discipline_by_state.csv")

write_csv(discipline_by_district,
          "data/exported/discipline_by_district.csv")
