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

school_characteristics <- read_csv("data/School_Characteristics.csv")

school_support <- read_csv("data/School_Support.csv")

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
                   restraint_seclusion, school_characteristics, school_support, free_reduced_lunch) %>% 
  reduce(full_join, by = "COMBOKEY") %>% 
  # Drop duplicate columns
  select(c(-contains(".y") & -contains(".x.x") & -contains(".x.x.x") & -matches("NAME$|ID$|STATE$|JJ$"))) %>% 
  # Rename columns
  rename_with(~str_replace(.x, ".x$", "")) %>% 
  # Convert reserved values to 0 (I MAY WANT TO CHANGE THIS TO "NA" LATER)
  mutate(across(where(is.numeric), ~ifelse(. %in% c(-3, -5, -6, -8, -9, -11), 0, .)))

# Calculate discipline by school
discipline_by_school <- discipline %>% 
  rename(ALT_SCH = SCH_STATUS_ALT,
         FTE_LEO = SCH_FTESECURITY_LEO) %>% 
  mutate(
    # Convert the alternative school flag to numeric
    ALT_SCH = case_when(
      ALT_SCH == "Yes" ~ 1,
      ALT_SCH == "No" ~ 0),
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
    REFERRALS_WHITE_EXCL_504 = rowSums(across(contains("REF_WH_") | contains("REF_IDEA_WH_"))),
    REFERRALS_HISPANIC_EXCL_504 = rowSums(across(contains("REF_HI_") | contains("REF_IDEA_HI_"))),
    REFERRALS_BLACK_EXCL_504 = rowSums(across(contains("REF_BL_") | contains("REF_IDEA_BL_"))),
    REFERRALS_ASIAN_EXCL_504 = rowSums(across(contains("REF_AS_") | contains("REF_IDEA_AS_"))),
    REFERRALS_NATIVE_EXCL_504 = rowSums(across(contains("REF_AM_") | contains("REF_IDEA_AM_"))),
    REFERRALS_PACIFIC_EXCL_504 = rowSums(across(contains("REF_HP_") | contains("REF_IDEA_HP_"))),
    REFERRALS_TWO_OR_MORE_EXCL_504 = rowSums(across(contains("REF_TR_") | contains("REF_IDEA_TR_"))),
    # Total disabled referrals (IDEA + 504) regardless of race as a percent of total referrals
    REFERRALS_DISABLED_PCT = REFERRALS_DISABLED / REFERRALS_TOTAL,
    # Referrals by race excluding 504 students (not disabled + IDEA) as a percent of total referrals
    REFERRALS_WHITE_EXCL_504_PCT = REFERRALS_WHITE_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_HISPANIC_EXCL_504_PCT = REFERRALS_HISPANIC_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_BLACK_EXCL_504_PCT = REFERRALS_BLACK_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_ASIAN_EXCL_504_PCT = REFERRALS_ASIAN_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_NATIVE_EXCL_504_PCT = REFERRALS_NATIVE_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_PACIFIC_EXCL_504_PCT = REFERRALS_PACIFIC_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_TWO_OR_MORE_EXCL_504_PCT = REFERRALS_TWO_OR_MORE_EXCL_504 / REFERRALS_TOTAL,
    # Total disabled enrollment (IDEA + 504) regardless of race
    ENROLLMENT_DISABLED = rowSums(across(contains("TOT_IDEAENR_") | contains("TOT_504ENR_"))),
    # Total enrollment by race (not disabled + IDEA + 504)
    ENROLLMENT_WHITE = rowSums(across(contains("SCH_ENR_WH_"))),
    ENROLLMENT_HISPANIC = rowSums(across(contains("SCH_ENR_HI_"))),
    ENROLLMENT_BLACK = rowSums(across(contains("SCH_ENR_BL_"))),
    ENROLLMENT_ASIAN = rowSums(across(contains("SCH_ENR_AS_"))),
    ENROLLMENT_NATIVE = rowSums(across(contains("SCH_ENR_AM_"))),
    ENROLLMENT_PACIFIC = rowSums(across(contains("SCH_ENR_HP_"))),
    ENROLLMENT_TWO_OR_MORE = rowSums(across(contains("SCH_ENR_TR_"))),
    # Total enrollment by race excluding 504 students for use as the denominator in calculating referrals by race per thousand (not disabled + IDEA) NOTE: THESE WILL DIFFER SLIGHTLY FROM THE COUNTS ON CRDC WEBSITE!
    ENROLLMENT_WHITE_EXCL_504 = rowSums(across(contains("SCH_ENR_WH_"))) - rowSums(across(contains("SCH_504ENR_WH_"))),
    ENROLLMENT_HISPANIC_EXCL_504 = rowSums(across(contains("SCH_ENR_HI_"))) - rowSums(across(contains("SCH_504ENR_HI_"))),
    ENROLLMENT_BLACK_EXCL_504 = rowSums(across(contains("SCH_ENR_BL_"))) - rowSums(across(contains("SCH_504ENR_BL_"))),
    ENROLLMENT_ASIAN_EXCL_504 = rowSums(across(contains("SCH_ENR_AS_"))) - rowSums(across(contains("SCH_504ENR_AS_"))),
    ENROLLMENT_NATIVE_EXCL_504 = rowSums(across(contains("SCH_ENR_AM_"))) - rowSums(across(contains("SCH_504ENR_AM_"))),
    ENROLLMENT_PACIFIC_EXCL_504 = rowSums(across(contains("SCH_ENR_HP_"))) - rowSums(across(contains("SCH_504ENR_HP_"))),
    ENROLLMENT_TWO_OR_MORE_EXCL_504 = rowSums(across(contains("SCH_ENR_TR_"))) - rowSums(across(contains("SCH_504ENR_TR_"))),
    # Calculate total referrals per thousand
    REFERRALS_TOTAL_PER_THOUSAND = REFERRALS_TOTAL / ENROLLMENT_TOTAL * 1000,
    # Calculate disabled referrals per thousand
    REFERRALS_DISABLED_PER_THOUSAND = REFERRALS_DISABLED / ENROLLMENT_DISABLED * 1000,
    # Calculate referrals by race excluding 504 students per thousand
    REFERRALS_WHITE_EXCL_504_PER_THOUSAND = REFERRALS_WHITE_EXCL_504 / ENROLLMENT_WHITE_EXCL_504 * 1000,
    REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND = REFERRALS_HISPANIC_EXCL_504 / ENROLLMENT_HISPANIC_EXCL_504 * 1000,
    REFERRALS_BLACK_EXCL_504_PER_THOUSAND = REFERRALS_BLACK_EXCL_504 / ENROLLMENT_BLACK_EXCL_504 * 1000,
    REFERRALS_ASIAN_EXCL_504_PER_THOUSAND = REFERRALS_ASIAN_EXCL_504 / ENROLLMENT_ASIAN_EXCL_504 * 1000,
    REFERRALS_NATIVE_EXCL_504_PER_THOUSAND = REFERRALS_NATIVE_EXCL_504 / ENROLLMENT_NATIVE_EXCL_504 * 1000,
    REFERRALS_PACIFIC_EXCL_504_PER_THOUSAND = REFERRALS_PACIFIC_EXCL_504 / ENROLLMENT_PACIFIC_EXCL_504 * 1000,
    REFERRALS_TWO_OR_MORE_EXCL_504_PER_THOUSAND = REFERRALS_TWO_OR_MORE_EXCL_504 / ENROLLMENT_TWO_OR_MORE_EXCL_504 * 1000,
    # Calculate the disparity between total referrals per thousand and each category
    PCT_DIFF_REFERRALS_DISABLED_PER_THOUSAND = (REFERRALS_DISABLED_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_WHITE_EXCL_504_PER_THOUSAND = (REFERRALS_WHITE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND = (REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_BLACK_EXCL_504_PER_THOUSAND = (REFERRALS_BLACK_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_ASIAN_EXCL_504_PER_THOUSAND = (REFERRALS_ASIAN_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_NATIVE_EXCL_504_PER_THOUSAND = (REFERRALS_NATIVE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_PACIFIC_EXCL_504_PER_THOUSAND = (REFERRALS_PACIFIC_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_TWO_OR_MORE_EXCL_504_PER_THOUSAND = (REFERRALS_TWO_OR_MORE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    # Calculate enrollment per thousand
    across(starts_with("ENROLLMENT") & -ENROLLMENT_TOTAL, ~ . / ENROLLMENT_TOTAL * 1000, .names = "{.col}_PER_THOUSAND"),
    # Calculate enrollment in each category as a percent of total enrollment
    PCT_ENROLLMENT_DISABLED = ENROLLMENT_DISABLED / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_WHITE = ENROLLMENT_WHITE / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_HISPANIC = ENROLLMENT_HISPANIC /ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_BLACK = ENROLLMENT_BLACK / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_ASIAN = ENROLLMENT_ASIAN / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_NATIVE = ENROLLMENT_NATIVE / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_PACIFIC = ENROLLMENT_PACIFIC / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_TWO_OR_MORE = ENROLLMENT_TWO_OR_MORE / ENROLLMENT_TOTAL,
    # Calculate law enforcement FTEs per thousand students
    FTE_LEO_PER_THOUSAND = FTE_LEO / ENROLLMENT_TOTAL * 1000,
    # Format the columns
    across(where(is.numeric), formattable::comma, digits = 0),
    across(-starts_with("PCT") & ends_with("PER_THOUSAND"), ~ formattable::digits(.x, 1)),
    across(contains("PCT"), ~ formattable::percent(.x, 1))) %>% 
  # Reorder the columns
  select(2:8, 533, 541, 634, 553, 563:567, 558:562,
         starts_with("REFERRALS_") & ends_with("_PER_THOUSAND"),
         starts_with("REFERRALS_") & ends_with("_PCT"),
         starts_with("ENROLLMENT_") & ends_with("_PER_THOUSAND") & -contains("_EXCL_504"),
         starts_with("ENROLLMENT_") & -ends_with("_PER_THOUSAND"),
         starts_with("PCT_") & ends_with("_PER_THOUSAND"),
         starts_with("REFERRALS_") & -ends_with("_PER_THOUSAND"),
         starts_with("PCT_") & -ends_with("_PER_THOUSAND") & -contains("_EXCL_504"))
  
which(names(discipline_by_school) == "DISABLED")
  
# Test how many schools have free and reduced lunch data
discipline_by_school %>% 
  filter(!is.na(PCT_FREE_AND_REDUCED_PRICE_LUNCH)) %>% 
  summarize(num = n())

# Test how many schools have more than 100% of students qualified for free and reduced price lunch (should presumably be zero))
discipline_by_school %>% 
  filter(PCT_FREE_AND_REDUCED_PRICE_LUNCH > 1) %>% 
  summarize(num = n())

# Test on Del Valle HS
View(discipline_by_school %>% 
       filter(COMBOKEY == "481662001424"))

discipline_by_school %>% 
  summarize(sum(ALT_SCH, na.rm = TRUE))

discipline_by_school %>% 
  summarize(sum(ENROLLMENT_TOTAL[ALT_SCH == 1], na.rm = TRUE))

# Calculate discipline nationally
discipline_nationally <- discipline_by_school %>% 
  summarize(ENROLLMENT_ALT_SCHOOLS = sum(ENROLLMENT_TOTAL[ALT_SCH == 1], na.rm = TRUE),
            across(c(starts_with("ALT_"),
                     starts_with("FTE_") & -ends_with("_PER_THOUSAND"),
                     -starts_with("PCT_") & ends_with("_LUNCH"),
                     starts_with("REFERRALS_") & -ends_with("_PER_THOUSAND") & -ends_with("_PCT"),
                     starts_with("ENROLLMENT_") & -ends_with("_PER_THOUSAND")), sum, na.rm = TRUE)) %>% 
  mutate(
    # Calculate student enrolled in alternative schools as a percent of total enrollment
    PCT_ENROLLMENT_ALT_SCHOOLS = ENROLLMENT_ALT_SCHOOLS / ENROLLMENT_TOTAL,
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
    # Calculate referrals by race excluding 504 students per thousand
    REFERRALS_WHITE_EXCL_504_PER_THOUSAND = REFERRALS_WHITE_EXCL_504 / ENROLLMENT_WHITE_EXCL_504 * 1000,
    REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND = REFERRALS_HISPANIC_EXCL_504 / ENROLLMENT_HISPANIC_EXCL_504 * 1000,
    REFERRALS_BLACK_EXCL_504_PER_THOUSAND = REFERRALS_BLACK_EXCL_504 / ENROLLMENT_BLACK_EXCL_504 * 1000,
    REFERRALS_ASIAN_EXCL_504_PER_THOUSAND = REFERRALS_ASIAN_EXCL_504 / ENROLLMENT_ASIAN_EXCL_504 * 1000,
    REFERRALS_NATIVE_EXCL_504_PER_THOUSAND = REFERRALS_NATIVE_EXCL_504 / ENROLLMENT_NATIVE_EXCL_504 * 1000,
    REFERRALS_PACIFIC_EXCL_504_PER_THOUSAND = REFERRALS_PACIFIC_EXCL_504 / ENROLLMENT_PACIFIC_EXCL_504 * 1000,
    REFERRALS_TWO_OR_MORE_EXCL_504_PER_THOUSAND = REFERRALS_TWO_OR_MORE_EXCL_504 / ENROLLMENT_TWO_OR_MORE_EXCL_504 * 1000,
    # Total disabled referrals (IDEA + 504) regardless of race as a percent of total referrals
    REFERRALS_DISABLED_PCT = REFERRALS_DISABLED / REFERRALS_TOTAL,
    # Referrals by race excluding 504 students (not disabled + IDEA) as a percent of total referrals
    REFERRALS_WHITE_EXCL_504_PCT = REFERRALS_WHITE_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_HISPANIC_EXCL_504_PCT = REFERRALS_HISPANIC_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_BLACK_EXCL_504_PCT = REFERRALS_BLACK_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_ASIAN_EXCL_504_PCT = REFERRALS_ASIAN_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_NATIVE_EXCL_504_PCT = REFERRALS_NATIVE_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_PACIFIC_EXCL_504_PCT = REFERRALS_PACIFIC_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_TWO_OR_MORE_EXCL_504_PCT = REFERRALS_TWO_OR_MORE_EXCL_504 / REFERRALS_TOTAL,
    # Calculate the disparity between total referrals per thousand and each category
    PCT_DIFF_REFERRALS_DISABLED_PER_THOUSAND = (REFERRALS_DISABLED_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_WHITE_EXCL_504_PER_THOUSAND = (REFERRALS_WHITE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND = (REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_BLACK_EXCL_504_PER_THOUSAND = (REFERRALS_BLACK_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_ASIAN_EXCL_504_PER_THOUSAND = (REFERRALS_ASIAN_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_NATIVE_EXCL_504_PER_THOUSAND = (REFERRALS_NATIVE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_PACIFIC_EXCL_504_PER_THOUSAND = (REFERRALS_PACIFIC_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_TWO_OR_MORE_EXCL_504_PER_THOUSAND = (REFERRALS_TWO_OR_MORE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    # Calculate enrollment per thousand
    across(starts_with("ENROLLMENT") & -ENROLLMENT_TOTAL, ~ . / ENROLLMENT_TOTAL * 1000, .names = "{.col}_PER_THOUSAND"),
    # Calculate enrollment in each category as a percent of total enrollment
    PCT_ENROLLMENT_DISABLED = ENROLLMENT_DISABLED / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_WHITE = ENROLLMENT_WHITE / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_HISPANIC = ENROLLMENT_HISPANIC /ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_BLACK = ENROLLMENT_BLACK / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_ASIAN = ENROLLMENT_ASIAN / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_NATIVE = ENROLLMENT_NATIVE / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_PACIFIC = ENROLLMENT_PACIFIC / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_TWO_OR_MORE = ENROLLMENT_TWO_OR_MORE / ENROLLMENT_TOTAL,
    # Calculate law enforcement FTEs per thousand students
    FTE_LEO_PER_THOUSAND = FTE_LEO / ENROLLMENT_TOTAL * 1000,
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
         starts_with("ALT_"),
         starts_with("FTE_"),
         ends_with("_LUNCH"))

# Calculate discipline by state
discipline_by_state <- discipline_by_school %>% 
  group_by(LEA_STATE_NAME) %>% 
  summarize(ENROLLMENT_ALT_SCHOOLS = sum(ENROLLMENT_TOTAL[ALT_SCH == 1], na.rm = TRUE),
            across(c(starts_with("ALT_"),
                     starts_with("FTE_") & -ends_with("_PER_THOUSAND"),
                     -starts_with("PCT_") & ends_with("_LUNCH"),
                     starts_with("REFERRALS_") & -ends_with("_PER_THOUSAND") & -ends_with("_PCT"),
                     starts_with("ENROLLMENT_") & -ends_with("_PER_THOUSAND")), sum, na.rm = TRUE)) %>% 
  mutate(
    # Calculate student enrolled in alternative schools as a percent of total enrollment
    PCT_ENROLLMENT_ALT_SCHOOLS = ENROLLMENT_ALT_SCHOOLS / ENROLLMENT_TOTAL,
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
    # Calculate referrals by race excluding 504 students per thousand
    REFERRALS_WHITE_EXCL_504_PER_THOUSAND = REFERRALS_WHITE_EXCL_504 / ENROLLMENT_WHITE_EXCL_504 * 1000,
    REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND = REFERRALS_HISPANIC_EXCL_504 / ENROLLMENT_HISPANIC_EXCL_504 * 1000,
    REFERRALS_BLACK_EXCL_504_PER_THOUSAND = REFERRALS_BLACK_EXCL_504 / ENROLLMENT_BLACK_EXCL_504 * 1000,
    REFERRALS_ASIAN_EXCL_504_PER_THOUSAND = REFERRALS_ASIAN_EXCL_504 / ENROLLMENT_ASIAN_EXCL_504 * 1000,
    REFERRALS_NATIVE_EXCL_504_PER_THOUSAND = REFERRALS_NATIVE_EXCL_504 / ENROLLMENT_NATIVE_EXCL_504 * 1000,
    REFERRALS_PACIFIC_EXCL_504_PER_THOUSAND = REFERRALS_PACIFIC_EXCL_504 / ENROLLMENT_PACIFIC_EXCL_504 * 1000,
    REFERRALS_TWO_OR_MORE_EXCL_504_PER_THOUSAND = REFERRALS_TWO_OR_MORE_EXCL_504 / ENROLLMENT_TWO_OR_MORE_EXCL_504 * 1000,
    # Total disabled referrals (IDEA + 504) regardless of race as a percent of total referrals
    REFERRALS_DISABLED_PCT = REFERRALS_DISABLED / REFERRALS_TOTAL,
    # Referrals by race excluding 504 students (not disabled + IDEA) as a percent of total referrals
    REFERRALS_WHITE_EXCL_504_PCT = REFERRALS_WHITE_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_HISPANIC_EXCL_504_PCT = REFERRALS_HISPANIC_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_BLACK_EXCL_504_PCT = REFERRALS_BLACK_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_ASIAN_EXCL_504_PCT = REFERRALS_ASIAN_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_NATIVE_EXCL_504_PCT = REFERRALS_NATIVE_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_PACIFIC_EXCL_504_PCT = REFERRALS_PACIFIC_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_TWO_OR_MORE_EXCL_504_PCT = REFERRALS_TWO_OR_MORE_EXCL_504 / REFERRALS_TOTAL,
    # Calculate the disparity between total referrals per thousand and each category
    PCT_DIFF_REFERRALS_DISABLED_PER_THOUSAND = (REFERRALS_DISABLED_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_WHITE_EXCL_504_PER_THOUSAND = (REFERRALS_WHITE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND = (REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_BLACK_EXCL_504_PER_THOUSAND = (REFERRALS_BLACK_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_ASIAN_EXCL_504_PER_THOUSAND = (REFERRALS_ASIAN_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_NATIVE_EXCL_504_PER_THOUSAND = (REFERRALS_NATIVE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_PACIFIC_EXCL_504_PER_THOUSAND = (REFERRALS_PACIFIC_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_TWO_OR_MORE_EXCL_504_PER_THOUSAND = (REFERRALS_TWO_OR_MORE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    # Calculate enrollment per thousand
    across(starts_with("ENROLLMENT") & -ENROLLMENT_TOTAL, ~ . / ENROLLMENT_TOTAL * 1000, .names = "{.col}_PER_THOUSAND"),
    # Calculate enrollment in each category as a percent of total enrollment
    PCT_ENROLLMENT_DISABLED = ENROLLMENT_DISABLED / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_WHITE = ENROLLMENT_WHITE / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_HISPANIC = ENROLLMENT_HISPANIC /ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_BLACK = ENROLLMENT_BLACK / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_ASIAN = ENROLLMENT_ASIAN / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_NATIVE = ENROLLMENT_NATIVE / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_PACIFIC = ENROLLMENT_PACIFIC / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_TWO_OR_MORE = ENROLLMENT_TWO_OR_MORE / ENROLLMENT_TOTAL,
    # Calculate law enforcement FTEs per thousand students
    FTE_LEO_PER_THOUSAND = FTE_LEO / ENROLLMENT_TOTAL * 1000,
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
         starts_with("ALT_"),
         starts_with("FTE_"),
         ends_with("_LUNCH"))

# Test numbers for California
discipline_by_state %>% 
  filter(LEA_STATE_NAME == "CALIFORNIA") %>% 
  select(FTE_LEO,
         FTE_LEO_PER_THOUSAND,
         ALT_SCH,
         ENROLLMENT_TOTAL,
         ENROLLMENT_ALT_SCHOOLS,
         PCT_ENROLLMENT_ALT_SCHOOLS) %>% 
  View()

discipline_by_school %>% 
  filter(LEA_STATE_NAME == "CALIFORNIA") %>% 
  summarize(sum(FTE_LEO, na.rm = TRUE),
            sum(ALT_SCH, na.rm = TRUE),
            sum(ENROLLMENT_TOTAL, na.rm = TRUE),
            n())

discipline_by_school %>% 
  filter(LEA_STATE_NAME == "CALIFORNIA") %>% 
  summarize(sum(FTE_LEO, na.rm = TRUE),
            sum(ALT_SCH, na.rm = TRUE),
            sum(ENROLLMENT_TOTAL, na.rm = TRUE),
            sum(ENROLLMENT_TOTAL[ALT_SCH == 1], na.rm = TRUE),
            n())

# Test a state that uses direct certification
discipline_by_state %>% 
  filter(LEA_STATE_NAME == "DISTRICT OF COLUMBIA") %>% 
  View()

# Calculate discipline by district
discipline_by_district <- discipline_by_school %>% 
  group_by(LEAID) %>% 
  summarize(ENROLLMENT_ALT_SCHOOLS = sum(ENROLLMENT_TOTAL[ALT_SCH == 1], na.rm = TRUE),
            across(c(starts_with("ALT_"),
                     starts_with("FTE_") & -ends_with("_PER_THOUSAND"),
                     -starts_with("PCT_") & ends_with("_LUNCH"),
                     starts_with("REFERRALS_") & -ends_with("_PER_THOUSAND") & -ends_with("_PCT"),
                     starts_with("ENROLLMENT_") & -ends_with("_PER_THOUSAND")), sum, na.rm = TRUE)) %>% 
  mutate(
    # Calculate student enrolled in alternative schools as a percent of total enrollment
    PCT_ENROLLMENT_ALT_SCHOOLS = ENROLLMENT_ALT_SCHOOLS / ENROLLMENT_TOTAL,
    # Calculate free or reduced lunch as a percent of total enrollment
    PCT_DIRECT_CERT_LUNCH = DIRECT_CERT_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_FREE_LUNCH = FREE_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_REDUCED_PRICE_LUNCH = REDUCED_PRICE_LUNCH / ENROLLMENT_TOTAL_NCES,
    PCT_FREE_AND_REDUCED_PRICE_LUNCH = case_when(
      !is.na(FREE_AND_REDUCED_PRICE_LUNCH)  ~ FREE_AND_REDUCED_PRICE_LUNCH / ENROLLMENT_TOTAL_NCES,
      is.na(FREE_AND_REDUCED_PRICE_LUNCH) ~ DIRECT_CERT_LUNCH / ENROLLMENT_TOTAL_NCES),
    # Calculate total referrals per thousand
    REFERRALS_TOTAL_PER_THOUSAND = REFERRALS_TOTAL / ENROLLMENT_TOTAL * 1000,
    # Calculate disabled referrals per thousand
    REFERRALS_DISABLED_PER_THOUSAND = REFERRALS_DISABLED / ENROLLMENT_DISABLED * 1000,
    # Calculate referrals by race excluding 504 students per thousand
    REFERRALS_WHITE_EXCL_504_PER_THOUSAND = REFERRALS_WHITE_EXCL_504 / ENROLLMENT_WHITE_EXCL_504 * 1000,
    REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND = REFERRALS_HISPANIC_EXCL_504 / ENROLLMENT_HISPANIC_EXCL_504 * 1000,
    REFERRALS_BLACK_EXCL_504_PER_THOUSAND = REFERRALS_BLACK_EXCL_504 / ENROLLMENT_BLACK_EXCL_504 * 1000,
    REFERRALS_ASIAN_EXCL_504_PER_THOUSAND = REFERRALS_ASIAN_EXCL_504 / ENROLLMENT_ASIAN_EXCL_504 * 1000,
    REFERRALS_NATIVE_EXCL_504_PER_THOUSAND = REFERRALS_NATIVE_EXCL_504 / ENROLLMENT_NATIVE_EXCL_504 * 1000,
    REFERRALS_PACIFIC_EXCL_504_PER_THOUSAND = REFERRALS_PACIFIC_EXCL_504 / ENROLLMENT_PACIFIC_EXCL_504 * 1000,
    REFERRALS_TWO_OR_MORE_EXCL_504_PER_THOUSAND = REFERRALS_TWO_OR_MORE_EXCL_504 / ENROLLMENT_TWO_OR_MORE_EXCL_504 * 1000,
    # Total disabled referrals (IDEA + 504) regardless of race as a percent of total referrals
    REFERRALS_DISABLED_PCT = REFERRALS_DISABLED / REFERRALS_TOTAL,
    # Referrals by race excluding 504 students (not disabled + IDEA) as a percent of total referrals
    REFERRALS_WHITE_EXCL_504_PCT = REFERRALS_WHITE_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_HISPANIC_EXCL_504_PCT = REFERRALS_HISPANIC_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_BLACK_EXCL_504_PCT = REFERRALS_BLACK_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_ASIAN_EXCL_504_PCT = REFERRALS_ASIAN_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_NATIVE_EXCL_504_PCT = REFERRALS_NATIVE_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_PACIFIC_EXCL_504_PCT = REFERRALS_PACIFIC_EXCL_504 / REFERRALS_TOTAL,
    REFERRALS_TWO_OR_MORE_EXCL_504_PCT = REFERRALS_TWO_OR_MORE_EXCL_504 / REFERRALS_TOTAL,
    # Calculate the disparity between total referrals per thousand and each category
    PCT_DIFF_REFERRALS_DISABLED_PER_THOUSAND = (REFERRALS_DISABLED_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_WHITE_EXCL_504_PER_THOUSAND = (REFERRALS_WHITE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND = (REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_BLACK_EXCL_504_PER_THOUSAND = (REFERRALS_BLACK_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_ASIAN_EXCL_504_PER_THOUSAND = (REFERRALS_ASIAN_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_NATIVE_EXCL_504_PER_THOUSAND = (REFERRALS_NATIVE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_PACIFIC_EXCL_504_PER_THOUSAND = (REFERRALS_PACIFIC_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    PCT_DIFF_REFERRALS_TWO_OR_MORE_EXCL_504_PER_THOUSAND = (REFERRALS_TWO_OR_MORE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND,
    # Calculate enrollment per thousand
    across(starts_with("ENROLLMENT") & -ENROLLMENT_TOTAL, ~ . / ENROLLMENT_TOTAL * 1000, .names = "{.col}_PER_THOUSAND"),
    # Calculate enrollment in each category as a percent of total enrollment
    PCT_ENROLLMENT_DISABLED = ENROLLMENT_DISABLED / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_WHITE = ENROLLMENT_WHITE / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_HISPANIC = ENROLLMENT_HISPANIC /ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_BLACK = ENROLLMENT_BLACK / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_ASIAN = ENROLLMENT_ASIAN / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_NATIVE = ENROLLMENT_NATIVE / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_PACIFIC = ENROLLMENT_PACIFIC / ENROLLMENT_TOTAL,
    PCT_ENROLLMENT_TWO_OR_MORE = ENROLLMENT_TWO_OR_MORE / ENROLLMENT_TOTAL,
    # Calculate law enforcement FTEs per thousand students
    FTE_LEO_PER_THOUSAND = FTE_LEO / ENROLLMENT_TOTAL * 1000,
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
         starts_with("ALT_"),
         starts_with("FTE_"),
         ends_with("_LUNCH"))

discipline_by_district %>% 
  filter(LEA_NAME == "BRIDGEPORT SCHOOL DISTRICT") %>% 
  View()

# Test Boston, MA
discipline_by_district %>% 
  filter(LEAID == "2502790") %>% 
  select(FTE_LEO,
         FTE_LEO_PER_THOUSAND,
         ALT_SCH,
         ENROLLMENT_TOTAL)

discipline_by_school %>% 
  filter(LEAID == "2502790") %>% 
  summarize(sum(FTE_LEO, na.rm = TRUE),
            sum(ALT_SCH, na.rm = TRUE),
            sum(ENROLLMENT_TOTAL, na.rm = TRUE),
            n())

### ANALYSIS

discipline_by_state %>% 
  arrange(desc(PCT_ENROLLMENT_ALT_SCHOOLS)) %>% 
  View()

# Do alternative schools have higher rates of referral than non-alternative schools?
## Alternative schools
discipline_by_school %>% 
  filter(ALT_SCH == 1) %>% 
  summarize(sum(REFERRALS_TOTAL, na.rm = TRUE),
            sum(ENROLLMENT_TOTAL, na.rm = TRUE))
# 20.2 per thousand

## Non-alternative schools
discipline_by_school %>% 
  filter(ALT_SCH == 0) %>% 
  summarize(sum(REFERRALS_TOTAL, na.rm = TRUE),
            sum(ENROLLMENT_TOTAL, na.rm = TRUE))
# 4.4 per thousand

# Correlations

which(names(discipline_by_school) == "FTE_LEO_PER_THOUSAND")

# Remove INF values prior to running the correlation analysis
discipline_by_district_inf_removed <- discipline_by_district %>% 
  mutate(across(everything(), ~na_if(., Inf))) %>% 
  select(1:3, 70, 31, 62, 4:12)

discipline_by_school_inf_removed <- discipline_by_school %>% 
  mutate(across(everything(), ~na_if(., Inf))) %>% 
  select(1, 6, 5, 8, 16, 9:10, 22:30)

discipline_by_high_school_inf_removed <- discipline_by_school_inf_removed %>% 
  filter(SCH_GRADE_G12 == "Yes")

# What's the correlation between LEOs-per-thousand-students, free/reduced lunch, percent of students in alternatives schools and referrals to law enforcement?
district_correlations <- data.frame(cor(discipline_by_district_inf_removed[4:15],
    use = "pairwise.complete.obs"))

school_correlations <- data.frame(cor(discipline_by_school_inf_removed[5:16],
                                        use = "pairwise.complete.obs"))

high_school_correlations <- data.frame(cor(discipline_by_high_school_inf_removed[5:16],
                                      use = "pairwise.complete.obs"))

write_csv(district_correlations,
          "data/exported/district_correlations.csv")

write_csv(school_correlations,
          "data/exported/school_correlations.csv")

write_csv(high_school_correlations,
          "data/exported/high_school_correlations.csv")

# Regressions
# Free/reduced price lunch
lm(formula = 
     REFERRALS_TOTAL_PER_THOUSAND ~ PCT_FREE_AND_REDUCED_PRICE_LUNCH,
   data = discipline_by_district_inf_removed)

discipline_by_district_inf_removed_model <- lm(formula = REFERRALS_TOTAL_PER_THOUSAND ~ PCT_FREE_AND_REDUCED_PRICE_LUNCH, data = discipline_by_district_inf_removed)

summary.data.frame(discipline_by_district_inf_removed_model)

# LEO-per-thousand-students
cor(discipline_by_district_info_removed[])

# What's the picture nationally?
View(discipline_nationally)

# What are the shares of disabled students and Black students in referrals and enrollment?
discipline_nationally %>% 
  select(REFERRALS_DISABLED_PCT, PCT_ENROLLMENT_DISABLED, REFERRALS_BLACK_EXCL_504_PCT, PCT_ENROLLMENT_BLACK) %>% 
  View()

# Which states disproportionately refer disabled students?
count(discipline_by_state %>% 
        filter(REFERRALS_DISABLED_PCT > PCT_ENROLLMENT_DISABLED))

# Which states disproportionately refer Black students?
count(discipline_by_state %>% 
        filter(REFERRALS_BLACK_EXCL_504_PCT > PCT_ENROLLMENT_BLACK))

# Which states disproportionately refer Native American students?
count(discipline_by_state %>% 
        filter(REFERRALS_NATIVE_EXCL_504_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND))

# Which states disproportionately refer Hispanic students?
count(discipline_by_state %>% 
        filter(REFERRALS_HISPANIC_EXCL_504_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND))

discipline_by_district %>% 
  filter(str_detect(LEA_NAME, regex("Mobile County"))) %>% 
  View()

# Which states disproportionately refer White students?
count(discipline_by_state %>% 
        filter(REFERRALS_WHITE_EXCL_504_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND))

# Which states refer Black students at a proportion double their share of the student population?
discipline_by_state %>% 
  filter(REFERRALS_BLACK_EXCL_504_PCT > PCT_ENROLLMENT_BLACK * 2) %>% 
  mutate(PCT_DIFF = ((REFERRALS_BLACK_EXCL_504_PCT - PCT_ENROLLMENT_BLACK)) / PCT_ENROLLMENT_BLACK) %>% 
  select(LEA_STATE_NAME, PCT_DIFF, REFERRALS_BLACK_EXCL_504_PCT, PCT_ENROLLMENT_BLACK) %>% 
  View()

# What proportion of schools refer a disproportionate number of disabled students to law enforcement?
count(discipline_by_school %>% 
        filter(ENROLLMENT_DISABLED >= 10,
               REFERRALS_DISABLED_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND)) /
  count(discipline_by_school %>% 
          filter(ENROLLMENT_DISABLED >= 10))

# What proportion of schools refer a disproportionate number of Black students to law enforcement?
count(discipline_by_school %>% 
        filter(ENROLLMENT_BLACK >= 10,
               REFERRALS_BLACK_EXCL_504_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND)) /
  count(discipline_by_school %>% 
          filter(ENROLLMENT_BLACK >= 10))

# What proportion of schools refer a disproportionate number of Native American students to law enforcement?
count(discipline_by_school %>% 
        filter(ENROLLMENT_NATIVE >= 10,
               REFERRALS_NATIVE_EXCL_504_PER_THOUSAND > REFERRALS_TOTAL_PER_THOUSAND)) /
  count(discipline_by_school %>% 
          filter(ENROLLMENT_NATIVE >= 10))

# Take a deeper look at Native American referrals.
discipline_by_state %>% 
  mutate(PCT_DIFF = (REFERRALS_NATIVE_EXCL_504_PER_THOUSAND - REFERRALS_TOTAL_PER_THOUSAND) / REFERRALS_TOTAL_PER_THOUSAND) %>% 
  select(LEA_STATE_NAME, ENROLLMENT_NATIVE, PCT_ENROLLMENT_NATIVE, PCT_DIFF, REFERRALS_NATIVE_EXCL_504_PER_THOUSAND, REFERRALS_TOTAL_PER_THOUSAND) %>%
  arrange(desc(PCT_DIFF)) %>% 
  View()

# What about Vermont?
discipline_by_state %>% 
  arrange(desc(REFERRALS_DISABLED_PCT)) %>% 
  select(LEA_STATE_NAME, REFERRALS_DISABLED_PCT, PCT_ENROLLMENT_DISABLED) %>% 
  View()

# What about Lancaster County, VA?
discipline_by_district %>% 
  filter(ALT_SCH == 0,
         LEA_STATE_NAME == "VIRGINIA") %>% 
  arrange(desc(REFERRALS_TOTAL_PER_THOUSAND)) %>% 
  select(LEA_NAME, REFERRALS_DISABLED_PCT, PCT_ENROLLMENT_DISABLED, REFERRALS_BLACK_EXCL_504_PCT, PCT_ENROLLMENT_BLACK) %>% 
  View()

# What about Des Moines, IA?
discipline_by_district %>% 
  filter(LEA_STATE_NAME == "IOWA",
         str_detect(LEA_NAME, regex("des moines", ignore_case = TRUE))) %>%
  select(LEA_NAME, REFERRALS_DISABLED_PCT, PCT_ENROLLMENT_DISABLED, REFERRALS_BLACK_EXCL_504_PCT, PCT_ENROLLMENT_BLACK) %>% 
  View()

# What about Iowa as a whole?
discipline_by_state %>% 
  filter(LEA_STATE_NAME == "IOWA") %>% 
  select(LEA_STATE_NAME, REFERRALS_DISABLED_PER_THOUSAND,
         REFERRALS_BLACK_EXCL_504_PER_THOUSAND,
         REFERRALS_WHITE_EXCL_504_PER_THOUSAND,
         REFERRALS_TOTAL_PER_THOUSAND) %>% 
  View()

# What about Mobile, AL?
discipline_by_district %>% 
  filter(LEA_STATE_NAME == "ALABAMA",
         str_detect(LEA_NAME, regex("mobile", ignore_case = TRUE))) %>%
  select(LEA_NAME, REFERRALS_DISABLED_PCT, PCT_ENROLLMENT_DISABLED, REFERRALS_BLACK_EXCL_504_PCT, PCT_ENROLLMENT_BLACK) %>% 
  View()

# What about Philadelphia?
discipline_by_district %>% 
  filter(LEA_STATE_NAME == "PENNSYLVANIA",
         str_detect(LEA_NAME, regex("philadelphia", ignore_case = TRUE))) %>%
  select(LEA_NAME, REFERRALS_DISABLED_PCT, PCT_ENROLLMENT_DISABLED, REFERRALS_BLACK_EXCL_504_PCT, PCT_ENROLLMENT_BLACK) %>% 
  View()

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

# Do the numbers for Philadelphia and Pittsburgh differ from what's in the state data?
discipline_by_district %>% 
  filter(LEAID == "4218990" | LEAID == "4219170") %>% 
  View()

# Philadelphia has only zero LEOs. How many other large school districts do as well?
discipline_by_district %>% 
  mutate(ENROLLMENT_TOTAL_DECILE = ntile(ENROLLMENT_TOTAL, 10)) %>% 
  filter(ENROLLMENT_TOTAL_DECILE == 10) %>% 
  arrange(desc(ENROLLMENT_TOTAL)) %>% 
  select(LEAID, LEA_NAME, LEA_STATE_NAME, ENROLLMENT_TOTAL, FTE_LEO, REFERRALS_TOTAL) %>% 
  head(30) %>% 
  write_csv("data/exported/top_30_districts.csv")

# Format the data for export to Datawrapper

discipline_by_school_datawrapper <- discipline_by_school %>% 
  select(STATE = LEA_STATE_NAME,
         DISTRICT = LEA_NAME,
         SCHOOL = SCH_NAME,
         starts_with("REFERRALS_") & -ends_with("_PER_THOUSAND") & -contains("_PCT"),
         starts_with("REFERRALS_") & ends_with("_PER_THOUSAND"),
         starts_with("ENROLLMENT_") & -ends_with("_PER_THOUSAND") & -contains("_EXCL_504") & -contains("_NCES"),
         starts_with("PCT_ENROLLMENT")) %>% 
  write_csv("data/exported/discipline_by_school_datawrapper.csv")

# Export the data
write_csv(discipline_by_school,
          "data/exported/discipline_by_school.csv")

write_csv(discipline_nationally,
          "data/exported/discipline_nationally.csv")

write_csv(discipline_by_state,
          "data/exported/discipline_by_state.csv")

write_csv(discipline_by_district,
          "data/exported/discipline_by_district.csv")
