# Title: Data wrangling for the International Sex Survey project (ASRS)
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2022-12-19
# Copyright (c) 2022 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Import packages
library(tidyverse)
library(haven)
library(sjlabelled)

# Define list of names mappings for additional variables to use
col_names_lookup <- c(
  Age = "age",
  gender = "gen_3cat",
  education = "edu",
  residence = "resid",
  sexual_orientation = "sexor_6cat",
  relationship_status = "rel_stat",
  children = "child",
  phy_ill = "phy_ill1",
  men_ill = "men_ill1",
  sex_prob = "sex_prob1",
  socioeconomic_status = "SES",
  BMI = "BMI",
  sex_fre_total = "sex_fre_total",
  porn_fre = "porn_fre",
  mast_fre = "mast_fre",
  porn_add = "porn_add",
  num_sex_par = "num_sex_par",
  age_at_first_sex = "age_at_first_sex",
  first_porn = "first_porn",
  gambling1_1_2 = "gambling1_1_2",
  gambling1_2_2 = "gambling1_2_2",
  covid4 = "COVID4",
  covid5 = "COVID5"
)

# Define list of demographic variables to use
demographic_variables <- c(
  "country", "UserLanguage", "sex_birth", "gender", "Age", "BMI",
  "education", "study", "work", "socioeconomic_status", 
  "residence", "children", "sexual_orientation",
  "relationship_status"
)

# Read and process base data set
iss_asrs <-
  read_sav(file.path("data", "raw", "iss_asrs.sav")) %>%
  # Rename variables using mappings defined above
  rename(col_names_lookup) %>%
  # Select variables of interest
  select(
    ResponseId, 
    all_of(demographic_variables), 
    all_of(names(col_names_lookup)), 
    ASRS1:ASRS6
  ) %>%
  mutate(
    # Convert labelled data to their label values
    across(all_of(demographic_variables), to_label),
    # Map 'UserLanguage' values to readable language names
    language = case_when(
      UserLanguage == "ALGR" ~ "Arabic",
      UserLanguage == "BE-NE" ~ "Dutch",
      UserLanguage == "BN-BANGLADESH" ~ "Bangla",
      UserLanguage == "BR-BRAZIL" ~ "Portuguese - Brazil",
      UserLanguage == "PT-PORTUGAL" ~ "Portuguese - Portugal",
      UserLanguage == "CA-CANADA" ~ "French",
      str_detect(UserLanguage, "FR-") ~ "French",
      UserLanguage == "CN-M" ~ "Mandarin - Simplified",
      UserLanguage == "TW-M" ~ "Mandarin - Traditional",
      UserLanguage == "CRO" ~ "Croatian",
      UserLanguage == "CZECH" ~ "Czech",
      UserLanguage == "DE-AUS" ~ "German",
      UserLanguage == "DE-GERMANY" ~ "German",
      UserLanguage == "EG" ~ "Arabic",
      UserLanguage == "EN" ~ "English",
      UserLanguage == "ENG" ~ "English",
      str_detect(UserLanguage, "ENG-|EN-") ~ "English",
      (str_detect(UserLanguage, "ES-|ESP-|SP-CO") & !UserLanguage == "ESP-ES") ~ "Spanish - Latin America",
      UserLanguage == "ESP-ES" ~ "Spanish - Spain",
      UserLanguage == "HU-HUNGARY" ~ "Hungarian",
      UserLanguage == "IL-ISRAEL" ~ "Hebrew",
      UserLanguage == "IN-INDIA" ~ "Hindi",
      UserLanguage == "IQ" ~ "Arabic",
      UserLanguage == "IT-ITALY" ~ "Italian",
      UserLanguage == "JP-JAPAN" ~ "Japanese",
      UserLanguage == "KOR" ~ "Korean",
      UserLanguage == "LT-LITHUANIA" ~ "Lithuanian",
      UserLanguage == "MAC" ~ "Macedonian",
      UserLanguage == "PL-POLAND" ~ "Polish",
      UserLanguage == "RO-ROMANIA" ~ "Romanian",
      UserLanguage == "SK-SLOVAKIA" ~ "Slovakian",
      UserLanguage == "TR-TURKEY" ~ "Turkish",
    ),
    # Remove unnecessary characters from country names
    country = str_remove(country, ":"),
    # Create 'Other' country category for select countries
    country = case_when(country %in% c("Egypt", "Romania") ~ "Other", T ~ country),
    # Create an 'id' column based on 'ResponseId'
    id = ResponseId,
    # Calculate ASRS score (sum)
    ASRS_score = rowSums(select(., all_of(sprintf("ASRS%i", 1:6)))),
    # Create alternative scoring for ASRS items
    ASRS3_alt = case_when(ASRS3 %in% 0:2 ~ 0, ASRS3 %in% 3:4 ~ 1),
    ASRS5_alt = case_when(ASRS5 %in% 0:2 ~ 0, ASRS5 %in% 3:4 ~ 1),
    ASRS6_alt = case_when(ASRS6 %in% 0:2 ~ 0, ASRS6 %in% 3:4 ~ 1),
    ASRS1_alt = case_when(ASRS1 %in% 0:1 ~ 0, ASRS1 %in% 2:4 ~ 1),
    ASRS2_alt = case_when(ASRS2 %in% 0:1 ~ 0, ASRS2 %in% 2:4 ~ 1),
    ASRS4_alt = case_when(ASRS4 %in% 0:1 ~ 0, ASRS4 %in% 2:4 ~ 1)
  ) %>%
  # Calculate alternative ASRS score (sum)
  mutate(ASRS_score_alt = rowSums(select(., all_of(sprintf("ASRS%i_alt", 1:6))))) %>%
  # Remove all variable labels
  zap_label() %>%
  # Remove unnecessary variables
  select(-ResponseId, -UserLanguage) %>%
  # Set column types
  mutate(
    id = as.factor(id),
    across(where(is.character), as.factor),
    across(!where(is.factor), as.numeric),
    socioeconomic_status = as.numeric(factor(socioeconomic_status, levels = c(
      "My life circumstances are among the worst",
      "My life circumstances are much worse than average",
      "My life circumstances are worse than average",
      "My life circumstances are average",
      "My life circumstances are better than average",
      "My life circumstances are much better than average",
      "My life circumstances are among the best"
    ))),
    country = factor(country, levels = sort(unique(country))),
    language = factor(language, levels = sort(unique(language))),
    gender = factor(gender, levels = c("Woman", "Man", "Gender diverse individual")),
    sexual_orientation = factor(sexual_orientation, levels = c(
      "Heterosexual", "Homosexual", "Bi+",
      "Homo- and heteroflexible identities",
      "Asexual", "Questioning or other"
    )),
    study = factor(study, levels = c(
      "Yes, in primary education (e.g., elementary school)",
      "Yes, in secondary education (e.g., high school)",
      "Yes, in tertiary education (e.g., college or university)",
      "No"
    )),
    work = factor(work, levels = c(
      "Yes, full time",
      "Yes, part-time",
      "Yes, I do odd jobs",
      "No"
    )),
    residence = factor(residence, levels = c(
      "Metropolis (population is over 1 million people)",
      "City (population is between 100,000-999,999 people)",
      "Town (population is between 1,000-99,999 people)",
      "Village (population is below 1,000 people)"
    )),
    relationship_status = factor(relationship_status, levels = c(
      "Married or common-law partners",
      "In a relationship",
      "Widow or widower",
      "Divorced",
      "Single"
    )),
    children = factor(str_to_sentence(children), levels = c("Yes, 1", "Yes, 2", "Yes, 3", "Yes, 4", "Yes, 5", "Yes, 6-9", "Yes, 10 or more", "No"))
  )

# Save processed data set
saveRDS(iss_asrs, file.path("data", "prepared", "iss_asrs.Rds"))
