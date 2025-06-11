# Import libraries --------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)

# Import files ------------------------------------------------------------
stub <- read_csv("/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/AlzheimersDiseaseRes-StubDetails_DATA_2024-06-28_1029.csv")

ppt_status <- read_csv("/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/2024-06-28T15_28_21.921Z_clinical_core_export.csv")

## Change column names
colnames(ppt_status)[which(names(ppt_status) == "globalId")] <- "Ripple Global ID"
colnames(ppt_status)[which(names(ppt_status) == "cv.core_participant_status")] <- "participant_status"
colnames(stub)[which(names(stub) == "global_id")] <- "Ripple Global ID"

mri_elig <- read_csv("/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/AlzheimersDiseaseRes-MRIElig_DATA_LABELS_2024-06-28_1038.csv")

pet_elig <- read_csv("/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/AlzheimersDiseaseRes-PETElig_DATA_LABELS_2024-06-28_1027.csv")

mri_all_scans <- read_csv("/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/AlzheimersDiseaseRes-AllCoreImagingScansM_DATA_LABELS_2024-06-28_1034.csv")

amyloid_pet_all_scans <- read_csv("/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/AlzheimersDiseaseRes-AllCoreImagingScansA_DATA_LABELS_2024-06-28_1038.csv")

tau_pet_all_scans <- read_csv("/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/AlzheimersDiseaseRes-AllCoreImagingScansT_DATA_LABELS_2024-06-28_1038.csv")

# Merging databases ----------------------------------------------------------
## Merging MR eligibility with all Core MR scans
mri_elig <- mri_elig %>%
  arrange(desc(`MRI: Eligibility Date`)) %>%
  distinct(`Ripple Global ID`, .keep_all = TRUE)

mri_scans_remaining_all <- anti_join(mri_elig, mri_all_scans, by = "Ripple Global ID")

## Merging Amyloid PET eligibility with all Core Amyloid PET scans
pet_elig <- pet_elig %>%
  arrange(desc(`PET: Eligibility Date`)) %>%
  distinct(`Ripple Global ID`, .keep_all = TRUE)

amyloid_pet_scans_remaining_all <- anti_join(pet_elig, amyloid_pet_all_scans, by = "Ripple Global ID")

## Merging Tau PET eligibility with all Core Tau PET scans
tau_pet_scans_remaining_all <- anti_join(pet_elig, tau_pet_all_scans, by = "Ripple Global ID")

## Merge stub data
### MRI
mri_scans_remaining_all <- merge(mri_scans_remaining_all, stub, by = "Ripple Global ID")

amyloid_pet_scans_remaining_all <- merge(amyloid_pet_scans_remaining_all, stub, by = "Ripple Global ID")

tau_pet_scans_remaining_all <- merge(tau_pet_scans_remaining_all, stub, by = "Ripple Global ID")

## Merge participant status
mri_scans_remaining_all <- merge(mri_scans_remaining_all, ppt_status, by = "Ripple Global ID")

amyloid_pet_scans_remaining_all <- merge(amyloid_pet_scans_remaining_all, ppt_status, by = "Ripple Global ID")

tau_pet_scans_remaining_all <- merge(tau_pet_scans_remaining_all, ppt_status, by = "Ripple Global ID")

# Calculating current age -------------------------------------------------
## MRI
mri_scans_remaining_all <- mri_scans_remaining_all %>%
  mutate(current_age = round(time_length(difftime(as.Date(Sys.Date()), as.Date(dob_stub)), "years"), 
                             digits = 2)) %>% # Creates a new column that calculates current age of participants
  mutate(age_dummy = ifelse(current_age >= 80.00, 1, 0)) # Creates new column with dummy variables (1 = 80+ age, 0 = everyone else)
print(mri_scans_remaining_all[46:47]) # Verify it was successful

## Amyloid PET
amyloid_pet_scans_remaining_all <- amyloid_pet_scans_remaining_all %>%
  mutate(current_age = round(time_length(difftime(as.Date(Sys.Date()), as.Date(dob_stub)), "years"),
                             digits = 2)) %>% # Creates a new column that calculates current age of participants
  mutate(age_dummy = ifelse(current_age >= 80.00, 1, 0)) # Creates a new column with dummy variables (1 = 80+ age, 0 = everyone else)
print(amyloid_pet_scans_remaining_all[46:47])

## Tau PET
tau_pet_scans_remaining_all <- tau_pet_scans_remaining_all %>%
  mutate(current_age = round(time_length(difftime(as.Date(Sys.Date()), as.Date(dob_stub)), "years"),
                             digits = 2)) %>% # Creates a new column that calculates current age of participants
  mutate(age_dummy = ifelse(current_age >= 80.00, 1, 0)) # Creates a new column with dummy variables (1 = 80+ age, 0 = everyone else)
print(tau_pet_scans_remaining_all[46:47])

# Cleaning databases ---------------------------------------------------------
## Remove duplicates & keep last instance
### MRI scans
mri_scans_remaining_all_updated <- mri_scans_remaining_all[order(mri_scans_remaining_all$`Ripple Global ID`, 
                                                 decreasing = FALSE), ] # Sort ptids in deceasing order
print(mri_scans_remaining_all_updated)

mri_scans_remaining_unique <- mri_scans_remaining_all_updated[!duplicated(mri_scans_remaining_all_updated$`Ripple Global ID`, fromLast = TRUE), ]
print(mri_scans_remaining_unique)

### Amyloid PET scans
amyloid_pet_scans_remaining_all_updated <- amyloid_pet_scans_remaining_all[order(amyloid_pet_scans_remaining_all$`Ripple Global ID`, 
                                                                 decreasing = FALSE), ] # Sort ptids in deceasing order
print(amyloid_pet_scans_remaining_all_updated)

amyloid_pet_remaining_unique <- amyloid_pet_scans_remaining_all_updated[!duplicated(amyloid_pet_scans_remaining_all_updated$`Ripple Global ID`, fromLast = TRUE), ]
print(amyloid_pet_remaining_unique)

### Tau PET scans
tau_pet_scans_remaining_all_updated <- tau_pet_scans_remaining_all[order(tau_pet_scans_remaining_all$`Ripple Global ID`, 
                                                                 decreasing = FALSE), ] # Sort ptids in deceasing order
print(tau_pet_scans_remaining_all_updated)

tau_pet_remaining_unique <- tau_pet_scans_remaining_all_updated[!duplicated(tau_pet_scans_remaining_all_updated$`Ripple Global ID`, fromLast = TRUE), ]
print(tau_pet_remaining_unique)

## Drop columns
### MRI scans
mri_scans_remaining_clean <- mri_scans_remaining_unique %>%
  select(-c(redcap_event_name, 
            redcap_repeat_instrument, 
            redcap_repeat_instance, 
            `Event Name`, 
            `Repeat Instrument`, 
            `Repeat Instance`))
print(mri_scans_remaining_clean)

### Amyloid PET scans
amyloid_pet_remaining_clean <- amyloid_pet_remaining_unique %>%
  select(-c(redcap_event_name, 
            redcap_repeat_instrument, 
            redcap_repeat_instance, 
            `Event Name`, 
            `Repeat Instrument`, 
            `Repeat Instance`))
print(amyloid_pet_remaining_clean)

### Tau PET scans
tau_pet_remaining_clean <- tau_pet_remaining_unique %>%
  select(-c(redcap_event_name, 
            redcap_repeat_instrument, 
            redcap_repeat_instance, 
            `Event Name`, 
            `Repeat Instrument`, 
            `Repeat Instance`))
print(tau_pet_remaining_clean)

# Pending -------------------------------------------------------
## MRI
### Pending
mri_pending <- mri_scans_remaining_clean[which(mri_scans_remaining_clean$`MRI: Screening Result` == "Pending"), ] # Filters by pending status
print(mri_pending)

## Amyloid PET
### Pending
amyloid_pet_pending <- amyloid_pet_remaining_clean[which(amyloid_pet_remaining_clean$`PET: Screening Result` == "Pending"), ] # Filters by pending status
print(amyloid_pet_pending)

## Tau PET
### Pending
tau_pet_pending <- tau_pet_remaining_clean[which(tau_pet_remaining_clean$`PET: Screening Result` == "Pending"), ] # Filters by pending status
print(tau_pet_pending)

# Final list --------------------------------------------------------------
## MRI
print(sort(unique(mri_pending$current_diagnosis_stub)))

mri_pending_final <- mri_pending %>%
  filter(participant_status == "Actively Followed") %>% # Filters by actively followed
  mutate(current_diagnosis_stub = case_when(current_diagnosis_stub == 3 ~ "Mild Cognitive Impairment (ClinicalDiagnosis)",
                                            current_diagnosis_stub == 5 ~ "Normal Control") # Change dx values to labels based on REDCap codebook
  )
print(mri_pending_final)

## Amyloid PET
print(sort(unique(amyloid_pet_pending$current_diagnosis_stub)))

amyloid_pet_pending_final <- amyloid_pet_pending %>%
  filter(participant_status == "Actively Followed") %>% # Filters by actively followed
  mutate(current_diagnosis_stub = case_when(current_diagnosis_stub == 1 ~ "Behavioral Variant Frontotemporal Dementia",
                                            current_diagnosis_stub == 2 ~ "OtherFrontotemporalDementia",
                                            current_diagnosis_stub == 3 ~ "Mild Cognitive Impairment (ClinicalDiagnosis)",
                                            current_diagnosis_stub == 4 ~ "Mild Cognitive Impairment (ResearchDiagnosis)",
                                            current_diagnosis_stub == 5 ~ "Normal Control",
                                            current_diagnosis_stub == 8 ~ "Other",
                                            current_diagnosis_stub == 14 ~ "Primary Progressive Aphasia")
         ) # Change dx values to labels based on REDCap codebook
print(amyloid_pet_pending_final)

## Tau PET
print(sort(unique(tau_pet_pending$current_diagnosis_stub)))

tau_pet_pending_final <- tau_pet_pending %>%
  filter(participant_status == "Actively Followed") %>% # Filters by actively followed
  mutate(current_diagnosis_stub = case_when(current_diagnosis_stub == 1 ~ "Behavioral Variant Frontotemporal Dementia",
                                            current_diagnosis_stub == 2 ~ "OtherFrontotemporalDementia",
                                            current_diagnosis_stub == 3 ~ "Mild Cognitive Impairment (ClinicalDiagnosis)",
                                            current_diagnosis_stub == 4 ~ "Mild Cognitive Impairment (ResearchDiagnosis)",
                                            current_diagnosis_stub == 5 ~ "Normal Control",
                                            current_diagnosis_stub == 8 ~ "Other",
                                            current_diagnosis_stub == 14 ~ "Primary Progressive Aphasia")
  ) # Change dx values to labels based on REDCap codebook
print(tau_pet_pending_final)

# Eligibility Tables ------------------------------------------------------------------
## MRI
mri_pending_total <- data.frame(number_of_pending=length(mri_pending_final$`Ripple Global ID`))
print(mri_pending_total)

mri_pending_dx <- as.data.frame(table(mri_pending_final$current_diagnosis_stub))
colnames(mri_pending_dx)[which(names(mri_pending_dx) == "Var1")] <- "Current_diagnosis"
print(mri_pending_dx)

## Amyloid
amyloid_pet_pending_total <- data.frame(number_of_pending=length(amyloid_pet_pending_final$`Ripple Global ID`)) 
print(amyloid_pet_pending_total)

amyloid_pet_pending_dx <- as.data.frame(table(amyloid_pet_pending_final$current_diagnosis_stub))
colnames(amyloid_pet_pending_dx)[which(names(amyloid_pet_pending_dx) == "Var1")] <- "Current_diagnosis"
print(amyloid_pet_pending_dx)

## Tau
tau_pet_pending_total <- data.frame(number_of_pending=length(tau_pet_pending_final$`Ripple Global ID`))
print(tau_pet_pending_total)

tau_pet_pending_dx <- as.data.frame(table(tau_pet_pending_final$current_diagnosis_stub))
colnames(tau_pet_pending_dx)[which(names(tau_pet_pending_dx) == "Var1")] <- "Current_diagnosis"
print(tau_pet_pending_dx)

# PPA ---------------------------------------------------------------------
## MRI
mri_pending_final_ppa <- mri_pending_final %>%
  filter(current_diagnosis_stub == "Primary Progressive Aphasia")
print(mri_pending_final_ppa)

mri_pending_final_ppa_total <- data.frame(number_of_ppa_with_no_mri_scan=length(mri_pending_final_ppa$`Ripple Global ID`))
print(mri_pending_final_ppa_total)
mri_pending_final_ppa_id <- data.frame(global_id = mri_pending_final_ppa$`Ripple Global ID`, 
                                       ptid = mri_pending_final_ppa$ptid, 
                                       subject_id = mri_pending_final_ppa$case_num_ppa)
print(mri_pending_final_ppa_id)

## aPET
amyloid_pet_pending_final_ppa <- amyloid_pet_pending_final %>%
  filter(current_diagnosis_stub == "Primary Progressive Aphasia")
print(amyloid_pet_pending_final_ppa)

amyloid_pet_pending_final_ppa_total <- data.frame(number_of_ppa_with_no_amyloid_pet_scan=length(amyloid_pet_pending_final_ppa$`Ripple Global ID`))
print(amyloid_pet_pending_final_ppa_total)
amyloid_pet_pending_final_ppa_id <- data.frame(global_id = amyloid_pet_pending_final_ppa$`Ripple Global ID`, 
                                               ptid = amyloid_pet_pending_final_ppa$ptid, 
                                               subject_id = amyloid_pet_pending_final_ppa$case_num_ppa,
                                               modality_amyloid = c("amyloid"))
print(amyloid_pet_pending_final_ppa_id)

## tPET
tau_pet_pending_final_ppa <- tau_pet_pending_final %>%
  filter(current_diagnosis_stub == "Primary Progressive Aphasia")
print(tau_pet_pending_final_ppa)

tau_pet_pending_final_ppa_total <- data.frame(number_of_ppa_with_no_tau_pet_scan=length(tau_pet_pending_final_ppa$`Ripple Global ID`))
print(tau_pet_pending_final_ppa_total)
tau_pet_pending_final_ppa_id <- data.frame(global_id = tau_pet_pending_final_ppa$`Ripple Global ID`,
                                           ptid = tau_pet_pending_final_ppa$ptid, 
                                           subject_id = tau_pet_pending_final_ppa$case_num_ppa,
                                           modality_tau = c("tau"))
print(tau_pet_pending_final_ppa_id)

## Final table
table_1 <- data.frame(has_not_been_scanned=sum(mri_pending_final_ppa_total, amyloid_pet_pending_final_ppa_total, tau_pet_pending_final_ppa_total))
table_1

pending_ppa_id_list <- list(mri_pending_final_ppa_id,amyloid_pet_pending_final_ppa_id, tau_pet_pending_final_ppa_id)
pending_ppa_id <- pending_ppa_id_list %>% 
  reduce(full_join, by=c("global_id", "ptid", "subject_id"))
print(pending_ppa_id)

# Elderly Controls (SA & non-SA) ----------------------------------------------------------------------
## MRI
mri_pending_final_elderly_nc <- mri_pending_final %>%
  filter(age_dummy == 1,
         current_diagnosis_stub == "Normal Control") # Filter by dummy variable & nc dx
print(mri_pending_final_elderly_nc)

mri_pending_final_elderly_nc_total <- data.frame(number_of_elderly_nc_with_no_mri_scan=length(mri_pending_final_elderly_nc$`Ripple Global ID`))
print(mri_pending_final_elderly_nc_total)

mri_pending_final_elderly_nc_id <- data.frame(global_id = mri_pending_final_elderly_nc$`Ripple Global ID`, 
                                      ptid = mri_pending_final_elderly_nc$ptid, 
                                      modality_mri = c("mri"))
print(mri_pending_final_elderly_nc_id)

## aPET
amyloid_pet_pending_final_elderly_nc <- amyloid_pet_pending_final %>%
  filter(age_dummy == 1,
         current_diagnosis_stub == "Normal Control")
print(amyloid_pet_pending_final_elderly_nc)

amyloid_pet_pending_final_elderly_nc_total <- data.frame(number_of_elderly_nc_with_no_apet_scan=length(amyloid_pet_pending_final_elderly_nc$`Ripple Global ID`))
print(amyloid_pet_pending_final_elderly_nc_total)

amyloid_pet_pending_final_elderly_nc_id <- data.frame(global_id = amyloid_pet_pending_final_elderly_nc$`Ripple Global ID`, 
                                              ptid = amyloid_pet_pending_final_elderly_nc$ptid,
                                              modality_amyloid = c("amyloid"))
print(amyloid_pet_pending_final_elderly_nc_id)

## tPET
tau_pet_pending_final_elderly_nc <- tau_pet_pending_final %>%
  filter(age_dummy == 1,
         current_diagnosis_stub == "Normal Control") # Filter by age dummy and nc dx
print(tau_pet_pending_final_elderly_nc)

tau_pet_pending_final_elderly_nc_total <- data.frame(number_of_elderly_nc_with_no_tpet_scan=length(tau_pet_pending_final_elderly_nc$`Ripple Global ID`))
print(tau_pet_pending_final_elderly_nc_total)

tau_pet_pending_final_elderly_nc_id <- data.frame(global_id = tau_pet_pending_final_elderly_nc$`Ripple Global ID`, 
                                          ptid = tau_pet_pending_final_elderly_nc$ptid,
                                          modality_tau = c("tau"))
print(tau_pet_pending_final_elderly_nc_id)

## Final table
table_2 <- data.frame(has_not_been_scanned=sum(mri_pending_final_elderly_nc_total, 
                                               amyloid_pet_pending_final_elderly_nc_total, 
                                               tau_pet_pending_final_elderly_nc_total))
print(table_2)

pending_elderly_nc_id_list <- list(mri_pending_final_elderly_nc_id,
                           amyloid_pet_pending_final_elderly_nc_id, 
                           tau_pet_pending_final_elderly_nc_id)

pending_elderly_nc_id <- pending_elderly_nc_id_list %>%
  reduce(full_join, by=c("global_id", "ptid"))
print(pending_elderly_nc_id)

# Middle Age Controls -----------------------------------------------------
## MRI
mri_pending_final_middle_age_nc <- mri_pending_final %>%
  filter(current_diagnosis_stub == "Normal Control", 
         age_dummy == 0) # Filter by current age under 80 & normal control dx
print(mri_pending_final_middle_age_nc)

mri_pending_final_middle_age_nc_total <- data.frame(number_of_middle_age_nc_with_no_mri_scan=length(mri_pending_final_middle_age_nc$`Ripple Global ID`))
print(mri_pending_final_middle_age_nc_total)

mri_pending_final_middle_age_nc_id <- data.frame(global_id = mri_pending_final_middle_age_nc$`Ripple Global ID`, 
                                      ptid = mri_pending_final_middle_age_nc$ptid)
print(mri_pending_final_middle_age_nc_id)

## aPET
amyloid_pet_pending_final_middle_age_nc <- amyloid_pet_pending_final %>%
  filter(current_diagnosis_stub == "Normal Control", 
         age_dummy == 0) # Filter by current age under 80 & normal control dx
print(amyloid_pet_pending_final_middle_age_nc)

amyloid_pet_pending_final_middle_age_nc_total <- data.frame(number_of_middle_age_nc_with_no_apet_scan=length(amyloid_pet_pending_final_middle_age_nc$`Ripple Global ID`))
print(amyloid_pet_pending_final_middle_age_nc_total)

amyloid_pet_pending_final_middle_age_nc_id <- data.frame(global_id = amyloid_pet_pending_final_middle_age_nc$`Ripple Global ID`,
                                                         ptid = amyloid_pet_pending_final_middle_age_nc$ptid,
                                                         modality_amyloid = "amyloid")
print(amyloid_pet_pending_final_middle_age_nc_id)

## tPET
tau_pet_pending_final_middle_age_nc <- tau_pet_pending_final %>%
  filter(current_diagnosis_stub == "Normal Control", 
         age_dummy == 0) # Filter by current age under 80 & normal control dx
print(tau_pet_pending_final_middle_age_nc)

tau_pet_pending_final_middle_age_nc_total <- data.frame(number_of_middle_age_nc_with_no_tpet_scan=length(tau_pet_pending_final_middle_age_nc$`Ripple Global ID`))
print(tau_pet_pending_final_middle_age_nc_total)

tau_pet_pending_final_middle_age_nc_id <- data.frame(global_id = tau_pet_pending_final_middle_age_nc$`Ripple Global ID`,
                                                     ptid = tau_pet_pending_final_middle_age_nc$ptid,
                                                     modality_tau = "tau")
print(tau_pet_pending_final_middle_age_nc_id)

## Final table
table_3 <- data.frame(has_not_been_scanned=sum(mri_pending_final_middle_age_nc_total, amyloid_pet_pending_final_middle_age_nc_total, tau_pet_pending_final_middle_age_nc_total))
table_3

pending_middle_age_nc_id_list <- list(mri_pending_final_middle_age_nc_id, amyloid_pet_pending_final_middle_age_nc_id, tau_pet_pending_final_middle_age_nc_id)

pending_middle_age_nc_id <- pending_middle_age_nc_id_list %>%
  reduce(full_join, by=c("global_id", "ptid"))
print(pending_middle_age_nc_id)

# Combine data tables -----------------------------------------------------
combined_tables <- rbind(table_1, table_2, table_3)
combined_tables <- combined_tables %>%
  mutate(Diagnosis = c("PPA", "Elderly NC", "Middle Age NC"), .before = has_not_been_scanned)
print(combined_tables)

# Export excel ------------------------------------------------------------
write_xlsx(combined_tables, "/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/tables/never_scanned.xlsx") # This will be used in longitudinal script
write_xlsx(pending_ppa_id, "/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/tables/ppa_pending.xlsx")
write_xlsx(pending_elderly_nc_id, "/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/tables/elderly_age_nc_pending.xlsx")
write_xlsx(pending_middle_age_nc_id, "/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/tables/middle_age_nc_pending.xlsx")

# Eligibility Graphs ------------------------------------------------------------------
## MRI
unique_dx_mri <- sort(unique(mri_pending_final$current_diagnosis_stub))
print(unique_dx_mri)
unique_dx_mri[[1]] <- "MCI (Clinical DX)"
print(unique_dx_mri)

ggplot(data=mri_pending_final, aes(x=factor(current_diagnosis_stub))) +
  geom_bar(width=0.7, fill = "steelblue", color = "black") +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  scale_x_discrete(labels= unique_dx_mri) +
  labs(title = "Current Diagnosis of Pending Eligibility Status for MRI Scans",
       x = "Diagnosis",
       y = "Count") +
  theme_bw()+
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## Amyloid 
unique_dx_apet <- sort(unique(amyloid_pet_pending_final$current_diagnosis_stub))
print(unique_dx_apet)
unique_dx_apet[[1]] <- "bvFTD"
unique_dx_apet[[4]] <- "Other FTD"
unique_dx_apet[[5]] <- "PPA"
print(unique_dx_apet)

ggplot(data=amyloid_pet_pending_final, aes(x=factor(current_diagnosis_stub))) +
  geom_bar(width=0.7, fill = "steelblue", color = "black") +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  scale_x_discrete(labels= unique_dx_apet) +
  labs(title = "Current Diagnosis of Pending Eligibility Status for Amyloid PET Scans",
       x = "Diagnosis",
       y = "Count") +
  theme_bw()+
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## Tau 
unique_dx_tpet <- sort(unique(tau_pet_pending_final$current_diagnosis_stub))
print(unique_dx_tpet)
unique_dx_tpet[[1]] <- "bvFTD"
unique_dx_tpet[[2]] <- "MCI (Clinical DX)"
unique_dx_tpet[[3]] <- "MCI (Research DX)"
unique_dx_tpet[[6]] <- "Other FTD"
unique_dx_tpet[[7]] <- "PPA"
print(unique_dx_tpet)

ggplot(data=tau_pet_pending_final, aes(x=factor(current_diagnosis_stub))) +
  geom_bar(width=0.7, fill = "steelblue", color = "black") +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  scale_x_discrete(labels= unique_dx_tpet) +
  labs(title = "Current Diagnosis of Pending Eligibility Status for Tau PET Scans",
       x = "Diagnosis",
       y = "Count") +
  theme_bw()+
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

# Mesulam Center tables & graphs ----------------------------------------------------
## MRI
mri_all_scans_unqiue <- mri_all_scans[!duplicated(mri_all_scans$`Ripple Global ID`), ]
print(mri_all_scans_unqiue)

mri_all_scans_unique_total <- data.frame(all_MR_scans=length(mri_all_scans_unqiue$`Ripple Global ID`))
print(mri_all_scans_unique_total)

mri_all_scans_unique_study_affiliation <- as.data.frame(table(mri_all_scans_unqiue$`MRI Study Affiliation  Please check all that apply.`))
colnames(mri_all_scans_unique_study_affiliation)[which(names(mri_all_scans_unique_study_affiliation) == "Var1")] <- "Study_Affiliation"
print(mri_all_scans_unique_study_affiliation)

ggplot(data = mri_all_scans_unqiue, aes(x = factor(`MRI Study Affiliation  Please check all that apply.`))) +
  geom_bar(width=0.7, fill = "lightblue", color = "black") +
  coord_flip() +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Study Affiliation of All Core MR Scans",
       x = "Study Affiliation",
       y = "Count") +
  theme_bw()+
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## Amyloid
amyloid_pet_all_scans_unqiue <- amyloid_pet_all_scans[!duplicated(amyloid_pet_all_scans$`Ripple Global ID`), ]
print(amyloid_pet_all_scans_unqiue)

amyloid_pet_all_scans_unique_total <- data.frame(all_amyloid_pet_scans=length(amyloid_pet_all_scans_unqiue$`Ripple Global ID`))
print(amyloid_pet_all_scans_unique_total)

amyloid_pet_all_scans_unique_study_affiliation <- as.data.frame(table(amyloid_pet_all_scans_unqiue$`Amyloid PET Study Affiliation  Please check all that apply.`))
colnames(amyloid_pet_all_scans_unique_study_affiliation)[which(names(amyloid_pet_all_scans_unique_study_affiliation) == "Var1")] <- "Study_Affiliation"
print(amyloid_pet_all_scans_unique_study_affiliation)

ggplot(data = amyloid_pet_all_scans_unqiue, aes(x = factor(`Amyloid PET Study Affiliation  Please check all that apply.`))) +
  geom_bar(width=0.7, fill = "lightblue", color = "black") +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Study Affiliation of All Core Amyloid PET Scans",
       x = "Study Affiliation",
       y = "Count") +
  theme_bw()+
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## Tau
tau_pet_all_scans_unqiue <- tau_pet_all_scans[!duplicated(tau_pet_all_scans$`Ripple Global ID`), ]
print(tau_pet_all_scans_unqiue)

tau_pet_all_scans_unique_total <- data.frame(all_tau_pet_scans=length(tau_pet_all_scans_unqiue$`Ripple Global ID`))
print(tau_pet_all_scans_unique_total)

tau_pet_all_scans_unique_study_affiliation <- as.data.frame(table(tau_pet_all_scans_unqiue$`Tau PET Study Affiliation  Please check all that apply.`))
colnames(tau_pet_all_scans_unique_study_affiliation)[which(names(tau_pet_all_scans_unique_study_affiliation) == "Var1")] <- "Study_Affiliation"
print(tau_pet_all_scans_unique_study_affiliation)

ggplot(data = tau_pet_all_scans_unqiue, aes(x = factor(`Tau PET Study Affiliation  Please check all that apply.`))) +
  geom_bar(width=0.7, fill = "lightblue", color = ) +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Study Affiliation of All Core Tau PET Scans",
       x = "Study Affiliation",
       y = "Count") +
  theme_bw()+
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

# IC scan tables & plots --------------------------------------------------
## IC
mastersheet <- read_excel("/Volumes/fsmresfiles/CNADC/Imaging_Core/imaging_core_docs/Recruitment/IC Goals/Y4/imaging_diagnosis_mastersheet_2024-06-10.xlsx")
mastersheet_ic <- mastersheet[mastersheet$study_affiliation == "ADRC_IC", ]

mastersheet_ic_total <- data.frame(table(mastersheet_ic$modality))
colnames(mastersheet_ic_total)[which(names(mastersheet_ic_total) == "Var1")] <- "Modality"
print(mastersheet_ic_total)

ggplot(data = mastersheet_ic, aes(x = factor(modality))) +
  geom_bar(width=0.7, fill = "steelblue", color = "black") +
  scale_x_discrete(labels=c("Amyloid", "MRI", "Tau")) +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Cumulative Imaging Core Scans By Modality",
       x = "Modality",
       y = "Count") +
  theme_bw()+
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## MRI
mri_ic_scans_all <- mastersheet[mastersheet$modality == "mri" & mastersheet$study_affiliation == "ADRC_IC", ]
print(mri_ic_scans_all)

mri_ic_scans_total <- data.frame(total_IC_MR_scans=length(mri_ic_scans_all$global_id))
print(mri_ic_scans_total)

mri_ic_scans_dx <- as.data.frame(table(mri_ic_scans_all$diagnosis_time_of_visit))
colnames(mri_ic_scans_dx)[which(names(mri_ic_scans_dx) == "Var1")] <- "DX_at_scan"
print(mri_ic_scans_dx)

mri_ic_scans_unqiue <- mri_ic_scans_all[!duplicated(mri_ic_scans_all$global_id), ]
print(mri_ic_scans_unqiue)

mri_ic_scans_unique_total <- data.frame(unique_IC_scans=length(mri_ic_scans_unqiue$global_id))
print(mri_ic_scans_unique_total)

unique_mr_ic_dx <- sort(unique(mri_ic_scans_all$diagnosis_time_of_visit))
print(unique_mr_ic_dx)
unique_mr_ic_dx[[1]] <- "AD"
print(unique_mr_ic_dx)

ggplot(data = mri_ic_scans_all, aes(x = factor(mri_adrc_visitid), fill = diagnosis_time_of_visit)) +
  geom_bar(width=0.7, color = "black") +
  coord_flip() +
  scale_fill_discrete("Diagnosis At Scan", labels = unique_mr_ic_dx) +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Cumulative Imaging Core MRI Scans By Diagnosis At Scan & MR Instance",
       x = "MR Instance",
       y = "Count") +
  theme_bw()+
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## Amyloid
amyloid_pet_ic_scans_all <- mastersheet[mastersheet$modality == "amyloid" & mastersheet$study_affiliation == "ADRC_IC", ]
print(amyloid_pet_ic_scans_all)

amyloid_pet_ic_scans_total <- data.frame(total_IC_scans=length(amyloid_pet_ic_scans_all$global_id))
print(amyloid_pet_ic_scans_total)

amyloid_pet_ic_scans_dx <- as.data.frame(table(amyloid_pet_ic_scans_all$diagnosis_time_of_visit))
colnames(amyloid_pet_ic_scans_dx)[which(names(amyloid_pet_ic_scans_dx) == "Var1")] <- "DX_at_scan"
print(amyloid_pet_ic_scans_dx)

amyloid_pet_ic_scans_unqiue <- amyloid_pet_ic_scans_all[!duplicated(amyloid_pet_ic_scans_all$global_id), ]
print(amyloid_pet_ic_scans_unqiue)

amyloid_pet_ic_scans_unqiue_total <- data.frame(unique_IC_scans=length(amyloid_pet_ic_scans_unqiue$global_id))
print(amyloid_pet_ic_scans_unqiue_total)

unique_apet_ic_dx <- sort(unique(amyloid_pet_ic_scans_all$diagnosis_time_of_visit))
print(unique_apet_ic_dx)
unique_apet_ic_dx[[1]] <- "AD"
print(unique_apet_ic_dx)

ggplot(data = amyloid_pet_ic_scans_all, aes(x = factor(diagnosis_time_of_visit))) +
  geom_bar(width=0.7, fill = "steelblue", color = "black") +
  scale_x_discrete(labels= unique_apet_ic_dx) +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Cumulative Imaging Core Amyloid PET Scans By Diagnosis At Scan",
       x = "Diagnosis At Scan",
       y = "Count") +
  theme_bw()+
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

## Tau
tau_pet_ic_scans_all <- mastersheet[mastersheet$modality == "tau" & mastersheet$study_affiliation == "ADRC_IC", ]
print(tau_pet_ic_scans_all)

tau_pet_ic_scans_total <- data.frame(total_IC_tau_pet_scans=length(tau_pet_ic_scans_all$global_id))
print(tau_pet_ic_scans_total)

tau_pet_ic_scans_dx <- as.data.frame(table(tau_pet_ic_scans_all$diagnosis_time_of_visit))
colnames(tau_pet_ic_scans_dx)[which(names(tau_pet_ic_scans_dx) == "Var1")] <- "DX_at_scan"
print(tau_pet_ic_scans_dx)

tau_pet_ic_scans_unqiue <- tau_pet_ic_scans_all[!duplicated(tau_pet_ic_scans_all$global_id), ]
print(tau_pet_ic_scans_unqiue)

tau_ic_scans_unqiue_total <- data.frame(unique_IC_tau_pet_scans=length(tau_pet_ic_scans_unqiue$global_id))
print(tau_ic_scans_unqiue_total)

unique_tpet_ic_dx <- sort(unique(tau_pet_ic_scans_all$diagnosis_time_of_visit))
print(unique_tpet_ic_dx)
unique_tpet_ic_dx[[1]] <- "AD"
print(unique_tpet_ic_dx)

ggplot(data = tau_pet_ic_scans_all, aes(x = factor(diagnosis_time_of_visit))) +
  geom_bar(width=0.7, fill = "steelblue", color = "black") +
  scale_x_discrete(labels= unique_tpet_ic_dx) +
  geom_text(aes(label = after_stat(count)), stat = "count", color = "black", position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Cumulative Imaging Core Tau PET Scans By Diagnosis At Scan",
       x = "Diagnosis At Scan",
       y = "Count") +
  theme_bw()+
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

