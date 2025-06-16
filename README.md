# ✅ Imaging Core Goals Y4: Eligibility Report

This script supports the Year 4 Imaging Core goals by determining participant eligibility for PET/MRI scans based on cognitive status, demographics, and protocol adherence. The script merges clinical data with site-tracked stub visit details to monitor recruitment targets and scan completion rates.

---

## 📘 Script Summary

### 📂 `IC_goals_Y4.R`

Performs the following tasks:
- Merges REDCap export of clinical data with stub visit information
- Flags participants who are eligible, pending, or ineligible
- Outputs summary tables by eligibility status and visit phase
- Supports internal PET/MRI tracking reports

---

## 📁 Required Data Files (Internal Access Only)

> 🔒 These files are located in a **shared lab directory**. To run the script, place them in a local `data/` folder.

| Filename                                                                  | Description                                     |
|---------------------------------------------------------------------------|-------------------------------------------------|
| `AlzheimersDiseaseRes-StubDetails_DATA_2024-06-28_1029.csv`              | Demographic information |
| `2024-06-28T15_28_21.921Z_clinical_core_export.csv`                      | Clinical Core REDCap export with key fields     |

---

## ▶️ How to Run

1. Ensure both data files are placed in a `data/` folder at the root of the repository.
2. Open the R script in RStudio: `IC_goals_Y4.R`
3. Install the necessary R packages (if not already):
   ```r
   install.packages(c("readr", "dplyr", "stringr", "lubridate"))
   ```
