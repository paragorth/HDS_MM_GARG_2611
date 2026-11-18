# 01_build_dataset.R
# Builds the analysis-ready dataset (ARD): adults aged 40+ with diabetes,
# NHANES 1999-2004, with a sensitive "at-risk foot" outcome for podiatry screening.
#
# Structure follows the cohort method taught in Module 1:
#   DEFINE  - the cohort as explicit rules
#   ANCHOR  - the time point (cross-sectional: the exam visit)
#   INSPECT - the grain: one row per person; joins must not drop or duplicate people
#   AUDIT   - attrition at every rule, and a log of every cleaning decision
#
# Outputs: data_ard/foot_risk_ard.rds and .csv, data_ard/attrition.csv,
#          data_ard/cleaning_log.csv

pkgs <- c("haven", "dplyr", "purrr", "tibble")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({ library(haven); library(dplyr); library(purrr); library(tibble) })

dir.create("data_raw", showWarnings = FALSE)
dir.create("data_ard", showWarnings = FALSE)

# ---- Source files ---------------------------------------------------------
# Three survey cycles ran the lower-extremity exam. File names differ by cycle.
cycles <- tibble(
  year   = c(1999, 2001, 2003),
  suffix = c("", "_B", "_C"),
  abi    = c("LEXABPI", "LEXAB_B", "LEXAB_C"),
  hba1c  = c("LAB10", "L10_B", "L10_C")
)

get_xpt <- function(name, year) {
  f <- file.path("data_raw", paste0(name, ".xpt"))
  if (!file.exists(f)) {
    url <- sprintf("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/%d/DataFiles/%s.xpt", year, name)
    download.file(url, f, mode = "wb")
  }
  read_xpt(f)
}

need <- function(df, vars, label) {
  miss <- setdiff(vars, names(df))
  if (length(miss) > 0) stop(label, " is missing: ", paste(miss, collapse = ", "))
  df
}

# Grain check: every source table must have one row per person
one_per_person <- function(df, label) {
  if (anyDuplicated(df$SEQN)) stop(label, ": SEQN is not unique")
  df
}

reason_pattern <- "^LEA(L|R)(M1|HA|M5)C[123]$"   # why a monofilament site was not tested

load_cycle <- function(year, suffix, abi, hba1c) {
  demo <- get_xpt(paste0("DEMO", suffix), year) %>% one_per_person(paste("DEMO", year)) %>%
    select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, SDMVPSU, SDMVSTRA, any_of(c("WTMEC2YR", "WTMEC4YR")))
  diq <- get_xpt(paste0("DIQ", suffix), year) %>% one_per_person(paste("DIQ", year)) %>%
    need(c("DIQ010", "DIQ090"), paste("DIQ", year)) %>% select(SEQN, DIQ010, DIQ090)
  gh  <- get_xpt(hba1c, year) %>% one_per_person(paste(hba1c, year)) %>%
    need("LBXGH", paste(hba1c, year)) %>% select(SEQN, LBXGH)
  pn  <- get_xpt(paste0("LEXPN", suffix), year) %>% one_per_person(paste("LEXPN", year)) %>%
    need(c("LEALPN", "LEARPN"), paste("LEXPN", year)) %>%
    need(c("LEALLES", "LEARLES"), paste("LEXPN", year)) %>%
    select(SEQN, LEALPN, LEARPN, LEALLES, LEARLES, LEALLESS, LEARLESS, matches(reason_pattern))
  ab  <- get_xpt(abi, year) %>% one_per_person(paste(abi, year)) %>%
    need(c("LEXLABPI", "LEXRABPI"), paste(abi, year)) %>%
    mutate(across(-SEQN, as.numeric)) %>%
    select(SEQN, LEXLABPI, LEXRABPI,
           any_of(c("LEDSCCT2", "LEALPTPD", "LEARPTPD", "LEALAPNC", "LEARAPNC")))  # why missing (2001-04 only)

  # Left joins from demographics (everyone sampled). relationship = "one-to-one"
  # makes R stop if any join would duplicate a person.
  out <- demo %>%
    left_join(diq, by = "SEQN", relationship = "one-to-one") %>%
    left_join(gh,  by = "SEQN", relationship = "one-to-one") %>%
    left_join(pn,  by = "SEQN", relationship = "one-to-one") %>%
    left_join(ab,  by = "SEQN", relationship = "one-to-one") %>%
    mutate(cycle = year)
  stopifnot(nrow(out) == nrow(demo))   # joins neither dropped nor added anyone
  # Reverse check: every questionnaire, lab and exam record belongs to someone
  # in the demographics file, so no record is left behind by the joins
  orphans <- sum(sapply(list(diq, gh, pn, ab), function(x) sum(!x$SEQN %in% demo$SEQN)))
  stopifnot(orphans == 0)
  out
}

raw <- pmap_dfr(cycles, load_cycle)
stopifnot(!anyDuplicated(raw$SEQN))   # SEQN is unique across cycles too

# ---- Cleaning log -----------------------------------------------------------
log <- tibble(step = character(), variable = character(), decision = character(),
              reason = character(), n_affected = integer())
add_log <- function(step, variable, decision, reason, n) {
  log <<- add_row(log, step = step, variable = variable, decision = decision,
                  reason = reason, n_affected = as.integer(n))
}

# ---- Recode non-answers to missing (human-readable codes -> machine-readable) --
n <- sum(raw$DIQ010 %in% c(7, 9))
add_log("recode", "DIQ010", "refused (7) / don't know (9) set to missing",
        "not an answer; cannot count as diagnosed or not", n)
n <- sum(raw$DIQ090 %in% c(7, 9))
add_log("recode", "DIQ090", "refused (7) / don't know (9) set to missing",
        "not an answer", n)
raw <- raw %>% mutate(DIQ010 = if_else(DIQ010 %in% c(7, 9), NA_real_, DIQ010),
                      DIQ090 = if_else(DIQ090 %in% c(7, 9), NA_real_, DIQ090))

n <- sum(raw$LEALPN %in% -1) + sum(raw$LEARPN %in% -1)
add_log("recode", "LEALPN / LEARPN", "-1 (not enough information) set to missing",
        "a count of insensate sites cannot be negative; the code means 'not classifiable'", n)
raw <- raw %>% mutate(LEALPN = if_else(LEALPN %in% 0:3, LEALPN, NA_real_),
                      LEARPN = if_else(LEARPN %in% 0:3, LEARPN, NA_real_))

# ===== LAYER 1: LINKED =========================================================
# All sampled people, all source files joined and non-answers recoded. No one removed.
layer1 <- raw

# ---- Cohort rules kept as FLAGS, not filters ----------------------------------
# Every person keeps a flag for each rule, so another analyst can change a rule
# without rebuilding from the raw files.
raw <- raw %>% mutate(
  dm_diagnosed   = DIQ010 %in% 1,
  dm_undiagnosed = !dm_diagnosed & !is.na(LBXGH) & LBXGH >= 6.5,
  dm_source      = case_when(dm_diagnosed ~ "told by a doctor",
                             dm_undiagnosed ~ "HbA1c 6.5% or higher only"),
  flag_age40     = RIDAGEYR >= 40,
  flag_diabetes  = dm_diagnosed | dm_undiagnosed,
  flag_exam      = !is.na(WTMEC2YR) & WTMEC2YR > 0,
  in_cohort      = flag_age40 & flag_diabetes & flag_exam,
  # CDC rule for combining cycles: 1999-2002 use 2/3 of the 4-year exam weight,
  # 2003-04 uses 1/3 of its 2-year weight.
  wt = if_else(cycle %in% c(1999, 2001), 2/3 * WTMEC4YR, 1/3 * WTMEC2YR)
)

attrition <- tibble(
  rule = c("All sampled people, 1999-2004",
           "Aged 40 or over (foot exam only offered at 40+)",
           "Has diabetes: told by a doctor, or HbA1c 6.5% or higher",
           "Attended the physical exam (interview-only people could not be examined)"),
  n = c(nrow(raw), sum(raw$flag_age40),
        sum(raw$flag_age40 & raw$flag_diabetes), sum(raw$in_cohort)))

add_log("flag", "flag_exam", "interview-only participants flagged, not in cohort",
        "never examined, so no foot data; exam weight is zero",
        attrition$n[3] - attrition$n[4])
add_log("note", "RIDAGEYR", "age kept as recorded",
        "NHANES top-codes age at 85 in these cycles; true ages above 85 are not known",
        sum(raw$in_cohort & raw$RIDAGEYR >= 85))

# ===== LAYER 2: CURATED ========================================================
# Everyone who could have had the foot exam (aged 40+, attended the exam), with
# or without diabetes. Diabetes is a flag, so the same checks can be studied in
# people without diabetes too.
reason_cols <- grep(reason_pattern, names(raw), value = TRUE)

layer2 <- raw %>%
  filter(flag_age40, flag_exam) %>%
  mutate(
    untested_foot_reason  = if_any(all_of(reason_cols), ~ .x %in% 1:4),  # lesion/bandage/stocking, cast, amputation, callus
    untested_other_reason = if_any(all_of(reason_cols), ~ .x %in% 5:6),  # technical, other

    lops = case_when(
      LEALPN >= 2 | LEARPN >= 2    ~ 1,   # 2 or more of 3 sites numb on the same foot
      LEALPN <= 1 & LEARPN <= 1    ~ 0,
      untested_foot_reason         ~ 1,
      untested_other_reason        ~ 0,
      TRUE                         ~ NA_real_
    ),
    lops_source = case_when(
      LEALPN >= 2 | LEARPN >= 2 | (LEALPN <= 1 & LEARPN <= 1) ~ "measured",
      untested_foot_reason  ~ "assigned at risk: foot problem prevented test",
      untested_other_reason ~ "assigned normal: technical or other reason",
      TRUE                  ~ "unknown"
    ),

    # Recorded findings that stop the index being calculated: no ankle pulse
    # (suggests severe poor circulation) or ankle pressure above 255 mm Hg
    # (non-compressible, calcified arteries). Both are abnormal, not missing.
    abi_no_pulse = LEALPTPD %in% 2 | LEARPTPD %in% 2,
    abi_noncompressible = LEALAPNC %in% 1 | LEARAPNC %in% 1,
    abi_low   = as.numeric(LEXLABPI < 0.9 | LEXRABPI < 0.9),
    abi_stiff = as.numeric(LEXLABPI >= 1.3 | LEXRABPI >= 1.3),
    pad = case_when(
      LEXLABPI < 0.9 | LEXRABPI < 0.9 | LEXLABPI >= 1.3 | LEXRABPI >= 1.3 ~ 1,
      LEXLABPI >= 0.9 & LEXLABPI < 1.3 & LEXRABPI >= 0.9 & LEXRABPI < 1.3 ~ 0,
      abi_no_pulse | abi_noncompressible ~ 1,
      TRUE ~ NA_real_
    ),
    pad_source = case_when(
      LEXLABPI < 0.9 | LEXRABPI < 0.9 | LEXLABPI >= 1.3 | LEXRABPI >= 1.3 |
        (LEXLABPI >= 0.9 & LEXLABPI < 1.3 & LEXRABPI >= 0.9 & LEXRABPI < 1.3) ~ "measured",
      abi_no_pulse | abi_noncompressible ~ "abnormal: no pulse or pressure unmeasurable",
      TRUE ~ "unknown"
    ),
    abi_missing_reason = case_when(
      pad_source != "unknown" ~ NA_character_,
      xor(is.na(LEXLABPI), is.na(LEXRABPI)) ~ "one leg normal, other missing",
      LEDSCCT2 %in% 58 ~ "unable to obtain all pressures",
      LEDSCCT2 %in% c(1, 4, 7, 22, 43, 104) ~ "physical limitation, pain, swelling or illness",
      LEDSCCT2 %in% c(3, 56) ~ "came late, left early or no time",
      LEDSCCT2 %in% 2 ~ "refused",
      cycle == 1999 ~ "not recorded (1999-2000)",
      TRUE ~ "other or not recorded"
    ),

    ulcer_hx = case_when(DIQ090 == 1 ~ 1, DIQ090 == 2 ~ 0, TRUE ~ NA_real_),

    # Lesion seen on either foot at the exam (1 yes, 2 no, 8 could not obtain)
    lesion_now = case_when(
      LEALLES == 1 | LEARLES == 1 ~ 1,
      LEALLES == 2 & LEARLES == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # >>> REFERRAL RULE (shown in the report)
    # Cleared only if all four checks were done AND all were normal.
    # Any positive check, or any check not done, means refer.
    refer = if_else(lops %in% 0 & pad %in% 0 & ulcer_hx %in% 0 & lesion_now %in% 0, 0, 1),
    # <<< END REFERRAL RULE

    # Labels used for the tables and figure (not part of the rule itself)
    at_risk = case_when(
      lops == 1 | pad == 1 | ulcer_hx == 1 | lesion_now == 1 ~ 1,
      lops == 0 & pad == 0 & ulcer_hx == 0 & lesion_now == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    refer_reason = case_when(
      at_risk %in% 1 ~ "refer: positive finding",
      is.na(at_risk) ~ "refer: screen incomplete",
      TRUE           ~ "cleared: all checks normal"
    ),
    refer_main_reason = case_when(
      lesion_now %in% 1 ~ "lesion on foot now",
      lops %in% 1       ~ "loss of sensation",
      pad %in% 1        ~ "abnormal ankle-brachial index",
      ulcer_hx %in% 1   ~ "past ulcer only",
      refer == 1        ~ "screen incomplete",
      TRUE              ~ "cleared"
    ),

    # Alternative decisions, kept as flags so another analyst can choose differently
    alt_lops_1site = case_when(LEALPN >= 1 | LEARPN >= 1 ~ 1,
                               LEALPN == 0 & LEARPN == 0 ~ 0, TRUE ~ lops),
    alt_pad_low_only = case_when(LEXLABPI < 0.9 | LEXRABPI < 0.9 ~ 1,
                                 !is.na(LEXLABPI) & !is.na(LEXRABPI) ~ 0)
  )
stopifnot(!anyDuplicated(layer2$SEQN))

# ===== LAYER 3: ANALYSIS-READY =================================================
# People with diabetes, for the referral analysis.
cohort <- layer2 %>% filter(flag_diabetes)
stopifnot(nrow(cohort) == sum(raw$in_cohort), all(!is.na(cohort$wt) & cohort$wt > 0))

add_log("derive", "lops", "two or more of three sites insensate on the same foot = loss of sensation",
        "sensitive threshold for a screen", sum(cohort$lops_source == "measured" & cohort$lops == 1, na.rm = TRUE))
add_log("derive", "lops", "untested because of lesion, bandage, stocking, cast, amputation or callus = at risk",
        "a foot problem severe enough to stop the test is itself a risk", sum(grepl("assigned at risk", cohort$lops_source)))
add_log("derive", "lops", "untested for technical or other reason = normal",
        "no clinical reason to suspect risk", sum(grepl("assigned normal", cohort$lops_source)))
add_log("derive", "pad", "no ankle pulse, or ankle pressure above 255 mm Hg = abnormal",
        "recorded findings of disease that stop the index being calculated; not missing data",
        sum(cohort$pad_source == "abnormal: no pulse or pressure unmeasurable"))
add_log("derive", "pad", "ABI below 0.9 or 1.3 and above on either leg = abnormal",
        "low = poor circulation; high = stiff, calcified arteries common in diabetes", sum(cohort$pad %in% 1))
add_log("derive", "ulcer_hx", "past leg or foot sore taking over 4 weeks to heal = past ulcer",
        "question covers leg as well as foot; accepted over-count for a sensitive screen", sum(cohort$ulcer_hx %in% 1))
add_log("derive", "lesion_now", "lesion seen on either foot at the exam = positive",
        "current skin breakdown; matches podiatry inspection; survey does not say if it is an ulcer", sum(cohort$lesion_now %in% 1))
add_log("derive", "at_risk", "any one of four components positive = at risk; all four negative = not at risk; otherwise unknown",
        "sensitive combined screen for podiatry referral", sum(is.na(cohort$at_risk)))
add_log("derive", "refer", "cleared only if all four checks done and normal; positive or incomplete = refer",
        "a check that could not be completed cannot clear anyone; podiatry makes the real judgement",
        sum(cohort$refer == 1))

# ---- AUDIT: checks ------------------------------------------------------------
cat("\n== Attrition ==\n"); print(attrition, n = Inf)
cat("\n== How diabetes was identified ==\n"); print(table(cohort$dm_source))
cat("\n== How the sensation result was obtained ==\n"); print(table(cohort$lops_source))
cat("\n== Past ulcer, measured vs not measured sensation ==\n")
print(round(prop.table(table(measured = cohort$lops_source == "measured",
                             past_ulcer = cohort$ulcer_hx), 1), 3))
cat("\n== Missing per component ==\n")
print(colSums(is.na(cohort[, c("lops", "pad", "ulcer_hx", "lesion_now", "at_risk")])))
cat("\n== At-risk foot (unweighted) ==\n"); print(table(cohort$at_risk, useNA = "ifany"))
cat("\n== PRIMARY OUTCOME: podiatry referral ==\n"); print(table(cohort$refer_reason))
cat("Weighted % cleared:", round(100 * weighted.mean(cohort$refer == 0, cohort$wt), 1), "\n")
cat("\n== ABI range ==\n"); print(summary(c(cohort$LEXLABPI, cohort$LEXRABPI)))

# ---- Final analysis-ready dataset: one row per person, labelled ---------------
ard <- cohort %>%
  transmute(
    seqn = SEQN, cycle,
    age = RIDAGEYR,
    sex = factor(RIAGENDR, 1:2, c("Male", "Female")),
    ethnicity = factor(RIDRETH1, 1:5, c("Mexican American", "Other Hispanic",
                                        "Non-Hispanic White", "Non-Hispanic Black", "Other")),
    hba1c = LBXGH, dm_source,
    abi_left = LEXLABPI, abi_right = LEXRABPI,
    lops, lops_source, pad, abi_low, abi_stiff, ulcer_hx, lesion_now,
    at_risk, refer, refer_reason, refer_main_reason,
    pad_source, abi_missing_reason, alt_lops_1site, alt_pad_low_only,
    psu = SDMVPSU, strata = SDMVSTRA, weight = wt
  )
stopifnot(!anyDuplicated(ard$seqn), nrow(ard) == nrow(cohort))

dictionary <- tibble::tribble(
  ~variable, ~description, ~values, ~source,
  "seqn", "Participant identifier", "integer, unique", "DEMO: SEQN",
  "cycle", "Survey cycle start year", "1999, 2001, 2003", "derived",
  "age", "Age at interview, years (top-coded at 85)", "40-85", "DEMO: RIDAGEYR",
  "sex", "Sex", "Male, Female", "DEMO: RIAGENDR",
  "ethnicity", "Race/ethnicity (NHANES categories)", "5 categories", "DEMO: RIDRETH1",
  "hba1c", "Glycated haemoglobin, %", "numeric", "LAB10 / L10_B / L10_C: LBXGH",
  "dm_source", "How diabetes was identified", "told by a doctor; HbA1c 6.5% or higher only", "DIQ010, LBXGH",
  "abi_left / abi_right", "Ankle-brachial index per leg", "numeric", "LEXABPI / LEXAB_B / LEXAB_C",
  "lops", "Loss of protective sensation, either foot", "1 yes, 0 no, NA unknown (2+ of 3 sites on one foot)", "LEXPN: LEALPN, LEARPN, reason codes",
  "lops_source", "How the sensation result was obtained", "measured; assigned at risk; assigned normal; unknown", "derived",
  "pad", "Abnormal ankle-brachial index (<0.9 or >=1.3) either leg", "1 yes, 0 no, NA unknown", "derived",
  "pad_source", "How the ankle result was obtained", "measured; abnormal: no pulse or pressure unmeasurable; unknown", "derived from LEXAB: LEALPTPD, LEARPTPD, LEALAPNC, LEARAPNC",
  "abi_missing_reason", "Why the ankle result is unknown (blank if known)", "6 categories", "derived from LEXAB: LEDSCCT2",
  "abi_low / abi_stiff", "Low (<0.9) or stiff (>=1.3) index on either leg", "1 yes, 0 no, NA", "derived",
  "ulcer_hx", "Ever had leg or foot sore taking >4 weeks to heal", "1 yes, 0 no, NA", "DIQ: DIQ090",
  "lesion_now", "Lesion seen on either foot at exam", "1 yes, 0 no, NA", "LEXPN: LEALLES, LEARLES",
  "at_risk", "Any of the four checks positive", "1 yes, 0 all four normal, NA incomplete", "derived",
  "refer", "PRIMARY OUTCOME: refer to podiatry", "1 refer, 0 cleared", "derived",
  "refer_reason", "Positive, incomplete or cleared", "3 categories", "derived",
  "refer_main_reason", "Most urgent reason for referral (deep phenotype)", "6 categories", "derived",
  "alt_lops_1site", "Alternative decision: sensation loss from 1 numb site", "1 yes, 0 no, NA", "derived",
  "alt_pad_low_only", "Alternative decision: only low index (<0.9) counts as abnormal", "1 yes, 0 no, NA", "derived",
  "psu / strata / weight", "Survey design variables; weight combined for 1999-2004", "numeric", "DEMO; CDC combining rule"
)

# The label must match the box: every column documented, every documented name present
documented <- trimws(unlist(strsplit(dictionary$variable, "/")))
stopifnot(setequal(documented, names(ard)))

# ---- Save -------------------------------------------------------------------
saveRDS(layer1, "data_ard/layer1_linked.rds")            # all sampled people, files linked and recoded
saveRDS(layer2, "data_ard/layer2_curated.rds")           # aged 40+ and examined; diabetes as a flag; all checks derived
saveRDS(cohort, "data_ard/foot_risk_full.rds")           # layer 3 with all source columns, for audit
saveRDS(ard, "data_ard/foot_risk_ard.rds")              # the analysis-ready dataset
write.csv(ard, "data_ard/foot_risk_ard.csv", row.names = FALSE)
write.csv(dictionary, "data_ard/data_dictionary.csv", row.names = FALSE)
write.csv(attrition, "data_ard/attrition.csv", row.names = FALSE)
write.csv(log, "data_ard/cleaning_log.csv", row.names = FALSE)
cat("\n== Cleaning log ==\n"); print(as.data.frame(log))
