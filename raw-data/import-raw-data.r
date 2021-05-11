
ProjectTemplate::reload.project(list(munging = FALSE, data_loading = FALSE))

memory.limit(size = 10000000000000)

# CRT survey --------------------------------------------------------------

pathcrt <- "C:/Users/Lina/STATISTIK/Projects/CRT Survey II/data"

crtprocedure <- read_sas(
  paste0(pathcrt, "/procedure_data.sas7bdat"),
  paste0(pathcrt, "/formats.sas7bcat")
)
crtprocedure <- crtprocedure %>%
  mutate(across(where(is.labelled), haven::as_factor))

crtpatient <- read_sas(
  paste0(pathcrt, "/patient_data.sas7bdat"),
  paste0(pathcrt, "/formats.sas7bcat")
)
crtpatient <- crtpatient %>%
  mutate(across(where(is.labelled), haven::as_factor))
crtcontents <- read_sas(paste0(pathcrt, "/contents_data.sas7bdat"))
crtcontentstable <- read_sas(paste0(pathcrt, "/contents_table.sas7bdat"))

# Store as RData in /data folder ------------------------------------------

save(file = "./data/crtsurvey.RData", list = c("crtpatient", "crtprocedure", "crtcontents", "crtcontentstable"))


# ESC ---------------------------------------------------------------------

pathesc <- "C:/Users/Lina/STATISTIK/Projects/ESC registry/data"

esc <- read_sas(
  paste0(pathesc, "/hf3_lt_fu_data_soladis_jan19.sas7bdat"),
  paste0(pathesc, "/formats.sas7bcat")
)


# Fix labels ect. ---------------------------------------------------------

esc <- esc %>%
  mutate(across(where(is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), ~ droplevels(., exclude = "Unknown"))) %>%
  mutate(across(where(is.factor), factor)) %>% ## Something odd with num_opRyth (two diff kinds of NA). This fixes it
  mutate(num_dmHepa = recode(num_dmHepa, "A" = "Yes", .default = levels(num_dmHepa))) # uses format for other hep variable. this fixes it

# Store as RData in /data folder ------------------------------------------

save(file = "./data/esc.RData", list = c("esc"))

# UCR data for centre -----------------------------------------------------

ucrpath <- "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/raw-data/UCR/"

# oldvals <- read_sas(paste0(ucrpath, "dat183_formats_old.sas7bdat"))

newrs <- read_sasdata(path = ucrpath, filename = "lb_ucr_lev_new_dat183_export")
oldrs <- read_sasdata(path = ucrpath, filename = "lb_ucr_lev_old_dat183_bas_ej")

## corr from UCR containing correct loop, arni, betablocker doses x 2 and variable migrated
newrsadd <- read_sasdata(path = ucrpath, filename = "new_data")

# two duplicate post need to be deleted
newrs <- newrs %>% filter(!(alt_patientreference %in%
  c(
    "64F86FD9906AE0A5F16EDB901B455C8E",
    "9550A7F16D30E37C9805209725ED411A"
  )
& TYPE == "INDEX" & LOCATION == "IX_SV"))

newrs <- left_join(newrs %>%
  select(
    -ACE_DOSE_CILAZAPRIL,
    -INOTROPE_SUPPORT,
    -L_ARNI_DOSE,
    -L_BETA_DOSE_BETAXOLOL,
    -L_BETA_DOSE_TIMOLOL,
    -L_LOOP_DIUR_DOSE
  ),
newrsadd %>% mutate(TYPE = "INDEX"),
by = c("alt_patientreference", "TYPE")
) %>%
  mutate(migrated = replace_na(migrated, 0))

# Store as RData in /data folder ------------------------------------------

save(file = "./data/rawrs.RData", list = c("newrs", "oldrs"))
