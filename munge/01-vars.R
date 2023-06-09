tabvars <- c(
  "sex",
  "age",
  "age_cat",
  "primaryetiology",
  "mi",
  "revascularisation",
  "hypertension",
  "af",
  "copd",
  "diabetes",
  "anemia",
  "ckd",
  "cancer",
  "hfhosp1yr",
  "device",
  "crt_d",
  "crt_p",
  "pmicd",
  "nodevice",
  "nyha",
  "nyha_cat",
  "bmi",
  "bmi_cat",
  "bpsys",
  "bpsys_cat",
  "bpdia",
  "bnp",
  "ntprobnp",
  "ntprobnp_cat",

  "creatinine",
  "gfrckdepi2021",
  "heartrate",
  "heartrate_cat",
  "hb",
  "ecg",
  #  "printernval",
  #  "avblock",
  "H_ECG_PMDEP_YN",
  "lbbb",
  "qrs",
  "qrs_cat",
  "ef",
  "ef_cat35",
  "ef_cat39",

  "lvedd",
  "mitreg_modsev",
  "loopdiuretic",
  "rasarni",
  "mra",
  "bbl",
  "digoxin",
  "anticoagulantia",
  "asaantiplatelet",

  "smoking_cat",
  "location",
  "famtype",
  "hospitaltype",
  "Implanting.CRT",
  "HFclinicavailablefollowup",
  "esc_hfpatstreated",
  "num_sqPaceYear", 
  "num_sqICDYear", 

  # "country",
  # "num_dmEthnic",
  # "scb_countryofbirth", denna gör så tab1 strular me merge
  "scb_dispincome_cat2",
  "scb_education_cat",
  "scb_child",
  "followuphfunit",
  "shf_followuplocation",

  "maggic_score"
)

modvars_esc <- c(
  "age_cat",
  "sex",

  "famtype",
  "bmi_cat",

  "ckd",
  "cancer",
  "af",
  "mi",

  "hfhosp1yr",
  "nyha_cat",
  "ef_cat39",
  "ntprobnp_cat",

  "bbl",
  "rasarni",
  "mra",
  "loopdiuretic",

  "heartrate_cat",
  
  "followuphfunit",
  "hospitaltype", 
  "Implanting.CRT"
)

modvars_rs <- c(
  "age_cat",
  "sex",

  "famtype",
  "scb_child",
  "scb_education_cat",
  "scb_dispincome_cat2",
  "smoking_cat",
  "bmi_cat",

  "diabetes",
  "ckd",
  "anemia",
  "cancer",
  "af",
  "mi",
  "copd",
  "hypertension",

  "hfhosp1yr",
  "nyha_cat",
  "ef_cat39",
  "ntprobnp_cat",

  "bbl",
  "rasarni",
  "mra",
  "loopdiuretic",
  "digoxin",
  "anticoagulantia",
  "asaantiplatelet",

  "bpsys_cat",
  "heartrate_cat",

  "followuphfunit",
  "shf_followuplocation_cat",
  "hospitaltype",
  "Implanting.CRT"
)
