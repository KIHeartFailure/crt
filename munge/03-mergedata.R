
# Merge 3 datasets and create necessary variables -------------------------


# add on procedure data for crt

crtpatient <- left_join(crtpatient,
  crtprocedure %>%
    group_by(cor_pt_patient_id) %>%
    arrange(h_proc_implant_no) %>%
    slice(n()) %>%
    ungroup() %>%
    select(cor_pt_patient_id, H_PROC_DEVICE_C),
  by = "cor_pt_patient_id"
)

ynfunc <- function(var) {
  var <- recode_factor(var, `0` = "No", `1` = "Yes")
}
crtpatient <- crtpatient %>%
  mutate(across(
    c(
      H_MH_HFHOSP_YES_YN,
      H_ECG_AVBLOCK_YN,
      H_ECG_QRSLBBB_YN,
      H_BASE_INDHFQRS_YN,
      H_BASE_INDLVDICD_YN,
      H_BASE_INDRVPD_YN,
      H_BASE_INDMDYS_YN,
      H_BASE_INDOTH_YN,

      # meds
      HDC_MT_LOOPDIU_YN,
      HDC_MT_ACE_YN,
      HDC_MT_MRA_YN,
      HDC_MT_BETA_YN,
      HDC_MT_IVABR_YN,
      HDC_MT_DIG_YN,
      HDC_MT_CCBLOCK_YN,
      HDC_MT_AMIO_YN,
      HDC_MT_OTHANTARR_YN,
      HDC_MT_ORAANTCOA_YN,
      HDC_MT_ANTPL_MC_YN
    ),
    ynfunc
  ))

# add on site data for esc
esc <- left_join(
  esc,
  siteesc %>% select(
    CentreID, sqAdmin,
    sqHFCard,
    sqHFInMed,
    sqHFHFunit,
    sqHFclinic
  ),
  by = c("CID" = "CentreID")
)

data3 <- bind_rows(
  crtpatient %>%
    mutate(
      data = "crt",
      tmp_lopnr_crt = 1:n()
    ),
  esc %>%
    mutate(
      data = "esc",
      tmp_lopnr_esc = 1:n()
    ),
  rsdata323 %>%
    mutate(data = "rs") %>%
    filter(casecontrol == "Case")
)

data3 <- data3 %>%
  mutate(
    lopnr = coalesce(tmp_lopnr_crt, tmp_lopnr_esc, LopNr),

    tmp_hfduresc = case_when(
      num_dmMonth %in% c("6 - 12 months", "> 12 months") ~ ">6mo",
      num_dmMonth %in% c("< 6 months") ~ "<6mo"
    ),

    hfdur = coalesce(tmp_hfduresc, shf_durationhf),

    sex = coalesce(H_BASE_SEX_C, num_dmgender, shf_sex),

    age = coalesce(H_BASE_AGE_NUM, num_age, shf_age),
    age_cat = case_when(
      age < 75 ~ "<75",
      age >= 75 ~ ">=75"
    ),

    tmp_primaryetiology = coalesce(H_BASE_PHFAET_C, num_dmEtio_c1, shf_primaryetiology),
    primaryetiology = case_when(
      is.na(tmp_primaryetiology) ~ NA_character_,
      tmp_primaryetiology %in% c("Ischaemic", "Ischemic heart disease", "IHD") ~ "Ischaemic",
      TRUE ~ "Non-ischaemic"
    ),

    mi = coalesce(H_MH_MI_YN, num_dmMi, sos_com_mi),

    tmp_revasc_esc = case_when(
      is.na(num_dmCabg) | is.na(num_dmPci) ~ NA_character_,
      num_dmCabg == "Yes" | num_dmPci == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    revascularisation = coalesce(H_MH_PREVASC_YN, tmp_revasc_esc, shf_revasc),

    shf_sos_com_hypertension = case_when(
      data %in% c("esc", "crt") ~ NA_character_,
      shf_hypertension == "Yes" |
        sos_com_hypertension == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    hypertension = coalesce(H_MH_HYPTEN_YN, num_dmHT, shf_sos_com_hypertension),

    shf_sos_com_af = case_when(
      data %in% c("esc", "crt") ~ NA_character_,
      sos_com_af == "Yes" |
        shf_af == "Yes" |
        shf_ekg == "Atrial fibrillation" ~ "Yes",
      TRUE ~ "No"
    ),
    af = coalesce(H_MH_AF_YN, num_dmAfib_c1, shf_sos_com_af),

    # denna måste kollas om matchar. Dessutom saknas från esc
    shf_sos_com_valvular = case_when(
      data %in% c("esc", "crt") ~ NA_character_,
      shf_valvedisease == "Yes" |
        sos_com_valvular == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    valvular = coalesce(H_MH_VHD_YN, shf_sos_com_valvular),

    copd = coalesce(H_MH_OLD_YN, num_dmCopd, sos_com_copd),

    shf_sos_com_diabetes = case_when(
      data %in% c("esc", "crt") ~ NA_character_,
      shf_diabetes == "Yes" |
        sos_com_diabetes == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    diabetes = coalesce(H_MH_DIAB_YN, num_dmDiab_c1, shf_sos_com_diabetes),

    num_Hb = coalesce(num_dcHb, num_opHb),
    hb = coalesce(H_LAB_HB_GDL_X_CORR * 10, num_Hb * 10, shf_hb),

    anemia = case_when(
      is.na(hb) | is.na(sex) ~ NA_character_,
      hb < 120 & sex == "Female" | hb < 130 & sex == "Male" ~ "Yes",
      TRUE ~ "No"
    ),

    # 1 Milligram/deciliter = 88.42 Micromole/liter
    # eGFR according to CKD-EPI
    num_Cre = coalesce(num_dcCre, num_opCre) * 88.42,
    tmp_sex = recode(num_dmgender, "Male" = 1, "Female" = 0),
    ethnicity = case_when(
      is.na(num_dmEthnic) ~ NA_real_,
      num_dmEthnic == "Black" ~ 1,
      TRUE ~ 0
    ),
    CKDEPI = nephro::CKDEpi.creat(num_Cre / 88.42, tmp_sex, num_age, ethnicity),

    gfrescrs = coalesce(CKDEPI, shf_gfrckdepi),
    ckdescrs = case_when(
      is.na(gfrescrs) ~ NA_character_,
      gfrescrs < 60 ~ "Yes",
      gfrescrs >= 60 ~ "No"
    ),
    ckd = coalesce(H_MH_CKD_YN, ckdescrs),

    cancer = coalesce(num_dmDis, sos_com_cancer3y),

    ## previous device få ihop

    # vars not in crt survey
    tmp_num_dmPtype = recode(num_dmPtype, Outpatient = "Out-patient", Hospital = "In-patient"),
    location = coalesce(tmp_num_dmPtype, shf_location),
    diffhfhosp = num_dmVisitdt - num_dmhdt,
    dmHFhosp1yr = case_when(
      is.na(num_dmHF) ~ NA_character_,
      num_dmHF == "Yes with previous hospitalisation" & diffhfhosp <= 365 ~ "Yes",
      num_dmHdy > 0 ~ "Yes",
      tmp_num_dmPtype == "In-patient" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_sos_prevhosphf1yr = case_when(
      data %in% c("esc", "crt") ~ NA_character_,
      sos_prevhosphf <= 365 | shf_location == "In-patient" ~ "Yes",
      TRUE ~ "No"
    ),
    hfhosp1yr = coalesce(H_MH_HFHOSP_YES_YN, dmHFhosp1yr, shf_sos_prevhosphf1yr),

    smoking = coalesce(num_dmSmoking, shf_smoking),
    smoking_cat = factor(case_when(
      smoking %in% c("Never") ~ 1,
      smoking %in% c("Former", "Current") ~ 2
    ),
    labels = c("Never", "Former/Current"),
    levels = 1:2
    ),

    num_Nyha = coalesce(num_dcNyha, num_opNyha),
    num_Nyha = str_replace_all(num_Nyha, "NYHA ", ""),
    nyha = coalesce(H_BASE_NYHA_C, num_Nyha, shf_nyha),
    nyha_cat = case_when(
      is.na(nyha) ~ NA_character_,
      nyha == "II" ~ "II",
      nyha %in% c("III", "IV") ~ "III-IV",
    ),

    bmi = coalesce(H_BASE_BMI_KGM2, num_dmBmi, shf_bmi),
    bmi_cat = case_when(
      is.na(bmi) ~ NA_character_,
      bmi < 30 ~ "<30",
      bmi >= 30 ~ ">=30"
    ),

    bpsys = coalesce(H_BASE_BPSYS_MMHG, num_dmBp1, shf_bpsys),
    bpsys_cat = case_when(
      bpsys < 100 ~ "<100",
      bpsys >= 100 ~ ">=100"
    ),

    bpdia = coalesce(H_BASE_BPDIA_MMHG, num_dmBp2, shf_bpdia),

    num_Bnp = coalesce(num_dcBnp, num_opBnp),
    bnp = coalesce(H_LAB_BNP_NGL_CAL_X, num_Bnp, shf_bnp),

    num_Nt = coalesce(num_dcNt, num_opNt),
    ntprobnp = coalesce(H_LAB_NTPBNP_PGML_CAL_X, num_Nt, shf_ntprobnp),
    ntprobnp_cat = case_when(
      ntprobnp <= 1500 ~ "<=1500",
      ntprobnp > 1500 ~ ">1500"
    ),

    creatinine = coalesce(H_LAB_SC_MOLL_CAL_X, num_Cre, shf_creatinine),

    num_Hr2 = coalesce(num_dcHr2, num_opHr2),
    heartrate = coalesce(H_ECG_HR_BPM, num_Hr2, shf_heartrate),
    heartrate_cat = case_when(
      heartrate <= 70 ~ "<=70",
      heartrate > 70 ~ ">70"
    ),

    num_Ryth = coalesce(num_dcRyth, num_opRyth),
    tmp_ecg = coalesce(H_ECG_AR_C, num_Ryth, shf_ekg),
    ecg = case_when(
      tmp_ecg == "Sinus" ~ "Sinus",
      tmp_ecg %in% c("Atrial fibrillation", "Atrial Fibrillation/Flutter") ~ "AF",
      tmp_ecg %in% c("Atrial paced", "Other", "PM/Other") ~ "PM/Other",
      TRUE ~ NA_character_
    ),

    # finns i esc/rs? strunta i dessa
    # printernval = H_ECG_PRINT_MS,
    # avblock = H_ECG_AVBLOCK_YN,

    # pacemaker dependent

    # inte sätta till missing om device
    num_Lbbb = coalesce(num_dcLbbb, num_opLbbb),
    lbbb = coalesce(H_ECG_QRSLBBB_YN, num_Lbbb, shf_lbbb),

    # inte sätta till missing om device
    num_QrsD = coalesce(num_dcQrsD, num_opQrsD),
    qrs = coalesce(H_ECG_IQRSDUR_MS, num_QrsD, shf_qrs),
    qrs_cat = factor(case_when(
      qrs < 120 ~ 1,
      qrs <= 129 ~ 2,
      qrs <= 149 ~ 3,
      qrs <= 179 ~ 4,
      qrs >= 180 ~ 5
    ),
    levels = 1:5,
    labels = c("<120", "120-129", "130-149", "150-179", ">180")
    ),

    num_Ef = coalesce(num_dcEf, num_opEf),
    ef = coalesce(H_PEX_ALLLVEF_PCT, num_Ef, shf_efproc),

    ef_cat35 = factor(case_when(
      ef < 25 ~ 1,
      ef <= 35 ~ 2,
      ef > 35 ~ 3
    ),
    levels = 1:3,
    labels = c("<25", "25-35", ">35")
    ),
    tmpef_cat39 = factor(case_when(
      ef < 30 ~ 1,
      ef <= 39 ~ 2,
      ef <= 49 ~ 3,
      ef > 50 ~ 4,
    ),
    levels = 1:4,
    labels = c("<30", "30-39", "40-49", ">=50")
    ),
    ef_cat39 = coalesce(tmpef_cat39, shf_ef),

    # saknas i rs
    num_Lvdd = coalesce(num_dcLvdd, num_opLvdd),
    lvedd = coalesce(H_PEX_ECHOLVEDD_MM, num_Lvdd),
    # saknas i rs
    num_MitReg = coalesce(num_dcMitReg, num_opMitReg),
    tmp_mitreg_crt = case_when(
      H_PEX_ECHOMR_C %in% c("None", "Mild") ~ "No",
      H_PEX_ECHOMR_C %in% c("Moderate", "Severe") ~ "Yes"
    ),
    mitreg_modsev = coalesce(tmp_mitreg_crt, num_MitReg),

    num_mdDiur_c2 = coalesce(num_mdDiurd_c2, num_mdDiurh_c2),
    num_mdDiur2_c2 = coalesce(num_mdDiur2d_c2, num_mdDiur2h_c2),
    loopDiurd = case_when(
      is.na(num_mdDiur_c2) ~ NA_character_,
      num_mdDiur_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") |
        num_mdDiur2_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") ~ "Yes",
      TRUE ~ "No"
    ),
    loopdiuretic = coalesce(HDC_MT_LOOPDIU_YN, loopDiurd, shf_loopdiuretic),

    num_mdACE = coalesce(num_mdACEd, num_mdACEh),
    num_mdAT = coalesce(num_mdATd, num_mdATh),
    num_mdARNI = coalesce(num_mdARNId, num_mdARNIh),
    num_rasarni = case_when(
      is.na(num_mdACE) | is.na(num_mdAT) ~ NA_character_,
      num_mdACE == "Yes" | num_mdAT == "Yes" | num_mdARNI == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    rasarni = coalesce(HDC_MT_ACE_YN, num_rasarni, shf_rasarni),

    num_mdAL = coalesce(num_mdALd, num_mdALh),
    mra = coalesce(HDC_MT_MRA_YN, num_mdAL, shf_mra),

    num_mdBB = coalesce(num_mdBBd, num_mdBBh),
    bbl = coalesce(HDC_MT_BETA_YN, num_mdBB, shf_bbl),

    # sinusknutehämmare enbart sedan april 2017 i rs
    # num_mdIvab = coalesce(num_mdIvabd, num_mdIvabh),
    # ivab = coalesce(HDC_MT_IVABR_YN, num_mdIvab),

    num_mdDigo = coalesce(num_mdDigod, num_mdDigoh),
    digoxin = coalesce(HDC_MT_DIG_YN, num_mdDigo, shf_digoxin),

    # slå ihop antiarythic och amiodarone
    # num_mdAmi = coalesce(num_mdAmid, num_mdAmih),
    # amiodarone = coalesce(HDC_MT_AMIO_YN, num_mdAmi),

    # num_mdAntiar = coalesce(num_mdAntiard, num_mdAntiarh),
    # otherantiar = coalesce(HDC_MT_OTHANTARR_YN, num_mdAntiar),

    num_mdAC = coalesce(num_mdACd, num_mdACh),
    anticoagulantia = coalesce(HDC_MT_ORAANTCOA_YN, num_mdAC, shf_anticoagulantia),

    num_mdAntipl = coalesce(num_mdAntipld, num_mdAntiplh),
    asaantiplatelet = coalesce(HDC_MT_ANTPL_MC_YN, num_mdAntipl, shf_asaantiplatelet),

    # vars not in crt survey

    tmp_num_dmhome = case_when(
      num_dmhome %in% c("Home alone", "Nursing home", "Other") ~ "Living alone",
      num_dmhome %in% c("Home with family(partner)") ~ "Cohabitating"
    ),
    famtype = coalesce(tmp_num_dmhome, scb_famtype),

    shf_followuplocation_cat = relevel(factor(
      if_else(shf_followuplocation %in% c("Primary care", "Other"),
        "Primary care/Other",
        as.character(shf_followuplocation)
      )
    ), ref = "Primary care/Other"),

    scb_education_cat = case_when(
      is.na(scb_education) ~ NA_character_,
      scb_education %in% c("Secondary school", "University") ~ "Secondary school/University",
      scb_education == "Compulsory school" ~ "Compulsory school"
    ),

    hospitaltype = factor(case_when(
      sjukhusstorlek %in% c(0, 1, 2, 4) | ADM_CTR_TYPE_UNI_YN == 0 | sqAdmin %in% c("Community or district hospital", "Private clinic") ~ 1,
      sjukhusstorlek %in% c(3) | ADM_CTR_TYPE_UNI_YN == 1 | sqAdmin %in% c("University hospital") ~ 2
    ),
    levels = 1:2, labels = c("Other", "University Hospital")
    ),

    Implanting.CRT = factor(Implanting.CRT,
      levels = 0:1,
      labels = c("No", "Yes")
    ),

    esc_hfpatstreated = case_when(
      sqHFCard == "2" & sqHFInMed == "" & sqHFHFunit == "" ~ "Only cardiology wards",
      sqHFCard == "2" & sqHFInMed == "2" & sqHFHFunit == "" ~ "Cardiology & IM wards",
      sqHFCard == "2" & sqHFInMed == "" & sqHFHFunit == "2" ~ "Cardiology & HF units",

      sqHFCard == "" & sqHFInMed == "2" & sqHFHFunit == "" ~ "Only IM wards",
      sqHFCard == "" & sqHFInMed == "2" & sqHFHFunit == "2" ~ "IM & HF units",

      sqHFCard == "" & sqHFInMed == "" & sqHFHFunit == "2" ~ "Only HF units",

      sqHFCard == "2" & sqHFInMed == "2" & sqHFHFunit == "2" ~ "Cardiology & IM & HF units"
    ),

    HFclinicavailablefollowup = sqAdmin, 
    
    # device and crt
    ## esc registry PM from characteristics and crt/icd at discharge/visit
    num_Icd = coalesce(num_dcIcd, num_opIcd),
    num_Crt = coalesce(num_dcCrt, num_opCrt),
    num_CrtT = coalesce(num_dcCrtT, num_opCrtT),
    num_CrtRe = coalesce(num_dcCrtRe, num_opCrtRe),

    tmp_device = case_when(
      num_Icd == "Already implanted" & num_Crt == "Already implanted" ~ "CRT-D",
      !is.na(num_Icd) & num_Crt == "Already implanted" ~ "CRT-P",
      num_Icd == "Already implanted" & !is.na(num_Crt) ~ "ICD"
    ),

    tmp_device2 = coalesce(tmp_device, num_dmDev),

    device = factor(case_when(
      H_PROC_DEVICE_C == "CRT-D" |
        shf_device == "CRT & ICD" |
        tmp_device2 == "CRT-D" ~ 1,
      H_PROC_DEVICE_C == "CRT-P" |
        shf_device == "CRT" |
        tmp_device2 == "CRT-P" ~ 2,
      shf_device %in% c("Pacemaker", "ICD") |
        tmp_device2 %in% c("PM", "ICD") ~ 3,
      shf_device == "No" |
        tmp_device2 == "No" ~ 4
    ),
    levels = 1:4,
    labels = c("CRT-D", "CRT-P", "PM/ICD", "No")
    ),

    crt = case_when(
      data == "crt" |
        device %in% c("CRT-D", "CRT-P") ~ "CRT",
      ((qrs > 149 | lbbb == "Yes") | (qrs >= 120 & lbbb == "Yes")) &
        (nyha %in% c("III", "IV") | nyha == "II" & ecg == "Sinus") ~ "Indication",
      TRUE ~ "No indication"
    ),

    # ind device for smd in tab 1
    crt_d = factor(if_else(device == "CRT-D", 1, 0), levels = 0:1, labels = c("No", "Yes")),
    crt_p = factor(if_else(device == "CRT-P", 1, 0), levels = 0:1, labels = c("No", "Yes")),
    pmicd = factor(if_else(device == "PM/ICD", 1, 0), levels = 0:1, labels = c("No", "Yes")),
    nodevice = factor(if_else(device == "No", 1, 0), levels = 0:1, labels = c("No", "Yes")),

    nation = str_to_title(tolower(nation)),
    country = case_when(
      data == "rs" ~ "Sweden",
      data == "esc" & nation != "" ~ nation,
      ADM_CTR_COUNTRY_ALG_YN == 1 ~ "Algeria",
      ADM_CTR_COUNTRY_ARM_YN == 1 ~ "Armenia",
      ADM_CTR_COUNTRY_AUT_YN == 1 ~ "Austria",
      ADM_CTR_COUNTRY_BEL_YN == 1 ~ "Belgium",
      ADM_CTR_COUNTRY_BUL_YN == 1 ~ "Bulgaria",
      ADM_CTR_COUNTRY_CRO_YN == 1 ~ "Croatia",
      ADM_CTR_COUNTRY_CZE_YN == 1 ~ "Czech Republic",
      ADM_CTR_COUNTRY_DEN_YN == 1 ~ "Denmark",
      ADM_CTR_COUNTRY_EGY_YN == 1 ~ "Egypt",
      ADM_CTR_COUNTRY_ESP_YN == 1 ~ "Spain",
      ADM_CTR_COUNTRY_EST_YN == 1 ~ "Estonia",
      ADM_CTR_COUNTRY_FIN_YN == 1 ~ "Finland",
      ADM_CTR_COUNTRY_FRA_YN == 1 ~ "France",
      ADM_CTR_COUNTRY_GEO_YN == 1 ~ "Georgia",
      ADM_CTR_COUNTRY_GER_YN == 1 ~ "Germany",
      ADM_CTR_COUNTRY_GRE_YN == 1 ~ "Greece",
      ADM_CTR_COUNTRY_HUN_YN == 1 ~ "Hungary",
      ADM_CTR_COUNTRY_ICE_YN == 1 ~ "Iceland",
      ADM_CTR_COUNTRY_IRL_YN == 1 ~ "Ireland",
      ADM_CTR_COUNTRY_ISR_YN == 1 ~ "Israel",
      ADM_CTR_COUNTRY_ITA_YN == 1 ~ "Italy",
      ADM_CTR_COUNTRY_KAZ_YN == 1 ~ "Kazakhstan",
      ADM_CTR_COUNTRY_LAT_YN == 1 ~ "Latvia",
      ADM_CTR_COUNTRY_LIB_YN == 1 ~ "Lebanon",
      ADM_CTR_COUNTRY_LTU_YN == 1 ~ "Lithuania",
      ADM_CTR_COUNTRY_LUX_YN == 1 ~ "Luxembourg",
      ADM_CTR_COUNTRY_MAR_YN == 1 ~ "Morocco",
      ADM_CTR_COUNTRY_MKD_YN == 1 ~ "Macedonia",
      ADM_CTR_COUNTRY_MLT_YN == 1 ~ "Malta",
      ADM_CTR_COUNTRY_MNE_YN == 1 ~ "Montenegro",
      ADM_CTR_COUNTRY_NED_YN == 1 ~ "Netherlands",
      ADM_CTR_COUNTRY_NOR_YN == 1 ~ "Norway",
      ADM_CTR_COUNTRY_POL_YN == 1 ~ "Poland",
      ADM_CTR_COUNTRY_POR_YN == 1 ~ "Portugal",
      ADM_CTR_COUNTRY_ROU_YN == 1 ~ "Romania",
      ADM_CTR_COUNTRY_RUS_YN == 1 ~ "Russian Federation",
      ADM_CTR_COUNTRY_SUI_YN == 1 ~ "Switzerland",
      ADM_CTR_COUNTRY_SVK_YN == 1 ~ "Slovakia",
      ADM_CTR_COUNTRY_SVN_YN == 1 ~ "Slovenia",
      ADM_CTR_COUNTRY_SWE_YN == 1 ~ "Sweden",
      ADM_CTR_COUNTRY_TUR_YN == 1 ~ "Turkey",
      ADM_CTR_COUNTRY_UK_YN == 1 ~ "United Kingdom"
    ),
    overlapping_country = case_when(
      country %in% c(
        "Austria", "Bulgaria", "Croatia", "Czech Republic",
        "Denmark", "Egypt", "Estonia", "France", "Georgia",
        "Greece", "Hungary", "Israel", "Italy", "Latvia",
        "Lithuania", "Macedonia", "Poland", "Portugal",
        "Romania", "Slovakia", "Spain", "Sweden", "Switzerland", "Turkey"
      ) ~ "Yes",
      TRUE ~ "No"
    ),

    # Outcomes

    ## censor at 1 yr in rs
    sos_out_death = if_else(sos_outtime_death <= 365, as.character(sos_out_death), "No"),
    sos_out_deathcv = if_else(sos_outtime_death <= 365, as.character(sos_out_deathcv), "No"),
    sos_out_hosphf = if_else(sos_outtime_hosphf <= 365, as.character(sos_out_hosphf), "No"),

    sos_outtime_death = if_else(sos_outtime_death <= 365, sos_outtime_death, 365),
    sos_outtime_hosphf = if_else(sos_outtime_hosphf <= 365, sos_outtime_hosphf, 365),

    # esc
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = coalesce(num_dcDischdt, num_dmVisitdt),

    esc_outtime_death = as.numeric(enddtm - startdtm),

    esc_out_death = case_when(
      num_f1vital == "Alive" ~ "No",
      num_f1vital == "Dead" ~ "Yes"
    ),

    esc_out_deathcv = case_when(
      is.na(esc_out_death) ~ NA_character_,
      num_f1DeathCs %in% c("Cardiac", "Vascular") ~ "Yes",
      TRUE ~ "No"
    ), # pats with missing info are NOT included in CV

    # HF hosp
    esc_out_hosphf = case_when(
      data != "esc" ~ NA_character_,
      num_f1lost != "No" ~ NA_character_,
      num_f1hosp1cs == "HF" |
        num_f1hosp2cs == "HF" |
        num_f1hosp3cs == "HF" |
        num_f1hosp4cs == "HF" |
        num_f1hosp5cs == "HF" ~ "Yes",
      TRUE ~ "No"
    ),
    out_hosphfdtm = case_when(
      num_f1hosp1cs == "HF" ~ num_f1hosp1dt,
      num_f1hosp2cs == "HF" ~ num_f1hosp2dt,
      num_f1hosp3cs == "HF" ~ num_f1hosp3dt,
      num_f1hosp4cs == "HF" ~ num_f1hosp4dt,
      num_f1hosp5cs == "HF" ~ num_f1hosp5dt
    ),
    esc_outtime_hosphf = as.numeric(out_hosphfdtm - startdtm),
    esc_outtime_hosphf = ifelse(esc_out_hosphf == "Yes" & is.na(esc_outtime_hosphf), esc_outtime_death / 2, esc_outtime_hosphf),
    esc_outtime_hosphf = pmin(esc_outtime_hosphf, esc_outtime_death, na.rm = TRUE),

    # together
    out_death = coalesce(esc_out_death, sos_out_death),
    out_deathcv = coalesce(esc_out_deathcv, sos_out_deathcv),
    out_hosphf = coalesce(esc_out_hosphf, sos_out_hosphf),
    outtime_death = coalesce(esc_outtime_death, sos_outtime_death),
    outtime_hosphf = coalesce(esc_outtime_hosphf, sos_outtime_hosphf),

    # comp risk
    out_deathcv_cr = create_crevent(out_deathcv, out_death),
    out_hosphf_cr = create_crevent(out_hosphf, out_death),

    # MAGGIC
    ## assumption since don't have ef continous in swedehf
    maggic_ef = case_when(
      ef_cat39 == "<30" ~ 6,
      ef_cat39 == "30-39" ~ 3
    ),
    maggic_age = case_when(
      age < 55 ~ 0,
      ef_cat39 == "<30" & age <= 59 ~ 1,
      ef_cat39 == "30-39" & age <= 59 ~ 2,
      ef_cat39 == "<30" & age <= 64 ~ 2,
      ef_cat39 == "30-39" & age <= 64 ~ 4,
      ef_cat39 == "<30" & age <= 69 ~ 4,
      ef_cat39 == "30-39" & age <= 69 ~ 6,
      ef_cat39 == "<30" & age <= 74 ~ 6,
      ef_cat39 == "30-39" & age <= 74 ~ 8,
      ef_cat39 == "<30" & age <= 79 ~ 8,
      ef_cat39 == "30-39" & age <= 79 ~ 10,
      ef_cat39 == "<30" & age > 79 ~ 10,
      ef_cat39 == "30-39" & age > 79 ~ 13
    ),
    maggic_bpsys = case_when(
      ef_cat39 == "<30" & bpsys < 110 ~ 5,
      ef_cat39 == "30-39" & bpsys < 110 ~ 3,
      ef_cat39 == "<30" & bpsys <= 119 ~ 4,
      ef_cat39 == "30-39" & bpsys <= 119 ~ 2,
      ef_cat39 == "<30" & bpsys <= 129 ~ 3,
      ef_cat39 == "30-39" & bpsys <= 129 ~ 1,
      ef_cat39 == "<30" & bpsys <= 139 ~ 2,
      ef_cat39 == "30-39" & bpsys <= 139 ~ 1,
      ef_cat39 == "<30" & bpsys <= 149 ~ 1,
      ef_cat39 == "30-39" & bpsys <= 149 ~ 0,
      bpsys > 149 ~ 0
    ),
    maggic_bmi = case_when(
      bmi < 15 ~ 6,
      bmi <= 19 ~ 5,
      bmi <= 24 ~ 3,
      bmi <= 29 ~ 2,
      bmi > 29 ~ 0
    ),
    maggic_creatinine = case_when(
      creatinine < 90 ~ 0,
      creatinine <= 109 ~ 1,
      creatinine <= 129 ~ 2,
      creatinine <= 149 ~ 3,
      creatinine <= 169 ~ 4,
      creatinine <= 209 ~ 5,
      creatinine <= 249 ~ 6,
      creatinine > 249 ~ 8
    ),
    maggic_nyha = case_when(
      nyha == "I" ~ 0,
      nyha == "II" ~ 2,
      nyha == "III" ~ 6,
      nyha == "IV" ~ 8
    ),
    maggic_sex = case_when(
      sex == "Female" ~ 0,
      sex == "Male" ~ 1
    ),
    # antagit alla  non-smokers
    maggic_smoking = 0,
    maggic_diabetes = case_when(
      diabetes == "No" ~ 0,
      diabetes == "Yes" ~ 3
    ),
    maggic_copd = case_when(
      copd == "No" ~ 0,
      copd == "Yes" ~ 2
    ),
    # # antagit alla duration hf > 1.5 år
    maggic_durationhf = 2,
    maggic_bbl = case_when(
      bbl == "No" ~ 3,
      bbl == "Yes" ~ 0
    ),
    maggic_ras = case_when(
      rasarni == "No" ~ 1,
      rasarni == "Yes" ~ 0
    ),
    maggic_score = maggic_ef + maggic_age + maggic_bpsys +
      maggic_bmi + maggic_creatinine + maggic_nyha + maggic_sex +
      maggic_smoking + maggic_diabetes + maggic_copd + maggic_durationhf +
      maggic_bbl + maggic_ras
  ) %>%
  select(-starts_with("tmp_")) %>%
  mutate(across(where(is_character), factor))
