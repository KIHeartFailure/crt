
centrenewrs <- left_join(
  newrs %>%
    select(LopNr, HEALTHCAREUNIT, d_DATE_FOR_ADMISSION, DATE_DISCHARGE, migrated),
  enheter.i.RiksSvikt.TT.Blad1 %>%
    select(ID, ORG_UNIT_NAME, sjukhusstorlek, Implanting.CRT),
  by = c("HEALTHCAREUNIT" = "ID")
) %>%
  mutate(
    shf_indexdtm = coalesce(DATE_DISCHARGE, d_DATE_FOR_ADMISSION),
    sjukhusstorlek = as.numeric(sjukhusstorlek),
    Implanting.CRT = as.numeric(Implanting.CRT)
  ) %>%
  rename(
    centreid = HEALTHCAREUNIT,
    centrename = ORG_UNIT_NAME
  ) %>%
  select(LopNr, shf_indexdtm, centreid, centrename, sjukhusstorlek, Implanting.CRT, migrated)

centreoldrs <- left_join(
  RiksSvikt.center.Blad1,
  enheter.i.RiksSvikt.TT.Blad1 %>%
    select(ORG_UNIT_NAME, sjukhusstorlek, Implanting.CRT),
  by = c("CENTRENAME" = "ORG_UNIT_NAME")
) %>%
  mutate(
    sjukhusstorlek = as.numeric(sjukhusstorlek),
    Implanting.CRT = as.numeric(Implanting.CRT)
  ) %>%
  select(ID, CENTRENAME, sjukhusstorlek, Implanting.CRT)

# for those not in new rs
centreoldrs <- left_join(
  centreoldrs,
  kollcentre.oldrs.20210422.TT.Sheet.1,
  by = c("ID" = "CENTREID", "CENTRENAME" = "CENTRENAME")
) %>%
  mutate(
    sjukhusstorlek = coalesce(sjukhusstorlek.x, sjukhusstorlek.y),
    Implanting.CRT = coalesce(Implanting.CRT.x, Implanting.CRT.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))


centreoldrs <- left_join(
  oldrs %>%
    select(LopNr, CENTREID, DTMIN, DTMUT),
  centreoldrs %>%
    select(ID, CENTRENAME, sjukhusstorlek, Implanting.CRT),
  by = c("CENTREID" = "ID")
) %>%
  mutate(shf_indexdtm = coalesce(DTMUT, DTMIN)) %>%
  rename(
    centreid = CENTREID,
    centrename = CENTRENAME
  ) %>%
  select(LopNr, shf_indexdtm, centreid, centrename, sjukhusstorlek, Implanting.CRT)


# koll <- centreoldrs %>%
#  filter(is.na(sjukhusstorlek)) %>%
#  group_by(centreid, centrename) %>%
#  slice(1) %>%
#  ungroup()

# below is replicating how rsdata is created
centerrs <- bind_rows(
  centrenewrs %>% mutate(source = 3),
  centreoldrs %>% mutate(source = 1)
) %>%
  mutate(
    shf_source = case_when(
      source == 3 & migrated == 1 ~ 2,
      TRUE ~ source
    ),
    shf_source = factor(shf_source, labels = c("Old SHF", "New SHF migrated from old SHF", "New SHF"))
  )

centerrs <- centerrs %>%
  group_by(LopNr, shf_indexdtm) %>%
  arrange(shf_source) %>%
  slice(1) %>%
  ungroup() %>%
  select(-migrated, -source, -shf_source)

# join centreinfo with rsdata
rsdata323 <- left_join(
  rsdata323,
  centerrs,
  by = c("LopNr", "shf_indexdtm")
)
