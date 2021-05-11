flow <- c("Number of posts", data3 %>% count(data) %>% pull(n))

data3 <- data3 %>%
  filter(data == "crt" | !is.na(ef_cat39))
flow <- rbind(flow, c("No missing EF (ESC & SwedeHF)", data3 %>% count(data) %>% pull(n)))

data3 <- data3 %>%
  filter(data == "crt" |
    data == "esc" & ef <= 35 |
    data == "rs" & ef_cat39 %in% c("<30", "30-39"))
flow <- rbind(flow, c("EF <= 35/39% (ESC/SwedeHF)", data3 %>% count(data) %>% pull(n)))

data3 <- data3 %>%
  filter(data == "crt" | !is.na(hfdur))
flow <- rbind(flow, c("No missing HF duration (ESC & SwedeHF)", data3 %>% count(data) %>% pull(n)))

data3 <- data3 %>%
  filter(data == "crt" | hfdur == ">6mo")
flow <- rbind(flow, c("HF duration > 6mo (ESC & SwedeHF)", data3 %>% count(data) %>% pull(n)))

data3 <- data3 %>%
  filter(data == "crt" |
    data == "esc" & !is.na(num_Crt) |
    data == "rs" & !is.na(shf_device))
flow <- rbind(flow, c("No missing crt/device (ESC/SwedeHF)", data3 %>% count(data) %>% pull(n)))

data3 <- data3 %>%
  filter(data == "crt" | !is.na(lbbb) & !is.na(qrs))
flow <- rbind(flow, c("No missing QRS or LBBB (ESC & SwedeHF)", data3 %>% count(data) %>% pull(n)))

data3 <- data3 %>%
  filter(data == "crt" | !is.na(ecg))
flow <- rbind(flow, c("No missing ECG (ESC & SwedeHF)", data3 %>% count(data) %>% pull(n)))

data3 <- data3 %>%
  filter(data == "crt" | !is.na(nyha))
flow <- rbind(flow, c("No missing NYHA (ESC & SwedeHF)", data3 %>% count(data) %>% pull(n)))

data3 <- data3 %>%
  filter(data == "crt" |
    data == "esc" & num_dmVisitdt >= ymd("2013-01-01") & num_dmVisitdt <= ymd("2016-01-01") |
    data == "rs" & shf_indexyear %in% c(2013:2016))
flow <- rbind(flow, c("Year 2013-2016 (ESC & SwedeHF)", data3 %>% count(data) %>% pull(n)))

data3 <- data3 %>%
  group_by(data, lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post/patient (SwedeHF)", data3 %>% count(data) %>% pull(n)))

flow <- rbind(flow, c(
  ".  Overlapping countries",
  data3 %>% filter(overlapping_country == "Yes") %>% count(data) %>% pull(n)
))

colnames(flow) <- c("Inclusion/Exclusion criteria", "CRT Survey II", "ESC Registry", "SwedeHF Registry")
