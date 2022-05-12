

# Impute missing values ESC ------------------------------------------------

data_esc <- data3 %>%
  filter(data == "esc", crt %in% c("CRT", "Indication")) %>%
  select(!!!syms(modvars_esc), crt)

## check no cores
cores_2_use <- detectCores() - 1
if (cores_2_use >= 10) {
  cores_2_use <- 10
  m_2_use <- 1
} else if (cores_2_use >= 5) {
  cores_2_use <- 5
  m_2_use <- 2
} else {
  stop("Need >= 5 cores for this computation")
}

cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 49956)
registerDoParallel(cl)

impdata_esc <-
  foreach(
    no = 1:cores_2_use,
    .combine = ibind,
    .export = c("data_esc"),
    .packages = "mice"
  ) %dopar% {
    mice(data_esc,
      m = m_2_use, maxit = 10,
      printFlag = FALSE
    )
  }
stopImplicitCluster()

# Impute missing values RS ------------------------------------------------

data_rs <- data3 %>%
  filter(data == "rs", crt %in% c("CRT", "Indication")) %>%
  select(!!!syms(modvars_rs), crt)

cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 49956)
registerDoParallel(cl)

impdata_rs <-
  foreach(
    no = 1:cores_2_use,
    .combine = ibind,
    .export = c("data_rs"),
    .packages = "mice"
  ) %dopar% {
    mice(data_rs,
      m = m_2_use, maxit = 10,
      printFlag = FALSE
    )
  }
stopImplicitCluster()
