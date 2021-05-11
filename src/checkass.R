
ProjectTemplate::reload.project()

dataass_rs <- mice::complete(impdata_rs, 6)
dataass_esc <- mice::complete(impdata_esc, 6)

# Logistic regression -----------------------------------------------------
modlm_esc <- glm(formula(paste0("crt == 'CRT' ~ ", paste(modvars_esc, collapse = " + "))),
  family = binomial(link = "logit"), data = dataass_esc
)
modlm_rs <- glm(formula(paste0("crt == 'CRT' ~ ", paste(modvars_esc, collapse = " + "))),
  family = binomial(link = "logit"), data = dataass_rs
)


# Linearity for continous variables ---------------------------------------

# No continous variables

# Outliers ---------------------------------------------------------------

plot(modlm_esc, which = 4, id.n = 3)
plot(modlm_rs, which = 4, id.n = 3)

# Multicollinearity -------------------------------------------------------

car::vif(modlm_esc)
car::vif(modlm_rs)
