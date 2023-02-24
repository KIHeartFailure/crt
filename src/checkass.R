
ProjectTemplate::reload.project()

dataass_rs <- mice::complete(impdata_rs, 6)
dataass_esc <- mice::complete(impdata_esc, 6)

# Logistic regression -----------------------------------------------------
modlm_esc <- glm(formula(paste0("crt == 'CRT' ~ ", paste(modvars_esc, collapse = " + "))),
  family = binomial(link = "logit"), data = dataass_esc
)
modlm_rs <- glm(formula(paste0("crt == 'CRT' ~ ", paste(modvars_rs, collapse = " + "))),
  family = binomial(link = "logit"), data = dataass_rs
)


# Linearity for continous variables ---------------------------------------

# No continous variables

# Outliers ---------------------------------------------------------------

x11()
plot(modlm_esc, which = 4, id.n = 3, )
savePlot("C:/Users/Lina/STATISTIK/Projects/20201126_thorvaldsen_crt/output/figs/cooks_esc.pdf", type = "pdf")

x11()
plot(modlm_rs, which = 4, id.n = 3, )
savePlot("C:/Users/Lina/STATISTIK/Projects/20201126_thorvaldsen_crt/output/figs/cooks_rs.pdf", type = "pdf")

# Multicollinearity -------------------------------------------------------

vif_esc <- tibble(var = names(vif_esc), esc_vif = car::vif(modlm_esc))
vif_rs <- tibble(var = names(vif_rs), rs_vif = car::vif(modlm_rs))

vifboth <- full_join(vif_esc, vif_rs, by = "var")

write.xlsx(vifboth, paste0("./output/tabs/vifvalues_", Sys.Date(), ".xlsx"), rowNames = FALSE)
