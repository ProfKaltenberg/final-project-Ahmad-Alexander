# model 1: pooled OLS, agency as covariate
model1_lm <- lm(
  deviation ~ agency + scheduled_headway + peak + timepoint_rate + headway_cv,
  data = gtfs
)

# model 2: route FE via dummy variables
model2_lm <- lm(
  deviation ~ scheduled_headway + peak + timepoint_rate + headway_cv + route_id,
  data = gtfs
)

# model 3: route + day of week FE
model3_lm <- lm(
  deviation ~ scheduled_headway + peak + timepoint_rate + headway_cv + route_id + day_of_week,
  data = gtfs
)

summary(model1_lm)
summary(model2_lm)
summary(model3_lm)

library(lmtest)
library(sandwich)
se1 <- sqrt(diag(vcovCL(model1_lm, cluster = ~route_id)))
se2 <- sqrt(diag(vcovCL(model2_lm, cluster = ~route_id)))
se3 <- sqrt(diag(vcovCL(model3_lm, cluster = ~route_id)))
stargazer(
  model1_lm, model2_lm, model3_lm,
  type = "html",
  out = "regression_results.html",
  se = list(se1, se2, se3),
  title = "Determinants of Bus Schedule Deviation",
  dep.var.caption = "", #removes "Dependent variable:" line
  dep.var.labels = "", #removes "Deviation (minutes)" line
  column.labels = c("Pooled OLS", "Route FE", "Two-Way FE"), #column names
  model.numbers = FALSE, #removes (1)(2)(3)
  covariate.labels = c("MTA", "WMATA", "Scheduled Headway",
                       "Peak Hours", "Timepoint Rate", "Headway CV"),
  omit = c("route_id", "day_of_week"),
  add.lines = list(
    c("Route FE", "No", "Yes", "Yes"),
    c("Day of Week FE", "No", "No", "Yes"),
    c("Clustered SE", "Yes", "Yes", "Yes")
  ),
  omit.stat = c("f", "ser"),
  notes = "Standard errors clustered at route level"
)

library(margins)
# model 5: agency x headway interaction
model5_lm <- lm(
  deviation ~ agency + scheduled_headway + agency:scheduled_headway +
    peak + timepoint_rate + headway_cv + day_of_week,
  data = gtfs
)
se5 <- sqrt(diag(vcovCL(model5_lm, cluster = ~route_id)))
summary(model5_lm)

# model 6: logistic regression, bunching as outcome
model6 <- glm(
  bunching ~ agency + scheduled_headway + peak + timepoint_rate + headway_cv,
  data = gtfs,
  family = "binomial"
)

# marginal effects — average marginal effects, not log odds
marginal_effects <- margins(model6)
summary(marginal_effects)
plot(marginal_effects)
cplot(model6, "scheduled_headway",
      what = "prediction",
      main = "Predicted Probability of Bunching by Scheduled Headway",
      xlab = "Scheduled Headway (minutes)",
      ylab = "Predicted Probability of Bunching")

# extract marginal effects as a model-like object for stargazer
margins_summary <- summary(marginal_effects)
margins_ordered$p <- ifelse(margins_ordered$p == 0, 0.0001, margins_ordered$p)

# stargazer with margins instead of log odds
stargazer(
  model1_lm, model2_lm, model3_lm, model5_lm, model6,
  type = "html",
  out = "regression_results2.html",
  se = list(se1, se2, se3, se5, margins_summary$SE), #clustered SEs for lm, margins SE for logit
  coef = list(NULL, NULL, NULL, NULL, margins_summary$AME), #replace log odds with AME
  title = "Determinants of Bus Schedule Deviation and Bunching",
  dep.var.caption = "",
  dep.var.labels = c("Deviation (minutes)", "Bunching (0/1)"),
  column.labels = c("Pooled OLS", "Route FE", "Two-Way FE", "Interaction", "Logit (AME)"),
  covariate.labels = c("MTA", "WMATA", "Scheduled Headway", "Peak Hours",
                       "Direction", "Number of Stops", "Timepoint Rate",
                       "Route Length (mi)", "Headway CV",
                       "MTA x Headway", "WMATA x Headway"),
  omit = c("route_id", "day_of_week"),
  add.lines = list(
    c("Route FE", "No", "Yes", "Yes", "Yes", "No"),
    c("Day of Week FE", "No", "No", "Yes", "Yes", "No"),
    c("Clustered SE", "Yes", "Yes", "Yes", "Yes", "No")
  ),
  omit.stat = c("f", "ser"),
  notes = c("Standard errors clustered at route level for OLS models",
            "Logit column reports average marginal effects")
)

# reorder margins to match covariate order in other models
margins_ordered <- margins_summary[match(
  c("agencyMTA", "agencyWMATA", "scheduled_headway", "peak", "timepoint_rate", "headway_cv"),
  margins_summary$factor
), ]

margins_ordered[, c("factor", "AME", "SE", "p")]
margins_ordered$p <- ifelse(margins_ordered$p == 0, 0.0001, margins_ordered$p)
p_logit    <- c(NA, margins_ordered$p)
se_logit   <- c(NA, margins_ordered$SE)
coef_logit <- c(NA, margins_ordered$AME)

stargazer(
  model1_lm, model2_lm, model3_lm, model5_lm, model6,
  type = "html",
  out = "regression_results2.html",
  se = list(se1, se2, se3, se5, margins_ordered$SE),
  coef = list(NULL, NULL, NULL, NULL, margins_ordered$AME),
  p = list(NULL, NULL, NULL, NULL, margins_ordered$p),
  title = "Cross-Agency Transit Reliability",
  dep.var.caption = "",
  dep.var.labels = c("Deviation (minutes)", "Bunching (0/1)"),
  column.labels = c("Pooled OLS", "Route FE", "Two-Way FE", "Agency Interaction", "Logit"),
  model.numbers = TRUE,
  model.names = FALSE,
  covariate.labels = c(
    "MTA", "WMATA",                          # agency dummies
    "Scheduled Headway",                      # headway
    "Peak Hours",                             # peak
    "Timepoint Rate",                         # timepoint
    "Headway CV",                             # headway cv
    "MTA x Headway",                          # interaction terms (model 5 only)
    "WMATA x Headway"
  ),
  omit = c("route_id", "day_of_week"),
  omit.stat = c("f", "ser"),
  add.lines = list(
    c("Route FE", "No", "Yes", "Yes", "No", "No"),
    c("Day of Week FE", "No", "No", "Yes", "Yes", "No"),
    c("Clustered SE", "Yes", "Yes", "Yes", "Yes", "No")
  ),
  notes = c("Standard errors clustered at route level for OLS models",
            "Logit column reports average marginal effects",
            "MBTA is the reference agency category")
)

margins_ordered$factor
length(margins_ordered$AME)
length(margins_ordered$SE)
length(margins_ordered$p)

length(coef(model6))
# check model6 coefficient names
names(coef(model6))

margins_ordered <- margins_summary[match(
  c("agencyMTA", "agencyWMATA", "scheduled_headway", "peak", "timepoint_rate", "headway_cv"),
  margins_summary$factor
), ]
margins_ordered$p <- ifelse(margins_ordered$p == 0, 0.0001, margins_ordered$p)
p_logit    <- c(NA, margins_ordered$p)
se_logit   <- c(NA, margins_ordered$SE)
coef_logit <- c(NA, margins_ordered$AME)

stargazer(
  model1_lm, model2_lm, model3_lm, model5_lm, model6,
  type = "html",
  out = "regression_results2.html",
  se   = list(se1, se2, se3, se5, se_logit),
  coef = list(NULL, NULL, NULL, NULL, coef_logit),
  p    = list(NULL, NULL, NULL, NULL, p_logit),
  title = "Cross-Agency Transit Reliability",
  dep.var.caption = "",
  dep.var.labels = c("Deviation (minutes)", "Bunching (0/1)"),
  column.labels = c("OLS", "Route FE", "Two-Way FE", "Agency Interaction", "Logit"),
  model.numbers = TRUE,
  model.names = FALSE,
  covariate.labels = c(
    "MTA", "WMATA",
    "Scheduled Headway",
    "Peak Hours",
    "Timepoint Rate",
    "Headway CV",
    "MTA x Headway",
    "WMATA x Headway"
  ),
  omit = c("route_id", "day_of_week"),
  omit.stat = c("f", "ser"),
  add.lines = list(
    c("Route FE", "No", "Yes", "Yes", "No", "No"),
    c("Day of Week FE", "No", "No", "Yes", "Yes", "No"),
    c("Clustered SE", "Yes", "Yes", "Yes", "Yes", "No")
  ),
  notes = c("Standard errors clustered at route level for OLS models",
            "Logit column reports average marginal effects",
            "MBTA is the reference agency category")
)