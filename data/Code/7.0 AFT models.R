## 6. AFT Models ---- 

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/HW/"
data_out <- "Data/Output/"

## Data ---- 

# BW
bw_data_lw <- rio::import(paste0(data_out, "births_1992_2020_last_week_hw", ".RData")) %>% drop_na()
bw_data_lm <- rio::import(paste0(data_out, "births_1992_2020_last_month_hw", ".RData")) %>% drop_na()

# Adjust data 
glimpse(bw_data_lw)

# Test initial models 

formula <- as.formula(paste("Surv(weeks, ", "birth_preterm", ") ~ ", "HW_p99_3d_bin", 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(year_nac) + vulnerability"))

m1 <- survreg(formula, dist = "gaussian", data = bw_data_lw) # Absolute time 
m2 <- survreg(formula, dist = "weibull", data = bw_data_lw) # log-time
#m3 <- survreg(formula, dist = "exponential", data = bw_data_lw) # logtime
#m4 <- survreg(formula, dist = "logistic", data = bw_data_lw) # logistic -> change survival time
m5 <- survreg(formula, dist = "loglogistic", data = bw_data_lw) # logtime 

screenreg(l=list(m1, m2, m3, m4, m5), digits=3)

m6 <- coxph(formula, data = bw_data_lw)

screenreg(l=list(m2, m6), digits=3)

# Decision: use weibull model to compare HR with cox model 

## HR AFT Models LW ---- 

# Weibull regression as AFT model 

# Variables dependientes y predictoras
dependent_vars <- c("birth_preterm", "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm") # , "birth_term", "birth_posterm"

heatwave_vars <- c("HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                   "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                   "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                   "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin", 
                   "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
                   #"HW_EHF_TMAX_2d_bin", "HW_EHF_TMAX_3d_bin", "HW_EHF_TMAX_4d_bin",
                   #"HW_EHF_TAD_sev_first", "HW_EHF_TMAX_sev_first"
                  )

# Función para ajustar el modelo Weibull AFT y transformar a Hazard Ratios
fit_weibull_aft <- function(dependent, predictor) {
  # Fórmula del modelo
  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(year_nac) + vulnerability"))
  
  # Ajuste del modelo Weibull
  model_fit <- survreg(formula, data = bw_data_lw, dist = "weibull")
  
  # Transformar coeficientes a Hazard Ratios
  scale <- model_fit$scale  # Parámetro de escala
  tidy_model <- tidy(model_fit) %>%
    mutate(HR = exp(-estimate / scale),  # Hazard Ratio
           conf.low = exp(-(estimate + 1.96 * std.error) / scale),  # Límite inferior del IC
           conf.high = exp(-(estimate - 1.96 * std.error) / scale),  # Límite superior del IC
           estimate = round(estimate, 3),
           HR = round(HR, 3),
           conf.low = round(conf.low, 3),
           conf.high = round(conf.high, 3)) %>%
    select(term, HR, std.error, conf.low, conf.high) %>%
    mutate(dependent_var = dependent, predictor = predictor)  
  
  return(tidy_model)
}


# Aplicar la función con manejo de errores
tic()
weibull_results <- map(dependent_vars, function(dep_var) {
  map(heatwave_vars, function(hw_var) {
    tryCatch({
      fit_weibull_aft(dep_var, hw_var)
    }, error = function(e) {
      message(paste("Error con:", dep_var, "y", hw_var, "-", e$message))
      return(NULL)  # Retornar NULL si hay error
    })
  }) %>% bind_rows()
}) %>% bind_rows()
toc()

# Resultados para los modelos Weibull transformados a HRs
weibull_results

writexl::write_xlsx(weibull_results, path =  paste0("Output/", "Models/", "AFT_models", ".xlsx"))

# Plots with HW - Effects

results_filtered <- weibull_results %>%
  filter(term %in% heatwave_vars, dependent_var %in% dependent_vars) %>% 
  rename(estimate=HR)

plots <- list()

for (dep_var in dependent_vars) {
  # Subset data for the current dependent variable
  data_subset <- results_filtered %>% 
    filter(dependent_var == dep_var) %>% 
    mutate(
      duration = str_extract(term, "\\d+d"), 
      duration_label = case_when( 
        duration == "2d" ~ "2 days",
        duration == "3d" ~ "3 days",
        duration == "4d" ~ "4 or more days",
        TRUE ~ NA_character_ 
      ),
      duration_label = factor(duration_label, levels = c("2 days", "3 days", "4 or more days")) 
    )
  
  data_subset <- data_subset %>% 
    filter(term %in% c(
      "HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
      "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
      "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
      "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin", 
      #"HW_EHF_TMAX_2d_bin", "HW_EHF_TMAX_3d_bin", "HW_EHF_TMAX_4d_bin",
      #"HW_EHF_TMAX_sev_firstLow", "HW_EHF_TMAX_sev_firstSevere", "HW_EHF_TMAX_sev_firstExtreme",
      "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
      #"HW_EHF_TAD_sev_firstLow",  "HW_EHF_TAD_sev_firstSevere", "HW_EHF_TAD_sev_firstExtreme"
      )) %>% 
    mutate(term = factor(term,
                         levels = c(
                          "HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                          "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                          "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                          "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin",
                          #"HW_EHF_TMAX_2d_bin", "HW_EHF_TMAX_3d_bin", "HW_EHF_TMAX_4d_bin",
                          #"HW_EHF_TMAX_sev_firstLow", "HW_EHF_TMAX_sev_firstSevere", "HW_EHF_TMAX_sev_firstExtreme",
                          "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
                          #"HW_EHF_TAD_sev_firstLow",  "HW_EHF_TAD_sev_firstSevere", "HW_EHF_TAD_sev_firstExtreme"
                                  ),
                         labels = c(
                          "HW-30ºC 2D", "HW-30ºC 3D", "HW-30ºC 4D",
                          "HW-P90 2D", "HW-P90 3D", "HW-P90 4D",
                          "HW-P95 2D", "HW-P95 3D", "HW-P95 4D",
                          "HW-P99 2D", "HW-P99 3D", "HW-P99 4D",
                          #"HW-EHF 2D - TMAX", "HW-EHF 3D - TMAX", "HW-EHF 4D - TMAX",
                          #"HW-EHF Low - TMAX", "HW-EHF Severe - TMAX", "HW-EHF Extreme - TMAX",
                          "HW-EHF 2D", "HW-EHF 3D", "HW-EHF 4D"
                          #"HW-EHF Low - TAD", "HW-EHF Severe - TAD", "HW-EHF Extreme - TAD"
                                  )))
                                      
  text_x_position <- if (dep_var == "birth_very_preterm" || dep_var == "birth_moderately_preterm") {
    1.33 
  } else {
    1.2 
  }

  x_limits <- if (dep_var == "birth_very_preterm" || dep_var == "birth_moderately_preterm") {
    c(0.8, 1.3) 
  } else {
    c(0.8, 1.3) 
  }

  
  p <- ggplot(data_subset, aes(x = estimate, y = term, color = duration_label)) +
    geom_point(size = 3, shape = 15) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_hline(yintercept = 12.5, color = "gray") +
    geom_hline(yintercept = 9.5, color = "gray") +
    geom_hline(yintercept = 6.5, color = "gray") +
    geom_hline(yintercept = 3.5, color = "gray") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    scale_colour_manual(name = "Duration HW:", values = c("#e59866", "#d35400", "#873600")) +
    scale_x_continuous(limits = x_limits) +
    geom_text(aes(x = text_x_position, label = paste0(format(round(estimate, 2), nsmall = 2), " (", 
                                                      format(round(conf.low, 2), nsmall = 2), " - ", 
                                                      format(round(conf.high, 2), nsmall = 2), ")")), 
              position = position_dodge(width = 0.75), size = 3, show.legend = FALSE) + 
    labs(title = NULL,
         x = "HRs and 95% CI", 
         y = "Heatwave Definition", 
         tag = "A.") +
    theme_light() +
    theme(panel.grid = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 11))
  
  #p <- ggarrange(p1, p2, ncol = 2, common.legend = TRUE)
  
  
  plots[[dep_var]] <- p
}

# Save plots

plots$birth_preterm + labs(title = "A. Preterm (<37 weeks)")
plots$birth_very_preterm + labs(title = "B. Very Preterm (28-32 weeks)")
plots$birth_moderately_preterm + labs(title = "C. Moderately Preterm (32-33 weeks)")
plots$birth_late_preterm + labs(title = "D. Late Preterm (34-37 weeks)")

plots_save <- ggarrange(
  plots$birth_preterm + labs(title = "A. Preterm (<37 weeks)"),
  plots$birth_very_preterm + labs(title = "B. Very Preterm (28-32 weeks)"),
  plots$birth_moderately_preterm + labs(title = "C. Moderately Preterm (32-33 weeks)"),
  plots$birth_late_preterm + labs(title = "D. Late Preterm (34-37 weeks)"),
  ncol=2, nrow=2, 
  common.legend = TRUE
)

ggsave(plots_save,
  filename = paste0("Output/", "Models/", "PTB_AFT_SUP", ".png"), 
  res = 300,
  width = 30,
  height = 25,
  units = 'cm',
  scaling = 0.90,
  device = ragg::agg_png)

  ggsave(plots$birth_preterm + labs(title = "A. Preterm (<37 weeks)"),
  filename = paste0("Output/", "Models/", "PTB_AFT", ".png"), # "Preterm_trendsrm1991"
  res = 300,
  width = 20,
  height = 15,
  units = 'cm',
  scaling = 0.90,
  device = ragg::agg_png)

ggsave(plots$birth_very_preterm + labs(title = "B. Very Preterm (28-32 weeks)"),
filename = paste0("Output/", "Models/", "PTB_very_AFT", ".png"), # "Preterm_trendsrm1991"
res = 300,
width = 20,
height = 15,
units = 'cm',
scaling = 0.90,
device = ragg::agg_png)

ggsave(plots$birth_moderately_preterm + labs(title = "C. Moderately Preterm (32-33 weeks)"),
filename = paste0("Output/", "Models/", "PTB_moderate_AFT", ".png"), # "Preterm_trendsrm1991"
res = 300,
width = 20,
height = 15,
units = 'cm',
scaling = 0.90,
device = ragg::agg_png)

ggsave(plots$birth_late_preterm + labs(title = "D. Late Preterm (34-37 weeks)"),
  filename = paste0("Output/", "Models/", "PTB_late_AFT", ".png"), # "Preterm_trendsrm1991"
  res = 300,
  width = 15,
  height = 15,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png)

  # Table with effects

table_models <- results_filtered %>% 
  mutate(HR=paste0(round(estimate, 3), " (", 
                   round(conf.low, 3), "; ",
                   round(conf.high, 3), ")" 
                  )) %>% 
  select(dependent_var, term, HR) %>% 
  pivot_wider(names_from = dependent_var, 
              values_from = HR) %>% 
  mutate(term = factor(term,
                        levels = c("HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                                    "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                                    "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                                    "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin",
                                    "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
                                  ),
                         labels = c("HW-30ºC 2D", "HW-30ºC 3D", "HW-30ºC 4D",
                                    "HW-P90 2D", "HW-P90 3D", "HW-P90 4D",
                                    "HW-P95 2D", "HW-P95 3D", "HW-P95 4D",
                                    "HW-P99 2D", "HW-P99 3D", "HW-P99 4D",
                                    "HW-EHF 2D", "HW-EHF 3D", "HW-EHF 4D")))

colnames(table_models) <- c("HR Definition", 
                            "Preterm (<37)", 
                            "Very Preterm (28-32)", 
                            "Moderate Preterm (32-33)",
                            "Late Preterm (34-37)")

writexl::write_xlsx(table_models, path =  paste0("Output/", "Models/", "Table_AFT_lw", ".xlsx"))

## HR AFT Models LM ---- 

# Variables dependientes y predictoras
dependent_vars <- c("birth_preterm", "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm") # , "birth_term", "birth_posterm"

heatwave_vars <- c("HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                   "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                   "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                   "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin", 
                   "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
                   #"HW_EHF_TMAX_2d_bin", "HW_EHF_TMAX_3d_bin", "HW_EHF_TMAX_4d_bin",
                   #"HW_EHF_TAD_sev_first", "HW_EHF_TMAX_sev_first"
                  )

# Función para ajustar el modelo Weibull AFT y transformar a Hazard Ratios
fit_weibull_aft <- function(dependent, predictor) {
  # Fórmula del modelo
  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(year_nac) + vulnerability"))
  
  # Ajuste del modelo Weibull
  model_fit <- survreg(formula, data = bw_data_lm, dist = "weibull")
  
  # Transformar coeficientes a Hazard Ratios
  scale <- model_fit$scale  # Parámetro de escala
  tidy_model <- tidy(model_fit) %>%
    mutate(HR = exp(-estimate / scale),  # Hazard Ratio
           conf.low = exp(-(estimate + 1.96 * std.error) / scale),  # Límite inferior del IC
           conf.high = exp(-(estimate - 1.96 * std.error) / scale),  # Límite superior del IC
           estimate = round(estimate, 3),
           HR = round(HR, 3),
           conf.low = round(conf.low, 3),
           conf.high = round(conf.high, 3)) %>%
    select(term, HR, std.error, conf.low, conf.high) %>%
    mutate(dependent_var = dependent, predictor = predictor)  
  
  return(tidy_model)
}


# Aplicar la función con manejo de errores
tic()
weibull_results <- map(dependent_vars, function(dep_var) {
  map(heatwave_vars, function(hw_var) {
    tryCatch({
      fit_weibull_aft(dep_var, hw_var)
    }, error = function(e) {
      message(paste("Error con:", dep_var, "y", hw_var, "-", e$message))
      return(NULL)  # Retornar NULL si hay error
    })
  }) %>% bind_rows()
}) %>% bind_rows()
toc()

# Resultados para los modelos Weibull transformados a HRs
weibull_results

writexl::write_xlsx(weibull_results, path =  paste0("Output/", "Models/", "AFT_models_lm", ".xlsx"))

# Plots with HW - Effects

results_filtered <- weibull_results %>%
  filter(term %in% heatwave_vars, dependent_var %in% dependent_vars) %>% 
  rename(estimate=HR)

plots <- list()

for (dep_var in dependent_vars) {
  # Subset data for the current dependent variable
  data_subset <- results_filtered %>% 
    filter(dependent_var == dep_var) %>% 
    mutate(
      duration = str_extract(term, "\\d+d"), 
      duration_label = case_when( 
        duration == "2d" ~ "2 days",
        duration == "3d" ~ "3 days",
        duration == "4d" ~ "4 or more days",
        TRUE ~ NA_character_ 
      ),
      duration_label = factor(duration_label, levels = c("2 days", "3 days", "4 or more days")) 
    )
  
  data_subset <- data_subset %>% 
    filter(term %in% c(
      "HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
      "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
      "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
      "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin", 
      #"HW_EHF_TMAX_2d_bin", "HW_EHF_TMAX_3d_bin", "HW_EHF_TMAX_4d_bin",
      #"HW_EHF_TMAX_sev_firstLow", "HW_EHF_TMAX_sev_firstSevere", "HW_EHF_TMAX_sev_firstExtreme",
      "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
      #"HW_EHF_TAD_sev_firstLow",  "HW_EHF_TAD_sev_firstSevere", "HW_EHF_TAD_sev_firstExtreme"
      )) %>% 
    mutate(term = factor(term,
                         levels = c(
                          "HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                          "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                          "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                          "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin",
                          #"HW_EHF_TMAX_2d_bin", "HW_EHF_TMAX_3d_bin", "HW_EHF_TMAX_4d_bin",
                          #"HW_EHF_TMAX_sev_firstLow", "HW_EHF_TMAX_sev_firstSevere", "HW_EHF_TMAX_sev_firstExtreme",
                          "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
                          #"HW_EHF_TAD_sev_firstLow",  "HW_EHF_TAD_sev_firstSevere", "HW_EHF_TAD_sev_firstExtreme"
                                  ),
                         labels = c(
                          "HW-30ºC 2D", "HW-30ºC 3D", "HW-30ºC 4D",
                          "HW-P90 2D", "HW-P90 3D", "HW-P90 4D",
                          "HW-P95 2D", "HW-P95 3D", "HW-P95 4D",
                          "HW-P99 2D", "HW-P99 3D", "HW-P99 4D",
                          #"HW-EHF 2D - TMAX", "HW-EHF 3D - TMAX", "HW-EHF 4D - TMAX",
                          #"HW-EHF Low - TMAX", "HW-EHF Severe - TMAX", "HW-EHF Extreme - TMAX",
                          "HW-EHF 2D", "HW-EHF 3D", "HW-EHF 4D"
                          #"HW-EHF Low - TAD", "HW-EHF Severe - TAD", "HW-EHF Extreme - TAD"
                                  )))
                                      
  text_x_position <- if (dep_var == "birth_very_preterm" || dep_var == "birth_moderately_preterm") {
    1.33 
  } else {
    1.2 
  }

  x_limits <- if (dep_var == "birth_very_preterm" || dep_var == "birth_moderately_preterm") {
    c(0.8, 1.3) 
  } else {
    c(0.8, 1.3) 
  }

  
  p <- ggplot(data_subset, aes(x = estimate, y = term, color = duration_label)) +
    geom_point(size = 3, shape = 15) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_hline(yintercept = 12.5, color = "gray") +
    geom_hline(yintercept = 9.5, color = "gray") +
    geom_hline(yintercept = 6.5, color = "gray") +
    geom_hline(yintercept = 3.5, color = "gray") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    scale_colour_manual(name = "Duration HW:", values = c("#e59866", "#d35400", "#873600")) +
    scale_x_continuous(limits = x_limits) +
    geom_text(aes(x = text_x_position, label = paste0(format(round(estimate, 2), nsmall = 2), " (", 
                                                      format(round(conf.low, 2), nsmall = 2), " - ", 
                                                      format(round(conf.high, 2), nsmall = 2), ")")), 
              position = position_dodge(width = 0.75), size = 3, show.legend = FALSE) + 
    labs(title = NULL,
         x = "HRs and 95% CI", 
         y = "Heatwave Definition", 
         tag = "A.") +
    theme_light() +
    theme(panel.grid = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 11))
  
  #p <- ggarrange(p1, p2, ncol = 2, common.legend = TRUE)
  
  
  plots[[dep_var]] <- p
}

# Save plots

plots$birth_preterm + labs(title = "A. Preterm (<37 weeks)")
plots$birth_very_preterm + labs(title = "B. Very Preterm (28-32 weeks)")
plots$birth_moderately_preterm + labs(title = "C. Moderately Preterm (32-33 weeks)")
plots$birth_late_preterm + labs(title = "D. Late Preterm (34-37 weeks)")

plots_save <- ggarrange(
  plots$birth_preterm + labs(title = "A. Preterm (<37 weeks)"),
  plots$birth_very_preterm + labs(title = "B. Very Preterm (28-32 weeks)"),
  plots$birth_moderately_preterm + labs(title = "C. Moderately Preterm (32-33 weeks)"),
  plots$birth_late_preterm + labs(title = "D. Late Preterm (34-37 weeks)"),
  ncol=2, nrow=2, 
  common.legend = TRUE
)

ggsave(plots_save,
  filename = paste0("Output/", "Models/", "PTB_AFT_SUP_lm", ".png"), 
  res = 300,
  width = 30,
  height = 25,
  units = 'cm',
  scaling = 0.90,
  device = ragg::agg_png)

  ggsave(plots$birth_preterm + labs(title = "A. Preterm (<37 weeks)"),
  filename = paste0("Output/", "Models/", "PTB_AFT_lm", ".png"), # "Preterm_trendsrm1991"
  res = 300,
  width = 20,
  height = 15,
  units = 'cm',
  scaling = 0.90,
  device = ragg::agg_png)

ggsave(plots$birth_very_preterm + labs(title = "B. Very Preterm (28-32 weeks)"),
filename = paste0("Output/", "Models/", "PTB_very_AFT_lm", ".png"), # "Preterm_trendsrm1991"
res = 300,
width = 20,
height = 15,
units = 'cm',
scaling = 0.90,
device = ragg::agg_png)

ggsave(plots$birth_moderately_preterm + labs(title = "C. Moderately Preterm (32-33 weeks)"),
filename = paste0("Output/", "Models/", "PTB_moderate_AFT_lm", ".png"), # "Preterm_trendsrm1991"
res = 300,
width = 20,
height = 15,
units = 'cm',
scaling = 0.90,
device = ragg::agg_png)

ggsave(plots$birth_late_preterm + labs(title = "D. Late Preterm (34-37 weeks)"),
  filename = paste0("Output/", "Models/", "PTB_late_AFT_lm", ".png"), # "Preterm_trendsrm1991"
  res = 300,
  width = 15,
  height = 15,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png)

  # Table with effects

table_models <- results_filtered %>% 
  mutate(HR=paste0(round(estimate, 3), " (", 
                   round(conf.low, 3), "; ",
                   round(conf.high, 3), ")" 
                  )) %>% 
  select(dependent_var, term, HR) %>% 
  pivot_wider(names_from = dependent_var, 
              values_from = HR) %>% 
  mutate(term = factor(term,
                        levels = c("HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                                    "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                                    "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                                    "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin",
                                    "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
                                  ),
                         labels = c("HW-30ºC 2D", "HW-30ºC 3D", "HW-30ºC 4D",
                                    "HW-P90 2D", "HW-P90 3D", "HW-P90 4D",
                                    "HW-P95 2D", "HW-P95 3D", "HW-P95 4D",
                                    "HW-P99 2D", "HW-P99 3D", "HW-P99 4D",
                                    "HW-EHF 2D", "HW-EHF 3D", "HW-EHF 4D")))

colnames(table_models) <- c("HR Definition", 
                            "Preterm (<37)", 
                            "Very Preterm (28-32)", 
                            "Moderate Preterm (32-33)",
                            "Late Preterm (34-37)")

writexl::write_xlsx(table_models, path =  paste0("Output/", "Models/", "Table_AFT_lm", ".xlsx"))



