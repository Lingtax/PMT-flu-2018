## ----load_packages, include = FALSE--------------------------------------
library("papaja")
library(knitr)
library(readr)
library(dplyr)
library(broom)
library(here)
library(ggplot2)
library(labelled)
library(tableone)
library(yhat)

invert_likert <- function(x, maxval = 5) {
  (maxval+1) - x # reverse scores items collected on a 1 to 5 likert scale
}

## ----setglobal, cache = FALSE, include = TRUE----------------------------

opts_knit$set(root.dir = "..")

options(knitr.kable.NA = '')

## ----load_data-----------------------------------------------------------
flu <- read_rds(here("data", "fludat.rds"))

## ----demographics, results='asis', warning=FALSE-------------------------

flu <- flu %>% 
  set_variable_labels(
    age = "Age in years",
    education = "Highest level of education",
    gender = "Gender",
    household_income = "Annual household income", 
    ideology_general = "Ideology - General", 
    ideology_economic = "Ideology - Economic",
    ideology_social = "Ideology - Social")

tab <- CreateTableOne(vars = c("age", 
                             "education",
                             "gender",
                             "household_income", 
                             "ideology_general", 
                             "ideology_economic", 
                             "ideology_social"), 
                    data = flu, test = FALSE)

tab <- print(tab, varLabels = T, printToggle = FALSE)

papaja::apa_table(tab, caption="Demographic characteristics of participants", align = "lc")

## ----correlation, results='asis'-----------------------------------------
cor_t <- flu %>% 
  select(flu_int, flu_mrr, flu_sev,  flu_sus, flu_re, flu_rc, flu_se) %>% 
  apaTables::apa.cor.table()

cor_t <- as.data.frame(cor_t$table.body) %>% 
  mutate(
    Variable = as.character(Variable),
    Variable = case_when(Variable == "1. flu_int" ~ "1. Intention",  
                               Variable == "2. flu_mrr" ~ "2. Maladaptive response rewards", 
                               Variable == "3. flu_sev" ~ "3. Severity",  
                               Variable == "4. flu_sus"  ~ "4. Susceptibility",  
                               Variable == "5. flu_re"  ~ "5. Response Efficacy",   
                               Variable == "6. flu_rc"  ~ "6. Response Costs",   
                               Variable == "7. flu_se"  ~ "7. Self-Efficacy",
                               TRUE ~ Variable))

apa_table(filter(cor_t , Variable != " "), row.names = FALSE, caption = "Correlations between PMT variables")

## ----regression, results='asis', warning=FALSE, message=FALSE------------
pmt_flu <-  lm(flu_int ~ flu_re + flu_sus + flu_mrr + flu_sev + flu_se + flu_rc, data = flu)

pmt_out <- yhat::regr(pmt_flu)
t <- t(pmt_out$Structure_Coefficients)
t <- c(NA, t)
pmt_output <- apa_print(pmt_flu)

pmt_output$table <- as.tbl(pmt_output$table) %>% 
  mutate(
    predictor = as.character(predictor),
    predictor = case_when(predictor == "Flu mrr" ~ "Maladaptive Response Rewards",  
                               predictor == "Flu sev" ~ "Severity", 
                               predictor == "Flu sus" ~ "Susceptibility",  
                               predictor == "Flu re"  ~ "Response Efficacy",  
                               predictor == "Flu rc"  ~ "Response Costs",   
                               predictor == "Flu se"  ~ "Self Efficacy",
                               TRUE ~ predictor),
    `$\\beta$` = lm.beta::lm.beta(pmt_flu)$standardized.coefficients, 
    `Structure \ncoefficients` = t) %>% 
  select(Predictor = predictor, `$\\beta$`, estimate, ci, `Structure \ncoefficients`, statistic, p.value)


apa_table(pmt_output$table, escape = FALSE, note = paste(pmt_output$full_result$modelfit$r2, "\n \nPredictors are presented in order of strength as determined by $\\beta$ weights"), caption = "Linear regression of vaccination intention on PMT constructs.", align = "lccccc")


## ----regressiondemo, results='asis', warning=FALSE, message=FALSE--------
pmt_flu_demo <-  lm(flu_int ~ flu_mrr + flu_sev + flu_sus + flu_re + flu_rc + flu_se + age + gender + education + household_income + ideology_general, data = flu)

pmt_output <- apa_print(pmt_flu_demo)


## ----create_r-references-------------------------------------------------
r_refs(file = "r-references.bib")

