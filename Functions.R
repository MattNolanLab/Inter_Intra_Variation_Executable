# Functions
# Converted using knitr:purl() from functions.Rmd in original project repository at https://github.com/MattNolanLab/Inter_Intra_Variation.

#' ---
#' title: "Functions"
#' author: "Matt Nolan"
#' date: "17/05/2018"
#' output: html_document
#' ---
#' 
## ----setup_functions, include=FALSE------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' 
#' # Functions {-}
#' 
#' This document contains functions for code used in later sections.  The functions are removed from individual .Rmd documents so they can be re-used in multiple analyses.
#' 
#' ----------------------- Models ------------------------
#' 
#' Model vs random intercept and slope.
## ----------------------------------------------------------------------
model_to_fit <- function(df) {
  lme4::lmer(value ~ dvlocmm +(1+dvlocmm||id), data = df, REML = FALSE, na.action = na.exclude)
}

#'  - used in Interanimal.rmd
#'  - used in PCA.rmd
#'  
#' 
## ----------------------------------------------------------------------
# Additional models

# Model for uncorrelated random intercept and slope including housing as a fixed effect
model_vsris_housing <- function(df) {
  lme4::lmer(value ~ dvlocmm * housing + (dvlocmm||id), data = df, REML = FALSE, na.action = na.exclude)
}

# Model for uncorrelated random intercept and slope including age as a fixed effect
model_vsris_age <- function(df) {
  df_na_remove <- filter(df, !is.na(age))
  lme4::lmer(value ~ dvlocmm * age + (dvlocmm||id), data = df_na_remove, REML = FALSE, na.action = na.exclude)
}

# Control model with data points without age excluded
model_vsris_age_con <- function(df) {
  df_na_remove <- filter(df, !is.na(age))
  lme4::lmer(value ~ dvlocmm + (dvlocmm||id), data = df_na_remove, REML = FALSE, na.action = na.exclude)
}

# Model for uncorrelated random intercept and slope including sex as a fixed effect
model_vsris_sex <- function(df) {
  df_na_remove <- filter(df, !is.na(sex))
  lme4::lmer(value ~ dvlocmm * sex + (dvlocmm||id), data = df_na_remove, REML = FALSE, na.action = na.exclude)
}

# Model for uncorrelated random intercept and slope for all possible random effects.
model_vsris_all_lmerTest <- function(df) {
  lmerTest::lmer(value ~ dvlocmm + (dvlocmm||id) + (dvlocmm||mlpos) + (dvlocmm||hemi) + (dvlocmm||age)+ (dvlocmm||housing) + (dvlocmm||expr) + (dvlocmm||patchdir) + (dvlocmm||rectime), data = df, REML = FALSE, na.action = na.exclude)
}

model_vsris_all <- function(df) {
  lme4::lmer(value ~ dvlocmm + (dvlocmm||id) + (dvlocmm||mlpos) + (dvlocmm||hemi) + (dvlocmm||age)+ (dvlocmm||housing) + (dvlocmm||expr) + (dvlocmm||patchdir) + (dvlocmm||rectime), data = df, REML = FALSE, na.action = na.exclude)
}

model_vsris_all_PC <- function(df) {
  lmerTest::lmer(value ~ dvlocmm + (dvlocmm||id) + (dvlocmm||mlpos) + (dvlocmm||age)+ (dvlocmm||housing) + (dvlocmm||expr) + (dvlocmm||patchdir), data = df, REML = FALSE, na.action = na.exclude)
}

model_vsris_all_NM <- function(df) {
  lmerTest::lmer(value ~ dvlocmm + (dvlocmm||id) + (dvlocmm||mlpos) + (dvlocmm||hemi) + (dvlocmm||age)+ (dvlocmm||housing) + (dvlocmm||expr) + (dvlocmm||patchdir) + (dvlocmm||rectime), data = df, REML = FALSE, na.action = na.exclude, control = lmerControl(optimizer ="Nelder_Mead"))
}

model_vsris_all_BFGS <- function(df) {
  lmerTest::lmer(value ~ dvlocmm + (dvlocmm||id) + (dvlocmm||mlpos) + (dvlocmm||hemi) + (dvlocmm||age)+ (dvlocmm||housing) + (dvlocmm||expr) + (dvlocmm||patchdir) + (dvlocmm||rectime), data = df, REML = FALSE, na.action = na.exclude, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
}

model_vsris_all_nlminb <- function(df) {
  lmerTest::lmer(value ~ dvlocmm + (dvlocmm||id) + (dvlocmm||mlpos) + (dvlocmm||hemi) + (dvlocmm||age)+ (dvlocmm||housing) + (dvlocmm||expr) + (dvlocmm||patchdir) + (dvlocmm||rectime), data = df, REML = FALSE, na.action = na.exclude, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
}

# Models for uncorrelated random intercept and slope with additional fixed effects
model_vsris_housing <- function(df) {
  lme4::lmer(value ~ dvlocmm +  housing + dvlocmm:housing + (dvlocmm||id), data = df, REML = FALSE, na.action = na.exclude)
}

model_vsris_age <- function(df) {
  lme4::lmer(value ~ dvlocmm +  age + dvlocmm:age + (dvlocmm||id), data = df, REML = FALSE, na.action = na.exclude)
}

model_vsris_ml <- function(df) {
  lme4::lmer(value ~ dvlocmm +  mlpos + dvlocmm:mlpos + (dvlocmm||id), data = df, REML = FALSE, na.action = na.exclude)
}

model_vsris_hemi <-  function(df) {
  lme4::lmer(value ~ dvlocmm +  hemi + dvlocmm:hemi + (dvlocmm||id), data = df, REML = FALSE, na.action = na.exclude)
}

model_vsris_exp <-  function(df) {
  lme4::lmer(value ~ dvlocmm +  expr + dvlocmm:expr + (dvlocmm||id), data = df, REML = FALSE, na.action = na.exclude)
}

model_vsris_dir <-  function(df) {
  lme4::lmer(value ~ dvlocmm + patchdir + dvlocmm:patchdir + (dvlocmm||id), data = df, REML = FALSE, na.action = na.exclude)
}

model_vsris_rect <-  function(df) {
  lme4::lmer(value ~ dvlocmm + rectime + dvlocmm:rectime + (dvlocmm||id), data = df, REML = FALSE, na.action = na.exclude)
}

model_vsris_counts <-  function(df) {
  lme4::lmer(value ~ dvlocmm + counts + dvlocmm:counts + (dvlocmm||id), data = df, REML = FALSE, na.action = na.exclude)
}

model_vsris_full_fixed <- function(df) {
  lme4::lmer(value ~ dvlocmm + expr + housing + mlpos + patchdir + dvlocmm:housing + dvlocmm:expr + dvlocmm:patchdir + dvlocmm:mlpos + (dvlocmm||id), data = df, REML = FALSE, na.action = na.exclude)
}

model_vsris_full_fixed_linear <- function(df) {
  lm(value ~ dvlocmm + expr + housing + mlpos + patchdir + dvlocmm:housing + dvlocmm:expr + dvlocmm:patchdir + dvlocmm:mlpos, data = df, na.action = na.exclude)
}


#' 
#'  
#' Linear models
## ----------------------------------------------------------------------
linearmodel_to_fit <- function(df) {
  lm(value ~ dvlocmm, data = df, na.action = na.exclude)
}

linearmodel_to_fit_1 <- function(df) {
  lm(value ~ dvlocmm1, data = df, na.action = na.exclude)
}

linearmodel_age <- function(df) {
  lm(value ~ dvlocmm * age, data = df, na.action = na.exclude)
}

linearmodel_housing <- function(df) {
  lm(value ~ dvlocmm * housing, data = df, na.action = na.exclude)
}

#' - used in Interanimal.rmd
#' - used in PCA.rmd
#' 
#' 
#' Mixed models fit with nlme
#' Alternative way to fit mixed model using nlme for compatibility with ANOVA.
## ----------------------------------------------------------------------
# Gives error when random term incluces dvlocmm
nlmemodel_to_fit <- function(df) {
  nlme::lme(value ~ dvlocmm, random = ~1|id, data = df, method = "ML", na.action = na.exclude)
}

#' 
#' 
#' ------------------ Helper functions ---------------------
#' 
#' Helper function to return model coefficients in a tidy format.
## ----------------------------------------------------------------------
coef_df <- function(model_name) {
  mod_coef <- coef(model_name)
  tibble(id = row.names(mod_coef[[1]]), intercept = mod_coef[[1]][[1]], slope = mod_coef[[1]][[2]])
}

#' - used in Internanmal.rmd
#' - used in PCA.rmd
#' 
#' Helper function to return model coefficients in a tidy format. This version also use gi to find the global intercept and returns global intercept and global intercept + slope.
## ----------------------------------------------------------------------
coef_df_2 <- function(model_name, gi) {
  mod_coef <- coef(model_name)
  tibble(id = row.names(mod_coef$id), ind_intercept = mod_coef$id[[1]], ind_slope = mod_coef$id[[2]], ind_intercept_slope = mod_coef$id[[1]] + mod_coef$id[[2]], global_intercept = gi, global_intercept_slope = gi + mod_coef$id[[2]])
}

coef_df_2_old <- function(model_name, gi) {
  mod_coef <- coef(model_name)
  tibble(id = row.names(mod_coef[[1]]), ind_intercept = mod_coef[[1]][[1]], ind_slope = mod_coef[[1]][[2]], ind_intercept_slope = mod_coef[[1]][[1]] + mod_coef[[1]][[2]], global_intercept = gi, global_intercept_slope = gi + mod_coef[[1]][[2]])
}

#' 
#' 
#' Helper functions for chi-squared test to compare linear with mixed models.
#' Performs chi-square test to compre models in two list-columns. Returns the dataframe with additional result columns appended.
#' dev_mixed, dev_linear, devdiff, dev_mixed_df, dev_linear_df will be over-written if the function is used more than once on the samde dataframe.
#' 
#' Analysis strategy modified from: https://web.stanford.edu/class/psych252/section/Mixed_models_tutorial.html
## ----------------------------------------------------------------------
devcalc <- function(df){
  dev <- -2*logLik(df)
}

extractdf <- function(dev){
  attr(dev,"df")
}

mixed_vs_linear_pchisqu <- function(df, mixedmod, linearmod){
  mixedmod <- enquo(mixedmod)
  linearmod <- enquo(linearmod)
  dfdiff <- sym(paste0(quo_name(mixedmod), "_vslinear_dfdiff"))
  pdiff <- sym(paste0(quo_name(mixedmod), "_vslinear_pdiff"))

  mutate(df,
         dev_mixed = map(!! mixedmod, devcalc),
         dev_linear = map(!! linearmod, devcalc),
         devdiff = as.numeric(dev_linear) - as.numeric(dev_mixed),
         dev_mixed_df = map(dev_mixed, extractdf),
         dev_linear_df = map(dev_linear, extractdf),
         !! dfdiff := as.numeric(dev_mixed_df) - as.numeric(dev_linear_df),
         !! pdiff := pchisq(devdiff,!! dfdiff,lower.tail=FALSE))
  }

#' 
#' 
#' Functions for presentation of data.
## ----Model plot theme--------------------------------------------------
hist_theme = theme(
    text = element_text(size=9),
    strip.background = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
  )

#' 
## ----PCA plot theme----------------------------------------------------
PCA_theme = theme(
    text = element_text(size=9),
    strip.background = element_blank(),
    axis.title.y=element_blank()
  )

#' 
#' Helper function to normalise a vector
## ----Normalise helper--------------------------------------------------
normalize<-function(m){
   (m - min(m, na.rm = TRUE))/(max(m, na.rm = TRUE)-min(m, na.rm = TRUE))
}

#' 
#' 
#' Helper function to extract fit properties from a column of models stored in a dataframe and then add the output to the dataframe as additional columns.
#' 
#' Use Broom (glance, tidy and augment) as returns values in data frames. Need to use summary to also obtain min, max and median slopes.
#' 
#' df - a dataframe containing columns to work on
#' mm_col - a column of df containing the mixed models fit with lmer
#' 
#' Properties to extract are:
#' Model gradient (extracted with summary / glance), marginal and conditional R2 (extracted with r.squaredGLMM).
## ----Extract properties of models--------------------------------------
mixedmod_extract <- function(df, mm_col){
  mm_col <- enquo(mm_col)
  mm_tidy <- sym(paste0(quo_name(mm_col),"_tidy"))
  mm_aug <- sym(paste0(quo_name(mm_col),"_aug"))
  mm_summary <- sym(paste0(quo_name(mm_col),"_summary"))
  gradient_slopes <- sym(paste0(quo_name(mm_col),"_gradient_slopes"))
  extractR2 <- sym(paste0(quo_name(mm_col),"_extractR2"))
  marginal.r2 <- sym(paste0(quo_name(mm_col),"_marginal.r2"))
  conditional.r2 <- sym(paste0(quo_name(mm_col),"_conditional.r2"))
  mm_simcoefs <- sym(paste0(quo_name(mm_col),"_simcoefs"))
  modelslope_min <- sym(paste0(quo_name(mm_col),"_slope_min"))
  modelslope_median <- sym(paste0(quo_name(mm_col),"_slope_median"))
  modelslope_max <- sym(paste0(quo_name(mm_col),"_slope_max"))
    
  mutate(df,
         !! mm_tidy := map(!! mm_col, broom::tidy),
         !! mm_aug := map(!! mm_col, broom::augment),
         !! mm_summary := map(!! mm_col, summary),
         !! gradient_slopes := map_dbl(!! mm_tidy, ~.$estimate[[2]]),
         !! extractR2 := map(!! mm_col, r.squaredGLMM),
         !! marginal.r2 := map_dbl(!! extractR2, ~.[[1]]),
         !! conditional.r2 := map_dbl(!! extractR2, ~.[[2]]),
         !! mm_simcoefs := map(!! mm_col, ~summary(coef(.x)[[1]][[2]])),
         !! modelslope_min := map_dbl(!! mm_simcoefs, ~.[[1]]),
         !! modelslope_median := map_dbl(!! mm_simcoefs, ~.[[3]]),
         !! modelslope_max := map_dbl(!! mm_simcoefs, ~.[[6]]))
  # To reduce clutter could remove extractR2, mm_tidy and vsris_simcoefs so they don't get returned?
}

#' 
#' 
#' 
#' 
#' Helper function to extract model predictions in a dataframe ready for plotting.
#' prep_int_slopes
#' 
#' Inputs
#' df - a dataframe containing columns to work on
#' group_col - a column of df containing the names of each group
#' mm_col - a column of df containing the mixed models fit with lmer
#' 
#' Returns
#' combined_intercepts_slopes - a data frame to be used to generate the plot
#' 
## ----Extract model predictions for plotting----------------------------
prep_int_slopes <- function(df, group_col, mm_col){

# Use broom::tidy to extract model fit parameters for each feature.
df <- df %>%
  mutate(mm_tidy = map(!! rlang::sym(mm_col), broom::tidy)) %>%
  mutate(pop_intercepts = map_dbl(mm_tidy, ~.$estimate[[1]]))

# Obtain individual intercepts and slopes for each feature as separate columns.
# coef_df is a helper function to return model coefficients in a tidy formt.
# coef_df_2 also calculate I+S, etc.
df <- df %>%
  mutate(coefs = map2(!! rlang::sym(mm_col), pop_intercepts, ~coef_df_2(.x, .y)))

df_unnest <- unnest(df, coefs) %>%
  select(!! rlang::sym(group_col), id, ind_intercept, ind_slope, ind_intercept_slope, global_intercept, global_intercept_slope)

# Make new tibble with model predictions ready for plotting
ind_intercept <- select(df_unnest, !! rlang::sym(group_col), id, ind_intercept) %>%
  mutate(measure = "ind_intercept") %>%
  mutate(value_2 = ind_intercept)

global_intercept <- select(df_unnest, !! rlang::sym(group_col), id, global_intercept) %>%
  mutate(measure = "global_intercept") %>%
  mutate(value_1 = global_intercept)

ind_intercept_slope <- select(df_unnest, !! rlang::sym(group_col), id, ind_intercept_slope) %>%
  mutate(measure = "ind_intercept_slope") %>%
  mutate(value_2 = ind_intercept_slope)

global_intercept_slope <- select(df_unnest, !! rlang::sym(group_col), id, global_intercept_slope) %>%
  mutate(measure = "global_intercept_slope") %>%
  mutate(value_1 = global_intercept_slope)

combined_intercepts_slopes <- bind_rows(ind_intercept, ind_intercept_slope, global_intercept, global_intercept_slope)

}

#' 
#' Helper function to extract model coefficients for PCA at animal level. This is a simplified version of prep_init_slopes above.
## ----------------------------------------------------------------------
prep_int_slopes_PCA <- function(df, group_col, mm_col){

# Use broom::tidy to extract model fit parameters for each feature.
df <- df %>%
  mutate(mm_tidy = map(!! rlang::sym(mm_col), broom::tidy)) %>%
  mutate(pop_intercepts = map_dbl(mm_tidy, ~.$estimate[[1]]))

# Obtain individual intercepts and slopes for each feature as separate columns.
# coef_df is a helper function to return model coefficients in a tidy formt.
# coef_df_2 also calculate I+S, etc.
df <- df %>%
  mutate(coefs = map2(!! rlang::sym(mm_col), pop_intercepts, ~coef_df_2(.x, .y)))

df_unnest <- unnest(df, coefs) %>%
  select(!! rlang::sym(group_col), id, ind_intercept, ind_slope, ind_intercept_slope, global_intercept, global_intercept_slope)

df_unnest
}

#' 
#' 
#' 
#' Helper function for copula transformation of data. Returns ranked data.
## ----------------------------------------------------------------------
edf <- function(x)
{
    n <- length(x)
    rank(x)/(n+1)                       
}

#' 
#' Function for plotting effects estimated with merTools ± SD. 
## ----------------------------------------------------------------------
plotFE_sd <- function(df) {
  df_FE <- merTools::FEsim(df)
  
  df_FE <- df_FE  %>%
  mutate(upper = mean + sd) %>%
  mutate (lower = mean - sd)
  
  ggplot(df_FE[2:4,], aes(term, mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = upper, ymax = lower)) +
  coord_flip()
}

#' 
#' 
#' ## Functions used for analysis of additional fixed effects
#' 
#' Function for adding model summary statistics to the main data frame.
#' Make name strings for fixed effects
#' Add values of names fixed effects to the df
#' 
#' Obtaining ngrps and nobs via summary. Could also do this by calling ngrps(model) and nobs(model), but produces an error that I can't figure out.
## ----------------------------------------------------------------------
summary_2fixedeffects <- function(df, model, name) {
  model <- enquo(model)
  sum_name <- sym(paste0("mm_", quo_name(name), "_sum"))
  ngrps_name <-sym(paste0("mm_", quo_name(name), "_ngrps"))
  nobs_name <- sym(paste0("mm_", quo_name(name), "_nobs"))
  ANOVA_name <- sym(paste0("mm_", quo_name(name), "_ANOVA"))
  dv_name <- sym(paste0("mm_", quo_name(name), "_dv"))
  name_name <- sym(paste0("mm_", quo_name(name), "_", quo_name(name)))
  dv_name_name <- sym(paste0("mm_", quo_name(name), "_dv_", quo_name(name)))
  FE_Int_name <- sym(paste0("mm_", quo_name(name), "_FE_Int"))
  FE_dv_name <- sym(paste0("mm_", quo_name(name), "_FE_dv"))
  FE_FE2_name <- sym(paste0("mm_", quo_name(name), "_FE_", quo_name(name)))
  FE_dv_FE2_name <- sym(paste0("mm_", quo_name(name), "_FE_dv_", quo_name(name)))
  
  mutate(df,
         !! sum_name := map(!! model, summary),
         !! ngrps_name := map_dbl(!!sum_name, ~.$ngrps),
         !! nobs_name := map_dbl(!!sum_name, ~.$devcomp$dims[1]),
         !! ANOVA_name := map(!!model, car::Anova),
         !! dv_name := map_dbl(!!ANOVA_name, ~.$`Pr(>Chisq)`[[1]]),
         !! name_name := map_dbl(!! ANOVA_name, ~.$`Pr(>Chisq)`[[2]]),
         !! dv_name_name := map_dbl(!! ANOVA_name, ~.$`Pr(>Chisq)`[[3]]),
         !! FE_Int_name := map_dbl(!!sum_name, ~.$coefficients[1]),
         !! FE_dv_name := map_dbl(!!sum_name, ~.$coefficients[2]),
         !! FE_FE2_name := map_dbl(!!sum_name, ~.$coefficients[3]),
         !! FE_dv_FE2_name := map_dbl(!!sum_name, ~.$coefficients[4])
  )
  
}

#' 
#' Function to calculate adjusted p values for significance values generated by  summary_2fixedeffects above and to return them in a modified dataframe.
#' **Arguments**
#' df is a dataframe containing columns mm_name_dv, mm_name_name and mm_name_dv_name
#' name is a string that refers to the identidier within the column names of df
#' **Value**
#' df as a modified version of df containing columns with the adjusted p values. Columns end in '_adj'.
## ----------------------------------------------------------------------
FE2_p.adjust <- function(df, name) {
  names_in <- c(paste0("mm_", name, "_dv"), paste0("mm_", name, "_", name), paste0("mm_", name, "_dv_", name))
  names_adj <- paste0(names_in, "_adj")

  df[names_adj] <- lapply(df[names_in], p.adjust, method = "BH")
  df
}

#' 
#' 
#' Function to make table summarising additional fixed effects
#' FE_table(data.sc_r, "rect")
## ----------------------------------------------------------------------
FE_table <- function(df, name, group_name = property) {
  group_name = enquo(group_name)
  ngrps <- sym(paste0("mm_", name, "_ngrps"))
  nobs <- sym(paste0("mm_", name, "_nobs"))
  FE_Int_name <- sym(paste0("mm_",name, "_FE_Int"))
  FE_dv_name <- sym(paste0("mm_", name, "_FE_dv"))
  FE_FE2_name <- sym(paste0("mm_", name, "_FE_", name))
  FE_dv_FE2_name <- sym(paste0("mm_", name, "_FE_dv_", name))
  name_dv <- sym(paste0("mm_", name, "_dv"))
  name_dv_adj <- sym(paste0("mm_", name, "_dv_adj"))
  name_name <- sym(paste0("mm_", name, "_", name))
  name_name_adj <- sym(paste0("mm_", name, "_", name, "_adj"))
  name_dv_name <- sym(paste0("mm_", name, "_dv_", name))
  name_dv_name_adj <- sym(paste0("mm_", name, "_dv_", name, "_adj"))
  
  col_list <- c(rlang::as_string(FE_Int_name),
                rlang::as_string(FE_dv_name),
                rlang::as_string(FE_FE2_name),
                rlang::as_string(FE_dv_FE2_name),
                rlang::as_string(name_dv),
                rlang::as_string(name_dv_adj),
                rlang::as_string(name_name),
                rlang::as_string(name_name_adj),
                rlang::as_string(name_dv_name),
                rlang::as_string(name_dv_name_adj)
                )
  
  df[col_list] <- lapply(df[col_list], format, digits = 2)
  
  df$property <- c("Vm (mV)", "IR (MΩ)", "Sag", "Tm (ms)", "Res. frequency (Hz)", "Res. magnitude", "Spike thresold (mV)", "Spike maximum (mV)", "Spike width (ms)", "Rheobase (pA)", "Spike AHP (mV)", "I-F slope (Hz/pA)")

  kableExtra::kable(select(df,
                           !! group_name,
                           !! ngrps,
                           !! nobs,
                           !! FE_Int_name,
                           !! FE_dv_name,
                           !! FE_FE2_name,
                           !! FE_dv_FE2_name,
                           !! name_dv,
                           !! name_name,
                           !! name_dv_name,
                           !! name_dv_adj,
                           !! name_name_adj,
                           !! name_dv_name_adj),
                    col.names = c("property", "N", "n", "Int", "dvloc", paste0(name), paste0("dv:", name), "dvloc", paste0(name), paste0("dv:", name), "dvloc_adj", paste0(name,"_adj"), paste0("dv:", name, "_adj"))) %>%
    kableExtra::add_header_above(c(" " = 3, "Fixed effects" = 4, "raw p" = 3, "adjusted p" = 3)) %>%
    kableExtra::kable_styling(bootstrap_options = "striped")
}

#' 
#' 
#' Function for plotting all data separated by a factor.
#' Requires a tidy dataframe containing columns vm:fi and then an additional column with the factor. 
#' An example of df is select(data.sc, vm:fi, dvlocmm, hemi).
## ----------------------------------------------------------------------
all_by_fac <- function(df, fac_col) {
  df_g <- df %>%
    gather("property", "value", vm:fi) %>%
    drop_na()
ggplot(df_g, aes(dvlocmm, value, colour = !! rlang::sym(fac_col))) +
  geom_point(alpha=0.2) +
  geom_smooth(method = 'lm') +
  facet_wrap(~property, scales = "free")
}

#' 
#' Plot predictions for model fit for individual animals.
## ----------------------------------------------------------------------
predict_plot <- function(df, model, xparam = dvlocmm, yparam = value) {
  prediction = predict(model)
  df <- cbind(df,prediction)
  
  predictplot <- ggplot(df, aes(x = dvlocmm, y = yparam, group = id)) +
    geom_line(aes(y = prediction))
}

#' 
#' 
#' Plot predictions for model fit for individual animals.
## ----------------------------------------------------------------------
predict_plot_2 <- function(df, model, xparam = dvlocmm, yparam = value) {
  prediction = predict(model)
  df <- cbind(df,prediction)
  
  predictplot <- ggplot(df, aes(x = xparam, y = yparam, group = id)) +
    geom_line(aes(y = prediction))
}

#' 
#' 
#' Format ggplot of input resistance as a function of location.
## ----------------------------------------------------------------------
gg_ir_format <- function(gg, ylim_min = 0, ylim_max = 60) {
  gg <- gg +
    xlab("Location (mm)") +
    ylab("IR (MΩ)") +
    scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5), label = c("0", "", "1", "", "2", "")) +
    scale_y_continuous(limits = c(ylim_min, ylim_max)) +
    theme_classic()
}

#' 
#' Format ggplot of rheobase as a function of location.
## ----------------------------------------------------------------------
gg_rheo_format <- function(gg, ylim_min = 0, ylim_max = 600) {
  gg <- gg +
    xlab("Location (mm)") +
    ylab("Rheobase (pA)") +
    scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5), label = c("0", "", "1", "", "2", "")) +
    scale_y_continuous(limits = c(ylim_min, ylim_max)) +
    theme_classic() 
}

#' 
#' 
#' Format ggplot of resonance frequency as a function of location.
## ----------------------------------------------------------------------
gg_resf_format <- function(gg, ylim_min = 0, ylim_max = 12) {
  gg <- gg +
    xlab("Location (mm)") +
    ylab("F (Hz)") +
    scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5), label = c("0", "", "1", "", "2", "")) +
    scale_y_continuous(limits = c(ylim_min, ylim_max)) +
    theme_classic() 
}

#' 
#' 
#' 
#' Make table to compare fit of mixed effect with linear model. This version requires that the models have already been generated.
#' table_mixedvslinear(data.sc_r, "mm_vsris_vslinear", "property")
## ----------------------------------------------------------------------
table_mixedvslinear <- function(df, name, group_name = "property"){
  p_name <- sym(paste0(quo_name(name),"_pdiff"))
  padj_name <- sym(paste0(quo_name(name),"_pdiff_adj"))
  group_name <- sym(paste0(quo_name(group_name)))
  
  df <- mutate(df,
               dev_mixed_num = map_dbl(dev_mixed, as.numeric),
               dev_linear_num = map_dbl(dev_linear, as.numeric),
               dev_mixed_df_num = map_dbl(dev_mixed_df, as.numeric),
               dev_linear_df_num = map_dbl(dev_linear_df, as.numeric),
               pdiff = !!p_name,
               pdiff_adj = !!padj_name,
               group = !! group_name)

  df <- select(df, group, dev_mixed_num, dev_linear_num, dev_mixed_df_num, dev_linear_df_num, pdiff, pdiff_adj)
  
  df$pdiff <- format(df$pdiff, digits = 3)
  df$pdiff_adj <- format(df$pdiff_adj, digits = 3)
  
kableExtra::kable(df, col.names = c(paste0(quo_name(group_name)), "deviance (mixed)", "deviance (linear)", "df (mixed)", "df (linear)", "p", "p_adj"), digits = c(2,2,2,4,4,10,10)) %>%
  kableExtra::kable_styling(bootstrap_options = "striped")
}

#' 
#' Second version of function to generate table to compare fit of mixed effect with linear model. This version fits a mixed model with random intercept and slope and an equivalent linear model, tests whether the models are different and then returns a table summarising the test results. It does not save the models.
#' table_mixedvslinear_2(data.sc_r_f2)
## ----------------------------------------------------------------------
table_mixedvslinear_2 <- function(df){
  df <- df %>%
      mutate(mm = map(data, model_vsris),
             lin = map(data, linearmodel_to_fit),
             nobs = map_dbl(mm, nobs),
             ngrps = map_dbl(mm, ngrps),
             extractR2 = map(mm, r.squaredGLMM),
             marginal.R2 = map_dbl(extractR2, ~.[[1]]),
             conditional.R2 = map_dbl(extractR2, ~.[[2]])) %>%
    mixed_vs_linear_pchisqu(mm, lin)

df$mm_vslinear_pdiff_adj <- p.adjust(df$mm_vslinear_pdiff, method = "BH")
df <- select(df, property, nobs, ngrps, marginal.R2, conditional.R2, dev_mixed, dev_linear, dev_mixed_df, dev_linear_df, mm_vslinear_pdiff, mm_vslinear_pdiff_adj)

df[c("dev_mixed", "dev_linear", "mm_vslinear_pdiff", "mm_vslinear_pdiff_adj")] <- lapply(df[c("dev_mixed", "dev_linear", "mm_vslinear_pdiff", "mm_vslinear_pdiff_adj")], format, digits =3)

kableExtra::kable(df, col.names = c("Property", "Nobs", "Ngrps", "marginal.R2", "Conditional.R2", "Dev(mixed)", "Dev(linear)", "Dev_DF (mixed)", "Dev_DF (linear)", "p", "p(adj)")) %>%
  kableExtra::kable_styling(bootstrap_options = "striped")
}

#' 
#' 
#' ## Functions related to PCA
#' 
#' Function to make ggplot version biplot. Input is a data frame containing rotation from prcomp, e.g. pca_biplot(as.data.frame(all.pca$rotation)).
#' 
#' Fontsize specifes the size of the fonts.
#' 
#' order_names and row_names can be used to rename the rows. order_names is a list of the row names in the desired order. new_names is a list of the names to be displayed on the figure.
## ----------------------------------------------------------------------
pca_biplot <- function(df, fontsize = 10, order_names = c("X"), new_names = c("X")) {
  if (order_names != "X") {
    df <- df[match(rownames(df), order_names)]
  }

  if (new_names == c("X")) {row_names <- row.names(df)} else
  {row_names <- new_names}

  ggplot(df,aes(x=PC1,y=PC2,color=row.names(df))) +
    geom_segment(aes(xend = 0, yend = 0), arrow = arrow(length = unit(0.03, "npc"), ends="first")) +
    geom_text(aes(label=row_names),hjust=0.5, vjust=1, size = fontsize, check_overlap = TRUE, nudge_y = 0.05) +
    theme_classic() +
    theme(legend.position="none")
}

#' 
#' 
#' 
#' Helper function for saving html and pdf files. Note order of saving pdf and html files is important here.
## ----------------------------------------------------------------------
save_kable_jpg_html <- function(df, file) {
  kableExtra::save_kable(df, file = paste0(file, ".jpg"), self_contained = T)
  kableExtra::save_kable(df, file = paste0(file, ".html"), self_contained = T)
}

#' 
#' 
#' Function to return a matrix of significant partial correlations.
#' Significance is tested using a bootstrap method.
#' Returns results as a data frame (Q_neurons).
#' To use with corrplot, convert to a matrix:
#' # Q <- as.matrix(Q_neurons)
#' # colnames(Q) <- colnames(df)
#' # rownames(Q) <- colnames(df)
## ----------------------------------------------------------------------
calcQ <- function(df) {
  Rho_neurons     <- cor(df)
  tol <- 0
  Q_neurons       <- corpcor::cor2pcor(Rho_neurons)
  Q_neurons       <- Q_neurons %>% as.data.frame
  
  ## bootstrap, resampling rows of data matrix
  M <- 1000
  index <- 1:nrow(df)

  Q_neurons_star <- array(dim=c(ncol(df), ncol(df), M))
  tmp <- NULL
  for(i in 1:M){
    index_star <- base::sample(x=index, length(index), replace=TRUE)
    Rho_neurons_star <- cor(df[index_star,])
    Q_neurons_star[,,i] <- as.matrix(corpcor::cor2pcor(Rho_neurons_star))
    tmp <- NULL
  }
  
  Q_neurons_low <- apply(Q_neurons_star, c(1,2), quantile, 0.025)
  Q_neurons_upp <- apply(Q_neurons_star, c(1,2), quantile, 0.975)    
  CI <- Q_neurons_low * Q_neurons_upp
  CI[CI<0]  <- 0
  CI[CI!=0] <- 1
  CI <- as.data.frame(CI)
  Q_neurons <- CI*Q_neurons
  
  return(Q_neurons)
}

#' 
