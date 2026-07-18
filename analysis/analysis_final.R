
#'
#' Analyses for CogSci 2026 final submission
#'



# INIT ----
rm(list = ls())
library(brms)
library(here)
library(patchwork)
library(tidyverse)



# FIGURE GLOBALS ----

# Condition globals
QUESTION_CATEGORY_COLORS = c(
  'free chat' = '#e8ad0f',
  'deep questions' = '#C8A2C8',
  'small talk' = '#8FB0A9'
)

QUESTION_CATEGORY_LEVELS = c(
  'free chat', 'small talk', 'deep questions'
)

# Figure theme
DEFAULT_THEME = theme(
  plot.title = element_text(size = 32, family = 'Open Sans', margin = margin(b = 0.5, unit = 'line')),
  axis.title.y = element_text(size = 34, family = 'Open Sans', margin = margin(r = 0.5, unit = 'line')),
  axis.title.x = element_text(size = 30, family = 'Open Sans', margin = margin(t = 0.5, unit = 'line')),
  legend.title = element_text(size = 40, family = 'Open Sans'),
  axis.text.x = element_text(size = 20, angle = 0, vjust = 1, family = 'Open Sans', margin = margin(t = 0.5, unit = 'line'), color = 'black'),
  axis.text.y = element_text(size = 36, family = 'Open Sans', margin = margin(r = 0.5, unit = 'line'), color = 'black'),
  legend.text = element_text(size = 20, family = 'Open Sans', margin = margin(b = 0.5, unit = 'line')),
  strip.text = element_text(size = 24, family = 'Open Sans'),
  panel.background = element_blank(),
  strip.background = element_blank(),
  panel.grid = element_line(color = 'gray'),
  axis.line = element_line(color = 'black'),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = 'bottom',
  legend.key = element_rect(colour = 'transparent', fill = 'transparent')
)



# READ DATA ----

setwd(here())
DATA_DIR = 'data'

combined_data = read_csv(here(DATA_DIR, 'conversation', 'predictions_combined.csv'))
combined_messages = read_csv(here(DATA_DIR, 'conversation', 'chat_messages_combined.csv'))
priors_data = read_csv(here(DATA_DIR, 'priors', 'scale_priors_processed.csv'))



# PROCESSING: update dataframes ----

# > 1. Filter priors df to only include BFI items ----
priors_processed = priors_data |>
  filter(scaleCategory == 'personality')


# > 2. Rename conditions to match pre-registration / text ----

# Update combined data
combined_data = combined_data |>
  mutate(
    condition = case_when(
      condition == 'baseline' ~ 'free chat',
      condition == 'deep' ~ 'deep questions',
      condition == 'shallow' ~ 'small talk',
      TRUE ~ condition
    )
  )
combined_data$condition = factor(
  combined_data$condition,
  levels = c('free chat', 'small talk', 'deep questions')
)
# Update chat data
combined_messages = combined_messages |>
  mutate(
    condition = case_when(
      condition == 'baseline' ~ 'free chat',
      condition == 'deep' ~ 'deep questions',
      condition == 'shallow' ~ 'small talk'
    )
  )
combined_messages$condition = factor(
  combined_messages$condition,
  levels = c('free chat', 'small talk', 'deep questions')
)


# > 3. Add columns for partner prolificId and partner (predicted) responses ----

# Split data into self and partner predictions, get group member lookup
self_data = combined_data |>
  filter(target == 'self')
partner_preds = combined_data |>
  filter(target == 'partner')
group_members = combined_data |>
  select(groupId, prolificId) |>
  arrange(groupId, prolificId) |>
  distinct()
# Add partner ID to self responses
self_data = self_data |>
  left_join(
    group_members,
    by = 'groupId',
    relationship = 'many-to-many'
  ) |>
  filter(
    prolificId.x != prolificId.y
  ) |>
  rename(
    prolificId = prolificId.x,
    partnerId = prolificId.y
  )

# Join partner predictions to self responses by matching partner ID
self_partner_data = self_data |>
  left_join(
    partner_preds |> select(prolificId, scale_id, response),
    by = join_by(partnerId == prolificId, scale_id)
  ) |>
  rename(
    self_response = response.x,
    partner_prediction = response.y
  )


# > 4. Add trait-level information ----
extraversion_scales = c(
  'I see myself as someone who is talkative',
  'I see myself as someone who is reserved',
  'I see myself as someone who is full of energy',
  'I see myself as someone who generates a lot of enthusiasm',
  'I see myself as someone who has an assertive personality',
  'I see myself as someone who is sometimes shy, inhibited',
  'I see myself as someone who is outgoing, sociable',
  'I see myself as someone who tends to be quiet'
)
agreeableness_scales = c(
  'I see myself as someone who tends to find fault with others',
  'I see myself as someone who is helpful and unselfish with others',
  'I see myself as someone who starts quarrels with others',
  'I see myself as someone who has a forgiving nature',
  'I see myself as someone who is generally trusting',
  'I see myself as someone who can be cold and aloof',
  'I see myself as someone who is considerate and kind to almost everyone',
  'I see myself as someone who is sometimes rude to others',
  'I see myself as someone who likes to cooperate with others'
)
conscientiousness_scales = c(
  'I see myself as someone who does a thorough job',
  'I see myself as someone who can be somewhat careless',
  'I see myself as someone who is a reliable worker',
  'I see myself as someone who tends to be disorganized',
  'I see myself as someone who tends to be lazy',
  'I see myself as someone who perseveres until the task is finished',
  'I see myself as someone who does things efficiently',
  'I see myself as someone who makes plans and follows through with them',
  'I see myself as someone who is easily distracted'
)
neuroticism_scales = c(
  'I see myself as someone who is depressed, blue',
  'I see myself as someone who is relaxed, handles stress well',
  'I see myself as someone who can be tense',
  'I see myself as someone who worries a lot',
  'I see myself as someone who is emotionally stable, not easily upset',
  'I see myself as someone who can be moody',
  'I see myself as someone who remains calm in tense situations',
  'I see myself as someone who gets nervous easily'
)
openness_scales = c(
  'I see myself as someone who is original, comes up with new ideas',
  'I see myself as someone who is curious about many different things',
  'I see myself as someone who is ingenious, a deep thinker',
  'I see myself as someone who is inventive',
  'I see myself as someone who values artistic, aesthetic experiences',
  'I see myself as someone who likes to reflect, play with ideas',
  'I see myself as someone who has few artistic interests',
  'I see myself as someone who prefers work that is routine',
  'I see myself as someone who is sophisticated in art, music, or literature',
  'I see myself as someone who has an active imagination'
)

# Get trait lookup for scales
scale_mapping = self_partner_data |>
  select(scale_id, scale_text) |>
  distinct() |>
  mutate(
    Big5_Dimension = case_when(
      scale_text %in% extraversion_scales ~ 'Extraversion',
      scale_text %in% agreeableness_scales ~ 'Agreeableness',
      scale_text %in% conscientiousness_scales ~ 'Conscientiousness',
      scale_text %in% neuroticism_scales ~ 'Neuroticism',
      scale_text %in% openness_scales ~ 'Openness',
      TRUE ~ 'NA'
    )
  )

# Add trait information to self_partner_data
self_partner_data = self_partner_data |>
  left_join(
    scale_mapping,
    by = c('scale_id', 'scale_text')
  )


# > 5. Update responses, predictions, and prior predictions for reverse coded items ----

# Lookup for reverse-coded scales
reverse_coded_scales = c(
  # Extraversion
  'I see myself as someone who is reserved',
  'I see myself as someone who tends to be quiet',
  'I see myself as someone who is sometimes shy, inhibited',
  # Agreeableness
  'I see myself as someone who tends to find fault with others',
  'I see myself as someone who starts quarrels with others',
  'I see myself as someone who can be cold and aloof',
  'I see myself as someone who is sometimes rude to others',
  # Conscientiousness
  'I see myself as someone who can be somewhat careless',
  'I see myself as someone who tends to be disorganized',
  'I see myself as someone who tends to be lazy',
  'I see myself as someone who is easily distracted',
  # Neuroticism
  'I see myself as someone who is relaxed, handles stress well',
  'I see myself as someone who is emotionally stable, not easily upset',
  'I see myself as someone who remains calm in tense situations',
  # Openness
  'I see myself as someone who prefers work that is routine',
  'I see myself as someone who has few artistic interests'
)

# Add reverse coded values to self_partner_data
self_partner_data = self_partner_data |>
  mutate(
    reverse_item = ifelse(
      scale_text %in% reverse_coded_scales,
      TRUE,
      FALSE
    ),
    self_response_coded = ifelse(
      scale_text %in% reverse_coded_scales,
      100 - self_response,
      self_response
    ),
    partner_prediction_coded = ifelse(
      scale_text %in% reverse_coded_scales,
      100 - partner_prediction,
      partner_prediction
    ),
  )

# Add reverse coded values to priors data
priors_processed = priors_processed |>
  mutate(
    reverse_item = ifelse(
      scaleText %in% reverse_coded_scales,
      TRUE,
      FALSE
    ),
    priorResponse_coded = ifelse(
      scaleText %in% reverse_coded_scales,
      100 - priorResponse,
      priorResponse
    )
  )


# > 6. Compute error values ----

# Add absolute, squared, signed error and variance of predictions
self_partner_data = self_partner_data |>
  mutate(
    abs_error = abs(partner_prediction_coded - self_response_coded),
    sq_error = (partner_prediction_coded - self_response_coded)^2,
    signed_error = partner_prediction_coded - self_response_coded
  )

# By condition: mean prediction error + 95% CIs
error_by_condition = self_partner_data |>
  group_by(condition, groupId, prolificId) |>
  summarize(
    mean_abs_error = mean(abs_error, na.rm = TRUE),
    mean_sq_error = mean(sq_error, na.rm = TRUE),
    rmse = sqrt(mean(sq_error, na.rm = TRUE)),
    mean_signed_error = mean(signed_error, na.rm = TRUE),
    prediction_variance = var(partner_prediction_coded, na.rm = TRUE),
    prediction_sd = sd(partner_prediction_coded, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  group_by(condition) |>
  summarize(
    mean_abs_error = list(Hmisc::smean.cl.boot(mean_abs_error)),
    mean_sq_error = list(Hmisc::smean.cl.boot(mean_sq_error)),
    rmse = list(Hmisc::smean.cl.boot(rmse)),
    mean_signed_error = list(Hmisc::smean.cl.boot(mean_signed_error)),
    mean_var = list(Hmisc::smean.cl.boot(prediction_variance)),
    mean_sd = list(Hmisc::smean.cl.boot(prediction_sd)),
    .groups = 'drop'
  ) |>
  tidyr::unnest_wider(
    col = c('mean_abs_error', 'mean_sq_error', 'rmse', 'mean_signed_error', 'mean_var', 'mean_sd'),
    names_sep = '_'
  )



# PROCESSING: priors bootstrapping ----

# Number of boostrapped samples
SAMPLES = 1000

# Initialize mean bootstrapped errors overall
sampled_error_overall = data.frame(
  sample_idx = numeric(),
  mean_abs_error = numeric(),
  mean_sq_error = numeric(),
  mean_rmse = numeric(),
  mean_signed_error = numeric(),
  mean_var = numeric(),
  mean_sd = numeric(),
  n_obs = numeric()
)
# Initialize mean bootstrapped errors by trait
sampled_error_trait = data.frame(
  sample_idx = numeric(),
  trait = character(),
  mean_abs_error = numeric(),
  mean_sq_error = numeric(),
  mean_rmse = numeric(),
  mean_signed_error = numeric(),
  mean_var = numeric(),
  mean_sd = numeric(),
  n_obs = numeric()
)

# Run bootstrap resampling
set.seed(123)
for (i in 1:SAMPLES) {
  # Print status
  if (i %% 100 == 0) {
    print(paste0('Bootstrap sample ', i, ' / ', SAMPLES))
  }
  # Sample prior prediction participants and assign to self-participants
  sampled_priors = self_partner_data |>
    group_by(condition, groupId, prolificId) |>
    mutate(
      sampled_prolificId = sample(unique(priors_processed$prolificID), 1, replace = TRUE)
    ) |>
    ungroup() |>
    left_join(
      priors_processed |>
        select(prolificID, scaleText, priorResponse_coded),
      by = join_by(sampled_prolificId == prolificID, scale_text == scaleText)
    ) |>
    rename(
      sampled_priorResponse = priorResponse_coded
    ) |>
    mutate(
      sampled_prior_abs_error = abs(sampled_priorResponse - self_response_coded),
      sampled_prior_sq_error = (sampled_priorResponse - self_response_coded)^2,
      sampled_prior_signed_error = sampled_priorResponse - self_response_coded
    )
  # Update error summary overall
  sampled_error_overall = rbind(
    sampled_error_overall,
    sampled_priors |>
      group_by(groupId, prolificId) |>
      summarize(
        subj_mean_abs_error = mean(sampled_prior_abs_error, na.rm = TRUE),
        subj_mean_sq_error = mean(sampled_prior_sq_error, na.rm = TRUE),
        subj_rmse = sqrt(mean(sampled_prior_sq_error, na.rm = TRUE)),
        subj_mean_signed_error = mean(sampled_prior_signed_error, na.rm = TRUE),
        subj_prediction_variance = var(sampled_priorResponse, na.rm = TRUE),
        subj_prediction_sd = sd(sampled_priorResponse, na.rm = TRUE),
        .groups = 'drop'
      ) |>
      summarize(
        mean_abs_error = mean(subj_mean_abs_error),
        mean_sq_error = mean(subj_mean_sq_error),
        mean_rmse = mean(subj_rmse),
        mean_signed_error = mean(subj_mean_signed_error),
        mean_var = mean(subj_prediction_variance),
        mean_sd = mean(subj_prediction_sd),
        n_obs = n()
      ) |>
      mutate(
        sample_idx = i
      )
  )
  # Update error summary by trait
  sampled_error_trait = rbind(
    sampled_error_trait,
    sampled_priors |>
      group_by(Big5_Dimension, groupId, prolificId) |>
      summarize(
        subj_mean_abs_error = mean(sampled_prior_abs_error, na.rm = TRUE),
        subj_mean_sq_error = mean(sampled_prior_sq_error, na.rm = TRUE),
        subj_rmse = sqrt(mean(sampled_prior_sq_error, na.rm = TRUE)),
        subj_mean_signed_error = mean(sampled_prior_signed_error, na.rm = TRUE),
        subj_prediction_variance = var(sampled_priorResponse, na.rm = TRUE),
        subj_prediction_sd = sd(sampled_priorResponse, na.rm = TRUE),
        .groups = 'drop'
      ) |>
      group_by(Big5_Dimension) |>
      summarize(
        mean_abs_error = mean(subj_mean_abs_error),
        mean_sq_error = mean(subj_mean_sq_error),
        mean_rmse = mean(subj_rmse),
        mean_signed_error = mean(subj_mean_signed_error),
        mean_var = mean(subj_prediction_variance),
        mean_sd = mean(subj_prediction_sd),
        n_obs = n(),
        .groups = 'drop'
      ) |>
      mutate(
        sample_idx = i
      )
  )
}

# Summarize bootstrapped errors overall
sampled_error_summary = sampled_error_overall |>
  summarize(
    mean_abs_error = list(Hmisc::smean.cl.boot(mean_abs_error)),
    mean_sq_error = list(Hmisc::smean.cl.boot(mean_sq_error)),
    mean_rmse = list(Hmisc::smean.cl.boot(mean_rmse)),
    mean_signed_error = list(Hmisc::smean.cl.boot(mean_signed_error)),
    mean_var = list(Hmisc::smean.cl.boot(mean_var)),
    mean_sd = list(Hmisc::smean.cl.boot(mean_sd))
  ) |>
  tidyr::unnest_wider(
    col = c('mean_abs_error', 'mean_sq_error', 'mean_rmse', 'mean_signed_error', 'mean_var', 'mean_sd'),
    names_sep = '_'
  )
# Summarize bootstrapped errors by trait
sampled_error_trait_summary = sampled_error_trait |>
  group_by(Big5_Dimension) |>
  summarize(
    mean_abs_error = list(Hmisc::smean.cl.boot(mean_abs_error)),
    mean_sq_error = list(Hmisc::smean.cl.boot(mean_sq_error)),
    mean_rmse = list(Hmisc::smean.cl.boot(mean_rmse)),
    mean_signed_error = list(Hmisc::smean.cl.boot(mean_signed_error)),
    mean_var = list(Hmisc::smean.cl.boot(mean_var)),
    mean_sd = list(Hmisc::smean.cl.boot(mean_sd)),
    .groups = 'drop'
  ) |>
  tidyr::unnest_wider(
    col = c('mean_abs_error', 'mean_sq_error', 'mean_rmse', 'mean_signed_error', 'mean_var', 'mean_sd'),
    names_sep = '_'
  )



# SUMMARY: participant demographics ----

# Dyads in each study/condition
combined_data |>
  group_by(condition) |>
  summarize(n_distinct(groupId))

# Gender
combined_data |>
  group_by(gender) |>
  summarize(
    n = n_distinct(prolificId)
  ) |>
  mutate(
    pct = round(100 * (n / sum(n)), 0)
  ) |>
  arrange(
    desc(pct)
  )

# Age
combined_data |>
  summarize(
    age_mean = round(mean(age, na.rm = T), 0),
    age_sd = round(sd(age, na.rm = T), 0),
    age_min = round(min(age, na.rm = T), 0),
    age_max = round(max(age, na.rm = T), 0),
  )

# Race
combined_data |>
  group_by(race) |>
  summarize(
    n = n_distinct(prolificId)
  ) |>
  mutate(
    pct = round(100 * (n / sum(n)), 0)
  ) |>
  arrange(
    desc(pct)
  )

# Priors study
priors_data |>
  summarize(
    n = n_distinct(prolificID)
  )



# TABLE: chat descriptive stats ----

# Total questions by dyad
combined_messages |>
  # T/F in question prompting condition, NA in baseline
  filter(isQuestionPrompt == TRUE) |>
  # Summarize by dyad
  group_by(condition, group_id) |>
  summarize(
    total_Qs = n(),
    .groups = 'drop'
  ) |>
  group_by(condition) |>
  summarize(
    question_count_mean = mean(total_Qs),
    question_count_sd = sd(total_Qs),
    .groups = 'drop'
  )

# Total words and total messages by dyad
combined_messages |>
  # T/F in question prompting condition, NA in baseline
  filter(is.na(isQuestionPrompt) | isQuestionPrompt == FALSE) |>
  mutate(word_count = str_count(message_string, '\\S+')) |>
  # Summarize by dyad
  group_by(condition, group_id) |>
  summarize(
    total_messages = n(),
    total_words = sum(word_count, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  group_by(condition) |>
  summarize(
    message_count_mean = mean(total_messages),
    message_count_sd = sd(total_messages),
    word_count_mean = mean(total_words),
    word_count_sd = sd(total_words),
    .groups = 'drop'
  )



# MODEL FITS: brms ----

# Random effects only
model_null = brm(
  formula = bf(signed_error ~ 1 +
                 (1 | groupId / prolificId) + # optimal nested structure
                 (1 | scale_id),
               sigma ~ 1),
  data = self_partner_data,
  family = gaussian(),
  file = 'analysis/brms_fits/null_model',
  iter = 4000, # NB: increasing from default to help with divergent transitions
  control = list(adapt_delta = 0.9), # NB: increasing from default to help with divergent transitions in nested random effects
  seed = 1
)

# Condition only
model_condition = brm(
  formula = bf(signed_error ~ 1 + condition +
                 (1 | groupId / prolificId) +
                 (1 | scale_id),
               sigma ~ condition),
  data = self_partner_data,
  family = gaussian(),
  file = 'analysis/brms_fits/condition',
  iter = 4000, # NB: increasing from default to help with divergent transitions
  control = list(adapt_delta = 0.98), # NB: increasing from default to help with divergent transitions in nested random effects
  seed = 1
)

# Trait only
model_trait = brm(
  formula = bf(signed_error ~ 1 + Big5_Dimension +
                 (1 | groupId / prolificId) +
                 (1 | scale_id),
               sigma ~ Big5_Dimension),
  data = self_partner_data,
  family = gaussian(),
  file = 'analysis/brms_fits/trait',
  iter = 5000, # NB: increasing from default to help with divergent transitions
  control = list(adapt_delta = 0.99), # NB: increasing from default to help with divergent transitions in nested random effects
  seed = 1
)

# Condition + Trait
model_condition_trait = brm(
  formula = bf(signed_error ~ 1 + condition + Big5_Dimension +
                 (1 | groupId / prolificId) +
                 (1 | scale_id),
               sigma ~ condition + Big5_Dimension),
  data = self_partner_data,
  family = gaussian(),
  file = 'analysis/brms_fits/condition_trait',
  iter = 5000, # NB: increasing from default to help with divergent transitions
  control = list(adapt_delta = 0.98), # NB: increasing from default to help with divergent transitions in nested random effects
  seed = 1
)

# Condition * Trait
model_condition_trait_int = brm(
  formula = bf(signed_error ~ 1 + condition * Big5_Dimension +
                 (1 | groupId / prolificId) +
                 (1 | scale_id),
               sigma ~ condition * Big5_Dimension),
  data = self_partner_data,
  family = gaussian(),
  file = 'analysis/brms_fits/condition_trait_int',
  iter = 5000, # NB: increasing from default to help with divergent transitions
  control = list(adapt_delta = 0.95), # NB: increasing from default to help with divergent transitions in nested random effects
  seed = 1
)

# Leave-one-out cross-validation
model_null = add_criterion(
  model_null,
  criterion = 'loo',
  reloo = T,
  file = 'analysis/brms_fits/null_model'
)
model_condition = add_criterion(
  model_condition,
  criterion = 'loo',
  reloo = T,
  file = 'analysis/brms_fits/condition'
)
model_trait = add_criterion(
  model_trait,
  criterion = 'loo',
  reloo = T,
  file = 'analysis/brms_fits/trait'
)
model_condition_trait = add_criterion(
  model_condition_trait,
  criterion = 'loo',
  reloo = T,
  file = 'analysis/brms_fits/condition_trait'
)
model_condition_trait_int = add_criterion(
  model_condition_trait_int,
  criterion = 'loo',
  reloo = T,
  file = 'analysis/brms_fits/condition_trait_int'
)



# ANALYSIS: Model comparison: impact of condition and trait ----

# Pairwise comparisons (lower == worse fit)
loo_compare(model_null, model_condition) # condition > null?
loo_compare(model_null, model_trait) # trait > null?
loo_compare(model_condition, model_condition_trait) # condition + trait > condition?
loo_compare(model_trait, model_condition_trait) # condition + trait > trait?
loo_compare(model_condition_trait, model_condition_trait_int) # condition * trait > condition + trait?
loo_compare(model_trait, model_condition_trait_int) # condition * trait > trait?

# NB: this ordering can be slightly different from pairwise orderings above
loo_compare(
  model_null,
  model_condition,
  model_trait,
  model_condition_trait,
  model_condition_trait_int
)



# ANALYSIS: Model estimated RMSE by condition, compared to bootstrapped prior ----

# Get condition RMSE
params_condition = data.frame(
  condition = sort(unique(as.character(self_partner_data$condition)))
)
# Model-estimated RMSE from posterior samples
set.seed(123)
mu_draws_condition = posterior_epred(model_condition, newdata = params_condition, dpar = 'mu', re_formula = NA)
set.seed(123)
sigma_draws_condition = posterior_linpred(model_condition, newdata = params_condition, dpar = 'sigma', re_formula = NA, transform = TRUE)
rmse_draws_condition = sqrt(mu_draws_condition^2 + sigma_draws_condition^2)

# Label conditions
colnames(mu_draws_condition) = params_condition$condition
colnames(sigma_draws_condition) = params_condition$condition
colnames(rmse_draws_condition) = params_condition$condition

# Summarize RMSE estimates by condition
# Naming convention set to match empirical RMSE below
rmse_ci_condition = apply(rmse_draws_condition, 2, function(x) quantile(x, probs = c(0.025, 0.975)))
rmse_means_condition = apply(rmse_draws_condition, 2, function(x) mean(x, na.rm = T))
rmse_df_condition = params_condition |>
  mutate(
    rmse_Mean = rmse_means_condition,
    rmse_Lower = rmse_ci_condition['2.5%',],
    rmse_Upper = rmse_ci_condition['97.5%',]
  )
rmse_df_condition

# Sample errors from the bootstrapped prior to match posterior samples above
set.seed(123)
sampled_prior_errors = sample(
  sampled_error_overall |> pull(mean_rmse),
  size = nrow(rmse_draws_condition),
  replace = TRUE
)

# Compare RMSE distributions
# Values > 0 mean the model improved over the prior
free_chat_improvement = sampled_prior_errors - rmse_draws_condition[,'free chat']
small_talk_improvement = sampled_prior_errors - rmse_draws_condition[,'small talk']
deep_questions_improvement = sampled_prior_errors - rmse_draws_condition[,'deep questions']

Hmisc::smean.cl.boot(free_chat_improvement > 0)
Hmisc::smean.cl.boot(small_talk_improvement > 0)
Hmisc::smean.cl.boot(deep_questions_improvement > 0)



# FIGURE: Empirical and model-based RMSE by condition ----

rmse_summary_fig = error_by_condition |>
  ggplot(
    aes(
      x = condition,
      y = rmse_Mean,
      color = condition
    )
  ) +
  # Empirical RMSE
  geom_point(
    size = 10
  ) +
  geom_errorbar(
    aes(ymin = rmse_Lower, ymax = rmse_Upper),
    width = 0,
    linewidth = 2,
  ) +
  # Add model-estimated RMSE values
  geom_point(
    data = rmse_df_condition,
    aes(x = condition, y = rmse_Mean),
    color = 'black',
    size = 6,
    stroke = 2,
    shape = 25,
    position = position_nudge(x = 0.2)
  ) +
  geom_errorbar(
    data = rmse_df_condition,
    aes(ymin = rmse_Lower, ymax = rmse_Upper),
    color = 'black',
    width = 0,
    linewidth = 1.5,
    position = position_nudge(x = 0.2)
  ) +
  # Priors
  geom_hline(
    data = sampled_error_summary,
    aes(yintercept = mean_rmse_Mean),
    color = 'gray50',
    linetype = 'dashed',
    linewidth = 2,
    alpha = 0.75,
  ) +
  scale_x_discrete(
    name = element_blank(),
    labels = element_blank()
  ) +
  scale_y_continuous(
    name = 'prediction error (rmse)',
    breaks = seq(30, 36, by = 2),
    labels = seq(30, 36, by = 2),
    limits = c(30, 36)
  ) +
  scale_color_manual(
    name = element_blank(),
    values = QUESTION_CATEGORY_COLORS
  ) +
  DEFAULT_THEME +
  theme(
    axis.ticks.x = element_blank(),
    legend.text = element_text(size = 28),
    axis.text.y = element_text(size = 28),
    axis.title.y = element_text(size = 36),
    legend.position = 'none',
  )
rmse_summary_fig

# Save figure
ggsave(
  rmse_summary_fig,
  filename = 'results/figure3_raw.pdf',
  device = cairo_pdf,
  width = 9,
  height = 9,
)



# ANALYSIS: Model-estimated RMSE, bias, variance by trait, compared across traits ----

params_trait = data.frame(
  Big5_Dimension = sort(unique(as.character(self_partner_data$Big5_Dimension)))
)

# Model-estimated RMSE from posterior samples
set.seed(123)
mu_draws_trait = posterior_epred(model_trait, newdata = params_trait, dpar = 'mu', re_formula = NA)
set.seed(123)
sigma_draws_trait = posterior_linpred(model_trait, newdata = params_trait, dpar = 'sigma', re_formula = NA, transform = TRUE)
rmse_draws_trait = sqrt(mu_draws_trait^2 + sigma_draws_trait^2)

# Reformat
colnames(mu_draws_trait) = params_trait$Big5_Dimension
colnames(sigma_draws_trait) = params_trait$Big5_Dimension
colnames(rmse_draws_trait) = params_trait$Big5_Dimension
# Get trait order that matches plot (based on mean RMSE)
sorted_traits = params_trait$Big5_Dimension[rev(order(apply(rmse_draws_trait, 2, mean)))]

# RMSE means + 95% CI
for (i in 1:(length(sorted_traits))) {
  trait = sorted_traits[i]
  means = mean(rmse_draws_trait[, trait], na.rm = T)
  CIs = quantile(rmse_draws_trait[, trait], probs = c(0.025, 0.975))
  cat(sprintf('Trait: %s, RMSE Mean=%.3f, 95%% CrI=[%.3f, %.3f]\n',
              trait, means[1], CIs[1], CIs[2]))
}

# Pairwise RMSE comparisons
for (i in 1:(length(sorted_traits) - 1)) {
  for (j in (i + 1):length(sorted_traits)) {
    trait1 = sorted_traits[i]
    trait2 = sorted_traits[j]
    diff = rmse_draws_trait[, trait1] - rmse_draws_trait[, trait2]
    prob = mean(diff > 0)
    mean_diff = mean(diff, na.rm = T)
    ci_diff = quantile(diff, probs = c(0.025, 0.975))
    cat(sprintf('RMSE difference between %s and %s: Mean=%.3f, 95%% CrI=[%.3f, %.3f], P(>%s)=%.3f\n',
                trait1, trait2, mean_diff[1], ci_diff[1], ci_diff[2], '0', prob))
  }
}

# RMSE compared to priors
# Sample errors from the bootstrapped prior to match posterior samples above
set.seed(123)
sampled_prior_errors_O = sample(
  sampled_error_trait |> filter(Big5_Dimension == 'Openness') |>  pull(mean_rmse),
  size = nrow(rmse_draws_trait),
  replace = TRUE
)
set.seed(123)
sampled_prior_errors_C = sample(
  sampled_error_trait |> filter(Big5_Dimension == 'Conscientiousness') |>  pull(mean_rmse),
  size = nrow(rmse_draws_trait),
  replace = TRUE
)
set.seed(123)
sampled_prior_errors_E = sample(
  sampled_error_trait |> filter(Big5_Dimension == 'Extraversion') |>  pull(mean_rmse),
  size = nrow(rmse_draws_trait),
  replace = TRUE
)
set.seed(123)
sampled_prior_errors_A = sample(
  sampled_error_trait |> filter(Big5_Dimension == 'Agreeableness') |>  pull(mean_rmse),
  size = nrow(rmse_draws_trait),
  replace = TRUE
)
set.seed(123)
sampled_prior_errors_N = sample(
  sampled_error_trait |> filter(Big5_Dimension == 'Neuroticism') |>  pull(mean_rmse),
  size = nrow(rmse_draws_trait),
  replace = TRUE
)

# Compare RMSE distributions
# Values > 0 mean the model improved over the prior
improvement_O = sampled_prior_errors_O - rmse_draws_trait[,'Openness']
improvement_C = sampled_prior_errors_C - rmse_draws_trait[,'Conscientiousness']
improvement_E = sampled_prior_errors_E - rmse_draws_trait[,'Extraversion']
improvement_A = sampled_prior_errors_E - rmse_draws_trait[,'Agreeableness']
improvement_N = sampled_prior_errors_E - rmse_draws_trait[,'Neuroticism']

Hmisc::smean.cl.boot(improvement_O)
Hmisc::smean.cl.boot(improvement_C)
Hmisc::smean.cl.boot(improvement_E)
Hmisc::smean.cl.boot(improvement_A)
Hmisc::smean.cl.boot(improvement_N)


# Bias comparisons: which ones are different from 0?
for (i in 1:(length(sorted_traits))) {
  trait = sorted_traits[i]
  trait_mean = mean(mu_draws_trait[, trait], na.rm = T)
  CIs = quantile(mu_draws_trait[, trait], probs = c(0.025, 0.975))
  prob_greater = mean(mu_draws_trait[, trait] > 0, na.rm = T)
  prob_less = mean(mu_draws_trait[, trait] < 0, na.rm = T)
  cat(sprintf('Trait: %s, Bias Mean=%.3f, 95%% CrI=[%.3f, %.3f], P(>%s)=%.3f, P(<%s)=%.3f \n',
              trait, trait_mean[1], CIs[1], CIs[2], '0', prob_greater, '0', prob_less))
}

# SD means + 95% CI
for (i in 1:(length(sorted_traits))) {
  trait = sorted_traits[i]
  trait_mean = mean(sigma_draws_trait[, trait], na.rm = T)
  CIs = quantile(sigma_draws_trait[, trait], probs = c(0.025, 0.975))
  cat(sprintf('Trait: %s, SD Mean=%.3f, 95%% CrI=[%.3f, %.3f]\n',
              trait, trait_mean[1], CIs[1], CIs[2]))
}

# Follow-up with SD comparisons: which ones are different from each other?
for (i in 1:(length(sorted_traits) - 1)) {
  for (j in (i + 1):length(sorted_traits)) {
    trait1 = sorted_traits[i]
    trait2 = sorted_traits[j]
    diff = sigma_draws_trait[, trait1] - sigma_draws_trait[, trait2]
    prob = mean(diff > 0)
    mean_diff = mean(diff, na.rm = T)
    ci_diff = quantile(diff, probs = c(0.025, 0.975))
    cat(sprintf('SD difference between %s and %s: Mean=%.3f, 95%% CrI=[%.3f, %.3f], P(>%s)=%.3f\n',
                trait1, trait2, mean_diff[1], ci_diff[1], ci_diff[2], '0', prob))
  }
}



# FIGURE: Model-estimated RMSE, bias, variance by condition * trait ----

params_condition_trait = expand.grid(
  Big5_Dimension = sort(unique(as.character(self_partner_data$Big5_Dimension))),
  condition = sort(unique(as.character(self_partner_data$condition)))
)

# Model-estimated RMSE from posterior samples
set.seed(123)
mu_draws_condition_trait = posterior_epred(model_condition_trait_int, newdata = params_condition_trait, dpar = 'mu', re_formula = NA)
set.seed(123)
sigma_draws_condition_trait = posterior_linpred(model_condition_trait_int, newdata = params_condition_trait, dpar = 'sigma', re_formula = NA, transform = TRUE)
rmse_draws_condition_trait = sqrt(mu_draws_condition_trait^2 + sigma_draws_condition_trait^2)

# Summarize RMSE estimates by condition * trait
rmse_ci_condition_trait = apply(rmse_draws_condition_trait, 2, function(x) quantile(x, probs = c(0.025, 0.975)))
rmse_means_condition_trait = apply(rmse_draws_condition_trait, 2, function(x) mean(x, na.rm = T))
rmse_df_condition_trait = params_condition_trait |>
  mutate(
    rmse = rmse_means_condition_trait,
    rmse_low = rmse_ci_condition_trait['2.5%',],
    rmse_high = rmse_ci_condition_trait['97.5%',]
  )
rmse_df_condition_trait

# Model-estimated bias and variance by condition * trait
bias_condition_trait = fitted(
  model_condition_trait_int,
  newdata = params_condition_trait,
  dpar = 'mu',
  re_formula = NA,
  summary = TRUE
)
sigmas_condition_trait = fitted(
  model_condition_trait_int,
  newdata = params_condition_trait,
  dpar = 'sigma',
  re_formula = NA,
  summary = TRUE
)

# Combine RMSE, bias, and variance estimates, convert to long
condition_trait_effects = cbind(
  params_condition_trait,
  bias = bias_condition_trait[, "Estimate"],
  bias_low = bias_condition_trait[, "Q2.5"],
  bias_high = bias_condition_trait[, "Q97.5"],
  sd = sigmas_condition_trait[, "Estimate"],
  sd_low = sigmas_condition_trait[, "Q2.5"],
  sd_high = sigmas_condition_trait[, "Q97.5"]
) |>
  left_join(
    rmse_df_condition_trait |> select(Big5_Dimension, condition, rmse, rmse_low, rmse_high),
    by = c('Big5_Dimension', 'condition')
  ) |>
  pivot_longer(
    cols = c('bias', 'sd', 'rmse'),
    names_to = 'parameter',
    values_to = 'estimate'
  ) |>
  mutate(
    low = case_when(
      parameter == 'bias' ~ bias_low,
      parameter == 'sd' ~ sd_low,
      parameter == 'rmse' ~ rmse_low
    ),
    high = case_when(
      parameter == 'bias' ~ bias_high,
      parameter == 'sd' ~ sd_high,
      parameter == 'rmse' ~ rmse_high
    ),
  ) |>
  select(Big5_Dimension, condition, parameter, estimate, low, high)

trait_order = condition_trait_effects |>
  filter(parameter == 'rmse') |>
  group_by(Big5_Dimension) |>
  summarize(
    mean_rmse = mean(estimate),
    .groups = 'drop'
  ) |>
  arrange(
    desc(mean_rmse)
  ) |>
  mutate(
    Big5_Dimension = as.character(Big5_Dimension)
  ) |>
  select(Big5_Dimension)


# RMSE fig
rmse_fig = condition_trait_effects |>
  filter(parameter == 'rmse') |>
  mutate(Big5_Dimension_ord = factor(as.character(Big5_Dimension), levels = trait_order$Big5_Dimension)) |>
  # poster figure: re-order conditions to match main effect figure
  mutate(condition = factor(condition, levels = QUESTION_CATEGORY_LEVELS)) |>
  ggplot(aes(x = condition, y = estimate, color = condition)) +
  geom_point(size = 6) +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    width = 0.25,
    linewidth = 1
  ) +
  # Priors
  geom_hline(
    data = sampled_error_trait_summary |>
      mutate(Big5_Dimension_ord = factor(as.character(Big5_Dimension), levels = trait_order$Big5_Dimension)),
    aes(yintercept = mean_rmse_Mean),
    color = 'gray50',
    linetype = 'dashed',
    linewidth = 1,
    alpha = 0.75
  ) +
  facet_wrap(~ Big5_Dimension_ord, nrow = 1) +
  scale_x_discrete(
    name = element_blank(),
    labels = element_blank()
  ) +
  scale_y_continuous(
    name = 'rmse',
    breaks = seq(25, 50, by = 5),
    labels = seq(25, 50, by = 5),
  ) +
  scale_color_manual(
    name = element_blank(),
    values = QUESTION_CATEGORY_COLORS
  ) +
  DEFAULT_THEME +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 24),
    legend.text = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.line.y = element_blank(),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5),
    legend.position = 'none'
  )
rmse_fig

# Bias fig
bias_fig = condition_trait_effects |>
  filter(parameter == 'bias') |>
  mutate(Big5_Dimension_ord = factor(as.character(Big5_Dimension), levels = trait_order$Big5_Dimension)) |>
  # poster figure: re-order conditions to match main effect figure
  mutate(condition = factor(condition, levels = QUESTION_CATEGORY_LEVELS)) |>
  ggplot(aes(x = condition, y = estimate, color = condition)) +
  # poster figure: replace dashed line with narrow dotted line to visually distinguish from "prior" line in RMSE fig
  geom_hline(
    yintercept = 0,
    # linetype = 'dashed',
    linetype = 'dotted',
    color = 'black',
    linewidth = 0.5
  ) +
  geom_point(size = 6) +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    width = 0.25,
    linewidth = 1
  ) +
  facet_wrap(~ Big5_Dimension_ord, nrow = 1) +
  scale_x_discrete(
    name = element_blank(),
    labels = element_blank()
  ) +
  scale_y_continuous(
    name = 'bias',
    breaks = seq(-15, 15, by = 5),
    labels = seq(-15, 15, by = 5),
    limits = c(-15, 17)
  ) +
  scale_color_manual(
    name = element_blank(),
    values = QUESTION_CATEGORY_COLORS
  ) +
  DEFAULT_THEME +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.line.y = element_blank(),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5),
    legend.position = 'none'
  )
bias_fig

# SD fig
sd_fig = condition_trait_effects |>
  filter(parameter == 'sd') |>
  mutate(Big5_Dimension_ord = factor(as.character(Big5_Dimension), levels = trait_order$Big5_Dimension)) |>
  # poster figure: re-order conditions to match main effect figure
  mutate(condition = factor(condition, levels = QUESTION_CATEGORY_LEVELS)) |>
  ggplot(aes(x = condition, y = estimate, color = condition)) +
  geom_point(size = 6) +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    width = 0.25,
    linewidth = 1
  ) +
  facet_wrap(~ Big5_Dimension_ord, nrow = 1) +
  scale_x_discrete(
    name = element_blank(),
    labels = element_blank()
  ) +
  scale_y_continuous(
    name = 'st. dev.',
    breaks = seq(25, 50, by = 5),
    labels = seq(25, 50, by = 5),
    limits = c(23, 50)
  ) +
  scale_color_manual(
    name = element_blank(),
    values = QUESTION_CATEGORY_COLORS
  ) +
  DEFAULT_THEME +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.line.y = element_blank(),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5),
    legend.position = 'none'
  )
sd_fig

# Combine figures
combined_fig = rmse_fig / bias_fig / sd_fig +
  plot_layout(guides = 'collect')
combined_fig

# Save figure
ggsave(
  combined_fig,
  filename = 'results/figure4_raw.pdf',
  device = cairo_pdf,
  width = 16,
  height = 9,
)


# Cog sci poster figure
combined_fig_poster_fig = rmse_fig / bias_fig / sd_fig +
  plot_layout(guides = 'collect')
combined_fig_poster_fig
# Save figure
# NB: copying this over to base repo for further editing
ggsave(
  combined_fig_poster_fig,
  filename = 'results/cogsci_poster_condition_by_trait_effects_raw.pdf',
  device = cairo_pdf,
  width = 16,
  height = 9,
)


# ANALYSIS: Relationship between individual questions discussed and prediction accuracy ----

# Get individual questions selected by each participant
participant_question_summary = combined_messages |>
  filter(isQuestionPrompt == TRUE) |>
  distinct(
    condition,
    group_id,
    author,
    message_string
  )

# Calculate participants' empirical trait-level RMSE
# NB: this includes two participants who do not show up in the question summary above
# because these participants typed questions in manually rather than selecting
# (so they never have an isQuestionPrompt row in the message data)
participant_trait_rmse = self_partner_data |>
  filter(condition != 'free chat') |>
  group_by(
    condition,
    groupId,
    prolificId,
    Big5_Dimension
  ) |>
  summarize(
    trait_rmse = sqrt(mean(sq_error, na.rm = TRUE)),
    .groups = 'drop'
  )

# Join questions discussed and trait-level RMSE
participant_question_trait_rmse = participant_question_summary |>
  left_join(
    participant_trait_rmse,
    by = join_by(
      condition,
      group_id == groupId,
      author == prolificId
    ),
    relationship = 'many-to-many'
  ) |>
  rename(
    prolificId = author,
    groupId = group_id,
    question = message_string
  )

# Fit brms model predicting trait-level RMSE from question
# Z-score RMSE for modeling
#' What global z-scoring does
#' Preserves:
#' * Trait differences in baseline difficulty
#' * Participant differences in overall accuracy
#' * Question-level variation
#' * Puts everything on a unitless, interpretable scale
#' * Makes priors easy and non-controversial
#' After global z-scoring:
#' * Intercept ≈ 0
#' * Fixed trait effects = mean differences in RMSE (in SD units)
#' * Random-effect SDs = proportion of total RMSE variance explained
participant_question_trait_rmse = participant_question_trait_rmse |>
  mutate(
    rmse_z = as.numeric(scale(trait_rmse))
  )

# Fit BRMS
question_trait_effect = brm(
  rmse_z ~ 1 + Big5_Dimension +
    (1 | prolificId) +
    (1 | groupId) +
    (1 | question) +
    (1 | question:Big5_Dimension),
  prior = c(
    # Question-level SDs larger than ~1 SD are possible but disfavored
    prior(normal(0, 1), class = 'Intercept'), # mean RMSE starts <= 1 SD from global mean
    prior(normal(0, 0.5), class = 'sd'),   # all random effect SDs: typical variation ~0.5 SD
    prior(student_t(3, 0, 1), class = 'sigma') # residual noise is probably within about 1 SD of the outcome scale
  ),
  data = participant_question_trait_rmse,
  family = gaussian(),
  file = 'analysis/brms_fits/question_trait_effect',
  seed = 1
)

# Model interpretation

# Overall effect of questions, trait-specific effects
# NB: interpret the magnitude of these effects in SD units
vc = VarCorr(question_trait_effect)
vc
# $question$sd = 0.01 -> "essentially no evidence that some questions systematically reduce or increase RMSE overall."
# $`question:Big5_Dimension`$sd = 0.03 -> "Even trait-specific effects are very weak; no question seems to have a meaningful impact on RMSE for any trait."

# Most of the variance in RMSE is explained by:
# Dyad-level differences: $groupId$sd
# Participant-level differences in inference ability: $prolificId$sd

# Identify most impactful individual questions, overall and by trait
df_question_RE = as.data.frame(ranef(question_trait_effect)$question) |>
  rownames_to_column('question') |>
  select(question, Estimate.Intercept, Est.Error.Intercept, Q2.5.Intercept, Q97.5.Intercept) |>
  rename(
    overall_question_effect = Estimate.Intercept,
    overall_question_se = Est.Error.Intercept,
    overall_question_ci_lower = Q2.5.Intercept,
    overall_question_ci_upper = Q97.5.Intercept
  )

df_question_trait_RE = as.data.frame(ranef(question_trait_effect)$`question:Big5_Dimension`) |>
  rownames_to_column('question_trait') |>
  separate(question_trait, into = c('question', 'trait'), sep = '_') |>
  select(question, trait, Estimate.Intercept, Est.Error.Intercept, Q2.5.Intercept, Q97.5.Intercept) |>
  rename(
    trait_effect = Estimate.Intercept,
    trait_se = Est.Error.Intercept,
    trait_ci_lower = Q2.5.Intercept,
    trait_ci_upper = Q97.5.Intercept
  )

# Join and compute total effect
df_question_trait_combined_RE = df_question_trait_RE |>
  left_join(df_question_RE, by = 'question') |>
  mutate(
    total_effect = overall_question_effect + trait_effect
  ) |>
  # Add condition info for each question
  left_join(
    participant_question_trait_rmse |> distinct(question, condition),
    by = 'question'
  )

# Top question for each trait
df_question_trait_combined_RE |>
  group_by(trait) |>
  slice_min(order_by = total_effect, n = 1) |>
  ungroup() |>
  print(Inf)

# Questions with largest (negative) effects on RMSE overall (i.e., best)
# Which traits were most representative among diagnostic questions?
# Which question-trait combinations were most diagnostic?
df_question_trait_combined_RE |>
  arrange(total_effect) |>
  head(10)

# Questions with largest (positive) effects on RMSE overall (i.e., worst)
# Which traits were most representative among misleading questions?
# Which question-trait combinations were most misleading?
df_question_trait_combined_RE |>
  arrange(desc(total_effect)) |>
  head(10)



# FIGURE: Top individual questions for individual trait inferences ----

# Posterior samples from model above
posterior_samples_question_trait = as_draws_df(question_trait_effect, regex = T, variable = 'r_')

# Function for extracting 95% CrI for top questions for each trait from posterior samples
get_total_effect_ci = function(question, trait, condition, post) {
  # Identify columns
  # NB: str_replace_all adds "." between each word of question
  overall_col = paste0('r_question[', str_replace_all(question, '\\s+', '.'), ',Intercept]')
  trait_col = paste0('r_question:Big5_Dimension[', str_replace_all(question, '\\s+', '.'), '_', trait, ',Intercept]')
  # Sum posterior draws
  mean_total = mean(post[[overall_col]] + post[[trait_col]])
  # 95% CrI
  ci_95 = quantile(post[[overall_col]] + post[[trait_col]], probs = c(0.025, 0.975))
  # 50% CrI
  ci_50 = quantile(post[[overall_col]] + post[[trait_col]], probs = c(0.25, 0.75))
  data.frame(
    question = question,
    trait = trait,
    condition = condition,
    mean = mean_total[1],
    ci_l50 = ci_50[1],
    ci_u50 = ci_50[2],
    ci_l95 = ci_95[1],
    ci_u95 = ci_95[2]
  )
}

# Top question for each trait
top_question_summary = df_question_trait_combined_RE |>
  group_by(trait) |>
  slice_min(order_by = total_effect, n = 1) |>
  ungroup() |>
  rowwise() |>
  do(
    get_total_effect_ci(.$question, .$trait, .$condition, posterior_samples_question_trait)
  ) |>
  arrange(mean)
top_question_summary

# Bottom question for each trait
bottom_question_summary = df_question_trait_combined_RE |>
  group_by(trait) |>
  slice_max(order_by = total_effect, n = 1) |>
  ungroup() |>
  rowwise() |>
  do(
    get_total_effect_ci(.$question, .$trait, .$condition, posterior_samples_question_trait)
  ) |>
  arrange(mean)
bottom_question_summary

joint_question_summary = rbind(
  top_question_summary |> mutate(question_type = 'top'),
  bottom_question_summary |> mutate(question_type = 'bottom')
)

# Figure
joint_question_fig = joint_question_summary |>
  rowwise() |>
  mutate(
    mean_inv = -1 * mean,
    ci_l50_inv = -1 * ci_l50,
    ci_u50_inv = -1 * ci_u50,
    ci_l95_inv = -1 * ci_l95,
    ci_u95_inv = -1 * ci_u95
  ) |>
  ggplot(
    aes(
      x = reorder(trait, mean),
      y = mean_inv, # NB: inverting value for interpretability
      color = condition,
      label = str_wrap(question, width = 60),
      group = factor(question_type),
      shape = factor(question_type)
    )
  ) +
  geom_point(
    size = 8,
    position = position_dodge(width = 1.5)
  ) +
  geom_errorbar(
    aes(
      ymin = ci_l50_inv,
      ymax = ci_u50_inv
    ),
    position = position_dodge(width = 1.5),
    width = 0,
    linewidth = 2.5,
  ) +
  geom_errorbar(
    aes(
      ymin = ci_l95_inv,
      ymax = ci_u95_inv
    ),
    position = position_dodge(width = 1.5),
    width = 0,
    linewidth = 0.5,
  ) +
  # Add text of question
  geom_text(
    size = 4,
    vjust = -.75,   # Adjust vertical justification to position text above the point
    position = position_dodge(width = 1)
  ) +
  scale_x_discrete(
    name = element_blank(),
  ) +
  scale_y_continuous(
    name = 'est. decrease in RMSE (z-scored)',
  ) +
  scale_color_manual(
    name = element_blank(),
    values = QUESTION_CATEGORY_COLORS
  ) +
  coord_flip() +
  DEFAULT_THEME +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.position = 'none'
  )
joint_question_fig

# Save figure
ggsave(
  joint_question_fig,
  filename = 'results/figure5_raw.pdf',
  device = cairo_pdf,
  width = 11,
  height = 11,
)



# ANALYSIS: Relationship between dyad similarity and prediction accuracy ----

# > 1. Calculate distances in trait space between self and predictions ----

# Format prediction data to align with self responses
# Reverse coded items
partner_preds = partner_preds |>
  mutate(
    reverse_item = ifelse(scale_text %in% reverse_coded_scales, TRUE, FALSE),
    prediction_coded = ifelse(scale_text %in% reverse_coded_scales, 100 - response, response)
  )

# Add predictions to self-partner data
self_partner_predictions = self_partner_data |>
  left_join(
    partner_preds |> select(groupId, prolificId, scale_id, scale_text, prediction_coded),
    by = join_by(groupId, prolificId, scale_id, scale_text)
  )

# Summarize self responses in trait space
self_trait_summary = self_partner_data |>
  group_by(condition, groupId, prolificId, target, Big5_Dimension) |>
  summarize(
    # Mean value used for all analyses below
    mean_trait_response = mean(self_response_coded, na.rm = TRUE),
    # SD not used in analyses below but included here in case it's useful
    sd_trait_response = sd(self_response_coded, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  pivot_wider(
    names_from = Big5_Dimension,
    values_from = c(mean_trait_response, sd_trait_response)
  )

# Summarize predictions in trait space
prediction_trait_summary = self_partner_predictions |>
  group_by(condition, groupId, prolificId, Big5_Dimension) |>
  summarize(
    # Mean value used for all analyses below
    mean_trait_prediction = mean(prediction_coded, na.rm = TRUE),
    # SD not used in analyses below but included here in case it's useful
    sd_trait_prediction = sd(prediction_coded, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  pivot_wider(
    names_from = Big5_Dimension,
    values_from = c(mean_trait_prediction, sd_trait_prediction)
  )

# Join self-response trait summary and prediction trait summary
self_resp_prediction_trait_summary = self_trait_summary |>
  left_join(
    prediction_trait_summary,
    by = join_by(condition, groupId, prolificId),
  )

# Calculate distances in trait space between self responses and predictions
self_resp_prediction_trait_summary = self_resp_prediction_trait_summary |>
  rowwise() |>
  mutate(
    # Euclidean distance
    big5_euclidean_distance_resp_pred = sqrt(
      (mean_trait_response_Extraversion - mean_trait_prediction_Extraversion)^2 +
      (mean_trait_response_Agreeableness - mean_trait_prediction_Agreeableness)^2 +
      (mean_trait_response_Conscientiousness - mean_trait_prediction_Conscientiousness)^2 +
      (mean_trait_response_Neuroticism - mean_trait_prediction_Neuroticism)^2 +
      (mean_trait_response_Openness - mean_trait_prediction_Openness)^2
    ),
    # Correlation distance
    correlation_distance_resp_pred = 1 - cor(
      c(
        mean_trait_response_Extraversion,
        mean_trait_response_Agreeableness,
        mean_trait_response_Conscientiousness,
        mean_trait_response_Neuroticism,
        mean_trait_response_Openness
      ),
      c(
        mean_trait_prediction_Extraversion,
        mean_trait_prediction_Agreeableness,
        mean_trait_prediction_Conscientiousness,
        mean_trait_prediction_Neuroticism,
        mean_trait_prediction_Openness
      ),
    ),
    # Cosine distance
    cosine_distance_resp_pred = 1 - (
      sum(
        c(
          mean_trait_response_Extraversion,
          mean_trait_response_Agreeableness,
          mean_trait_response_Conscientiousness,
          mean_trait_response_Neuroticism,
          mean_trait_response_Openness
        ) * c(
          mean_trait_prediction_Extraversion,
          mean_trait_prediction_Agreeableness,
          mean_trait_prediction_Conscientiousness,
          mean_trait_prediction_Neuroticism,
          mean_trait_prediction_Openness
        )
      ) / (
        sqrt(
          sum(
            c(
              mean_trait_response_Extraversion,
              mean_trait_response_Agreeableness,
              mean_trait_response_Conscientiousness,
              mean_trait_response_Neuroticism,
              mean_trait_response_Openness
            )^2
          )
        ) * sqrt(
          sum(
            c(
              mean_trait_prediction_Extraversion,
              mean_trait_prediction_Agreeableness,
              mean_trait_prediction_Conscientiousness,
              mean_trait_prediction_Neuroticism,
              mean_trait_prediction_Openness
            )^2
          )
        )
      )
    )
  )


# > 2. Calculate distances in trait space between self and partner ----

# Add partner responses by joining trait response summary dataframe with itself on group ID,
# then keeping only rows where the prolific IDs don't match
self_partner_trait_summary = self_trait_summary |>
  left_join(
    self_trait_summary |>
      select(groupId, prolificId, starts_with('mean_trait_response_'), starts_with('sd_trait_response_')) |>
      rename_with(
        ~ paste0('partner_', .),
        starts_with('mean_trait_response_')
      ) |>
      rename_with(
        ~ paste0('partner_', .),
        starts_with('sd_trait_response_')
      ) |>
      rename(partner_prolificId = prolificId),
    by = join_by(groupId),
    relationship = 'many-to-many'
  ) |>
  filter(prolificId != partner_prolificId)

# Calculate distances in trait space between self responses and partner responses
# NB: same function as above
self_partner_trait_summary = self_partner_trait_summary |>
  rowwise() |>
  mutate(
    # Euclidean distance
    big5_euclidean_distance = sqrt(
      (mean_trait_response_Extraversion - partner_mean_trait_response_Extraversion)^2 +
      (mean_trait_response_Agreeableness - partner_mean_trait_response_Agreeableness)^2 +
      (mean_trait_response_Conscientiousness - partner_mean_trait_response_Conscientiousness)^2 +
      (mean_trait_response_Neuroticism - partner_mean_trait_response_Neuroticism)^2 +
      (mean_trait_response_Openness - partner_mean_trait_response_Openness)^2
    ),
    # Correlation distance
    correlation_distance = 1 - cor(
      c(
        mean_trait_response_Extraversion,
        mean_trait_response_Agreeableness,
        mean_trait_response_Conscientiousness,
        mean_trait_response_Neuroticism,
        mean_trait_response_Openness
      ),
      c(
        partner_mean_trait_response_Extraversion,
        partner_mean_trait_response_Agreeableness,
        partner_mean_trait_response_Conscientiousness,
        partner_mean_trait_response_Neuroticism,
        partner_mean_trait_response_Openness
      ),
    ),
    # Cosine distance
    cosine_distance = 1 - (
      sum(
        c(
          mean_trait_response_Extraversion,
          mean_trait_response_Agreeableness,
          mean_trait_response_Conscientiousness,
          mean_trait_response_Neuroticism,
          mean_trait_response_Openness
        ) * c(
          partner_mean_trait_response_Extraversion,
          partner_mean_trait_response_Agreeableness,
          partner_mean_trait_response_Conscientiousness,
          partner_mean_trait_response_Neuroticism,
          partner_mean_trait_response_Openness
        )
      ) / (
        sqrt(
          sum(
            c(
              mean_trait_response_Extraversion,
              mean_trait_response_Agreeableness,
              mean_trait_response_Conscientiousness,
              mean_trait_response_Neuroticism,
              mean_trait_response_Openness
            )^2
          )
        ) * sqrt(
          sum(
            c(
              partner_mean_trait_response_Extraversion,
              partner_mean_trait_response_Agreeableness,
              partner_mean_trait_response_Conscientiousness,
              partner_mean_trait_response_Neuroticism,
              partner_mean_trait_response_Openness
            )^2
          )
        )
      )
    )
  )


# > 3. Relate distances in trait space to dyad RMSE ----

# Calculate dyad RMSE summaries
self_partner_rmse_summary = self_partner_data |>
  group_by(condition, groupId, prolificId) |>
  summarize(
    rmse = sqrt(mean(sq_error, na.rm = TRUE)),
    .groups = 'drop'
  )

# Join RMSE summary with distances calculated above
self_partner_rmse_distance = self_partner_rmse_summary |>
  left_join(
    self_partner_trait_summary |>
      select(
        condition,
        groupId,
        prolificId,
        big5_euclidean_distance,
        correlation_distance,
        cosine_distance
      ),
    by = join_by(condition, groupId, prolificId)
  ) |>
  left_join(
    self_resp_prediction_trait_summary |>
      select(
        condition,
        groupId,
        prolificId,
        big5_euclidean_distance_resp_pred,
        correlation_distance_resp_pred,
        cosine_distance_resp_pred
      ),
    by = join_by(condition, groupId, prolificId)
  )


# > 4. Fit models ----

dyad_rmse_distance_summary = self_partner_rmse_distance |>
  group_by(condition, groupId) |>
  summarize(
    dyad_mean_rmse = mean(rmse, na.rm = TRUE),
    big5_euclidean_distance_resp_pred = mean(big5_euclidean_distance_resp_pred, na.rm = TRUE),
    big5_euclidean_distance = mean(big5_euclidean_distance, na.rm = TRUE),
    correlation_distance_resp_pred = mean(correlation_distance_resp_pred),
    correlation_distance = mean(correlation_distance),
    cosine_distance_resp_pred = mean(cosine_distance_resp_pred),
    cosine_distance = mean(cosine_distance),
    .groups = 'drop'
  )


# Euclidean distance (individuals)
euc_model_individ = lm(
  rmse ~ big5_euclidean_distance_resp_pred + big5_euclidean_distance,
  data = self_partner_rmse_distance
)
summary(euc_model_individ)
confint(euc_model_individ)

# Euclidean distance (dyads)
euc_model_dyad = lm(
  dyad_mean_rmse ~ big5_euclidean_distance_resp_pred + big5_euclidean_distance,
  data = dyad_rmse_distance_summary
)
summary(euc_model_dyad)
confint(euc_model_dyad)

# Corr distance (individuals)
corr_model_individ = lm(
  rmse ~ correlation_distance_resp_pred + correlation_distance,
  data = self_partner_rmse_distance
)
summary(corr_model_individ)
confint(corr_model_individ)

# Corr distance (dyads)
corr_model_dyad = lm(
  dyad_mean_rmse ~ correlation_distance_resp_pred + correlation_distance,
  data = dyad_rmse_distance_summary
)
summary(corr_model_dyad)
confint(corr_model_dyad)

# Cos distance (individuals)
cos_model_individ = lm(
  rmse ~ cosine_distance_resp_pred + cosine_distance,
  data = self_partner_rmse_distance
)
summary(cos_model_individ)
confint(cos_model_individ)

# Cos distance (dyads)
cos_model_dyad = lm(
  dyad_mean_rmse ~ cosine_distance_resp_pred + cosine_distance,
  data = dyad_rmse_distance_summary
)
summary(cos_model_dyad)
confint(cos_model_dyad)



# FIGURE: Relationship between dyad similarity and prediction accuracy ----

euc_dist_fig = dyad_rmse_distance_summary |>
  ggplot(aes(x = big5_euclidean_distance, y = dyad_mean_rmse, color = condition)) +
  geom_point(
    alpha = 0.5,
    size = 3
  ) +
  geom_smooth(
    method = 'lm',
    col = 'black'
  ) +
  scale_x_continuous(
    name = 'dyad personality distance',
    breaks = seq(0, 150, by = 50),
    labels = seq(0, 150, by = 50),
    limits = c(0, 150)
  ) +
  scale_y_continuous(
    name = 'dyad prediction error (rmse)',
    breaks = seq(20, 60, by = 20),
    labels = seq(20, 60, by = 20),
    limits = c(15, 65)
  ) +
  scale_color_manual(
    name = element_blank(),
    values = QUESTION_CATEGORY_COLORS
  ) +
  DEFAULT_THEME +
  theme(
    axis.title.y = element_text(size = 32),
    axis.title.x = element_text(size = 32),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    legend.position = 'none',
  )
euc_dist_fig

ggsave(
  euc_dist_fig,
  filename = 'results/figure6A_raw.pdf',
  device = cairo_pdf,
  width = 6.5,
  height = 7,
)



# ANALYSIS: relationship between metacognition (get to know partner, expected accuracy) and prediction accuracy ----

# Metacognition slider summary
meta_cog_summary = self_partner_data |>
  group_by(condition, groupId, prolificId) |>
  summarize(
    rmse = sqrt(mean(sq_error, na.rm = TRUE)),
    sliderGetToKnowPartnerMean = mean(sliderGetToKnowPartner, na.rm = TRUE),
    sliderPartnerPredictionAccuracyMean = mean(sliderPartnerPredictionAccuracy, na.rm = TRUE),
    .groups = 'drop'
  )

# "Get to know partner" response summary
meta_cog_summary |>
  summarize(
    ci_stats = list(Hmisc::smean.cl.boot(sliderGetToKnowPartnerMean)),
    sd = sd(sliderGetToKnowPartnerMean),
    .groups = 'drop'
  ) |>
  tidyr::unnest_wider(ci_stats)

# Variance in "Get to know partner" responses by condition
summary(aov(
  sliderGetToKnowPartnerMean ~ condition,
  data = meta_cog_summary
))

# Relationship between "Get to know partner" responses and prediction accuracy
model_get_to_know_partner = lm(
  rmse ~ sliderGetToKnowPartnerMean,
  data = meta_cog_summary
)
summary(model_get_to_know_partner)
confint(model_get_to_know_partner)

# Accurate partner prediction response summary
meta_cog_summary |>
  summarize(
    ci_stats = list(Hmisc::smean.cl.boot(sliderPartnerPredictionAccuracyMean)),
    sd = sd(sliderPartnerPredictionAccuracyMean),
    .groups = 'drop'
  ) |>
  tidyr::unnest_wider(ci_stats)

# Variance in accurate partner prediction responses by condition
summary(aov(
  sliderPartnerPredictionAccuracyMean ~ condition,
  data = meta_cog_summary
))

# Relationship between accurate partner prediction responses and prediction accuracy
model_partner_prediction_accuracy = lm(
  rmse ~ sliderPartnerPredictionAccuracyMean,
  data = meta_cog_summary
)
summary(model_partner_prediction_accuracy)
confint(model_partner_prediction_accuracy)



# ANALYSIS: Relationship between message length and prediction accuracy ----

# Summarize total words and total messages for each subject
message_metrics_summary = combined_messages |>
  # T/F in question prompting condition, NA in baseline
  filter(is.na(isQuestionPrompt) | isQuestionPrompt == FALSE) |>
  mutate(word_count = str_count(message_string, '\\S+')) |>
  group_by(condition, group_id, author) |>
  summarize(
    total_messages = n(),
    total_words = sum(word_count, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  rename(
    groupId = group_id,
    prolificId = author
  )

# Join with subject-level RMSE
subject_summary = self_partner_data |>
  group_by(condition, groupId, prolificId) |>
  summarize(
    rmse = sqrt(mean(sq_error, na.rm = TRUE)),
    .groups = 'drop'
  ) |>
  left_join(
    message_metrics_summary,
    by = c('condition', 'groupId', 'prolificId')
  ) |>
  rename(
    'partner_total_messages' = total_messages,
    'partner_total_words' = total_words
  )


# > Total words ----

# Model summary
total_words_model = lm(
  rmse ~ log10(partner_total_words),
  data = subject_summary
)
summary(total_words_model)
confint(total_words_model)

# Effect of condition
total_words_condition = lm(
  rmse ~ condition + log10(partner_total_words),
  data = subject_summary
)
total_words_condition_int = lm(
  rmse ~ condition * log10(partner_total_words),
  data = subject_summary
)
anova(total_words_condition, total_words_condition_int)


# > Total messages ----

# Model summary
total_msgs_model = lm(
  rmse ~ log10(partner_total_messages),
  data = subject_summary
)
summary(total_msgs_model)
confint(total_msgs_model)

# Effect of condition
total_msgs_condition = lm(
  rmse ~ condition + log10(partner_total_messages),
  data = subject_summary
)
total_msgs_condition_int = lm(
  rmse ~ condition * log10(partner_total_messages),
  data = subject_summary
)
anova(total_msgs_condition, total_msgs_condition_int)



# FIGURE: Relationship between message length and prediction accuracy ----

# > Total words ----

total_words_deciles = subject_summary |>
  mutate(
    decile = ntile(partner_total_words, n = 10),
    total_words_eq_bins = cut_interval(partner_total_words, n = 5)
  ) |>
  group_by(decile) |>
  summarize(
    min_total_words = min(partner_total_words),
    max_total_words = max(partner_total_words),
    mean_total_words = mean(partner_total_words),
    median_total_words = median(partner_total_words),
    mean_rmse = Hmisc::smean.cl.boot(rmse)['Mean'],
    rmse_low = Hmisc::smean.cl.boot(rmse)['Lower'],
    rmse_high = Hmisc::smean.cl.boot(rmse)['Upper'],
    n = n(),
    .groups = 'drop'
  )

fig_words = subject_summary |>
  # NB: filtering participants with very few words (<= 15) to improve visualization
  filter(partner_total_words > 15) |>
  ggplot() +
  # Raw data
  geom_point(
    aes(x = partner_total_words, y = rmse),
    alpha = 0.15,
    size = 2,
    color = '#6082b6'
  ) +
  # Decile means
  geom_point(
    data = total_words_deciles,
    aes(x = mean_total_words, y = mean_rmse),
    color = '#6082b6',
    size = 5
  ) +
  geom_errorbar(
    data = total_words_deciles,
    aes(x = mean_total_words, ymin = rmse_low, ymax = rmse_high),
    color = '#6082b6',
    width = 0,
    linewidth = 1
  ) +
  geom_smooth(
    aes(x = partner_total_words, y = rmse),
    method = 'lm',
    col = '#6082b6',
    formula = 'y ~ x',
    alpha = 0.25
  ) +
  scale_x_log10(
    name = 'partner total words',
  ) +
  scale_y_continuous(
    name = 'prediction error (rmse)',
    breaks = seq(0, 80, by = 20),
    labels = seq(0, 80, by = 20),
    limits = c(0, 80)
  ) +
  DEFAULT_THEME +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 24),
  )
fig_words


# > Total messages ----

total_msgs_deciles = subject_summary |>
  mutate(
    total_msgs = partner_total_messages,
    decile = ntile(total_msgs, n = 10),
    total_msgs_eq_bins = cut_interval(total_msgs, n = 10)
  ) |>
  group_by(decile) |>
  summarize(
    min_total_msgs = min(total_msgs),
    max_total_msgs = max(total_msgs),
    mean_total_msgs = mean(total_msgs),
    median_total_msgs = median(total_msgs),
    mean_rmse = Hmisc::smean.cl.boot(rmse)['Mean'],
    rmse_low = Hmisc::smean.cl.boot(rmse)['Lower'],
    rmse_high = Hmisc::smean.cl.boot(rmse)['Upper'],
    n = n(),
    .groups = 'drop'
  )

fig_msgs = subject_summary |>
  ggplot() +
  # Raw data
  geom_point(
    aes(x = partner_total_messages, y = rmse),
    alpha = 0.1,
    size = 2,
    color = '#eb1f48'
  ) +
  # Decile means
  geom_point(
    data = total_msgs_deciles,
    aes(x = mean_total_msgs, y = mean_rmse),
    color = '#eb1f48',
    size = 5
  ) +
  geom_errorbar(
    data = total_msgs_deciles,
    aes(x = mean_total_msgs, ymin = rmse_low, ymax = rmse_high),
    color = '#eb1f48',
    width = 0.0,
    linewidth = 1
  ) +
  geom_smooth(
    aes(x = partner_total_messages, y = rmse),
    method = 'lm',
    formula = y ~ x,
    col = '#eb1f48',
    alpha = 0.25
  ) +
  scale_x_log10(
    name = 'partner total messages',
  ) +
  scale_y_continuous(
    name = 'prediction error (rmse)',
    breaks = seq(0, 80, by = 20),
    labels = seq(0, 80, by = 20),
    limits = c(0, 80)
  ) +
  DEFAULT_THEME +
  theme(
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 24),
  )
fig_msgs


# > Combined ----

fig_preds_combined = fig_words + fig_msgs +
  plot_layout(
    ncol = 1,
    axes = 'collect'
  )
fig_preds_combined
# Save figure
ggsave(
  fig_preds_combined,
  filename = 'results/figure6B_raw.pdf',
  device = cairo_pdf,
  width = 6,
  height = 8,
)





