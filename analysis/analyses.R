

#'
#' Scratch analyses for CogSci 2026 
#' 


# INIT ----
rm(list = ls())
library(brms)
library(emmeans)
library(here)
library(lme4)
library(patchwork)
library(systemfonts)
library(tidyverse)


# system_fonts() |> 
#   dplyr::filter(grepl("Open Sans", family))



# FIGURE GLOBALS ----


QUESTION_CATEGORY_COLORS = c(
        'baseline' = '#e8ad0f',
        'deep' = '#C8A2C8',
        'small talk' = '#8FB0A9'
)

DEFAULT_THEME = theme(
        # titles
        plot.title = element_text(size = 32, family = 'Open Sans', margin = margin(b = 0.5, unit = 'line')),
        axis.title.y = element_text(size = 34, family = 'Open Sans', margin = margin(r = 0.5, unit = 'line')),
        axis.title.x = element_text(size = 30, family = 'Open Sans', margin = margin(t = 0.5, unit = 'line')),
        legend.title = element_text(size = 40, family = 'Open Sans'),
        # axis text
        axis.text.x = element_text(size = 20, angle = 0, vjust = 1, family = 'Open Sans', margin = margin(t = 0.5, unit = 'line'), color = 'black'),
        axis.text.y = element_text(size = 36, family = 'Open Sans', margin = margin(r = 0.5, unit = 'line'), color = 'black'),
        # legend text
        legend.text = element_text(size = 20, family = 'Open Sans', margin = margin(b = 0.5, unit = 'line')),
        # facet text
        strip.text = element_text(size = 24, family = 'Open Sans'),
        # backgrounds, lines
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.grid = element_line(color = 'gray'),
        axis.line = element_line(color = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        # positioning
        legend.position = 'bottom',
        legend.key = element_rect(colour = 'transparent', fill = 'transparent')
)



# READ DATA ----
DATA_DIR = 'data'

combined_data = read_csv(here(DATA_DIR, 'conversation', 'processed', 'combined', 'predictions_combined.csv'))
combined_messages = read_csv(here(DATA_DIR, 'conversation', 'processed', 'combined', 'chat_messages_combined.csv'))
# sanity checks
# glimpse(combined_data)
# glimpse(combined_messages)

priors_data = read_csv(here(DATA_DIR, 'priors', 'processed', 'scale_priors_processed.csv'))
# sanity checks
# glimpse(priors_data)


# PROCESSING: update dataframes ----

# > 1. Filter priors df to only include BFI items ----
# TODO: move this to processing pipeline
priors_processed = priors_data |> 
        filter(scaleCategory == 'personality')
# sanity checks
# glimpse(priors_processed)


# > 2. Rename conditions to match pre-registration / text ----
# TODO: move this to processing pipeline
combined_data = combined_data |> 
        mutate(
                condition = case_when(
                        condition == 'baseline' ~ 'baseline',
                        condition == 'deep' ~ 'deep',
                        condition == 'shallow' ~ 'small talk',
                        TRUE ~ condition
                )
        )
combined_data$condition = factor(
        combined_data$condition,
        levels = c('baseline', 'small talk', 'deep')
)
# sanity checks
# glimpse(combined_data)

# Update condition in message data
combined_messages = combined_messages |> 
        mutate(
                condition = case_when(
                        condition == 'baseline' ~ 'baseline',
                        condition == 'deep' ~ 'deep',
                        condition == 'shallow' ~ 'small talk'
                )
        )
combined_messages$condition = factor(
        combined_messages$condition,
        levels = c('baseline', 'small talk', 'deep')
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
# sanity checks
# glimpse(self_data)

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
# sanity checks
# glimpse(self_partner_data)
# self_data |> filter(prolificId == '808d', scale_id == 30) |> select(response)
# partner_preds |> filter(prolificId == '9ce5', scale_id == 30) |> select(response)
# self_data |> filter(prolificId == '808d', scale_id == 14) |> select(response)
# partner_preds |> filter(prolificId == '9ce5', scale_id == 14) |> select(response)



# > 4. Add trait-level information ----
# TODO: we should put these in a CSV somewhere
extraversion_scales = c(
        "I see myself as someone who is talkative",
        "I see myself as someone who is reserved",
        "I see myself as someone who is full of energy",
        "I see myself as someone who generates a lot of enthusiasm",
        "I see myself as someone who has an assertive personality",
        "I see myself as someone who is sometimes shy, inhibited",
        "I see myself as someone who is outgoing, sociable",
        "I see myself as someone who tends to be quiet"
)
agreeableness_scales = c(
        "I see myself as someone who tends to find fault with others",
        "I see myself as someone who is helpful and unselfish with others",
        "I see myself as someone who starts quarrels with others",
        "I see myself as someone who has a forgiving nature",
        "I see myself as someone who is generally trusting",
        "I see myself as someone who can be cold and aloof",
        "I see myself as someone who is considerate and kind to almost everyone",
        "I see myself as someone who is sometimes rude to others",
        "I see myself as someone who likes to cooperate with others"
)
conscientiousness_scales = c(
        "I see myself as someone who does a thorough job",
        "I see myself as someone who can be somewhat careless",
        "I see myself as someone who is a reliable worker",
        "I see myself as someone who tends to be disorganized",
        "I see myself as someone who tends to be lazy",
        "I see myself as someone who perseveres until the task is finished",
        "I see myself as someone who does things efficiently",
        "I see myself as someone who makes plans and follows through with them",
        "I see myself as someone who is easily distracted"
)
neuroticism_scales = c(
        "I see myself as someone who is depressed, blue",
        "I see myself as someone who is relaxed, handles stress well",
        "I see myself as someone who can be tense",
        "I see myself as someone who worries a lot",
        "I see myself as someone who is emotionally stable, not easily upset",
        "I see myself as someone who can be moody",
        "I see myself as someone who remains calm in tense situations",
        "I see myself as someone who gets nervous easily"
)
openness_scales = c(
        "I see myself as someone who is original, comes up with new ideas",
        "I see myself as someone who is curious about many different things",
        "I see myself as someone who is ingenious, a deep thinker",
        "I see myself as someone who is inventive",
        "I see myself as someone who values artistic, aesthetic experiences",
        "I see myself as someone who likes to reflect, play with ideas",
        "I see myself as someone who has few artistic interests",
        "I see myself as someone who prefers work that is routine",
        "I see myself as someone who is sophisticated in art, music, or literature",
        "I see myself as someone who has an active imagination"
)

# Get trait lookup for scales
scale_mapping = self_partner_data |> 
        select(scale_id, scale_text) |> 
        distinct() |> 
        mutate(
                Big5_Dimension = case_when(
                        scale_text %in% extraversion_scales ~ "Extraversion",
                        scale_text %in% agreeableness_scales ~ "Agreeableness",
                        scale_text %in% conscientiousness_scales ~ "Conscientiousness",
                        scale_text %in% neuroticism_scales ~ "Neuroticism",
                        scale_text %in% openness_scales ~ "Openness",
                        TRUE ~ "NA"
                )
        )
# sanity checks
# table(scale_mapping$Big5_Dimension)
# scale_mapping |> filter(Big5_Dimension == 'NA')

# Add trait information to self_partner_data 
self_partner_data = self_partner_data |> 
        left_join(
                scale_mapping,
                by = c('scale_id', 'scale_text')
        )
# sanity checks
# table(self_partner_data$Big5_Dimension)
# unique(self_partner_data$Big5_Dimension)
# sum(is.na(self_partner_data$Big5_Dimension))
# glimpse(self_partner_data)


# > 5. Update responses, predictions, and prior predictions for reverse coded items ----
# Lookup for reverse-coded scales
reverse_coded_scales = c(
        # Extraversion
        "I see myself as someone who is reserved",
        "I see myself as someone who tends to be quiet",
        "I see myself as someone who is sometimes shy, inhibited",
        # Agreeableness
        "I see myself as someone who tends to find fault with others",
        "I see myself as someone who starts quarrels with others",
        "I see myself as someone who can be cold and aloof",
        "I see myself as someone who is sometimes rude to others",
        # Conscientiousness
        "I see myself as someone who can be somewhat careless",
        "I see myself as someone who tends to be disorganized",
        "I see myself as someone who tends to be lazy",
        "I see myself as someone who is easily distracted",
        # Neuroticism
        "I see myself as someone who is relaxed, handles stress well",
        "I see myself as someone who is emotionally stable, not easily upset",
        "I see myself as someone who remains calm in tense situations",
        # Openness
        "I see myself as someone who prefers work that is routine",
        "I see myself as someone who has few artistic interests"
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
# sanity checks
# glimpse(self_partner_data)
# self_partner_data |> filter(reverse_item) |> select(self_response, self_response_coded, partner_prediction, partner_prediction_coded) |> glimpse()

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
# sanity checks
# glimpse(priors_processed)
# priors_processed |> filter(reverse_item) |> select(priorResponse, priorResponse_coded) |> glimpse()



# > 6. Compute error values ----

# Add absolute, squared, signed error and variance of predictions
self_partner_data = self_partner_data |> 
        mutate(
                abs_error = abs(partner_prediction_coded - self_response_coded),
                sq_error = (partner_prediction_coded - self_response_coded)^2,
                signed_error = partner_prediction_coded - self_response_coded
        )
# sanity checks
# glimpse(self_partner_data)


# > 7. Summarize error values ----

# Overall (by subject): mean prediction error + 95% CIs
error_overall = self_partner_data |> 
        group_by(groupId, prolificId) |> 
        summarize(
                mean_abs_error = mean(abs_error, na.rm = TRUE),
                mean_sq_error = mean(sq_error, na.rm = TRUE),
                rmse = sqrt(mean(sq_error, na.rm = TRUE)),
                mean_signed_error = mean(signed_error, na.rm = TRUE),
                prediction_variance = var(partner_prediction_coded, na.rm = TRUE),
                prediction_sd = sd(partner_prediction_coded, na.rm = TRUE),
                .groups = 'drop'
        ) |>
        summarize(
                mean_abs_error = list(Hmisc::smean.cl.boot(mean_abs_error)),
                mean_sq_error = list(Hmisc::smean.cl.boot(mean_sq_error)),
                rmse = list(Hmisc::smean.cl.boot(rmse)),
                mean_signed_error = list(Hmisc::smean.cl.boot(mean_signed_error)),
                mean_var = list(Hmisc::smean.cl.boot(prediction_variance)),
                mean_sd = list(Hmisc::smean.cl.boot(prediction_sd))
        ) |>
        tidyr::unnest_wider(
                col = c('mean_abs_error', 'mean_sq_error', 'rmse', 'mean_signed_error', 'mean_var', 'mean_sd'),
                names_sep = '_'
        )
# sanity checks
glimpse(error_overall)

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
# sanity checks
glimpse(error_by_condition)


# By condition * trait: mean prediction error + 95% CIs
error_by_condition_trait = self_partner_data |> 
        group_by(condition, groupId, prolificId, Big5_Dimension) |> 
        summarize(
                mean_abs_error = mean(abs_error, na.rm = TRUE),
                mean_sq_error = mean(sq_error, na.rm = TRUE),
                rmse = sqrt(mean(sq_error, na.rm = TRUE)),
                mean_signed_error = mean(signed_error, na.rm = TRUE),
                prediction_variance = var(partner_prediction_coded, na.rm = TRUE),
                prediction_sd = sd(partner_prediction_coded, na.rm = TRUE),
                .groups = 'drop'
        ) |>
        group_by(condition, Big5_Dimension) |>
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
# sanity checks
glimpse(error_by_condition_trait)



# PROCESSING: priors bootstrapping ----

# Number of boostrapped samples
# NB: 1k samples takes 1-2 mins. 
# TODO: run with 10k, save as CSV / Rdata
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
# Initialize mean bootstrapped errors by condition
sampled_error_condition = data.frame(
        sample_idx = numeric(),
        condition = character(),
        mean_abs_error = numeric(),
        mean_sq_error = numeric(),
        mean_rmse = numeric(),
        mean_signed_error = numeric(),
        mean_var = numeric(),
        mean_sd = numeric(),
        n_obs = numeric()
)
# Initialize mean bootstrapped errors by condition * trait
sampled_error_condition_trait = data.frame(
        sample_idx = numeric(),
        condition = character(),
        Big5_Dimension = character(),
        mean_abs_error = numeric(),
        mean_sq_error = numeric(),
        mean_rmse = numeric(),
        mean_signed_error = numeric(),
        mean_var = numeric(),
        mean_sd = numeric(),
        n_obs = numeric()
)

# Run bootstrap resampling
# NB: takes ~3-4 mins to run
for (i in 1:SAMPLES) {
        if (i %% 100 == 0) {
                print(paste0('Bootstrap sample ', i, ' / ', SAMPLES))
        }
        # sample prior prediction participants and assign to self-participants
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
        # Update error summary by condition
        sampled_error_condition = rbind(
                sampled_error_condition,
                sampled_priors |>
                        group_by(condition, groupId, prolificId) |>
                        summarize(
                                subj_mean_abs_error = mean(sampled_prior_abs_error, na.rm = TRUE),
                                subj_mean_sq_error = mean(sampled_prior_sq_error, na.rm = TRUE),
                                subj_rmse = sqrt(mean(sampled_prior_sq_error, na.rm = TRUE)),
                                subj_mean_signed_error = mean(sampled_prior_signed_error, na.rm = TRUE),
                                subj_prediction_variance = var(sampled_priorResponse, na.rm = TRUE),
                                subj_prediction_sd = sd(sampled_priorResponse, na.rm = TRUE),
                                .groups = 'drop'
                        ) |>
                        group_by(condition) |>
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
        # Update error summary by condition * trait
        sampled_error_condition_trait = rbind(
                sampled_error_condition_trait,
                sampled_priors |>
                        group_by(condition, groupId, prolificId, Big5_Dimension) |>
                        summarize(
                                subj_mean_abs_error = mean(sampled_prior_abs_error, na.rm = TRUE),
                                subj_mean_sq_error = mean(sampled_prior_sq_error, na.rm = TRUE),
                                subj_rmse = sqrt(mean(sampled_prior_sq_error, na.rm = TRUE)),
                                subj_mean_signed_error = mean(sampled_prior_signed_error, na.rm = TRUE),
                                subj_prediction_variance = var(sampled_priorResponse, na.rm = TRUE),
                                subj_prediction_sd = sd(sampled_priorResponse, na.rm = TRUE),
                                .groups = 'drop'
                        ) |>
                        group_by(condition, Big5_Dimension) |>
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
# sanity checks
# TODO: are the "obs" values expected?
glimpse(sampled_error_overall)
glimpse(sampled_error_condition)
glimpse(sampled_error_condition_trait)



# PROCESSING: summarize priors bootstrapping ----

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
# sanity checks
glimpse(sampled_error_summary)

# Summarize bootstrapped errors by condition
sampled_error_condition_summary = sampled_error_condition |> 
        group_by(condition) |>
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
# sanity checks
glimpse(sampled_error_condition_summary)


# Summarize bootstrapped errors by condition * trait
sampled_error_condition_trait_summary = sampled_error_condition_trait |> 
        group_by(condition, Big5_Dimension) |>
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
# sanity checks
glimpse(sampled_error_condition_trait_summary)



# FIGURE: distributions of error overall ----
# TODO: consider summarizing as subject-level averages and plotting distribution of subject-level SDs as well

# Absolute error
self_partner_data |> 
        ggplot(
                aes(x = abs_error)
        ) +
        geom_density(
                linewidth = 1,
                fill = 'lightgray',
        ) +
        geom_vline(
                data = sampled_error_summary,
                aes(xintercept = mean_abs_error_Mean),
                linetype = 'dashed', linewidth = 1, alpha = 0.75, color = 'black'
        ) +
        # geom_vline(
        #   aes(xintercept = 0), linetype = 'dashed', linewidth = 0.5, color = 'black'
        # ) +
        scale_y_continuous(
                name = element_blank()
        ) +
        scale_x_continuous(
                name = 'absolute prediction error'
        ) +
        DEFAULT_THEME +
        theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'right'
        )

# Signed error
self_partner_data |>
        ggplot(
                aes(x = signed_error)
        ) +
        geom_density(
                linewidth = 1,
                fill = 'lightgray',
        ) +
        geom_vline(
                data = sampled_error_summary,
                aes(xintercept = mean_signed_error_Mean),
                linetype = 'dashed', linewidth = 1, alpha = 0.75, color = 'black'
        ) +
        # geom_vline(
        #   aes(xintercept = 0), linetype = 'dashed', linewidth = 0.5, color = 'black'
        # ) +
        scale_y_continuous(
                name = element_blank()
        ) +
        scale_x_continuous(
                name = 'signed prediction error'
        ) +
        DEFAULT_THEME +
        theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'right'
        )


# FIGURE: error distributions by condition ----
# Look at overall distributions of error by condition before summarizing below
# TODO: consider summarizing as subject-level averages and plotting distribution of subject-level SDs as well

# Absolute error
self_partner_data |> 
        ggplot(
                aes(x = abs_error, color = condition)
        ) +
        geom_density(
                linewidth = 1  
        ) +
        geom_vline(
                data = sampled_error_condition_summary,
                aes(xintercept = mean_abs_error_Mean, color = condition),
                linetype = 'dashed', linewidth = 1, alpha = 0.75
        ) +
        geom_vline(
                aes(xintercept = 0), linetype = 'dashed', linewidth = 0.5, color = 'black'
        ) +
        scale_y_continuous(
                name = element_blank()
        ) +
        scale_x_continuous(
                name = 'absolute prediction error'
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'right'
        )

# Signed error
self_partner_data |>
        ggplot(
                aes(x = signed_error, color = condition)
        ) +
        geom_density(
                linewidth = 1  
        ) +
        geom_vline(
                data = sampled_error_condition_summary,
                aes(xintercept = mean_signed_error_Mean, color = condition),
                linetype = 'dashed', linewidth = 1, alpha = 0.75
        ) +
        # geom_vline(
        #   aes(xintercept = 0), linetype = 'dashed', linewidth = 0.5, color = 'black'
        # ) +
        scale_y_continuous(
                name = element_blank()
        ) +
        scale_x_continuous(
                name = 'signed prediction error'
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'right'
        )



# FIGURE: prediction error by condition ----

# Plot
error_by_condition |> 
        ggplot(aes(
                x = condition, 
                y = mean_abs_error_Mean, 
                color = condition
        )
        ) +
        geom_point(size = 6) +
        geom_errorbar(
                aes(ymin = mean_abs_error_Lower, ymax = mean_abs_error_Upper), 
                width = 0, 
                linewidth = 1
        ) +
        # Priors -- bootstrapped
        geom_hline(
                data = sampled_error_condition_summary,
                aes(yintercept = mean_abs_error_Mean, color = condition),
                linetype = 'dashed',
                # color = 'gray50',
                linewidth = 2,
                alpha = 0.75
        ) +
        # annotate(
        #         'rect',
        #         xmin = -Inf, xmax = Inf,
        #         ymin = sampled_error_summary$Lower, ymax = sampled_error_summary$Upper,
        #         alpha = 0.2, fill = 'gray75'
        # ) +
        scale_x_discrete(
                name = element_blank(),
                labels = element_blank()
        ) +
        scale_y_continuous(
                name = 'absolute prediction error',
                breaks = seq(25, 30, by = 1),
                labels = seq(25, 30, by = 1),
                limits = c(25, 30)
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                legend.position = 'right',
                axis.ticks.x = element_blank(),
                # axis.line.x = element_blank()
        )



# ANALYSIS: absolute prediction error by condition ----

# Lmer models
null_model = lmer(
        abs_error ~ 1 +
                (1 | groupId / prolificId) +
                # (1 + condition | scale_id), # singular fit; swapped with below
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)
condition_effect = lmer(
        abs_error ~ 1 + condition +
                (1 | groupId / prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)

# Model comparison
anova(null_model, condition_effect)
# Emmeans
emmeans(condition_effect, pairwise ~ condition)



# FIGURE: absolute prediction error by condition * trait ----

# Plot
error_by_condition_trait |> 
        mutate(Big5_Dimension = fct_reorder(Big5_Dimension, mean_abs_error_Mean, .fun = mean, .desc = TRUE)) |>
        ggplot(aes(x = condition, y = mean_abs_error_Mean, color = condition)) +
        geom_point(size = 6) +
        geom_errorbar(aes(ymin = mean_abs_error_Lower, ymax = mean_abs_error_Upper), width = 0, linewidth = 1) +
        # Priors -- bootstrapped
        geom_hline(
                # data = sampled_error_summary_trait,
                data = sampled_error_condition_trait_summary |> mutate(Big5_Dimension = fct_reorder(Big5_Dimension, mean_abs_error_Mean, .fun = mean, .desc = TRUE)),
                aes(yintercept = mean_abs_error_Mean, color = condition),
                linetype = 'dashed',
                # color = 'gray50',
                linewidth = 1,
                alpha = 0.75
        ) +
        scale_x_discrete(
                name = element_blank(),
                labels = element_blank()
        ) +
        scale_y_continuous(
                name = 'absolute prediction error',
                breaks = seq(22, 34, by = 2),
                labels = c("", "24", "", "28", "", "32", ""),
                limits = c(22, 34)
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        facet_wrap(
                ~ Big5_Dimension,
                nrow = 1
        ) +
        DEFAULT_THEME +
        theme(
                legend.position = 'right',
                axis.ticks.x = element_blank(),
                # axis.line.x = element_blank()
        )


# ANALYSIS: absolute prediction error by condition * trait ----

# Lmer models
null_model = lmer(
        abs_error ~ 1 +
                (1 | groupId / prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)
condition_model = lmer(
        abs_error ~ 1 + condition +
                (1 | groupId / prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)
additive_model = lmer(
        abs_error ~ 1 + condition + Big5_Dimension +
                (1 | groupId / prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)
interaction_model = lmer(
        abs_error ~ 1 + condition * Big5_Dimension +
                (1 | groupId / prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)

# Model comparison
anova(null_model, condition_model, additive_model, interaction_model)
# Emmeans
emmeans(additive_model, pairwise ~ Big5_Dimension)
emmeans(interaction_model, pairwise ~ condition | Big5_Dimension)



# FIGURE: signed prediction error by condition ----

# Plot
error_by_condition |> 
        ggplot(aes(x = condition, y = mean_signed_error_Mean, color = condition)) +
        geom_point(size = 6) +
        geom_errorbar(aes(ymin = mean_signed_error_Lower, ymax = mean_signed_error_Upper), width = 0, linewidth = 1) +
        # Priors -- bootstrapped
        geom_hline(
                data = sampled_error_condition_summary,
                aes(yintercept = mean_signed_error_Mean, color = condition),
                linetype = 'dashed',
                # color = 'gray50',
                linewidth = 2,
                alpha = 0.75
        ) +
        geom_hline(yintercept = 0, linetype = 'dashed', color = 'black', linewidth = 0.5) +
        scale_x_discrete(
                name = element_blank(),
                labels = element_blank()
        ) +
        scale_y_continuous(
                name = 'signed prediction error',
                # breaks = seq(25, 30, by = 1),
                # labels = seq(25, 30, by = 1),
                # limits = c(25, 30)
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                legend.position = 'right',
                axis.ticks.x = element_blank(),
                # axis.line.x = element_blank()
        )


# ANALYSIS: signed prediction error by condition ----

# Lmer models
null_model = lmer(
        signed_error ~ 1 +
                (1 | prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F,
        control = lmerControl(optimizer = 'bobyqa')
        # control = lmerControl(optimizer = "Nelder_Mead")
)
condition_effect = lmer(
        signed_error ~ 1 + condition +
                (1 | prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F,
        control = lmerControl(optimizer = 'bobyqa')
        # control = lmerControl(optimizer = "Nelder_Mead")
)

# Model comparison
anova(null_model, condition_effect)
# Emmeans
emmeans(condition_effect, pairwise ~ condition)



# FIGURE: signed prediction error by condition * trait ----

# Plot
error_by_condition_trait |> 
        mutate(Big5_Dimension = fct_reorder(Big5_Dimension, mean_signed_error_Mean, .fun = mean, .desc = TRUE)) |>
        ggplot(aes(x = condition, y = mean_signed_error_Mean, color = condition)) +
        geom_point(size = 6) +
        geom_errorbar(aes(ymin = mean_signed_error_Lower, ymax = mean_signed_error_Upper), width = 0, linewidth = 1) +
        # Priors -- bootstrapped
        geom_hline(
                # data = sampled_error_summary_trait,
                data = sampled_error_condition_trait_summary |> mutate(Big5_Dimension = fct_reorder(Big5_Dimension, mean_signed_error_Mean, .fun = mean, .desc = TRUE)),
                aes(yintercept = mean_signed_error_Mean, color = condition),
                linetype = 'dashed',
                # color = 'gray50',
                linewidth = 1,
                alpha = 0.75
        ) +
        geom_hline(yintercept = 0, linetype = 'dashed', color = 'black', linewidth = 0.5) +
        scale_x_discrete(
                name = element_blank(),
                labels = element_blank()
        ) +
        scale_y_continuous(
                name = 'signed prediction error',
                # breaks = seq(22, 34, by = 2),
                # labels = c("", "24", "", "28", "", "32", ""),
                # limits = c(22, 34)
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        facet_wrap(
                ~ Big5_Dimension,
                nrow = 1
        ) +
        DEFAULT_THEME +
        theme(
                legend.position = 'right',
                axis.ticks.x = element_blank(),
                # axis.line.x = element_blank()
        )


# ANALYSIS: signed prediction error by condition * trait ----

# Regressions
null_model = lmer(
        signed_error ~ 1 +
                (1 | prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F,
        control = lmerControl(optimizer = 'bobyqa')
)
condition_model = lmer(
        signed_error ~ 1 + condition +
                (1 | prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F,
        control = lmerControl(optimizer = 'bobyqa')
)
additive_model = lmer(
        signed_error ~ 1 + condition + Big5_Dimension +
                (1 | prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)
interaction_model = lmer(
        signed_error ~ 1 + condition * Big5_Dimension +
                (1 | prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F,
        control = lmerControl(optimizer = 'bobyqa')
)

# Model comparison
anova(null_model, condition_model, additive_model, interaction_model)
# Compare main effect of trait
emmeans(additive_model, pairwise ~ Big5_Dimension)
emmeans(interaction_model, pairwise ~ condition | Big5_Dimension)


# FIGURE: RMSE by condition ----
error_by_condition$condition = factor(
        error_by_condition$condition,
        levels = c('baseline', 'small talk', 'deep')
)

# Plot
rmse_summary_fig = error_by_condition |> 
        ggplot(aes(
                x = condition, 
                y = rmse_Mean, 
                color = condition
        )
        ) +
        geom_point(size = 6) +
        geom_errorbar(
                aes(ymin = rmse_Lower, ymax = rmse_Upper), 
                width = 0.25, 
                linewidth = 1
        ) +
        # Priors -- bootstrapped
        geom_hline(
                data = sampled_error_condition_summary,
                aes(yintercept = mean_rmse_Mean, color = condition),
                linetype = 'dashed',
                # color = 'gray50',
                linewidth = 2,
                alpha = 0.75
        ) +
        # Priors error bars
        annotate(
                'rect',
                xmin = -Inf, xmax = Inf,
                ymin = sampled_error_condition_summary$mean_rmse_Lower[sampled_error_condition_summary$condition == 'baseline'],
                ymax = sampled_error_condition_summary$mean_rmse_Upper[sampled_error_condition_summary$condition == 'baseline'],
                alpha = 0.25,
                fill = QUESTION_CATEGORY_COLORS['baseline']
        ) +
        annotate(
                'rect',
                xmin = -Inf, xmax = Inf,
                ymin = sampled_error_condition_summary$mean_rmse_Lower[sampled_error_condition_summary$condition == 'small talk'],
                ymax = sampled_error_condition_summary$mean_rmse_Upper[sampled_error_condition_summary$condition == 'small talk'],
                alpha = 0.25,
                fill = QUESTION_CATEGORY_COLORS['small talk']
        ) +
        annotate(
                'rect',
                xmin = -Inf, xmax = Inf,
                ymin = sampled_error_condition_summary$mean_rmse_Lower[sampled_error_condition_summary$condition == 'deep'],
                ymax = sampled_error_condition_summary$mean_rmse_Upper[sampled_error_condition_summary$condition == 'deep'],
                alpha = 0.25,
                fill = QUESTION_CATEGORY_COLORS['deep']
        ) +
        scale_x_discrete(
                name = element_blank(),
                labels = element_blank()
        ) +
        scale_y_continuous(
                name = 'rmse',
                breaks = seq(30, 36, by = 2),
                labels = seq(30, 36, by = 2),
                limits = c(30, 36)
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        # theme_bw() +
        theme(
                legend.position = 'right',
                axis.ticks.x = element_blank(),
                legend.text = element_text(size = 28),
                axis.text.y = element_text(size = 28),
                axis.title.y = element_text(size = 36),
                # axis.line.x = element_blank()
        )
rmse_summary_fig
# Save figure
ggsave(
        rmse_summary_fig,
        filename = here('results', 'condition_effects_RAW.pdf'),
        # path = FIGURE_PATH,
        device = cairo_pdf,
        # NB: width and height need to be the same for digits in heatmap tiles to be sized right
        width = 11,
        height = 9,
)





# ANALYSIS: squared prediction error by condition ----

# Lmer models
null_model = lmer(
        sq_error ~ 1 +
                (1 | groupId / prolificId) +
                # (1 + condition | scale_id), # singular fit; swapped with below
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)
condition_effect = lmer(
        sq_error ~ 1 + condition +
                (1 | groupId / prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)

# Model comparison
anova(null_model, condition_effect)
# Emmeans
emmeans(condition_effect, pairwise ~ condition)


# FIGURE: RMSE by condition * trait ----

# Plot
error_by_condition_trait |> 
        mutate(Big5_Dimension = fct_reorder(Big5_Dimension, rmse_Mean, .fun = mean, .desc = TRUE)) |>
        ggplot(aes(x = condition, y = rmse_Mean, color = condition)) +
        geom_point(size = 6) +
        geom_errorbar(aes(ymin = rmse_Lower, ymax = rmse_Upper), width = 0, linewidth = 1) +
        # Priors -- bootstrapped
        geom_hline(
                # data = sampled_error_summary_trait,
                data = sampled_error_condition_trait_summary |> mutate(Big5_Dimension = fct_reorder(Big5_Dimension, mean_rmse_Mean, .fun = mean, .desc = TRUE)),
                aes(yintercept = mean_rmse_Mean, color = condition),
                linetype = 'dashed',
                # color = 'gray50',
                linewidth = 1,
                alpha = 0.75
        ) +
        scale_x_discrete(
                name = element_blank(),
                labels = element_blank()
        ) +
        scale_y_continuous(
                name = 'rmse',
                # breaks = seq(22, 34, by = 2),
                # labels = c("", "24", "", "28", "", "32", ""),
                # limits = c(22, 34)
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        facet_wrap(
                ~ Big5_Dimension,
                nrow = 1
        ) +
        DEFAULT_THEME +
        theme(
                legend.position = 'right',
                axis.ticks.x = element_blank(),
                # axis.line.x = element_blank()
        )


# ANALYSIS: squared prediction error by condition * trait ----

# Lmer models
null_model = lmer(
        sq_error ~ 1 +
                (1 | groupId / prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)
condition_model = lmer(
        sq_error ~ 1 + condition +
                (1 | groupId / prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)
additive_model = lmer(
        sq_error ~ 1 + condition + Big5_Dimension +
                (1 | groupId / prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)
interaction_model = lmer(
        sq_error ~ 1 + condition * Big5_Dimension +
                (1 | groupId / prolificId) +
                (1 | scale_id),
        data = self_partner_data,
        REML = F
)

# Model comparison
anova(null_model, condition_model, additive_model, interaction_model)
# Emmeans
emmeans(additive_model, pairwise ~ Big5_Dimension)
emmeans(interaction_model, pairwise ~ condition | Big5_Dimension)






# ANALYSIS: brms model comparison ----

# random effects only
model_null = brm(
        formula = bf(signed_error ~ 1 + 
                             (1 | groupId / prolificId) + # optimal nested structure
                             # (1 | groupId) + (1 | prolificId) + # non-nested but equivalent for us (each prolificId has exactly one groupId)
                             # (1 | prolificId) + # no nesting
                             (1 | scale_id),
                     sigma ~ 1),
        data = self_partner_data,
        family = gaussian(),
        file = here('analysis', 'brms_fits', 'null_model'),
        iter = 4000, # NB: increasing from default to help with divergent transitions
        control = list(adapt_delta = 0.9), # NB: increasing from default to help with divergent transitions in nested random effects
        seed = 1
)
# sanity checks
summary(model_null)

# condition only
model_condition = brm(
        formula = bf(signed_error ~ 1 + condition + 
                             (1 | groupId / prolificId) + # optimal nested structure
                             # (1 | groupId) + (1 | prolificId) + # non-nested but equivalent for us (each prolificId has exactly one groupId)
                             # (1 | prolificId) + # no nesting
                             (1 | scale_id), 
                     sigma ~ condition),
        data = self_partner_data,
        family = gaussian(),
        file = here('analysis', 'brms_fits', 'condition'),
        iter = 4000, # NB: increasing from default to help with divergent transitions
        control = list(adapt_delta = 0.98), # NB: increasing from default to help with divergent transitions in nested random effects
        seed = 1
)
# sanity checks
summary(model_condition)

# trait only
model_trait = brm(
        formula = bf(signed_error ~ 1 + Big5_Dimension + 
                             (1 | groupId / prolificId) + # optimal nested structure
                             # (1 | groupId) + (1 | prolificId) + # non-nested but equivalent for us (each prolificId has exactly one groupId)
                             # (1 | prolificId) + # no nesting
                             (1 | scale_id), 
                     sigma ~ Big5_Dimension),
        data = self_partner_data,
        family = gaussian(),
        file = here('analysis', 'brms_fits', 'trait'),
        iter = 5000, # NB: increasing from default to help with divergent transitions
        control = list(adapt_delta = 0.99), # NB: increasing from default to help with divergent transitions in nested random effects
        seed = 1
)
# sanity checks
summary(model_trait)

# condition + trait
model_condition_trait_ind = brm(
        formula = bf(signed_error ~ 1 + condition + Big5_Dimension + 
                             (1 | groupId / prolificId) + # optimal nested structure
                             # (1 | groupId) + (1 | prolificId) + # non-nested but equivalent for us (each prolificId has exactly one groupId)
                             # (1 | prolificId) + # no nesting
                             (1 | scale_id), 
                     sigma ~ condition + Big5_Dimension),
        data = self_partner_data,
        family = gaussian(),
        file = here('analysis', 'brms_fits', 'condition_trait_ind'),
        iter = 5000, # NB: increasing from default to help with divergent transitions
        control = list(adapt_delta = 0.98), # NB: increasing from default to help with divergent transitions in nested random effects
        seed = 1
)
# sanity checks
summary(model_condition_trait_ind)

# condition * trait
model_condition_trait = brm(
        formula = bf(signed_error ~ 1 + condition * Big5_Dimension + 
                             (1 | groupId / prolificId) + # optimal nested structure
                             # (1 | groupId) + (1 | prolificId) + # non-nested but equivalent for us (each prolificId has exactly one groupId)
                             # (1 | prolificId) + # no nesting
                             (1 | scale_id), 
                     sigma ~ condition * Big5_Dimension),
        data = self_partner_data,
        family = gaussian(),
        file = here('analysis', 'brms_fits', 'condition_trait'),
        iter = 4000, # NB: increasing from default to help with divergent transitions
        control = list(adapt_delta = 0.95), # NB: increasing from default to help with divergent transitions in nested random effects
        seed = 1
)
# sanity checks
summary(model_condition_trait)


# Leave-one-out cross-validation
model_null = add_criterion(
        model_null,
        criterion = 'loo',
        reloo = T,
        file = here('analysis', 'brms_fits', 'null_model')
)
model_condition = add_criterion(
        model_condition,
        criterion = 'loo',
        reloo = T,
        file = here('analysis', 'brms_fits', 'condition')
)
model_trait = add_criterion(
        model_trait,
        criterion = 'loo',
        reloo = T,
        file = here('analysis', 'brms_fits', 'trait')
)
# NB: this can take a few minutes
model_condition_trait_ind = add_criterion(
        model_condition_trait_ind,
        criterion = 'loo',
        reloo = T,
        file = here('analysis', 'brms_fits', 'condition_trait_ind')
)
# NB: this can take a few minutes
model_condition_trait = add_criterion(
        model_condition_trait,
        criterion = 'loo',
        reloo = T,
        file = here('analysis', 'brms_fits', 'condition_trait')
)

# Model comparison (brms)
# Lower == worse fit
loo_compare(model_null, model_condition) # condition only
loo_compare(model_null, model_trait) # trait only
loo_compare(model_condition, model_condition_trait_ind) # condition + trait > condition
loo_compare(model_trait, model_condition_trait_ind) # condition + trait > trait
loo_compare(model_condition_trait_ind, model_condition_trait) # condition * trait
loo_compare(model_trait, model_condition_trait) # condition * trait


# NB: this ordering can be slightly different from pairwise orderings above
loo_compare(
        model_null,
        model_condition,
        model_trait,
        model_condition_trait_ind,
        model_condition_trait
)

# TODO: consider more sophisticated comparisons 
# 1. stacking weights, 2. p(\delta elpd > 0), 3. \delta elpd -> expected predictive gain



# FIGURE: brms estimates of error by condition ----

# Get condition RMSE
params_condition = data.frame(
        condition = sort(unique(self_partner_data$condition))
)
# RMSE of model predicted signed errors
mu_draws_condition = posterior_epred(model_condition, newdata = params_condition, dpar = 'mu', re_formula = NA)
sigma_draws_condition = posterior_linpred(model_condition, newdata = params_condition, dpar = 'sigma', re_formula = NA, transform = TRUE)
rmse_draws_condition = sqrt(mu_draws_condition^2 + sigma_draws_condition^2)

# Summarize RMSE estimates by condition
rmse_summary_condition = apply(rmse_draws_condition, 2, function(x) Hmisc::smean.cl.boot(x))
rmse_df_condition = params_condition |> 
        mutate(
                rmse = rmse_summary_condition['Mean',],
                rmse_low = rmse_summary_condition['Lower',],
                rmse_high = rmse_summary_condition['Upper',]
        )
rmse_df_condition$condition = factor(rmse_df_condition$condition, levels = c('baseline', 'small talk', 'deep'))

# RMSE plot
rmse_fig_condition = rmse_df_condition |> 
        ggplot(aes(x = condition, y = rmse, color = condition)) +
        geom_point(size = 6) +
        geom_errorbar(
                aes(ymin = rmse_low, ymax = rmse_high),
                # width = 0,
                linewidth = 1,
                width = 0.25
        ) +
        geom_hline(
                data = sampled_error_condition_summary,
                aes(yintercept = sqrt(mean_sq_error_Mean), color = condition),
                linetype = 'dashed', linewidth = 1, alpha = 0.75
        ) +
        scale_x_discrete(
                name = element_blank(),
                labels = element_blank()
        ) +
        scale_y_continuous(
                name = 'rmse',
                # breaks = seq(32, 34, by = 1),
                # labels = seq(32, 34, by = 1),
                # limits = c(32, 34)
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        # theme_bw() +
        DEFAULT_THEME +
        theme(
                strip.background = element_blank(),
                strip.text = element_text(size = 24),
                # strip.text = element_blank(),
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
                legend.position = 'right'
                # legend.position = 'none'
        )
rmse_fig_condition
# Save figure
# ggsave(
#   rmse_fig_condition,
#   filename = here('results', 'condition_effects_RAW.pdf'),
#   # path = FIGURE_PATH,
#   device = cairo_pdf,
#   # NB: width and height need to be the same for digits in heatmap tiles to be sized right
#   width = 8,
#   height = 6,
# )


# ANALYSIS: compare model condition RMSE to bootstrapped "prior" RMSE ----
colnames(rmse_draws_condition) = params_condition$condition
# sanity checks
glimpse(rmse_draws_condition)
glimpse(sampled_error_condition)

sampled_prior_errors_baseline = sample(
        sampled_error_condition |> filter(condition == 'baseline') |> pull(mean_rmse),
        size = nrow(rmse_draws_condition),
        replace = TRUE
)

sampled_prior_errors_small_talk = sample(
        sampled_error_condition |> filter(condition == 'small talk') |> pull(mean_rmse),
        size = nrow(rmse_draws_condition),
        replace = TRUE
)

sampled_prior_errors_deep = sample(
        sampled_error_condition |> filter(condition == 'deep') |> pull(mean_rmse),
        size = nrow(rmse_draws_condition),
        replace = TRUE
)

# Compare RMSE distributions
# Values > 0 mean the model improved over the prior
baseline_improvement = sampled_prior_errors_baseline - rmse_draws_condition[,'baseline']
small_talk_improvement = sampled_prior_errors_small_talk - rmse_draws_condition[,'small talk']
deep_improvement = sampled_prior_errors_deep - rmse_draws_condition[,'deep']

Hmisc::smean.cl.boot(baseline_improvement > 0)
Hmisc::smean.cl.boot(small_talk_improvement > 0)
Hmisc::smean.cl.boot(deep_improvement > 0)


# FIGURE: brms estimates of error by condition * trait ----

# Get model condition * trait RMSEs
# Dataframe for storing condition * trait params
params_condition_trait = expand.grid(
        Big5_Dimension = sort(unique(self_partner_data$Big5_Dimension)),
        condition = sort(unique(self_partner_data$condition))
)

# NB: code here from GPT
# NB: this is RMSE of model predicted signed error relative to actual signed error
# Posterior predictions for each observation
# pp = posterior_predict(model_condition_trait, re_formula = NA) # fixed effects only
# pp is a matrix: rows = posterior draws, cols = observations
# dim(pp)  # e.g., 4000 draws × N observations

# Function to compute RMSE per posterior draw
# compute_rmse = function(y, yhat) {
#   sqrt(mean((y - yhat)^2))
# }

# Loop over Big5 × condition
# rmse_df = self_partner_data |> 
#   mutate(obs_id = row_number()) |>
#   group_by(Big5_Dimension, condition) |> 
#   summarize(
#     obs_ids = list(obs_id), # get row numbers for this condition * trait combination
#     .groups = 'drop'
#   ) |> 
#   rowwise() |> 
#   mutate(
#     # TODO: understand this code better...
#     rmse_posterior_draws = list({
#       y_ids = obs_ids[[1]]
#       pp_group = as.matrix(pp[, y_ids, drop = FALSE])
#       apply(pp_group, 1, function(yhat_draw) compute_rmse(self_partner_data$signed_error[y_ids], yhat_draw))
#     }),
#     # Get mean and 95% CIs from the posteriors
#     rmse_summary = list(Hmisc::smean.cl.boot(unlist(rmse_posterior_draws))),
#   ) |> 
#   tidyr::unnest_wider(rmse_summary) |>
#   rename(
#     rmse = Mean,
#     rmse_low = Lower,
#     rmse_high = Upper
#   )
# sanity checks
# glimpse(rmse_df)

# RMSE of model predicted signed errors
mu_draws_condition_trait = posterior_epred(model_condition_trait, newdata = params_condition_trait, dpar = 'mu', re_formula = NA)
sigma_draws_condition_trait = posterior_linpred(model_condition_trait, newdata = params_condition_trait, dpar = 'sigma', re_formula = NA, transform = TRUE)
rmse_draws_condition_trait = sqrt(mu_draws_condition_trait^2 + sigma_draws_condition_trait^2)
# sanity checks
# glimpse(mu_draws_condition_trait)
# dim(mu_draws_condition_trait)
# rownames(mu_draws_condition_trait)
# colnames(mu_draws_condition_trait)
# glimpse(sigma_draws_condition_trait)
# dim(sigma_draws_condition_trait)
# rownames(sigma_draws_condition_trait)
# colnames(sigma_draws_condition_trait)
# glimpse(rmse_draws_condition_trait)
# dim(rmse_draws_condition_trait)
# rownames(rmse_draws_condition_trait)
# colnames(rmse_draws_condition_trait)

rmse_summary_condition_trait = apply(rmse_draws_condition_trait, 2, function(x) Hmisc::smean.cl.boot(x))
# sanity checks
# glimpse(rmse_summary_condition_trait)
# dim(rmse_summary_condition_trait)
# rownames(rmse_summary_condition_trait)
# colnames(rmse_summary_condition_trait)

rmse_df_condition_trait = params_condition_trait |> 
        mutate(
                rmse = rmse_summary_condition_trait['Mean',],
                rmse_low = rmse_summary_condition_trait['Lower',],
                rmse_high = rmse_summary_condition_trait['Upper',]
        )

# Extract condition * trait means and SDs
bias_condition_trait = fitted(
        model_condition_trait,
        newdata = params_condition_trait,
        dpar = 'mu',
        re_formula = NA,
        summary = TRUE
)
sigmas_condition_trait = fitted(
        model_condition_trait,
        newdata = params_condition_trait,
        dpar = 'sigma',
        re_formula = NA,
        summary = TRUE
)
condition_trait_effects = cbind(
        params_condition_trait,
        bias = bias_condition_trait[, "Estimate"],
        bias_low = bias_condition_trait[, "Q2.5"],
        bias_high = bias_condition_trait[, "Q97.5"],
        sd = sigmas_condition_trait[, "Estimate"],
        sd_low = sigmas_condition_trait[, "Q2.5"],
        sd_high = sigmas_condition_trait[, "Q97.5"]
)
# sanity checks
glimpse(condition_trait_effects)

# Add RMSEs to condition_trait_effects
# RMSE of prediction error
# condition_trait_effects = condition_trait_effects |> 
#   left_join(
#     rmse_df |> select(Big5_Dimension, condition, rmse, rmse_low, rmse_high),
#     by = c('Big5_Dimension', 'condition')
#   )

# RMSE of model predicted signed error
condition_trait_effects = condition_trait_effects |> 
        left_join(
                rmse_df_condition_trait |> select(Big5_Dimension, condition, rmse, rmse_low, rmse_high),
                by = c('Big5_Dimension', 'condition')
        )
# sanity checks
glimpse(condition_trait_effects)

# Format for graphs
plot_df = condition_trait_effects |> 
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
# sanity checks
glimpse(plot_df)

# Re-order traits by descending order of bias
trait_order = condition_trait_effects |> 
        group_by(Big5_Dimension) |> 
        summarize(
                mean_rmse = mean(rmse),
                mean_bias = mean(bias),
                mean_sd = mean(sd),
                .groups = 'drop'
        ) |> 
        arrange(
                # desc(mean_bias)
                desc(mean_rmse)
        )

plot_df = plot_df |> 
        mutate(
                Big5_Dimension = factor(
                        Big5_Dimension,
                        levels = trait_order$Big5_Dimension
                ),
                condition = factor(
                        condition,
                        levels = c('baseline', 'small talk', 'deep')
                )
        )
# sanity checks
levels(plot_df$Big5_Dimension)
levels(plot_df$condition)

sampled_error_condition_trait_summary_fct = sampled_error_condition_trait_summary |> 
        mutate(
                Big5_Dimension = factor(
                        Big5_Dimension,
                        levels = trait_order$Big5_Dimension
                ),
                condition = factor(
                        condition,
                        levels = c('baseline', 'small talk', 'deep')
                )
        )
# sanity checks: same order as above?
levels(sampled_error_condition_trait_summary_fct$Big5_Dimension)
levels(sampled_error_condition_trait_summary_fct$condition)

# RMSE plot
rmse_fig = plot_df |> 
        filter(parameter == 'rmse') |>
        ggplot(aes(x = condition, y = estimate, color = condition)) +
        geom_point(size = 6) +
        geom_errorbar(
                aes(ymin = low, ymax = high),
                # width = 0,
                width = 0.25,
                linewidth = 1
        ) +
        # Priors
        geom_hline(
                data = sampled_error_condition_trait_summary_fct,
                aes(yintercept = mean_rmse_Mean, color = condition),
                linetype = 'dashed', linewidth = 1, alpha = 0.75
        ) +
        facet_wrap(~ Big5_Dimension, nrow = 1) +
        scale_x_discrete(
                name = element_blank(),
                labels = element_blank()
        ) +
        scale_y_continuous(
                name = 'rmse',
                breaks = seq(25, 50, by = 5),
                labels = seq(25, 50, by = 5),
                limits = c(24, 50)
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        # theme_bw() +
        DEFAULT_THEME +
        theme(
                strip.background = element_blank(),
                strip.text = element_text(size = 24),
                # strip.text = element_blank(),
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
                # legend.position = 'right'
                legend.position = 'none'
        )
rmse_fig


# Bias plot
bias_fig = plot_df |> 
        filter(parameter == 'bias') |>
        # mutate(Big5_Dimension = fct_reorder(Big5_Dimension, estimate, .fun = mean, .desc = TRUE)) |>
        ggplot(aes(x = condition, y = estimate, color = condition)) +
        geom_point(size = 6) +
        geom_errorbar(
                aes(ymin = low, ymax = high),
                # width = 0,
                width = 0.25,
                linewidth = 1
        ) +
        geom_hline(
                yintercept = 0,
                linetype = 'dashed',
                color = 'black',
                linewidth = 0.5
        ) +
        # Priors
        # geom_hline(
        #   data = sampled_error_condition_trait_summary_fct,
        #   aes(yintercept = mean_signed_error_Mean, color = condition),
        #   linetype = 'dashed', linewidth = 1, alpha = 0.75
        # ) +
        facet_wrap(~ Big5_Dimension, nrow = 1) +
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
        # theme_bw() +
        DEFAULT_THEME +
        theme(
                strip.background = element_blank(),
                # strip.text = element_text(size = 24),
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
                # legend.position = 'right'
                legend.position = 'none'
        )
bias_fig

# SD plot
sd_fig = plot_df |> 
        filter(parameter == 'sd') |>
        ggplot(aes(x = condition, y = estimate, color = condition)) +
        geom_point(size = 6) +
        geom_errorbar(
                aes(ymin = low, ymax = high),
                # width = 0,
                width = 0.25,
                linewidth = 1
        ) +
        facet_wrap(~ Big5_Dimension, nrow = 1) +
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
        # theme_bw() +
        DEFAULT_THEME +
        theme(
                strip.background = element_blank(),
                # strip.text = element_text(size = 24),
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
                # legend.position = 'right'
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
        filename = here('results', 'condition_by_trait_effects_RAW.pdf'),
        # path = FIGURE_PATH,
        device = cairo_pdf,
        # NB: width and height need to be the same for digits in heatmap tiles to be sized right
        width = 16,
        height = 9,
)


# ANALYSIS: trait differences ----

params_trait = data.frame(
        Big5_Dimension = sort(unique(self_partner_data$Big5_Dimension))
)

# Calculate RMSE estimates
mu_draws_trait = posterior_epred(model_trait, newdata = params_trait, dpar = 'mu', re_formula = NA)
sigma_draws_trait = posterior_linpred(model_trait, newdata = params_trait, dpar = 'sigma', re_formula = NA, transform = TRUE)
rmse_draws_trait = sqrt(mu_draws_trait^2 + sigma_draws_trait^2)

# Reformat
colnames(rmse_draws_trait) = params_trait$Big5_Dimension
colnames(mu_draws_trait) = params_trait$Big5_Dimension
colnames(sigma_draws_trait) = params_trait$Big5_Dimension
# Get trait order that matches plot (based on mean RMSE)
sorted_traits = params_trait$Big5_Dimension[rev(order(apply(rmse_draws_trait, 2, mean)))]


# RMSE means + 95% CI
for (i in 1:(length(sorted_traits))) {
        trait = sorted_traits[i]
        ci = Hmisc::smean.cl.boot(rmse_draws_trait[, trait])
        cat(sprintf('Trait: %s, RMSE Mean=%.3f, 95%% CrI=[%.3f, %.3f]\n',
                    trait, ci[1], ci[2], ci[3]))
}

# Pairwise RMSE comparisons
get_trait_diff = function(draws_matrix, trait1, trait2) {
        diff = draws_matrix[, trait1] - draws_matrix[, trait2]
        return(diff)
}

for (i in 1:(length(sorted_traits) - 1)) {
        for (j in (i + 1):length(sorted_traits)) {
                trait1 = sorted_traits[i]
                trait2 = sorted_traits[j]
                diff = get_trait_diff(rmse_draws_trait, trait1, trait2)
                prob = mean(diff > 0)
                ci = Hmisc::smean.cl.boot(diff)
                cat(sprintf('RMSE difference between %s and %s: Mean=%.3f, 95%% CrI=[%.3f, %.3f], P(>%s)=%.3f\n',
                            trait1, trait2, ci[1], ci[2], ci[3], '0', prob))
        }
}

# Bias comparisons: which ones are different from 0?
for (i in 1:(length(sorted_traits))) {
        trait = sorted_traits[i]
        ci = Hmisc::smean.cl.boot(mu_draws_trait[, trait])
        prob_greater = mean(mu_draws_trait[, trait] > 0)
        prob_less = mean(mu_draws_trait[, trait] < 0)
        cat(sprintf('Trait: %s, Bias Mean=%.3f, 95%% CrI=[%.3f, %.3f], P(>%s)=%.3f, P(<%s)=%.3f \n',
                    trait, ci[1], ci[2], ci[3], '0', prob_greater, '0', prob_less))
}


# SD means + 95% CI
for (i in 1:(length(sorted_traits))) {
        trait = sorted_traits[i]
        ci = Hmisc::smean.cl.boot(sigma_draws_trait[, trait])
        cat(sprintf('Trait: %s, SD Mean=%.3f, 95%% CrI=[%.3f, %.3f]\n',
                    trait, ci[1], ci[2], ci[3]))
}


# Follow-up with SD comparisons: which ones are different from each other?
for (i in 1:(length(sorted_traits) - 1)) {
        for (j in (i + 1):length(sorted_traits)) {
                trait1 = sorted_traits[i]
                trait2 = sorted_traits[j]
                diff = get_trait_diff(sigma_draws_trait, trait1, trait2)
                prob = mean(diff > 0)
                ci = Hmisc::smean.cl.boot(diff)
                cat(sprintf('SD difference between %s and %s: Mean=%.3f, 95%% CrI=[%.3f, %.3f], P(>%s)=%.3f\n',
                            trait1, trait2, ci[1], ci[2], ci[3], '0', prob))
        }
}


# ANALYSIS: conversation length ----

# Calculate subject-level RMSE
# TODO move this up to processing?
subject_summary = self_partner_data |> 
        group_by(condition, groupId, prolificId, partnerId) |> 
        summarize(
                mean_abs_error = mean(abs_error, na.rm = TRUE),
                mean_sq_error = mean(sq_error, na.rm = TRUE),
                rmse = sqrt(mean(sq_error, na.rm = TRUE)),
                mean_signed_error = mean(signed_error, na.rm = TRUE),
                prediction_variance = var(partner_prediction_coded, na.rm = TRUE),
                prediction_sd = sd(partner_prediction_coded, na.rm = TRUE),
                .groups = 'drop'
        )
subject_summary$condition = factor(
        subject_summary$condition,
        levels = c('baseline', 'small talk', 'deep')
)
# sanity checks
glimpse(subject_summary)

# Calculate conversation length metrics
# TODO: move this to processing?

# Add word count to messages df
message_metrics_df = combined_messages |> 
        # T/F in question prompting condition, NA in baseline
        filter(is.na(isQuestionPrompt) | isQuestionPrompt == FALSE) |> 
        mutate(word_count = str_count(message_string, '\\S+'))

# Get total messages and total words by subject
message_metrics_summary = message_metrics_df |> 
        group_by(condition, group_id, author) |> 
        summarize(
                total_messages = n(),
                total_words = sum(word_count, na.rm = TRUE),
                mean_words_per_message = mean(word_count, na.rm = TRUE),
                sd_words_per_message = sd(word_count, na.rm = TRUE),
                .groups = 'drop'
        ) |> 
        rename(
                groupId = group_id,
                prolificId = author
        )
# sanity checks
glimpse(message_metrics_summary)

# Stats: condition differences
# NOTE: participants seemed to write much more in `baseline` condition
message_metrics_summary |> 
        group_by(condition) |> 
        summarize(
                n_msg_summary = list(Hmisc::smean.cl.boot(total_messages)),
                n_word_summary = list(Hmisc::smean.cl.boot(total_words)),
                .groups = 'drop'
        ) |> 
        tidyr::unnest_wider(
                col = c('n_msg_summary', 'n_word_summary'),
                names_sep = '_'
        ) |> 
        glimpse()


# Combine with subject summary using *partner* prolific ID
subject_summary = subject_summary |> 
        left_join(
                message_metrics_summary,
                by = join_by('condition', 'groupId', 'partnerId' == 'prolificId')
        ) |> 
        rename(
                'partner_total_messages' = total_messages,
                'partner_total_words' = total_words,
                'partner_mean_words_per_message' = mean_words_per_message,
                'partner_sd_words_per_message' = sd_words_per_message
        )
# sanity checks
glimpse(subject_summary)
# get message metrics for a sample participant
subject_summary |> filter(prolificId == 'bcd6') |> select(partner_total_messages, partner_total_words, partner_mean_words_per_message, partner_sd_words_per_message)
# message metrics should be not equal to the above
message_metrics_summary |> filter(prolificId == 'bcd6') |> select(total_messages, total_words, mean_words_per_message, sd_words_per_message)
# message metrics should be equal to the above
message_metrics_summary |> filter(prolificId == '1010') |> select(total_messages, total_words, mean_words_per_message, sd_words_per_message)


# Figures: conversation length by condition

# DV: rmse
subject_summary |> 
        ggplot(aes(x = rmse, color = condition, fill = condition)) +
        geom_density(
                linewidth = 1,
                alpha = 0.2
        ) +
        scale_x_continuous(
                name = 'rmse',
                breaks = seq(round(min(subject_summary$rmse), -1), max(subject_summary$rmse), by = 10),
                labels = seq(round(min(subject_summary$rmse), -1), max(subject_summary$rmse), by = 10),
                limits = c(min(subject_summary$rmse), max(subject_summary$rmse))
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        scale_fill_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'bottom'
        )

# RMSE does not differ by condition
summary(aov(
        rmse ~ condition,
        data = subject_summary
)) # rmse does not vary significantly by condition


# > RMSE ~ TOTAL WORDS ----

# Distribution of total words by condition
subject_summary |>
        # filter(condition != 'baseline') |> 
        ggplot(aes(x = partner_total_words, color = condition, fill = condition)) +
        geom_density(
                linewidth = 1,
                alpha = 0.2
        ) +
        scale_x_log10(
                name = 'total words',
                # breaks = seq(0, max(subject_summary$partner_total_words), by = 50),
                # labels = seq(0, max(subject_summary$partner_total_words), by = 50),
                # limits = c(0, max(subject_summary$partner_total_words))
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        scale_fill_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'bottom'
        )

# Total words varies significantly by condition
summary(aov(
        log10(partner_total_words) ~ condition,
        data = subject_summary
        # data = subject_summary |> filter(condition != 'baseline')
))
# Baseline has more words than small talk and deep
emmeans(
        aov(
                log10(partner_total_words) ~ condition,
                data = subject_summary
                # data = subject_summary |> filter(condition != 'baseline')
        ),
        pairwise ~ condition,
        adjust = 'bonferroni'
)

# Relationship between total words and RMSE
subject_summary |> 
        # filter(condition != 'baseline') |>
        filter(partner_total_words > 15) |>
        ggplot(aes(x = partner_total_words, y = rmse)) +
        geom_point(
                alpha = 0.25,
                size = 2
        ) +
        geom_smooth(
                method = 'lm', 
                col = 'black'
        ) +
        scale_x_log10(
                name = 'partner total words',
                # breaks = seq(0, max(subject_summary$partner_total_words), by = 10),
                # labels = seq(0, max(subject_summary$partner_total_words), by = 10),
                # limits = c(0, max(subject_summary$partner_total_words))
        ) +
        scale_y_continuous(
                name = 'rmse',
                # breaks = seq(0, max(subject_summary$rmse), by = 10),
                # labels = seq(0, max(subject_summary$rmse), by = 10),
                # limits = c(0, max(subject_summary$rmse))
        ) +
        DEFAULT_THEME +
        theme(
                axis.text.y = element_text(size = 24),
                axis.text.x = element_text(size = 24),
        )

# Total words modestly predicts RMSE
summary(lm(
        # rmse ~ partner_total_words,
        rmse ~ log10(partner_total_words),
        data = subject_summary
        # data = subject_summary |> filter(condition != 'baseline')
        # sensitivity to outliers
        # data = subject_summary |> slice_max(partner_total_messages, n = 549)
        # data = subject_summary |> slice_max(partner_total_messages, prop = 0.9)
))
confint(
        lm(
                rmse ~ log10(partner_total_words),
                data = subject_summary
        )
)

# Effect of total words by condition
subject_summary |> 
        # filter(condition != 'baseline') |>
        ggplot(aes(x = partner_total_words, y = rmse, group = condition, color = condition)) +
        geom_point(
                alpha = 0.5,
                size = 2
        ) +
        geom_smooth(
                method = 'lm', 
        ) +
        scale_x_log10(
                name = 'partner total words',
                # breaks = seq(0, max(subject_summary$partner_total_words), by = 10),
                # labels = seq(0, max(subject_summary$partner_total_words), by = 10),
                # limits = c(0, max(subject_summary$partner_total_words))
        ) +
        scale_y_continuous(
                name = 'rmse',
                # breaks = seq(0, max(subject_summary$rmse), by = 10),
                # labels = seq(0, max(subject_summary$rmse), by = 10),
                # limits = c(0, max(subject_summary$rmse))
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME
# This effect remains insignificant after controlling for condition
summary(lm(
        rmse ~ condition + log10(partner_total_words),
        data = subject_summary
        # data = subject_summary |> filter(condition != 'baseline')
))
# No significant interaction between condition and total words
summary(lm(
        rmse ~ condition * log10(partner_total_words),
        data = subject_summary
        # data = subject_summary |> filter(condition != 'baseline')
))

# Final figure
total_words_deciles = subject_summary |> 
        mutate(
                # total_words_eq_deciles = cut_number(partner_total_words, n = 5),
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
# sanity checks
glimpse(total_words_deciles)

fig_words = subject_summary |> 
        # NB: filtering participants with very few words (<= 15) to improve visualization
        filter(partner_total_words > 15) |>
        ggplot() +
        # raw data
        geom_point(
                aes(x = partner_total_words, y = rmse),
                alpha = 0.15,
                size = 2,
                color = '#6082b6'
        ) +
        # decile means
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
                # breaks = seq(0, 300, by = 100),
                # labels = seq(0, 300, by = 100),
                # limits = c(0, 315)
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




# > RMSE ~ TOTAL MESSAGES ----

# Distribution of total messages by condition
subject_summary |>
        # filter(condition != 'baseline') |>
        ggplot(aes(x = partner_total_messages, color = condition, fill = condition)) +
        geom_density(
                linewidth = 1,
                alpha = 0.2
        ) +
        scale_x_log10(
                name = 'total messages',
                # breaks = seq(0, max(subject_summary$partner_total_messages), by = 10),
                # labels = seq(0, max(subject_summary$partner_total_messages), by = 10),
                # limits = c(0, max(subject_summary$partner_total_messages))
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        scale_fill_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'bottom'
        )

# Total messages varies significantly by condition
summary(aov(
        log10(partner_total_messages) ~ condition,
        data = subject_summary
        # data = subject_summary |> filter(condition != 'baseline')
))

# Baseline has more messages than small talk and deep
emmeans(
        aov(
                log10(partner_total_messages) ~ condition,
                data = subject_summary
                # data = subject_summary |> filter(condition != 'baseline')
        ),
        pairwise ~ condition,
        adjust = 'bonferroni'
)

# Relationship between total messages and RMSE
subject_summary |>
        # filter(condition != 'baseline') |> 
        ggplot(aes(x = partner_total_messages, y = rmse)) +
        geom_point(
                alpha = 0.25,
                size = 2
        ) +
        geom_smooth(
                method = 'lm', 
                col = 'black'
        ) +
        scale_x_log10(
                name = 'partner total messages',
                # breaks = seq(0, max(subject_summary$partner_total_messages), by = 10),
                # labels = seq(0, max(subject_summary$partner_total_messages), by = 10),
                # limits = c(0, max(subject_summary$partner_total_messages))
        ) +
        scale_y_continuous(
                name = 'rmse',
                # breaks = seq(0, max(subject_summary$rmse), by = 10),
                # labels = seq(0, max(subject_summary$rmse), by = 10),
                # limits = c(0, max(subject_summary$rmse))
        ) +
        DEFAULT_THEME +
        theme(
                axis.text.y = element_text(size = 24),
                axis.text.x = element_text(size = 24),
        )

# Total messages is significant predictor of RMSE
summary(lm(
        # rmse ~ partner_total_messages,
        rmse ~ log10(partner_total_messages),
        data = subject_summary
        # data = subject_summary |> filter(condition != 'baseline')
        # sensitivity to outliers
        # data = subject_summary |> slice_max(partner_total_messages, prop = .5)
))
# TODO: interpret coef: every additional message == 0.23 decrease in rmse?
# TODO: is this driven by outliers?

# Effect of total messages by condition
subject_summary |> 
        # filter(condition != 'baseline') |>
        ggplot(aes(x = partner_total_messages, y = rmse, group = condition, color = condition)) +
        geom_point(
                alpha = 0.5,
                size = 2
        ) +
        geom_smooth(
                method = 'lm', 
        ) +
        scale_x_log10(
                name = 'partner total messages',
                # breaks = seq(0, max(subject_summary$partner_total_messages), by = 10),
                # labels = seq(0, max(subject_summary$partner_total_messages), by = 10),
                # limits = c(0, max(subject_summary$partner_total_messages))
        ) +
        scale_y_continuous(
                name = 'rmse',
                # breaks = seq(0, max(subject_summary$rmse), by = 10),
                # labels = seq(0, max(subject_summary$rmse), by = 10),
                # limits = c(0, max(subject_summary$rmse))
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME
# This effect remains after controlling for condition
summary(lm(
        rmse ~ condition + log10(partner_total_messages),
        data = subject_summary
        # data = subject_summary |> filter(condition != 'baseline')
))
# However, no significant interaction between condition and total messages
summary(lm(
        rmse ~ condition * log10(partner_total_messages),
        data = subject_summary
        # data = subject_summary |> filter(condition != 'baseline')
))


# Final figure
total_msgs_deciles = subject_summary |> 
        mutate(
                total_msgs = partner_total_messages,
                # total_msgs_eq_deciles = cut_number(log_total_msgs, n = 10),
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
# sanity checks
glimpse(total_msgs_deciles)

fig_msgs = subject_summary |> 
        ggplot() +
        # raw data
        geom_point(
                aes(x = partner_total_messages, y = rmse),
                alpha = 0.1,
                size = 2,
                color = '#eb1f48'
        ) +
        # decile means
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
                # breaks = seq(0, 40, by = 10),
                # labels = seq(0, 40, by = 10),
                # limits = c(0, 40)
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

fig_preds_combined = fig_words + fig_msgs + 
        plot_layout(
                ncol = 1, # 2 for side by side
                axes = 'collect'
        )
fig_preds_combined
# Save figure
ggsave(
        fig_preds_combined,
        filename = here('results', 'message_stats_RAW.pdf'),
        # path = FIGURE_PATH,
        device = cairo_pdf,
        # NB: width and height need to be the same for digits in heatmap tiles to be sized right
        width = 6, # 12
        height = 8, # 8
)




# > RMSE ~ MEAN WORDS PER MESSAGE ----

# Distribution of mean words per message by condition
subject_summary |> 
        ggplot(aes(x = partner_mean_words_per_message, color = condition, fill = condition)) +
        geom_density(
                linewidth = 1,
                alpha = 0.2
        ) +
        scale_x_continuous(
                name = 'avg. words / message',
                breaks = seq(0, max(subject_summary$partner_mean_words_per_message), by = 10),
                labels = seq(0, max(subject_summary$partner_mean_words_per_message), by = 10),
                limits = c(0, max(subject_summary$partner_mean_words_per_message))
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        scale_fill_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'bottom'
        )

# Mean words per message varies significantly by condition
summary(aov(
        partner_mean_words_per_message ~ condition,
        data = subject_summary
))
# Baseline has fewer words per message than small talk and deep; small talk has less than deep
emmeans(
        aov(
                partner_mean_words_per_message ~ condition,
                data = subject_summary
        ),
        pairwise ~ condition,
        adjust = 'bonferroni'
)

# Relationship between mean words per message and RMSE
subject_summary |> 
        ggplot(aes(x = partner_mean_words_per_message, y = rmse)) +
        geom_point(
                alpha = 0.25,
                size = 2
        ) +
        geom_smooth(
                method = 'lm', 
                col = 'black'
        ) +
        scale_x_continuous(
                name = 'partner words / message',
                # breaks = seq(0, max(subject_summary$partner_mean_words_per_message), by = 10),
                # labels = seq(0, max(subject_summary$partner_mean_words_per_message), by = 10),
                # limits = c(0, max(subject_summary$partner_mean_words_per_message))
        ) +
        scale_y_continuous(
                name = 'rmse',
                # breaks = seq(0, max(subject_summary$rmse), by = 10),
                # labels = seq(0, max(subject_summary$rmse), by = 10),
                # limits = c(0, max(subject_summary$rmse))
        ) +
        DEFAULT_THEME

# Mean words per message is *almost* significant predictor of RMSE
# BUT this might be driven by large outlier 
summary(lm(
        rmse ~ partner_mean_words_per_message,
        data = subject_summary
))

# Effect of mean words per message by condition
subject_summary |> 
        ggplot(aes(x = partner_mean_words_per_message, y = rmse, group = condition, color = condition)) +
        geom_point(
                alpha = 0.5,
                size = 2
        ) +
        geom_smooth(
                method = 'lm', 
        ) +
        scale_x_continuous(
                name = 'partner words / message',
                # breaks = seq(0, max(subject_summary$partner_mean_words_per_message), by = 10),
                # labels = seq(0, max(subject_summary$partner_mean_words_per_message), by = 10),
                # limits = c(0, max(subject_summary$partner_mean_words_per_message))
        ) +
        scale_y_continuous(
                name = 'rmse',
                # breaks = seq(0, max(subject_summary$rmse), by = 10),
                # labels = seq(0, max(subject_summary$rmse), by = 10),
                # limits = c(0, max(subject_summary$rmse))
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME
# This effect remains (modestly) after controlling for condition
summary(lm(
        rmse ~ condition + partner_mean_words_per_message,
        data = subject_summary
))
# However, no significant interaction between condition and mean words per message
summary(lm(
        rmse ~ condition * partner_mean_words_per_message,
        data = subject_summary
))





# > RMSE ~ SD WORDS PER MESSAGE ----

# Distribution of sd words per message by condition
subject_summary |> 
        ggplot(aes(x = partner_sd_words_per_message, color = condition, fill = condition)) +
        geom_density(
                linewidth = 1,
                alpha = 0.2
        ) +
        scale_x_continuous(
                name = 'st. dev. words / message',
                breaks = seq(0, max(subject_summary$partner_sd_words_per_message), by = 10),
                labels = seq(0, max(subject_summary$partner_sd_words_per_message), by = 10),
                limits = c(0, max(subject_summary$partner_sd_words_per_message))
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        scale_fill_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'bottom'
        )

# SD words per message varies significantly by condition
summary(aov(
        partner_sd_words_per_message ~ condition,
        data = subject_summary
))
# Baseline has lower SD than small talk and deep; small talk has lower SD than than deep
emmeans(
        aov(
                partner_sd_words_per_message ~ condition,
                data = subject_summary
        ),
        pairwise ~ condition,
        adjust = 'bonferroni'
)

# Relationship between SD words per message and RMSE
subject_summary |> 
        ggplot(aes(x = partner_sd_words_per_message, y = rmse)) +
        geom_point(
                alpha = 0.25,
                size = 2
        ) +
        geom_smooth(
                method = 'lm', 
                col = 'black'
        ) +
        scale_x_continuous(
                name = 'st. dev. words / message',
                # breaks = seq(0, max(subject_summary$partner_sd_words_per_message), by = 10),
                # labels = seq(0, max(subject_summary$partner_sd_words_per_message), by = 10),
                # limits = c(0, max(subject_summary$partner_sd_words_per_message))
        ) +
        scale_y_continuous(
                name = 'rmse',
                # breaks = seq(0, max(subject_summary$rmse), by = 10),
                # labels = seq(0, max(subject_summary$rmse), by = 10),
                # limits = c(0, max(subject_summary$rmse))
        ) +
        DEFAULT_THEME

# SD words per message is not significant predictor of RMSE
# Large outlier here as well
summary(lm(
        rmse ~ partner_sd_words_per_message,
        data = subject_summary
))

# Effect of SD words per message by condition
subject_summary |> 
        ggplot(aes(x = partner_sd_words_per_message, y = rmse, group = condition, color = condition)) +
        geom_point(
                alpha = 0.5,
                size = 2
        ) +
        geom_smooth(
                method = 'lm', 
        ) +
        scale_x_continuous(
                name = 'st. dev. words / message',
                # breaks = seq(0, max(subject_summary$partner_sd_words_per_message), by = 10),
                # labels = seq(0, max(subject_summary$partner_sd_words_per_message), by = 10),
                # limits = c(0, max(subject_summary$partner_sd_words_per_message))
        ) +
        scale_y_continuous(
                name = 'rmse',
                # breaks = seq(0, max(subject_summary$rmse), by = 10),
                # labels = seq(0, max(subject_summary$rmse), by = 10),
                # limits = c(0, max(subject_summary$rmse))
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME
# This effect unchanged after controlling for condition
summary(lm(
        rmse ~ condition + partner_sd_words_per_message,
        data = subject_summary
))
# No significant interaction between condition and mean words per message
summary(lm(
        rmse ~ condition * partner_sd_words_per_message,
        data = subject_summary
))



# ANALYSIS: effect of individual questions ----


# Get depth ratings of each question from prior work
# Read in question evals
question_evals = read_csv(here(DATA_DIR, 'question_rating_data.csv'))
glimpse(question_evals)

# Process data
# Exclude participants who did not pass the attention check trials
attention_exclusions = question_evals |>
        filter(questionCategory == 'comprehension check') |>
        rowwise() |>
        mutate(
                comprehensionCheckDiff = comprehensionCheckRequestedResponse - response
        ) |>
        filter(
                comprehensionCheckDiff != 0
        ) |>
        select(subjectID, prolificID, comprehensionCheckRequestedResponse, response, comprehensionCheckDiff) |>
        # how many unique participants is this?
        select(prolificID) |>
        unique()
attention_exclusions

# Remove participants identified above
question_evals = question_evals |>
        filter(!(prolificID %in% attention_exclusions$prolificID))

# Remove comprehension check trials from main data
question_evals = question_evals |> 
        filter(questionCategory != 'comprehension check')

# DEBUG: duplicate question in the evals
unique(question_evals$question) # length 234
length(unique(question_evals$questionID)) # length 235
question_evals |> filter(question == 'What is your favorite holiday? Why?') |> distinct(questionCategory, questionID)

# Correct the above by assigning all same questionID (lower value)
question_evals = question_evals |> 
        mutate(
                questionID = ifelse(
                        question == 'What is your favorite holiday? Why?',
                        143,
                        questionID
                )
        )
# sanity checks: repeat the above
unique(question_evals$question) # length 234
length(unique(question_evals$questionID)) # length 234


# Get mean of all scale evaluations by question
# TODO: add factor 1 value for each question (correlated factor 1 and mean to confirm equivalence)
question_eval_summary = question_evals |> 
        group_by(questionCategory, questionID, question, prolificID) |>
        summarize(
                mean_subj_eval = mean(response, na.rm = TRUE),
                # scales = n(),
                .groups = 'drop'
        ) |> 
        ungroup() |> 
        group_by(questionCategory, questionID, question) |>
        summarize(
                scale_mean = Hmisc::smean.cl.boot(mean_subj_eval)[1],
                scale_lower = Hmisc::smean.cl.boot(mean_subj_eval)[2],
                scale_upper = Hmisc::smean.cl.boot(mean_subj_eval)[3],
                .groups = 'drop'
        ) |> 
        mutate(
                questionCategory = ifelse(
                        questionCategory == 'small talk',
                        'small talk',
                        'deep'
                )
        )
# sanity checks
glimpse(question_eval_summary)
# table(question_eval_summary$questionCategory)
# unique(question_eval_summary$questionCategory)
# sum(is.na(question_eval_summary$questionCategory))




# Add question evals to message data

# Get only question messages
chosen_Qs = combined_messages |> 
        filter(isQuestionPrompt == TRUE)

# sanity check
# glimpse(chosen_Qs)
# unique(chosen_Qs$message_string) 
# unique(question_eval_summary$question)
# All questions in chosen_Qs are in question_eval_summary 
# setdiff(unique(chosen_Qs$message_string), unique(question_eval_summary$question))
# Six questions in question_eval_summary are not in chosen_Qs
# setdiff(unique(question_eval_summary$question), unique(chosen_Qs$message_string))


# Add average ratings for each chosen question
chosen_Q_ratings = chosen_Qs |>
        left_join(
                question_eval_summary,
                by = join_by(
                        condition == questionCategory,
                        message_string == question
                ),
        )

# Summarize chosen question average ratings by dyad
dyad_question_ratings = chosen_Q_ratings |> 
        group_by(condition, group_id) |> 
        summarize(
                mean_question_rating = mean(scale_mean),
                n_questions = n()
        )

# sanity checks
glimpse(dyad_question_ratings)
table(dyad_question_ratings$condition)
# Plot distribution of average question ratings by condition
dyad_question_ratings |> 
        ggplot(aes(x = mean_question_rating, color = condition, fill = condition)) +
        geom_density(
                alpha = 0.25,
                linewidth = 1
        ) +
        scale_x_continuous(
                name = 'average question rating',
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        scale_fill_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'bottom'
        )


# Get average RMSE per dyad
dyad_rmse_summary = subject_summary |> 
        filter(condition != 'baseline') |> 
        group_by(condition, groupId) |>
        summarize(
                mean_rmse = mean(rmse),
                min_rmse = min(rmse),
                max_rmse = max(rmse),
                .groups = 'drop'
        )
# sanity checks
glimpse(dyad_rmse_summary)

# Plot distribution of mean RMSE
dyad_rmse_summary |> 
        ggplot(aes(x = mean_rmse, color = condition, fill = condition)) +
        geom_density(
                alpha = 0.25,
                linewidth = 1
        ) +
        scale_x_continuous(
                name = 'dyad mean RMSE',
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        scale_fill_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'bottom'
        )

# Plot relationship between RMSE for each dyad
dyad_rmse_summary |> 
        ggplot(aes(x = min_rmse, y = max_rmse, color = condition)) +
        geom_point(
                alpha = 0.5,
                size = 2
        ) +
        geom_smooth(
                method = 'lm',
                col = 'black'
        ) +
        geom_abline(
                linetype = 'dashed',
                linewidth = 0.75,
                color = 'black'
        ) +
        scale_x_continuous(
                name = 'dyad min RMSE',
                breaks = seq(0, max(dyad_rmse_summary$max_rmse), by = 10),
                labels = seq(0, max(dyad_rmse_summary$max_rmse), by = 10),
                limits = c(0, max(dyad_rmse_summary$max_rmse))
        ) +
        scale_y_continuous(
                name = 'dyad max RMSE',
                breaks = seq(0, max(dyad_rmse_summary$max_rmse), by = 10),
                labels = seq(0, max(dyad_rmse_summary$max_rmse), by = 10),
                limits = c(0, max(dyad_rmse_summary$max_rmse))
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                legend.position = 'bottom'
        )


# Add dyad average question ratings to dyad rmse summary
dyad_combined = dyad_rmse_summary |> 
        left_join(
                dyad_question_ratings,
                by = join_by(
                        condition,
                        groupId == group_id
                )
        )

# Plot relationship between average question rating and RMSE
dyad_combined |>
        ggplot(
                aes(
                        x = mean_question_rating, 
                        y = mean_rmse,
                        # y = min_rmse,
                        # y = max_rmse,
                        color = condition
                )
        ) +
        geom_point(
                alpha = 0.5,
                size = 2
        ) +
        geom_smooth(
                method = 'lm',
                col = 'black'
        ) +
        scale_x_continuous(
                name = 'average question rating',
                # breaks = seq(1, 7, by = 1),
                # labels = seq(1, 7, by = 1),
                # limits = c(1, 7)
        ) +
        scale_y_continuous(
                name = 'dyad mean RMSE',
                # breaks = seq(0, max(dyad_combined$mean_rmse), by = 10),
                # labels = seq(0, max(dyad_combined$mean_rmse), by = 10),
                # limits = c(0, max(dyad_combined$mean_rmse))
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.text.x = element_text(size = 18),
                axis.text.y = element_text(size = 18),
                legend.position = 'bottom'
        )

# What is the relationship
summary(lm(
        mean_rmse ~ mean_question_rating,
        data = dyad_combined
))
summary(lm(
        min_rmse ~ mean_question_rating,
        data = dyad_combined
))
summary(lm(
        max_rmse ~ mean_question_rating,
        data = dyad_combined
))



# ANALYSIS: brms question -> trait analysis ----

# Pre-processing: how many people discussed each question?
# NB: estimates of question-level effects may be v noisy for ones w small Ns
combined_messages |> 
        filter(isQuestionPrompt == TRUE) |>
        # NB: why do some of these have NA for question_idx, etc.?
        group_by(condition, message_string) |>
        summarize(
                n_participants = n_distinct(author),
                .groups = 'drop'
        ) |> 
        arrange(desc(n_participants)) |> 
        print(n = Inf)

# Calculate each participant's trait-level RMSE
# participant ID | group ID | trait | question ID | trait-level RMSE

# -> Summarize message data by participant, group, question
participant_question_summary = combined_messages |> 
        filter(isQuestionPrompt == TRUE) |> 
        distinct(
                condition,
                group_id,
                author,
                message_string,
                # question_idx # includes NAs so we can't include this
        )
# sanity checks
glimpse(participant_question_summary)
length(unique(participant_question_summary$message_string))
length(unique(participant_question_summary$author))
# Who is missing from participant_question_summary?
setdiff(
        unique(self_partner_data$prolificId[self_partner_data$condition != 'baseline']),
        unique(participant_question_summary$author)
)
# setdiff(
#   unique(participant_question_summary$author),
#   unique(self_partner_data$prolificId[self_partner_data$condition != 'baseline'])
# )
# Are these people in combined_messages?
combined_messages |> filter(author == '982c') |> glimpse()
combined_messages |> filter(author == '95b1') |> glimpse()
# Separate dyads. What's happening?
combined_messages |> 
        filter(group_id == 'printed-bronze') |> # Choosing questions "manually", not actually clicking them
        select(absolute_timestamp, isQuestionPrompt, message_string) |>
        arrange(absolute_timestamp) |>
        print(n = Inf) # Choosing questions "manually", not actually clicking them
combined_messages |> 
        filter(group_id == 'vicarious-plum') |> # Choosing questions "manually", not actually clicking them
        select(absolute_timestamp, isQuestionPrompt, message_string) |>
        arrange(absolute_timestamp) |>
        print(n = Inf) # Choosing questions "manually", not actually clicking them

# NB: may need to exclude these participants from analysis (left join from questions df not RMSE df)


# -> Calculate participants' (empirical!) trait-level RMSE
participant_trait_rmse = self_partner_data |> 
        # filter(condition != 'baseline') |> 
        group_by(
                condition,
                groupId,
                prolificId,
                Big5_Dimension
        ) |> 
        summarize(
                trait_rmse = sqrt(mean(sq_error, na.rm = TRUE)),
                n_items = n(),
                .groups = 'drop'
        )
# sanity checks
glimpse(participant_trait_rmse)
unique(participant_trait_rmse$Big5_Dimension)


# -> Join participant question data with trait RMSE data

setdiff(
        unique(participant_question_summary$author),
        unique(participant_trait_rmse$prolificId)
)
setdiff(
        unique(participant_question_summary$group_id),
        unique(participant_trait_rmse$groupId)
)

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

# sanity check: make sure rows increase as expected
glimpse(participant_question_summary) # 896 author-question combinations
# glimpse(participant_trait_rmse) # NB: this includes baseline and "extra" two people above so nrow isn't useful
glimpse(participant_question_trait_rmse) # 4480 author-question-trait rows = 896 * 5 traits


# -> Fit brms model predicting trait-level RMSE from question

# Z-score RMSE for modeling (GPT recommendation)
#' What global z-scoring does
#' Preserves:
#' * Trait differences in baseline difficulty
#' * Participant differences in overall accuracy
#' * Question-level variation
#' * Puts everything on a unitless, interpretable scale
#' *Makes priors easy and non-controversial
#' After global z-scoring:
#' * Intercept ≈ 0
#' * Fixed trait effects = mean differences in RMSE (in SD units)
#' * Random-effect SDs = proportion of total RMSE variance explained
participant_question_trait_rmse = participant_question_trait_rmse |> 
        mutate(
                rmse_z = as.numeric(scale(trait_rmse))
        )
# sanity checks
glimpse(participant_question_trait_rmse)
participant_question_trait_rmse |> 
        ggplot(
                aes(
                        # x = trait_rmse
                        x = rmse_z
                )
        ) +
        geom_density(
                alpha = 0.5,
                linewidth = 1
        ) +
        scale_x_continuous(
                name = 'trait-level RMSE (z scored)',
        ) +
        DEFAULT_THEME +
        theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                axis.title.y = element_blank()
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
        file = here('analysis', 'brms_fits', 'question_trait_effect'),
        # iter = 4000, # NB: increasing from default to help with divergent transitions
        # control = list(adapt_delta = 0.9), # NB: increasing from default to help with divergent transitions in nested random effects
        seed = 1
)

# -> Model interpretation

# Overall effect of questions, trait-specific effects
# NB: interpret the magnitude of these effects in SD units
vc = VarCorr(question_trait_effect)
vc
# $question$sd = 0.01 -> "essentially no evidence that some questions systematically reduce or increase RMSE overall."
# $`question:Big5_Dimension`$sd = 0.03 -> "Even trait-specific effects are very weak; no question seems to have a meaningful impact on RMSE for any trait."

# Most of the variance in RMSE is explained by:
# Participant-level differences in inference ability (sd(participant_id))
# Dyad-level differences (sd(dyad_id))



# Individual question comparisons
question_REs  = ranef(question_trait_effect)$question
question_trait_REs = ranef(question_trait_effect)$`question:Big5_Dimension`

# sanity checks
dim(question_REs) # 228 questions by 4 stats (estimate, SE, 2.5%, 97.5%)
head(question_REs)
# colnames(question_REs)
# rownames(question_REs)
question_REs[1,,]
dim(question_trait_REs) # 1140 question-trait combinations by 4 stats above
head(question_trait_REs)
colnames(question_trait_REs)
# rownames(question_trait_REs)
question_trait_REs[1,,]


# Identify top and bottom questions
df_question_RE = as.data.frame(question_REs) |> 
        rownames_to_column('question') |> 
        select(question, Estimate.Intercept, Est.Error.Intercept, Q2.5.Intercept, Q97.5.Intercept) |> 
        rename(
                overall_question_effect = Estimate.Intercept,
                overall_question_se = Est.Error.Intercept,
                overall_question_ci_lower = Q2.5.Intercept,
                overall_question_ci_upper = Q97.5.Intercept
        )
# sanity checks
glimpse(df_question_RE)

df_question_trait_RE = as.data.frame(question_trait_REs) |> 
        rownames_to_column('question_trait') |> 
        separate(question_trait, into = c('question', 'trait'), sep = '_') |>
        select(question, trait, Estimate.Intercept, Est.Error.Intercept, Q2.5.Intercept, Q97.5.Intercept) |> 
        rename(
                trait_effect = Estimate.Intercept,
                trait_se = Est.Error.Intercept,
                trait_ci_lower = Q2.5.Intercept,
                trait_ci_upper = Q97.5.Intercept
        )
# sanity checks
glimpse(df_question_trait_RE)


# Join and compute total effect
df_question_trait_combined_RE = df_question_trait_RE |> 
        left_join(df_question_RE, by = 'question') |> 
        mutate(
                total_effect = overall_question_effect + trait_effect
        ) 
# sanity checks
glimpse(df_question_trait_combined_RE)

# View questions with largest (negative) effects on RMSE overall (i.e., best)
df_question_trait_combined_RE |> 
        left_join(
                participant_question_trait_rmse |> distinct(condition, question),
                by = 'question'
        ) |> 
        arrange(total_effect) |> 
        head(10)
# Which traits were most representative among diagnostic questions?
# Which question-trait combinations were most diagnostic?

# View questions with largest (positive) effects on RMSE overall (i.e., worst)
df_question_trait_combined_RE |> 
        left_join(
                participant_question_trait_rmse |> distinct(condition, question),
                by = 'question'
        ) |>
        arrange(desc(total_effect)) |> 
        head(10)
# Which traits were most representative among misleading questions? -> Take-away: neuroticism!
# Which question-trait combinations were most misleading?


# Top question for each trait
top_qs = df_question_trait_combined_RE |> 
        group_by(trait) |> 
        slice_min(order_by = total_effect, n = 1) |> 
        ungroup()
top_qs


posterior_samples_question_trait = posterior_samples(question_trait_effect)
# sanity checks
dim(posterior_samples_question_trait)
head(rownames(posterior_samples_question_trait))
# head(colnames(posterior_samples_question_trait), 1000)

# GPT function for extracting 95% CrI for top questions for each trait from posterior samples
get_total_effect_ci = function(question, trait, post) {
        # Identify columns
        # NB: str_replace_all adds "." between each word of question
        overall_col = paste0('r_question[', str_replace_all(question, '\\s+', '.'), ',Intercept]')
        trait_col = paste0('r_question:Big5_Dimension[', str_replace_all(question, '\\s+', '.'), '_', trait, ',Intercept]')
        
        # Sum posterior draws
        summary_question = Hmisc::smean.cl.boot(post[[overall_col]])
        summary_trait = Hmisc::smean.cl.boot(post[[trait_col]])
        summary_total = Hmisc::smean.cl.boot(post[[overall_col]] + post[[trait_col]])
        
        data.frame(
                question = question,
                trait = trait,
                # mean_question_eff = summary_question[1],
                # ci_lower_question_eff = summary_question[2],
                # ci_upper_question_eff = summary_question[3],
                # mean_trait_eff = summary_trait[1],
                # ci_lower_trait_eff = summary_trait[2],
                # ci_upper_trait_eff = summary_trait[3],
                mean = summary_total[1],
                ci_l = summary_total[2],
                ci_u = summary_total[3]
        )
}


top_question_summary = top_qs |> 
        rowwise() |>
        do(
                get_total_effect_ci(.$question, .$trait, posterior_samples_question_trait)
        ) |> 
        arrange(mean)
top_question_summary


# FIGURE: Top diagnostic questions for each trait ----

top_question_fig = top_question_summary |> 
        left_join(
                participant_question_trait_rmse |> distinct(condition, question),
                by = 'question'
        ) |>
        ggplot(
                aes(
                        x = reorder(trait, mean),
                        y = mean,
                        ymin = ci_l,
                        ymax = ci_u,
                        color = condition,
                        label = str_wrap(question, width = 40)
                )
        ) +
        geom_point(
                size = 6
        ) +
        geom_errorbar(
                width = 0.1,
                linewidth = 1
        ) +
        geom_hline(
                yintercept = 0,
                linetype = 'dashed',
                linewidth = 0.5,
        ) +
        # add text of question
        geom_text(
                size = 4,
                angle = 90,    # Set the text angle to 90 degrees (vertical)
                hjust = 0,     # Adjust horizontal justification (0=bottom, 1=top of text aligned to point's y-coord)
                vjust = -.75   # Adjust vertical justification to position text above the point
        ) +
        scale_x_discrete(
                name = element_blank(),
        ) +
        scale_y_continuous(
                name = 'est. effect on trait-level RMSE (z-scored)',
                breaks = seq(-0.015, 0, by = 0.005),
                labels = seq(-0.015, 0, by = 0.005),
                limits = c(-0.015, 0)
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
                axis.text.y = element_text(size = 14),
                axis.title.y = element_text(size = 18),
                legend.position = 'none'
        )
top_question_fig
# Save figure
ggsave(
        top_question_fig,
        filename = here('results', 'question_trait_effect_RAW.pdf'),
        # path = FIGURE_PATH,
        device = cairo_pdf,
        # NB: width and height need to be the same for digits in heatmap tiles to be sized right
        width = 11,
        height = 9,
)


# Re-orient figure above
top_question_fig_horiz = top_question_summary |> 
        left_join(
                participant_question_trait_rmse |> distinct(condition, question),
                by = 'question'
        ) |>
        mutate(
                mean_inv = -1 * mean,
                ci_l_inv = -1 * ci_l,
                ci_u_inv = -1 * ci_u
        ) |> 
        ggplot(
                aes(
                        x = reorder(trait, mean),
                        y = mean_inv, # NB: inverting value for interpretability
                        ymin = ci_l_inv,
                        ymax = ci_u_inv,
                        color = condition,
                        label = str_wrap(question, width = 40)
                )
        ) +
        geom_point(
                size = 6
        ) +
        geom_errorbar(
                width = 0.1,
                linewidth = 1
        ) +
        # geom_hline(
        #   yintercept = 0,
        #   linetype = 'dashed',
        #   linewidth = 0.5,
        # ) +
        # add text of question
        geom_text(
                size = 4,
                # angle = 90,    # Set the text angle to 90 degrees (vertical)
                # hjust = 0,     # Adjust horizontal justification (0=bottom, 1=top of text aligned to point's y-coord)
                vjust = -.75   # Adjust vertical justification to position text above the point
        ) +
        scale_x_discrete(
                name = element_blank(),
        ) +
        scale_y_continuous(
                name = 'est. decrease in RMSE (z-scored)',
                breaks = seq(0, 0.015, by = 0.005),
                labels = seq(0, 0.015, by = 0.005),
                limits = c(0, 0.015)
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
                axis.title.y = element_text(size = 18),
                legend.position = 'none'
        )
top_question_fig_horiz
# Save figure
ggsave(
        top_question_fig_horiz,
        filename = here('results', 'question_trait_effect_horiz_RAW.pdf'),
        # path = FIGURE_PATH,
        device = cairo_pdf,
        # NB: width and height need to be the same for digits in heatmap tiles to be sized right
        width = 11,
        height = 9,
)



# ANALYSIS: Relationship between partner similarity and prediction accuracy ----

# Get partner similarity data
glimpse(self_partner_data)
unique(self_partner_data$target)
unique(self_partner_data$category)

# Summarize self responses by subject, trait
self_trait_summary = self_partner_data |> 
        group_by(
                condition, groupId, prolificId, target, Big5_Dimension
        ) |> 
        summarize(
                mean_trait_response = mean(self_response_coded, na.rm = TRUE),
                sd_trait_response = sd(self_response_coded, na.rm = TRUE),
                .groups = 'drop'
        ) |> 
        pivot_wider(
                names_from = Big5_Dimension,
                values_from = c(mean_trait_response, sd_trait_response)
        )
# sanity checks
glimpse(self_trait_summary)

# Join partner responses
self_partner_trait_summary = self_trait_summary |> 
        left_join(
                self_trait_summary |> 
                        select(
                                groupId,
                                prolificId,
                                starts_with('mean_trait_response_'),
                                starts_with('sd_trait_response_')
                        ) |> 
                        rename_with(
                                ~ paste0('partner_', .),
                                starts_with('mean_trait_response_')
                        ) |> 
                        rename_with(
                                ~ paste0('partner_', .),
                                starts_with('sd_trait_response_')
                        ) |> 
                        rename(
                                partner_prolificId = prolificId
                        ),
                by = join_by(
                        groupId,
                        # prolificId != partner_prolificId
                ),
                relationship = 'many-to-many'
        ) |> 
        filter(prolificId != partner_prolificId)
# sanity checks
glimpse(self_partner_trait_summary)


# Calculate distances in trait space
# TODO do we need to z-score the trait values before getting distance?
self_partner_trait_summary = self_partner_trait_summary |> 
        rowwise() |>
        mutate(
                big5_euclidean_distance = sqrt(
                        (mean_trait_response_Extraversion - partner_mean_trait_response_Extraversion)^2 +
                                (mean_trait_response_Agreeableness - partner_mean_trait_response_Agreeableness)^2 +
                                (mean_trait_response_Conscientiousness - partner_mean_trait_response_Conscientiousness)^2 +
                                (mean_trait_response_Neuroticism - partner_mean_trait_response_Neuroticism)^2 +
                                (mean_trait_response_Openness - partner_mean_trait_response_Openness)^2
                ),
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
                        use = 'complete.obs'
                ),
                # TODO: make this a separate function...
                cosine_distance = 1 - ( 
                        sum(
                                c(
                                        mean_trait_response_Extraversion,
                                        mean_trait_response_Agreeableness,
                                        mean_trait_response_Conscientiousness,
                                        mean_trait_response_Neuroticism,
                                        mean_trait_response_Openness
                                ) *
                                        c(
                                                partner_mean_trait_response_Extraversion,
                                                partner_mean_trait_response_Agreeableness,
                                                partner_mean_trait_response_Conscientiousness,
                                                partner_mean_trait_response_Neuroticism,
                                                partner_mean_trait_response_Openness
                                        )
                        ) /
                                ( 
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
                                        ) *
                                                sqrt(
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
# sanity checks
glimpse(self_partner_trait_summary)

# Distances are symmetric: just keep one for dyad summary
dyad_distance_summary = self_partner_trait_summary |> 
        group_by(
                condition,
                groupId
                # prolificId,
                # partner_prolificId
        ) |> 
        summarize(
                big5_euclidean_distance = mean(big5_euclidean_distance),
                correlation_distance = mean(correlation_distance),
                cosine_distance = mean(cosine_distance),
                .groups = 'drop'
        )

# sanity checks
glimpse(dyad_distance_summary)


# Calculate participant and dyad RMSE summaries
self_partner_rmse_summary = self_partner_data |> 
        group_by(condition, groupId, prolificId) |> 
        summarize(
                mean_abs_error = mean(abs_error, na.rm = TRUE),
                mean_sq_error = mean(sq_error, na.rm = TRUE),
                rmse = sqrt(mean(sq_error, na.rm = TRUE)),
                mean_signed_error = mean(signed_error, na.rm = TRUE),
                prediction_variance = var(partner_prediction_coded, na.rm = TRUE),
                prediction_sd = sd(partner_prediction_coded, na.rm = TRUE),
                .groups = 'drop'
        )
# sanity checks
glimpse(self_partner_rmse_summary)

# Summarize RMSEs by dyad
dyad_rmse_summary = self_partner_rmse_summary |>
        group_by(
                condition,
                groupId
        ) |> 
        summarize(
                dyad_mean_rmse = mean(rmse),
                dyad_min_rmse = min(rmse),
                dyad_max_rmse = max(rmse),
                .groups = 'drop'
        )
# sanity checks
glimpse(dyad_rmse_summary)



# Join dyad rmse with dyad distance
dyad_distance_rmse = dyad_rmse_summary |>
        left_join(
                dyad_distance_summary,
                by = join_by(
                        condition,
                        groupId
                )
        )
# sanity checks
glimpse(dyad_distance_rmse)


# RMSE ~ Euclidean distance
euc_dist_fig = dyad_distance_rmse |> 
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
                name = 'personality distance (Euclidean)',
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
                axis.text.y = element_text(size = 18),
                axis.text.x = element_text(size = 18),
                legend.position = 'none',
        )
euc_dist_fig

ggsave(
        euc_dist_fig,
        filename = here('results', 'personality_distance_RAW.pdf'),
        # path = FIGURE_PATH,
        device = cairo_pdf,
        # NB: width and height need to be the same for digits in heatmap tiles to be sized right
        width = 6.5,
        height = 7,
)

# Model
summary(lm(
        dyad_mean_rmse ~ condition * big5_euclidean_distance,
        data = dyad_distance_rmse 
))

# RMSE ~ cosine distance
dyad_distance_rmse |> 
        ggplot(aes(x = cosine_distance, y = dyad_mean_rmse, color = condition)) +
        geom_point(
                alpha = 0.5,
                size = 2
        ) +
        geom_smooth(
                method = 'lm',
                # col = 'black'
        ) +
        scale_x_continuous(
                name = 'cosine distance',
        ) +
        scale_y_continuous(
                name = 'dyad mean RMSE',
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.text.y = element_text(size = 18),
                axis.text.x = element_text(size = 18),
                legend.position = 'bottom',
        )

# Model
summary(lm(
        dyad_mean_rmse ~ condition * cosine_distance,
        data = dyad_distance_rmse 
))

# RMSE ~ Correlation distance
dyad_distance_rmse |> 
        ggplot(aes(x = correlation_distance, y = dyad_mean_rmse, color = condition)) +
        geom_point(
                alpha = 0.5,
                size = 2
        ) +
        geom_smooth(
                method = 'lm',
                # col = 'black'
        ) +
        scale_x_continuous(
                name = 'correlation distance',
        ) +
        scale_y_continuous(
                name = 'dyad mean RMSE',
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.text.y = element_text(size = 18),
                axis.text.x = element_text(size = 18),
                legend.position = 'bottom',
        )

# Model
summary(lm(
        dyad_mean_rmse ~ condition * correlation_distance,
        data = dyad_distance_rmse 
))


# > Control for distance between self responses and predictions ----

# Format prediction data to compare to self responses
# Reverse coded items
partner_preds = partner_preds |> 
        mutate(
                reverse_item = ifelse(
                        scale_text %in% reverse_coded_scales,
                        TRUE,
                        FALSE
                ),
                prediction_coded = ifelse(
                        scale_text %in% reverse_coded_scales,
                        100 - response,
                        response
                )
        )
# sanity checks
glimpse(partner_preds)

# Add predictions to self-partner data
self_partner_predictions = self_partner_data |> 
        left_join(
                partner_preds |> 
                        select(
                                groupId,
                                prolificId,
                                scale_id,
                                scale_text,
                                prediction_coded
                        ),
                by = join_by(
                        groupId,
                        prolificId,
                        scale_id,
                        scale_text
                )
        )
# sanity checks
glimpse(self_partner_predictions)


# Summarize predictions in trait space
prediction_trait_summary = self_partner_predictions |> 
        group_by(
                condition, groupId, prolificId, Big5_Dimension
        ) |> 
        summarize(
                mean_trait_prediction = mean(prediction_coded, na.rm = TRUE),
                sd_trait_prediction = sd(prediction_coded, na.rm = TRUE),
                .groups = 'drop'
        ) |> 
        pivot_wider(
                names_from = Big5_Dimension,
                values_from = c(mean_trait_prediction, sd_trait_prediction)
        )
# sanity checks
glimpse(prediction_trait_summary)


# Join self-response trait summary and prediction trait summary
self_resp_prediction_trait_summary = self_trait_summary |> 
        left_join(
                prediction_trait_summary,
                by = join_by(
                        condition,
                        groupId,
                        prolificId
                ),
                # relationship = 'many-to-many'
        )
# sanity checks
glimpse(self_resp_prediction_trait_summary)

# Calculate distances in trait space between self responses and predictions
self_resp_prediction_trait_summary = self_resp_prediction_trait_summary |>
        rowwise() |>
        mutate(
                big5_euclidean_distance_resp_pred = sqrt(
                        (mean_trait_response_Extraversion - mean_trait_prediction_Extraversion)^2 +
                                (mean_trait_response_Agreeableness - mean_trait_prediction_Agreeableness)^2 +
                                (mean_trait_response_Conscientiousness - mean_trait_prediction_Conscientiousness)^2 +
                                (mean_trait_response_Neuroticism - mean_trait_prediction_Neuroticism)^2 +
                                (mean_trait_response_Openness - mean_trait_prediction_Openness)^2
                ),
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
                        use = 'complete.obs'
                ),
                cosine_distance_resp_pred = 1 - ( 
                        sum(
                                c(
                                        mean_trait_response_Extraversion,
                                        mean_trait_response_Agreeableness,
                                        mean_trait_response_Conscientiousness,
                                        mean_trait_response_Neuroticism,
                                        mean_trait_response_Openness
                                ) *
                                        c(
                                                mean_trait_prediction_Extraversion,
                                                mean_trait_prediction_Agreeableness,
                                                mean_trait_prediction_Conscientiousness,
                                                mean_trait_prediction_Neuroticism,
                                                mean_trait_prediction_Openness
                                        )
                        ) /
                                ( 
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
                                        ) *
                                                sqrt(
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
# sanity checks
glimpse(self_resp_prediction_trait_summary)
# Plot these distances -- do they vary? Or did everyone guess their own responses?
# Euc distance
self_resp_prediction_trait_summary |> 
        ggplot(aes(x = big5_euclidean_distance_resp_pred, color = condition, fill = condition)) +
        geom_density(
                alpha = 0.25,
                linewidth = 1
        ) +
        scale_x_continuous(
                name = 'Euclidean distance (self response vs. prediction)',
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        scale_fill_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'bottom',
        )
# Correlation distance
self_resp_prediction_trait_summary |> 
        ggplot(aes(x = correlation_distance_resp_pred, color = condition, fill = condition)) +
        geom_density(
                alpha = 0.25,
                linewidth = 1
        ) +
        scale_x_continuous(
                name = 'Correlation distance (self response vs. prediction)',
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        scale_fill_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'bottom',
        )
# Cosine distance
self_resp_prediction_trait_summary |> 
        ggplot(aes(x = cosine_distance_resp_pred, color = condition, fill = condition)) +
        geom_density(
                alpha = 0.25,
                linewidth = 1
        ) +
        scale_x_continuous(
                name = 'Cos distance (self response vs. prediction)',
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        scale_fill_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        DEFAULT_THEME +
        theme(
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                legend.position = 'bottom',
        )

# Join subject-level RMSE values with distances above
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
                by = join_by(
                        condition, groupId, prolificId
                )
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
                by = join_by(
                        condition, groupId, prolificId
                )
        )
# sanity checks
glimpse(self_partner_rmse_distance)


# Model: RMSE ~ prediction Euc distance + partner Euc distance
# Individuals
summary(lm(
        rmse ~ big5_euclidean_distance_resp_pred + big5_euclidean_distance,
        data = self_partner_rmse_distance
))
# Dyads
summary(lm(
        rmse ~ big5_euclidean_distance_resp_pred + big5_euclidean_distance,
        data = self_partner_rmse_distance |> 
                group_by(condition, groupId) |> 
                summarize(
                        rmse = mean(rmse),
                        big5_euclidean_distance_resp_pred = mean(big5_euclidean_distance_resp_pred),
                        big5_euclidean_distance = mean(big5_euclidean_distance),
                        .groups = 'drop'
                )
))
# Condition 
# Dyads
summary(lm(
        rmse ~ big5_euclidean_distance_resp_pred + condition*big5_euclidean_distance,
        data = self_partner_rmse_distance |> 
                group_by(condition, groupId) |> 
                summarize(
                        rmse = mean(rmse),
                        big5_euclidean_distance_resp_pred = mean(big5_euclidean_distance_resp_pred),
                        big5_euclidean_distance = mean(big5_euclidean_distance),
                        .groups = 'drop'
                )
))

confint(
        lm(
                rmse ~ big5_euclidean_distance_resp_pred + big5_euclidean_distance,
                data = self_partner_rmse_distance |> 
                        group_by(condition, groupId) |> 
                        summarize(
                                rmse = mean(rmse),
                                big5_euclidean_distance_resp_pred = mean(big5_euclidean_distance_resp_pred),
                                big5_euclidean_distance = mean(big5_euclidean_distance),
                                .groups = 'drop'
                        )
        )
)

# Model: RMSE ~ prediction Correlation distance + partner Correlation distance
# Individuals
summary(lm(
        rmse ~ correlation_distance_resp_pred + correlation_distance,
        data = self_partner_rmse_distance
))
# Dyads
summary(lm(
        rmse ~ correlation_distance_resp_pred + correlation_distance,
        data = self_partner_rmse_distance |> 
                group_by(condition, groupId) |> 
                summarize(
                        rmse = mean(rmse),
                        correlation_distance_resp_pred = mean(correlation_distance_resp_pred),
                        correlation_distance = mean(correlation_distance),
                        .groups = 'drop'
                )
))

# Model: RMSE ~ prediction Cosine distance + partner Cosine distance
# Individuals
summary(lm(
        rmse ~ cosine_distance_resp_pred + cosine_distance,
        data = self_partner_rmse_distance
))
# Dyads
summary(lm(
        rmse ~ cosine_distance_resp_pred + cosine_distance,
        data = self_partner_rmse_distance |> 
                group_by(condition, groupId) |> 
                summarize(
                        rmse = mean(rmse),
                        cosine_distance_resp_pred = mean(cosine_distance_resp_pred),
                        cosine_distance = mean(cosine_distance),
                        .groups = 'drop'
                )
))






# ANALYSIS: did people *think* they got to know their partner? ----
# TODO: clean this up

# Get to know partner
self_partner_data |> 
        distinct(condition, prolificId, sliderGetToKnowPartner) |> 
        group_by(condition) |>
        summarize(
                ci_stats = list(Hmisc::smean.cl.boot(sliderGetToKnowPartner)),
                .groups = 'drop'
        ) |>
        tidyr::unnest_wider(ci_stats)

summary(aov(
        sliderGetToKnowPartner ~ condition,
        data = self_partner_data |> 
                distinct(condition, prolificId, sliderGetToKnowPartner)
))

# Partner prediction accuracy
self_partner_data |> 
        distinct(condition, prolificId, sliderPartnerPredictionAccuracy) |> 
        group_by(condition) |>
        summarize(
                ci_stats = list(Hmisc::smean.cl.boot(sliderPartnerPredictionAccuracy)),
                .groups = 'drop'
        ) |>
        tidyr::unnest_wider(ci_stats)

summary(aov(
        sliderPartnerPredictionAccuracy ~ condition,
        data = self_partner_data |> 
                distinct(condition, prolificId, sliderPartnerPredictionAccuracy)
))


# Does thinking you got to know your partner align with prediction error?
subject_rmse = self_partner_data |> 
        group_by(condition, prolificId) |>
        summarize(
                rmse = sqrt(mean(abs_error^2, na.rm = TRUE)),
                .groups = 'drop'
        ) |>
        left_join(
                self_partner_data |> 
                        distinct(prolificId, sliderGetToKnowPartner, sliderPartnerPredictionAccuracy),
                by = 'prolificId'
        )
glimpse(subject_rmse)

# Figure: get to know partner by RMSE
subject_rmse |> 
        ggplot(aes(x = sliderGetToKnowPartner, y = rmse, color = condition)) +
        geom_point(size = 2, alpha = 0.5) +
        geom_smooth(method = "lm", col = "black") +
        scale_x_continuous(
                name = 'got to know my partner',
                breaks = seq(0, 100, by = 25),
                labels = seq(0, 100, by = 25),
                limits = c(0, 100)
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        facet_wrap(~ condition) +
        DEFAULT_THEME +
        theme(
                legend.position = 'none',
                axis.text.y = element_text(size = 18),
                axis.text.x = element_text(size = 18),
        )

# Correlations
cor.test(
        subject_rmse$sliderGetToKnowPartner[subject_rmse$condition == 'baseline'],
        subject_rmse$rmse[subject_rmse$condition == 'baseline']
)
cor.test(
        subject_rmse$sliderGetToKnowPartner[subject_rmse$condition == 'deep'],
        subject_rmse$rmse[subject_rmse$condition == 'deep']
)
cor.test(
        subject_rmse$sliderGetToKnowPartner[subject_rmse$condition == 'small talk'],
        subject_rmse$rmse[subject_rmse$condition == 'small talk']
)


# Figure: accurately predicted partner by RMSE
subject_rmse |> 
        ggplot(aes(x = sliderPartnerPredictionAccuracy, y = rmse, color = condition)) +
        geom_point(size = 2, alpha = 0.5) +
        geom_smooth(method = "lm", col = "black") +
        scale_x_continuous(
                name = 'accurately predicted my partner',
                breaks = seq(0, 100, by = 25),
                labels = seq(0, 100, by = 25),
                limits = c(0, 100)
        ) +
        scale_color_manual(
                name = element_blank(),
                values = QUESTION_CATEGORY_COLORS
        ) +
        facet_wrap(~ condition) +
        DEFAULT_THEME +
        theme(
                legend.position = 'none',
                axis.text.y = element_text(size = 18),
                axis.text.x = element_text(size = 18),
        )

# Correlations
cor.test(
        subject_rmse$sliderPartnerPredictionAccuracy[subject_rmse$condition == 'baseline'],
        subject_rmse$rmse[subject_rmse$condition == 'baseline']
)
cor.test(
        subject_rmse$sliderPartnerPredictionAccuracy[subject_rmse$condition == 'deep'],
        subject_rmse$rmse[subject_rmse$condition == 'deep']
)
cor.test(
        subject_rmse$sliderPartnerPredictionAccuracy[subject_rmse$condition == 'small talk'],
        subject_rmse$rmse[subject_rmse$condition == 'small talk']
)




# TEST: read in data separately and compare/combine ----

priors_data = read_csv(here(DATA_DIR, 'priors', 'processed', 'scale_priors_processed.csv'))
baseline_data = read_csv(here(DATA_DIR, 'conversation', 'processed', 'baseline', 'baseline_full_participant_data_processed.csv'))
baseline_messages = read_csv(here(DATA_DIR, 'conversation', 'processed', 'baseline', 'baseline_full_chat_messages_processed.csv'))
question_prompting_data = read_csv(here(DATA_DIR, 'conversation', 'processed', 'question_prompting', 'question_prompting_full_participant_data_processed.csv'))
question_prompting_messages = read_csv(here(DATA_DIR, 'conversation', 'processed', 'question_prompting', 'question_prompting_full_chat_messages_processed.csv'))


glimpse(priors_data)
glimpse(baseline_data)
glimpse(baseline_messages)
glimpse(question_prompting_data)
glimpse(question_prompting_messages)

# Filter priors df to only include BFI items
priors_processed = priors_data |> 
        filter(scaleCategory == 'personality')
# Sanity check
glimpse(priors_processed)

# Calculate scale-average summary
priors_summary = priors_processed |> 
        group_by(scaleID, scaleText) |>
        summarize(
                meanResponse = mean(priorResponse, na.rm = TRUE),
                n = n(),
                .groups = 'drop'
        )
# Sanity check
glimpse(priors_summary)

# Remove attention checks from baseline and question prompting data frames
baseline_processed = baseline_data |> 
        filter(category != 'ATTENTION CHECK')
# Sanity check
glimpse(baseline_processed)

question_prompting_processed = question_prompting_data |> 
        filter(category != 'ATTENTION CHECK')
# Sanity check
glimpse(question_prompting_processed)



