# traits_from_conversation_cogsci_2026
Public repository for experiment code, data, and analyses featured in CogSci 2026 conference submission

## Contents
- [Overview](#overview)
- [Repo structure](#repo-structure)
- [How to run](#how-to-run)

## Overview
<img width="100%" src="/results/figure1_cogsci.png" style="max-width: 100%;">

When meeting somebody for the first time, how do we figure out what they are like? In the current work, we investigate how people learn about others' personalities through the questions they ask in conversation. Across two studies, participants completed a personality inventory then were paired with an online partner for a ten-minute chat. They were either instructed to get to know their partner in freeform conversation or were provided questions to discuss. The questions were either informative or uninformative for getting to know a stranger. Participants completed the same personality inventory about their partner afterwards. We test whether choosing from informative questions enabled participants to form a more accurate impression of their partner. We find that conversation improved personality predictions in all conditions, but the questions discussed in each condition had minimal effects on accuracy; informative questions may be only as good as the disclosures they elicit.

## Repo structure
```
traits_from_conversation_cogsci_2026/
|-- analysis/
|   |-- baseline_processing.Rmd
|   |-- question_prompting_processing.Rmd
|   |-- priors_study_processing.Rmd
|   |-- data_combining.Rmd
|   |-- analyses.R
|   |-- brms_fits/
|-- data/
|   |-- conversation/
|   |   |-- raw/
|   |   |   |-- baseline/
|   |   |   |-- question_prompting/
|   |   |-- processed/
|   |       |-- baseline/
|   |       |-- question_prompting/
|   |       |-- combined/
|   |-- priors/
|       |-- raw/
|       |-- processed/
|-- experiments/
|-- results/
```

## How to run
Open `.Rproj` in RStudio, then:
1. Run the three processing scripts (any order): `baseline_processing.Rmd`, `question_prompting_processing.Rmd`, `priors_study_processing.Rmd`
2. Run `data_combining.Rmd` to merge everything
3. Run `analyses.R` for models + figures
