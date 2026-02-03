# traits_from_conversation_cogsci_2026

Public repository for experiment code, data, and analyses featured in CogSci 2026 conference submission

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
