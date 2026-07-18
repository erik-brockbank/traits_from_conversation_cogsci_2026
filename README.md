# Asking the right questions? What people learn about strangers in conversation
Public repository for data and analyses presented at CogSci 2026.

The dyadic experiment described in these results was pre-registered on the Open Science Framework (OSF); the work is available as a pre-print on Psyarxiv.
* [Preregistration](https://osf.io/9sjwt/overview)
* [Preprint](https://osf.io/preprints/psyarxiv/rxq5u_v1) (also in the repository, see `cogsci_final.pdf`)


## Overview
<img width="100%" src="/results/figure1_final.png" style="max-width: 100%;">

When meeting somebody for the first time, how do we get to know them? In the current work, we investigate how people learn about others' personalities through the questions they ask in conversation. Across two studies, participants completed a personality inventory then were paired with an online partner for a ten-minute chat. They were either instructed to get to know their partner in freeform conversation or were provided questions to discuss. The questions were either informative or uninformative for getting to know a stranger. Participants completed the same personality inventory about their partner afterwards. We test whether choosing from informative questions enabled participants to form a more accurate impression of their partner. We find that freeform conversation improved personality predictions overall, but differences in the informativeness of the questions discussed had minimal effects on accuracy; deep questions may only be as good as the disclosures they elicit.


## Repo structure
```
traits_from_conversation_cogsci_2026/
|-- analysis/
|   |-- analysis_final.R
|   |-- brms_fits/
|-- data/
|   |-- conversation/
|   |   |-- chat_messages_combined.csv
|   |   |-- predictions_combined.csv
|   |-- priors/
|       |-- scale_priors_processed.csv
|       |-- survey_data_processed.csv
|-- experiments/
|-- results/
```

* [`/analysis`](https://github.com/erik-brockbank/traits_from_conversation_cogsci_2026/tree/main/analysis): folder containing analysis scripts and model outputs
    * [`analysis_final.R`](https://github.com/erik-brockbank/traits_from_conversation_cogsci_2026/blob/main/analysis/analysis_final.R): R script for all analyses and figures reported in the manuscript
    * [`/brms_fits`](https://github.com/erik-brockbank/traits_from_conversation_cogsci_2026/blob/main/analysis/brms_fits): directory for storing BRMS models fit in the analyses
* [`/data`](https://github.com/erik-brockbank/traits_from_conversation_cogsci_2026/tree/main/data): folder for data used in analyses
    * [`/conversation`](https://github.com/erik-brockbank/traits_from_conversation_cogsci_2026/tree/main/data/conversation): folder for data collected during dyadic conversation study
        * [`chat_messages_combined.csv`](https://github.com/erik-brockbank/traits_from_conversation_cogsci_2026/blob/main/data/conversation/chat_messages_combined.csv): CSV with processed message data from all chat dyads, after pre-registered exclusions
        * [`predictions_combined.csv`](https://github.com/erik-brockbank/traits_from_conversation_cogsci_2026/blob/main/data/conversation/predictions_combined.csv): CSV with processed introspection and partner prediction data
    * [`/priors`](https://github.com/erik-brockbank/traits_from_conversation_cogsci_2026/tree/main/data/priors): folder for data from the control study in which participants made personality predictions about a typical person
        * [`scale_priors_processed.csv`](https://github.com/erik-brockbank/traits_from_conversation_cogsci_2026/blob/main/data/priors/scale_priors_processed.csv): CSV with personality prediction data
        * [`survey_data_processed.csv`](https://github.com/erik-brockbank/traits_from_conversation_cogsci_2026/blob/main/data/priors/survey_data_processed.csv): CSV with post-experiment survey responses from the control study
* [`/experiments`](https://github.com/erik-brockbank/traits_from_conversation_cogsci_2026/tree/main/experiments): folder with sanitized experiment code. Note this will not run by itself as-is; requires setup with a firestore back-end and must be build with all dependencies in `package.json`.
* [`/results`](https://github.com/erik-brockbank/traits_from_conversation_cogsci_2026/tree/main/results): folder with PDF outputs for all figures generated in the analysis script (mansucript figures reflect additional visual processing)


## CRediT author statement

* **Erik Brockbank**: Conceptualization, Methodology, Software, Validation, Formal Analysis, Investigation, Data Curation, Writing - Original Draft, Writing - Review & Editing, Visualization, Supervision, Project Administration
* **Nora Dee**: Conceptualization, Methodology, Software, Validation, Formal Analysis, Investigation, Data Curation, Writing - Review & Editing, Visualization
* **Misha O'Keeffe**: Conceptualization, Methodology, Software, Validation, Formal Analysis, Investigation, Data Curation, Writing - Review & Editing, Visualization
* **Wasita Mahaphanit**: Conceptualization, Methodology, Software, Validation, Formal Analysis, Investigation, Resources, Data Curation, Writing - Review & Editing, Supervision
* **Tobias Gerstenberg**: Conceptualization, Writing - Review & Editing, Supervision, Funding Acquisition
* **Judith E. Fan**: Conceptualization, Writing - Review & Editing, Supervision, Funding Acquisition
* **Robert D. Hawkins**: Conceptualization, Methodology, Resources, Writing - Review & Editing, Supervision, Project Administration, Funding Acquisition
