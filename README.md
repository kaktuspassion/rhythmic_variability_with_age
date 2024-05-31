# Rhythmic Variability Between Young and Old Age Groups

## Project Overview

This study examines the rhythmic differences in speech between young and old adult speakers using a set of carefully selected rhythmic and intensity metrics. By employing Decision Tree and Random Forest models, the project seeks to assess the impact of aging on speech rhythm and the efficacy of these metrics in classifying age groups.

## Objectives

- To evaluate whether selected rhythmic metrics can effectively differentiate between young and old adult speakers.
- To determine the feature importance of each metric using Random Forest analysis.

## Dataset

The dataset includes recordings of *60 utterances* each from 10 older adults (ages 66-81) and 16 younger adults (ages 18-32), all native Zurich German speakers. A total of 1560 utterances were analyzed.

## Metrics Used

- **Durational Metrics:** %V (percentage of vocalization), deltaV (durational variability of vowels), deltaC (durational variability of consonants), rPVI_C (raw Pairwise Variability Index for consonants).
- **Intensity Metrics:** VarcoP (variability of peak intensity), nPVIp (normalized Pairwise Variability Index for syllable intensity peaks), stdevP (standard deviation of peak syllable intensity).

## Methods

1. **Corpus Annotation:** Utilizing Praat’s tools, syllables and intensity peaks were annotated for all speech samples.
2. **Feature Extraction:** Metrics such as %V, deltaV, deltaC, rPVI_C, VarcoP, nPVIp, and stdevP were computed from the annotated data. 

3. **Computational Modeling:** Data was partitioned into training (80%) and testing (20%) sets. Models including Decision Trees and Random Forests were developed and optimized in R.

## Results

- The Random Forest model showed the best performance, achieving an accuracy of 73.31% and an AUC value of 0.829.
- Feature importance analysis revealed *deltaV and %V* as significant predictors in age classification.

## Conclusion

This research supports the hypothesis that specific rhythmic and intensity metrics can distinguish between age groups in speech, with implications for understanding aging effects on speech production.

