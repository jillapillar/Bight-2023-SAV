The Bight 2023 SAV element is part of the Southern California Bight Regional Monitoring Program. 
Eelgrass structural data was collected 2022-2024 from embayments across the Southern California Bight as an application of the Zostera Ecosystem Function Reporter (ZEFR) index. 

Two driving questions of this study were: (1) what is the regional condition of eelgrass, and (2) are there differences across strata (small embayments, large embayments, estuaries)?

## Project structure

```text
data/      # raw, intermediate, and final datasets
scripts/   # R scripts
figures/   # figures found in Technical Report XXXX
output/    # tables and data summaries
docs/      # various SOPs, draft versdions, field workplan, and technical reports for the ZEFR index (Gillett et al. 2026)
```

## Requirements

- R (version 4.5.2)
- required packages (spelled out in R script)
    - spsurvey
    - sf
    - ggplot2
    - dplyr
    - stringr
    - vegan
 
# How to run the index with your own data

 1. Clone the repository
 2. Open the B23_eelgrass.R script
 3. Copy data structure found in the data/raw/ files (field, lab, sample draw, station metadata, bed perimenter data)
 4. Wrangle, go through QAQC and use established strucutral metric breaks to score data (treat new data as the 'eelgrass.targeted' dataset)
 5. Generate outputs & figures

# Authors
Dr. Jill Tupitza, Dr. David Gillett, Dr. Jan Walker, Adriana Le Compte Santiago
