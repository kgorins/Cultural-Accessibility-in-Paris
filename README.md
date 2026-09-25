---

editor_options: 
  markdown: 
    wrap: 72
---

# Cultural Accessibility and Socioeconomic Inequality in Paris

A geospatial data-analysis project exploring how access to cultural facilities varies across Paris and whether these differences are associated with local socioeconomic conditions.

The project combines cultural-facility data from the French Ministry of Culture with the **INSEE Filosofi 200 m population grid** and uses **R** for data cleaning, geospatial analysis, statistics and visualization.

## Key findings

- The analysis includes **3,752 cultural facilities** and **2,164 populated 200 m grid cells**, representing approximately **2.05 million residents**.
- The population-weighted mean distance to the nearest cultural facility is approximately **128 m**.
- Overall cultural accessibility does **not** follow a simple socioeconomic gradient. The middle poverty group has the shortest mean distance to a cultural facility (\~107 m), while both the lowest-poverty (\~158 m) and highest-poverty (\~154 m) groups have longer distances.
- Results vary substantially by cultural category.
- The highest-poverty areas have comparatively long mean distances to **museums (\~1.04 km)** and **heritage locations (\~310 m)**.
- Other categories show different patterns. For example, performing-arts facilities are, on average, closer to residents of the highest-poverty grid cells than to those of the lowest-poverty cells.
- Mean living standard and distance to the nearest cinema have only a **very weak negative correlation (r ≈ -0.077)**.
- A simple model using poverty, an approximate social-housing indicator and local population explains only about **5.4%** of variation in cinema distance.

These results suggest that cultural inequality in Paris cannot be summarized by a single "access to culture" measure: spatial patterns differ considerably between museums, cinemas, books, performing arts, heritage and other categories.

## Research questions

1.  How are cultural facilities distributed across Paris?
2.  How far are residents from the nearest cultural facility?
3.  How does accessibility differ between cultural categories?
4.  Is cultural accessibility associated with local socioeconomic conditions?

## Methodology

Cultural facilities are cleaned, geocoded and grouped into broader analytical categories.

The INSEE Filosofi 200 m population grid provides fine-scale population and socioeconomic information.

For every populated grid cell, accessibility is measured using:

- distance to the nearest cultural facility;
- number of facilities within 500 m;
- number of facilities within 1 km;
- category-specific nearest-facility distances.

Accessibility statistics are population-weighted when comparing socioeconomic groups.

The statistical analysis includes descriptive comparisons, Pearson correlation and exploratory linear regression.

## Cultural categories

The analysis distinguishes between:

- Heritage
- Books & reading
- Performing arts
- Cinema
- Museum
- Parks & gardens
- Arts & creation
- Education & archives
- Press & stationery

## Example output

![Distance to nearest cultural facility](output/figures/distance_to_nearest_culture.png)

The full analysis, maps, statistical results and methodological limitations are available in the [**Quarto report**](report/report.html).

## Repository structure

``` text
paris-cultural-access/
├── R/
│   ├── 01_explore_cultural_data.R
│   ├── 02_clean_cultural_data.R
│   ├── 03_prepare_geography.R
│   ├── 04_prepare_population.R
│   ├── 05_calculate_accessibility.R
│   ├── 06_category_accessibility.R
│   └── 07_inequality_analysis.R
│
├── data/
│   ├── raw/
│   └── processed/
│
├── output/
│   ├── figures/
│   └── tables/
│
├── report/
│   ├── report.qmd
│   └── report.html
│
└── README.md
```

## Reproducibility

The analysis is organized as a sequence of R scripts:

1.  explore the cultural dataset;
2.  clean and classify cultural facilities;
3.  prepare geographic data;
4.  prepare the INSEE population grid;
5.  calculate general cultural accessibility;
6.  calculate category-specific accessibility;
7.  analyze socioeconomic inequalities.

Large raw and processed datasets are not stored in the repository.

The final Quarto report can be rendered after the processing scripts have been run:

``` bash
quarto render report/report.qmd
```

## Tools

- **R**
- **tidyverse**
- **sf**
- **ggplot2**
- **Quarto**
- geospatial data processing
- statistical analysis and visualization

## Data sources

- French Ministry of Culture — Basilic cultural-facility database
- INSEE — Filosofi 200 m grid
- Paris administrative boundaries

## Limitations

Accessibility is based on straight-line distance rather than walking or public-transport travel time.

The analysis measures spatial proximity to cultural infrastructure, not actual cultural participation. It does not account for admission prices, opening hours, capacity or individual preferences.

Socioeconomic indicators are area-level measures, and the statistical analysis does not currently model spatial autocorrelation.

Facilities outside the administrative boundary of Paris are also excluded, which may underestimate accessibility for residents near the city boundary.

## Possible extensions

Future versions could include:

- walking-network accessibility;
- facilities in neighbouring communes;
- public versus commercial cultural facilities;
- population-weighted socioeconomic quantiles;
- spatial autocorrelation analysis;
- spatial regression models.
