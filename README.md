# Winter Travel: Modeling Backcountry Recreation Patterns

*A statistical analysis of winter user behavior across Colorado trail systems, examining how environmental conditions and avalanche danger shape visitation patterns across different user types.*

## Overview

This project analyzes trail camera data from eight backcountry trailheads in the Gunnison Valley, Colorado, to understand how weather conditions, avalanche danger ratings, and day-of-week effects drive visitation patterns across different user types (skiers, snowmobilers, mechanized, hybrid). The analysis uses negative binomial generalized linear models with AIC-selected interaction terms to reveal strong behavioral segmentation and risk-aversion patterns.

**Conducted for**: US Forest Service, Colorado Public Lands Data Collection Initiative  
**Analysis period**: 2017–2021 (four winter seasons)  
**Data points**: 1,000+ observation days across 8 trailheads  
**Statistical approach**: Negative binomial GLM with spatial/modal stratification

---

## Key Findings

### 1. Weather Effects on Visitation
- **New snow**: 2.7% decrease in users per cm (recent snowfall discourages use, likely due to difficult conditions)
- **Temperature**: 0.6% increase per °C (warmer drives recreation)
- **Historical snow**: Lagged effects are minimal; cumulative 3–5 day snow has limited predictive power beyond current conditions

### 2. Avalanche Danger Response
- **High danger (rating 4)**: Clear, significant decrease in overall visitation
- **Heterogeneous response by location**: Some trailheads (Kebler, Washington Gulch) show strong risk-aversion; others (Slate River Rd) more responsive at moderate danger levels
- **User type differences**: Motorized users less sensitive to danger at some sites; non-motorized and hybrid users show stronger avoidance

### 3. Behavioral Segmentation
- **Dominant modal preference**: Motorized users concentrate at Kebler Rd (70%+ of motorized trips); non-motorized prefer Snodgrass
- **Weekend effect**: 13% increase Saturday vs. Sunday; weekdays ~50% of weekend volume
- **Seasonal patterns**: 
  - Year-end spike (holidays)
  - Gradual increase January–February
  - Sharp mid-March peak (motorized > non-motorized)
  - Motorized variance exceeds non-motorized (more weather-dependent)

### 4. Year-to-Year Trends & COVID Impact
- Overall usage stable 2017–2020; **non-motorized surge in 2020–2021** (ski area closure → backcountry alternative)
- **COVID effect**: When ski resorts closed and people worked remotely, backcountry non-motorized use jumped significantly (ski area alternative + temporal availability)
- Motorized users unaffected or slightly down (less affected by ski closure)
- Motorized users increasingly concentrating at Kebler (structural trend independent of COVID)

---

## Methodological Contributions

### Observational Study Design
This analysis demonstrates key principles for causal inference in observational data:
- **Stratification**: Eight separate models fit by location to allow heterogeneous avalanche effects
- **Pooled analyses**: Aggregate models borrow strength across sites to detect main weather effects
- **Confounder control**: Day-of-week and temporal aggregation (lagged snow) address within-season confounding

### Count Data Modeling
- **Overdispersion**: Trail camera data contains ~35% zeros and substantial right skew
- **Model selection**: Poisson GLM rejected via AIC comparison; negative binomial chosen for robustness
- **Interaction terms**: AIC-based model selection identified modality × zone and modality × weekend interactions as significant

### Spatial Structure in Observational Data
- Trailheads share common environmental drivers (regional weather, avalanche danger)
- Pooled model estimates general relationships; stratified models capture site-specific responses
- Analogous to clustered surgical outcomes: institutions/surgeons share case-mix, but response varies by provider type

---

## Repository Structure

```
.
├── README.md                           # This file
├── 00_READ_ME_FIRST.md                 # Quick orientation (start here)
├── PORTFOLIO_REVIEW.md                 # Detailed project review and cleanup guidance
├── NEXT_STEPS.md                       # Prioritized cleanup checklist
├── INTERVIEW_SCRIPT.md                 # Interview preparation talking points
├── COVID_AND_DATA_UPDATE.md            # COVID findings & data scope decision
├── Winter_Travel.Rproj                 # RStudio project file
├── winter_travel.r                     # Main analysis script (data prep, models, diagnostics)
├── avyweather.r                        # Data loading helper (CAIC and SNOTEL data)
│
├── winter_travel.rmd                   # Exploratory analysis with 10 research questions
├── winter_travel_report.rmd            # Main report with model results and figures
├── winter_travel_slides.rmd            # Presentation format
│
├── winter_travel.html                  # Rendered exploratory analysis
├── winter_travel_report.pdf            # Rendered report (print-ready)
├── winter_travel_slides.html           # Rendered slides
│
├── Data_for_R/                         # Raw data by season and location
│   ├── 2017-2018/  (8 trailheads × season)
│   ├── 2018-2019/
│   ├── 2019-2020/
│   ├── 2020-2021/
│   ├── 23-24_Data/                    # 2023-2024 trail camera data (not yet incorporated)
│   ├── CAIC_Data_2017-2020/           # Colorado Avalanche Information Center
│   ├── SNOTEL_2017-2020/              # Butte SNOTEL station 380
│   └── Hybrid_recount/                # QA/recount data
│
├── data/                               # Processed data (output from winter_travel.r)
│   ├── winter_travel.csv               # Full dataset (trailhead × day × modality)
│   ├── all_users.csv                   # Aggregated by day (all trailheads, all modalities)
│   ├── all_modes.csv                   # Aggregated by trailhead (all modalities)
│   ├── all_locations.csv               # Aggregated by modality (all trailheads)
│   └── by_zone.csv                     # Aggregated by zone (East River, Brush/Cement, etc.)
│
└── CPL DCI R Analysis - Zachary Treisman.pdf  # Earlier summary report
```

---

## Data Sources

### 1. Trail Camera Counts
- **Cameras**: 8 trailheads, operated November–May each winter
- **Locations**: 
  - Brush Creek Trailhead, Brush Creek Rd
  - Cement Creek
  - Gothic Rd, Snodgrass (grouped as "East River" zone)
  - Kebler Rd
  - Slate River Rd
  - Washington Gulch
- **Variables**: Daily counts by user modality (non-motorized, mechanized, hybrid, motorized)
- **Data cleaning**: Manual inspection; resolved data entry errors, standardized naming, handled missing dates

### 2. CAIC Avalanche Danger Ratings
- Source: Colorado Avalanche Information Center (published daily)
- Variables: `rating_above`, `rating_near`, `rating_below` (above treeline, near treeline, below treeline)
- Ratings: 1=Low, 2=Moderate, 3=Considerable, 4=High
- Used: `rating_near` (most relevant for backcountry travel)

### 3. SNOTEL Weather & Snow Data
- Source: Butte SNOTEL station 380 (representative of study area)
- Variables: 
  - `snow_depth` (cm)
  - `change_depth` (daily new snow / compaction / melt)
  - `air_temp` (°F)
  - `snow_density`
  - Lagged aggregates: `lag1snow`–`lag4snow`, `past2snow`–`past5snow`

---

## How to Use This Repository

### Run the Full Analysis
```R
# 1. Load dependencies
library(tidyverse)
library(MASS)           # glm.nb
library(mgcv)           # gam
library(glmmTMB)        # AR(1) models
library(pscl)           # Alternative NB specs
library(glarma)         # GLARMA models

# 2. Source data prep and initial models
source("winter_travel.r")

# 3. Render reports
rmarkdown::render("winter_travel_report.rmd")
rmarkdown::render("winter_travel_slides.rmd")
```

### Key Objects Created by `winter_travel.r`
After running the script, R workspace contains:
- **Main dataset**: `winter_travel` (full trailhead × day × modality data)
- **Aggregates**: `all_users`, `all_modes`, `all_locations`, `by_zone`
- **Models**: 
  - `glm0nb`, `glm1nb` (pooled NB GLMs)
  - `glm2*` (per-trailhead avalanche models)
  - `glm3` (weather interaction model)
  - `glm5`, `glm5a` (weekend effects)
  - `gam6` (seasonal GAM)

### Generate Report
```R
# HTML output (interactive, for sharing)
rmarkdown::render("winter_travel_report.rmd", output_format = "html_document")

# PDF output (for printing, presentations)
rmarkdown::render("winter_travel_report.rmd", output_format = "pdf_document")
```

---

## Statistical Methods

### Negative Binomial GLM

**Why NB over Poisson?**
- Poisson assumes mean = variance; overdispersion ratio ~3 detected
- NB allows flexible variance: Var(Y) = μ + μ²/θ
- 35% of observations are zero counts (too many for Poisson)

**Specification**:
log(E[user.count_i]) = β₀ + β_snow × change_depth_i + β_lag × lag3snow_i + β_temp × air_temp_i + β_weekend × weekend_i

**Link function**: Log (canonical for NB)  
**Overdispersion parameter**: Estimated via maximum likelihood

### Model Selection
- **Criterion**: Akaike Information Criterion (AIC)
- **Approach**: Incremental model building; lower AIC preferred
- **Interaction selection**: `modality × zone` and `modality × weekend` retained based on AIC improvement

### Temporal Autocorrelation (Partial Implementation)
- **Issue**: Trail use shows temporal clustering (consecutive days similar)
- **Explored**: GLARMA (generalized linear ARMA), AR(1) via glmmTMB
- **Status**: Implemented but not fully integrated into final report (see PORTFOLIO_REVIEW.md)

---

## Variables & Data Dictionary

| Variable | Type | Range | Definition |
|----------|------|-------|-----------|
| `date` | Date | 2017-12-21 to 2021-04-30 | Observation date |
| `Trailhead` | Factor | 8 levels | Location of trail camera |
| `Zone` | Factor | 5 levels | Aggregated trailhead zone |
| `year` | Factor | w1718–w2021 | Winter season (Nov–May) |
| `modality` | Factor | 4 levels | Non.motorized, Mechanized, Hybrid, Motorized |
| `user.count` | Integer | 0–500 | Daily count of users at trailhead |
| `weekend` | Factor | weekday, weekend | Day-of-week indicator |
| `has_sled` | Factor | sled, no_sled | Motorized status (Hybrid/Motorized = sled) |
| `rating_near` | Factor | 1–4 | CAIC danger rating (near treeline) |
| `rating_above`, `rating_below` | Factor | 1–4 | CAIC ratings (above/below treeline) |
| `change_depth` | Numeric | -15 to +25 cm | New snow (positive) or melt/compaction (negative) |
| `snow_depth` | Numeric | 0–200 cm | Total snow on ground |
| `air_temp` | Numeric | -30 to +5 °F | Average daily air temperature |
| `snow_density` | Numeric | 0.1–0.5 | Snow density (g/cm³) |
| `lag1snow`–`lag4snow` | Numeric | —cm | Prior 1–4 days' snow change |
| `past2snow`–`past5snow` | Numeric | —cm | Cumulative snow change (current + prior days) |

---

## Key Results by Research Question

1. **Snowfall correlation**: Recent snow inversely correlated with visitation (–2.7% per cm); lagged snow (3–5 days prior) minimal effect
2. **Avalanche impact**: High danger (rating 4) significantly reduces use; effect heterogeneous by location and modality
3. **Weather combinations**: Warmer temps (coef: +0.6% per °C) + less recent snow = peak visitation
4. **Weekend effect**: Saturdays ~13% busier than Sundays; weekdays ~50% of weekend
5. **Seasonal trends**: Stable year-to-year; uptick in non-motorized 2020–2021; motorized consolidating at Kebler
6. **COVID impact**: Strong effect on non-motorized users: when ski resorts closed (2020), backcountry use surged (~40% increase in non-motorized visitation); motorized use unchanged (unaffected by ski closure)

---

## Limitations & Future Directions

### Known Limitations
1. **Trail cameras**: Count users, not visits (groups counted as one unit; individuals not tracked)
2. **Temporal autocorrelation**: Not fully addressed in final model (AR/GLARMA implemented but not reported)
3. **Spatial clustering**: Eight locations modeled separately or pooled; spatial correlation not explicit
4. **Confounding**: Weather and avalanche danger correlated; cannot isolate causal effects
5. **Generalization**: Colorado-specific; may not transfer to other regions
6. **Data scope**: Analysis focuses on 2017–2021; more recent data (2023-2024) available for future extension

### Extensions
- **Spatial autocorrelation**: Explicit spatial covariance model (CAR/ICAR)
- **Causal inference**: Instrumental variable or regression discontinuity approaches (e.g., policy changes)
- **Cross-validation**: Leave-one-location-out validation to assess spatial generalizability
- **Machine learning**: Image classification for automated trail camera analysis (mentioned but not implemented)
- **Visitor heterogeneity**: Survey data linking users to specific modality/timing choices
- **Recent data**: Incorporate 2021-2024 trail camera data with updated environmental covariates

---

## Citation & Attribution

**Project**: Colorado Public Lands Data Collection Initiative  
**Data custodian**: US Forest Service  
**Analysis**: Zachary Treisman  
**Date**: 2017–2021  
**License**: [Specify: MIT, CC0, etc. if public]

If using this analysis in publications, please cite:
> Treisman, Z. (2021). Winter travel patterns in the Gunnison Valley: A statistical analysis of backcountry recreation behavior. Unpublished analysis.

---

## Contact & Questions

For questions about this analysis:
- **Statistical methodology**: See winter_travel_report.rmd (methods section)
- **Data access**: Contact US Forest Service Colorado Public Lands initiative
- **Code reproduction**: Run `source("winter_travel.r")` after installing dependencies listed above

---

## Interview Framing (For SDSC)

### Key Methodological Contributions (for Margeaux)

This project demonstrates three methodological insights that transfer directly to surgical outcomes research:

1. **Spatial Autocorrelation in Observational Data**
   - *Problem*: Trail use across 8 locations shares regional confounders (weather, avalanche danger)
   - *Solution*: Pooled model for strength + stratified model for heterogeneity
   - *Surgical analogy*: Procedures from same surgeon/institution cluster on outcome; same pooled + stratified approach applies

2. **Count Data with Overdispersion**
   - *Problem*: User counts are zero-inflated, right-skewed; Poisson inadequate
   - *Solution*: Negative binomial GLM with AIC model selection
   - *Surgical analogy*: Complication counts per surgeon similarly non-Gaussian; NB is standard for provider quality measurement

3. **Behavioral Heterogeneity via Interaction Terms**
   - *Problem*: Users aren't homogeneous; skiers ≠ snowmobilers in risk response
   - *Solution*: Interaction terms (modality × location, modality × weekend) reveal segmentation
   - *Surgical analogy*: Surgeon type (generalist vs. specialist) and case complexity interact on complication risk; same interaction framework

---

*Last updated: May 2026*
