# Visualizing Urban Travel Behaviour in Victoria

This project is an interactive R Shiny dashboard that explores **urban travel patterns in Victoria (VIC), Australia** using the Victorian Integrated Survey of Travel and Activity (VISTA, 2012–2020). It focuses on how **household characteristics**—such as income, household size, and vehicle ownership—relate to **choice of travel mode** across different times of day and days of the week. :contentReference[oaicite:0]{index=0}  

The app is designed primarily for **urban planners, policymakers, and transport researchers**, but is also suitable for anyone interested in how socio-economic factors influence travel behaviour.

---

## Features

### 1. Overview Tab

**Purpose:** High-level view of travel mode usage across time periods and years.

- **Stacked bar chart** of travel mode by:
  - Time of day (e.g. 6–9am, 3–6pm)
  - Weekday vs weekend
  - Year (2012–2020)
- **Interactive controls:**
  - Travel mode selection (e.g. vehicle, bike, walk, public transport)
  - Income group filter
  - Year slider with **animation button** to play changes over time
  - Tooltip on hover showing counts (log-transformed) and proportions
- Color palette is **colour-blind friendly**, using distinct hues for each travel mode.

This tab helps answer questions like:

> “Do people choose different ways to travel depending on when they travel and their income group?”

---

### 2. Household Tab

**Purpose:** Explore how household characteristics influence travel mode choices.

This tab contains **two visualisations**, switchable with a radio button:

#### a. Ridge Plot

- Shows distributions of:
  - Household income
  - Household size
  - Number of vehicles
- Faceted by or mapped to **travel mode**.
- **Interactive controls:**
  - Select travel mode
  - Select household size groups (checkboxes)
  - Year slider with optional animation

This view highlights how different kinds of households tend to choose different travel modes. :contentReference[oaicite:1]{index=1}  

#### b. Sankey Diagram

- Nodes: income groups, household size groups, vehicle ownership groups, travel modes.
- Links represent **flows** between categories.
- **Interactive controls:**
  - Variable selection to control which groups are included in the flows.
- Tooltips on hover show:
  - Source and target
  - Flow counts (incoming / outgoing)

This diagram emphasises how household characteristics “flow” into particular travel modes.

---

### 3. About Tab

- Brief description of:
  - The **dataset** and its source (VISTA).
  - Purpose of the dashboard.
  - Main features and interactions.
- Includes **step-by-step instructions** so first-time users can quickly learn how to interact with the dashboard. :contentReference[oaicite:2]{index=2}  

---

## Data

- **Source:** Victorian Integrated Survey of Travel and Activity (VISTA) – Victorian Government Data Directory. :contentReference[oaicite:3]{index=3}  
- **Years used:** 2012–2020.
- **Key variables (after wrangling and cleaning):**
  - Household income (including derived **income groups** from clustering)
  - Household size
  - Number of vehicles owned
  - Travel mode
  - Time of day of travel
  - Weekday vs weekend

Data wrangling (including K-means clustering for income groups) was performed in a separate data exploration and processing step before building the Shiny app. :contentReference[oaicite:4]{index=4}  

---

## Technology Stack

- **Language:** R
- **Framework:** [Shiny](https://cran.r-project.org/package=shiny)
- **Plotting:**
  - [ggplot2](https://ggplot2.tidyverse.org) – base plots
  - [ggridges](https://cran.r-project.org/package=ggridges) – ridge plots
  - [plotly](https://plotly-r.com) – interactive plots and Sankey diagram
- Additional layout and interaction patterns adapted from examples in the Shiny Gallery and related resources. :contentReference[oaicite:5]{index=5}  

---

## Getting Started

### 1. Prerequisites

Make sure you have:

- R (version ≥ 4.x recommended)
- RStudio (optional but convenient)
- The following R packages installed:

```r
install.packages(c(
  "shiny",
  "ggplot2",
  "plotly",
  "ggridges",
  "dplyr",
  "tidyr",
  "readr"
))
```