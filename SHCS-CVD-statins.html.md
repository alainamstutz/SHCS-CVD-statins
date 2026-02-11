---
title: "NUDGE cluster randomized trial"
author: "A.Amstutz"
format:
  html:
    toc: true
    toc-float: true
    toc-depth: 4
    code-fold: true
    keep-md: true
  pdf:
    toc: true
editor: visual
---

# NUDGE SHCS precision CVD/statin CRT

Nudging intervention on the level of SHCS physicians at SHCS sites to promote statin prescription:

-   **Control**: Usual Care

-   **Intervention**: Nudge shared-decision making based on improved CVD risk score

## **Parameters and design considerations**

-   Two-arm parallel-group, 1:1 allocation, superiority, cluster-randomized

-   Cluster eligibility:

    -   SHCS physicians

    -   From xxx sites

-   Individual eligibility:

    -   XXX

-   Primary outcome (binary): “being prescribed a statin at first patient-encounter”

-   Unit of data collection with be the SHCS cohort participants, but level of inference will be the physicians, i.e. cluster-average

-   Baseline primary outcome rate, see calculation below:

    -   ca. 20%

-   Expected delta:

    -   10 pp ?

-   Cluster size (m) of eligible overall participants, see calculation below:

    -   on average 20 eligible participants per physician-cluster

-   CV (coefficient of variation), see calculation below:

    -   0.90

-   ICC for the primary outcome, see calculation below:

    -   0.10

-   Max. ca. ??? eligible clusters, i.e. ??? clusters per arm

-   Min. desired power 80%, two-sided alpha of 0.05

-   1:1 allocation

**Packages & seed**


::: {.cell}

```{.r .cell-code}
req_pkgs <- c("pwr",
              "dplyr",
              "tidyr",
              "purrr",
              "ggplot2",
              "here",
              "lme4",
              "performance",
              "gt"
)
install_if_missing <- function(pkgs){
  for(p in pkgs){
    if(!requireNamespace(p, quietly=TRUE)){
      install.packages(p, repos="https://cloud.r-project.org")
    }
    library(p, character.only=TRUE)
  }
}
install_if_missing(req_pkgs)

# set global RNG seed for reproducibility
set.seed(20250809)
```
:::


**Prep Data SHCS**


::: {.cell}

```{.r .cell-code}
df_strict <- readRDS(here("01-strict_selection.rds"))
df_liberal <- readRDS(here("01-liberal_selection.rds"))

# Remove any existing grouping
df_strict <- df_strict |> 
  ungroup()
df_liberal <- df_liberal |> 
  ungroup()
```
:::


## **Calculation CV**


::: {.cell}

```{.r .cell-code}
# # Keep only physicians who have at least seen 10 pat
# df_strict <- df_strict |>
#   filter(n_elig_study >= 10)

# Calculate CV for cluster sizes (=physicians)
# CV = sd(n_elig_study) / mean(n_elig_study)
cv_strict <- df_strict |>
  summarise(
    n_clusters = n(),
    mean_cluster_size = mean(n_elig_study),
    sd_cluster_size = sd(n_elig_study),
    cv = sd_cluster_size / mean_cluster_size
  )

cv_strict |>
  gt() |>
  cols_label(
    n_clusters = "Number of Clusters",
    mean_cluster_size = "Mean Cluster Size",
    sd_cluster_size = "SD Cluster Size",
    cv = "CV"
  ) |>
  fmt_number(
    columns = c(mean_cluster_size, sd_cluster_size, cv),
    decimals = 2
  # ) |>
  # tab_header(
  #   title = "Coefficient of Variation"
  ) |>
  tab_options(
    table.font.size = 14
  )
```

::: {.cell-output-display}

```{=html}
<div id="lxdcnzwllu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lxdcnzwllu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#lxdcnzwllu thead, #lxdcnzwllu tbody, #lxdcnzwllu tfoot, #lxdcnzwllu tr, #lxdcnzwllu td, #lxdcnzwllu th {
  border-style: none;
}

#lxdcnzwllu p {
  margin: 0;
  padding: 0;
}

#lxdcnzwllu .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 14px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#lxdcnzwllu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#lxdcnzwllu .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lxdcnzwllu .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lxdcnzwllu .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lxdcnzwllu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lxdcnzwllu .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lxdcnzwllu .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#lxdcnzwllu .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#lxdcnzwllu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lxdcnzwllu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lxdcnzwllu .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#lxdcnzwllu .gt_spanner_row {
  border-bottom-style: hidden;
}

#lxdcnzwllu .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#lxdcnzwllu .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#lxdcnzwllu .gt_from_md > :first-child {
  margin-top: 0;
}

#lxdcnzwllu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lxdcnzwllu .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#lxdcnzwllu .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#lxdcnzwllu .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#lxdcnzwllu .gt_row_group_first td {
  border-top-width: 2px;
}

#lxdcnzwllu .gt_row_group_first th {
  border-top-width: 2px;
}

#lxdcnzwllu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lxdcnzwllu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lxdcnzwllu .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lxdcnzwllu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lxdcnzwllu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lxdcnzwllu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lxdcnzwllu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#lxdcnzwllu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lxdcnzwllu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lxdcnzwllu .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lxdcnzwllu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lxdcnzwllu .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lxdcnzwllu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lxdcnzwllu .gt_left {
  text-align: left;
}

#lxdcnzwllu .gt_center {
  text-align: center;
}

#lxdcnzwllu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lxdcnzwllu .gt_font_normal {
  font-weight: normal;
}

#lxdcnzwllu .gt_font_bold {
  font-weight: bold;
}

#lxdcnzwllu .gt_font_italic {
  font-style: italic;
}

#lxdcnzwllu .gt_super {
  font-size: 65%;
}

#lxdcnzwllu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#lxdcnzwllu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lxdcnzwllu .gt_indent_1 {
  text-indent: 5px;
}

#lxdcnzwllu .gt_indent_2 {
  text-indent: 10px;
}

#lxdcnzwllu .gt_indent_3 {
  text-indent: 15px;
}

#lxdcnzwllu .gt_indent_4 {
  text-indent: 20px;
}

#lxdcnzwllu .gt_indent_5 {
  text-indent: 25px;
}

#lxdcnzwllu .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#lxdcnzwllu div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n_clusters">Number of Clusters</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean_cluster_size">Mean Cluster Size</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sd_cluster_size">SD Cluster Size</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="cv">CV</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="n_clusters" class="gt_row gt_right">229</td>
<td headers="mean_cluster_size" class="gt_row gt_right">21.60</td>
<td headers="sd_cluster_size" class="gt_row gt_right">29.15</td>
<td headers="cv" class="gt_row gt_right">1.35</td></tr>
  </tbody>
  
  
</table>
</div>
```

:::
:::


## **Calculation baseline rate and ICC**


::: {.cell}

```{.r .cell-code}
# Calculate number of eligible patients who received statins
df_strict <- df_strict |>
  mutate(n_elig_received_statin = round(statin_presc_rate * n_elig_study))

# Reconstruct individual patient data (only eligible patients)
individual_data <- df_strict |>
  rowwise() |>
  mutate(
    patient_data = list(
      tibble(
        statin_prescribed = c(
          rep(1, n_elig_received_statin),
          rep(0, n_elig_study - n_elig_received_statin)
        )
      )
    )
  ) |>
  ungroup() |>
  unnest(patient_data) |>
  select(center, physician, statin_prescribed)

# Verify
ind_data <- individual_data |>
  group_by(physician) |>
  summarise(
    n_total = n(),
    n_received_statin = sum(statin_prescribed),
    rate = mean(statin_prescribed)
  )

# baseline rate
baseline_rate <- individual_data |>
  summarise(
    baseline_rate = mean(statin_prescribed)
  )

baseline_rate
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 1 × 1
  baseline_rate
          <dbl>
1         0.218
```


:::

```{.r .cell-code}
# Calculate ICC
model <- glmer(statin_prescribed ~ 1 + (1 | physician), 
               data = individual_data, 
               family = binomial)

icc(model)
```

::: {.cell-output .cell-output-stdout}

```
# Intraclass Correlation Coefficient

    Adjusted ICC: 0.058
  Unadjusted ICC: 0.058
```


:::
:::


## **Corresponding individual randomized trial**

Sample size for the individual randomized trial on the same question


::: {.cell}

```{.r .cell-code}
# Parameters
p_C <- 0.20
p_I <- 0.30
power <- 0.80 
ICC <- 0.10
alpha <- 0.05

# Effect size, standardized as Cohen's h
h_I_C <- ES.h(p1 = p_I, p2 = p_C)
cat("Cohen's h for Intervention vs Control:", round(h_I_C, 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
Cohen's h for Intervention vs Control: 0.232 
```


:::

```{.r .cell-code}
# Sample size pair-wise comparison
ss_I_C <- pwr.2p.test(h = h_I_C, sig.level = alpha, power = power)
n_per_arm <- ceiling(ss_I_C$n)
n_total <- n_per_arm * 2

cat("Sample size per arm:", n_per_arm, "\n")
```

::: {.cell-output .cell-output-stdout}

```
Sample size per arm: 292 
```


:::

```{.r .cell-code}
cat("Total trial sample size (2-arm trial):", n_total)
```

::: {.cell-output .cell-output-stdout}

```
Total trial sample size (2-arm trial): 584
```


:::
:::


# **(1) Sample size calculation CRT: formula-based**

Add the design effect (DEFF) to the individual RCT sample size. The usual standard DEFF formula:

DEFF = 1+(m−1)ICC , whereby m = cluster size

However, let's not forget the cluster size variation. The usual conservative adjustment of the DEFF with cluster size variation is (e.g. see here: [https://pmc.ncbi.nlm.nih.gov/articles/PMC7394950/#sup1](#0)):

DEFF_cv = 1+((m(1+CV\^2)−1))ICC , whereby CV is the coefficient of variation (ratio of standard deviation of cluster sizes to mean of cluster sizes)


::: {.cell}

```{.r .cell-code}
# Parameters
p_C <- 0.20
p_I <- 0.30  
power <- 0.80 
ICC <- 0.10
alpha <- 0.05

m <- 20 # average cluster size
CV <- 0.90 # CV

deff <- 1+(m-1)*ICC # standard DEFF
deff_cv <- 1+((m*(1+CV^2))-1)*ICC # DEFF with cluster size variation

# Effect size
h_I_C <- ES.h(p1 = p_I, p2 = p_C)

# sample size for corresponding individual RCT 
ss <- pwr.2p.test(h = h_I_C, power = power, sig.level = alpha)$n

# CRT sample size
ss_crt <- ceiling(ss * deff_cv)
n_clusters <- ceiling(ss_crt / m)
cat("Cluster sample size one arm:", n_clusters, "\n")
```

::: {.cell-output .cell-output-stdout}

```
Cluster sample size one arm: 66 
```


:::

```{.r .cell-code}
# cat("Individual sample size one arm:", ss_crt, "\n")

# Total
tot_clusters <- n_clusters * 2
tot_ind <- ss_crt * 2
cat("Total cluster sample size:", tot_clusters, "\n")
```

::: {.cell-output .cell-output-stdout}

```
Total cluster sample size: 132 
```


:::

```{.r .cell-code}
cat("Total individual sample size:", tot_ind, "\n")
```

::: {.cell-output .cell-output-stdout}

```
Total individual sample size: 2638 
```


:::
:::


## **(1.1) Varying assumptions - Standard sample size calculation**

### **(1.1.1) Varying Effect size and varying ICC**

3-D plot, varying the effect size (10 pp to 20 pp) & varying ICC (0.05 to 0.2)

Keep the baseline prescription rate at 20% (control rate)

Keep m (cluster size) at 20

Keep the CV at 0.9


::: {.cell}

```{.r .cell-code}
# Define parameters
power <- 0.80
alpha <- 0.05
p_C <- 0.20
CV <- 0.9
m <- 20

# Ranges
ICC_values <- seq(0.05, 0.20, by = 0.01)
effect_sizes_pp <- seq(10, 20, by = 1)

# Create grid
results_3d <- expand.grid(
  ICC = ICC_values,
  effect_size_pp = effect_sizes_pp
)

results_3d$n_clusters_per_arm <- NA

cohen_h <- function(p1, p2) {
  2 * (asin(sqrt(p1)) - asin(sqrt(p2)))
}

for (i in 1:nrow(results_3d)) {
  icc <- results_3d$ICC[i]
  delta_pp <- results_3d$effect_size_pp[i]
  
  p_I <- p_C + (delta_pp / 100)
  
  if (p_I < 0) {
    next
  }
  
  deff_cv <- 1 + ((m * (1 + CV^2)) - 1) * icc
  
  h <- cohen_h(p_I, p_C)
  
  ss <- pwr.2p.test(h = h, power = power, sig.level = alpha)$n
  
  n_per_arm_crt <- ceiling(ss * deff_cv)
  
  n_clusters_per_arm <- ceiling(n_per_arm_crt / m)
  
  results_3d$n_clusters_per_arm[i] <- n_clusters_per_arm
}
```
:::



::: {.cell}

```{.r .cell-code}
ggplot(results_3d, aes(x = effect_size_pp, y = ICC, fill = n_clusters_per_arm)) +
  geom_tile() +
  geom_text(aes(label = n_clusters_per_arm), size = 2.5, color = "black") +
  scale_fill_gradient2(
    low = "darkgreen", 
    mid = "yellow", 
    high = "darkred",
    midpoint = median(results_3d$n_clusters_per_arm, na.rm = TRUE),
    name = "Clusters\nper arm"
  ) +
  labs(
    title = "Clusters per arm: ICC vs Effect size",
    x = "Effect size (percentage point increase)",
    y = "ICC"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_x_continuous(breaks = seq(10, 20, by = 1)) +
  scale_y_continuous(breaks = seq(0.05, 0.30, by = 0.01))
```

::: {.cell-output-display}
![](SHCS-CVD-statins_files/figure-html/unnamed-chunk-8-1.png){width=960}
:::
:::


### **(1.1.2) Varying CV and varying ICC**

3-D plot, varying the CV (0.3 to 1.4) & varying ICC (0.05 to 0.2)

Keep the baseline prescription rate at 20% (control rate) and delta at 10 pp

Keep m (cluster size) at 20


::: {.cell}

```{.r .cell-code}
# Define parameters
power <- 0.80
alpha <- 0.05
p_C <- 0.20
delta_pp <- 10
m <- 20

# Ranges
ICC_values <- seq(0.05, 0.20, by = 0.01)
CV_values <- seq(0.3, 1.4, by = 0.1)

# Create grid
results_3d <- expand.grid(
  ICC = ICC_values,
  CV = CV_values
)

results_3d$n_clusters_per_arm <- NA

cohen_h <- function(p1, p2) {
  2 * (asin(sqrt(p1)) - asin(sqrt(p2)))
}

for (i in 1:nrow(results_3d)) {
  icc <- results_3d$ICC[i]
  cv <- results_3d$CV[i]
  
  p_I <- p_C + (delta_pp / 100)
  
  deff_cv <- 1 + ((m * (1 + cv^2)) - 1) * icc
  
  h <- cohen_h(p_I, p_C)
  
  ss <- pwr.2p.test(h = h, power = power, sig.level = alpha)$n
  
  n_per_arm_crt <- ceiling(ss * deff_cv)
  n_clusters_per_arm <- ceiling(n_per_arm_crt / m)
  
  results_3d$n_clusters_per_arm[i] <- n_clusters_per_arm
}
```
:::



::: {.cell}

```{.r .cell-code}
ggplot(results_3d, aes(x = CV, y = ICC, fill = n_clusters_per_arm)) +
  geom_tile() +
  geom_text(aes(label = n_clusters_per_arm), size = 2.5, color = "black") +
  scale_fill_gradient2(
    low = "darkgreen",
    mid = "yellow",
    high = "darkred",
    midpoint = median(results_3d$n_clusters_per_arm, na.rm = TRUE),
    name = "Clusters\nper arm"
  ) +
  labs(
    title = "Clusters per arm: ICC vs CV (Effect size = 10pp)",
    x = "Coefficient of Variation (CV)",
    y = "ICC"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_x_continuous(breaks = seq(0.3, 1.4, by = 0.1)) +
  scale_y_continuous(breaks = seq(0.05, 0.20, by = 0.01))
```

::: {.cell-output-display}
![](SHCS-CVD-statins_files/figure-html/unnamed-chunk-10-1.png){width=960}
:::
:::


# **(2) Sample size calculation CRT: Simulation-based**

# **(3) Simulate the full dataset and implement the main analysis strategy**

# **(4) Stratified randomization algorithm**

## **(4.1) Covariate-constrained randomization**

If we use batch-randomization (all clusters randomized at once and no new clusters entering later), the probably simple covariate-constrained randomization to be used: <https://rethinkingclinicaltrials.org/chapters/design/experimental-designs-and-randomization-schemes/covariate-constrained-randomization/>

-   Exact 1:1 overall allocation:

    -   XX Control / XX Intervention

-   Soft site stratification:

    -   Each site gets roughly half clusters in each arm

    -   Small deviations allowed for odd-sized sites

-   Global numeric balance re baseline outcome rate:

    -   Optimises baseline_rate mean difference between arms

-   Random selection among best allocations:

    -   Preserves randomness while enforcing optimal balance


::: {.cell}

```{.r .cell-code}
set.seed(20250820)

n_clusters <- 84
site <- factor(sample(
  1:7, n_clusters, replace = TRUE,
  prob = c(0.05, 0.15, 0.20, 0.10, 0.15, 0.25, 0.10)
))

cluster_data <- data.frame(
  cluster_id = 1:n_clusters,
  baseline_rate = runif(n_clusters, 0.10, 0.30),
  site = site
)

treatments <- c("Control", "Intervention")

### CCR function for global randomisation
run_global_ccr <- function(df, n_sims = 10000, top_pct = 0.10) {
  n <- nrow(df)
  n_ctrl <- n_int <- n / 2  # exact 1:1 allocation
  
  scores <- numeric(n_sims)
  allocs <- matrix(NA, nrow = n_sims, ncol = n)
  
  for (i in 1:n_sims) {
    # Random 1:1 allocation
    arm_assign <- sample(c(rep("Control", n_ctrl),
                           rep("Intervention", n_int)))
    allocs[i, ] <- arm_assign
    temp <- df
    temp$arm <- arm_assign
    
    # Numeric covariate imbalance
    means_diff <- abs(tapply(temp$baseline_rate, temp$arm, mean)["Control"] -
                      tapply(temp$baseline_rate, temp$arm, mean)["Intervention"])
    
    # Soft site stratification: imbalance = sum of squared differences between observed vs ideal allocation per site
    site_table <- table(temp$site, temp$arm)
    ideal_site <- table(temp$site) / 2  # target per arm per site (soft)
    site_diff <- sum((site_table[, "Control"] - ideal_site)^2 +
                     (site_table[, "Intervention"] - ideal_site)^2)
    
    # Total score: numeric imbalance + small weight for site imbalance
    scores[i] <- means_diff + 0.01 * site_diff
  }
  
  # Select best allocations
  threshold <- quantile(scores, top_pct)
  best_idx <- which(scores <= threshold)
  chosen <- sample(best_idx, 1)
  
  df$final_arm <- allocs[chosen, ]
  return(df)
}

### Run it
final_result <- run_global_ccr(cluster_data)
print(final_result)
```

::: {.cell-output .cell-output-stdout}

```
   cluster_id baseline_rate site    final_arm
1           1     0.2036752    4 Intervention
2           2     0.1538713    6 Intervention
3           3     0.2872879    1 Intervention
4           4     0.2553020    7      Control
5           5     0.2265234    6 Intervention
6           6     0.1547697    3 Intervention
7           7     0.2297495    2      Control
8           8     0.1310071    6      Control
9           9     0.1890881    6      Control
10         10     0.2775121    1      Control
11         11     0.2726826    2      Control
12         12     0.2003763    6 Intervention
13         13     0.2689853    6 Intervention
14         14     0.2375503    5 Intervention
15         15     0.1873635    6      Control
16         16     0.2894014    6      Control
17         17     0.2152192    4 Intervention
18         18     0.1715369    5 Intervention
19         19     0.1718225    6 Intervention
20         20     0.1714866    3 Intervention
21         21     0.2793366    6      Control
22         22     0.2825824    5      Control
23         23     0.1194723    1 Intervention
24         24     0.1319895    3 Intervention
25         25     0.2551463    6      Control
26         26     0.1119110    1 Intervention
27         27     0.1765808    7 Intervention
28         28     0.2055012    3 Intervention
29         29     0.2521738    4      Control
30         30     0.2106645    6 Intervention
31         31     0.2167770    3      Control
32         32     0.1228993    4      Control
33         33     0.2726487    7      Control
34         34     0.2020688    6 Intervention
35         35     0.1361247    6 Intervention
36         36     0.2728437    5 Intervention
37         37     0.1800930    7      Control
38         38     0.1551175    5      Control
39         39     0.2382141    2 Intervention
40         40     0.1007362    3 Intervention
41         41     0.1347393    1      Control
42         42     0.1894948    5 Intervention
43         43     0.1076908    6      Control
44         44     0.2282972    3 Intervention
45         45     0.2232728    6 Intervention
46         46     0.1500583    6 Intervention
47         47     0.2791531    4      Control
48         48     0.2603365    2 Intervention
49         49     0.1791727    2      Control
50         50     0.1377176    3      Control
51         51     0.2838408    3      Control
52         52     0.1341835    4 Intervention
53         53     0.2962462    1 Intervention
54         54     0.2248252    6      Control
55         55     0.2579708    4      Control
56         56     0.1543211    6 Intervention
57         57     0.2567312    6      Control
58         58     0.1809671    2 Intervention
59         59     0.1522663    5      Control
60         60     0.1476439    4      Control
61         61     0.1748280    3      Control
62         62     0.1879722    1      Control
63         63     0.1729529    3 Intervention
64         64     0.1822520    6      Control
65         65     0.2968177    3 Intervention
66         66     0.1541102    3      Control
67         67     0.2025564    4 Intervention
68         68     0.1435154    2 Intervention
69         69     0.2201463    1      Control
70         70     0.1301909    6 Intervention
71         71     0.2772403    3      Control
72         72     0.2622415    4 Intervention
73         73     0.2408050    5 Intervention
74         74     0.2476875    5      Control
75         75     0.1948942    3      Control
76         76     0.1611351    2      Control
77         77     0.1865994    7      Control
78         78     0.2982802    4      Control
79         79     0.1610247    7 Intervention
80         80     0.1639609    1      Control
81         81     0.2554029    4 Intervention
82         82     0.1377876    5      Control
83         83     0.1803049    2      Control
84         84     0.2568941    6 Intervention
```


:::

```{.r .cell-code}
### Checks
cat("\nOverall treatment counts:\n")
```

::: {.cell-output .cell-output-stdout}

```

Overall treatment counts:
```


:::

```{.r .cell-code}
print(table(final_result$final_arm))
```

::: {.cell-output .cell-output-stdout}

```

     Control Intervention 
          42           42 
```


:::

```{.r .cell-code}
cat("\nBalance within each site (aim: approximate 1:1):\n")
```

::: {.cell-output .cell-output-stdout}

```

Balance within each site (aim: approximate 1:1):
```


:::

```{.r .cell-code}
print(table(final_result$site, final_result$final_arm))
```

::: {.cell-output .cell-output-stdout}

```
   
    Control Intervention
  1       5            4
  2       5            4
  3       7            8
  4       6            6
  5       5            5
  6      10           13
  7       4            2
```


:::

```{.r .cell-code}
cat("\nBalance by mean baseline_rate by arm (aim: approximate 1:1):\n")
```

::: {.cell-output .cell-output-stdout}

```

Balance by mean baseline_rate by arm (aim: approximate 1:1):
```


:::

```{.r .cell-code}
print(tapply(final_result$baseline_rate, final_result$final_arm, mean))
```

::: {.cell-output .cell-output-stdout}

```
     Control Intervention 
   0.2089960    0.1978283 
```


:::
:::

