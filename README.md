# Standardised inflows of permanent-type migrants


Data source: “Standardised inflows of permanent-type migrants” by OECD
(Last updated: November 03, 2025)

``` r
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(forcats)

d <- readr::read_csv(
  "data/OECD.ELS.IMD,DSD_MIG_INT@DF_MIG_INT_PER,1.0,complete,2026-10-17 15-19-58.csv"
)
```

    Registered S3 method overwritten by 'bit64':
      method          from 
      print.bitstring tools

    Rows: 1882 Columns: 24
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (18): STRUCTURE, STRUCTURE_ID, STRUCTURE_NAME, ACTION, REF_AREA, Referen...
    dbl  (4): TIME_PERIOD, OBS_VALUE, UNIT_MULT, DECIMALS
    lgl  (2): Time period, Observation value

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
d_summarised <- d |>
  group_by(
    area = `Reference area`,
    year = TIME_PERIOD,
    type = recode(`Entry category`, "Accompanying family of workers" = "Family")
  ) |>
  summarise(n = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop")
```

## Number of Migrants

``` r
d_by_country <- d_summarised |>
  summarise(n = sum(n), .by = c(area, year)) |>
  mutate(area = fct_reorder(area, n, .desc = TRUE))

ggplot(d_by_country, aes(year, n, colour = area)) +
  geom_line() +
  geom_point() +
  gghighlight::gghighlight(
    sum(n),
    max_highlight = 15L,
    line_label_type = "sec_axis"
  ) +
  theme_light() +
  theme(
    text = element_text(size = 12)
  )
```

    label_key: area

![](README_files/figure-commonmark/unnamed-chunk-3-1.png)

## Migration Reasons

``` r
d_pct <- d_summarised |>
  filter(
    area %in%
      c(
        "United States",
        "Germany",
        "United Kingdom",
        "Canada",
        "Korea",
        "Japan"
      )
  ) |>
  mutate(`%` = n / sum(n), .by = c(area, year))

ggplot(d_pct, aes(type, `%`, colour = year, group = factor(year))) +
  geom_polygon(fill = NA) +
  see::coord_radar() +
  facet_wrap(vars(area)) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_blank(),
    strip.text = element_text(size = 15, face = "bold")
  )
```

![](README_files/figure-commonmark/unnamed-chunk-4-1.png)
