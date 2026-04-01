# fluorojip Heatmap Parameter Cheat Sheet

This note explains the main `fluorojip` parameters used in the heatmap:

- `Fv_Fm`
- `PI_abs`
- `ABS_RC`
- `ETo_RC`
- `DIo_RC`

## Important Reminder

The heatmap usually shows scaled values, not raw values.

- red = higher relative to the other samples in the plotted dataset
- blue = lower relative to the other samples in the plotted dataset
- white = near the dataset average

So the heatmap is best for comparing patterns among samples, not for reading exact biological values.

## Fv_Fm

- Meaning: maximum quantum yield of PSII photochemistry.
- Formula: `(Fm - Fo) / Fm`
- Typical range: about `0.75` to `0.85` in healthy dark-adapted samples.
- Higher values: usually indicate better PSII efficiency.
- Lower values: often suggest stress, photoinhibition, poor dark adaptation, or measurement problems.

Heatmap reading:

- red = relatively higher PSII efficiency
- blue = relatively lower PSII efficiency

## PI_abs

- Meaning: performance index on absorption basis.
- Interpretation: a compound PSII performance indicator combining reaction-center density/activity, primary photochemistry, and electron transport efficiency.
- Typical range: often around `0` to `20`, sometimes higher, but very large values should be checked carefully.
- Higher values: usually indicate stronger overall PSII performance.
- Lower values: usually indicate weaker PSII vitality or stress.

Heatmap reading:

- red = relatively better overall PSII performance
- blue = relatively poorer overall performance

## ABS_RC

- Meaning: absorbed energy flux per active reaction center.
- Interpretation: how much antenna absorption is effectively assigned to each active RC.
- Higher values: can mean fewer active reaction centers are carrying more excitation load; this is not automatically good and may indicate stress.
- Lower values: can mean more active reaction centers are sharing the absorbed energy.

Heatmap reading:

- red = relatively higher absorption load per RC
- blue = relatively lower absorption load per RC

## ETo_RC

- Meaning: electron transport flux per reaction center.
- Interpretation: how much trapped energy is successfully passed onward into electron transport beyond `Q_A`.
- Higher values: usually indicate stronger downstream photochemical performance.
- Lower values: may indicate reduced electron transport efficiency or stress effects on PSII function.

Heatmap reading:

- red = relatively stronger electron transport per RC
- blue = relatively weaker electron transport per RC

## DIo_RC

- Meaning: dissipated energy flux per reaction center.
- Interpretation: energy lost as heat and fluorescence instead of being used in photochemistry.
- Higher values: often indicate greater energy dissipation and can be associated with stress or reduced photochemical use.
- Lower values: often indicate less dissipation and relatively more efficient use of absorbed energy.

Heatmap reading:

- red = relatively more energy dissipation
- blue = relatively less energy dissipation

## How To Read Them Together

- High `Fv_Fm` + high `PI_abs` + high `ETo_RC` + low `DIo_RC`:
  usually a strong, healthy sample.
- Low `Fv_Fm` + low `PI_abs` + low `ETo_RC` + high `DIo_RC`:
  usually a stressed or impaired sample.
- High `ABS_RC` together with high `DIo_RC`:
  can suggest that fewer active reaction centers are carrying more absorbed energy and dissipating more of it.

## Suggested Check In RStudio

To compare the heatmap with the real calculated values, run:

```r
res[, c("sample_id", "Fv_Fm", "PI_abs", "ABS_RC", "ETo_RC", "DIo_RC")]
```

That makes it easier to connect:

- the raw parameter values
- the heatmap colors
- the biological interpretation
