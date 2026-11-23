# fluorojip 0.2.0

## Trace import and extraction

* New `trace_to_ojip()` converts fluorescence traces into OJIP summary values
  with explicit target times, tolerance windows, optional background
  subtraction, an explicit or inferred time unit, and quality flags. For every
  sample it reports the times actually used (`t_fo_ms`, `t_f300us_ms`,
  `t_j_ms`, `t_i_ms`, `t_fm_ms`). A value whose nearest recorded point lies
  outside the tolerance window is returned as `NA` and flagged; it is no
  longer replaced silently by the nearest available point.
* New `ojip_preset()` with documented presets `"biolyzer"`/`"handypea"`
  (Fo 20 us, early point 270 us, J 2 ms, I 30 ms) and `"fluorpen"`
  (background subtraction, Fo at the nominal 50 us point, 300 us, 2 ms,
  30 ms).
* New quality flags: `fm_at_last_point`, `fm_before_i`,
  `*_time_out_of_tolerance`, `mo_interval_deviation`, `few_points`, and,
  from `calc_fluorojip()`, `fo_fm_invalid`, `fj_out_of_range`,
  `fi_out_of_range`, `f300us_out_of_range`, `f300us_missing`,
  `non_monotonic`.
* Optional, experimental `step_method = "derivative"` locates J and I at the
  minima of the smoothed derivative dF/dlog(t).
* Zero-padded and non-finite time points (for example at the end of Biolyzer
  exports) are discarded before extraction; previously they could cause the
  time unit to be misidentified.

## Importers

* New primary names `read_biolyzer_csv()`, `biolyzer_to_ojip()`, and
  `calc_fluorojip_biolyzer()`; `read_handypea_csv()`, `handypea_to_ojip()`,
  and `calc_fluorojip_handypea()` remain as aliases (the names refer to the
  Hansatech Handy-PEA instrument whose traces Biolyzer exports).
* New `read_biolyzer_xls()` reads Biolyzer Excel exports (`Trace data` and
  `JIP Parameters` sheets).
* `fluorpen_to_ojip()` now uses the FluorPen convention: the first exported
  point (background) is subtracted and Fo is the nominal 50 us point. In
  0.1.x, Fo was taken at 21 us without background subtraction, which biased
  FluorPen-derived parameters (for example mean Mo -0.013, ABS/RC -0.084 for
  the bundled workbook). Light settings and protocol information are kept.
* `read_fluorpen_xlsx()` repairs footer values that lost their decimal mark
  (locale conversion, `1.017` stored as `1017`) by dividing by 1000. The
  0.1.x heuristic (repeated division by 10 below a parameter-specific bound)
  was incorrect for values above the bound.

## Calculation

* `calc_fluorojip()` accepts `f300us` as the preferred name of the early O-J
  point (`k` and other aliases remain accepted), gains `mo_interval_ms`
  (default 0.25 ms, i.e. the factor 4 in Mo) and `digits`, and returns
  `qc_flags`.
* New outputs `phi_Do`, `RC_ABS`, and the cross-section family at t = 0
  (`ABS_CSo`, `TRo_CSo`, `ETo_CSo`, `DIo_CSo`, `RC_CSo`). All equations are
  listed in `?calc_fluorojip`.
* The always-`NA` columns `Kn` and `Kp` were removed.

## Validation

* New `agreement_metrics()`, `validate_biolyzer()`, and `validate_fluorpen()`
  compare fluorojip with the values exported by Biolyzer and by the FluorPen
  software, either for the equations only (`mode = "engine"`) or for import
  plus equations (`mode = "importer"`).
* The test suite now covers hand-calculated values, invalid inputs,
  non-uniform, truncated, noisy, and multi-peak synthetic traces, and
  numerical agreement with the bundled Biolyzer and FluorPen exports.

## Visualization and Shiny

* `plot_heatmap_fluorojip()` now draws a colour key, uses a colour-blind-safe
  diverging palette with limits symmetric around zero for z-scores, and
  supports clustering on/off and control-based scaling.
* `plot_3d_fluorojip()` uses the Okabe-Ito palette, different symbols per
  group, and a legend.
* New `plot_ojip_trace()` draws traces with the extracted O, K, J, I, and P
  points.
* The Shiny app gains Biolyzer import, preset selection, a quality-flag
  summary, curve display with extracted points, validation in both modes using
  the bundled files, and an *analysis log* download (an R script recording
  versions, input checksum, settings, and the commands that reproduce the
  analysis).

# fluorojip 0.1.1

* First CRAN release.
