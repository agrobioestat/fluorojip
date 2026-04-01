# fluorojip Core Outputs Cheat Sheet

This note explains several additional `fluorojip` outputs that are useful when reviewing JIP-test results:

- `Vj`
- `Vi`
- `Mo`
- `TRo_RC`
- `phi_Eo`
- `psi_Eo`
- `Sm`
- `N`

## Vj

- Meaning: relative variable fluorescence at the J step.
- Formula: `(Fj - Fo) / (Fm - Fo)`
- Interpretation: reflects the reduction state around the acceptor side of PSII, especially around `Q_A`.
- Higher values: often indicate stronger accumulation of reduced `Q_A` and restriction further downstream.
- Lower values: often indicate smoother electron flow away from PSII.

In practice:

- high `Vj` can be a stress signal
- lower `Vj` is often associated with better electron transport performance

## Vi

- Meaning: relative variable fluorescence at the I step.
- Formula: `(Fi - Fo) / (Fm - Fo)`
- Interpretation: reflects later stages of the OJIP rise than `Vj`.
- Higher values: can indicate stronger accumulation of reduced intermediates farther along the chain.
- Lower values: can indicate less accumulation and more efficient downstream transfer.

In practice:

- `Vi` is often interpreted together with `Vj`
- changes in `Vi` may suggest effects beyond the very early PSII acceptor side

## Mo

- Meaning: approximated initial slope of the fluorescence rise.
- In this corrected package: computed from the K-step / 300 us signal.
- Interpretation: reflects the initial rate of closure of PSII reaction centers.
- Higher values: faster initial rise, often associated with stronger RC closure pressure or stress-related effects.
- Lower values: slower initial rise.

Important:

- `Mo` should not be interpreted alone
- it is more informative when read together with `Vj`, `ABS_RC`, and `PI_abs`

## TRo_RC

- Meaning: trapped energy flux per reaction center.
- Interpretation: how much absorbed excitation is trapped in PSII per active RC.
- Higher values: more trapping per active reaction center.
- Lower values: less trapping per RC.

Important:

- high `TRo_RC` is not automatically “better”
- its interpretation depends on the balance with `ETo_RC` and `DIo_RC`

## phi_Eo

- Meaning: quantum yield of electron transport.
- Formula: `phi_Po * psi_Eo`
- Interpretation: probability that an absorbed photon leads not only to trapping, but also to electron transport beyond `Q_A`.
- Higher values: better photochemical performance and downstream transport.
- Lower values: reduced electron transport efficiency.

In practice:

- often tracks sample vitality well
- lower `phi_Eo` is common under stress

## psi_Eo

- Meaning: efficiency/probability that a trapped exciton moves an electron further than `Q_A-`.
- Formula: `1 - Vj`
- Interpretation: expresses how well trapped energy is converted into forward electron transport.
- Higher values: better transfer beyond primary quinone acceptors.
- Lower values: more limitation at or beyond `Q_A`.

In practice:

- high `Vj` usually means low `psi_Eo`
- `psi_Eo` is an important component of `PI_abs`

## Sm

- Meaning: normalized area above the fluorescence induction curve.
- Interpretation: related to the energy needed to close all PSII reaction centers and to the size/behavior of the electron acceptor pool.
- Higher values: larger normalized area.
- Lower values: smaller normalized area.

Important:

- `Sm` is often useful but less intuitive than `Fv_Fm` or `PI_abs`
- it should usually be interpreted alongside other parameters, not by itself

## N

- Meaning: turnover-related term derived from `Sm` and trapping terms.
- Interpretation: often linked to the number of reduction/turnover events needed to reach maximum fluorescence.
- Higher values: may reflect more turnover steps before full closure.
- Lower values: fewer effective turnovers.

Important:

- `N` is sensitive to upstream terms and is best treated as a supporting parameter

## How To Read These Together

- High `Vj` + low `psi_Eo`:
  likely restriction in forward electron transport.
- High `Mo` + high `Vj`:
  rapid early RC closure with stronger downstream limitation.
- High `TRo_RC` but low `ETo_RC`:
  trapping occurs, but a smaller share proceeds to electron transport.
- High `phi_Eo` + high `PI_abs`:
  usually indicates strong PSII function.

## Suggested Check In RStudio

To inspect these values directly, run:

```r
res[, c("sample_id", "Vj", "Vi", "Mo", "TRo_RC", "phi_Eo", "psi_Eo", "Sm", "N")]
```

That helps connect:

- the calculated table
- the underlying JIP-test interpretation
- any patterns you see in the heatmap or 3D plot
