"""Generate the repeated-measures teaching dataset and reference calculations.

The file is deliberately in wide format: one row per child and one outcome
column per measurement occasion, which is the structure used by jamovi's
Repeated Measures ANOVA.
"""

from __future__ import annotations

import itertools
import json
from pathlib import Path

import numpy as np
import pandas as pd
from scipy import stats
from scipy.linalg import helmert


ROOT = Path(__file__).resolve().parents[1]
SEED = 7_102_026
N_CHILDREN = 48
COLUMNS = [
    "participation_pre",
    "participation_w2",
    "participation_w4",
    "participation_w6",
]
LABELS = ["Πριν", "2 εβδομάδες", "4 εβδομάδες", "6 εβδομάδες"]


def holm_adjust(p_values: list[float]) -> list[float]:
    """Holm step-down adjusted p values, in the original comparison order."""
    order = np.argsort(p_values)
    adjusted = np.empty(len(p_values), dtype=float)
    previous = 0.0
    m = len(p_values)
    for rank, index in enumerate(order):
        value = min(1.0, (m - rank) * p_values[index])
        previous = max(previous, value)
        adjusted[index] = previous
    return adjusted.tolist()


def mauchly_and_epsilons(values: np.ndarray) -> dict[str, float]:
    """Return the conventional Mauchly approximation and GG/HF epsilons.

    The contrast matrix is orthonormal, so the calculation is invariant to the
    particular Helmert contrasts used. These values are only a numerical
    cross-check for the classroom dataset; jamovi is the analysis interface
    used by students.
    """
    n, k = values.shape
    q = k - 1
    contrasts = helmert(k, full=False)
    covariance = np.cov(values, rowvar=False, ddof=1)
    contrast_covariance = contrasts @ covariance @ contrasts.T
    mean_eigenvalue = np.trace(contrast_covariance) / q
    w = float(np.linalg.det(contrast_covariance) / mean_eigenvalue**q)
    correction = 1 - (2 * q**2 + q + 2) / (6 * q * (n - 1))
    chi_square = float(-(n - 1) * correction * np.log(w))
    df = q * (q + 1) / 2 - 1
    p_value = float(stats.chi2.sf(chi_square, df))
    epsilon_gg = float(
        np.trace(contrast_covariance) ** 2
        / (q * np.trace(contrast_covariance @ contrast_covariance))
    )
    epsilon_hf = float(
        min(1.0, (n * q * epsilon_gg - 2) / (q * (n - 1 - q * epsilon_gg)))
    )
    return {
        "W": w,
        "chi_square": chi_square,
        "df": df,
        "p": p_value,
        "epsilon_GG": epsilon_gg,
        "epsilon_HF": epsilon_hf,
    }


def main() -> None:
    rng = np.random.default_rng(SEED)

    # Simulated observed participation (0--30) in cooperative play. A child
    # random intercept reflects stable individual differences; correlated
    # occasion-level errors retain the within-child dependence.
    population_means = np.array([13.2, 14.6, 16.5, 17.3])
    child_baseline = rng.normal(0, 3.2, N_CHILDREN)[:, None]
    residual_sd, residual_correlation = 2.6, 0.45
    residual_covariance = residual_sd**2 * (
        (1 - residual_correlation) * np.eye(4)
        + residual_correlation * np.ones((4, 4))
    )
    residuals = rng.multivariate_normal(
        np.zeros(4), residual_covariance, N_CHILDREN
    )
    values = np.round(population_means + child_baseline + residuals, 1)
    assert np.all((values >= 0) & (values <= 30))

    data = pd.DataFrame(values, columns=COLUMNS)
    data.insert(0, "child_id", [f"N{index:02d}" for index in range(1, N_CHILDREN + 1)])
    data.to_csv(ROOT / "repeated_measures_kindergarten.csv", index=False, encoding="utf-8-sig")

    # Re-read the delivered data: all reference values below use the rounded CSV.
    data = pd.read_csv(ROOT / "repeated_measures_kindergarten.csv", encoding="utf-8-sig")
    values = data[COLUMNS].to_numpy(dtype=float)
    n, k = values.shape
    grand_mean = float(values.mean())
    time_means = values.mean(axis=0)
    child_means = values.mean(axis=1)

    ss_total = float(((values - grand_mean) ** 2).sum())
    ss_time = float(n * ((time_means - grand_mean) ** 2).sum())
    ss_children = float(k * ((child_means - grand_mean) ** 2).sum())
    ss_error = float(ss_total - ss_time - ss_children)
    df_time, df_children, df_error = k - 1, n - 1, (n - 1) * (k - 1)
    ms_time, ms_error = ss_time / df_time, ss_error / df_error
    f_value = ms_time / ms_error

    residual_matrix = values - child_means[:, None] - time_means[None, :] + grand_mean
    descriptives = []
    for label, column in zip(LABELS, COLUMNS):
        x = data[column]
        descriptives.append(
            {
                "occasion": label,
                "column": column,
                "n": int(x.count()),
                "mean": float(x.mean()),
                "sd": float(x.std(ddof=1)),
                "median": float(x.median()),
                "min": float(x.min()),
                "max": float(x.max()),
            }
        )

    pairwise = []
    raw_p = []
    pairs = list(itertools.combinations(range(k), 2))
    for first, second in pairs:
        difference = values[:, second] - values[:, first]
        t_value, p_value = stats.ttest_1samp(difference, popmean=0)
        raw_p.append(float(p_value))
        pairwise.append(
            {
                "first": LABELS[first],
                "second": LABELS[second],
                "difference_second_minus_first": float(difference.mean()),
                "sd_difference": float(difference.std(ddof=1)),
                "t": float(t_value),
                "df": n - 1,
                "p_unadjusted": float(p_value),
            }
        )
    for result, p_adjusted in zip(pairwise, holm_adjust(raw_p)):
        result["p_holm"] = p_adjusted

    sphericity = mauchly_and_epsilons(values)
    result = {
        "dataset": {
            "N": n,
            "seed": SEED,
            "outcome": "Συμμετοχή στο συνεργατικό παιχνίδι (0–30)",
            "factor": "Χρόνος",
            "levels": LABELS,
            "format": "wide",
        },
        "descriptives": descriptives,
        "anova": {
            "SS_total": ss_total,
            "SS_time": ss_time,
            "SS_children": ss_children,
            "SS_error": ss_error,
            "df_time": df_time,
            "df_children": df_children,
            "df_error": df_error,
            "MS_time": ms_time,
            "MS_error": ms_error,
            "F": f_value,
            "p_uncorrected": float(stats.f.sf(f_value, df_time, df_error)),
            "partial_eta_squared": float(ss_time / (ss_time + ss_error)),
            "residual_shapiro_W": float(stats.shapiro(residual_matrix.ravel()).statistic),
            "residual_shapiro_p": float(stats.shapiro(residual_matrix.ravel()).pvalue),
        },
        "sphericity": sphericity,
        "corrected_tests": {
            "Greenhouse_Geisser_df1": float(sphericity["epsilon_GG"] * df_time),
            "Greenhouse_Geisser_df2": float(sphericity["epsilon_GG"] * df_error),
            "Greenhouse_Geisser_p": float(
                stats.f.sf(
                    f_value,
                    sphericity["epsilon_GG"] * df_time,
                    sphericity["epsilon_GG"] * df_error,
                )
            ),
            "Huynh_Feldt_df1": float(sphericity["epsilon_HF"] * df_time),
            "Huynh_Feldt_df2": float(sphericity["epsilon_HF"] * df_error),
            "Huynh_Feldt_p": float(
                stats.f.sf(
                    f_value,
                    sphericity["epsilon_HF"] * df_time,
                    sphericity["epsilon_HF"] * df_error,
                )
            ),
        },
        "pairwise_paired_t_holm": pairwise,
    }
    (ROOT / "results.json").write_text(
        json.dumps(result, ensure_ascii=False, indent=2), encoding="utf-8"
    )
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
