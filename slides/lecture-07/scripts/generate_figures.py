"""Create the SVG figures used in lecture 7."""

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
FIGURES = ROOT / "figures"
BLUE = "#4a6fa5"
RED = "#c74440"
TEXT = "#253447"
OCCASIONS = ["Πριν", "2 εβδομάδες", "4 εβδομάδες", "6 εβδομάδες"]
COLUMNS = [
    "participation_pre",
    "participation_w2",
    "participation_w4",
    "participation_w6",
]

plt.rcParams.update(
    {
        "font.family": "DejaVu Sans",
        "font.size": 15,
        "svg.fonttype": "none",
        "text.color": TEXT,
        "axes.labelcolor": TEXT,
        "xtick.color": TEXT,
        "ytick.color": TEXT,
    }
)


def clean_axis(axis: plt.Axes) -> None:
    axis.spines[["top", "right"]].set_visible(False)
    axis.grid(axis="y", alpha=0.18)
    axis.set_axisbelow(True)


def demo_trajectories() -> None:
    data = pd.read_csv(ROOT / "demo_repeated_measures.csv")
    values = data.iloc[:, 1:].to_numpy(dtype=float)
    x = np.arange(4)
    figure, axis = plt.subplots(figsize=(10.8, 4.1))
    figure.subplots_adjust(left=0.10, right=0.96, bottom=0.22, top=0.93)
    for row, child in zip(values, data["child_id"]):
        axis.plot(x, row, marker="o", linewidth=1.8, alpha=0.58, label=child)
    axis.plot(
        x,
        values.mean(axis=0),
        color=RED,
        marker="D",
        linewidth=3.3,
        markersize=7,
        label="Μέσος όρος",
        zorder=10,
    )
    axis.set_xticks(x, OCCASIONS)
    axis.set_yticks(range(8, 18, 2))
    axis.set_ylim(7, 18)
    axis.set_ylabel("Συμμετοχή στο συνεργατικό παιχνίδι (0–30)")
    clean_axis(axis)
    axis.legend(
        ncol=3,
        loc="upper center",
        bbox_to_anchor=(0.5, -0.27),
        frameon=False,
        fontsize=12,
    )
    figure.savefig(FIGURES / "01-demo-trajectories.svg")
    plt.close(figure)


def practical_means() -> None:
    data = pd.read_csv(ROOT / "repeated_measures_kindergarten.csv", encoding="utf-8-sig")
    values = data[COLUMNS].to_numpy(dtype=float)
    means = values.mean(axis=0)
    standard_errors = values.std(axis=0, ddof=1) / np.sqrt(len(values))
    ci_half_width = 2.012 * standard_errors  # t(.975, 47)
    x = np.arange(4)
    figure, axis = plt.subplots(figsize=(10.8, 4.1))
    figure.subplots_adjust(left=0.10, right=0.96, bottom=0.20, top=0.93)
    axis.errorbar(
        x,
        means,
        yerr=ci_half_width,
        color=BLUE,
        marker="o",
        linewidth=3,
        markersize=9,
        capsize=6,
    )
    for index, value in enumerate(means):
        axis.annotate(
            f"{value:.1f}",
            (index, value),
            xytext=(0, 13),
            textcoords="offset points",
            ha="center",
            color=BLUE,
            fontweight="bold",
        )
    axis.set_xticks(x, OCCASIONS)
    axis.set_ylim(9, 21)
    axis.set_yticks(range(10, 22, 2))
    axis.set_ylabel("Μέση συμμετοχή στο συνεργατικό παιχνίδι (0–30)")
    clean_axis(axis)
    axis.text(
        0.99,
        0.03,
        "Ράβδοι: 95% ΔΕ του μέσου",
        transform=axis.transAxes,
        ha="right",
        va="bottom",
        fontsize=11,
        color="#596779",
    )
    figure.savefig(FIGURES / "02-practical-means.svg")
    plt.close(figure)


if __name__ == "__main__":
    demo_trajectories()
    practical_means()
