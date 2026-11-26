"""Generate reproducible teaching data, results, and SVG figures.

Requires Python 3 only. Run from any directory:
python3 scripts/generate_assets.py
"""

from __future__ import annotations

import csv
import json
import math
import random
from collections import defaultdict
from pathlib import Path
from statistics import median
from xml.sax.saxutils import escape


ROOT = Path(__file__).resolve().parents[1]
FIGURES = ROOT / "figures"
NAVY = "#1e3a5f"
BLUE = "#4a6fa5"
RED = "#c74440"
GREEN = "#249786"
GOLD = "#e2a552"
TEXT = "#253447"
GRID = "#d9e1ea"
FONT = "Noto Sans, Segoe UI, Arial, sans-serif"


TRANSITION_ROWS = [
    ("Ν01", 25, "Λίγος χρόνος", 15),
    ("Ν02", 32, "Λίγος χρόνος", 19),
    ("Ν03", 40, "Λίγος χρόνος", 17),
    ("Ν04", 48, "Λίγος χρόνος", 21),
    ("Ν05", 58, "Λίγος χρόνος", 19),
    ("Ν06", 72, "Μέτριος χρόνος", 20),
    ("Ν07", 82, "Μέτριος χρόνος", 25),
    ("Ν08", 92, "Μέτριος χρόνος", 22),
    ("Ν09", 104, "Μέτριος χρόνος", 27),
    ("Ν10", 116, "Μέτριος χρόνος", 24),
    ("Ν11", 130, "Πολύς χρόνος", 29),
    ("Ν12", 145, "Πολύς χρόνος", 27),
    ("Ν13", 158, "Πολύς χρόνος", 31),
    ("Ν14", 172, "Πολύς χρόνος", 30),
    ("Ν15", 190, "Πολύς χρόνος", 35),
]

GROUPS = ["Λίγος χρόνος", "Μέτριος χρόνος", "Πολύς χρόνος"]
GROUP_COLORS = {
    "Λίγος χρόνος": BLUE,
    "Μέτριος χρόνος": GREEN,
    "Πολύς χρόνος": GOLD,
}


def mean(values: list[float]) -> float:
    return sum(values) / len(values)


def sample_sd(values: list[float]) -> float:
    value_mean = mean(values)
    return math.sqrt(sum((value - value_mean) ** 2 for value in values) / (len(values) - 1))


def regularized_beta(a: float, b: float, x: float) -> float:
    """Return I_x(a, b) using a continued-fraction approximation."""
    if x <= 0:
        return 0.0
    if x >= 1:
        return 1.0

    def beta_fraction(alpha: float, beta: float, value: float) -> float:
        max_iterations = 200
        epsilon = 3.0e-14
        tiny = 1.0e-300
        qab = alpha + beta
        qap = alpha + 1.0
        qam = alpha - 1.0
        c_value = 1.0
        d_value = 1.0 - qab * value / qap
        if abs(d_value) < tiny:
            d_value = tiny
        d_value = 1.0 / d_value
        h_value = d_value

        for iteration in range(1, max_iterations + 1):
            multiplier = iteration * (beta - iteration) * value
            multiplier /= (qam + 2.0 * iteration) * (alpha + 2.0 * iteration)
            d_value = 1.0 + multiplier * d_value
            if abs(d_value) < tiny:
                d_value = tiny
            c_value = 1.0 + multiplier / c_value
            if abs(c_value) < tiny:
                c_value = tiny
            d_value = 1.0 / d_value
            h_value *= d_value * c_value

            multiplier = -(alpha + iteration) * (qab + iteration) * value
            multiplier /= (alpha + 2.0 * iteration) * (qap + 2.0 * iteration)
            d_value = 1.0 + multiplier * d_value
            if abs(d_value) < tiny:
                d_value = tiny
            c_value = 1.0 + multiplier / c_value
            if abs(c_value) < tiny:
                c_value = tiny
            d_value = 1.0 / d_value
            change = d_value * c_value
            h_value *= change
            if abs(change - 1.0) < epsilon:
                break
        return h_value

    log_beta_term = (
        math.lgamma(a + b)
        - math.lgamma(a)
        - math.lgamma(b)
        + a * math.log(x)
        + b * math.log1p(-x)
    )
    beta_term = math.exp(log_beta_term)
    threshold = (a + 1.0) / (a + b + 2.0)
    if x < threshold:
        return beta_term * beta_fraction(a, b, x) / a
    return 1.0 - beta_term * beta_fraction(b, a, 1.0 - x) / b


def two_sided_t_p_value(t_value: float, degrees_freedom: int) -> float:
    x_value = degrees_freedom / (degrees_freedom + t_value**2)
    return regularized_beta(degrees_freedom / 2.0, 0.5, x_value)


def f_survival_probability(f_value: float, numerator_df: int, denominator_df: int) -> float:
    x_value = denominator_df / (denominator_df + numerator_df * f_value)
    return regularized_beta(denominator_df / 2.0, numerator_df / 2.0, x_value)


def correlation_analysis(rows: list[dict[str, object]]) -> dict[str, float | int]:
    x_values = [float(row["shared_reading_minutes"]) for row in rows]
    y_values = [float(row["vocabulary_score"]) for row in rows]
    x_mean = mean(x_values)
    y_mean = mean(y_values)
    sum_products = sum(
        (x_value - x_mean) * (y_value - y_mean)
        for x_value, y_value in zip(x_values, y_values)
    )
    sum_x_squared = sum((x_value - x_mean) ** 2 for x_value in x_values)
    sum_y_squared = sum((y_value - y_mean) ** 2 for y_value in y_values)
    r_value = sum_products / math.sqrt(sum_x_squared * sum_y_squared)
    count = len(rows)
    degrees_freedom = count - 2
    t_value = r_value * math.sqrt(degrees_freedom / (1.0 - r_value**2))
    p_value = two_sided_t_p_value(abs(t_value), degrees_freedom)
    fisher_z = math.atanh(r_value)
    standard_error = 1.0 / math.sqrt(count - 3)
    ci_low = math.tanh(fisher_z - 1.959963984540054 * standard_error)
    ci_high = math.tanh(fisher_z + 1.959963984540054 * standard_error)
    slope = sum_products / sum_x_squared
    intercept = y_mean - slope * x_mean
    return {
        "n": count,
        "r": r_value,
        "t": t_value,
        "df": degrees_freedom,
        "p": p_value,
        "ci_low": ci_low,
        "ci_high": ci_high,
        "slope": slope,
        "intercept": intercept,
    }


def one_way_anova(rows: list[dict[str, object]]) -> dict[str, object]:
    group_values: dict[str, list[float]] = defaultdict(list)
    for row in rows:
        group_values[str(row["reading_time_group"])].append(float(row["vocabulary_score"]))

    all_values = [float(row["vocabulary_score"]) for row in rows]
    grand_mean = mean(all_values)
    group_summaries = []
    ss_between = 0.0
    ss_within = 0.0
    for group in GROUPS:
        values = group_values[group]
        group_mean = mean(values)
        ss_between += len(values) * (group_mean - grand_mean) ** 2
        ss_within += sum((value - group_mean) ** 2 for value in values)
        group_summaries.append(
            {
                "group": group,
                "n": len(values),
                "mean": group_mean,
                "sd": sample_sd(values),
                "median": median(values),
                "minimum": min(values),
                "maximum": max(values),
            }
        )
    numerator_df = len(GROUPS) - 1
    denominator_df = len(all_values) - len(GROUPS)
    ms_between = ss_between / numerator_df
    ms_within = ss_within / denominator_df
    f_value = ms_between / ms_within
    ss_total = ss_between + ss_within
    return {
        "groups": group_summaries,
        "ss_between": ss_between,
        "ss_within": ss_within,
        "ss_total": ss_total,
        "df_between": numerator_df,
        "df_within": denominator_df,
        "ms_between": ms_between,
        "ms_within": ms_within,
        "f": f_value,
        "p": f_survival_probability(f_value, numerator_df, denominator_df),
        "eta_squared": ss_between / ss_total,
    }


def transition_summary() -> dict[str, object]:
    rows = [
        {
            "child_id": child_id,
            "shared_reading_minutes": minutes,
            "reading_time_group": group,
            "vocabulary_score": score,
        }
        for child_id, minutes, group, score in TRANSITION_ROWS
    ]
    return {
        "anova": one_way_anova(rows),
        "correlation": correlation_analysis(rows),
    }


def practical_rows() -> list[dict[str, object]]:
    rng = random.Random(8102026)
    ranges = {
        "Λίγος χρόνος": (25, 70),
        "Μέτριος χρόνος": (75, 125),
        "Πολύς χρόνος": (130, 190),
    }
    rows = []
    child_number = 1
    for group in GROUPS:
        lower, upper = ranges[group]
        for _ in range(20):
            minutes = round(rng.uniform(lower, upper))
            score = 15.0 + 0.105 * minutes + rng.gauss(0.0, 4.2)
            score = round(min(40.0, max(0.0, score)), 1)
            rows.append(
                {
                    "child_id": f"Π{child_number:02d}",
                    "shared_reading_minutes": minutes,
                    "reading_time_group": group,
                    "vocabulary_score": score,
                }
            )
            child_number += 1
    rng.shuffle(rows)
    return rows


def write_csv(path: Path, rows: list[dict[str, object]]) -> None:
    with path.open("w", encoding="utf-8-sig", newline="") as file:
        writer = csv.DictWriter(
            f=file,
            fieldnames=[
                "child_id",
                "shared_reading_minutes",
                "reading_time_group",
                "vocabulary_score",
            ],
            lineterminator="\n",
        )
        writer.writeheader()
        writer.writerows(rows)


def svg_document(width: int, height: int, body: str) -> str:
    return f'''<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}">
<style>
text {{ font-family: {FONT}; fill: {TEXT}; }}
.axis {{ stroke: {NAVY}; stroke-width: 1.5; }}
.grid {{ stroke: {GRID}; stroke-width: 1; }}
.label {{ font-size: 17px; }}
.tick {{ font-size: 14px; fill: #596779; }}
.title {{ font-size: 18px; font-weight: 600; fill: {NAVY}; }}
</style>{body}</svg>'''


def point(cx: float, cy: float, color: str, radius: float = 5.0, opacity: float = 0.8) -> str:
    return f'<circle cx="{cx:.1f}" cy="{cy:.1f}" r="{radius}" fill="{color}" fill-opacity="{opacity}"/>'


def text(x: float, y: float, value: str, css_class: str, anchor: str = "start") -> str:
    return f'<text x="{x:.1f}" y="{y:.1f}" class="{css_class}" text-anchor="{anchor}">{escape(value)}</text>'


def scatter_figure(
    rows: list[dict[str, object]],
    filename: str,
    title: str,
    show_line: bool,
) -> None:
    width, height = 1060, 520
    left, right, top, bottom = 100, 55, 55, 75
    plot_width = width - left - right
    plot_height = height - top - bottom
    x_min, x_max = 0, 210
    y_min, y_max = 0, 40

    def x_position(value: float) -> float:
        return left + (value - x_min) / (x_max - x_min) * plot_width

    def y_position(value: float) -> float:
        return height - bottom - (value - y_min) / (y_max - y_min) * plot_height

    pieces = [text(left, 28, title, "title")]
    for value in range(0, 41, 10):
        y_value = y_position(value)
        pieces.append(f'<line x1="{left}" y1="{y_value:.1f}" x2="{width - right}" y2="{y_value:.1f}" class="grid"/>')
        pieces.append(text(left - 12, y_value + 5, str(value), "tick", "end"))
    for value in range(0, 201, 50):
        x_value = x_position(value)
        pieces.append(f'<line x1="{x_value:.1f}" y1="{top}" x2="{x_value:.1f}" y2="{height - bottom}" class="grid"/>')
        pieces.append(text(x_value, height - bottom + 27, str(value), "tick", "middle"))
    pieces.append(f'<line x1="{left}" y1="{height - bottom}" x2="{width - right}" y2="{height - bottom}" class="axis"/>')
    pieces.append(f'<line x1="{left}" y1="{top}" x2="{left}" y2="{height - bottom}" class="axis"/>')
    pieces.append(text(left + plot_width / 2, height - 18, "Χρόνος κοινής ανάγνωσης (λεπτά/εβδομάδα)", "label", "middle"))
    pieces.append(f'<text x="25" y="{top + plot_height / 2:.1f}" class="label" text-anchor="middle" transform="rotate(-90 25 {top + plot_height / 2:.1f})">Βαθμολογία λεξιλογίου (0–40)</text>')

    if show_line:
        analysis = correlation_analysis(rows)
        line_start = float(analysis["intercept"])
        line_end = float(analysis["intercept"]) + float(analysis["slope"]) * 210
        pieces.append(f'<line x1="{x_position(0):.1f}" y1="{y_position(line_start):.1f}" x2="{x_position(210):.1f}" y2="{y_position(line_end):.1f}" stroke="{RED}" stroke-width="3"/>')

    for row in rows:
        group = str(row["reading_time_group"])
        pieces.append(
            point(
                x_position(float(row["shared_reading_minutes"])),
                y_position(float(row["vocabulary_score"])),
                GROUP_COLORS[group],
            )
        )
    if show_line:
        pieces.append(f'<line x1="{width - 272}" y1="{top + 18}" x2="{width - 242}" y2="{top + 18}" stroke="{RED}" stroke-width="3"/>')
        pieces.append(text(width - 235, top + 23, "γραμμή τάσης", "tick"))
    FIGURES.joinpath(filename).write_text(svg_document(width, height, "".join(pieces)), encoding="utf-8")


def group_figure(rows: list[dict[str, object]], filename: str, title: str) -> None:
    width, height = 1060, 500
    left, right, top, bottom = 100, 40, 55, 80
    plot_width = width - left - right
    plot_height = height - top - bottom
    grouped: dict[str, list[dict[str, object]]] = defaultdict(list)
    for row in rows:
        grouped[str(row["reading_time_group"])].append(row)

    def y_position(value: float) -> float:
        return height - bottom - value / 40 * plot_height

    pieces = [text(left, 28, title, "title")]
    for value in range(0, 41, 10):
        y_value = y_position(value)
        pieces.append(f'<line x1="{left}" y1="{y_value:.1f}" x2="{width - right}" y2="{y_value:.1f}" class="grid"/>')
        pieces.append(text(left - 12, y_value + 5, str(value), "tick", "end"))
    pieces.append(f'<line x1="{left}" y1="{height - bottom}" x2="{width - right}" y2="{height - bottom}" class="axis"/>')
    pieces.append(f'<line x1="{left}" y1="{top}" x2="{left}" y2="{height - bottom}" class="axis"/>')
    pieces.append(f'<text x="25" y="{top + plot_height / 2:.1f}" class="label" text-anchor="middle" transform="rotate(-90 25 {top + plot_height / 2:.1f})">Βαθμολογία λεξιλογίου (0–40)</text>')
    jitter = random.Random(81)
    for index, group in enumerate(GROUPS):
        x_value = left + (index + 0.5) * plot_width / 3
        values = [float(row["vocabulary_score"]) for row in grouped[group]]
        for value in values:
            pieces.append(point(x_value + jitter.uniform(-42, 42), y_position(value), GROUP_COLORS[group], 4.5, 0.72))
        group_mean = mean(values)
        y_mean = y_position(group_mean)
        pieces.append(f'<line x1="{x_value - 48:.1f}" y1="{y_mean:.1f}" x2="{x_value + 48:.1f}" y2="{y_mean:.1f}" stroke="{RED}" stroke-width="4"/>')
        pieces.append(f'<path d="M {x_value:.1f} {y_mean - 8:.1f} L {x_value + 8:.1f} {y_mean:.1f} L {x_value:.1f} {y_mean + 8:.1f} L {x_value - 8:.1f} {y_mean:.1f} Z" fill="{RED}"/>')
        label = group.replace(" χρόνος", "")
        pieces.append(text(x_value, height - bottom + 28, label, "label", "middle"))
    pieces.append(text(width - 45, top + 18, "κόκκινο: μέσος", "tick", "end"))
    FIGURES.joinpath(filename).write_text(svg_document(width, height, "".join(pieces)), encoding="utf-8")


def pattern_figure() -> None:
    width, height = 1120, 520
    panels = [
        ("Θετική", [(1, 8), (2, 7), (3, 6), (4, 5), (5, 5), (6, 3), (7, 3), (8, 2)]),
        ("Αρνητική", [(1, 2), (2, 3), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7), (8, 8)]),
        ("Χωρίς γραμμικό μοτίβο", [(1, 3), (2, 7), (3, 5), (4, 2), (5, 6), (6, 4), (7, 8), (8, 3)]),
        ("Καμπύλη σχέση", [(1, 8), (2, 5), (3, 3), (4, 2), (5, 2), (6, 3), (7, 5), (8, 8)]),
    ]
    pieces = [text(55, 28, "Μοτίβα σε διαγράμματα διασποράς", "title")]
    for index, (label, values) in enumerate(panels):
        left = 50 + index * 270
        top = 70
        right = left + 215
        bottom = 430
        pieces.append(f'<rect x="{left}" y="{top}" width="215" height="360" fill="#f8fafc" stroke="{GRID}"/>')
        pieces.append(f'<line x1="{left + 28}" y1="{bottom - 25}" x2="{right - 18}" y2="{bottom - 25}" class="axis"/>')
        pieces.append(f'<line x1="{left + 28}" y1="{top + 25}" x2="{left + 28}" y2="{bottom - 25}" class="axis"/>')
        pieces.append(text((left + right) / 2, 468, label, "label", "middle"))
        for x_value, y_value in values:
            x_position = left + 28 + (x_value - 1) / 7 * 164
            y_position = bottom - 25 - (y_value - 1) / 7 * 275
            pieces.append(point(x_position, y_position, BLUE, 5.2))
    FIGURES.joinpath("03-patterns.svg").write_text(svg_document(width, height, "".join(pieces)), encoding="utf-8")


def strength_figure() -> None:
    width, height = 1120, 440
    panels = [
        ("Πιο διάσπαρτη θετική σχέση", [7, 3, 8, 4, 8, 5, 7, 6]),
        ("Πιο συνεπής θετική σχέση", [8, 7, 7, 6, 5, 4, 3, 2]),
    ]
    pieces = [text(55, 28, "Η ένταση αφορά τη συγκέντρωση των σημείων γύρω από μια γραμμή", "title")]
    for index, (label, y_values) in enumerate(panels):
        left = 85 + index * 530
        top, right, bottom = 70, left + 390, 360
        pieces.append(f'<line x1="{left}" y1="{bottom}" x2="{right}" y2="{bottom}" class="axis"/>')
        pieces.append(f'<line x1="{left}" y1="{top}" x2="{left}" y2="{bottom}" class="axis"/>')
        pieces.append(text((left + right) / 2, 405, label, "label", "middle"))
        for point_index, y_value in enumerate(y_values, start=1):
            x_position = left + 28 + (point_index - 1) / 7 * 330
            y_position = bottom - 22 - (y_value - 1) / 7 * 220
            pieces.append(point(x_position, y_position, BLUE, 6.0))
    FIGURES.joinpath("04-strength.svg").write_text(svg_document(width, height, "".join(pieces)), encoding="utf-8")


def create_figures(practical: list[dict[str, object]]) -> None:
    transition = [
        {
            "child_id": child_id,
            "shared_reading_minutes": minutes,
            "reading_time_group": group,
            "vocabulary_score": score,
        }
        for child_id, minutes, group, score in TRANSITION_ROWS
    ]
    group_figure(transition, "01-anova-groups.svg", "Η ίδια μελέτη ως τρεις ομάδες")
    scatter_figure(transition, "02-transition-scatter.svg", "Οι αρχικές ποσοτικές τιμές των ίδιων 15 παιδιών", False)
    pattern_figure()
    strength_figure()
    group_figure(practical, "05-practical-groups.svg", "Πρακτική άσκηση: βαθμολογία λεξιλογίου ανά ομάδα χρόνου")
    scatter_figure(practical, "06-practical-scatter.svg", "Πρακτική άσκηση: χρόνος κοινής ανάγνωσης και λεξιλόγιο", True)


def main() -> None:
    FIGURES.mkdir(exist_ok=True)
    transition = [
        {
            "child_id": child_id,
            "shared_reading_minutes": minutes,
            "reading_time_group": group,
            "vocabulary_score": score,
        }
        for child_id, minutes, group, score in TRANSITION_ROWS
    ]
    practical = practical_rows()
    write_csv(ROOT / "transition_demo.csv", transition)
    write_csv(ROOT / "shared_reading_correlation.csv", practical)
    results = {
        "transition_demo": transition_summary(),
        "practical": {
            "anova": one_way_anova(practical),
            "correlation": correlation_analysis(practical),
        },
    }
    (ROOT / "results.json").write_text(
        json.dumps(results, ensure_ascii=False, indent=2),
        encoding="utf-8",
    )
    create_figures(practical)
    practical_correlation = results["practical"]["correlation"]
    practical_anova = results["practical"]["anova"]
    assert len(practical) == 60
    assert all(0.0 <= float(row["vocabulary_score"]) <= 40.0 for row in practical)
    assert 0.55 < float(practical_correlation["r"]) < 0.90
    assert float(practical_anova["p"]) < 0.05
    print(json.dumps(results, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
