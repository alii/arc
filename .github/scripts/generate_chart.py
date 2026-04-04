#!/usr/bin/env python3
"""Generate a test262 conformance line chart from historical data.

Reads .github/test262/history.json (array of {date, pass, fail, skip, tested, percent})
and produces .github/test262/conformance.png.

Usage:
    python3 .github/scripts/generate_chart.py [--add-result results.json]

If --add-result is given, appends today's result to history before generating.
"""

import argparse
import json
import os
import sys
from datetime import datetime

HISTORY_FILE = os.path.join(os.path.dirname(__file__), "..", "test262", "history.json")
CHART_FILE = os.path.join(os.path.dirname(__file__), "..", "test262", "conformance.png")
CHART_FILE_DARK = os.path.join(os.path.dirname(__file__), "..", "test262", "conformance-dark.png")


def load_history():
    if not os.path.exists(HISTORY_FILE):
        return []
    with open(HISTORY_FILE) as f:
        return json.load(f)


def save_history(history):
    with open(HISTORY_FILE, "w") as f:
        json.dump(history, f, indent=2)
        f.write("\n")


def add_result(history, result_file):
    with open(result_file) as f:
        result = json.load(f)

    today = datetime.utcnow().strftime("%Y-%m-%d")

    entry = {
        "date": today,
        "pass": result["pass"],
        "fail": result["fail"],
        "skip": result["skip"],
        "tested": result["tested"],
        "total": result["total"],
        "percent": float(result["percent"]),
    }

    # Replace today's entry if it already exists, otherwise append
    history = [h for h in history if h["date"] != today]
    history.append(entry)
    history.sort(key=lambda h: h["date"])
    return history


def generate_chart(history):
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
        import matplotlib.dates as mdates
    except ImportError:
        print("matplotlib not installed, skipping chart generation", file=sys.stderr)
        print("Install with: pip install matplotlib", file=sys.stderr)
        return

    if len(history) == 0:
        print("No data to chart", file=sys.stderr)
        return

    dates = [datetime.strptime(h["date"], "%Y-%m-%d") for h in history]
    percents = [h["percent"] for h in history]
    passes = [h["pass"] for h in history]
    latest = history[-1]

    def render(out_file, dark):
        if dark:
            # Rose Pine
            fg = "#e0def4"
            bg = "#191724"
            color_pct = "#9ccfd8"  # foam
            color_pass = "#c4a7e7"  # iris
            grid_color = "#26233a"  # overlay
            grid_alpha = 0.8
        else:
            # Rose Pine Dawn
            fg = "#575279"
            bg = "#faf4ed"
            color_pct = "#56949f"  # foam
            color_pass = "#907aa9"  # iris
            grid_color = "#f2e9e1"  # overlay
            grid_alpha = 0.8

        fig, ax1 = plt.subplots(figsize=(10, 5))
        fig.patch.set_facecolor(bg)
        ax1.set_facecolor(bg)

        ax1.set_xlabel("Date", color=fg)
        ax1.set_ylabel("Conformance %", color=color_pct)
        ax1.plot(dates, percents, color=color_pct, linewidth=2, marker="o" if len(dates) < 30 else None, markersize=5)
        ax1.tick_params(axis="y", labelcolor=color_pct)
        ax1.tick_params(axis="x", colors=fg)
        ax1.set_ylim(bottom=0)
        for spine in ax1.spines.values():
            spine.set_color(fg)

        # Add pass count on right axis
        ax2 = ax1.twinx()
        ax2.set_ylabel("Tests passing", color=color_pass)
        ax2.plot(dates, passes, color=color_pass, linewidth=1, linestyle="--", alpha=0.7)
        ax2.tick_params(axis="y", labelcolor=color_pass)
        ax2.set_ylim(bottom=0)
        for spine in ax2.spines.values():
            spine.set_color(fg)

        # Format x-axis dates
        if len(dates) == 1:
            from datetime import timedelta
            ax1.set_xlim(dates[0] - timedelta(days=3), dates[0] + timedelta(days=3))
            ax1.xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d"))
        else:
            ax1.xaxis.set_major_formatter(mdates.DateFormatter("%b %d"))
            ax1.xaxis.set_major_locator(mdates.AutoDateLocator())
            fig.autofmt_xdate()

        ax1.set_title(
            f"Arc test262 Conformance: {latest['percent']}% "
            f"({latest['pass']:,} / {latest['tested']:,} tests)",
            fontsize=14,
            fontweight="bold",
            color=fg,
        )

        ax1.grid(True, alpha=grid_alpha, color=grid_color)
        fig.tight_layout()
        fig.savefig(out_file, dpi=150, bbox_inches="tight", facecolor=bg)
        plt.close(fig)
        print(f"Chart saved to {out_file}")

    render(CHART_FILE, dark=False)
    render(CHART_FILE_DARK, dark=True)


def main():
    parser = argparse.ArgumentParser(description="Generate test262 conformance chart")
    parser.add_argument("--add-result", help="Path to results.json to append")
    args = parser.parse_args()

    history = load_history()

    if args.add_result:
        history = add_result(history, args.add_result)
        save_history(history)
        print(f"Added result to history ({len(history)} entries)")

    generate_chart(history)


if __name__ == "__main__":
    main()
