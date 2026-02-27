from pathlib import Path
from typing import Dict, Tuple, Set
from dataclasses import dataclass
import re
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

@dataclass
class AnalysisConfig:
    evaluation_dir: Path = Path("evaluation")
    efficient_prefix: str = "selective-experiment"
    inefficient_prefix: str = "broadcasting-experiment"
    output_chart_filename: str = "communication-overhead.pdf"
    dpi: int = 300
    
    # Plot styling
    figure_size: Tuple[float, float] = (6, 4)
    bar_width: float = 0.35
    efficient_color: str = "#24D45F"
    inefficient_color: str = "#D3177B"
    font_family: str = "serif"
    font_size: int = 12

def discover_worker_counts(config: AnalysisConfig) -> Set[int]:
    """
    Discover the number of workers used in the experiments by scanning the evaluation directory for CSV files.
    """
    workers: Set[int] = set()
    if not config.evaluation_dir.exists():
        print(f"Warning: Directory {config.evaluation_dir} does not exist")
        return workers
    # Pattern to match CSV files: `selective-experiment-N-workers.csv` or `broadcasting-experiment-N-workers.csv`
    pattern = re.compile(rf"(?:{config.efficient_prefix}|{config.inefficient_prefix})-(\d+)-workers\.csv")
    for file in config.evaluation_dir.iterdir():
        if file.is_file() and file.suffix == '.csv':
            match = pattern.match(file.name)
            if match:
                worker_count = int(match.group(1))
                workers.add(worker_count)
    return workers

def compute_total_sent_bytes(csv_file: Path) -> float:
    """
    Calculate total bytes sent from a CSV file.
    """
    df = pd.read_csv(csv_file)
    if "operation" not in df.columns or "payload_size" not in df.columns:
        print(f"  Warning: Missing required columns in {csv_file.name}")
        return 0.0
    total_bytes = df.loc[df["operation"] == "send", "payload_size"].sum()
    return total_bytes / 1024


def process_experiment_type(
    config: AnalysisConfig,
    prefix: str,
    worker_counts: Set[int]
) -> Dict[int, float]:
    """
    Process all experiments of a given type (selective vs. broadcasting).
    """
    results: Dict[int, float] = {}    
    for n_workers in sorted(worker_counts):
        csv_filename = f"{prefix}-{n_workers}-workers.csv"
        csv_path = config.evaluation_dir / csv_filename
        try:
            total_kb = compute_total_sent_bytes(csv_path)
            results[n_workers] = total_kb
        except FileNotFoundError:
            print(f"Workers: {n_workers:2d} | File not found, skipping...")
            continue
    return results


def configure_plot_style(config: AnalysisConfig) -> None:
    plt.rcParams.update({
        "text.usetex": True,
        "text.latex.preamble": r"\usepackage[T1]{fontenc}",
        "font.family": config.font_family,
        "font.serif": ["Computer Modern Roman", "Times New Roman", "DejaVu Serif"],
        "font.size": config.font_size,
        "axes.labelsize": config.font_size,
        "xtick.labelsize": config.font_size - 1,
        "ytick.labelsize": config.font_size - 1,
        "legend.fontsize": config.font_size - 1,
        "savefig.dpi": config.dpi,
        "savefig.bbox": "tight",
        "savefig.pad_inches": 0.1,
        "axes.edgecolor": "#333333",
        "axes.linewidth": 0.8,
        "legend.frameon": True,
        "legend.framealpha": 0.9,
        "legend.edgecolor": "#CCCCCC",
    })


def create_plot(
    selective_results: Dict[int, float],
    broadcasting_results: Dict[int, float],
    config: AnalysisConfig
) -> plt.Figure:
    """
    Create the side-by-side bar chart comparing selective vs broadcasting approaches.
    """
    # Prepare data
    workers = sorted(set(selective_results.keys()) | set(broadcasting_results.keys()))
    selective_values = [selective_results.get(w, 0) for w in workers]
    broadcasting_values = [broadcasting_results.get(w, 0) for w in workers]
    # Create figure
    fig, ax = plt.subplots(figsize=config.figure_size)
    # Bar positions
    x_positions = np.arange(len(workers))
    width = config.bar_width
    # Create bars
    bars_selective = ax.bar(
        x_positions - width/2,
        selective_values,
        width,
        label='Using selective communication primitives',
        color=config.efficient_color,
        edgecolor='black',
        linewidth=0.5,
        alpha=0.9
    )
    bars_broadcasting = ax.bar(
        x_positions + width/2,
        broadcasting_values,
        width,
        label='Using broadcasting communication primitives',
        color=config.inefficient_color,
        edgecolor='black',
        linewidth=0.5,
        alpha=0.9
    )
    # Add value labels on bars
    _add_bar_labels(ax, bars_selective, fontsize=8)
    _add_bar_labels(ax, bars_broadcasting, fontsize=8)
    # Configure axes
    ax.set_xlabel('Number of Workers', fontweight='normal')
    ax.set_ylabel('Data sent by the Master (KB)', fontweight='normal')
    # X-axis ticks
    ax.set_xticks(x_positions)
    ax.set_xticklabels(workers)
    # Legend
    ax.legend(loc='upper left', framealpha=0.95)
    # Grid
    ax.grid(axis='y', alpha=0.3, linestyle='--', linewidth=0.5)
    ax.set_axisbelow(True)
    # Y-axis formatting
    ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'{x:,.2f}'))
    # Tight layout
    plt.tight_layout()
    return fig


def _add_bar_labels(ax: plt.Axes, bars, fontsize: int = 8) -> None:
    for bar in bars:
        height = bar.get_height()
        if height > 0:
            ax.text(
                bar.get_x() + bar.get_width() / 2.0,
                height,
                f'{height:,.2f}',
                ha='center',
                va='bottom',
                fontsize=fontsize,
            )

def main() -> None:
    config = AnalysisConfig()
    workers_per_experiment = discover_worker_counts(config)
    if not workers_per_experiment:
        return
    configure_plot_style(config)
    # Process experiments
    selective_results = process_experiment_type(
        config,
        config.efficient_prefix,
        workers_per_experiment
    )
    broadcasting_results = process_experiment_type(
        config,
        config.inefficient_prefix,
        workers_per_experiment
    )
    if not selective_results and not broadcasting_results:
        print("\nError: No data found for any experiments.")
        return
    fig = create_plot(selective_results, broadcasting_results, config)
    # Save figure
    output_path = config.evaluation_dir / config.output_chart_filename
    output_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(output_path, dpi=config.dpi, bbox_inches='tight')
    print(f"✓ Figure saved: {output_path}")
    print(f"  Resolution: {config.dpi} DPI")
    print(f"  Size: {config.figure_size[0]}\" × {config.figure_size[1]}\"")
    plt.close(fig)

if __name__ == "__main__":
    main()