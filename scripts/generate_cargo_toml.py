from pathlib import Path

RUST_DIR = Path("archive/r/rust")
OUTPUT_FILE = Path("Cargo.toml")

CARGO_HEADER = """[package]
name = "sample-programs"
version = "0.1.0"
edition = "2024"
"""

BIN_TEMPLATE = """
[[bin]]
name = "{name}"
path = "{path}"
"""


def generate_cargo_toml():
    """Generates a Cargo.toml file mapping all .rs files to binary targets."""

    if not RUST_DIR.exists():
        raise FileNotFoundError(f"Rust directory not found: {RUST_DIR}")

    lines = [CARGO_HEADER]
    rust_files = sorted(RUST_DIR.glob("*.rs"))
    template = BIN_TEMPLATE

    for rs_file in rust_files:
        lines.append(
            template.format(
                name=rs_file.stem,
                path=rs_file.as_posix(),
            )
        )

    OUTPUT_FILE.write_text("".join(lines), encoding="utf-8")


if __name__ == "__main__":
    generate_cargo_toml()
