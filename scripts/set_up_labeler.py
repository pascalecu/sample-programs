"""
Run this to update the labeler workflow:

    poetry run python scripts/set_up_labeler.py

Run whenever a new project is added.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import yaml

GLOTTER_PATH = Path(".glotter.yml")
LABELER_CONFIG_PATH = Path(".github/labeler.yml")


def main() -> None:
    projects = read_projects()
    config = build_labeler_config(projects)

    LABELER_CONFIG_PATH.write_text(
        yaml.safe_dump(config, sort_keys=False),
        encoding="utf-8",
    )


def read_projects() -> list[str]:
    data = yaml.safe_load(GLOTTER_PATH.read_text(encoding="utf-8"))

    return sorted(" ".join(p["words"]) for p in data["projects"].values())


def rule(*conditions: dict[str, Any]) -> list[dict[str, Any]]:
    return [{"all": list(conditions)}]


def changed_files(globs: dict[str, Any]) -> dict[str, Any]:
    return {"changed-files": [globs]}


def base_branch(name: str) -> dict[str, Any]:
    return {"base-branch": name}


def all_files() -> dict[str, Any]:
    return {"all-globs-to-all-files": ["**"]}


def any_files(*patterns: str) -> dict[str, Any]:
    return {"any-glob-to-any-file": list(patterns)}


def build_labeler_config(projects: list[str]) -> dict[str, Any]:
    # Common labels
    config: dict[str, Any] = {
        # Not just language READMEs
        "enhancement": rule(
            changed_files(all_files()),
            base_branch("main"),
        ),
        # README.md and any Markdown file in .github directory
        "needs docs": rule(
            changed_files(any_files("README.md", ".github/*.md")),
            base_branch("main"),
        ),
        # Any language testinfo.yml files
        "tests": rule(
            changed_files(any_files("archive/*/*/testinfo.yml")),
            base_branch("main"),
        ),
    }

    # Add project-specific labels
    for project in projects:
        # Project-specific language files (with all possible variations)
        words = project.split()
        filenames = sorted(
            {
                camel_case(words),
                snake_case(words),
                kebab_case(words),
                pascal_case(words),
            }
        )

        config[project] = rule(
            changed_files(any_files(*[f"archive/*/*/{name}.*" for name in filenames])),
            base_branch("main"),
        )

    return config


def camel_case(words: list[str]) -> str:
    return words[0] + "".join(w.title() for w in words[1:])


def snake_case(words: list[str]) -> str:
    return "_".join(words)


def kebab_case(words: list[str]) -> str:
    return "-".join(words)


def pascal_case(words: list[str]) -> str:
    return "".join(w.title() for w in words)


if __name__ == "__main__":
    main()
