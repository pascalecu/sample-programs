import argparse
import json
import re
from collections import defaultdict
from dataclasses import dataclass, field
from fnmatch import translate
from pathlib import Path

LINUX, MACOS = "ubuntu-latest", "macos-latest"


@dataclass(frozen=True)
class LanguageConfig:
    language: str
    build_mode: str = "none"
    os: str = LINUX

    def to_dict(self, paths: set[str]) -> dict:
        return {
            "language": self.language,
            "build-mode": self.build_mode,
            "os": self.os,
            "paths": " ".join(sorted(paths)),
        }


LANG_MAPPING = {
    "scripts/*.py": LanguageConfig("python"),
    "archive/c/c/*.c": LanguageConfig("c", "manual"),
    "archive/c/c-plus-plus/*.cpp": LanguageConfig("cpp", "manual"),
    "archive/c/c-sharp/*.cs": LanguageConfig("c#"),
    "archive/g/go/*.go": LanguageConfig("go", "autobuild"),
    "archive/j/java/*.java": LanguageConfig("java", "manual"),
    "archive/j/javascript/*.js": LanguageConfig("javascript"),
    "archive/k/kotlin/*.kt": LanguageConfig("kotlin", "manual"),
    "archive/p/python/*.py": LanguageConfig("python"),
    "archive/r/ruby/*.rb": LanguageConfig("ruby"),
    "archive/r/rust/*.rs": LanguageConfig("rust"),
    "archive/t/typescript/*.ts": LanguageConfig("typescript"),
    "archive/s/swift/*.swift": LanguageConfig("swift", "manual", MACOS),
    ".github/workflows/*.yml": LanguageConfig("actions"),
}

CORE_FILES = {
    ".github/workflows/codeql-analysis.yml",
    "repo-config.yml",
    "scripts/get_codeql_languages.py",
    "scripts/build_codeql_language.py",
    "pyproject.toml",
    "poetry.lock",
}

# Create a reverse map for testinfo.yml and a combined regex for path matching
TESTINFO_MAP = {
    str(Path(g).parent / "testinfo.yml"): (g, c) for g, c in LANG_MAPPING.items()
}

# We use named groups (?P<g0>...) to identify which glob matched in the combined regex
GROUP_TO_CONFIG = {}
regex_parts = []
for i, (glob, config) in enumerate(LANG_MAPPING.items()):
    group_name = f"g{i}"
    GROUP_TO_CONFIG[group_name] = (glob, config)
    regex_parts.append(f"(?P<{group_name}>{translate(glob)})")

COMBINED_REGEX = re.compile("|".join(regex_parts))
RELEVANT_PREFIXES = tuple(set(g.split("/")[0] for g in LANG_MAPPING if "/" in g))


def get_matched_configs(event: str, files: set[str]) -> dict[LanguageConfig, set[str]]:
    """Determines which language configurations are affected by changed files."""
    matched = defaultdict(set)

    # Full run check
    if event == "schedule" or not CORE_FILES.isdisjoint(files):
        for glob, config in LANG_MAPPING.items():
            matched[config].add(glob)
        return matched

    # Targeted match
    for file_path in files:
        # Quick check: is this file even in a directory we care about?
        if not file_path.startswith(RELEVANT_PREFIXES) and not file_path.startswith(
            ".github"
        ):
            continue

        # Strategy A: testinfo.yml direct lookup
        if file_path in TESTINFO_MAP:
            glob, config = TESTINFO_MAP[file_path]
            matched[config].add(glob)
            continue

        # Strategy B: single-pass regex match
        match = COMBINED_REGEX.match(file_path)
        if match:
            # lastgroup gives us the name of the group that actually matched
            glob, config = GROUP_TO_CONFIG[match.lastgroup]
            matched[config].add(glob)

    return matched


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--event", help="GitHub event name")
    parser.add_argument("files_changed", nargs="*", help="List of changed file paths")
    args = parser.parse_args()

    # Use a set for O(1) lookups
    matched = get_matched_configs(args.event, set(args.files_changed))

    output = sorted(
        [config.to_dict(paths) for config, paths in matched.items()],
        key=lambda x: x["language"],
    )

    print(json.dumps(output))


if __name__ == "__main__":
    main()
