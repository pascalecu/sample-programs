import argparse
import re
import subprocess
from dataclasses import dataclass
from pathlib import Path

import yaml

TEST_INFO_DIR = {
    "c": "c",
    "cpp": "c-plus-plus",
    "java": "java",
    "kotlin": "kotlin",
    "swift": "swift",
}

SOURCE_NAME_PATTERN = re.compile(r"\{\{\s*source\.name\s*\}\}")
SOURCE_EXTENSION_PATTERN = re.compile(r"\{\{\s*source\.extension\s*\}\}")


@dataclass(frozen=True)
class TestInfoStruct:
    path: Path
    language: str
    build: str


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("language", help="language to build")
    parser.add_argument("files_changed", nargs="*", help="files that have changed")
    args = parser.parse_args()

    testinfo = get_test_info_struct(args.language)

    for file in args.files_changed:
        path = Path(file)
        print(f"Building {path}")

        command = get_build_command(testinfo, path)
        subprocess.run(command, cwd=path.parent, check=True, shell=True)


def get_test_info_struct(language: str) -> TestInfoStruct:
    try:
        lang_dir = TEST_INFO_DIR[language]
    except KeyError as e:
        raise ValueError(f"Unknown language: {language}") from e

    path = Path("archive") / language[0] / lang_dir / "testinfo.yml"

    with path.open("r", encoding="utf-8") as f:
        data = yaml.safe_load(f)

    return TestInfoStruct(
        path=path,
        language=language,
        build=data["container"]["build"],
    )


def get_build_command(testinfo: TestInfoStruct, path: Path) -> str:
    build = testinfo.build

    build = SOURCE_NAME_PATTERN.sub(path.stem, build)
    build = SOURCE_EXTENSION_PATTERN.sub(path.suffix, build)

    return build


if __name__ == "__main__":
    main()
