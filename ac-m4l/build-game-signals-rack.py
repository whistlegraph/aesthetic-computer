#!/usr/bin/env python3
"""Build the sample-free GAME SIGNALS Drum Rack for Ableton Live 12."""

from __future__ import annotations

import argparse
import copy
import gzip
import io
import json
import shutil
import xml.etree.ElementTree as ET
from pathlib import Path


HERE = Path(__file__).resolve().parent
DEFAULT_TEMPLATE = Path(
    "/Applications/Ableton Live 12 Suite.app/Contents/App-Resources/Core Library/"
    "Racks/Drum Racks/Sampled/MPE Butta Kit.adg"
)
DEFAULT_OUTPUT = HERE / "presets" / "GAME SIGNALS.adg"
DEFAULT_INSTALL = (
    Path.home()
    / "Music/Ableton/User Library/Presets/Instruments/Drum Rack/GAME SIGNALS.adg"
)

# Keep this in the same order as the route object in AC-GameSignals.amxd.json.
SIGNALS = (
    ("move", 36),
    ("jump", 37),
    ("ultrajump", 38),
    ("dash", 39),
    ("fastdrop", 40),
    ("ko", 41),
    ("roundwin", 42),
    ("matchwin", 43),
    ("tie", 44),
    ("kick", 45),
    ("punch", 46),
    ("shield", 47),
    ("block", 48),
    ("wind", 49),
    ("ballserve", 50),
    ("wack", 51),
    ("boot", 52),
    ("crosswack", 53),
    ("ballblock", 54),
    ("balled", 55),
    ("hello", 56),
)


def require(element: ET.Element | None, description: str) -> ET.Element:
    if element is None:
        raise RuntimeError(f"Ableton template is missing {description}")
    return element


def set_value(element: ET.Element | None, value: object, description: str) -> None:
    require(element, description).set("Value", str(value))


def validate_max_note_map() -> None:
    source = json.loads((HERE / "AC-GameSignals.amxd.json").read_text())
    boxes = {entry["box"]["id"]: entry["box"] for entry in source["patcher"]["boxes"]}
    addresses = boxes["route"]["text"].split()[1:]
    routes: dict[int, int] = {}
    for entry in source["patcher"]["lines"]:
        line = entry["patchline"]
        if line["source"][0] == "route":
            routes[int(line["source"][1])] = int(boxes[line["destination"][0]]["text"])
    actual = tuple((address.removeprefix("/oskiewar/"), routes[index])
                   for index, address in enumerate(addresses))
    if actual != SIGNALS:
        raise RuntimeError(f"Max note map drifted: expected {SIGNALS!r}, got {actual!r}")


def blank_drum_cell_preset(branch: ET.Element) -> ET.Element:
    presets = require(branch.find("DevicePresets"), "Drum Rack branch device presets")
    for preset in presets.findall("AbletonDevicePreset"):
        if preset.find("./Device/DrumCell") is not None:
            result = copy.deepcopy(preset)
            cell = require(result.find("./Device/DrumCell"), "DrumCell")
            sample_value = require(cell.find("./UserSample/Value"), "DrumCell sample")
            sample_value.clear()
            return result
    raise RuntimeError("Ableton template branch has no DrumCell preset")


def build(template: Path) -> bytes:
    validate_max_note_map()
    with gzip.open(template, "rb") as source:
        root = ET.fromstring(source.read())

    preset = require(root.find("./GroupDevicePreset"), "GroupDevicePreset")
    drum_group = require(preset.find("./Device/DrumGroupDevice"), "top-level Drum Rack")
    set_value(drum_group.find("UserName"), "GAME SIGNALS", "Drum Rack name")
    set_value(
        drum_group.find("Annotation"),
        "OSC /oskiewar/* to MIDI 36-56. Drop a sample or instrument on each labeled pad.",
        "Drum Rack annotation",
    )
    set_value(drum_group.find("PadScrollPosition"), 9, "Drum Rack pad position")

    branches = require(preset.find("BranchPresets"), "Drum Rack branches")
    prototype = require(branches.find("DrumBranchPreset"), "Drum Rack branch")
    cell_preset = blank_drum_cell_preset(prototype)
    branches.clear()

    for branch_id, (name, note) in enumerate(SIGNALS):
        branch = copy.deepcopy(prototype)
        branch.set("Id", str(branch_id))
        set_value(branch.find("Name"), name.upper(), "branch name")

        device_presets = require(branch.find("DevicePresets"), "branch devices")
        device_presets.clear()
        device_presets.append(copy.deepcopy(cell_preset))
        cell = require(device_presets.find("./AbletonDevicePreset/Device/DrumCell"), "DrumCell")
        set_value(cell.find("UserName"), name.upper(), "DrumCell name")
        set_value(
            cell.find("Annotation"),
            f"/oskiewar/{name} — MIDI note {note}",
            "DrumCell annotation",
        )

        set_value(branch.find("./ZoneSettings/ReceivingNote"), note, "receiving note")
        set_value(branch.find("./ZoneSettings/SendingNote"), note, "sending note")
        set_value(branch.find("./ZoneSettings/ChokeGroup"), 0, "choke group")
        set_value(branch.find("DocumentColorIndex"), 14 + branch_id % 6, "pad color")

        source_context = branch.find("SourceContext")
        if source_context is not None:
            for value in source_context.iter():
                if "Value" in value.attrib:
                    value.set("Value", "")
        branches.append(branch)

    returns = preset.find("ReturnBranchPresets")
    if returns is not None:
        returns.clear()

    # DeviceId identifies the built-in Drum Rack, DrumCell, and branch mixer.
    # Their template FileRefs can therefore be blank, as in other factory racks;
    # remove developer paths and Core Library provenance from the distributable.
    for file_ref in root.findall(".//FileRef"):
        for child in file_ref:
            if child.tag in {"RelativePath", "Path", "LivePackName", "LivePackId", "SourceHint"}:
                child.set("Value", "")
            elif child.tag == "RelativePathType":
                child.set("Value", "0")
            elif child.tag in {"OriginalFileSize", "OriginalCrc"}:
                child.set("Value", "0")

    ET.indent(root, space="\t")
    xml = ET.tostring(root, encoding="utf-8", xml_declaration=True)
    stream = io.BytesIO()
    with gzip.GzipFile(fileobj=stream, mode="wb", compresslevel=9, mtime=0) as output:
        output.write(xml)
    data = stream.getvalue()
    validate_rack(data)
    return data


def validate_rack(data: bytes) -> None:
    root = ET.fromstring(gzip.decompress(data))
    branches = root.findall("./GroupDevicePreset/BranchPresets/DrumBranchPreset")
    actual = tuple(
        (
            require(branch.find("Name"), "branch name").get("Value", "").lower(),
            int(require(branch.find("./ZoneSettings/ReceivingNote"), "receiving note").get("Value", "-1")),
        )
        for branch in branches
    )
    if actual != SIGNALS:
        raise RuntimeError(f"Generated rack map is invalid: {actual!r}")
    for branch in branches:
        devices = require(branch.find("DevicePresets"), "branch devices")
        if len(devices) != 1 or devices.find("./AbletonDevicePreset/Device/DrumCell") is None:
            raise RuntimeError("Each GAME SIGNALS pad must contain exactly one blank DrumCell")
        sample = require(devices.find(".//DrumCell/UserSample/Value"), "DrumCell sample")
        if len(sample) or sample.text:
            raise RuntimeError("Generated GAME SIGNALS rack unexpectedly contains sample data")
    for file_ref in root.findall(".//FileRef"):
        for field in ("RelativePath", "Path", "LivePackName", "LivePackId"):
            value = file_ref.find(field)
            if value is not None and value.get("Value", ""):
                raise RuntimeError(f"Generated GAME SIGNALS rack contains a {field} dependency")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--template", type=Path, default=DEFAULT_TEMPLATE)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--install", action="store_true")
    parser.add_argument("--check", action="store_true", help="validate the existing output")
    args = parser.parse_args()

    if args.check:
        validate_max_note_map()
        validate_rack(args.output.read_bytes())
        print(f"Valid: {args.output}")
        return

    data = build(args.template)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_bytes(data)
    print(f"Built: {args.output} ({len(data)} bytes, {len(SIGNALS)} labeled pads)")
    if args.install:
        DEFAULT_INSTALL.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(args.output, DEFAULT_INSTALL)
        print(f"Installed: {DEFAULT_INSTALL}")


if __name__ == "__main__":
    main()
