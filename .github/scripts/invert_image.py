#!/usr/bin/env python3
"""Invert an image's colors to produce a light-mode variant.

Usage:
    python3 .github/scripts/invert_image.py .github/js.png .github/js-light.png
"""

import sys
from PIL import Image, ImageOps


def main():
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <input.png> <output.png>", file=sys.stderr)
        sys.exit(1)

    src, dst = sys.argv[1], sys.argv[2]
    img = Image.open(src).convert("RGBA")

    # ImageOps.invert doesn't handle alpha, so split it off and reattach.
    rgb, alpha = img.convert("RGB"), img.getchannel("A")
    inverted = ImageOps.invert(rgb)
    inverted.putalpha(alpha)

    inverted.save(dst)
    print(f"Wrote {dst}")


if __name__ == "__main__":
    main()
