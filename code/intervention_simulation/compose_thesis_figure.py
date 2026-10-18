"""Assemble the approved A panel and the original B layout; create print PDF.

Requires Pillow and reportlab. Called by 05_reproduce_thesis_figures.R.
"""
from pathlib import Path
import sys
from PIL import Image
from reportlab.pdfgen import canvas
from reportlab.lib.utils import ImageReader

build, output = map(Path, sys.argv[1:3])
a = Image.open(build / 'A-approved.png').convert('RGB')
combined = Image.open(build / 'combined-B.png').convert('RGB')
assert a.size == (1650, 1275) and combined.size == (3300, 5100)
# Retain every approved A pixel, doubled without interpolation. The B half
# is the original combined-plot layout, with its title updated in R.
combined.paste(a.resize((3300, 2550), Image.Resampling.NEAREST), (0, 0))
png = output / 'Figure_6_8_corrected.png'
combined.save(png, dpi=(300, 300))
pdf = canvas.Canvas(str(output / 'Figure_6_8_corrected.pdf'), pagesize=(792, 1224))
pdf.setTitle('Figure 6.8 - corrected calibration, noise multiplier 0.3')
pdf.drawImage(ImageReader(combined), 0, 0, width=792, height=1224)
pdf.showPage()
pdf.save()
