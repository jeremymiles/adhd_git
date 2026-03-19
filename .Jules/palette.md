## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).

## 2024-10-24 - Avoid default basic colors in Metafor forest plots
**Learning:** Hardcoded colors like `red` in forest plot functions (e.g., `metafor::addpoly`) can create contrast and accessibility issues for visually impaired users compared to colorblind-friendly alternatives.
**Action:** Ensure custom polygons and highlights in forest plots use accessible colors like the Okabe-Ito palette (e.g., `#D55E00` instead of `red`).