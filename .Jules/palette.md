## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Accessible polygon colors in Metafor forest plots
**Learning:** Hardcoded basic colors like "red" for the summary polygons (`addpoly`) in `metafor` forest plots can cause issues for colorblind users and fail accessibility contrast checks.
**Action:** Use an accessible colorblind-friendly hex code (e.g., `#D55E00` for Okabe-Ito vermilion) instead of basic strings when highlighting summary effects in R plot methods like `addpoly`.
