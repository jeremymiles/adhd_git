## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Improve accessibility of metafor forest plots
**Learning:** Hardcoded basic colors like "red" for polygons in metafor library forest plots can reduce accessibility for users with color vision deficiencies.
**Action:** Replace basic hardcoded colors with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#D55E00`) in `addpoly` calls for forest plots to ensure maximum accessibility and readability.
