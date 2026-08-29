## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2026-08-29 - Add alt text to ggplot2 charts
**Learning:** When dynamically generating multiple `ggplot2` visualizations (especially in automated loops or multifaceted data narratives), screen readers cannot interpret the visual data points without a textual description.
**Action:** Always dynamically generate and include an `alt` text argument within the `labs(alt = ...)` function to describe the visualization intent or findings.
