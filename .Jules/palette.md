## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2026-04-04 - Add alt text to ggplot visualizations in Quarto
**Learning:** Screen readers cannot interpret data visualizations in Quarto documents automatically. Without explicit `alt` text, users relying on assistive technologies miss out on crucial data insights.
**Action:** Always provide descriptive alternative text for `ggplot2` visualizations using the `labs(alt = "...")` function. When rendering plots within loops, dynamically generate the alt text (e.g., using `paste()`) to reflect the specific data being plotted.
