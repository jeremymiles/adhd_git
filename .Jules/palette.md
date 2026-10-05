## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add alt text to ggplot2 visualizations
**Learning:** Screen readers cannot interpret data visualizations without alt text, rendering ggplot2 plots inaccessible. Using `labs(alt = ...)` (with dynamically generated text via `paste()` in loops if necessary) ensures that non-visual users can understand the insights conveyed by the charts.
**Action:** Always add descriptive `alt` attributes via `labs()` to all ggplot2 plots, especially within Quarto documents and R scripts.
