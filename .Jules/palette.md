## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-24 - Add alt text to ggplot2 visualizations
**Learning:** Data visualizations without alternative text (alt text) are inaccessible to screen reader users, preventing them from understanding the relationships, patterns, and insights being conveyed in charts and plots.
**Action:** Always include an `alt` argument within the `labs()` function for all ggplot2 visualizations to provide descriptive text for screen readers (e.g., `labs(alt = "Scatter plot showing...")`).
