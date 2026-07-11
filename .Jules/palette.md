## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Dynamic alt text for loop-generated visualizations
**Learning:** When generating multiple ggplot2 visualizations within a loop (common in this app's reports), static `alt` text causes screen readers to announce identical descriptions for different charts, confusing visually impaired users.
**Action:** Always dynamically generate the `alt` text within the `labs(alt = ...)` function (e.g., using `paste()`) to provide context-specific descriptions for each generated plot.
