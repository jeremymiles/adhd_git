## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add dynamic alt text to ggplot visualizations for screen reader accessibility
**Learning:** Data visualizations generated via ggplot2 in R/Quarto documents are inaccessible to screen readers by default. Relying solely on surrounding text or static captions can leave visually impaired users without full context, particularly in dynamically generated plots (e.g. inside loops).
**Action:** Always dynamically generate and include `alt` text using the `labs(alt = ...)` function in all `ggplot2` chains to ensure comprehensive screen reader accessibility.
