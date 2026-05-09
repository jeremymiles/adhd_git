## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Improve accessibility and performance for loops generating plots
**Learning:** When looping over categories to generate multiple data visualizations, screen readers miss the context if static alt text is used. Furthermore, iterating over all rows rather than unique values causes redundant iterations and significant performance degradation.
**Action:** Use `unique()` in the loop definition to ensure each category is processed only once, and dynamically construct `alt` text in `labs(alt = ...)` (e.g. using `paste()`) to correctly label each generated plot with its specific category.
