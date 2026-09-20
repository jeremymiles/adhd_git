## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add dynamic `alt` text for screen readers to R Quarto data visualizations
**Learning:** Automatically generated plots in Quarto (using `ggplot2`) are completely inaccessible to screen reader users unless an `alt` attribute is explicitly provided in the `labs()` function.
**Action:** When adding or modifying data visualizations in `.qmd` or `.R` files, always include descriptive `alt` text within the `labs(alt = "...")` call. If generating plots iteratively, construct dynamic `alt` text using `paste()` to ensure screen reader users receive meaningful context for each distinct figure.
