## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Ensure dynamic alt text for accessible generated plots in loops
**Learning:** When using `ggplot2` inside a `for` loop to generate multiple related plots, hardcoded static `alt` text causes screen readers to redundantly announce the exact same description for each distinct image, frustrating and confusing users navigating the document.
**Action:** Always dynamically generate the `alt` text in `labs(alt = ...)` (e.g., using `paste()` or `glue::glue()`) to reflect the specific context or data slice of that iteration, ensuring screen reader users receive accurate and distinct context for every plot.
