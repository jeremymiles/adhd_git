## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Screen reader accessibility for dynamically generated ggplot2 visualisations
**Learning:** Generating multiple `ggplot2` visualizations inside a loop in R/Quarto scripts can result in images missing crucial `alt` text or having identical, non-descriptive alt text if hardcoded, severely impacting screen reader accessibility.
**Action:** Always dynamically generate the `alt` text using the loop iterator variables within the `labs(alt = ...)` function (e.g., `alt = paste("Plot for", category)`) to ensure each generated image has a unique, contextually appropriate description for screen readers.
