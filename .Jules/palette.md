## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).

## 2024-05-15 - Add alt text to data visualizations
**Learning:** Complex data visualizations generated with `ggplot2` in reports or `.qmd` files are completely opaque to screen readers unless informative alternative text is provided describing the plot's structure, variables, and key visual encodings (like color and shape).
**Action:** Always provide descriptive `alt` text to `ggplot2` visualizations using the `labs(alt = "...")` argument to ensure accessibility for visually impaired users consuming the rendered reports.
