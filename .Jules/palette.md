## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Ensure accessible plots with alt text in ggplot2
**Learning:** Screen reader users miss out on crucial data visualization insights when `ggplot2` charts in RMarkdown/Quarto files lack alternative text descriptions. Plots often convey complex trends (e.g., sensitivity vs. specificity) that need text summarization.
**Action:** Always add descriptive `alt` text to `ggplot2` visualizations using the `labs(alt = "description")` function. If generating plots programmatically in a loop, dynamically generate the `alt` text using `paste()` to ensure the description matches the specific chart being rendered.
