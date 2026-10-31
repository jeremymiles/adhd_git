## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Add accessible alternative text to R/Quarto visualizations
**Learning:** Screen readers cannot automatically interpret the content of images or plots generated in R/Quarto outputs unless explicit alternative text is provided, severely impacting visually impaired users' ability to understand data visualizations.
**Action:** When adding or modifying data visualizations (like `ggplot2` in R or `.qmd`), always provide concise, descriptive alternative text. For `ggplot2`, add or append the `alt` parameter to the `labs()` function (e.g., `labs(..., alt = "Description of the plot")`).
