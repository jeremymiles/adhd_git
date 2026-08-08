## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add alt text to ggplot2 charts
**Learning:** Screen readers and other assistive technologies cannot interpret the contents of dynamically generated plots, such as those made with `ggplot2`, leaving visually impaired users without context.
**Action:** When creating `ggplot2` plots in RMarkdown/Quarto, use `labs(alt = "descriptive text")` to add descriptive alternative text to generated plot images. Ensure the text accurately describes the plot's content and structure.
