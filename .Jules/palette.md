## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Add alternative text to ggplot2 visualizations
**Learning:** In Quarto/RMarkdown documents, `ggplot2` plots generated within loops or independently often lack alternative text, making the visual data inaccessible to screen readers. Relying on default behavior or static descriptions for dynamic plots is insufficient.
**Action:** Always add `labs(alt = '...')` directly within `ggplot2::ggplot()` calls to provide alternative text. When generating plots in a loop, dynamically construct the `alt` text using functions like `paste()` to ensure the description accurately reflects the specific iteration's data.
