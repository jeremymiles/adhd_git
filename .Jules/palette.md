## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add dynamic alt text to ggplot2 charts
**Learning:** Generated plots without alt text are inaccessible to screen readers. When generating plots inside a loop, static alt text is insufficient as the context changes.
**Action:** Always add dynamic `alt` text to `labs()` in `ggplot2` (e.g., `labs(alt = paste("Scatter plot of sensitivity vs specificity for", name_1))`) when generating visualizations in loops to improve accessibility for Quarto/RMarkdown documents.
