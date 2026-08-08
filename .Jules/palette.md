## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add alt text to ggplot visualizations
**Learning:** Adding alt text to ggplot2 visualizations is a critical accessibility best practice for Quarto/RMarkdown documents. Screen readers rely on alt text to convey the information presented in images.
**Action:** Always include labs(alt = '...') directly within ggplot2::ggplot() calls to add alternative text to generated plot images.
