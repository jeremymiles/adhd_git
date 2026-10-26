## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Ensure alt texts in R/Quarto visualizations are integrated into labs
**Learning:** When adding accessibility features like alt text to ggplot2 visualizations in R/Quarto, adding a separate labs(alt = "...") at the end can create redundancy or syntax issues if labs() is already present. R visualizations lack web-standard aria-labels out of the box unless explicitly included in output metadata, so properly scoped labs(alt = ...) is critical.
**Action:** Always check if a labs() call already exists in the ggplot2 chain and insert the alt parameter within it rather than appending a new labs() call.
