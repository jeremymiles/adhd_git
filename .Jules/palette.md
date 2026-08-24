## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-24 - Dynamic Alt Text in R Loops
**Learning:** Generating multiple visualizations within a loop (e.g., in R or Quarto) requires dynamically constructed alternative text to ensure each plot has an accurate, screen-reader-accessible description, as static text will be incorrect for all but one iteration.
**Action:** Always dynamically generate `alt` text within the `labs(alt = ...)` function (e.g., using `paste()` or `glue::glue()`) when rendering plots within a loop.
