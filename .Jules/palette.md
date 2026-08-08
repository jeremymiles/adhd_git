## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2025-01-28 - Dynamic alt text in ggplot2 loops
**Learning:** When generating multiple `ggplot2` visualizations within a loop in R or Quarto, using a static `alt` text description results in poor screen reader accessibility as all charts share the same label, making them indistinguishable.
**Action:** Always dynamically generate the `alt` text within the `labs(alt = ...)` function (e.g., using `paste()`) based on the loop variable to ensure each generated chart has a unique and descriptive accessibility label.
