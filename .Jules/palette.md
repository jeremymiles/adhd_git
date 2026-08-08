## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add dynamic alt text to generated plots in loops
**Learning:** Generating multiple `ggplot2` plots in a loop without dynamic alt text results in identical or missing descriptions for all plots, which severely limits accessibility for users relying on screen readers.
**Action:** When generating `ggplot2` plots within a loop, always dynamically generate the `alt` text in `labs(alt = ...)` (e.g., using `paste()`) to accurately reflect the specific iteration's data and context.
