## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-06-03 - Add dynamic alternative text to plots inside loops
**Learning:** Generating plots within a loop without dynamically assigned `alt` text can lead to either missing or duplicated descriptions for all visual iterations, rendering the generated document inaccessible for screen reader users relying on distinct context.
**Action:** When generating `ggplot2` plots inside loops in R Markdown/Quarto, always include `alt = ...` within `labs()` and construct a dynamic string (e.g. `paste()`) that includes the looping variable to ensure unique, contextual descriptions.
