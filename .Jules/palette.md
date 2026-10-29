## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - R/Quarto accessible plotting
**Learning:** `ggplot2` calls in R/Quarto allow for alternative text describing the plot (via `labs(alt = "description")`), which is crucial for accessibility. Also, `metafor::forest` uses base R graphics and doesn't accept `alt` text in the same way.
**Action:** When working on data visualizations, ensure accessibility by replacing hardcoded basic colors with accessible colorblind-friendly palettes (like Okabe-Ito), and append `alt` text via `labs(alt = "...")` to `ggplot2` plots whenever possible to describe the visual information to screen readers.
