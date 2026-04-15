## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add dynamic alt text to generated plots in loops
**Learning:** When generating plots iteratively within a loop using ggplot2, a static `alt` text in `labs()` results in repetitive and less informative descriptions for screen readers. In addition, iterating over a non-unique column in R can lead to redundant plot generation and unnecessary computational overhead.
**Action:** Use `paste(...)` inside `labs(alt = ...)` to dynamically generate context-specific alternative text for each plot. Also, ensure the loop iterator uses `unique(df$column)` to process each category exactly once.
