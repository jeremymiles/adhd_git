## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).

## 2024-05-24 - Dynamic alt text for iterated plots in Quarto
**Learning:** When generating multiple plots in a loop (e.g., using a `for` loop in R with ggplot2) within a Quarto or RMarkdown document, setting a single static `alt` text attribute creates redundant, less informative accessibility descriptions for screen readers.
**Action:** Dynamically generate the `alt` text in `labs(alt = ...)` (e.g., using `paste()` in R) using the loop iterator variables to accurately reflect each specific plot's data, ensuring distinct and informative alternative text for every generated visualization.
