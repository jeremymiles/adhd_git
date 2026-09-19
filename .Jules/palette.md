## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Dynamic Alt Text for Iterative Visualizations
**Learning:** When generating multiple ggplot2 charts within a loop, using a static `alt` description causes screen readers to announce the same text for every distinct plot, which is inaccessible.
**Action:** Always dynamically generate `alt` text within the `labs(alt = ...)` function (e.g., using `paste()` with the loop variable) to ensure each visualization in the sequence has a unique and descriptive accessible name.
