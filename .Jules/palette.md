## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Use accessible alt text in ggplot2 plots
**Learning:** Data visualizations without alternative text (alt text) present a major accessibility barrier for users relying on screen readers. This makes charts and graphs completely opaque to those users.
**Action:** Always add accessible alternative text using the `labs(alt = "...")` function directly within `ggplot2::ggplot()` calls to ensure screen readers can describe the generated plot image. For plots generated in loops, dynamically construct the alt text (e.g., using `paste()`) to reflect the specific iteration's data.
