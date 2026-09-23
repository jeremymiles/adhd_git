## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Add dynamic alt text for looped ggplot2 visualizations
**Learning:** When generating multiple ggplot2 visualizations dynamically within a loop, relying on static alt text or omitting it entirely renders the individual plots inaccessible to screen readers because they lack distinct descriptions identifying what each plot represents.
**Action:** Always dynamically generate descriptive `alt` text within the `labs(alt = ...)` function (e.g., using `paste()`) for visualizations created in a loop to ensure screen readers can accurately interpret and distinguish each generated plot.
