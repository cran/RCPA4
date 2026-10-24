# Example Timing Inventory

Generated: 2026-10-08 17:28:20 EDT
R: R version 4.6.1 (2026-06-24 ucrt)

Complete generated Rd example blocks, including donttest, run in separate R processes.
Default RCPA4 options and pauses are retained. Interactive guards are not entered.
Each process uses a temporary working directory and PDF graphics device.
Timing excludes process startup, pkgload setup, and device setup; library calls inside examples are timed.
This measures the current source package via pkgload, not an installed-package CRAN check.
Borderline pages (3-7 seconds on the first run) are repeated twice. Other pages run once.
The per-process timeout, including setup, is 90 seconds.
Classification uses the slowest measured run: under 3s safely under, 3-7s borderline, over 7s slow.
These margins are inventory heuristics, not CRAN rules. Errors are not recommendations to unwrap.

Follow-up: after correcting bare-column resolution in the shared input helper,
the complete sortC example block (including donttest) passed in 9.91 seconds
with default pauses. The sortC error and its 2.31-second timing below are the
original, incomplete baseline run, not its current status. Keep its partial
donttest wrapper. Existing wrappers, including printC's file-writing examples,
remain unchanged.

| Topic | Group | Runs | Median (s) | Slowest (s) | Status | donttest |
|---|---|---:|---:|---:|---|---|
| boxplotC | featured | 3 | 4.86 | 4.93 | borderline | FALSE |
| freqC | featured | 3 | 4.95 | 4.97 | borderline | FALSE |
| orci | helper/other | 3 | 6.33 | 6.36 | borderline | TRUE |
| logregR2 | helper/other | 3 | 6.35 | 6.47 | borderline | TRUE |
| CIprop | featured | 3 | 6.87 | 6.89 | borderline | TRUE |
| CImean | featured | 3 | 6.89 | 6.95 | borderline | TRUE |
| sortC | featured | 1 | 2.31 | 2.31 | error | TRUE |
| getC | featured | 1 | 0 | 0 | interactive-only (skipped) | FALSE |
| helpC | featured | 1 | 0 | 0 | interactive-only (skipped) | FALSE |
| widgetFactory | helper/other | 1 | 0 | 0 | interactive-only (skipped) | FALSE |
| CramersV | helper/other | 1 | 0 | 0 | safely under | FALSE |
| lambda | helper/other | 1 | 0 | 0 | safely under | FALSE |
| random_names | helper/other | 1 | 0 | 0 | safely under | FALSE |
| tablesomersDC | helper/other | 1 | 0 | 0 | safely under | FALSE |
| inverse_logit | helper/other | 1 | 0.02 | 0.02 | safely under | FALSE |
| graph.info.functions | helper/other | 1 | 0.04 | 0.04 | safely under | FALSE |
| wtd_hist | helper/other | 1 | 0.04 | 0.04 | safely under | FALSE |
| wtd_mean | helper/other | 1 | 0.2 | 0.2 | safely under | FALSE |
| wtd_sd | helper/other | 1 | 0.21 | 0.21 | safely under | FALSE |
| wtd_kurtosis | helper/other | 1 | 0.22 | 0.22 | safely under | FALSE |
| wtd_median | helper/other | 1 | 0.22 | 0.22 | safely under | FALSE |
| wtd_quantile | helper/other | 1 | 0.22 | 0.22 | safely under | FALSE |
| wtd_skewness | helper/other | 1 | 0.22 | 0.22 | safely under | FALSE |
| wtd_var | helper/other | 1 | 0.23 | 0.23 | safely under | FALSE |
| wtd_mode | helper/other | 1 | 0.24 | 0.24 | safely under | FALSE |
| transformC | featured | 1 | 0.31 | 0.31 | safely under | FALSE |
| wtd_cor | helper/other | 1 | 0.33 | 0.33 | safely under | FALSE |
| printC | featured | 1 | 2.08 | 2.08 | safely under | TRUE |
| histC | featured | 1 | 2.33 | 2.33 | safely under | FALSE |
| fit_svyglm | helper/other | 1 | 8.69 | 8.69 | slow | TRUE |
| compmeansC | featured | 1 | 11.45 | 11.45 | slow | TRUE |
| pchisqC | helper/other | 1 | 12.89 | 12.89 | slow | TRUE |
| correlateC | featured | 1 | 13.07 | 13.07 | slow | TRUE |
| describeC | featured | 1 | 13.24 | 13.24 | slow | TRUE |
| sampdistC | featured | 1 | 17.16 | 17.16 | slow | TRUE |
| crosstabC | featured | 1 | 23.88 | 23.88 | slow | TRUE |
| logregC | featured | 1 | 27.5 | 27.5 | slow | TRUE |
| regC | featured | 1 | 29.61 | 29.61 | slow | TRUE |
| testpropsC | featured | 1 | 31.62 | 31.62 | slow | TRUE |
| testmeansC | featured | 1 | 33.69 | 33.69 | slow | TRUE |
| welcomeC | featured | 1 | 54.08 | 54.08 | slow | TRUE |

## Warnings And Errors

- `sortC`: Unknown variable(s) in `x`: Afghanistan, Albania, Algeria, Angola, Argentina, Armenia, Australia, Austria, Azerbaijan, Bahrain, Bangladesh, Belarus, Belgium, Belize, Benin, Bhutan, Bolivia, Bosnia and Herzegovina, Botswana, Brazil, Bulgaria, Burkina Faso, Burundi, Cambodia, Cameroon, Canada, Cape Verde, Central African Republic, Chad, Chile, China, Colombia, Comoros, Congo, Democratic Republic of the, Congo, Republic of the, Costa Rica, Croatia, Cuba, Cyprus, Czech Republic, Denmark, Djibouti, Dominican Republic, Ecuador, Egypt, El Salvador, Equatorial Guinea, Eritrea, Estonia, Eswatini, Ethiopia, Fiji, Finland, France, Gabon, Gambia, Georgia, Germany, Ghana, Greece, Guatemala, Guinea, Guinea-Bissau, Guyana, Haiti, Honduras, Hungary, Iceland, India, Indonesia, Iran, Iraq, Ireland, Israel, Italy, Ivory Coast, Jamaica, Japan, Jordan, Kazakhstan, Kenya, Kuwait, Kyrgyzstan, Laos, Latvia, Lebanon, Lesotho, Liberia, Libya, Lithuania, Luxembourg, Macedonia, Madagascar, Malawi, Malaysia, Maldives, Mali, Malta, Mauritania, Mauritius, Mexico, Moldova, Mongolia, Montenegro, Morocco, Mozambique, Myanmar, Namibia, Nepal, Netherlands, New Zealand, Nicaragua, Niger, Nigeria, North Korea, Norway, Oman, Pakistan, Panama, Papua New Guinea, Paraguay, Peru, Philippines, Poland, Portugal, Qatar, Romania, Russia, Rwanda, Saudi Arabia, Senegal, Serbia, Sierra Leone, Singapore, Slovakia, Slovenia, Somalia, South Africa, South Korea, South Sudan, Spain, Sri Lanka, Sudan, Suriname, Sweden, Switzerland, Syria, Taiwan, Tajikistan, Tanzania, Thailand, Timor-Leste, Togo, Trinidad and Tobago, Tunisia, Turkey, Turkmenistan, Uganda, Ukraine, United Arab Emirates, United Kingdom, United States, Uruguay, Uzbekistan, Venezuela, Vietnam, Yemen, Zambia, Zimbabwe
- `crosstabC`: You did not specify a plot.response value. So, crosstabC will plot r_favor_opps_death's first category: "Favor strongly". To plot another DV value, use the plot.response argument.

