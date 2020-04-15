# phenology

<!-- badges: start -->
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/kongdd/phenology_TP?branch=master&svg=true)](https://ci.appveyor.com/project/kongdd/phenology_TP)
<!-- badges: end -->

This package is for my Ph.D. thesis, Phenology research in Tibet Plateau.

## Installation

You can install the released version of phenology from GitHub with:

``` R
devtools:install.packages("kongdd/plsdepot")
devtools:install.packages("kongdd/phenology_TP")
```

**This repository replies heavily on `Rcmip5` and `Ipaper` package.**

## Updates

1. MCD12Q2 modified to V6
2. meteorological forcing update to 2018

## Findings

## Tasklists

新方法鉴定植被物候的影响

1. 剔除LC变化率大于5%的区域，以控制LC变化的影响
going on

2. 通过dynamic_veg - static_veg，剔除因素的影响，剥离出植被变化的影响（植被变化包括植被物候变化和植被覆盖度变化）。
植被物候通过生长季开始时间和生长季结束时间来反映；植被覆盖度通过年最大LAI来反映。

delta_GPP = dynamic_veg - static_veg (in PML-V2 model)
detla_GPP ~ SOS + EOS + LAI_yearlyMaxe

The same method for ET
