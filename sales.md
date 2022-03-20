Predicting if price reduction will lead to an increase in sales
================
ELegushi Adetunji
3/20/2022

This lab will be used to show if factors like price deduction,coupon
usage and cart will lead to increase in sales.

Use this data to determine how these factors influence sales.

Predict sales of canned tomatoes during a week in which you use a
shopping cart notice, a coupon, and reduce price by 1 cent.

``` r
#loading the library 
library(pacman)
p_load(tidyverse,statsr,broom,openintro,janitor,infer,readxl)
library(broom)
```

``` r
#loading the data
Grocery <- read_excel("C:/Users/adeju/Downloads/Grocery.xlsx")
glimpse(Grocery)
```

    ## Rows: 12
    ## Columns: 5
    ## $ Week              [3m[38;5;246m<dbl>[39m[23m 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
    ## $ `Cart Notice`     [3m[38;5;246m<chr>[39m[23m "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", "No", "No~
    ## $ Coupon            [3m[38;5;246m<chr>[39m[23m "Yes", "Yes", "Yes", "No", "No", "No", "Yes", "Yes", "Yes~
    ## $ `Price Reduction` [3m[38;5;246m<dbl>[39m[23m 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2
    ## $ Sales             [3m[38;5;246m<dbl>[39m[23m 36, 38, 40, 40, 42, 44, 12, 20, 30, 8, 16, 33

``` r
Grocery %>% 
  clean_names()->Grocery
```

Performing EDA

``` r
library(GGally)
ggpairs(Grocery)
```

    ##  plot: [1,1] [=>-----------------------------------------------------]  4% est: 0s  plot: [1,2] [===>---------------------------------------------------]  8% est: 1s  plot: [1,3] [======>------------------------------------------------] 12% est: 1s  plot: [1,4] [========>----------------------------------------------] 16% est: 1s  plot: [1,5] [==========>--------------------------------------------] 20% est: 1s  plot: [2,1] [============>------------------------------------------] 24% est: 1s `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ##  plot: [2,2] [==============>----------------------------------------] 28% est: 1s  plot: [2,3] [=================>-------------------------------------] 32% est: 1s  plot: [2,4] [===================>-----------------------------------] 36% est: 1s  plot: [2,5] [=====================>---------------------------------] 40% est: 1s  plot: [3,1] [=======================>-------------------------------] 44% est: 1s `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ##  plot: [3,2] [=========================>-----------------------------] 48% est: 1s  plot: [3,3] [============================>--------------------------] 52% est: 1s  plot: [3,4] [==============================>------------------------] 56% est: 1s  plot: [3,5] [================================>----------------------] 60% est: 1s  plot: [4,1] [==================================>--------------------] 64% est: 0s  plot: [4,2] [====================================>------------------] 68% est: 0s `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ##  plot: [4,3] [=======================================>---------------] 72% est: 0s `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ##  plot: [4,4] [=========================================>-------------] 76% est: 0s  plot: [4,5] [===========================================>-----------] 80% est: 0s  plot: [5,1] [=============================================>---------] 84% est: 0s  plot: [5,2] [===============================================>-------] 88% est: 0s `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ##  plot: [5,3] [==================================================>----] 92% est: 0s `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ##  plot: [5,4] [====================================================>--] 96% est: 0s  plot: [5,5] [=======================================================]100% est: 0s                                                                                    

![](sales_files/figure-gfm/looking%20for%20relationship-1.png)<!-- -->

``` r
#using the linear model 
m =lm(sales ~ coupon+cart_notice+ price_reduction, data = Grocery)
```

``` r
summary(m)
```

    ## 
    ## Call:
    ## lm(formula = sales ~ coupon + cart_notice + price_reduction, 
    ##     data = Grocery)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.0417 -3.3229 -0.0625  3.3125  6.2083 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       14.042      3.089   4.545 0.001887 ** 
    ## couponYes         -1.167      2.913  -0.401 0.699248    
    ## cart_noticeYes    20.167      2.913   6.923 0.000122 ***
    ## price_reduction    6.375      1.784   3.574 0.007251 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.045 on 8 degrees of freedom
    ## Multiple R-squared:  0.8838, Adjusted R-squared:  0.8403 
    ## F-statistic: 20.29 on 3 and 8 DF,  p-value: 0.0004268

### Interpreting the model

intercept= this means that when all the predictors used
(coupon,cart\_notice,price\_reduction) are all zero then we can expect
to sell 14.5 cases of tomatoes per. This is usually not the case for
this store bcause at one point in time a customer might use their
coupon.

`coupon` = this has two levels, when the coupon was used and when it was
not used, analysing the report here means, when the coupon was yes or
used() at -1.167, sales went down by -1.167 than when it was not used
and looking at the p\_value, this is not a good predictor of sales and
in the new model, it will be removed.

`cart_notice` = another categorical data, yes or no, this indicates
that, when cart was used the model predicts that sales will increase by
20.167. A good predictor of sales.

`price_reduction` = this is a numerical variable, and according to the
model, when there is price reduction, the model predicts that sales will
go up by 6.375

Looking at the the R-Squared, its is high but removing coupon will
increase the r-squared in an improved model.

To forecast using the model, the linear equation will be sales =
14.5-1.167(coupon)+20.167(cart\_notice)+6.375(price\_reduction)

A new model will be used and compared to the previous r-squared to see
the winner, in this model, coupon will be removed.

``` r
m2 =lm(sales ~cart_notice+ price_reduction, data = Grocery)
```

looking at the coefficients

``` r
summary(m2)
```

    ## 
    ## Call:
    ## lm(formula = sales ~ cart_notice + price_reduction, data = Grocery)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.3750 -2.7396 -0.6458  2.7292  6.7917 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       13.458      2.594   5.187 0.000574 ***
    ## cart_noticeYes    20.167      2.774   7.271 4.71e-05 ***
    ## price_reduction    6.375      1.698   3.753 0.004531 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.804 on 9 degrees of freedom
    ## Multiple R-squared:  0.8815, Adjusted R-squared:  0.8552 
    ## F-statistic: 33.48 on 2 and 9 DF,  p-value: 6.786e-05

looking at the model, the adjusted r-squared as increased from 0.84 to
0.85 and the second model did a better job at predicting sales. we can
either use forecast manually or use the predict fuction.

``` r
p=data.frame(cart_notice ="Yes", price_reduction = 2)
```

``` r
predict(m2,p,interval = "prediction",level = 0.95)
```

    ##      fit      lwr      upr
    ## 1 46.375 34.02395 58.72605

the model predict with a confidence level of 0.95 that when price
reduction is at 2% and cart\_notice is yes, sales might increase and
will always be between $34 and $58.

looking at sales for that period

``` r
Grocery %>% 
  filter(cart_notice == "Yes" & price_reduction == 2) %>% 
select(cart_notice,price_reduction,sales)
```

    ## # A tibble: 2 x 3
    ##   cart_notice price_reduction sales
    ##   <chr>                 <dbl> <dbl>
    ## 1 Yes                       2    40
    ## 2 Yes                       2    44

The model predicted that sales will always between $34 and $58 and
looking at when sales is 40 a nd 44 in the original dataand when price
reduction is 2 and cart\_notice is yes, sales is between $40 and $44.

\#\#Observations We can conclude that when there is price reduction all
things being held equal,sales might increase. We can also say that when
cart\_notice is yes sales might increase too as long as every other
variable is held equal.
