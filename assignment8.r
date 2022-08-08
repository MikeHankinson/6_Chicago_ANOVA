# =====================================================
# Module 8 Homework - ANOVA
#                     
# Mike Hankinson
# =====================================================
# - Execute the provided R script to answer the questions related to ANOVA models. 
#   This assignment uses a data set on crabs and require us to use gender and species 
#   of crab to predict the body depth of the animal.

# - Answer the provided questions:
# 1.  In one-way ANOVA, what is the sum of squares between for gender? 
#     Answer to 2 decimal places:
#     ANSWER: 18.79     See model results below for detail


# 2.  In one-way ANOVA, what is the sum of squares within for gender? 
#     Answer to 2 decimal places:
#     ANSWER: 2315.30   See model results below for detail


# 3.  Does Gender have a significant effect on body depth for crabs? Yes/No?
#     Answer - NO
#     - Small F value (1.61) means this predictor is not effective in accounting for variation
#       in body depth of crabs.   
#     - P-value >> 0.05.  Therefore, this is in agreement that gender does not 
#       have a significant effect on body depth for crabs


# 4.  When performing two-way ANOVA, which of the main effects are significant?
#       a. None
#       b. Gender
#       c. Species
#       d. Both
#     Answer - Species is significant
#     - Large F value (44.31) means this predictor is effective in accounting for variation
#       in body depth of crabs.   
#     - P-value < 0.05 (2.751e-10)  Therefore, this is in agreement that species does 
#       have a significant effect on body depth for crabs.


# 5.  At what level is the interaction between gender and species significant? 
#     Select the single best answer:
#       a. 99%
#       b. 97.5%
#       c. 95%
#       d. 90%
#     Answer - 95% (see the TukeyHSD)



# ******************************************
# Load Data
# ******************************************
library(MASS)
data(crabs)
# dim(crabs)          # [1] 200   8
# head(crabs)
    #   sp  sex  index   FL   RW    CL    CW    BD
    # 1  B   M     1    8.1   6.7   16.1  19.0  7.0
    # 2  B   M     2    8.8   7.7   18.1  20.8  7.4
    # 3  B   M     3    9.2   7.8   19.0  22.4  7.7
    # 4  B   M     4    9.6   7.9   20.1  23.1  8.2
    # 5  B   M     5    9.8   8.0   20.3  23.0  8.2
    # 6  B   M     6    10.8   9.0   23.0  26.5 9.8

help(crabs)
    # sp species - "B" or "O" for blue or orange.
    # sex
    # index 1:50 within each of the four groups.
    # FL frontal lobe size (mm)
    # RW rear width (mm)
    # CL carapace length (mm)
    # CW carapace width (mm)
    # BD body depth (mm)


# ******************************************
# One Way ANOVA
# ******************************************
gender <- lm(BD ~ sex, data=crabs)
summary(gender)
    # Residuals:
    #   Min     1Q Median     3Q    Max 
    # -7.624 -2.449  0.076  2.463  7.376 
    # 
    # Coefficients:
    #           Estimate Std.   Error   t value   Pr(>|t|)    
    # (Intercept)   13.7240     0.3420  40.134   <2e-16 ***
    #  sexM          0.6130     0.4836   1.268    0.206    
    # ---
    #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    # 
    # Residual standard error: 3.42 on 198 degrees of freedom
    # Multiple R-squared:  0.00805,	Adjusted R-squared:  0.00304 
    # F-statistic: 1.607 on 1 and 198 DF,  p-value: 0.2064


anova(gender)
    # Analysis of Variance Table
    # 
    # Response: BD
    #             Df    Sum Sq    Mean Sq   F value   Pr(>F)
    # sex         1     18.79     18.788    1.6068    0.2064
    # Residuals 198   2315.30     11.693


anova <- anova(gender)
anova$`Sum Sq`    # Question 1:   18.78845 -- Sum of Squares BETWEEN Groups (18.79)
anova$`Sum Sq`[2] # Question 2:   2315.295 -- Sum of Squares BETWEEN Groups (2315.30)

# Question 3: Answer - NO
# - Small F value (1.61) means this predictor is not effective in accounting for variation
#   in body depth of crabs.   
# - P-value >> 0.05.  Therefore, this is in agreement that gender does not 
#   have a significant effect on body depth for crabs


# ******************************************
# Two Way ANOVA
# ******************************************

gender.species <- lm(BD~sex*sp,data=crabs)
summary(gender.species)
    # Residuals:
    #   Min     1Q Median     3Q    Max 
    # -7.924 -2.224  0.059  2.250  6.650 
    # 
    # Coefficients:
    #             Estimate    Std. Error t value    Pr(>|t|)    
    # (Intercept)    11.8160     0.4349  27.167   < 2e-16 ***
    #   sexM          1.5340     0.6151   2.494   0.0135 *  
    #   spO           3.8160     0.6151   6.204   3.21e-09 ***
    #   sexM:spO     -1.8420     0.8699  -2.118   0.0355 *  
    #   ---
    #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    # 
    # Residual standard error: 3.075 on 196 degrees of freedom
    # Multiple R-squared:  0.2058,	Adjusted R-squared:  0.1936 
    # F-statistic: 16.93 on 3 and 196 DF,  p-value: 8.131e-10


anova(gender.species)
    # Analysis of Variance Table
    # Response: BD
    #           Df  Sum Sq    Mean Sq   F value   Pr(>F)    
    # sex         1   18.79   18.79    1.9864   0.16030    
    # sp          1  419.05  419.05   44.3050   2.751e-10 ***
    # sex:sp      1   42.41   42.41    4.4841   0.03547 *  
    # Residuals 196 1853.83    9.46                      

tuk <- TukeyHSD(aov(gender.species))
        # Tukey multiple comparisons of means
        # 95% family-wise confidence level
        # 
        # Fit: aov(formula = gender.species)
        # 
        # $sex
        # diff        lwr      upr     p adj
        # M-F 0.613 -0.2447489 1.470749 0.1602977
        # 
        # $sp
        # diff      lwr      upr p adj
        # O-B 2.895 2.037251 3.752749     0
        # 
        # $`sex:sp`
        #           diff    lwr         upr     p adj
        # M:B-F:B  1.534 -0.05982032 3.12782 0.0639702
        # F:O-F:B  3.816  2.22217968 5.40982 0.0000000
        # M:O-F:B  3.508  1.91417968 5.10182 0.0000003
        # F:O-M:B  2.282  0.68817968 3.87582 0.0015279
        # M:O-M:B  1.974  0.38017968 3.56782 0.0083935
        # M:O-F:O -0.308 -1.90182032 1.28582 0.9588102

# Question 4: Answer - Species is significant
# - Large F value (44.31) means this predictor is effective in accounting for variation
#   in body depth of crabs.   
# - P-value < 0.05 (2.751e-10)  Therefore, this is in agreement that species does 
#   have a significant effect on body depth for crabs.

# Question 5: Answer - 95% (see the TukeyHSD)



