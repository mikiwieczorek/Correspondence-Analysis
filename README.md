# Correspondence-Analysis


Multiple Correspondence Analysis
================================

``` r
library(FactoMineR)
library(factoextra)
```

    ## Loading required package: ggplot2

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(MASS)
library(ca)
```

``` r
paintings = read.csv("BobRossCommon.csv")
names(paintings)
```

    ##  [1] "TITLE"          "BUSHES"         "CABIN"          "CIRRUS"        
    ##  [5] "CLOUDS"         "CONIFER"        "CUMULUS"        "DECIDUOUS"     
    ##  [9] "FENCE"          "FLOWERS"        "FOG"            "GRASS"         
    ## [13] "HILLS"          "LAKE"           "MOUNTAIN"       "PATH"          
    ## [17] "RIVER"          "ROCKS"          "SNOW"           "SNOWY_MOUNTAIN"
    ## [21] "STRUCTURE"      "SUN"            "TREE"           "TREES"         
    ## [25] "WATERFALL"      "WINTER"

Each <b>row</b> corresponds to a painting. The <b>columns</b> consist of
the name of the painting followed by columns (1/0) for each of the
possible major features of these paintings, where 1 = present and 0 =
absent.

``` r
#Set row names/numbers to correspond to TITLE rows
par(mfrow=c(3,2))
row.names(paintings) = paintings$TITLE
pt.mat = paintings[,-1]
pt.mat = apply(pt.mat,2,as.factor)
pt.MCA = MCA(pt.mat)
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-3-1.png) MCA
factor maps above show the point cloud of paintings in their feature
selection. To understand it, we need to consider the distance between
them.

Multiple Correspondence Analysis
--------------------------------

We are going to use multiple correpondence analysis (MCA) to analyze the
thematic structure of the 316 paintings in this data base.

We will construct plots showing individuals, variables, and both
individuals & variables. We will use the “cos2” measure of quality of
representation to color code the individual painting and use the
“contrib” measure of importance to color code the variables.

``` r
plot(pt.MCA, invisible = "var")
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-4-1.png)

### Exploring the variance explained for our MCA dimensions

J = 26, as we have 26 different categories/features that Bob’s paintings
can have, each has 2 levels k=52 levels (=26\*2),

The first dimension explains about 12.375% of variation:

``` r
summary(pt.MCA)
```

    ## 
    ## Call:
    ## MCA(X = pt.mat) 
    ## 
    ## 
    ## Eigenvalues
    ##                        Dim.1   Dim.2   Dim.3   Dim.4   Dim.5   Dim.6
    ## Variance               0.124   0.102   0.077   0.068   0.065   0.055
    ## % of var.             12.375  10.215   7.681   6.812   6.525   5.549
    ## Cumulative % of var.  12.375  22.590  30.271  37.083  43.608  49.157
    ##                        Dim.7   Dim.8   Dim.9  Dim.10  Dim.11  Dim.12
    ## Variance               0.050   0.047   0.044   0.040   0.040   0.037
    ## % of var.              4.997   4.663   4.366   4.037   4.001   3.740
    ## Cumulative % of var.  54.154  58.817  63.183  67.220  71.221  74.961
    ##                       Dim.13  Dim.14  Dim.15  Dim.16  Dim.17  Dim.18
    ## Variance               0.035   0.033   0.032   0.029   0.026   0.023
    ## % of var.              3.515   3.275   3.214   2.902   2.593   2.271
    ## Cumulative % of var.  78.476  81.751  84.965  87.867  90.460  92.730
    ##                       Dim.19  Dim.20  Dim.21  Dim.22  Dim.23  Dim.24
    ## Variance               0.021   0.015   0.012   0.010   0.008   0.004
    ## % of var.              2.057   1.507   1.206   1.025   0.770   0.407
    ## Cumulative % of var.  94.788  96.294  97.501  98.526  99.295  99.702
    ##                       Dim.25
    ## Variance               0.003
    ## % of var.              0.298
    ## Cumulative % of var. 100.000
    ## 
    ## Individuals (the 10 first)
    ##                                Dim.1    ctr   cos2    Dim.2    ctr   cos2
    ## WOODGRAIN_VIEW              | -0.154  0.061  0.025 |  0.215  0.144  0.049
    ## WOODED_STREAM_OVAL          | -0.337  0.291  0.193 |  0.334  0.347  0.190
    ## WINTERTIME_DISCOVERY        |  0.795  1.617  0.629 |  0.177  0.097  0.031
    ## WINTERTIME_BLUES            |  0.432  0.478  0.215 | -0.385  0.460  0.171
    ## WINTERS_PEACE               |  0.411  0.432  0.228 | -0.036  0.004  0.002
    ## WINTERS_PARADISE            |  0.334  0.285  0.112 | -0.563  0.983  0.320
    ## WINTERS_GRACE               |  0.472  0.569  0.184 | -0.110  0.038  0.010
    ## WINTERS_ELEGANCE            |  0.236  0.142  0.050 | -0.029  0.003  0.001
    ## WINTER_SUN                  |  0.291  0.216  0.085 |  0.014  0.001  0.000
    ## WINTER_STILLNESS            |  0.437  0.488  0.276 |  0.057  0.010  0.005
    ##                                Dim.3    ctr   cos2  
    ## WOODGRAIN_VIEW              |  0.374  0.577  0.148 |
    ## WOODED_STREAM_OVAL          | -0.234  0.226  0.093 |
    ## WINTERTIME_DISCOVERY        | -0.197  0.160  0.039 |
    ## WINTERTIME_BLUES            | -0.082  0.028  0.008 |
    ## WINTERS_PEACE               |  0.146  0.088  0.029 |
    ## WINTERS_PARADISE            | -0.192  0.152  0.037 |
    ## WINTERS_GRACE               | -0.285  0.335  0.067 |
    ## WINTERS_ELEGANCE            | -0.125  0.064  0.014 |
    ## WINTER_SUN                  | -0.181  0.135  0.033 |
    ## WINTER_STILLNESS            | -0.234  0.226  0.079 |
    ## 
    ## Categories (the 10 first)
    ##                                 Dim.1     ctr    cos2  v.test     Dim.2
    ## BUSHES_0                    |   0.146   0.440   0.038   3.462 |   0.215
    ## BUSHES_1                    |  -0.261   0.790   0.038  -3.462 |  -0.386
    ## CABIN_0                     |  -0.322   2.701   0.426 -11.583 |  -0.118
    ## CABIN_1                     |   1.321  11.066   0.426  11.583 |   0.482
    ## CIRRUS_0                    |  -0.015   0.006   0.003  -0.991 |   0.027
    ## CIRRUS_1                    |   0.215   0.094   0.003   0.991 |  -0.401
    ## CLOUDS_0                    |  -0.039   0.030   0.002  -0.846 |   0.382
    ## CLOUDS_1                    |   0.058   0.044   0.002   0.846 |  -0.561
    ## CONIFER_0                   |  -0.370   1.809   0.095  -5.458 |   0.815
    ## CONIFER_1                   |   0.255   1.248   0.095   5.458 |  -0.562
    ##                                 ctr    cos2  v.test     Dim.3     ctr
    ## BUSHES_0                      1.164   0.083   5.117 |   0.057   0.109
    ## BUSHES_1                      2.091   0.083  -5.117 |  -0.103   0.196
    ## CABIN_0                       0.437   0.057  -4.231 |  -0.027   0.030
    ## CABIN_1                       1.789   0.057   4.231 |   0.110   0.123
    ## CIRRUS_0                      0.027   0.011   1.848 |  -0.056   0.151
    ## CIRRUS_1                      0.398   0.011  -1.848 |   0.823   2.233
    ## CLOUDS_0                      3.396   0.214   8.212 |  -0.181   1.012
    ## CLOUDS_1                      4.988   0.214  -8.212 |   0.265   1.486
    ## CONIFER_0                    10.617   0.458  12.013 |   0.204   0.885
    ## CONIFER_1                     7.324   0.458 -12.013 |  -0.141   0.611
    ##                                cos2  v.test  
    ## BUSHES_0                      0.006   1.359 |
    ## BUSHES_1                      0.006  -1.359 |
    ## CABIN_0                       0.003  -0.961 |
    ## CABIN_1                       0.003   0.961 |
    ## CIRRUS_0                      0.046  -3.797 |
    ## CIRRUS_1                      0.046   3.797 |
    ## CLOUDS_0                      0.048  -3.887 |
    ## CLOUDS_1                      0.048   3.887 |
    ## CONIFER_0                     0.029   3.008 |
    ## CONIFER_1                     0.029  -3.008 |
    ## 
    ## Categorical variables (eta2)
    ##                               Dim.1 Dim.2 Dim.3  
    ## BUSHES                      | 0.038 0.083 0.006 |
    ## CABIN                       | 0.426 0.057 0.003 |
    ## CIRRUS                      | 0.003 0.011 0.046 |
    ## CLOUDS                      | 0.002 0.214 0.048 |
    ## CONIFER                     | 0.095 0.458 0.029 |
    ## CUMULUS                     | 0.014 0.185 0.020 |
    ## DECIDUOUS                   | 0.032 0.347 0.025 |
    ## FENCE                       | 0.134 0.089 0.000 |
    ## FLOWERS                     | 0.002 0.008 0.023 |
    ## FOG                         | 0.001 0.002 0.024 |

<br> And the average variance explained (inertia) for all dimensions is
1/J = 1/26.

``` r
options(digits = 2)
1/26
```

    ## [1] 0.038

So any dimension with % of variance over 3.8% could be interpreted in
our MCA. So based on our summary(pr.MCA) above or our scree plot below,
we should retain 11 dimensions.

``` r
fviz_screeplot(pt.MCA, addlabels = TRUE, ylim = c(0, 45))
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-7-1.png)

<br> </br>

Now, we’re going to construct plots showing individuals, variables
(product ratings), and both individuals & variables. We will use the
“cos2” measure of quality of representation to color code the individual
painting and the “contrib” measure of importance to color code the
variables.

We are going to construct MCA plot using SVD components. To label levels
of categorical columns, we have to extract column labels from different
component of the MCA results (pt.MCA):

``` r
pt.SVD = pt.MCA$svd
attributes(pt.SVD)
```

    ## $names
    ## [1] "vs" "U"  "V"

``` r
delta2 = pt.SVD$vs
V2 = pt.SVD$V
U2 = pt.SVD$U
```

``` r
plot(delta2[1]*V2[,1], delta2[2]*V2[,2], type = "n", xlim = c(-1.1,1.1), ylim = c(-1.1,1.1),
     xlab = "Dimension 1", ylab = "Dimension 2", main = "Construction of MCA Plot Using SVD Components")
text(delta2[1]*V2[,1], delta2[2]*V2[,2], labels = row.names(pt.MCA$var$coord), cex = 0.4,
     col = "black")
abline(h=0, v=0, lty = 2, col = "red")
points(delta2[1]*U2[,1], delta2[2]*U2[,2], pch = ".", col = "blue", cex = 2.5)
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-10-1.png)

### Construct plots showing variables (features):

``` r
plot(pt.MCA, invisible = "ind", cex = 0.8)
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-12-1.png)

Using the “cos2” measure of quality of representation to color code
variables:

``` r
fviz_mca_var(pt.MCA, repel = F, col.var = "cos2")
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-13-1.png)

Using the “contrib” measure of importance to color code the variables:

``` r
fviz_mca_var(pt.MCA, repel = F, col.var = "contrib")
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-14-1.png)
<br></br>

Contribution or quality of representation Cos2 of variables and
individuals can be organized graphically as below, from highest to
lowest <b>contribution of variables</b> to Dimension 1 in this case.

``` r
fviz_contrib(pt.MCA, choice = "var", cex =0.2)
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-15-1.png)

Selecting only 8 features whose contribution was the highest:

``` r
fviz_mca_var(pt.MCA, repel = T, select.var = list(contrib = 8))
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-16-1.png)

Selecting only 8 features whose quality of representaiton was the
highest:

If cos2 is in \[0,1\], ex: 0.6, then individuals/variables with a
cos&gt;0.6 are shown. If cos2&gt;1, ex:5, then the top 8
individuals/variables with the highest cos2 are shown.

``` r
fviz_mca_var(pt.MCA, repel = T, select.var = list(cos2 = 8))
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-17-1.png)

If a variable category is well represented by two dimensions, the sum of
the cos2 is closed to one.

### Construct plots showing individuals (paintings):

``` r
plot(pt.MCA, invisible = "var", cex = 0.5)
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-19-1.png)

Using the “cos2” measure of quality of representation to color code
individuals:

``` r
fviz_mca_ind(pt.MCA, repel = F, col.ind = "cos2")
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-20-1.png)

Using the “contrib” measure of importance to color code the individuals:

``` r
fviz_mca_ind(pt.MCA, repel = F, col.ind = "contrib")
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-21-1.png)

Contribution or quality of representation Cos2 of variables and
individuals can be organized graphically as below, from highest to
lowest <b>contribution of individuals</b> to Dimension 1 in this case
(as it is difficult to read text on x-axis, we will use color intensity
representation above:

``` r
fviz_contrib(pt.MCA, choice = "ind", cex =0.1)
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-22-1.png)

Selecting only 8 painitngs whose contribution was the highest:

``` r
fviz_mca_ind(pt.MCA, repel = F, select.ind = list(contrib = 8))
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-23-1.png) As
depicted above, these are the names of Bob’s paintings that have the
most contribution to the two dimensions.

Selecting only 8 paintings whose quality of representaiton was the
highest:

If cos2 is in \[0,1\], ex: 0.6, then individuals/variables with a
cos&gt;0.6 are shown. If cos2&gt;1, ex:5, then the top 5
individuals/variables with the highest cos2 are shown.

``` r
fviz_mca_ind(pt.MCA, repel = T, select.ind = list(cos2 = 8))
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-24-1.png)

PART B
------

1.  Use plots with the individuals labeled to identify at least four
    paintings that are in the extreme.

Conclude the above sections on (\#\#\#Construct plots showing
individuals (paintings)):…

Biplot
------

Using a biplot (i.e. plot of both individuals and variables) to name the
quadrants to identify groups of paintings that have similar subject
matter or similar features.

``` r
fviz_mca_biplot(pt.MCA, repel = F, ggtheme = theme_minimal())
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-25-1.png)
<br>The plot above shows a global pattern within the data. Rows
(individuals) are represented by blue points and columns (variable
categories) by red triangles.

The distance between any row points or column points gives a measure of
their similarity (or dissimilarity). Row points with similar profile are
closed on the factor map. The same holds true for column points.

<br>The blue labels are paintings and the red labels are their features
with number 1 meaning the feature is present and 0 meaning that it’s
not. It semms like in Quadrant I there are features such as Fence,
Structure, Sun, Fog, and Cabin, and the higher the Dim1 values (to the
right) are, we can see a group of winter-representing paintings. With
closer to y-axis there is a mix of paintings also depicting Fall season,
or an extreme painting like The Old Home Place. In Quandrant II, there
are features such as FLowers, River, Deciduous, Path, and Grass. Looking
at the individual paintings, they represent warmer days in summer time,
with flowers rivers, also in the autumn time and in the woods. In
Quandrant III, we can see some groups with similar features as in
Quandrant II: Bushes, Cumulus, Waterfall and also Rocks. The paintings
here show mountain landscapes, waterfalls, there is more autumn season
present here. But also mountain painitngs showing winter season.
Quandrant IV has paintings with features such as Cirrus, SnowyMountian,
Lake, Snow and Winter (farther to the right). The paintings here are
mostly winter paintings. It seems like y-axis contrasts the paintings
groups between QI and IV vs QII and III.

</br>

Less messy was the plot of both variables and individuals we constructed
using SVD components:

``` r
plot(delta2[1]*V2[,1], delta2[2]*V2[,2], type = "n", xlim = c(-1.1,1.1), ylim = c(-1.1,1.1),
     xlab = "Dimension 1", ylab = "Dimension 2", main = "Construction of MCA Plot Using SVD Components")
text(delta2[1]*V2[,1], delta2[2]*V2[,2], labels = row.names(pt.MCA$var$coord), cex = 0.4,
     col = "black")
abline(h=0, v=0, lty = 2, col = "red")
points(delta2[1]*U2[,1], delta2[2]*U2[,2], pch = ".", col = "blue", cex = 2.5)
```

![](MCA_BobRoss_files/figure-markdown_github/unnamed-chunk-26-1.png)
