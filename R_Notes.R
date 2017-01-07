#' ---
#' title: "R Notes to Supplement the SAS and R textbook"
#' author: |
#'     | Georges Monette and MATH 4939
#' date: "January 6, 2017"
#' output:
#'     html_document:
#'         toc: true
#'         toc_depth: 6
#' ---
#' 
#' Output produced on: `r format(Sys.time(), '%B %d, %Y at %H:%M')`
#'
#' <!--
#' Incorporate Warwick Exercises
#' -->
#' 
#' # Introduction
#' 
#' The topics in these notes are organized to parallel the material in Kleinman and Horton (2015) "SAS and R: Data Management,
#' Statistical Analysis and Graphics, Second Edition" CRC Press.
#' 
#+ eval=FALSE
install.packages('haven')
install.packages('readxl')
install.packages('rgl')
install_github('gmonette/spida2')
install_github('gmonette/p3d')
#'
#' # 1. Data input and output
#' 
#' ## 1.1 Input
#' 
#' ### 1.1.6 Read sheets from an Excel file
#' 
#' I believe that currently (January 2017) the most effective way to read an Excel file
#' is to use Hadley Wickam's 'readxl' CRAN package.  If 'file.xlsx' is an Excel file and you
#' want to read the second worksheet that uses 'NA' for missing values, use:
#+ eval=FALSE
library(readxl)
dd <- read_excel('file.xlsx', sheet = 2, na = 'NA')
dd <- as.data.frame(as.list(dd))
#' The last line is important if you want a data frame in the usual format -- e.g  with factors
#' for categorical variables. 
#' The file created by 'read_excel' has class 'tibble' which is not used outside the
#' Hadleyverse.
#' 
#' ### 1.1.A Read sheets from an SPSS file
#' 
#' SPSS files have long been a problem for R but there is a recent package, 'haven', on
#' CRAN that seems to do an excellent job. It uses R attributes to store SPSS variable labels
#' and correctly transforms SPSS date into R objects of class 'Date'. Be aware that it is common
#' in SPSS to have user-defined missing values. By default all these values are converted to
#' 'NA' in R but the distinct values are likely to be informative. Use the argument, 'user_na = TRUE'
#' to recover missing value labels. Like 'read_excel', 'read_sav' creates a 'tibble' but
#' the trick that works with Excel files of using 'as.data.frame(as.list(...))' to turn it
#' into a standard data frame does not work here. You might have to some surgery on the
#' variables in some cases.
#' 
#' {{red|Warning:}} Some functions, e.g. 'lm' may treat a categorical variable as a numeric
#' variable producing embarrassingly  non-sensical results.
#' 
#+ eval=FALSE
library(haven)
path <- system.file('examples', 'iris.sav', package = 'haven') # get the path to a system file
path
dd <- haven::read_sav(path)
head(dd)  # a tibble
class(dd)
fit <- lm(Petal.Width ~ Species, dd)
summary(fit)  # Species is numerical
ds <- as.data.frame(as.list(dd))
head(ds) # Species is still numerical
# You are not expected to understand the next line ... yet!
dd$Species <- factor(names(attr(dd$Species,'labels'))[dd$Species]) # complicated fix 
str(dd$Species) # now it's a factor!
fit <- lm(Petal.Width ~ Species, dd) # treats 'Species' as a factor with correct levels
summary(fit)  
#' If you are working with SPSS files you might want to create a function to take
#' care of coercing 'labelled' variables to a factor.
#'
#' ## 1.1.X Exercises
#' 
#' ## 1.2 Output
#' 
#' ## 1.3 Further resources
#' 
#' # 2 Data management
#' 
#' ## 2.1 Structure and meta-data
#' 
#' ## 2.2 Derived variable and data manipulation
#' 
#' ## 2.3 Merging, combining and subsetting datasets
#' 
#' ## 2.4 Date and time variables
#' 
#' ## 2.5 Further resources
#' 
#' ## 2.6 Examples
#' 
#' ## 2.X Exercises
#' 
#' Warwick Exercises:
#' 
#' 1. [Warwick] Create the vectors:
#' 
#'     a. (1,2,3,...,19,20)
#'     b. (20,19,...,2,1)
#'     c. (1,2,3,...,19,20,19,18,...,2,1)
#'     d. (4,6,3) and assign it to the name 'tmp'
#'     e. (4,6,3,4,6,3,...,4,6,3) where there are 10 occurrances of 4 (Hint: ?rep)
#'     f. (4,6,3,4,6,3,...,4,6,3,4) where there are 11 occurrances of 4 and 10 of 6 and 3
#'     g. (4,4,...,4,6,6,...,6,3,3,...,3) where there are 10 occurrances of 4, 20 of 6 and 30 of 3.
#'     
#' 2. [Warwick] Create the vector of the values of $e^x \cos(x)$ at $x=3, 3.1, 3.2, ..., 6$.
#' 
#' 3. [Warwick] Create the following vectors:
#' 
#'     a. $(0.1^3 0.2^1, 0.1^6 0.2^4, ... , 0.1^{36} 0.2^{34} )$
#'     b. $\left({2,\frac{2^2}{2},\frac{2^3}{3},...,\frac{2^{25}}{25}}\right)$      
#'    
#' 4. [Warwick] Calculate the following:
#' 
#'     a. $\sum_{i=10}^{100} (i^3 + 4i^2)$
#'     b. $\sum_{i=1}^{25} \left({\frac{2^i}{i} + \frac{3^i}{i^2}}\right)$
#'     
#' 5. [Warwick] Use the function 'paste' to create the following character vectors of length 30:
#'
#'     a. ("label 1", "label 2", ... , "label 30"). Note that there is a single space
#'        between 'label' and the number following.
#'     b. ("fn1", "fn2", ..., "fn30"). In this case there is no space.
#'     
#' 6. [Warwick] Execute the following lines which create two vectors of random integers which 
#'    are chosen with replacement from the integers 0, 1, ..., 999. Both vectors have length 250.
#'    <pre><code> set.seed(50)
#'      xVec <- sample(0:999, 250, replace = T)
#'      yVec <- sample(0:999, 250, replace = T)</code></pre>  
#'    Suppose $\mathbf{x} = (x_1, x_2, ..., x_n)$ denotes the vector 'xVec' and similarly
#'    for $\mathbf{y}$. 
#'    
#'      a. Create the vector $(y_2 - x_1, ..., y_n - x_{n-1})$
#'      b. Create the vector $\left({\frac{\sin(y_1)}{\cos(x_2)},\frac{\sin(y_2)}{\cos(x_3)},...,\frac{\sin(y_{n-1})}{\cos(x_n)} }\right)$  
#'      c. Create the vector $(x_1 + 2x_2 - x_3, x_2 + 2 x_3 - x_4, ..., x_{n-1} + 2x_{n-1} - x_n)$
#'      d. Calculate $\sum_{i=1}^{n-1}\left.\frac{e^{-x_{i+1}}}{x_i + 10}\right.$
#'      
#' 7. [Warwick] This question uses the vectors 'xVec' and 'yVec' created in the previous question and the 
#'    function 'sort', 'order', 'mean', 'sqrt', 'sum' and 'abs'.
#'    
#'      a. Pick you the value in 'yVec' which are > 100.
#'      b. What are the index positions in 'yVec' of the values which are > 600?
#'      c. What are the values in 'xVec' which correspond to the values in 'yVec' which are >600?
#'      d. Create the vector 
#'         $\left(
#'         \left|x_1-\bar{\mathbf{x}}\right|^{1/2},
#'         \left|x_2-\bar{\mathbf{x}}\right|^{1/2},...,
#'         \left|x_n-\bar{\mathbf{x}}\right|^{1/2}\right)$      
#'      e. How many values in 'yVec' are within 200 of the maximum value of the terms in 'yVec'?
#'      f. Sort the numbers in the vector 'xVec' in the order of increasing values in 'yVec'.
#'      g. How many numbers in 'xVec' are divisible by 2?
#'      h. Pick out the elements in 'yVec' at index positions 1,4,7,10,13,...
#'      
#' 8. [Warwick] By using the function 'cumprod' or otherwise, calculate 
#'    $$ 1 + \frac{2}{3} +\frac{2}{3}\frac{4}{5} + \frac{2}{3}\frac{4}{5}\frac{6}{7}+...+\frac{2}{3}\frac{4}{5}...\frac{38}{39}$$
#'
#' # 3 Statistical and mathematical functions
#' 
#' ## 3.1 Probability distributions and random number generation
#' 
#' ## 3.2 Mathematical functions
#' 
#' ## 3.3 Matrix operations
#' 
#' ## 3.4 Examples
#' 
#' ## 3.X Exercises
#' 
#' 1. Suppose 
#'    $$\mathbf{A}= \begin{bmatrix} 1 & 1 & 1 \\ 5 & 2 & 6 \\ -1 & -1 & -3\end{bmatrix}$$
#'    
#'       a. Check that $\mathbf{A}^3 = \mathbf{0}$ where $\mathbf{0}$ is a $3 \times 3$ matrix with every entry equal to 0.
#'       b. Replace the third column of $\mathbf{A}$ by the sum of the second and third columns.
#'    
#' 2. Create the following matrix $\mathbf{B}$ with 15 rows:
#'    $$\mathbf{A}= \begin{bmatrix} 10 & -10 & 10 \\ 10 & -10 & 10 \\ \vdots & \vdots & \vdots \\10 & -10 & 10\end{bmatrix}$$
#'    Calculate the $3 \times 3$ matrix $\mathbf{B}^T\mathbf{B}$. Consider: ?crossprod
#'    
#' 3. Create a $6 \times 6$ matrix 'matE' with every entry equal to 0. Check what the functions
#'    'row' and 'col' return when applied to 'matE'. Hence create the $6 \times 6$ matrix:
#'    $$\begin{bmatrix} 
#'    0 & 1 & 0 & 0 & 0 & 0 \\ 1 & 0 & 1 & 0 & 0 & 0 \\
#'    0 & 1 & 0 & 1 & 0 & 0 \\ 0 & 0 & 1 & 0 & 1 & 0 \\
#'    0 & 0 & 0 & 1 & 0 & 1 \\ 0 & 0 & 0 & 0 & 1 & 0 \end{bmatrix}$$ 
#' 
#' 4. Look at '?outer'. Hence create the following patterned matrix:
#'    $$\begin{bmatrix} 
#'    0 & 1 & 2 & 3 & 4 & 5 \\ 1 & 2 & 3 & 4 & 5 & 6 \\
#'    2 & 3 & 4 & 5 & 6 & 7 \\ 3 & 4 & 5 & 6 & 7 & 8 \\
#'    4 & 5 & 6 & 7 & 8 & 9 \\ 5 & 6 & 7 & 8 & 9 & 10 \end{bmatrix}$$ 
#' 
#' 5. Create the following patterned matrices. In each case, your solution should make use of the special form
#'    of the matrix -- this means that the solution should easily generalize to creating a larger matrix with the
#'    same structure and should not involve typing in all the entries in the matrix.
#'       a. $\begin{pmatrix} 
#'          0 & 1 & 2 & 3 & 4 & 5 \\ 1 & 2 & 3 & 4 & 5 & 0 \\
#'          2 & 3 & 4 & 5 & 0 & 1 \\ 3 & 4 & 5 & 0 & 1 & 2 \\
#'          4 & 5 & 0 & 1 & 2 & 3 \\ 5 & 0 & 1 & 2 & 3 & 4 \end{pmatrix}$
#'       b. $\begin{pmatrix} 
#'          0 & 5 & 4 & 3 & 2 & 1 \\ 1 & 0 & 5 & 4 & 3 & 2 \\
#'          2 & 1 & 0 & 5 & 4 & 3 \\ 3 & 2 & 1 & 0 & 5 & 4 \\
#'          4 & 3 & 2 & 1 & 0 & 5 \\ 5 & 4 & 3 & 2 & 1 & 0 \end{pmatrix}$
#'          
#' 6. Solve the following system of linear equations in five unknowns
#'    $$\begin{eqnarray}
#'    x_1 + 2x_2 + 3x_3 + 4x_4 +5 x_5 &=& 7 \\
#'    2x_1 + x_2 + 2x_3 + 3x_4 +4 x_5 &=& -1 \\
#'    3x_1 + 2x_2 + x_3 + 2x_4 +3 x_5 &=& -3 \\
#'    4x_1 + 3x_2 + 2x_3 + x_4 +2 x_5 &=& 5 \\
#'    5x_1 + 4x_2 + 3x_3 + 2x_4 +x_5 &=& 17 
#'    \end{eqnarray}$$
#'    by considering and appropriate matrix equation $\mathbf{A}\mathbf{x}=\mathbf{y}$.<br>
#'    Make use of the special form of the matrix $\mathbf{A}$. The method used for the solution should easily
#'    generalize to a larger set of equations where the matrix $\mathbf{A}$ has the same structure.
#'    
#' 7. Create a $6 \times 10$ matrix of random integers chose from $1,2,...10$ by executing the folllowing two lines 
#'    of code:<pre><code>
#'    set.seed(75)
#'    aMat <- matrix( sample(10, size = 60, replace = T), nr = 6)
#'    </code></pre>
#'    
#'       a. Find the number of entries in each row which are greater than 4.
#'       b. Which rows contain exactly two occurrences of the number seven.
#'       c. Find those pairs of columns wose total (over both columns) is greater than 75.
#'          The answer should be a matrix with two columns; so, for example, 
#'          the row (1,2) in the output matrix means
#'          that the sum of columns 1 and 2 in the original matrix is greater than 75. 
#'          Repeating a column
#'          is permitted; so, for example, the final output matrix 
#'          could contain the rows (1,2),(2,1) and (2,2).<br>
#'          What if repetitions are not permitted? Then, only (1,2) from (1,2), (2,1) and (2,2)
#'          would be permitted.
#' 
#' 8. Calculate:
#' 
#'        a. $\sum_{i=1}^{20} \sum_{j=1}^{5} \frac{i^4}{(3+j)}$
#'        b. (Hard) $\sum_{i=1}^{20} \sum_{j=1}^{5} \frac{i^4}{(3+ij)}$
#'        c. (Even harder!) $\sum_{i=1}^{10} \sum_{j=1}^{i} \frac{i^4}{(3+ij)}$
#'          
#' # 4 Programming and operating system interface
#' 
#' ## 4.1 Control flow, programming, and data generation
#' 
#' ## 4.2 Functions and macros
#' 
#' ## 4.3 Interactions with the operating system
#' 
#' ## 4.X Exercises
#' 
#' 1.
#' 
#'     a. Write functions 'tmpFn1' and 'tmpFn2' such that if 'xVec' is the vector
#'        $(x_1, x_2, ..., x_n)$,
#'        then 'tmpFn1(xVec)' returns the vector
#'        $(x_1,x_2^2,...,x_n^n)$ 
#'        and 'tmpFn2(xVec)' returns the vector
#'        $\left({x_1,\frac{x_2^2}{2},...,\frac{x_n^n}{n}}\right)$ 
#'     b. Now write a function 'tmpFn3' which takes two arguments $x$ and $n$ where $x$ is a
#'        single number and $n$   is a strictly positive integer. The function should return
#'        the value of
#'        $$1 + \frac{x}{1}  + \frac{x^2}{2}  + \frac{x^3}{3}  + ... + \frac{x^n}{n}$$
#'        
#' 2. Write a function 'tmpFn(xVec)' such that if 'xVec' is the vector 
#'    $\mathbf{x}=(x_1,...,x_n)$
#'    then 'tmpFn(xVec)' returns the vector of moving averages:
#'    $$\frac{x_1 + x_2 + x_3}{3}, \frac{x_2 + x_3 + x_4}{3}, ... ,\frac{x_3 + x_4 + x_5}{3}$$ 
#'    Try out your function; for example, try 'tmpFn( c(1:5,6:1))'
#'    
#' 3. Consider the continuous function:
#'    $$f(x) =  
#'    \begin{cases}
#'    x^2 + 2x + 3 & \quad \text{if } x < 0 \\
#'    x+3          & \quad \text{if } 0 \le x \lt 2 \\ 
#'    x^2 + 4x - 7 & \quad \text{if } 2 \le x \\
#'    \end{cases}$$
#'    Write a function 'tmpFn' which takes a single argument 'xVec'. The function should 
#'    return the vector of values of the function $f(x)$ evaluated at the values in 'xVec'.<br>
#'    Hence plot the function $f(x)$ for $-3 \lt x \lt 3$.
#'    
#' 4. Write a function which takes a single argument which is a matrix. The function should
#'    return a matrix which is the same as the function argument but every odd number is doubled.
#'    
#' 5. Write a function which takes two arguments 'n' and 'k' which are positive integers.
#'    It should return the $n \times n$ matrix:
#'    $$\begin{bmatrix}
#'    k & 1 & 0 & 0 & \cdots & 0 & 0 \\
#'    1 & k & 1 & 0 & \cdots & 0 & 0 \\
#'    0 & 1 & k & 1 & \cdots & 0 & 0 \\
#'    0 & 0 & 1 & k & \cdots & 0 & 0 \\
#'    \vdots & \vdots & \vdots & \vdots & \ddots & \vdots & \vdots \\
#'    0 & 0 & 0 & 0 & \cdots & k & 1 \\
#'    0 & 0 & 0 & 0 & \cdots & 1 & k \\
#'    \end{bmatrix}$$
#'    _Hint:_ First try to do it for a specific case such as $n=5$ and $k=2$ on the Command Line.
#'    
#' 6. Suppose an angle $\alpha$ is given as a positive real number of degrees counting counter-clockwise
#'    from the positive horizontal axis. Write a function 'quadrant(alpha)' which returns
#'    the quadrant, 1, 2, 3 or 4, corresponding 'alpha'.
#'    
#' 7. 
#'       a. Zeller's congruence is the formula:
#'          $$f = ([2.6m-0.2] + k + y + [y/4] + [c/4] - 2c) \mod 7$$
#'          where $[x]$ denotes the integer part of $x$; for example $[7.5]=7$.<br>
#'          Zeller's congruence returns the day of the week $f$ given:<br>
#'               $k =$ the day of the month,<br>
#'               $y =$ the year in the century,<br>
#'               $c =$ the first 2 digits of the year (the century number)<br>
#'               $m =$ the month number (where January is month 11 of the preceding year, February is month 12 of the preceding year, March is month 1, etc)
#'          For example, the data July 21, 1963 has $m=5, k = 21, c=19, y = 63$; while the date
#'          February 21, 1963 has $m=12, k=21, c=19$ and $y=62$.<br>
#'          Write a function 'weekday(day, month, year)' which returns the day of the week
#'          when given the numerical inputs of the day, month and year.<br>
#'          Note that the value 1 for $f$ denotes Sunday, 2 denotes Monday, etc.
#'       b. Does your function work if the input parameters 'day', 'month' and 'year' are
#'          vectors with the same length and with valid entries?
#'          
#'          
#'                 
#'               
#' # 5 Common Statistical Procedures
#' 
#' ## 5.1 Summary statistics
#' 
#' ## 5.2 Bivariate statistics
#' 
#' ## 5.3 Contingency tables
#' 
#' ## 5.4 Tests for continuous variables
#' 
#' ## 5.5 Analytic power and sample size calculations
#' 
#' ## 5.6 Further resources
#' 
#' ## 5.7 Examples
#' 
#' ## 5.X Exercises
#' 
#' # 6 Linear regression and ANOVA
#' 
#' ## 6.1 Model fitting
#' 
#' ## 6.2 Test, contrasts, and linear functions of parameters
#' 
#' ## 6.3 Model diagnostics
#' 
#' ## 6.4 Model parameters and results
#' 
#' ## 6.5 Further resources
#' 
#' ## 6.6 Examples
#' 
#' ## 6.X Exercises
#' 
#' # 7 Regression generalizations and modeling
#' 
#' ## 7.1 Generalized linear models
#' 
#' ## 7.2 Further generalizations
#' 
#' ## 7.3 Robust methods
#' 
#' ## 7.4 Models for correlated data
#' 
#' ## 7.5 Survival analysis
#'
#' ## 7.6 Multivariate statistics and discriminant procedures
#' 
#' ## 7.7 Complex survey design
#' 
#' ## 7.8 Model selection and assessment 
#' 
#' ## 7.9 Further resources
#' 
#' ## 7.10 Examples
#' 
#' ## 7.X Exercises
#' 
#' # 8 A graphical compendium
#' 
#' ## 8.1 Univariate plots
#' 
#' ## 8.2 Univariate plots by grouping variable
#' 
#' ## 8.3 Bivariate plots
#' 
#' ## 8.4 Multivariate plots
#' 
#' ## 8.5 Special purpose plots
#' 
#' ## 8.6 Further resources
#' 
#' ## 8.7 Examples
#' 
#' ## 8.X Exercises
#' 
#' # 9 Graphical options and configuration
#' 
#' ## 9.1 Adding elements
#' 
#' ## 9.2 Options and parameters
#' 
#' ## 9.3 Saving graphs
#' 
#' ## 9.X Exercises
#' 
#' # 10 Simulation
#' ## 10.1 Generating data
#' 
#' ## 10.2 Simulation application
#' 
#' ## 10.3 Further resources
#' 
#' ## 10.X Exercises
#' 
#' # 11 Special topics
#' 
#' ## 11.1 Processing by group
#' 
#' ## 11.2 Simulation-based power calculations
#' 
#' ## 11.3 Reproducible analysis and output
#' 
#' ## 11.4 Advanced statistical methods
#' 
#' ## 11.5 Further resources
#' 
#' # 12 Case studies
#' 
#' ## 12.1 Data management and related tasks
#' 
#' ## 12.2 Read variable format file
#' 
#' ## 12.3 Plotting maps
#' 
#' ## 12.4 Data scraping and visualization
#' 
#' ## 12.5 Manipulating bigger datasets
#' 
#' ## 12.6 Constrained optimization: the knapsack problem
#' 
#' ## 12.X Exercises
#' 
#' # Appendix
#' 
#' ## Pitfalls
#' 
#' ## How tos - not yet categorized
#' 