#' ---
#' title: "R Notes to Supplement the SAS and R textbook"
#' author: |
#'     | Georges Monette and MATH 4939 Class
#' date: "January 7, 2017"
#' output:
#'     html_document:
#'         toc: true
#'         toc_depth: 6
#'         keep_md: true
#' ---
#' 
#' Output produced on: `r format(Sys.time(), '%B %d, %Y at %H:%M')`
#+ include=FALSE
knitr::opts_chunk$set(comment = '', cache = TRUE)
#'
#' <!--
#' Incorporate Warwick Exercises: 
#'  -- Part 4: Harder functions and Part 5: Data frame, list, array and time series
#'  -- Migrate other R material
#' -->
#' 
#' # Introduction
#' 
#' The topics in these notes are organized to parallel the material in 
#' Kleinman and Horton (2015) "SAS and R: Data Management,
#' Statistical Analysis and Graphics, Second Edition" CRC Press.
#' 
#' Material that is relevant to a particular section is entered under that 
#' section's number and title. Sections that have no
#' additional material have (--) appended to their titles.
#' 
#' Material that belongs to a chapter but not to any section in the book appears under 
#' additional 'lettered' sections, e.g. 1.1.A etc.
#' 
#' Exercises from various sources are appended to each chapter under the 
#' section lettered 'X', e.g. 1.X
#' 
#' ## Discussion and Solutions
#' 
#' Links to notable materials and discussions about these notes can
#' take place on our [wiki](http://capstone.stats.yorku.ca/index.php/R_Notes_Discussion).
#' 
#' Solutions to exercises should be posted [here](http://capstone.stats.yorku.ca/index.php/R_Notes_Solutions).
#' 
#+ eval=FALSE
install.packages('haven')
install.packages('readxl')
install.packages('rgl')
install.packages('devtools')
devtools::install_github('gmonette/spida2')
devtools::install_github('gmonette/p3d')
#'
#' # 1. Data input and output
#' 
#' ## 1.1 Input
#' 
#' ### 1.1.6 Read sheets from an Excel file
#' 
#' I believe that currently (early January 2017) may be
#' __(but see some reservatons below)__
#' the most effective way to read an Excel file
#' is to use Hadley Wickam's 'readxl' CRAN package.  If 'file.xlsx' is an Excel file and you
#' want to read the second worksheet that uses 'NA' for missing values, use:
#+ eval=FALSE
library(readxl)
dd <- read_excel('file.xlsx', sheet = 2, na = 'NA')
dd <- as.data.frame(as.list(dd))
#'
#' The last line is important if you want a data frame in the usual format -- e.g  with factors
#' for categorical variables. 
#' The file created by '<tt>read_excel</tt>' has class 'tibble' which is not used outside the
#' Hadleyverse.
#' 
#' Sometimes, <tt>read_excel</tt> will report a warning like: <tt>... expecting numeric: got ...</tt>.
#' This happens when <tt>read_excel</tt> has decided that a column is numeric based on its inspection of the 
#' top entries but then encounters non-numeric data. The remedy is to read the file as character and to 
#' modify the entries that need to be modified.  See Section 2.2.14 on how to use regular expressions to fix 
#' variables __without touching the original data__.
#' 
#' Reread the data this way:
#+ eval=FALSE
dd <- read_excel('file.xlsx', sheet = 2, na = 'NA', col_type = rep('text', ncol(dd)))
dd <- as.data.frame(as.list(dd))
#' All variables will now be factors.
#' You need to go through them and modify them accordingly.
#' If a variable <tt>x</tt>, say, should be numeric, and __does not need any editing__, you can fix it with:
#+ eval=FALSE
z <- as.numeric(as.character(dd$x))  # note that 'as.character' is ESSENTIAL
z # have a look
dd$x <- z # if everything is ok
#' If a variable needs editing, for example suppose student numbers that should have 9 digits
#' have been entered in a variety of ways: '123 456 789', or '123-456-789', or '#12346789' or with the wrong number
#' of digits, you could do this:
#' 
#+ eval=FALSE
dd$x.orig <- dd$x  # keep the original in case you need to go back and check
z <- as.character(dd$x)
# Have a look:
z
z <- gsub('[ -]','', z)  # remove all blanks and hyphens. 
    # Note that the hyphen must be first or last in the brackets, 
    # otherwise it denotes a range, i.e. '[A-Z]' matches any
    # capital letter.
z <- sub('^#','', z)  # remove leading # signs
z # have another look
table(nchar(z))
z9 <- nchar(z) == 9
z <- ifelse(z9, as.numeric(z), NA)
dd$x <- z # Fixed! Invalid input is NA
#'
#' To write Excel files consider using the <tt>writeXLS</tt> package. I have no
#' experience with it. If you use it post your observations in the 
#' [wiki](http://capstone.stats.yorku.ca/index.php/R_Notes_Discussion).
#'
#' #### Annoyance in 'readxl'
#' 
#' Using <tt>read_excel</tt> to read a file with 9-digit student numbers 
#' as text because some were 
#' entered incorrectly by students produced the following output:
#' 
#' ![](images/readxl_bug.JPG)
#' 
#' The 9-digit numbers appear to have been transformed to scientific notation for no apparent
#' reason because they were read as 'text'.
#' The transformation back to numeric variables works correctly for 9 digits. One would need
#' to experiment with more digits in the input.  It's annoying that the string is altered in a way
#' that seems unnecessary. 
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
#' ## 1.1.X Exercises--
#' 
#' ## 1.2 Output--
#' 
#' ## 1.3 Further resources--
#' 
#' # 2 Data management
#' 
#' ## 2.1 Structure and meta-data--
#' 
#' ## 2.2 Derived variable and data manipulation
#' 
#' ### 2.2.14 Replace strings within string variables
#' 
#' Expertise with regular expressions is one of the most valuable skills you
#' can learn for data manipulation. 
#' 
#' Here's a (http://www.regexr.com/)[site you can use to experiment with regular expressions].
#' Add it to your R editing bookmarks.
#' 
#' Contribute to the (http://capstone.stats.yorku.ca/index.php?title=Regular_Expressions)[class wiki's discussion of regular expressions].
#' 
#' Here's a summary prepared by Jeff Lee in the Winter 2016 class.
#' 
#' #### Basic Regular Expressions 
#' 
#' Using regular expressions is a way to alter, search, count, adjust texts or strings of characters.
#' 
#' There are 3 main R functions that use regular expressions that we will look at.
#' <pre>
#' grep, grepl 
#' sub, gsub 
#' </pre>
#' First look at the function <tt>grep</tt>
x <- c("Hello", "He", "Hel", "hello", "hel1")
grep("hel", x)
#' As you can see, <tt>grep</tt> returns the index of all elements of <tt>x</tt> that contain "hel".
#' It does not return the index of  "Hello" because <tt>grep</tt> is case sensitive. 
#' 
#' We say the _pattern_ <tt>"hel"</tt> _matches_ substrings in the target.  
#' To ignore the case, we can use:
grep("hel", x, ignore.case = T)
#' A similar effect is achieved by using square brackets: <tt>[]</tt>. A pattern consisting 
#' of a sequence of characters enclosed
grep("[Hh]el", x)
#' Suppose you want to know how many of these elements of <tt>x</tt> contain <tt>"hel"</tt>
#' or <tt>"Hel"</tt>
length(grep("[hH]el", x))
#' If you want to see the actual strings matched instead of their indices, use
grep("[Hh]el", x, value = TRUE)
#' Finally is you want a logical index vector:
grepl("[Hh]el", x)
#'
#' ##### Taking a look at gsub
#' 
#' <tt>gsub</tt> and <tt>sub</tt> are great ways to modify substrings in a reproducible way. 
#' For example, you can use them to modify variable names in a way that will work when
#' you receive an updated version of a data set. In most data sets, you will
#' have variables names that are acrynoms or short forms and you want to replace those variable names with
#' something that people will understand.
#' 
#' The difference between <tt>sub</tt> and <tt>gsub</tt> is that <tt>sub</tt> will replace only the first match, 
#' <tt>gsub</tt> (g stands for global) will 
#' replace all matches. Compare:
sub("l","WWW", x)
gsub("l","WWW", x)
#' 
#' The most difficult part about regular expressions is the syntax. 
#' These are helpful websites with information on syntax. 
#' 
#' * [Quick-Start: Regex Cheat Sheet](http://www.rexegg.com/regex-quickstart.html)
#' * [Regular Expressions in R by Albert Y. Kim](https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html)
#' * [RegExr](http://www.regexr.com/) to interactively try out regular expressions
#' 
#' There's a thorough treatment at [Microsoft's Regular Expression Language -- Quick Reference](https://msdn.microsoft.com/en-us/library/az24scfc(v=vs.110).aspx)
#' 
#' Also you can get help in R:
#+ eval=FALSE
?regex
?gsub
#' There are many special characters that let you do almost anything you want with regular 
#' expressions. Here are the most important ones:
#' 
#' * Special characters: All characters match themselves except the special characters:
#'   <tt>. $ ^ { [ ( | ) * + ? \\</tt>. Also <tt>} ]</tt> are special characters when they close 
#'   a matching brace
#'   and <tt>-</tt> is a special character when it appears within square brackets.
#' * Special matching characters:
#'      * <tt>.</tt>: a period matches any single character
#'      * <tt>[abc]</tt>: matches any single character in the list
#'      * <tt>[A-Z]</tt>: matches a single character in the range A to Z
#'      * <tt>[A-Za-z0-9]</tt>: matches any single alphanumeric character
#'      * <tt>[^a-z]</tt>: matches any single character that is NOT a lower case letter. The caret
#'        <tt>^</tt> at the beginning of the bracketed list negates the rest of the list. A caret anywhere
#'        else is just a caret.
#' * Anchors:
#'      *  <tt>\^</tt> matches the beginning and <tt>\$</tt> matches the end of a string. Thus 
#'         <tt>\"\^and"</tt> matches only strings that start with \"and", <tt>\"and\$"</tt> matches
#'         only strings that end with "and".  To only get exact matches, i.e. string that are
#'         exactly equal to a pattern, use both <tt>\^</tt> and <tt>\$</tt>: <tt>\"\^match this exactly\$"</tt>.
#'         
#' * Quantifiers:
#'      *  \* matches the previous match 0 or more times
#'      *  \+ matches the previous match one or more times
#'      *  ? matches the previous match zero or one time
#'      * {n} matches the previous match exactly n times
#'      * {n,m} matches the previous match n to m times
#'      * {n,} matches the previous match at least n times
#'      * {},m} matches the previous match at most m times
#'      
#'      Quantifiers are 'greedy' in the sense that they will match as much of the string as they can.
#'      Adding '?' to a quantifier makes it 'lazy'. It will match as few occurences as possible.
#' 
#' A powerful tool for substitution is the 'backreference' <tt>\\N</tt> where <tt>N</tt> is a single
#' digit from 1 to 9.  In a replacement string <tt>\\N</tt> refers to the Nth parenthesized 
#' expression in the pattern. For example:
x <- c('Wong, Rodney','Smith,   John', 'Robert Jones')
sub("^([^, ]+), +([^ ]*)$", "\\2 \\1", x)
#' 
#' 
#' In order to match a special character it needs to be escaped with a backslach '\\' before the
#' character.
#' 
#' Some examples of <tt>gsub</tt>:
#' 
s <- ("HEL$LO")
s
gsub("$", replacement = ".", s)
gsub("\\$", replacement = ".", s)

#' As you can see, using two back slashes will actually replace $ with a period
#' In a string in R you need to use two backslashes to produce one backslash, i.e.
#' you need to escape the escape. Try the command: <tt>gsub(\"\\$\", replacement = \".\", s)</tt>
y <- c("hello123","hello213","hel2222lo","llo he123" )
gsub(".*2", "", y) # Note that "" will delete everything
#' This will remove everything up to and including a 2 in each string. 
#' As you can see in hel2222lo, it removes 
#' the last 2.
gsub("^hel2","4939", y)
#' The ^ will replace everything that starts with hel2. In this case only the 3rd word started with hel2 so
#' it replaces it with 4939.
gsub("213$","4939", y)
#' The $ will replace everything that ends with 213. In this case only the 2nd word ended with 213 so
#' it replaces it with 4939.
gsub("\\bhe","4939", y)
#' The double backslash b will replace everything that starts at with 'he' on words instead of strings. 
#' In this case, every word had a 'he' in this case.
gsub("hel*1", "4939", y)
#' The * will replace anything that matches at least 0 times. In this case, the last word matches hel and 1
#' matches 0 times.
#' 
#' The special character <tt>|</tt> allows alternative choices. It matches either what comes
#' before the <tt>|</tt> or what comes after it.
gsub("hel|213", "4939", y)
#' The <tt>|</tt> is an 'or' feature. This pattern will replace anything with a <tt>hel</tt> or <tt>213</tt>.
#' If it can match bother <tt>hel</tt> and <tt>213</tt>
#' it will replace both.
#'
#' Note that you can use and mix quantifiers and operators together.
#' Perhaps the most common combination is <tt>.*</tt> which matches anything
#'
#' ##### Taking a look at regexpr
#' 
y <- c("hello123","hello213","hel2222lo","llo he123","zork")
regexpr("he(.*)", y)
#' <tt>regexpr</tt> returns the position of the first character matched.<br>
#' <tt>attr(,"match.length")</tt> is the number of characters matched in each string, -1 if no match.
regexpr("hel(.*)", y)
#' As you can see, the 4th word does not have <tt>hel</tt> in it.
gregexpr("he(.*)", y)
#' <tt>gregexpr</tt> will return a list of all the matches. 
#' 
#' ## 2.3 Merging, combining and subsetting datasets
#' 
#' ### 2.3.7 Convert from wide to long (tall) format
#' 
#' Since reshaping longitudinal data efficiently is an important operation in the analysis of 
#' longitudinal and hierarchical data the <tt>spida2</tt> package has two functions, <tt>tolong</tt>
#' and <tt>towide</tt> to facilitate the process.
#' 
#' Reshaping is easiest when variable names are formatted appropriately which, in turn, is often made quite
#' easy with regular expressions.
#' 
#' Look at help files for <tt>tolong</tt> and <tt>towide</tt> in the spida2 package for examples on the use
#' of these functions.
#' 
#' ## 2.4 Date and time variables--
#' 
#' ## 2.5 Further resources--
#' 
#' ## 2.6 Examples--
#' 
#' ## 2.X Exercises
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
#'     a. <tt>("label 1", "label 2", ... , "label 30")</tt>. 
#'        Note that there is a single space
#'        between <tt>label</tt> and the number following.
#'     b. <tt>("fn1", "fn2", ..., "fn30")</tt>. In this case there is no space.
#'     
#' 6. [Warwick] Execute the following lines which create two vectors of random integers which 
#'    are chosen with replacement from the integers 0, 1, ..., 999. Both vectors have length 250.
#'    <pre><code> set.seed(50)
#'      xVec <- sample(0:999, 250, replace = T)
#'      yVec <- sample(0:999, 250, replace = T)</code></pre>  
#'    Suppose $\mathbf{x} = (x_1, x_2, ..., x_n)$ denotes the vector <tt>>xVec</tt> and similarly
#'    for $\mathbf{y}$. 
#'    
#'      a. Create the vector $(y_2 - x_1, ..., y_n - x_{n-1})$
#'      b. Create the vector $\left({\frac{\sin(y_1)}{\cos(x_2)},\frac{\sin(y_2)}{\cos(x_3)},...,\frac{\sin(y_{n-1})}{\cos(x_n)} }\right)$  
#'      c. Create the vector $(x_1 + 2x_2 - x_3, x_2 + 2 x_3 - x_4, ..., x_{n-1} + 2x_{n-1} - x_n)$
#'      d. Calculate $\sum_{i=1}^{n-1}\left.\frac{e^{-x_{i+1}}}{x_i + 10}\right.$
#'      
#' 7. [Warwick] This question uses the vectors <tt>xVec</tt> and <tt>yVec</tt> created in the previous question and the 
#'    function '<tt>sort</tt>', '<tt>order</tt>', '<tt>mean</tt>', '<tt>sqrt</tt>', '<tt>sum</tt>' and '<tt>abs</tt>'.
#'    
#'      a. Pick you the value in '<tt>yVec</tt>' which are > 100.
#'      b. What are the index positions in '<tt>yVec</tt>' of the values which are > 600?
#'      c. What are the values in '<tt>xVec</tt>' which correspond to the values in '<tt>yVec</tt>' which are >600?
#'      d. Create the vector 
#'         $\left(
#'         \left|x_1-\bar{\mathbf{x}}\right|^{1/2},
#'         \left|x_2-\bar{\mathbf{x}}\right|^{1/2},...,
#'         \left|x_n-\bar{\mathbf{x}}\right|^{1/2}\right)$      
#'      e. How many values in '<tt>yVec</tt>' are within 200 of the maximum value of the terms in '<tt>yVec</tt>'?
#'      f. Sort the numbers in the vector '<tt>xVec</tt>' in the order of increasing values in '<tt>yVec</tt>'.
#'      g. How many numbers in '<tt>xVec</tt>' are divisible by 2?
#'      h. Pick out the elements in '<tt>yVec</tt>' at index positions 1,4,7,10,13,...
#'      
#' 8. [Warwick] By using the function '<tt>cumprod</tt>' or otherwise, calculate 
#'    $$ 1 + \frac{2}{3} +\frac{2}{3}\frac{4}{5} + \frac{2}{3}\frac{4}{5}\frac{6}{7}+...+\frac{2}{3}\frac{4}{5}...\frac{38}{39}$$
#'
#' 9. [Regular expressions] Suppose money data for a variable has been entered 
#'    in a variety of
#'    formats: 
#'    <pre>"$1,000.00", "1000.00", "123.2$"</pre>
#'    Write an R function using 'gsub' and
#'    'as.numeric' to turn these various entries into a numeric variable. Experiment with your 
#'    function to make sure it works.
#'  
#' 10. [Regular expressions] Write a function that takes a character vector and collapses
#'     multiple adjoining blanks into a single blank.
#'     
#' 11. [Regular expressions] Use the file [SampleClassFile.csv](SampleClassFile.csv). One of its variables is a
#'     string that contains information about a student's faculty and programme: are they in
#'     an ordinary programme or in an honours program and the department of their major and
#'     minor.  Write a function that uses regular expression to create four new variables:
#'     the faculty in which a student is enrolled, whether they are in an ordinary or in
#'     an honours programme, their major program and their minor program if any. 
#'     
#' 12. [Merging and reshaping] Use the site Gapminder.org to download at least three longitudinal variables
#'     into separate data sets. Merge the data sets into one for which each row represents
#'     one country and year and contains the values of each of the three variables you downloaded.
#' 
#' 13. [Merging and reshaping] Use the site Gapminder.org to download at least three longitudinal variables
#'     into separate data sets. Merge the data sets into one for which each row represents
#'     one country and year and contains the values of each of the three variables you downloaded.
#' 
#' 14. [Regular expressions] Write a function that removes every variable whose
#'     name starts with the letter 'X' and ends in a number from a data frame.
#'     
#' 15. [Data] Write a function that takes a data frame and returns it with
#'     variable names in alphabetical order.     
#' 
#' # 3 Statistical and mathematical functions
#' 
#' ## 3.1 Probability distributions and random number generation--
#' 
#' ## 3.2 Mathematical functions--
#' 
#' ## 3.3 Matrix operations--
#' 
#' ## 3.4 Examples--
#' 
#' ## 3.X Exercises
#' 
#' 1. [Warwick] Suppose 
#'    $$\mathbf{A}= \begin{bmatrix} 1 & 1 & 1 \\ 5 & 2 & 6 \\ -1 & -1 & -3\end{bmatrix}$$
#'    
#'       a. Check that $\mathbf{A}^3 = \mathbf{0}$ where $\mathbf{0}$ is a $3 \times 3$ matrix with every entry equal to 0.
#'       b. Replace the third column of $\mathbf{A}$ by the sum of the second and third columns.
#'    
#' 2. [Warwick] Create the following matrix $\mathbf{B}$ with 15 rows:
#'    $$\mathbf{A}= \begin{bmatrix} 10 & -10 & 10 \\ 10 & -10 & 10 \\ \vdots & \vdots & \vdots \\10 & -10 & 10\end{bmatrix}$$
#'    Calculate the $3 \times 3$ matrix $\mathbf{B}^T\mathbf{B}$. Consider: <tt>?crossprod</tt>
#'    
#' 3. [Warwick] Create a $6 \times 6$ matrix '<tt>matE</tt>' with every entry equal to 0. Check what the functions
#'    '<tt>row</tt>' and '<tt>col</tt>' return when applied to '<tt>matE</tt>'. Hence create the $6 \times 6$ matrix:
#'    $$\begin{bmatrix} 
#'    0 & 1 & 0 & 0 & 0 & 0 \\ 1 & 0 & 1 & 0 & 0 & 0 \\
#'    0 & 1 & 0 & 1 & 0 & 0 \\ 0 & 0 & 1 & 0 & 1 & 0 \\
#'    0 & 0 & 0 & 1 & 0 & 1 \\ 0 & 0 & 0 & 0 & 1 & 0 \end{bmatrix}$$ 
#' 
#' 4. [Warwick] Look at <tt>?outer</tt>. Hence create the following patterned matrix:
#'    $$\begin{bmatrix} 
#'    0 & 1 & 2 & 3 & 4 & 5 \\ 1 & 2 & 3 & 4 & 5 & 6 \\
#'    2 & 3 & 4 & 5 & 6 & 7 \\ 3 & 4 & 5 & 6 & 7 & 8 \\
#'    4 & 5 & 6 & 7 & 8 & 9 \\ 5 & 6 & 7 & 8 & 9 & 10 \end{bmatrix}$$ 
#' 
#' 5. [Warwick] Create the following patterned matrices. In each case, your solution should make use of the special form
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
#' 6. [Warwick] Solve the following system of linear equations in five unknowns
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
#' 7. [Warwick] Create a $6 \times 10$ matrix of random integers chose from $1,2,...10$ by executing the folllowing two lines 
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
#' 8. [Warwick] Calculate:
#' 
#'        a. $\sum_{i=1}^{20} \sum_{j=1}^{5} \frac{i^4}{(3+j)}$
#'        b. (Hard) $\sum_{i=1}^{20} \sum_{j=1}^{5} \frac{i^4}{(3+ij)}$
#'        c. (Even harder!) $\sum_{i=1}^{10} \sum_{j=1}^{i} \frac{i^4}{(3+ij)}$
#'          
#' # 4 Programming and operating system interface
#' 
#' ## 4.1 Control flow, programming, and data generation--
#' 
#' ## 4.2 Functions and macros--
#' 
#' ## 4.3 Interactions with the operating system--
#' 
#' ## 4.X Exercises
#' 
#' 1. [Warwick] 
#' 
#'     a. Write functions '<tt>tmpFn1</tt>' and '<tt>tmpFn2</tt>' such that if '<tt>xVec</tt>' is the vector
#'        $(x_1, x_2, ..., x_n)$,
#'        then '<tt>tmpFn1(xVec)</tt>' returns the vector
#'        $(x_1,x_2^2,...,x_n^n)$ 
#'        and '<tt>tmpFn2(xVec)</tt>' returns the vector
#'        $\left({x_1,\frac{x_2^2}{2},...,\frac{x_n^n}{n}}\right)$ 
#'     b. Now write a function '<tt>tmpFn3</tt>' which takes two arguments $x$ and $n$ where $x$ is a
#'        single number and $n$   is a strictly positive integer. The function should return
#'        the value of
#'        $$1 + \frac{x}{1}  + \frac{x^2}{2}  + \frac{x^3}{3}  + ... + \frac{x^n}{n}$$
#'        
#' 2. [Warwick] Write a function '<tt>tmpFn(xVec)</tt>' such that if '<tt>xVec</tt>' is the vector 
#'    $\mathbf{x}=(x_1,...,x_n)$
#'    then '<tt>tmpFn(xVec)</tt>' returns the vector of moving averages:
#'    $$\frac{x_1 + x_2 + x_3}{3}, \frac{x_2 + x_3 + x_4}{3}, ... ,\frac{x_3 + x_4 + x_5}{3}$$ 
#'    Try out your function; for example, try '<tt>tmpFn( c(1:5,6:1))</tt>'
#'    
#' 3. [Warwick] Consider the continuous function:
#'    $$f(x) =  
#'    \begin{cases}
#'    x^2 + 2x + 3 & \quad \text{if } x < 0 \\
#'    x+3          & \quad \text{if } 0 \le x \lt 2 \\ 
#'    x^2 + 4x - 7 & \quad \text{if } 2 \le x \\
#'    \end{cases}$$
#'    Write a function <tt>tmpFn</tt> which takes a single argument '<tt>xVec</tt>'. The function should 
#'    return the vector of values of the function $f(x)$ evaluated at the values in '<tt>xVec</tt>'.<br>
#'    Hence plot the function $f(x)$ for $-3 \lt x \lt 3$.
#'    
#' 4. [Warwick] Write a function which takes a single argument which is a matrix. The function should
#'    return a matrix which is the same as the function argument but every odd number is doubled.
#'    
#' 5. [Warwick] Write a function which takes two arguments '<tt>n</tt>' and '<tt>k</tt>' which are positive integers.
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
#' 6. [Warwick] Suppose an angle $\alpha$ is given as a positive real number of degrees counting counter-clockwise
#'    from the positive horizontal axis. Write a function <tt>quadrant(alpha)</tt> which returns
#'    the quadrant, 1, 2, 3 or 4, corresponding to '<tt>alpha</tt>'.
#'    
#' 7. [Warwick] 
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
#'          Write a function '<tt>weekday(day, month, year)</tt>' which returns the day of the week
#'          when given the numerical inputs of the day, month and year.<br>
#'          Note that the value 1 for $f$ denotes Sunday, 2 denotes Monday, etc.
#'       b. Does your function work if the input parameters '<tt>day</tt>', '<tt>month</tt>' and '<tt>year</tt>' are
#'          vectors with the same length and with valid entries?
#'          
#' 8. [Warwick] 
#'       a. Suppose $x_0=1$ and $x_1=2$ and
#'          $$x_j = x_{j-1}+\frac{2}{x_{j-1}} \qquad \text{for }j= 1,2,...$$
#'          Write a function '<tt>testLoop</tt>' which takes a single argument $n$ and returns
#'          the first $n-1$ values of the sequence $\{x_j\}_{j \ge 0}$, that is, the values of
#'          $x_0, x_1, x_2, ... , x_{n-2}$.
#'          
#'       b. Now write a function '<tt>testLoop2</tt>' which takes a single argument 
#'          '<tt>yVec</tt>' which is a vector.
#'          The function should return
#'          $$\sum_{j=1}^{n} e^j$$
#'          where $n$ is the length of '<tt>yVec</tt>'.
#'          
#' 9. [Warwick] _Solution of the difference equation_ 
#'    $x_n = r x_{n-1}(1 - x_{n-1})$ 
#'    _with starting values_ $x_1$.
#'    
#'       a. Write a function '<tt>quadmap(start, rho, niter)</tt>' which returns the vector
#'          $(x_1, ....,. x_n)$
#'          where
#'          $x_k=r x_{k-1}(1 - x_{k-1})$ and <br>
#'          $\quad$ '<tt>niter</tt>' denotes $n$,<br>
#'          $\quad$ '<tt>start</tt>' denotes $x_1$, and<br>
#'          $\quad$ '<tt>rho</tt>' denotes $r$.<br>
#'          Try out the function you have written:
#'            -  for $r=2$ and $0 < x_1 < 1$ you should get 
#'               $x_n \rightarrow 0.5$ as 
#'               $n \rightarrow \infty$.
#'            -  try '<tt>tmp <- quadmap(start=0.95, rho=2.99, niter=500)</tt>'
#'            
#'          Now switch back to the Commands window and type:
#'          <pre><code>plot(tmp, type = 'l')</code></pre>
#'          Also try <tt>'plot(tmp[300:500], type = 'l')</tt>'
#'       b. Now write a function which determines the number of iterations needed to get
#'          $| x_n - x_{n-1}| < 0.02$. This function has only 
#'          2 arguments: '<tt>start</tt>' and '<tt>rho</tt>'. 
#'          (For '<tt>start = 0.95</tt>' and '<tt>rho=2.99</tt>', the answer is 84.) 
#'          
#' 10. [Warwick]  
#'       a. Given a vector 
#'          $(x_1, ... ,x_n)$,
#'          the sample autocorrelation of lag $k$ is defined to be
#'          $$r_k = \frac{\sum_{i=k+1}^{n}(x_i-\bar{x})(x_{i-k}-\bar{x})}{\sum_{i=1}^{n}(x_i-\bar{x})^2}$$
#'          Write a function '<tt>tmpFn(xVec)</tt>' which takes a single
#'          argument '<tt>xVec</tt>' which is a vector and returns a
#'          list of two values: $r_1$ and $r_2$.<br>
#'          In particular, find $r_1$ and $r_2$ for the vector
#'          $(2, 5, 8, ..., 53, 56)$.
#'       b. (Harder) Generalize the function so that it takes two arguments: 
#'          the vector '<tt>xVec</tt>' and an integer '<tt>k</tt>' which lies between 1 and 
#'          $n-1$ where $n$ is the length of '<tt>xVec</tt>'.
#'          The function should return a vector of the values
#'          $(r_0 = 1, r_1, ..., r_k)$. <br>
#'          If you used a loop to answer part (b), then you need to be 
#'          aware that much, much better solutions are possible. Hint: '<tt>sapply</tt>'.
#'       
#'                 
#'               
#' # 5 Common Statistical Procedures--
#' 
#' ## 5.1 Summary statistics--
#' 
#' ## 5.2 Bivariate statistics--
#' 
#' ## 5.3 Contingency tables--
#' 
#' ## 5.4 Tests for continuous variables--
#' 
#' ## 5.5 Analytic power and sample size calculations--
#' 
#' ## 5.6 Further resources--
#' 
#' ## 5.7 Examples--
#' 
#' ## 5.X Exercises--
#' 
#' # 6 Linear regression and ANOVA--
#' 
#' ## 6.1 Model fitting--
#' 
#' ## 6.2 Test, contrasts, and linear functions of parameters--
#' 
#' ## 6.3 Model diagnostics--
#' 
#' ## 6.4 Model parameters and results--
#' 
#' ## 6.5 Further resources--
#' 
#' ## 6.6 Examples--
#' 
#' ## 6.X Exercises--
#' 
#' # 7 Regression generalizations and modeling--
#' 
#' ## 7.1 Generalized linear models--
#' 
#' ## 7.2 Further generalizations--
#' 
#' ## 7.3 Robust methods--
#' 
#' ## 7.4 Models for correlated data--
#' 
#' ## 7.5 Survival analysis--
#'
#' ## 7.6 Multivariate statistics and discriminant procedures--
#' 
#' ## 7.7 Complex survey design--
#' 
#' ## 7.8 Model selection and assessment--
#' 
#' ## 7.9 Further resources--
#' 
#' ## 7.10 Examples--
#' 
#' ## 7.X Exercises--
#' 
#' # 8 A graphical compendium--
#' 
#' ## 8.1 Univariate plots--
#' 
#' ## 8.2 Univariate plots by grouping variable--
#' 
#' ## 8.3 Bivariate plots--
#' 
#' ## 8.4 Multivariate plots--
#' 
#' ## 8.5 Special purpose plots--
#' 
#' ## 8.6 Further resources--
#' 
#' ## 8.7 Examples--
#' 
#' ## 8.X Exercises--
#' 
#' # 9 Graphical options and configuration--
#' 
#' ## 9.1 Adding elements--
#' 
#' ## 9.2 Options and parameters--
#' 
#' ## 9.3 Saving graphs--
#' 
#' ## 9.X Exercises--
#' 
#' # 10 Simulation--
#' 
#' ## 10.1 Generating data--
#' 
#' ## 10.2 Simulation application--
#' 
#' ## 10.3 Further resources--
#' 
#' ## 10.X Exercises--
#' 
#' # 11 Special topics--
#' 
#' ## 11.1 Processing by group--
#' 
#' ## 11.2 Simulation-based power calculations--
#' 
#' ## 11.3 Reproducible analysis and output--
#' 
#' ## 11.4 Advanced statistical methods--
#' 
#' ## 11.5 Further resources--
#' 
#' # 12 Case studies--
#' 
#' ## 12.1 Data management and related tasks--
#' 
#' ## 12.2 Read variable format file--
#' 
#' ## 12.3 Plotting maps--
#' 
#' ## 12.4 Data scraping and visualization--
#' 
#' ## 12.5 Manipulating bigger datasets--
#' 
#' ## 12.6 Constrained optimization: the knapsack problem--
#' 
#' ## 12.X Exercises--
#' 
#' # Appendix
#' 
#' ## Appendix B: R Basics
#' 
#' ### Using R Script with Markdown
#' 
#' [Here's a posting](http://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/) that describes quite well the difference between an R Markdown
#' script (with extension .Rmd) and a .R script with Markdown. The main advantages of the
#' latter are expressed well: 1) you don't need to transform your original .R script
#' manually into a .Rmd script and 2) the same script can be run interactively in R
#' and be used to generate a clean report.
#' 
#' One problem is that <tt>Ctrl-Shift-K</tt> produces diagnostics that refer to 
#' line numbers in the <tt>.Rmd</tt> file, whose numbering can be very different from
#' that of the <tt>.R</tt> file. When this happens you can 'knit' the <tt>.R</tt> file in
#' a way that keeps the intermediate <tt>.Rmd</tt> file by using the command:
#' <pre>
#'      rmarkdown::render("YourFile.R", clean = FALSE)
#' </pre>
#' This will leave the 
#' intermediate files in your directory so you can interpret error messages.
#' 
#' ## Pitfalls
#' 
#' Contribute pitfalls [here](http://capstone.stats.yorku.ca/index.php/R_Notes_Discussion#Pitfalls)
#' 
#' ## How tos - not yet categorized
#' 
#' Contribute 'how tos' [here](http://capstone.stats.yorku.ca/index.php/R_Notes_Discussion#How_To).
#' 
#' ### Changing all variables to characters in a data frame
#' 
#' When data frames are being manipulated only as data sets, not for immediate
#' statistical analyses, it is often convenient to have all variables as characters to
#' avoid problems due to the inconsistent behaviour of factors. A very easy way to
#' do this, if <tt>dd</tt> is a data frame:
#+ eval=FALSE
dd[] <- lapply(dd, as.character)
#' Any side effects? 
#' 
#'   * Some variable attributes may be lost with <tt>as.character</tt>.
#' 
#' ## Appendix B: Exercises
#' 
#' 1. [paste] What is the difference between <tt>paste(x, y, sep = ':' )</tt> and  <tt>paste(x, y, collapse = ':' )</tt> 
#' 
#' 2. [paste] Suppose <tt>x <- list('a','b','c')</tt>. Use <tt>paste</tt> and <tt>do.call</tt>
#'    to produce the output string  <tt>'a:b:c'</tt>.
#'    
#'   
#'    What is the difference between <tt>paste(x, y, sep = ':' )</tt> and  <tt>paste(x, y, collapse = ':' )</tt> 