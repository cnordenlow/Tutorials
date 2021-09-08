# Tutorials


Instructions for publishing a bookdown on Github


1) Create an repository on GitHub.
2) Clone to Repository using R-studio and Git (create new project) and create a local folder.
3) In Github-repository, create a file named "_ _bookdown.yml"_ with the code output_dir: "docs".
4) Create a rmd-file called Index.rmd as below.
5) In R-studio, run bookdown::render_book("index.Rmd") 
6) In R-studio, go to the tab Git -> Commit -> Commit -> Pull -> Push to push the created bookdown-files to Github.
7) In GIthub, create a file named .nojekyll in the docs folder containing info below:
touch .nojekyll
git add .nojekyll
8) In Github, go to settings and change Github Pages to master branch /docs folder

9) Finito! Update your book and Github running 5 and 6.

https://cnordenlow.github.io/tutorials/index.html


```
In the index.rmd file, create as below
---
title: "Testing"
author: "christoffer.nordenlow@outlook.se"
date: '2020-05-26'
site: bookdown::bookdown_site
output: bookdown::gitbook
---
```
