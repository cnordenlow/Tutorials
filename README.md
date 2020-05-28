# Tutorials


Instructions for publishing a bookdown on Github

1) Create an easy rmd-file called Index.rmd.
2) Create an repository on GitHub.
3) Clone to Repository using R-studio and Git (create new project) and create a local folder.
4) In Github-repository, create a file named "_bookdown.yml"_ with the code output_dir: "docs".
5) In R-studio, run bookdown::render_book("index.Rmd") 
6) In R-studio, go to the tab Git -> Commit -> Commit -> Pull -> Push to push the created bookdown-files to Github.
7) In GIthub, create a file named .nojekyll in the docs folder containing info below:
touch .nojekyll
git add .nojekyll
8) In Github, go to settings and change Github Pages to master branch /docs folder

9) Finito! Update your book and Github running 5 and 6.
