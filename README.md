This project contains the source LaTeX files for the thesis "**Poolcasting: an intelligent technique to customise music programmes for their audience**" that completed [my Ph.D. studies](http://www.iiia.csic.es/~claudio) in Artificial Intelligence at the Universitat Autònoma de Barcelona in November 2009.

Excluded from the project are the fonts I used: [Minister Std Light](http://myfonts.com/fonts/adobe/minister/light), [Helvetica](http://myfonts.com/fonts/adobe/helvetica), and [Inconsolata](http://levien.com/type/myfonts/inconsolata.html).

Typeset by running [latexmk.pl](http://www.phys.psu.edu/~collins/software/latexmk-jcc/) on the thesis.tex file. If you use Textmate, simply set *TX_LATEX_COMPILER* to *latexmk.pl* in the Advanced menu (Shell Variables) and then run  ⌘+R.

Feel free to reuse the style I crafted for your own work.

More detailed install instructions (OS X)
=========================================

1. Install [BasicTex](http://www.tug.org/mactex/morepackages.html)
2. Download [latexmk](http://users.phys.psu.edu/~collins/software/latexmk-jcc)
3. Make `latexmk.pl` executable and move in a $PATH folder (e.g. /usr/local/bin)
4. Create the folder ~/Library/texmf/tex/latex/
5. Download in this folder [framed.sty](http://ctan.mirrors.hoobly.com/macros/latex/contrib/framed/framed.sty)
6. Download in this folder [multirow.sty](http://ctan.math.washington.edu/tex-archive/macros/latex/contrib/multirow/multirow.sty)
7. Buy or install [Minister Std Light](http://myfonts.com/fonts/adobe/minister/light)
8. Install [Inconsolata font](http://levien.com/type/myfonts/Inconsolata.otf)
9. Run the command `latexmk -xelatex thesis.tex` and finally open thesis.pdf :sweat_smile: