$latex  = 'uplatex -shell-escape -src-specials -synctex=1 -interaction=batchmode';
$pdflatex  = 'uplatex -shell-escape -src-specials -interaction=nonstopmode';
$bibtex = 'upbibtex';
$dvipdf  = 'dvipdfmx %O -o %D %S';
$makeindex  = 'mendex %O -o %D %S';
$pdf_previewer = 'open %O %S';
$pdf_mode = 3;
$pdf_update_method = 0;
