# OCamlRisk

To run the gui you must first have lablgtk2 installed on your device. We assume that oCaml 4.06.0 is installed.
- This can be done on mac devices using "brew install lablgtk" (will install the required dependencies like gtk+)  
- And then "opam install lablgtk"

Once you have lablgtk2 installed properly (you can tell by running "opam list" and one of the installed packages should be lablgtk 2.18.6) you can run the gui in 2 different ways.
1. Run "make gui" from the command line.
2. Run from utop, to do this you must enter utop from the directory then 
  - "#require "lablgtk2""
  - "camlc -I +../lablgtk2 -o gui lablgtk.cma gtkInit.cmo gui.ml"
  - "./gui"
  
To exit the gui on a mac press control and c at the same time. 
