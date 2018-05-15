We will assume for this project that you have OCaml installed since
this is a final project for CS3110! However, if you do not, please follow the
instructions for installation here:

https://www.cs.cornell.edu/courses/cs3110/2018sp/install.html

Next, you have to install lablgtk in order to run it.

For Mac users with Homebrew:
1) Run "brew install lablgtk" on Mac devices to install dependencies
2) Run "opam install lablgtk" to install lablgtk.

However, for 2 of our group members, this didn't work because GTK did not get
installed for some reason. If this is the case, try the following:

For Mac users with MacPorts:
1) Run “sudo port install opam” to install opam
2) Run “opam init” to intialize opam to the Mac’s environment
3) Run “sudo port install lablgtk2 +gtksourceview2” to install lablgtk2 and necessary gtk dependencies
4) Run “opam install lablgtk-extras” to install supplementary lablgtk dependencies 
5) Download the XQuartz-2.7.11.dmg package from https://www.xquartz.org/
6) Run “opam config env” to establish XQuartz as the package responsible for displaying the GUI

For People using VM:

1) sudo apt-get install gtk
2) opam install lablgtk

Then finally, once you have everything installed, type the commands

"make play" to run the game and
"make tests" to run the test cases. 
