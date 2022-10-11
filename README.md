# ece3504-project1
## build instructions

Basically, run the following command from the root of the project folder after installing sbcl (https://www.sbcl.org/) from a package manager... or the website if you run windows (click on the green square on the download page)

``` sh
sbcl --load assembler.lisp --eval "(sb-ext:save-lisp-and-die \"myAssembler\" :toplevel #'main :executable t)"
```

What this does is load the source code into sbcl, then runs the command to generate an executable from it. If the above command doesn’t work for any reason, first run `sbcl --load assembler.lisp`, then run the following command from the sbcl REPL: `(sb-ext:save-lisp-and-die "myAssembler" :toplevel #'main :executable t)`.

myAssembler in the above commands is the name of the output binary; change this as needed (on Windows, may need to change it to myAssembler.exe?).
