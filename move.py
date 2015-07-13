import shutil
print "Copy from gitHub to workPlace"
shutil.copyfile("/home/rafael/Documents/tese/gui.rkt", "/home/rafael/share/racket/pkgs/drracket/drracket/private/syncheck/gui.rkt")
shutil.copyfile("/home/rafael/Documents/tese/code-walker.rkt", "/home/rafael/share/racket/pkgs/drracket/drracket/private/syncheck/code-walker.rkt")
shutil.copyfile("/home/rafael/Documents/tese/online-comp.rkt", "/home/rafael/share/racket/pkgs/drracket/drracket/private/syncheck/online-comp.rkt")

#shutil.copyfile("/home/rafael/Documents/tese/aux.txt", "/home/rafael/share/racket/pkgs/drracket/drracket/private/syncheck/aux.txt")

print "Done"
