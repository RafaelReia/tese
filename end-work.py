import shutil

print "Starting the copy back to the git"

shutil.copyfile("/home/rafael/share/racket/pkgs/drracket/drracket/private/syncheck/gui.rkt","/home/rafael/Documents/tese/gui.rkt")
shutil.copyfile("/home/rafael/share/racket/pkgs/drracket/drracket/private/syncheck/online-comp.rkt","/home/rafael/Documents/tese/online-comp.rkt")


print "finished! Enjoy your commit"
