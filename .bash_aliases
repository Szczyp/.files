gvim () { /usr/bin/gvim -f $* & disown; }
ssh () { /usr/bin/ssh -t $* screen -U -R; }
eclipse () { ~/bin/eclipse $* & disown; }
idea () { IDEA_JDK=~/idea/jdk1.6.0_34 ~/idea/bin/idea.sh $* & disown; }
emacs () { /usr/bin/emacs $* & disown; }