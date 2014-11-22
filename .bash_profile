export PS1="hrishi>"

export NODE_ENV="development"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
PATH=$PATH:/usr/local/sbin

alias nginxStart=/opt/nginx/sbin/nginx
export EDITOR=emacsclient
ulimit -n 1024
ulimit -u 1024
