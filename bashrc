alias z="trans :zh"
alias e="trans"

source /etc/profile.d/alias.sh
source ~/.bashrc

if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH
shopt -s expand_aliases
