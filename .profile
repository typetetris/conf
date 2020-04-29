export EDITOR="nvim"
set -o vi

eval "$(dircolors)"
export _JAVA_AWT_WM_NONREPARENTING=1

#if [[ -e "${HOME}/.nix-profile/etc/profile.d/my-profile.sh" ]]
#then
#	source "${HOME}/.nix-profile/etc/profile.d/my-profile.sh"
#fi

# Thats even bad in nixos
#for f in $(find -L ${HOME}/.nix-profile/etc/profile.d -type f -name '*.sh')
#do
#	source ${f}
#done
