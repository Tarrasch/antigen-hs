# See https://github.com/Tarrasch/antigen-hs
if [[ "$0" != $HOME/.zsh/antigen-hs/init.zsh ]]
then
  echo "Put this file in '~/.zsh/antigen-hs/init.zsh' please!"
fi

local FILE_TO_SOURCE="$HOME/.antigen-hs/antigen-hs.zsh"
if [[ -f $FILE_TO_SOURCE ]]
then
  source $FILE_TO_SOURCE
else
  echo "Didn't find file $FILE_TO_SOURCE"
  echo "Try running antigen-hs-compile"
fi

antigen-hs-compile () {
  runghc -i"$HOME/.zsh/antigen-hs/" -- "$HOME/.zsh/MyAntigen.hs"
}
