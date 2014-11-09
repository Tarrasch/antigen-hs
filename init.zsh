# See https://github.com/Tarrasch/antigen-hs
ANTIGEN_HS_HOME=${${0:A}:h}
if [[ -z "$ANTIGEN_HS_OUT" ]]; then
    ANTIGEN_HS_OUT="$HOME/.antigen-hs"
fi
if [[ -z "$ANTIGEN_HS_MY" ]]; then
    ANTIGEN_HS_MY="$ANTIGEN_HS_HOME/../MyAntigen.hs"
fi

antigen-hs-compile () {
  runghc -i"$ANTIGEN_HS_HOME/" -- "$ANTIGEN_HS_MY"
}

() {
  local FILE_TO_SOURCE="$ANTIGEN_HS_OUT/antigen-hs.zsh"
  if [[ -f $FILE_TO_SOURCE ]]
  then
    source $FILE_TO_SOURCE
  else
    echo "Didn't find file $FILE_TO_SOURCE"
    echo "Try running antigen-hs-compile"
  fi
}
