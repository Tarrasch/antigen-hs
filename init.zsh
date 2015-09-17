#!/usr/bin/env zsh
# See https://github.com/Tarrasch/antigen-hs#readme

ANTIGEN_HS_HOME=${${0:A}:h}

if [[ -z "$ANTIGEN_HS_OUT" ]] ; then
  ANTIGEN_HS_OUT="$HOME/.antigen-hs"
fi

if [[ -z "$ANTIGEN_HS_MY" ]] ; then
  ANTIGEN_HS_MY="$ANTIGEN_HS_HOME/../MyAntigen.hs"
fi

antigen-hs-sandbox () {
  cd "$ANTIGEN_HS_HOME" && stack setup && stack build
  cd $OLDPWD
}

antigen-hs-source () {
  local FILE_TO_SOURCE="$ANTIGEN_HS_OUT/antigen-hs.zsh"

  if [[ -f $FILE_TO_SOURCE ]] ; then
    source $FILE_TO_SOURCE
  else
    echo "Didn't find file $FILE_TO_SOURCE"
    return 1
  fi
}

antigen-hs-compile () {
  local ANTIGEN_COMPILE_CMD='runghc -i"$ANTIGEN_HS_HOME/" -- "$ANTIGEN_HS_MY"'

  if (( $+commands[stack] )) ; then
      local STACK_YAML="$ANTIGEN_HS_HOME"/stack.yaml
      local ANTIGEN_COMPILE_CMD="stack exec -- "$ANTIGEN_COMPILE_CMD

    if [[ ! -d "$ANTIGEN_HS_HOME"/.stack-work ]] ; then
      antigen-hs-sandbox
    fi
  else
    unfunction antigen-hs-sandbox
  fi
  eval "$ANTIGEN_COMPILE_CMD"
  antigen-hs-source
}

() {
  if ! antigen-hs-source ; then

    while true; do
      read -sk 1 "response?Try to run antigen-hs-compile? [Y/n] "

      case $response in
        [yY] | $'\n' ) echo 'Yes' ; antigen-hs-compile ; break
                       ;;
        [nN] ) echo 'No' ; break
               ;;
        * ) echo $response ; echo "Please answer yes or no."
            ;;
      esac
    done
  fi
}
