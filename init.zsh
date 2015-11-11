# See https://github.com/Tarrasch/antigen-hs#readme

ANTIGEN_HS_HOME=${${0:A}:h}

if [[ -z "$ANTIGEN_HS_OUT" ]] ; then
  ANTIGEN_HS_OUT="$HOME/.antigen-hs"
fi

if [[ -z "$ANTIGEN_HS_MY" ]] ; then
  ANTIGEN_HS_MY="$ANTIGEN_HS_HOME/../MyAntigen.hs"
fi

# usage: antigen-hs-ask "question" action
# or     antigen-hs-ask "question" action1 action2
antigen-hs-ask () {
  while true; do
    local REQUEST="read"

    if ! [[ -t 0 ]]; then
      REQUEST=$REQUEST" -u 0"
    fi
    REQUEST=$REQUEST' -sk 1 "RESPONSE?$fg[green]$1 $fg_bold[green][Y/n] "$reset_color'

    local RESPONSE=$(eval $REQUEST ; echo $RESPONSE)

    case $RESPONSE in
      [yY] | "" ) echo $fg[yellow]'Yes'$reset_color
                  eval $2
                  break
                  ;;
      [nN] ) echo $fg[blue]'No'$reset_color
             if [[ $# == 3 ]]; then
               eval $3
             fi
             break
             ;;
      * ) echo $fg[red]$RESPONSE$reset_color
          echo $fg[red]"Please answer yes or no."$reset_color
          ;;
    esac
  done
}

antigen-hs-sandbox () {
  ANTIGEN_HS_SANDBOX=$1

  eval "${@:3}" 2>&1
  local ANTIGEN_HS_SANDBOX_RESULT=$?
  return "$ANTIGEN_HS_SANDBOX_RESULT"
}

antigen-hs-sandbox-stack () {
  antigen-hs-sandbox "stack" .stack-work "stack setup && stack build"
}

antigen-hs-sandbox-cabal () {
  antigen-hs-sandbox "cabal" .cabal-sandbox "cabal sandbox init && antigen-hs-cabal"
}

antigen-hs-sandbox-cabal-check () {
  if (( $+commands[cabal] )) ; then
    echo $fg[green]"Cabal executable found and will be used."$reset_color
    antigen-hs-sandbox-cabal
  else
    echo $fg[red]'Executable for cabal not found. Install it or check your $PATH. Skip setup.'$reset_color
    return 1
  fi
}

antigen-hs-cabal () {
  cabal update
  cabal install --only-dependencies
}

antigen-hs-cabal-global () {
  ANTIGEN_HS_SANDBOX="empty"
  antigen-hs-cabal
}

antigen-hs-init-sandbox () {
  if (( $+commands[stack] )) ; then
    antigen-hs-ask "Stack executable found. Use it for sandbox?" antigen-hs-sandbox-stack antigen-hs-sandbox-cabal-check
  else
    antigen-hs-sandbox-cabal-check
  fi
}

antigen-hs-repeat () {
  if ! eval $1 ; then
    antigen-hs-ask "$2 finish without success. Would you like try again?" "$0 $*"
  fi
}

antigen-hs-init () {
  antigen-hs-ask "Use sandbox for haskell dependencies?" antigen-hs-init-sandbox antigen-hs-cabal-global
}

antigen-hs-setup () {
  antigen-hs-repeat antigen-hs-init "Setup"
  antigen-hs-repeat antigen-hs-compile "Compilation"
}

antigen-hs-source () {
  local FILE_TO_SOURCE="$ANTIGEN_HS_OUT/antigen-hs.zsh"

  if [[ -f $FILE_TO_SOURCE ]] ; then
    source $FILE_TO_SOURCE
  else
    echo $fg[red]"Didn't find file $FILE_TO_SOURCE"$reset_color
    return 1
  fi
}

antigen-hs-init-source () {
  antigen-hs-source
  local ANTIGEN_HS_SOURCE_RESULT=$?

  if [[ "$ANTIGEN_HS_SOURCE_RESULT" == 0 ]] ; then
    if [[ -z $ANTIGEN_HS_SANDBOX ]]; then

      if [[ -d "$ANTIGEN_HS_HOME"/.stack-work ]] ; then
        ANTIGEN_HS_SANDBOX="stack"
      elif [[ -d "$ANTIGEN_HS_HOME"/.cabal-sandbox ]] ; then
        ANTIGEN_HS_SANDBOX="cabal"
      else
        ANTIGEN_HS_SANDBOX="empty"
      fi

    fi
  else
    return "$ANTIGEN_HS_SOURCE_RESULT"
  fi
}

antigen-hs-compile () {
  local ANTIGEN_HS_COMPILE_CMD='runghc -i"$ANTIGEN_HS_HOME/" -- "$ANTIGEN_HS_MY"'

  case $ANTIGEN_HS_SANDBOX in
    "stack" )
      local ANTIGEN_HS_COMPILE_CMD='STACK_YAML="$ANTIGEN_HS_HOME"/stack.yaml stack exec -- '"$ANTIGEN_HS_COMPILE_CMD"
      unfunction antigen-hs-sandbox-cabal-check 2>/dev/null
      unfunction antigen-hs-sandbox-cabal 2>/dev/null
      unfunction antigen-hs-cabal 2>/dev/null
      ;;
    "cabal" )
      local ANTIGEN_HS_COMPILE_CMD='CABAL_SANDBOX_CONFIG="$ANTIGEN_HS_HOME"/cabal.sandbox.config cabal exec -- '"$ANTIGEN_HS_COMPILE_CMD"
      unfunction antigen-hs-sandbox-stack 2>/dev/null
      ;;
    * )
      unfunction antigen-hs-sandbox-cabal-check 2>/dev/null
      unfunction antigen-hs-sandbox-cabal 2>/dev/null
      unfunction antigen-hs-sandbox-stack 2>/dev/null
      unfunction antigen-hs-sandbox 2>/dev/null
      ;;
  esac
  eval "$ANTIGEN_HS_COMPILE_CMD"
  local ANTIGEN_HS_COMPILE_RESULT=$?

  if [[ "$ANTIGEN_HS_COMPILE_RESULT" == 0 ]] ; then
    antigen-hs-source
  else
    return "$ANTIGEN_HS_COMPILE_RESULT"
  fi
}

() {
    if ! antigen-hs-init-source ; then
     ( cd $ANTIGEN_HS_HOME;
       autoload -U colors && colors
       antigen-hs-ask "Try to setup?" antigen-hs-setup
     )
    fi
  unfunction antigen-hs-init-source
  unfunction antigen-hs-setup
  unfunction antigen-hs-repeat
  unfunction antigen-hs-init
  unfunction antigen-hs-cabal-global
  unfunction antigen-hs-init-sandbox
}
