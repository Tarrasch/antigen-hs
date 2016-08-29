# See https://github.com/Tarrasch/antigen-hs#readme

ANTIGEN_HS_HOME=${${0:A}:h}

if [[ -z "$ANTIGEN_HS_OUT" ]] ; then
  ANTIGEN_HS_OUT="$HOME/.antigen-hs"
fi

if [[ -z "$ANTIGEN_HS_MY" ]] ; then
  ANTIGEN_HS_MY="$ANTIGEN_HS_HOME/../MyAntigen.hs"
fi

# usage: _antigen-hs-ask "question" action
# or     _antigen-hs-ask "question" action1 action2
_antigen-hs-ask () {
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

_antigen-hs-sandbox () {
  _ANTIGEN_HS_SANDBOX=$1

  eval "${@:3}" 2>&1
  local ANTIGEN_HS_SANDBOX_RESULT=$?
  return "$ANTIGEN_HS_SANDBOX_RESULT"
}

_antigen-hs-sandbox-stack () {
  _antigen-hs-sandbox "stack" .stack-work "stack setup && stack build"
}

_antigen-hs-sandbox-cabal () {
  _antigen-hs-sandbox "cabal" .cabal-sandbox "cabal sandbox init && _antigen-hs-cabal"
}

_antigen-hs-sandbox-cabal-check () {
  if (( $+commands[cabal] )) ; then
    echo $fg[green]"Cabal executable found and will be used."$reset_color
    _antigen-hs-sandbox-cabal
  else
    echo $fg[red]'Executable for cabal not found. Install it or check your $PATH. Skip setup.'$reset_color
    return 1
  fi
}

_antigen-hs-cabal () {
  cabal update
  cabal install --only-dependencies
}

_antigen-hs-cabal-global () {
  _ANTIGEN_HS_SANDBOX="empty"
  _antigen-hs-cabal
}

_antigen-hs-init-sandbox () {
  if (( $+commands[stack] )) ; then
    _antigen-hs-ask "Stack executable found. Use it for sandbox?" _antigen-hs-sandbox-stack _antigen-hs-sandbox-cabal-check
  else
    _antigen-hs-sandbox-cabal-check
  fi
}

_antigen-hs-repeat () {
  if ! eval $1 ; then
    _antigen-hs-ask "$2 finish without success. Would you like try again?" "$0 $*"
  fi
}

_antigen-hs-init () {
  _antigen-hs-ask "Use sandbox for haskell dependencies?" _antigen-hs-init-sandbox _antigen-hs-cabal-global
}

antigen-hs-setup () {
  _antigen-hs-repeat _antigen-hs-init "Setup"
  _antigen-hs-repeat _antigen-hs-compile "Compilation"
}

_antigen-hs-source () {
  local FILE_TO_SOURCE="$ANTIGEN_HS_OUT/antigen-hs.zsh"

  if [[ -f $FILE_TO_SOURCE ]] ; then
    source $FILE_TO_SOURCE
  else
    echo $fg[red]"Didn't find file $FILE_TO_SOURCE"$reset_color
    return 1
  fi
}

_antigen-hs-init-source () {
  _antigen-hs-source
  local ANTIGEN_HS_SOURCE_RESULT=$?

  if [[ "$ANTIGEN_HS_SOURCE_RESULT" == 0 ]] ; then
    if [[ -z $_ANTIGEN_HS_SANDBOX ]]; then

      if [[ -d "$ANTIGEN_HS_HOME"/.stack-work ]] ; then
        _ANTIGEN_HS_SANDBOX="stack"
      elif [[ -d "$ANTIGEN_HS_HOME"/.cabal-sandbox ]] ; then
        _ANTIGEN_HS_SANDBOX="cabal"
      else
        _ANTIGEN_HS_SANDBOX="empty"
      fi

    fi
  else
    return "$ANTIGEN_HS_SOURCE_RESULT"
  fi
}

_antigen-hs-compile () {
  local ANTIGEN_HS_COMPILE_CMD='runghc -i"$ANTIGEN_HS_HOME/" -- "$ANTIGEN_HS_MY"'

  case $_ANTIGEN_HS_SANDBOX in
    "stack" )
      local ANTIGEN_HS_COMPILE_CMD='STACK_YAML="$ANTIGEN_HS_HOME"/stack.yaml stack exec -- '"$ANTIGEN_HS_COMPILE_CMD"
      ;;
    "cabal" )
      local ANTIGEN_HS_COMPILE_CMD='CABAL_SANDBOX_CONFIG="$ANTIGEN_HS_HOME"/cabal.sandbox.config cabal exec -- '"$ANTIGEN_HS_COMPILE_CMD"
      ;;
    * )
      ;;
  esac
  eval "$ANTIGEN_HS_COMPILE_CMD"
  local ANTIGEN_HS_COMPILE_RESULT=$?

  if [[ "$ANTIGEN_HS_COMPILE_RESULT" == 0 ]] ; then
    _antigen-hs-source
  else
    return "$ANTIGEN_HS_COMPILE_RESULT"
  fi
}

() {
    if ! _antigen-hs-init-source ; then
     ( cd $ANTIGEN_HS_HOME;
       autoload -U colors && colors
       _antigen-hs-ask "Try to setup?" antigen-hs-setup
     )
    fi
}
