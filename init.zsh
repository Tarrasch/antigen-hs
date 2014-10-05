# See https://github.com/Tarrasch/antigen-hs
if [[ "$0" != $HOME/.zsh/antigen-hs/init.zsh ]]
then
  echo "Put this file in '~/.zsh/antigen-hs/init.zsh' please!"
fi

() {
  local FILE_TO_SOURCE="$HOME/.antigen-hs/antigen-hs.zsh"
  if [[ -f $FILE_TO_SOURCE ]]
  then
    source $FILE_TO_SOURCE
  else
    echo "Didn't find file $FILE_TO_SOURCE"
    echo "Try running antigen-hs-compile"
  fi
}

antigen-hs-compile () {
  runghc -i"$HOME/.zsh/antigen-hs/" -- "$HOME/.zsh/MyAntigen.hs"
}

antigen-update() {
    for folder in $(find $HOME/.antigen-hs/repos/ -maxdepth 1 -mindepth 1 -type d); do
        echo "\nIn folder $folder"
        cd $folder && git pull origin master
    done
    antigen-hs-compile
}

antigen-bundle() {
    echo "\tbundle \"$1\"," >> $HOME/.zsh/bundles
    BUNDLES=$(cat $HOME/.zsh/bundles)
    rm -f $HOME/.zsh/MyAntigen.hs

    HEADER='
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE ExtendedDefaultRules #-}
    module MyAntigen where

    import Antigen (AntigenConfiguration (..), bundle, antigen)
    import Shelly (shelly)

    bundles =
    [
    '

    FOOTER='
    bundle "srijanshetty/custom"
    -- Add your plugins here
    ]

    config = AntigenConfiguration bundles

    main :: IO ()
    main = shelly $ antigen config
    '

    echo "$HEADER $BUNDLES $FOOTER" > $HOME/.zsh/MyAntigen.hs
    antigen-hs-compile
}

antigen-list() {
    cat $HOME/.zsh/bundles
}
