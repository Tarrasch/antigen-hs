A test to see if we register #compdef marks

  $ compile-and-source "$TESTDIR/configs/Compdef.hs"
  $ [[ -f "$ANTIGEN_HS_OUT/antigen-hs.zsh" ]] || (echo 'File not created! :(' && exit 1)
  $ echo $set_some_var
  123
  $ echo $fpath
  yay
