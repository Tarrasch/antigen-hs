A test to see if we register #compdef marks

  $ compile-and-source "$TESTDIR/configs/Compdef.hs" >& /dev/null
  $ [[ -f "$ANTIGEN_HS_OUT/antigen-hs.zsh" ]] || (echo 'File not created! :(' && exit 1)
  $ mcd a/b/c/d
  $ bd b
  $ ls
  c
