A test to see if we register #compdef marks (setting the fpath)

  $ compile-and-source "$TESTDIR/configs/Compdef.hs" >& /dev/null
  $ [[ -f "$ANTIGEN_HS_OUT/antigen-hs.zsh" ]] || (echo 'File not created! :(' && exit 1)
  $ echo $set_some_var
  123
  $ echo $fpath[-1]
  /tmp/antigen-hs/tests/repos/compdef/etc
