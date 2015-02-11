A small test of the main functionality

  $ compile-and-source "$TESTDIR/configs/Basic.hs" >& /dev/null
  $ [[ -f "$ANTIGEN_HS_OUT/antigen-hs.zsh" ]] || (echo 'File not created! :(' && exit 1)
  $ mcd a/b/c/d
  $ bd b
  $ ls
  c
  $ echo $fpath[-1]
  /tmp/cramtests-??????/basic.t/antigen-hs-out/repos/https-COLON--SLASH--SLASH-github.com-SLASH-Tarrasch-SLASH-zsh-mcd (glob)
