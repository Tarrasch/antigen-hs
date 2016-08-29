export ANTIGEN_HS_HOME="$TESTDIR/.."
export ANTIGEN_HS_OUT="$PWD/antigen-hs-out"

# Copy over repos
rm -rf /tmp/antigen-hs/tests/
mkdir -p /tmp/antigen-hs/tests/
cp -r -t /tmp/antigen-hs/tests/ $TESTDIR/repos

# First time we source to define antigen-hs-compile
# We pipe to /dev/null to mute the advice being echoed
yes N | source "$TESTDIR/../init.zsh" > /dev/null

compile-and-source () {
  rm -f $ANTIGEN_HS_OUT/antigen-hs.zsh
  export ANTIGEN_HS_MY="$1"
  _antigen-hs-compile
  # Now we source to source the file antigen-hs-compile created
  source "$TESTDIR/../init.zsh"
}
