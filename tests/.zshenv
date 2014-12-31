export ANTIGEN_HS_HOME="$TESTDIR/.."
export ANTIGEN_HS_OUT="$PWD/antigen-hs-out"

# First time we source to define antigen-hs-compile
# We pipe to /dev/null to mute the advice being echoed
source "$TESTDIR/../init.zsh" > /dev/null

compile-and-source () {
  export ANTIGEN_HS_MY="$1"
  antigen-hs-compile
  # Now we source to source the file antigen-hs-compile created
  source "$TESTDIR/../init.zsh"
}
