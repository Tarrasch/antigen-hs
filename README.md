[![Build Status](https://travis-ci.org/Tarrasch/antigen-hs.svg?branch=master)](https://travis-ci.org/Tarrasch/antigen-hs)

Once a zsh user realizes that [oh-my-zsh] and [antigen] actually is bloatware,
they will rejoice in antigen-hs. Here's a quote from one content user.

>  Great work on antigen-hs, you just saved me more than a second *each time* I
>  split a tmux pane. Probably a month or two in some longer timespan. Never
>  thought about how much time oh-my-zsh took to run its stuff.
>   - [Mariano Casco](https://github.com/mdsn)

# antigen-hs

A replacement for [antigen] which has a low overhead when starting up the
shell. I created this plugin because I realized that antigen were the main
cause of slow zsh startup times. After the switch, I went from a loading time
of 1.00 seconds to less than 0.10 seconds (wall clock).

The performance improvement is achieved by *generating* a shell script
consisting of commands to source your plugin scripts. The script contains
full absolute paths of your plugins scripts. In antigen, the paths are
calculated every time a new shell is opened.

## Installing

Since this plugin is not entirely written in zsh (unlike antigen), it has a few
more installation steps. In addition to this README, check out this [diff] to
my dotfiles repository where I replace antigen with antigen-hs.

### Haskell and packages

Since this plugin is written in Haskell, you have to download it:

- installation through `cabal`:

        $ sudo apt-get install ghc cabal-install

  And if you won't use `cabal sandbox`:

        $ cabal update
        $ cabal install base text directory filepath process

- installation through `stack`:

  Follow [Stack installation guide](https://github.com/commercialhaskell/stack/wiki/Downloads).

### Clone and source

This plugin assumes that you put it in `~/.zsh/antigen-hs/`:

    git clone https://github.com/Tarrasch/antigen-hs.git ~/.zsh/antigen-hs/

### Create plugins file

    touch ~/.zsh/MyAntigen.hs
    $EDITOR ~/.zsh/MyAntigen.hs

And paste this example content

```haskell
{-# LANGUAGE OverloadedStrings #-}
module MyAntigen where

import Antigen (
                -- Rudimentary imports
                AntigenConfig (..)
              , defaultConfig
              , bundle
              , antigen
                -- If you want to source a bit trickier plugins
              , ZshPlugin (..)
              , antigenSourcingStrategy
              , filePathsSourcingStrategy
              )

bundles =
  [ bundle "Tarrasch/zsh-functional"
  , bundle "Tarrasch/zsh-bd"
  , bundle "zsh-users/zsh-syntax-highlighting"
  , (bundle "zsh-users/zsh-history-substring-search") { sourcingStrategy = antigenSourcingStrategy }

  -- If you use a plugin that doesn't have a *.plugin.zsh file. You can set a
  -- more liberal sourcing strategy.
  --
  -- , (bundle "some/stupid-plugin") { sourcingStrategy = antigenSourcingStrategy }

  -- If you use a plugin that has sub-plugins. You can specify that as well
  --
  -- NOTE: If you want to use oh-my-zsh for real (please don't), you still need
  -- to set the $ZSH env var manually.
  -- , (bundle "robbyrussell/oh-my-zsh")
  --    { sourcingLocations = [ "plugins/wd"
  --                          , "plugins/colorize"] }

  -- Sourcing a list of files
  -- , (bundle "alfredodeza/zsh-plugins")
  --    { sourcingStrategy = filePathsSourcingStrategy
  --                          [ "vi/zle_vi_visual.zsh"
  --                          , "pytest/pytest.plugin.zsh"
  --                          ] }

  -- Alternatively, this way will give you the same result
  -- , (bundle "alfredodeza/zsh-plugins")
  --    { sourcingStrategy = antigenSourcingStrategy
  --    , sourcingLocations = [ "vi"
  --                          , "pytest"
  --                          ] }

  -- vvv    Add your plugins here    vvv
  ]

config = defaultConfig { plugins = bundles }

main :: IO ()
main = antigen config
```

Now edit this file accordingly by adding your own plugins. Then you're done!
You can get some inspiration from the author's
[~/.zsh/MyAntigen.hs](https://github.com/Tarrasch/dotfiles/blob/master/zsh/MyAntigen.hs).

Finally, source it from you zshrc:

    $ echo 'source ~/.zsh/antigen-hs/init.zsh' | tee -a ~/.zshrc | env zsh

And let withard to help you setup and recompile your configuration:

    Didn't find file ~/.antigen-hs/antigen-hs.zsh
    Try to setup? [Y/n] Yes
    Use sandbox for haskell dependencies? [Y/n] Yes
    Stack executable found. Use it for sandbox? [Y/n] Yes

## Usage

Each time you update `MyAntigen.hs` you have to run `antigen-hs-setup`

## Limitations

The original [antigen] plugin does way more than it should (just look at its bloated
command list). This plugin tries to be minimal, if you are enough experienced
with computers to use a zsh plugin manager, you can delete and manage the
repositories in `~/.antigen-hs/repos` manually yourself!

[antigen]: https://github.com/zsh-users/antigen
[oh-my-zsh]: https://github.com/robbyrussell/oh-my-zsh
[diff]: https://github.com/Tarrasch/dotfiles/commit/00c3b34c1e1e13d9b0f634611e5bdb5e42211b22

## Configuration

The following variables alter the behavior of antigen-hs, and must be set before
sourcing init.zsh:

    ANTIGEN_HS_HOME # This is only set automatically, and is set to the location
                    # of the antigen-hs repository (which by default is
                    # $HOME/.zsh/antigen-hs)
    ANTIGEN_HS_OUT  # Output directory for cloned repositories and generated
                    # scripts, defaults to $HOME/.antigen-hs.
    ANTIGEN_HS_MY   # Your configuration file, defaults to
                    # $ANTIGEN_HS_HOME/../MyAntigen.hs


## FAQ

**Is Prezto or oh-my-zsh supported in some way?** No, if you load them they
will be treated like any other plugin. Remember that those two basically are
just bundles of plugins, I recommend against using them because one should load
the individual plugins through a plugin manager like antigen or antigen-hs.
If there's anything inside those plugins that you want, just create a seperate
repository for it [like I did][command-not-found].

**Why do I get `No *.plugin.zsh file` error?** Because all zsh plugins
should have that file. I want to enforce it so that people start following this convention.
The easiest way is to just
[fork the plugin you want](https://github.com/Tarrasch/pure/) and/or
[make them change it](https://github.com/sindresorhus/pure/pull/73)
If you're interested in some discussion about this, take a look at
<https://github.com/Tarrasch/antigen-hs/issues/3> and
<https://github.com/zsh-users/antigen/issues/23>.

**What is the relationship of antigen-hs with antigen?** antigen-hs can achieve
100% of antigen if the user is just willing to spend some time doing things
right (like not using plugin bundles like oh-my-zsh). In reward they don't need
to use software that is bloated. To be fair, many (including my good github
friend @sharat87) might want to say that antigen is more featureful/convenient,
not bloated. I'll let you choose side yourself. :)

**What about the antigen commands like `theme` and others?** I do not found
them useful or stable so I rather not have them. I rather rely on my own zsh fu
when it comes to chores like updating the repositories (nuking the
`~/.antigen-hs` folder) than relying on some shaky commands of antigen.

**Can I only source GitHub repositories?** At the moment yes, patches are very welcome! :)

**Where are some more plugins?** [awesome-zsh-plugins](https://github.com/unixorn/awesome-zsh-plugins)
is a list of zsh plugins that you may find helpful.

**Do it use sandbox for haskell dependencies?**
Yes, it support installation by `stack` or in `cabal sandbox` as an option.

**Why not write it in pure zsh?**
[Trust](https://github.com/Tarrasch/zsh-bd)
[me](https://github.com/Tarrasch/zsh-autoenv), [it](https://github.com/Tarrasch/zsh-mcd)
[is](https://github.com/Tarrasch/zsh-colors)
[a](https://github.com/Tarrasch/zsh-i-know)
[horrible](https://github.com/Tarrasch/zsh-command-not-found)
[language](https://github.com/Tarrasch/zsh-functional).

**Why Haskell?** I love it.

[command-not-found]: https://github.com/Tarrasch/zsh-command-not-found
