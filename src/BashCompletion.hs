-- | Wrapper around 'Options.Applicative.BashCompletion'
-- because the bash completion script is buggy (and not
-- configurable).

module BashCompletion
  ( bashCompletionParser
  ) where

import Control.Applicative
import System.Exit

import qualified Options.Applicative.BashCompletion

import Options.Applicative.Builder
import Options.Applicative.Common
import Options.Applicative.Types

-- | Wraps
-- 'Options.Applicative.BashCompletion.bashCompletionParser'
-- and fixes the completion script.
bashCompletionParser :: Parser a -> ParserPrefs -> Parser ParserFailure
bashCompletionParser parser pprefs
  = Options.Applicative.BashCompletion.bashCompletionParser parser pprefs
  <|> completeScript
  where
    failure opts = ParserFailure
      { errMessage = \ progn -> unlines <$> opts progn
      , errExitCode = ExitSuccess
      }

    completeScript = failure . bashCompletionScript
      <$> strOption (long "bash-completion-script-spaceport" `mappend` internal)

bashCompletionScript :: String -> String -> IO [String]
bashCompletionScript prog progn = return
  [ "_" ++ progn ++ "()"
  , "{"
  , "    local CMDLINE"
  , "    local OLD_IFS"
  , "    CMDLINE=(--bash-completion-index \"$COMP_CWORD\")"
  , ""
  , "    for arg in \"${COMP_WORDS[@]}\"; do"
  , "        CMDLINE=(\"${CMDLINE[@]}\" --bash-completion-word \"$arg\")"
  , "    done"
  , ""
  , "    OLD_IFS=\"$IFS\""
  , "    IFS=$'\\n'"
  , "    COMPREPLY=($(" ++ prog ++ " \"${CMDLINE[@]}\"))"
  , "    IFS=\"$OLD_IFS\""
  , "}"
  , ""
  , "complete -o filenames -F _" ++ progn ++ " " ++ progn
  ]
