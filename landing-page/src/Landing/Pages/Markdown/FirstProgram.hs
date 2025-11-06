{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Landing.Pages.Markdown.FirstProgram where

import Landing.Utils
import Lamarckian.MMark
import Common.Route
import Data.Text

localLink :: Text -> Text
localLink = renderLocalLink FirstProgram

firstProgramMd :: Text
firstProgramMd = [immark|
# Running our first Haskell program


In this lesson we are going to be looking at a very simple Haskell command-line program that takes our command-line args and runs a program.

A program in haskell is roughly a synonym of a function or more officially, an IO action. If you do not know what that is, no worry, just know that it is ultimately a function that has the distinction through Haskell's type-system to handle real-world data as input, as well as produce real-world outputs and you could argue that this distinction is the most important feature of Haskell.

The program is already written in a way that you can immediately test how the code works, as long as you have ghc installed.

We recommend following [our Setup Instructions](https://acetalent.io/landing/Blog/SetupInstructions) so that you are set-up for any project however sometimes this can be a challenge and we want to provide a way to get started ASAP so that you can feel like a Haskell master ASAP 🥷

However the way to get set-up in under one minute is as follows:

## Quick Setup

we can get set-up ASAP by using GHCup

### Windows

For Windows, run this in a PowerShell session:

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```

You can find a walkthrough here: https://www.youtube.com/watch?v=bB4fmQiUYPw

Alternatively, you can run the Linux style command in a git bash terminal

### Linux & MacOS

For Linux, macOS, FreeBSD or Windows Subsystem 2 for Linux, run this in a terminal:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

### Through Nix (from Setup Instructions)

```bash
nix-shell -p ghc
```

### Confirm setup

Run the command

```bash
ghci
```

This should enter an interactive REPL that you can exit by running:

```bash
ghci> :quit
```

## Our First Program

Copy paste the following code block into a file named CLI.hs

```haskell
module Main where

-- Try me!
-- with ghc in your shell/environment run:
-- (on Windows: Use git bash)
--
-- ghc CLI.hs -o magical-sentence-maker-cli
-- ./magical-sentence-maker-cli
-- ./magical-sentence-maker-cli --dev
-- ./magical-sentence-maker-cli --dev --spoke
-- ./magical-sentence-maker-cli --dev --speak --opt "hey"
--
-- As a *magical* sentence maker, I am quite communicative
-- and verbose so that you can follow along with my behaviour
--
-- You can run a Haskell function by editing the function `myPureLogic`

import Prelude -- haskell's base library
import Data.Maybe
import Text.Read
import System.Environment (getArgs)

-- | The magical sentence maker :O -- from standard in (stdin) -- it takes 3 elements and generates MYSTERY logs :O :O :O
main :: IO ()
main = do
  args <- getArgs
  putStrLn "I'm the magical sentence maker!"
  let dev   = "--dev"   `elem` args
      spoke = "--speak" `elem` args
      optVal = getOptValue "--opt" args
  putStrLn ("Development mode: " ++ show dev)
  putStrLn ("Speak mode: " ++ show spoke)

  let myArgString = fromMaybe "empty string" optVal
  let lengthOfArg = length myArgString
  let lengthAsString = show lengthOfArg
  let log =
        "My custom arg: " ++ myArgString
        ++ " has length == " ++ (show lengthOfArg)


  -- Run your custom logic
  myFunction myArgString
  pure ()

-- | Run your custom logic
myFunction :: String -> IO ()
myFunction myArgString = do
  -- write logic here
  putStrLn "Output:"
  putStrLn $ myPureLogic myArgString
  myImpureLogic myArgString

  pure ()


myPureLogic :: String -> String
myPureLogic str = str

myImpureLogic :: String -> IO ()
myImpureLogic str = do
  putStrLn "myImpureLogic"

-- | HELPER FUNCTIONS -- -- Helper to find a value after a flag
getOptValue :: String -> [String] -> Maybe String
getOptValue flag args = case dropWhile (/= flag) args of
  (_:val:_) -> Just val
  _         -> Nothing
```

## Run this script

First we must compile our program

```bash
ghc CLI.hs -o magical-sentence-maker-cli
```

Then we can run the generated binary with any of the following individual commands

```bash
./magical-sentence-maker-cli
./magical-sentence-maker-cli --dev
./magical-sentence-maker-cli --dev --spoke
./magical-sentence-maker-cli --dev --speak --opt "hey"
```

We will see how to work with larger projects later on, but for now this is all we need for a single .hs file with only the basic libraries


we can edit this program by changing either the function `myPureLogic` or `myImpureLogic` and get quick easy feedback on our logic

For example, let's say the user will give us an Integer in the arg string we can parse it and multiply it by 2. Let's see how that looks:

```haskell
myPureLogic :: String -> String
myPureLogic str = case readMaybe str of
  Just (anInteger :: Integer) ->
    let
      times2 = anInteger * 2
    in
      show times2 -- turn to String
```

and for our more flexible "impure" code

```haskell
myImpureLogic :: String -> IO ()
myImpureLogic str = do
  putStrLn "myImpureLogic"
  case readMaybe str of
    Just (anInteger :: Integer) -> do
      let times2 = anInteger * 2
      writeFile "test.txt" (show times2)
      -- turn input to a string and then print it
      print times2
    Nothing -> do
      print "failed"
```

We have given two different function stubs to get you familiar with the important distinction between functions that have what we call "side effects" (impure) and one that unlike pharmaceuticals (😬) guarantees zero side effects (pure).

In this short snippet, you have seen all of the basic "syntax" of the language which is surprisingly small. You may not grasp any of it yet and it may feel overwhelming, but we assure you this is because it is new. It is a major departure from typical OOP languages like Python or JavaScript and as we will see in further chapters, there are many reasons why this is so important and awesome. For example, the Haskell code to render this page from a Markdown format is remarkably easy to understand.

**Haskell is the best language for our brains that I have ever seen.** It has been said that Haskell programmers feel like they need to code with rubber gloves when using other languages. It is backed by over 30 years of research and design through Programming Language Theory and more simply put, it doesn't overcomplicate and it doesn't oversimplify. You may have taken a course in object-oriented that teaches that the above code is ugly or some similar argument, but this is actually much easier to follow and inspect than a python or Java function which ensures that every possible case is handled correctly, such as if our assumption is wrong and the user accidentally gives their name instead of an Integer. In fact, there are cases where it is literally impossible to ensure a function will never have a bug and these cases usually become apparent when a company is looking to scale.

Haskell teaches us how to be brilliant software engineers who can construct a problem, solve a problem, and have that solution be so easy to understand for our later self and others. No matter how complex the reality that a program is meant to handle, like a video game, AI, or a robot's behaviour, Haskell teaches us how to compose our logic together and keep it easy to immediately tell what a program or function does.

Most developers for some reason believe that if they code in Haskell they need to create every library that JavaScript has themselves. However not only will that never be the case, but Haskell is such a powerfully expressive language that the best Haskellers have written libraries that far exceed what any object-oriented ecosystem can do, despite being a tiny group. [Reflex-Dom](https://hackage.haskell.org/package/reflex-dom-core) is a great example as not only has it made me 100x more productive than with React or Angular due to leveraging Haskell's typesystem, but we can use it to easily swap out what platform where are going to run on whether thats JavaScript, Web-Assembly, or even iOS and Android!

You will find that after learning Haskell you will find you have a shockingly strong intuition for how software works and capacity to solve large undefined problems.

We hope you enjoy the next Chapters (do I dare say pun intended?) and we are here to help you thrive.

[Next Chapter](https://acetalent.io/landing/Blog/Introduction)

|]
