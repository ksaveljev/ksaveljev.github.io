---
layout: post
title: "Learn From Hackage: Hsay"
modified:
categories: 
excerpt: We deconstruct a small program written by Alexander Berntsen which (ab)uses Google Translate as a speech synthesiser.
tags: [learn from hackage, hackage, haskell, tutorial, series, blog, deconstruct]
image:
  feature:
date: 2014-11-25T11:11:18+02:00
---

**Some introduction about the package we are going to look at**

**Small video showing how the program works**

Let us imagine that we are writing this project from scratch on our own. But we
keep everything related to the original author as it should be (name, email,
license).

First thing we need to do is to setup our project. I could simply create a
folder "hsay" and start working there but I prefer to create a project on my
GitHub page and clone it to my machine and start working in it. You can chose
the best way you like if you are going to follow along.

Now that we are inside our project folder we need to setup our Cabal project

{% highlight bash %}
cabal init
{% endhighlight %}

Our initial cabal file will look something like this:

{% highlight text %}
blahblahb
{% endhighlight %}

We have kept the original author's details and the license. We will keep our
main module in "src" folder, that is why there is "hs-source-dirs" set to "src".
Our main module will reside in Hsay.hs file that is why there is a "main-is" set
to "Hsay.hs"

We will initialize our cabal sandbox as we are pretty sure to be adding some
extra dependencies to the project in the nearest future.

{% highlight bash %}
cabal sandbox init
{% endhighlight %}

We create the structure of our program:

{% highlight bash %}
mkdir src
touch src/Hsay.hs
{% endhighlight %}

And we are ready to start working on the project!

{% highlight haskell %}
module Main where

import System.Environment (getArgs)
   
data Language = MkLang {language :: String}
              | DefLang {language :: String}

main :: IO ()
main = do
    args <- getArgs
    uncurry tts (getLanguage args)

getLanguage :: [String] -> (Language, [String])
getLanguage = undefined

tts :: Language -> [String] -> IO ()
tts = undefined
{% endhighlight %}

blahblahblah

getArgs returned us a list of strings. We check that if the first string starts
with '-' then the rest of it is considered to be a language. Otherwise we
default to english language:

{% highlight haskell %}
getLanguage :: [String] -> (Language, [String])
getLanguage (('-':l):xs) = (MkLang l, xs)
getLanguage xs           = (DefLang "en", xs)
{% endhighlight %}

Something about "uncurry" used in tts call with getLanguage

There are two different ways hsay is going to work. First one is where we get
the input straight away via command line argument. Second one is interactive, so
called Read Evaluate Say Loop (resl) where user will input a string and our
program is going to "read" it, user will be able to input another string and so
on, until he/she decies to quit this resl.

First, we will implement the part of tts which is responsible for handling input
via command line arguments.

{% highlight haskell %}
tts :: Language -> [String] -> IO ()
tts lang [] = undefined
tts lang args = run lang args

run :: Language -> [String] -> IO ()
run = undefined
{% endhighlight %}

Our run function accepts the language and the text to say. We are going to bla
bla bla using spawnProcess and waitForProcess:

{% highlight haskell %}
run :: Language -> [String] -> IO ()
run lang args = fork (build lang args) >>= exitWith

fork :: (FilePath, [String]) -> IO ExitCode
fork p = uncurry spawnProcess p >>= waitForProcess

build :: Language -> [String] -> (FilePath, [String])
build = undefined
{% endhighlight %}

The code we have added requires us to add a few more imports:

{% highlight haskell %}
import System.Exit (exitWith, ExitCode)
import System.Process (spawnProcess, waitForProcess)
{% endhighlight %}

We are going to use a program called mpg123 which will be able to read the
generated audio stream from the given url (google translate service url). Our
build function is going to prepare everything for the spawnProcess to launch the
mpg123 with correct arguments. For that we need some intial variables like path
to the program and the arguments:

{% highlight haskell %}
defprogopts :: (String, [String])
defprogopts = ("mpg123", ["-q"])
{% endhighlight %}

mpg123 can take multiple streams after the -q and play them in sequence. For now
we are only going to build one url with everything that we want to be read:

{% highlight haskell %}
build :: Language -> [String] -> (FilePath, [String])
build lang args =
    let textToRead = intercalate " " args
    in (++ [mkUrl lang textToRead]) `second` defprogopts
{% endhighlight %}

Everything we want to be read is in args. We simply join everything using a
space and get a string of a text. Then we want create a correct url to google
translate service for it to convert our text into audio. Finally we are using a
fancy second method from Control.Arrow which applies the partially applied
function (++ ourUrlToGoogleTranslateService) to the second element of our
defprogopts pair... I am not familiar with Arrows yet, so this is a bit of magic
for myself, I could figure it out in ghci, but still lack full understanding
  (which hopefully will come with time):

{% highlight haskell %}
ghci
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
λ: import Control.Arrow (second)
λ: (++ ["http://example.com/somestuff"]) `second` ("mpg123", ["-q"])
("mpg123",["-q","http://example.com/somestuff"])
{% endhighlight %}

When we give this final pair to the spawn process it will launch mpg123 with the
list of arguments we provided as a second item in this pair.

Here is how we make an url to access google translate service to convert text to
audio:

{% highlight haskell %}
mkUrl :: Language -> String -> String
mkUrl lang str = "http://translate.google.com/translate_tts?ie=UTF-8&tl="
               ++ language lang ++ "&q=" ++ urlEncode str
{% endhighlight %}

Pretty simple, you can check [this url](http://translate.google.com/translate_tts?ie=UTF-8&tl=en&q=Hello world){:target="_blank"}
to hear "Hello world" being read by google translate.

Finally, in order for our code to compile and run we need to add a few imports
we just used in our methods:

{% highlight haskell %}
import Data.List (intercalate)
import Control.Arrow (second)
import Network.HTTP.Base (urlEncode)
{% endhighlight %}

Now we can run our program and play with it a bit:

{% highlight bash %}
cabal run -- Hello world! "Isn't" it "great?"
{% endhighlight %}

**TODO: somewhere in the blog post we need to add the correct dependencies to
the cabal file**

Our next step is run the program in RESL mode. For that we need to return to our
tts function and work on the case when no text has been supplied as a command
line argument.
