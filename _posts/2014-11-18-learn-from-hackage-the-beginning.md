---
layout: post
title: "Learn From Hackage: The Beginning"
modified:
categories: 
excerpt: An introduction to the "Learn From Hackage" series
tags: [learn from hackage, haskell, tutorial, series, blog]
image:
  feature:
date: 2014-11-18T15:53:57+02:00
comments: true
---

Learning Haskell it not an easy task. When you work with some imperative language and then switch to some other imperative language you can start working on complex projects in no time as the foundations are the same. Switching to Haskell brings frustration, we want to start coding complex projects in no time but cannot even imagine that it might take a lot of time and effort to learn all those things which are essential in Haskeller's toolbox.

There are good resources to start learning Haskell, like [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) and [Real World Haskell](http://book.realworldhaskell.org/). There are also other good books and resources which might help you to start fiddling with Haskell.

The problem comes after you finish with those books and resources. What now?  Pretty much every single time it is answered with something like "you need to start coding your own project(s)" or "you could contribute to some open source project". Both of these suggestions are 100% correct but not all of us can easily go and start doing something or contributing to some project.

I am one of those eager to learn but not really knowing what projects to work on or which open source project to contribute to. I look for different tutorials and articles which come up from time and I learn a lot from them. But for some reason I cannot work on something of my own. And I really want to.  

Looking at the great [Code Deconstructed](https://www.youtube.com/watch?v=FEFETKhhq8w&list=PLxj9UAX4Em-IBXkvcC3MycLlcxyoi7v8B) series on YouTube I thought that it might be a good idea to start something similar. I thought it might help to learn a bit more if I go through some small projects on Hackage and try learning a thing or two from them. And sharing the knowledge with the others along the way. I am no good with screencasting, so I decided to go with blog posts.

The idea is to deconstruct a project and "rebuild" it from ground up, describing the process via the blog posts. Hopefully this will not end up as an idea and you will see a lot of projects "deconstructed".

And to beging these series we will look at an awesome program you can find on Hackage which goes under name [hnop](http://hackage.haskell.org/package/hnop).  According to the description it was written by Ashley Yakeley.  The name can give you hint of what it does... It does nothing. ***No Operation*** in Haskell: 

{% highlight haskell %}
module Main where

-- | main, do nothing
main :: IO ()
main = return ()
{% endhighlight %}

Hopefully future projects will be more useful than this one and will help us to learn something new. But I had to start with something!
