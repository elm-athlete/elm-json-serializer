# Elm Serializer

This utility provides an easy way to generate `Json.Encode.Value` generators (i.e. `a -> Json.Encode.Value` functions) and `Json.Decode.Decoder` (i.e. `Json.Decode.Decoder a` functions) directly from your elm source files. How does it work? It is looking into your source files, parse them, and generates the corresponding encoders and decoders direcly into your source tree, in elm files, readable and formatted! (Thanks to elm-format!)

# Table of Contents

1. [Supported Types](#supported-types)
2. [Installation](#installation)
3. [Usage](#usage)
4. [Why doing such a thing?](#why-doing-such-a-thing)
5. [Feedback](#feedback)
6. [Contributing](#contributing)
7. [Thanks](#thanks)

# Supported types

Right now, it supports encoders and decoders generation for records and union types, and is able to handle dependencies automatically.  
Unfortunately, it can't see if one type is opaque or not at this time. Decoders and Encoders will be generated, but they probably won't be usable, unless your types become visible. Such a behavior could seem strange, but in your applications, you're probably writing really few opaque types, and opaque types really shines on elm-package, where it allows to change the internals without changing the public API. But, in reality, you rarely want to send data contained in opaque types over the wire. So it's not a big default today.  
Furthermore, it can not check into `elm-stuff/` at the time, because most of the time, you don't need to serialize structures from your elm packages to send them to your backend/server. I'll be happy to think about it when a valid use case will be there!

# Installation

Elm Serializer uses elm 0.19.0. It is not widely available. So, to avoid limitiation you have to set $ELM to your path before installing this package.

```sh
export ELM=/path/to/elm/0.19
```

It's simpler to install it from your local computer:

```sh
git clone git@github.com:ghivert/elm-json-serializer.git
cd elm-json-serializer
npm install
npm install -g .
```

Because elm@0.19.0 is not yet available, the package is not released on NPM yet. It will be when elm will be available.

# Usage

You have to indicate the source file where resides the type you want to generate decoders and encoders, the type itself and the output folder. If you miss one of those element, the program can't be launched.  
You also need elm-format to run the program (it is required in order to have readable code). It is unavoidable at that time.

```sh
elm-serializer --file src/Model.elm --type Model --output src
```

# Why doing such a thing?

Two reasons.

1. Writing `Json.Decode.Decoder` is painful and complicated for newcomers. It's always better to have generators which can't be wrong, because they already are well-tested. They could use it to learn, and before writing their own decoders. It could also be embed into larger frameworks.

2. If you're planning to use elm on your server (yes, you can!), you're planning to share types between frontend and backend. And you probably want to send those types from one side to the other for free. Because, hey! After all we're using the same language everywhere, with the same type system, and the same underlying representation.  
  While you can just type `JSON.stringify(myFavoriteObject)` and `JSON.parse(myFavoriteObject)` to share objects in JavaScript, you can't do something as simple in elm yet. The only thing you can do is serializing your structures into JSON, and deserializing them into elm after that.

  This is old story.  

  Now, the only thing you have to do is writing your types, generating decoders and encoders, and throw them over the wire! Everything will be correctly handled on both side!

# Feedback

I'll be more than happy to have feedbacks, issues about your problems, or love letters! 💌

# Contributing

Feel free to submit a PR if you want! I'm also open to discuss on the subject on Slack @ghivert or by mail (it's on my GitHub profile).

# Thanks

This project partly takes inspiration from Clojure EDN, and from my work with @tibastral on elm. (Thanks to him, this project strongly lives with his help!)
