FROM        darinmorrison/haskell
MAINTAINER  Andrew Rademacher <andrewrademacher@gmail.com>

RUN apt-get update
RUN apt-get install llvm freeglut3-dev -y

RUN cabal update
RUN cabal install
