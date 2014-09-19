FROM        darinmorrison/haskell
MAINTAINER  Andrew Rademacher <andrewrademacher@gmail.com>

RUN apt-get update
RUN apt-get install freeglut3-dev git -y

RUN git clone https://github.com/AndrewRademacher/game-of-life.git
RUN     cd game-of-life\
    &&  cabal update\
    &&  cabal install --only-dependencies\
    &&  cabal configure -f llvm\
    &&  cabal install
