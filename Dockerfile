FROM        darinmorrison/haskell
MAINTAINER  Andrew Rademacher <andrewrademacher@gmail.com>

RUN apt-get update
RUN apt-get install build-essential freeglut3-dev -y

RUN mkdir /src
ADD . /src

WORKDIR /src

RUN cabal update
RUN cabal sandbox init
RUN cabal install --only-dependencies
RUN cabal configure -f llvm
RUN cabal build

RUN cp ./dist/build/life/life           /bin/life
RUN cp ./dist/build/profile/profile     /bin/profile

RUN rm -rf /src

WORKDIR /

CMD profile -w 1000 -h 1000 -g 100 +RTS -N
