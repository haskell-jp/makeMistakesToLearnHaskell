#!/bin/bash

# To run `stack install` successfully, using GHC 9.0.2 is easier until
# LTS Haskell with the later version of GHC is published.
docker run -e PS1='shell> ' --entrypoint sh -it --rm haskell:9.0.2-slim
