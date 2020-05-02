#!/bin/sh
git clone https://github.com/gambit/gambit.git
cd gambit
./configure        # --enable-single-host optional but recommended
make -j            # build runtime library, gsi and gsc
make modules       # compile the builtin modules (optional but recommended)
sudo make install  # install
cd ..
