FROM archlinux

RUN yes | pacman -Sy git ghc make haskell-zlib haskell-cryptohash \
        haskell-base16-bytestring haskell-unix-compat haskell-split \
        vim

ENV EDITOR=vim
ENV PATH="/oshit:${PATH}"
CMD bash -c "pushd /oshit && make; popd; bash"
