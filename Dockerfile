# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.187.0/containers/ubuntu/.devcontainer/base.Dockerfile

ARG DEBIAN_FRONTEND=noninteractive

# [Choice] Ubuntu version: bionic, focal
ARG VARIANT="focal"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

RUN apt-get update \
    && apt-get -y install --no-install-recommends build-essential software-properties-common xz-utils g++ sbcl julia python3 python3-pip python3-dev ghc openjdk-11-jdk libssl-dev gfortran libxml2-dev libyaml-dev libgmp-dev libz-dev libncurses5 gnuplot nodejs npm lua5.3 ocaml php ruby-full gnu-smalltalk scratch libfftw3-dev cmake mono-devel

# Setup Crystal
RUN echo 'deb http://download.opensuse.org/repositories/devel:/languages:/crystal/xUbuntu_20.04/ /' | sudo tee /etc/apt/sources.list.d/devel:languages:crystal.list
RUN curl -fsSL https://download.opensuse.org/repositories/devel:languages:crystal/xUbuntu_20.04/Release.key | gpg --dearmor | sudo tee /etc/apt/trusted.gpg.d/devel_languages_crystal.gpg > /dev/null

# Setup Dart
RUN sudo sh -c 'wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -'
RUN sudo sh -c 'wget -qO- https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list'

# Setup Powershell
RUN sudo sh -c 'wget -q https://packages.microsoft.com/config/ubuntu/$(lsb_release -rs)/packages-microsoft-prod.deb -O packages-microsoft-prod.deb'
RUN sudo sh -c 'dpkg -i packages-microsoft-prod.deb'

# Setup Clojure
RUN sudo sh -c 'curl -O https://download.clojure.org/install/linux-install-1.10.3.967.sh'
RUN sudo sh -c 'chmod +x linux-install-1.10.3.967.sh'
RUN sudo sh -c 'sudo ./linux-install-1.10.3.967.sh'

# Setup dotnet
RUN sudo sh -c 'wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb'
RUN sudo sh -c 'sudo dpkg -i packages-microsoft-prod.deb'
RUN sudo sh -c 'rm packages-microsoft-prod.deb'

# Setup D Lang
ENV DLANG_VERSION=2.097.2
RUN mkdir -p ~/dlang && wget https://dlang.org/install.sh -O ~/dlang/install.sh
RUN bash ~/dlang/install.sh dmd-$DLANG_VERSION
ENV PATH=$PATH:~/dlang/dmd-$DLANG_VERSION/linux/bin64/

# Setup Go
RUN sudo sh -c 'wget -c https://dl.google.com/go/go1.14.2.linux-amd64.tar.gz -O - | sudo tar -xz -C /usr/local'
ENV PATH=$PATH:/usr/local/go/bin

# Setup Kotlin
RUN mkdir -p ~/kotlin && wget -c https://github.com/JetBrains/kotlin/releases/download/v1.5.30/kotlin-compiler-1.5.30.zip -O ~/kotlin/kotlinc.zip && cd ~/kotlin && unzip kotlinc.zip
ENV PATH=$PATH:~/kotlin/kotlinc/bin

# Setup lolcode
RUN git clone https://github.com/justinmeza/lci.git ~/lolcode && cd ~/lolcode && mkdir build && cd build && cmake .. && make -B
ENV PATH=$PATH:~/lolcode/build

# Setup Piet
RUN python3 -m pip install --no-cache-dir repiet

# Setup Matlab
# ?????? This is a licensed language???

# Setup Emojicode
RUN mkdir -p ~/emojicode && wget -c https://github.com/emojicode/emojicode/releases/download/v1.0-beta.2/Emojicode-1.0-beta.2-Linux-x86_64.tar.gz -O ~/emojicode/emojicode.tar.gz && \
    tar -xzf ~/emojicode/emojicode.tar.gz -C ~/emojicode --strip-components=1
ENV PATH=$PATH:~/emojicode

# Setup Factor
RUN mkdir -p ~/factor && wget https://downloads.factorcode.org/releases/0.98/factor-linux-x86-64-0.98.tar.gz -O ~/factor/factor.tar.gz && tar -xzf ~/factor/factor.tar.gz -C ~/factor --strip-components=1
ENV PATH=$PATH:~/factor/factor

# Setup R
RUN sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'

# Setup Racket and Scheme
# To run scheme files, use `racket -f <file.ss>`
RUN sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys D9D33FCD84D82C17288BA03B3C9A6980F827E01E
RUN sudo add-apt-repository 'deb http://ppa.launchpad.net/plt/racket/ubuntu focal main'

# Setup Rust
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y

# Setup Scratch
## using 1.x right now.... in future checkout snap or adobe air?

# Setup Swift
RUN mkdir -p ~/swift && wget https://swift.org/builds/swift-5.5-release/ubuntu2004/swift-5.5-RELEASE/swift-5.5-RELEASE-ubuntu20.04.tar.gz -O ~/swift/swift.tar.gz && \
    tar -xzf ~/swift/swift.tar.gz -C ~/swift --strip-components=1
ENV PATH=$PATH:~/swift/usr/bin

# Setup viml
# To run vim script commands use `/usr/bin/vim -c ":source %" <path_to_file>`
RUN apt-get -y install --no-install-recommends vim

# Setup whitespace
RUN mkdir -p ~/whitespace && git clone https://github.com/wspace/whitespace-haskell ~/whitespace && cd ~/whitespace && make -B
ENV PATH=$PATH:~/whitespace

# Setup Elm
RUN mkdir -p ~/elm && curl -L -o ~/elm/elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz && \
    gunzip ~/elm/elm.gz && chmod +x ~/elm/elm
ENV PATH=$PATH:~/elm

# Setup V
RUN mkdir -p ~/vlang && wget https://github.com/vlang/v/releases/download/weekly.2021.44/v_linux.zip -O ~/vlang/vlang.zip && \
    unzip ~/vlang/vlang.zip -d ~/vlang
ENV PATH=$PATH:~/vlang/v

# Install the packages that needed extra help
RUN apt-get update \
    && apt-get -y install --no-install-recommends crystal dart nim powershell scala dotnet-sdk-5.0 r-base racket


RUN python3 -m pip install --no-cache-dir wheel matplotlib numpy coconut scons

RUN sudo sh -c 'npm install -g typescript'
