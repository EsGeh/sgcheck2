# sgcheck2: manage synchronization of directories using config files

# Concept

The purpose of this tool is to synchronize directories between a so called "server" and your local system (e.g. your laptop).

Draft:

  LOCAL:                            SERVER:

    /local/files/                     /some/subdir/
    |- a/...       <---check out      |- a/...
                     --check in -->   |
                                      |- b/...
                                      |- c/
                                      |- ...

Copying of files is based on rsync.
The advantage of using this tool (over directly typing a copy command into your terminal) is that it will memorize the important informations, like:

- from which path on the server did I check out file A?
- which rsync options should I use?
- how and where to store rsync logging output?

All this information is stored in a config dir inside your HOME directory.
Its a simple tool and simple to use.
For details, check out the "usage" section.

Work in progress. Use at your own risk.
Feedback welcome.

# Dependencies

- [rsync](https://rsync.samba.org/)

# Prerequisits

- git
- stack

# Installation

1. clone repository

		$ git clone https://github.com/EsGeh/sgcheck2.git

1. run tests

		$ ./scripts/test.sh

1. install sgcheck2

		$ stack install

1. add `stack path --local-bin` to your PATH, if necessary

# Usage

- print basic usage info:

		$ sgcheck2 --help

- create main config file:

		$ sgcheck2 writeConfig
	
	edit as needed, especially `serverPath` and `thisPath`
