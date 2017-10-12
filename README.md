# KeyWar

KeyWar is a twitch overlay displaying what you are typing, greatly inspired by [Jessica's Mak](https://www.twitch.tv/jessicamak) one.

This software works conjointly with the [logkeys](https://github.com/kernc/logkeys) keylogger. 

## Passwords

Be extra cautious while you stream, you do not want to leak any password. 

I have your back, just hit the menu keyboard key to toggle the password mode. In this mode, keywar displays question marks instead of the actual keys.

## Getting Started

Keywar needs to be used in conjunction with a keylogger. You first need to install [Logkeys](https://github.com/kernc/logkeys/blob/master/INSTALL). Depending on your Linux distribution, it could be pre-packaged or you could need to build it from the source.

Keywar will also need the [GLFW](http://www.glfw.org/) library. This library is probably already packaged for your distribution somewhere.

Once installed, you need to add two lines to your shell configuration to make logkeys handy to use. 

```
alias logUp="touch /tmp/keylogger.log && sudo logkeys --start --output /tmp/keylogger.log --no-timestamps"
alias logDown="sudo logkeys --kill && sudo rm /tmp/keylogger.log"
```

You can now use the logUp command to start the keylogger and the logDown command to stop it.

Note that we delete the log file after stopping the keylogger to avoid leaking data everywhere.

After building keywar (see Build section in this readme), you can launch it by typing


```
  keywar 
```

## Usage

```
  keywar [options]
     [-l,--logFile <logFile>]  Logger output file, defaults to /tmp/keylogger.log.  
```

## Building

You will need Haskell's [Stack](https://docs.haskellstack.org/en/stable/README/) to build this project. 

Once stack is installed, clone this git repository and type in it:

```
stack install
```

The executable will be generated in your ~/.local/bin directory. You probably want to add this directory to your PATH environment variable by adding the following line to your ~/.bashrc or ~/.zshrc


```
export PATH="~/.local/bin:${PATH}"
```

## Bug / Feature Request

Just create a GitHub issue, we'll see what we can do.

PR welcomed!
