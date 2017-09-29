;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; witty-lines.lisp

(in-package :raptor-launcher)

(defparameter *witty-lines*
  '("Bouncing raptors on dragons..."
    "Painting all characters green..."
    "Setting all passwords to qwerty123..."
    "Randomly flipping characters' genders..."
    "Loading up catapults..."
    "Shaving cats..."
    "Rewriting the launcher in C++..."
    "Obfuscating configuration..."
    "Brewing original British tea on your CPU..."
    "Launching all alts at once..."
    "Launching pies..."
    "Wasting your time..."
    "Ordering higher-quality raptors..."
    "Rewriting launcher code..."
    "Stopping the world..."
    "Compiling the kernel..."
    "Finding the kin of bees..."
    "Translating descriptions to Klingon..."
    "Collecting deez nuts..."
    "Loading Windows 98..."
    "Multiplying kirins..."
    "Trading digos around..."
    "Summoning the secret Watu tribe..."
    "Finding the different turret..."
    "Raptor Launcher: better than Uranus."
    "[Insert witty text here]"
    "Flipping gryphons..."
    "Retrieving the mislaunched raptor..."
    "Fetching registry info..."
    "Displacing tentacle bunnies..."
    "Altering character DNA..."
    "[test message please ignore]"
    "Qwertyuiop..."
    "Implementing third-party protocols..."
    "Connecting to Test World..."
    "Washing rubies in rain..."
    "Applying functions to arguments..."
    "Cultivating crops..."
    "Reticulating splines..."
    "Roleplaying..."
    "Fixing Furcadia's keyboard shortcuts..."
    "Destroying the European Union..."
    "Shifting dogs bitwise left..."
    "Implementing bass guitars..."
    "Deleting Furcadia..."
    "Formatting the hard drive..."
    "Setting up hacker backdoors..."
    "Reading floppy disk..."
    "Planting seeds of doubt..."
    "Finding rubber duckies..."
    "Feeding trolls..."
    "Writing philosophical treaties..."
    "(defvar *some-bits-of-code* nil)..."
    "Increasing security by 25%...")
  "These are meant to be funny, you know.")

(defun witty-line ()
  "Returns a random witty line."
  (random-elt *witty-lines*))
