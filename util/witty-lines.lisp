;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; witty-lines.lisp

(in-package :raptor-launcher/util)

(defparameter *witty-lines*
  '("Loading..."
    "Processing..."
    "Proceeding..."
    "Translating the responses into Klingon..."
    "Uploading your hard drive to the cloud..."
    "Bouncing raptors on dragons..."
    "Painting all characters green..."
    "Setting all passwords to qwerty123..."
    "Randomly flipping characters' genders..."
    "Loading up catapults..."
    "Shaving cats..."
    "Rewriting the launcher in C++..."
    "Obfuscating configuration..."
    "Brewing original British tea on your CPU..."
    "Launching a 90kg projectile over 300 meters..."
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
    "Wiretapping Beekin channels..."
    "Consulting the 8-ball..."
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
    "Shooting a 90kg character over 300 meters..."
    "Connecting to Test World..."
    "Converting GIF portraits into FOX5..."
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

(defparameter *witty-passwords*
  '("hunter2"
    "password"
    "admin"
    "root"
    "123456"
    "12345678"
    "smelly_socks12"
    "look_ma_no_pants"
    "RaptorLauncher"
    "donthackmeplease"
    "qwerty"
    "totallyNotAFakePasswordDude"
    "furcadia-best-game"
    "$up4HP4$$w0Rd_xD"
    "********"))

(defun witty-password ()
  "Returns a random witty password."
  (prin1-to-string (random-elt *witty-passwords*)))
