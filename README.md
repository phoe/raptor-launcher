# Furcadia Launcher
A variant launcher for Furcadia written in Common Lisp and (in the future) CommonQt/Qt4.

## Goals
  * Ability to launch Furcadia from command line for any given character, skipping the official launcher. (DONE)
  * Ability to edit descriptions and other attributes as pure text.
  * Ability to show arbitrary images when a given character is selected.
  * Ability to mix characters from different mail accounts on a single list.
  * Ability to automatically refresh characters to prevent them from expiration.

## TODO
  * Add logging everywhere

## Mockup
Don't get too excited - it's just a mockup so far, but that's how I imagine the main screen of the launcher to be modeled.

![Mockup](img/mockup.png)

## Requirements
  * Ironclad
  * Drakma
  * CL-JSON
  * ASDF 3.2.1

## Current technique
  1. **Login into Furcadia CMS.**
     1. GET the login page.
     2. Extract the authentication secret from HTML.
     3. POST the login which includes the authentication secret.
  2. **Get the `furc://` login string.**
     1. GET the FurEd page.
     2. Extract the character login secret from HTML.
     3. Fetch the character we want to login as.
     4. POST the character with the character login secret.
     5. Receive the login string from the HTTP response.
  3. **Launch Furcadia.**
     1. Construct the Furcadia launch command, depending on the running OS.
     2. Launch the Furcadia client with the resulting command.
