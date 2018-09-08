;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; chat.lisp

(in-package :raptor-launcher/protocol)

(define-protocol chat
    (:documentation "The CHAT protocol describes Raptor Launcher modules that ~
are used for communicating between players using their characters."
     :tags (:raptor-launcher :module :editor)
     :dependencies (post)
     :export t)
  (:class chat (module) ())
  "A chat object. Each class participating in the protocol must subclass this ~
protocol class."
  (:function ic-posts ((chat chat)) list)
  "Returns the list of all IC posts available in the chat, in increasing ~
chronological order."
  (:function (setf ic-posts) ((new-value list) (chat chat)) list)
  "Sets the list of all IC posts in the chat and updates its state to show the
post in the chat."
  (:function add-ic-post ((chat chat) (post post)))
  "Adds a new IC post into the chat.
\
This function is equivalent to (PUSH POST (IC-POSTS CHAT)), but programmers are
advised to use ADD-IC-POSTS for performance reasons."
  (:function ooc-posts ((chat chat)) list)
  "Returns the list of all OOC posts available in the chat, in increasing ~
chronological order."
  (:function (setf ooc-posts) ((new-value list) (chat chat)) list)
  "Sets the list of all OOC posts in the chat and updates its state to show the
post in the chat."
  (:function add-ooc-post ((chat chat) (post post)))
  "Adds a new OOC post into the chat.
\
This function is equivalent to (PUSH POST (OOC-POSTS CHAT)), but programmers are
advised to use ADD-IC-POSTS for performance reasons.")

(execute-protocol chat)
