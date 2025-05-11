;;; ai-mode-anthropic.el --- ai-mode integration with Anthropic API  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Alex

;; Author: Alex <Lispython@users.noreply.github.com>
;; Maintainer: Alex <Lispython@users.noreply.github.com>
;; URL: https://github.com/ai-mode/ai-mode-anthropic
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: help tools ai

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides integration between ai-mode and Anthropic's Claude API.
;; It enables the use of Claude models (Opus, Sonnet, Haiku) within Emacs
;; through the ai-mode interface.
;;
;; Features:
;; - Support for multiple Claude model variants
;; - Asynchronous API communication
;; - Customizable parameters (temperature, max tokens, etc.)
;; - Role mapping for different message types
;; - Error handling and response processing
;;
;; To use this package:
;; 1. Install ai-mode and ai-mode-anthropic
;; 2. Set your Anthropic API key: (setq ai-mode-anthropic--api-key "your-key")
;; 3. The Claude models will be available in ai-mode's model selection
;;

;;; Code:

;; Happy coding! ;)


(require 'ai-utils)

(require 'url)


(defgroup ai-mode-anthropic nil
  "Integration with Anthropic Claude API."
  :prefix "ai-mode"
  :group 'ai-mode
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode-anthropic"))

(defcustom ai-mode-anthropic--model-temperature 0.7
  "What sampling temperature to use, between 0 and 2."
  :type '(choice integer (const nil))
  :group 'ai-mode-anthropic)

(defcustom ai-mode-anthropic--completion-choices 1
  "How many completions to generate for each prompt."
  :type '(choice integer (const nil))
  :group 'ai-mode-anthropic)

(defcustom ai-mode-anthropic--default-max-tokens 4096
  "The maximum number of tokens to generate in the completion."
  :type '(choice integer (const nil))
  :group 'ai-mode-anthropic)

(defcustom ai-mode-anthropic--api-key ""
  "Key for Anthropic API."
  :type 'string
  :group 'ai-mode-anthropic)

(defcustom ai-mode-anthropic-version "2023-06-01"
  "Anthropic API version."
  :type 'string
  :group 'ai-mode-anthropic)

(defcustom ai-mode-anthropic-request-timeout 60
  "Anthropic request timeout in seconds."
  :type '(choice integer (const nil))
  :group 'ai-mode-anthropic)


(defun ai-mode-anthropic--get-response-choices (response)
  "Extract choices list from RESPONSE."
  (cdr (assoc 'content response)))


(cl-defun ai-mode-anthropic--extract-response-or-error (response)
  "Extract success response from RESPONSE or raise error."
  (if (assoc 'error response)
      (error (cdr (assoc 'message (cdr (assoc 'error response)))))
    response))

(cl-defun ai-mode-anthropic--extract-error-messages (response)
  "Extract error message from RESPONSE."
  (if (assoc 'error response)
      (cdr (assoc 'message (cdr (assoc 'error response))))
    "unknown"))

(defvar ai-mode-anthropic--url "https://api.anthropic.com/v1/messages"
  "Base URL for Anthropic API messages endpoint.")


(defcustom ai-mode-anthropic--default-role-mapping
  '(("system" . "assistant")
    ("assistant" . "assistant")
    ("user" . "user"))
  "Default mapping from generic roles to Anthropic-specific roles."
  :group 'ai-mode-anthropic)


(cl-defun ai-mode-anthropic--async-api-request (request-data callback &key (fail-callback nil) (extra-params nil))
  "Asynchronous REQUEST-DATA execution of a request to the Anthropic Claude API.

In case of a successful request execution, a CALLBACK function is called.

If request failed then FAIL-CALLBACK called if it is provided.
EXTRA-PARAMS is a list of properties (plist) that can be used to store parameters."
  (when (null ai-mode-anthropic--api-key)
    (error "Anthropic API key is not set"))

  (let* ((api-url (map-elt extra-params :api-url ai-mode-anthropic--url))
         (timeout (map-elt extra-params :timeout ai-mode-anthropic-request-timeout))
         (encoded-request-data (encode-coding-string (json-encode request-data) 'utf-8))
         (headers  `(("Content-Type" . "application/json")
                     ("anthropic-version" . ,ai-mode-anthropic-version)
                     ("x-api-key" . ,(format "%s" ai-mode-anthropic--api-key)))))
    (ai-utils--async-request api-url "POST" encoded-request-data headers callback :timeout timeout)))

(defcustom ai-mode-anthropic--struct-type-role-mapping
  '((system . "user")
    (assistant . "assistant")
    (user . "user")
    (agent-instructions . "user")
    (buffer-bound-prompt . "user")
    (global-system-prompt . "user")
    (global-memory-item . "user")
    (additional-context . "user")
    (user-input . "user")
    (assistant-response . "assistant")
    (action-context . "user")
    (file-context . "user"))
  "Mapping from structure types to roles for Anthropic API.
Supports both string keys and keyword symbols as types that map to
the two valid Anthropic role values: 'assistant' and 'user'.
Note: Anthropic doesn't support system role directly, so system messages are mapped to user role."
  :type '(alist :key-type (choice string symbol)
                :value-type string)
  :group 'ai-mode-anthropic)


(defun ai-mode-anthropic--get-role-for-struct-type (struct-type)
  "Return the role for the given STRUCT-TYPE using customizable role mapping."
  (let* ((role-mapping ai-mode-anthropic--struct-type-role-mapping)
         (type (if (symbolp struct-type) (symbol-name struct-type) struct-type))
         (role (if (symbolp struct-type)
                   (cdr (cl-assoc struct-type role-mapping))
                 (cdr (cl-assoc type role-mapping :test #'equal)))))
    role))


(defun ai-mode-anthropic--convert-struct-to-model-message (item role-mapping)
  "Convert a single ITEM into a message format using ROLE-MAPPING.
Supports both alist and plist structures for ITEM."
  (cond
   ;; Handle alist structure
   ((and (listp item) (consp (car item)) (stringp (caar item)))
    (let* ((role (cdr (assoc "role" item)))
           (model-role (or (cdr (assoc role role-mapping))
                           (ai-mode-anthropic--get-role-for-struct-type role)))
           (content (cdr (assoc "content" item))))
      `(("role" . ,model-role)
        ("content" . ,content))))
   ;; Handle plist structure
   ((plistp item)
    (let* ((type (plist-get item :type))
           (model-role (ai-mode-anthropic--get-role-for-struct-type type))
           (content (plist-get item :content)))
      `(("role" . ,model-role)
        ("content" . ,content))))))


(defun ai-mode-anthropic--structs-to-model-messages (messages model)
  "Convert MESSAGES into Anthropic API messages using MODEL role-mapping."
  (let* ((role-mapping (map-elt model :role-mapping)))
    (mapcar (lambda (item)
              (ai-mode-anthropic--convert-struct-to-model-message item role-mapping))
            messages)))


(defun ai-mode-anthropic--make-typed-struct (message)
  "Convert a single MESSAGE into an internal typed structure."
  (let* ((content (cdr (assoc 'text message))))
    (ai-common--make-typed-struct content 'assistant-response)))


(defun ai-mode-anthropic--convert-items-to-context-structs (items)
  "Convert ITEMS into internal context.
ITEMS should contain message entries with 'text fields."
  (mapcar #'ai-mode-anthropic--make-typed-struct items))


(cl-defun ai-mode-anthropic--convert-context-to-request-data (context model &key (extra-params nil))
  "Convert CONTEXT associative array to request data format.

CONTEXT should be an alist containing:
- :messages - list of message structures to send

MODEL should contain configuration like:
- :version - model version string
- :temperature - sampling temperature
- :max-tokens - maximum tokens to generate
- :n - number of completions
- :rest-params - additional API parameters

EXTRA-PARAMS can provide additional request parameters."
  (let* ((version (map-elt model :version))
         (temperature (map-elt model :temperature))
         (max-tokens (map-elt model :max-tokens ai-mode-anthropic--default-max-tokens))
         (n (map-elt model :n ai-mode-anthropic--completion-choices))
         (model-rest-params (map-elt model :rest-params))
         (messages (ai-mode-anthropic--structs-to-model-messages (map-elt context :messages) model))
         (payload (append
                   `(("model" . ,version)
                     ("messages" . ,messages))
                   (if max-tokens `(("max_tokens" . ,max-tokens)))
                   (if temperature `(("temperature" . ,temperature)))
                   (if model-rest-params model-rest-params))))
    payload))



(defun ai-mode-anthropic--json-error-to-typed-struct (json-response)
  "Convert JSON-RESPONSE error into a typed structure with type 'error."
  (let* ((error (cdr (assoc 'error json-response)))
         (message (cdr (assoc 'message error)))
         (additional-props (list :type (cdr (assoc 'type error))
                                 :code (cdr (assoc 'code error)))))
    (ai-common--make-typed-struct message 'error :additional-props additional-props)))



(cl-defun ai-mode-anthropic--async-send-context (context model &key success-callback (fail-callback nil) (extra-params nil))
  "Async execute CONTEXT, extract message from response and call SUCCESS-CALLBACK.

If request fails, call FAIL-CALLBACK.

EXTRA-PARAMS is a list of properties (plist) that can be used to store parameters."

  (let ((request-data (ai-mode-anthropic--convert-context-to-request-data context model :extra-params extra-params)))
    (ai-mode-anthropic--async-api-request
     request-data
     (lambda (response)
       (if (assoc 'error response)
           (when fail-callback
             (funcall fail-callback request-data (ai-mode-anthropic--json-error-to-typed-struct response)))
         (let* ((response-content (ai-mode-anthropic--extract-response-or-error response))
                (choices (ai-mode-anthropic--get-response-choices response-content))
                (messages (ai-mode-anthropic--convert-items-to-context-structs choices)))
           (funcall success-callback messages))))
     :fail-callback fail-callback
     :extra-params extra-params)))


(defun ai-mode-anthropic--setup-assistant-backend ()
  "Return the async send context function for the assistant backend."
  ;; Setup model specified variables
  'ai-mode-anthropic--async-send-context)


(defun ai-mode-anthropic--setup-api-token ()
  "Setup Anthropic API token interactively."
  (interactive)
  (let* ((api-token (read-from-minibuffer "Enter api token: ")))
    (customize-set-variable 'ai-mode-anthropic--api-key api-token)))


(cl-defun ai-mode-anthropic--make-model (version &key
                                                 name
                                                 max-tokens
                                                 num-choices
                                                 temperature
                                                 (role-mapping ai-mode-anthropic--default-role-mapping)
                                                 rest-params)
  "Create model configuration for a specific VERSION.

Optional keyword arguments:
- NAME: Display name for the model (defaults to \"Anthropic VERSION\")
- MAX-TOKENS: Maximum tokens to generate
- NUM-CHOICES: Number of completions to generate
- TEMPERATURE: Sampling temperature
- ROLE-MAPPING: Custom role mapping (defaults to ai-mode-anthropic--default-role-mapping)
- REST-PARAMS: Additional API parameters

Returns an alist with model configuration."

  (let* ((name (if name name (format "Anthropic %s" version)))
         (model (append `((:name . ,name)
                          (:provider . "Anthropic")
                          (:version . ,version)
                          (:execution-backend . ,'ai-mode-anthropic--async-send-context)
                          (:setup-function . ,'ai-mode-anthropic--setup-assistant-backend)
                          (:role-mapping . ,role-mapping)
                          (:rest-params . ,rest-params))
                        (if max-tokens `((:max-tokens . ,max-tokens)))
                        (if num-choices `((:num-choices . ,num-choices)))
                        (if temperature `((:temperature . ,temperature))))))
    model))



(defun ai-mode-anthropic--get-models ()
  "Return a list of available Anthropic Claude models with their configurations."
  (list
   (ai-mode-anthropic--make-model "claude-opus-4-0" :max-tokens 32000)
   (ai-mode-anthropic--make-model "claude-sonnet-4-0" :max-tokens 64000)
   (ai-mode-anthropic--make-model "claude-3-7-sonnet-latest" :max-tokens 64000)
   (ai-mode-anthropic--make-model "claude-3-5-haiku-latest" :max-tokens 8192)
   (ai-mode-anthropic--make-model "claude-3-5-sonnet-latest" :max-tokens 8192)
   (ai-mode-anthropic--make-model "claude-3-opus-latest" :max-tokens 4096)))

(provide 'ai-mode-anthropic)
;;; ai-mode-anthropic.el ends here
