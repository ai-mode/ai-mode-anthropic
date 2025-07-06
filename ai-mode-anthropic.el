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
;; - Prompt caching support for optimized performance
;;
;; To use this package:
;; 1. Install ai-mode and ai-mode-anthropic
;; 2. Set your Anthropic API key: (setq ai-mode-anthropic--api-key "your-key")
;; 3. The Claude models will be available in ai-mode's model selection
;;

;;; Code:

;; Happy coding! ;)


(require 'ai-mode-adapter-api) ; Added require statement
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

(defcustom ai-mode-anthropic-max-cache-blocks 4
  "Maximum number of blocks with cache_control allowed in a single request.
Anthropic API has a limit of 4 cached blocks per request."
  :type 'integer
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


(cl-defun ai-mode-anthropic--async-api-request (request-data callback &key (fail-callback nil) (extra-params nil) (use-1h-cache nil))
  "Asynchronous REQUEST-DATA execution of a request to the Anthropic Claude API.

In case of a successful request execution, a CALLBACK function is called.

If request failed then FAIL-CALLBACK called if it is provided.
EXTRA-PARAMS is a list of properties (plist) that can be used to store parameters.
USE-1H-CACHE when non-nil enables 1-hour cache support by adding the appropriate beta header."
  (when (null ai-mode-anthropic--api-key)
    (error "Anthropic API key is not set"))

  (let* ((api-url (map-elt extra-params :api-url ai-mode-anthropic--url))
         (timeout (map-elt extra-params :timeout ai-mode-anthropic-request-timeout))
         (encoded-request-data (encode-coding-string (json-encode request-data) 'utf-8))
         (headers  `(("Content-Type" . "application/json")
                     ("anthropic-version" . ,ai-mode-anthropic-version)
                     ("x-api-key" . ,(format "%s" ai-mode-anthropic--api-key)))))
    ;; Add beta header for 1-hour cache support if needed
    (when use-1h-cache
      (push '("anthropic-beta" . "extended-cache-ttl-2025-04-11") headers))
    (ai-mode-adapter-api-async-request api-url "POST" encoded-request-data headers callback :timeout timeout)))

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
    (file-context . "user")
    (project-context . "user")
    (file-metadata . "user")
    (project-ai-summary . "user")
    (memory . "user"))
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
         (struct-type-string (if (symbolp struct-type) (symbol-name struct-type) struct-type))
         (role (if (symbolp struct-type)
                   (cdr (cl-assoc struct-type role-mapping))
                 (cdr (cl-assoc type role-mapping :test #'equal)))))
    (or role struct-type-string)))


(defun ai-mode-anthropic--create-cache-control (ttl enforced-ttl)
  "Create cache control object with specified TTL.
TTL should be a string like '5m', '1h', etc. Uses ai-mode-adapter-api-ttl-to-seconds
to convert TTL to seconds and applies threshold logic:
- If TTL seconds > 300 (5 minutes), use 1-hour cache
- If TTL seconds is between 1 and 300 (inclusive), use 5-minute cache
- If TTL seconds is 0, no cache control is returned

ENFORCED-TTL when non-nil forces the cache to use specific TTL instead of the calculated one
based on the original TTL. This is used to ensure proper TTL ordering in Anthropic API."
  (let ((ttl-seconds (ai-mode-adapter-api-ttl-to-seconds ttl)))
    (cond
     ((= ttl-seconds 0) nil)  ; No caching
     (enforced-ttl  ; Use enforced TTL
      (if (equal enforced-ttl "1h")
          `(("type" . "ephemeral") ("ttl" . "1h"))
        `(("type" . "ephemeral"))))
     ((> ttl-seconds 300) `(("type" . "ephemeral") ("ttl" . "1h")))  ; 1h cache for TTL > 5m
     (t `(("type" . "ephemeral"))))))  ; 5m cache for TTL between 1s and 5m inclusive


(defun ai-mode-anthropic--convert-struct-to-model-message (item role-mapping &optional enable-caching cache-counter enforced-cache-ttl)
  "Convert a single ITEM into a message format using ROLE-MAPPING.
ITEM should be a plist structure.

When ENABLE-CACHING is non-nil, evaluates whether the item should be cached
using `ai-mode-adapter-api-should-cache-content-p` and adds cache_control
parameter if appropriate. TTL is determined individually for each record
based on its parameters using `ai-mode-adapter-api-get-cache-ttl`.

CACHE-COUNTER should be a cons cell (current-count . max-count) to track
the number of cached blocks. The car is incremented when cache is applied.

ENFORCED-CACHE-TTL when non-nil forces all cached items to use this specific TTL
instead of their individually calculated TTL. This ensures TTL ordering requirements."
  (let* ((type (ai-mode-adapter-api-get-struct-type item))
         (model-role (ai-mode-anthropic--get-role-for-struct-type type))
         (content (ai-mode-adapter-api-get-struct-content item))
         (should-cache (and enable-caching
                           (ai-mode-adapter-api-should-cache-content-p item)
                           cache-counter
                           (< (car cache-counter) (cdr cache-counter))))
         (content-block `(("type" . "text")
                          ("text" . ,content))))

    ;; Add cache control if needed and under limit
    (when should-cache
      (let* ((item-ttl (ai-mode-adapter-api-get-cache-ttl item))
             (cache-control (ai-mode-anthropic--create-cache-control item-ttl enforced-cache-ttl)))
        (when cache-control
          (setq content-block (append content-block `(("cache_control" . ,cache-control))))
          ;; Increment the cache counter
          (setcar cache-counter (1+ (car cache-counter))))))

    ;; Always use array format with content blocks
    `(("role" . ,model-role)
      ("content" . ,(vector content-block)))))


(defun ai-mode-anthropic--determine-enforced-cache-ttl (messages)
  "Determine the enforced cache TTL to ensure proper ordering.
Analyzes MESSAGES to find cacheable items and their TTLs, then returns
the minimum TTL that should be enforced to avoid ordering violations.
Returns nil if no enforcement is needed."
  (let* ((cacheable-ttls '()))
    ;; Collect TTLs of all cacheable messages
    (dolist (item messages)
      (when (ai-mode-adapter-api-should-cache-content-p item)
        (let* ((item-ttl (ai-mode-adapter-api-get-cache-ttl item))
               (ttl-seconds (ai-mode-adapter-api-ttl-to-seconds item-ttl)))
          (when (> ttl-seconds 0)
            (push ttl-seconds cacheable-ttls)))))

    ;; If we have both long (>300s) and short (<=300s) TTLs, enforce the shorter one
    (when cacheable-ttls
      (let ((has-long (cl-some (lambda (ttl) (> ttl 300)) cacheable-ttls))
            (has-short (cl-some (lambda (ttl) (<= ttl 300)) cacheable-ttls)))
        (when (and has-long has-short)
          nil)))))  ; Use 5-minute cache for all when mixed


(defun ai-mode-anthropic--structs-to-model-messages (messages model &optional enable-caching)
  "Convert MESSAGES into Anthropic API messages using MODEL role-mapping.
When ENABLE-CACHING is non-nil, evaluates caching for each message.
TTL is determined individually for each record based on its parameters.
Limits the number of cached blocks to ai-mode-anthropic-max-cache-blocks.

Ensures proper TTL ordering by enforcing consistent TTL when mixed long/short TTLs are present."
  (let* ((prepared-messages (ai-mode-adapter-api-prepare-messages messages))
         (role-mapping (map-elt model :role-mapping))
         (cache-counter (when enable-caching (cons 0 ai-mode-anthropic-max-cache-blocks)))
         (enforced-cache-ttl (when enable-caching
                              (ai-mode-anthropic--determine-enforced-cache-ttl prepared-messages))))
    (mapcar (lambda (item)
              (ai-mode-anthropic--convert-struct-to-model-message
               item role-mapping enable-caching cache-counter enforced-cache-ttl))
            prepared-messages)))


(defun ai-mode-anthropic--make-typed-struct (message)
  "Convert a single MESSAGE into an internal typed structure."
  (let* ((content (cdr (assoc 'text message))))
    (ai-common--make-typed-struct content 'assistant-response)))


(defun ai-mode-anthropic--convert-items-to-context-structs (items)
  "Convert ITEMS into internal context.
ITEMS should contain message entries with 'text fields."
  (mapcar #'ai-mode-anthropic--make-typed-struct items))


(defun ai-mode-anthropic--convert-usage-metadata-to-plist (usage-metadata)
  "Convert USAGE-METADATA from Anthropic API response to a plist.
Maps Anthropic API field names to standardized keys:
- input_tokens -> :input-tokens
- cache_creation_input_tokens -> :input-tokens-write-cache
- cache_read_input_tokens -> :input-tokens-read-cache
- output_tokens -> :output-tokens
- total-tokens calculated as sum of input and output tokens"
  (let ((input-tokens (cdr (assoc 'input_tokens usage-metadata)))
        (cache-creation-tokens (cdr (assoc 'cache_creation_input_tokens usage-metadata)))
        (cache-read-tokens (cdr (assoc 'cache_read_input_tokens usage-metadata)))
        (output-tokens (cdr (assoc 'output_tokens usage-metadata))))
    (list :input-tokens input-tokens
          :input-tokens-write-cache cache-creation-tokens
          :input-tokens-read-cache cache-read-tokens
          :output-tokens output-tokens
          :total-tokens (+ (or input-tokens 0) (or output-tokens 0)))))


(cl-defun ai-mode-anthropic--convert-context-to-request-data (context model &key (extra-params nil) enable-caching)
  "Convert CONTEXT associative array to request data format.

CONTEXT should be an alist containing:
- :messages - list of message structures to send

MODEL should contain configuration like:
- :version - model version string
- :temperature - sampling temperature
- :max-tokens - maximum tokens to generate
- :n - number of completions
- :rest-params - additional API parameters

EXTRA-PARAMS can provide additional request parameters.

When ENABLE-CACHING is non-nil, prompt caching will be evaluated for messages.
TTL is determined individually for each record based on its parameters.
The number of cached blocks is limited to ai-mode-anthropic-max-cache-blocks."
  (let* ((version (map-elt model :version))
         (temperature (map-elt model :temperature))
         (max-tokens (map-elt model :max-tokens ai-mode-anthropic--default-max-tokens))
         (n (map-elt model :n ai-mode-anthropic--completion-choices))
         (model-rest-params (map-elt model :rest-params))
         (messages (ai-mode-anthropic--structs-to-model-messages
                    (map-elt context :messages) model enable-caching))
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



(cl-defun ai-mode-anthropic--async-send-context (context model &key success-callback (fail-callback nil) update-usage-callback enable-caching (extra-params nil))
  "Async execute CONTEXT, extract message from response and call SUCCESS-CALLBACK.

If request fails, call FAIL-CALLBACK.

EXTRA-PARAMS is a list of properties (plist) that can be used to store parameters.

When ENABLE-CACHING is non-nil, prompt caching will be evaluated and applied
to appropriate messages based on their content and type using
`ai-mode-adapter-api-should-cache-content-p`. TTL is determined individually
for each record based on its parameters. The number of cached blocks is
limited to ai-mode-anthropic-max-cache-blocks.

When UPDATE-USAGE-CALLBACK is provided, it will be called with usage statistics
converted from the response's usage field."

  (let* ((request-data (ai-mode-anthropic--convert-context-to-request-data
                        context model
                        :extra-params extra-params
                        :enable-caching enable-caching))
         ;; Check if any message uses 1-hour cache by examining the request data
         (use-1h-cache (ai-mode-anthropic--request-uses-1h-cache request-data)))
    (ai-mode-anthropic--async-api-request
     request-data
     (lambda (response)
       (if (assoc 'error response)
           (when fail-callback
             (funcall fail-callback request-data (ai-mode-anthropic--json-error-to-typed-struct response)))
         (let* ((response-content (ai-mode-anthropic--extract-response-or-error response))
                (choices (ai-mode-anthropic--get-response-choices response-content))
                (messages (ai-mode-anthropic--convert-items-to-context-structs choices))
                (usage-metadata (cdr (assoc 'usage response))))
           (when (and update-usage-callback usage-metadata)
             (let ((usage-stats (ai-mode-anthropic--convert-usage-metadata-to-plist usage-metadata)))
               (funcall update-usage-callback usage-stats)))
           (funcall success-callback messages))))
     :fail-callback fail-callback
     :extra-params extra-params
     :use-1h-cache use-1h-cache)))


(defun ai-mode-anthropic--request-uses-1h-cache (request-data)
  "Check if REQUEST-DATA contains any messages with 1-hour cache control."
  (let ((messages (cdr (assoc "messages" request-data))))
    (cl-some (lambda (message)
               (let ((content (cdr (assoc "content" message))))
                 ;; Check if content is an array (vector) of content blocks
                 (when (vectorp content)
                   (cl-some (lambda (content-block)
                              (let ((cache-control (cdr (assoc "cache_control" content-block))))
                                (and cache-control
                                     (equal (cdr (assoc "ttl" cache-control)) "1h"))))
                            content))))
             messages)))


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
