# Anthropic Backend for AI Mode

## Table of Contents

- [Overview](#overview)
- [Installation](#installation)
  - [Git Installation](#git-installation)
  - [Package Manager (Future)](#package-manager-future)
- [Configuration](#configuration)
  - [Basic Configuration](#basic-configuration)
  - [Prompt Caching Configuration](#prompt-caching-configuration)
- [Usage](#usage)
  - [Model Selection](#model-selection)
  - [Integration with AI Mode Features](#integration-with-ai-mode-features)
  - [Prompt Caching](#prompt-caching)
  - [Advanced Configuration](#advanced-configuration)
  - [Adding Custom Models](#adding-custom-models)
- [Related Resources](#related-resources)
  - [AI Mode Ecosystem](#ai-mode-ecosystem)
  - [Documentation and Community](#documentation-and-community)
- [Legal Notice](#legal-notice)

## Overview

The Anthropic backend for `ai-mode` provides seamless integration with Anthropic's Claude models through their API. It serves as a bridge between `ai-mode`'s powerful AI features and Anthropic's advanced language models, enabling you to leverage Claude 3.5 Sonnet, Claude 3 Opus, and other variants within your Emacs environment.

Key features:
- **Multiple model support**: Access various Claude models including Opus, Sonnet, and Haiku variants through a unified interface
- **Flexible configuration**: Customize temperature, max tokens, and other parameters per model to fine-tune AI responses
- **Native API integration**: Direct communication with Anthropic's API using proper authentication and request handling
- **Asynchronous operation**: Non-blocking API calls ensure smooth editor performance during AI interactions
- **Model management**: Easy model selection and switching between different Claude offerings
- **Prompt caching support**: Optimize performance and reduce costs with intelligent caching of frequently used content

This backend plugin enables `ai-mode` to harness the power of Anthropic's Claude models, providing the necessary infrastructure for API communication, authentication, and response processing within the broader `ai-mode` ecosystem.

## Installation

### Git Installation

To get started, clone the repository into your Emacs plugins directory:

```bash
cd ~/.emacs.d/plugins
git clone --recursive https://github.com/ai-mode/ai-mode-anthropic
```

Next, update your `.emacs` configuration file to include the new plugin:

```elisp
(add-to-list 'load-path "~/.emacs.d/plugins/ai-mode-anthropic")
(require 'ai-mode-anthropic)
```

Alternatively, you can use `use-package` for a more modular setup:

```elisp
(use-package ai-mode-anthropic
  :load-path "~/.emacs.d/plugins/ai-mode-anthropic"
  :config
  (setq ai-mode--models-providers
        (append ai-mode--models-providers '(ai-mode-anthropic--get-models)))
  (setq ai-chat--models-providers
        (append ai-chat--models-providers '(ai-mode-anthropic--get-models))))
```

## Configuration

### Basic Configuration

To enable the Anthropic backend, you need to set your API key in your `.emacs` file:

```elisp
(setq ai-mode-anthropic--api-key "your-api-key-here")
```

Make sure that your API key is valid and has the necessary permissions for making API requests to ensure seamless interaction.

### Prompt Caching Configuration

The backend includes support for Anthropic's prompt caching feature, which can significantly reduce latency and costs for frequently used content. You can configure caching behavior:

```elisp
;; Maximum number of cached blocks per request (default: 4)
(setq ai-mode-anthropic-max-cache-blocks 4)

;; API version with caching support (automatically managed)
(setq ai-mode-anthropic-version "2023-06-01")
```

The backend automatically manages cache headers and TTL ordering based on content analysis. When caching is enabled, the system:
- Evaluates content for caching eligibility using `ai-mode-adapter-api-should-cache-content-p`
- Determines appropriate TTL values individually for each content block
- Ensures proper ordering to comply with Anthropic's API requirements
- Automatically adds the required beta header for 1-hour cache support when needed

## Usage

Once configured, the Anthropic backend integrates seamlessly with `ai-mode`, enabling you to use Claude models in all `ai-mode` features:

### Model Selection

The backend provides multiple Claude models with different configurations:
- **Claude Opus**: Most capable model for complex tasks (claude-opus-4-0, claude-3-opus-latest)
- **Claude Sonnet**: Balanced performance and cost (claude-sonnet-4-0, claude-3-7-sonnet-latest, claude-3-5-sonnet-latest)
- **Claude Haiku**: Fast and efficient for simpler tasks (claude-3-5-haiku-latest)

You can switch between models using `ai-mode`'s model selection interface, with each model optimized for different use cases.

### Integration with AI Mode Features

The Anthropic backend works with all `ai-mode` capabilities:
- **Code completion and generation**: Leverage Claude models for intelligent code suggestions
- **Chat interactions**: Use `ai-chat` with Claude models for conversational AI assistance
- **Code refactoring**: Apply Claude's understanding to improve code structure
- **Documentation generation**: Create comprehensive docs using language models
- **Custom prompts**: Send any prompt through the unified `ai-mode` interface

### Prompt Caching

When enabled by `ai-mode`, the backend automatically optimizes requests using Anthropic's prompt caching:

- **Automatic cache detection**: Content is automatically evaluated for caching eligibility
- **TTL management**: Individual TTL values are calculated based on content characteristics
- **Cost optimization**: Frequently accessed content is cached to reduce API costs
- **Performance improvement**: Cached content reduces response latency
- **Intelligent ordering**: The system ensures proper TTL ordering to meet API requirements

Caching is most effective for:
- System prompts and instructions
- Large context files
- Frequently referenced documentation
- Project-specific context that doesn't change often

### Advanced Configuration

Customize model behavior per use case:

```elisp
;; Set custom parameters for specific models
(setq ai-mode-anthropic--model-temperature 0.7)  ; Sampling temperature (default: 0.7)
(setq ai-mode-anthropic--default-max-tokens 4096)  ; Maximum tokens limit (default: 4096)
(setq ai-mode-anthropic-request-timeout 60)  ; Request timeout in seconds (default: 60)
(setq ai-mode-anthropic-version "2023-06-01")  ; API version (default: "2023-06-01")
(setq ai-mode-anthropic--completion-choices 1)  ; Number of completions (default: 1)

;; Prompt caching settings
(setq ai-mode-anthropic-max-cache-blocks 4)  ; Maximum cached blocks per request (default: 4)
```

### Adding Custom Models

You can extend the list of available Anthropic Claude models by adding your own custom configurations to `ai-mode`'s model providers. This is useful if you want to test specific model versions, experiment with different `temperature` or `max-tokens` settings, or integrate models not explicitly listed by default.

To add a custom model, modify your Emacs configuration file (e.g., `.emacs` or `init.el`) like this:

```elisp
(add-to-list 'ai-mode--models-providers
             (lambda ()
               (list (ai-mode-anthropic--make-model "claude-3-5-sonnet-20240620"
                                                    :name "My Custom Claude Sonnet"
                                                    :temperature 0.5
                                                    :max-tokens 8192))))

;; If you use ai-chat, also add it to ai-chat--models-providers
(add-to-list 'ai-chat--models-providers
             (lambda ()
               (list (ai-mode-anthropic--make-model "claude-3-5-sonnet-20240620"
                                                    :name "My Custom Claude Sonnet"
                                                    :temperature 0.5
                                                    :max-tokens 8192))))
```

In this example:
- `claude-3-5-sonnet-20240620` is the model version (which needs to be a valid Anthropic model name).
- `:name` sets a custom display name for your model in `ai-mode`'s selection interface.
- `:temperature` and `:max-tokens` allow you to override the default settings for this specific model.

Remember to restart Emacs or re-evaluate your configuration after making changes. Your custom model will then appear in `ai-mode`'s model selection list.

The backend handles all API communication, authentication, response processing, and prompt caching automatically, allowing you to focus on your work while benefiting from Anthropic's powerful Claude models with optimized performance and cost efficiency.

## Related Resources

### AI Mode Ecosystem

- **[AI Mode](https://github.com/ai-mode/ai-mode)**: The core AI-powered Emacs extension that this backend supports
- **[AI Mode OpenAI](https://github.com/ai-mode/ai-mode-openai)**: OpenAI backend for `ai-mode`.
- **[AI Mode Anthropic](https://github.com/ai-mode/ai-mode-anthropic)**: Anthropic Claude backend for ai-mode
- **[AI Mode DeepSeek](https://github.com/ai-mode/ai-mode-deepseek)**: DeepSeek backend for ai-mode
- **[AI Mode Hugging Face](https://github.com/ai-mode/ai-mode-hf)**: Hugging Face models backend for ai-mode
- **[AI Mode Google Generative AI](https://github.com/ai-mode/ai-mode-google-genai)**: Google Generative AI backend for `ai-mode`.

### Documentation and Community

-   **[AI Mode Discussions](https://github.com/ai-mode/ai-mode/discussions)**: Community forum for questions and ideas

## Legal Notice

This project is an independent open-source initiative and is not affiliated with, endorsed by, or sponsored by Anthropic, PBC, OpenAI, Inc., DeepSeek, or Hugging Face, Inc.

Claude is a trademark of Anthropic, PBC. OpenAI, GPT, and ChatGPT are trademarks or registered trademarks of OpenAI, Inc. DeepSeek is a trademark of DeepSeek. Hugging Face and the Hugging Face logo are trademarks or registered trademarks of Hugging Face, Inc. All other trademarks mentioned in this documentation are the property of their respective owners.

The use of Anthropic's API is subject to Anthropic's terms of service and usage policies. Users are responsible for ensuring their usage complies with all applicable terms and regulations.
