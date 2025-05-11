# Anthropic Backend for AI Mode

## Overview

The Anthropic backend for `ai-mode` provides seamless integration with Anthropic's Claude models through their API. It serves as a bridge between `ai-mode`'s powerful AI features and Anthropic's advanced language models, enabling you to leverage Claude 3.5 Sonnet, Claude 3 Opus, and other variants within your Emacs environment.

Key features:
- **Multiple model support**: Access various Claude models including Opus, Sonnet, and Haiku variants through a unified interface
- **Flexible configuration**: Customize temperature, max tokens, and other parameters per model to fine-tune AI responses
- **Native API integration**: Direct communication with Anthropic's API using proper authentication and request handling
- **Asynchronous operation**: Non-blocking API calls ensure smooth editor performance during AI interactions
- **Model management**: Easy model selection and switching between different Claude offerings

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

To enable the Anthropic backend, you need to set your API key in your `.emacs` file:

```elisp
(setq ai-mode-anthropic--api-key "your-api-key-here")
```

Make sure that your API key is valid and has the necessary permissions for making API requests to ensure seamless interaction.

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

### Advanced Configuration

Customize model behavior per use case:

```elisp
;; Set custom parameters for specific models
(setq ai-mode-anthropic--model-temperature 0.2)  ; Lower temperature for more focused responses
(setq ai-mode-anthropic--default-max-tokens 4096)  ; Adjust token limit
(setq ai-mode-anthropic-request-timeout 120)  ; Extend timeout for complex requests
(setq ai-mode-anthropic-version "2023-06-01")  ; API version
```

The backend handles all API communication, authentication, and response processing automatically, allowing you to focus on your work while benefiting from Anthropic's powerful Claude models.

## Related Resources

### AI Mode Ecosystem

- **[AI Mode](https://github.com/ai-mode/ai-mode)**: The core AI-powered Emacs extension that this backend supports
- **[AI Mode OpenAI](https://github.com/ai-mode/ai-mode-openai)**: OpenAI GPT backend for ai-mode
- **[AI Mode DeepSeek](https://github.com/ai-mode/ai-mode-deepseek)**: DeepSeek backend for ai-mode
- **[AI Mode Hugging Face](https://github.com/ai-mode/ai-mode-hf)**: Hugging Face models backend for ai-mode

### Documentation and Community

- **[AI Mode Discussions](https://github.com/ai-mode/ai-mode/discussions)**: Community forum for questions and ideas

## Legal Notice

This project is an independent open-source initiative and is not affiliated with, endorsed by, or sponsored by Anthropic, PBC, OpenAI, Inc., DeepSeek, or Hugging Face, Inc.

Claude is a trademark of Anthropic, PBC. OpenAI, GPT, and ChatGPT are trademarks or registered trademarks of OpenAI, Inc. DeepSeek is a trademark of DeepSeek. Hugging Face and the Hugging Face logo are trademarks or registered trademarks of Hugging Face, Inc. All other trademarks mentioned in this documentation are the property of their respective owners.

The use of Anthropic's API is subject to Anthropic's terms of service and usage policies. Users are responsible for ensuring their usage complies with all applicable terms and regulations.
