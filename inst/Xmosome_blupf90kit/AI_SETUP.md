# Author AI configuration

Provide the following variables to the deployed R process using the target
platform's supported secret/configuration mechanism. Verify them in the test
deployment; local environment variables are not automatically deployed. Never
commit real keys or put them in `www/` or client-side configuration.

| Variable | Purpose | Example |
|----------|---------|---------|
| `XMOSOME_AI_API_KEY` | Author API key used when the user leaves Settings empty | `sk-…` |
| `XMOSOME_AI_BASE_URL` | OpenAI-compatible base URL | `https://api.openai.com/v1` |
| `XMOSOME_AI_MODEL` | Default model id | `gpt-4o-mini` (OpenRouter: `openai/gpt-4o-mini`) |
| `XMOSOME_AI_ALLOWED_BASE_URLS` | Additional administrator-approved public HTTPS base URLs, comma separated | `https://YOUR-RESOURCE.openai.azure.com/openai/v1` |
| `XMOSOME_AI_DAILY_LIMIT` | Author-funded questions allowed per client IP per UTC day | `10` (default) |
| `XMOSOME_AI_QUOTA_DIR` | Optional persistent directory for quota counters | `/var/lib/xmosome/ai_quota` |

shinyapps.io note: `deployApp(envVars=...)` is not supported there. For author defaults on shinyapps, use a gitignored `inst/Xmosome_blupf90kit/ai_author_env.local.R` that calls `Sys.setenv(...)`, and force-include that file in the deploy bundle. Never commit real keys.

In the floating AI panel (⚙):
- **API provider** — dropdown (Author configuration / OpenAI / OpenRouter / DeepSeek / Groq / Together / Ollama / Azure / Custom URL)
- **Model** — provider-specific dropdown (switches when you change provider)
- **Test provider** — sends a tiny chat ping with the current settings and reports OK / error + latency (does **not** consume the daily question quota)
- OpenRouter ids must be `vendor/model` (e.g. `openai/gpt-4o`, `deepseek/deepseek-chat`). Plain `gpt-4o` is auto-normalized server-side.
- **HTTP 402** = no credits on that provider — top up, or pick a `:free` OpenRouter model / another provider
- Choosing **Custom** reveals a free-text field
- Select **Author configuration** and leave the key blank to use the author connection. The author endpoint and model cannot be overridden while using the author key.
- Author-funded chat is limited to **10 questions per client IP per UTC day** (configurable via `XMOSOME_AI_DAILY_LIMIT`). Users who paste their own API key bypass this quota.
- Selecting a custom provider requires the user's own key. The endpoint must be one of the built-in approved providers or an exact administrator-approved base URL. URLs with credentials, query strings, non-HTTPS schemes or IP literals are rejected; redirects are not followed.
- Local Ollama endpoints are unavailable in hosted mode. A cloud process cannot access the user's local Ollama instance.

Quota notes:
- Counters are stored as hashed IP + UTC-date files under `XMOSOME_AI_QUOTA_DIR` (or a temp directory).
- On multi-instance hosts without a shared quota directory, each instance enforces its own local counter.
- Behind proxies, the first `X-Forwarded-For` / `X-Real-IP` value is used when present.

Administrators must approve only trusted public provider endpoints, not domains
controlled by untrusted users or resolving to private networks. This is an
endpoint allowlist, not a general-purpose URL proxy. The author-configured endpoint
is also trusted by the administrator.

HTTP response bodies and connection errors are not echoed to the chat, to avoid
leaking credentials through a provider that reflects request headers.

Overrides stay in browser `localStorage` and the Shiny session only.

Required R packages for AI: `jsonlite`, `digest`, and either `httr2` or `httr`.

Before publicly enabling author-funded AI, configure provider-side spending limits
or a shared quota gateway. Per-IP daily throttling alone is not an account-wide budget.
