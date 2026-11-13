/* Xmosome AI floating chat — client */
(function () {
  var STORAGE_KEY = "xmosome_blupf90kit_ai_cfg";

  var API_PRESETS = [
    { id: "host",       label: "Author configuration (default)", url: "" },
    { id: "openai",     label: "OpenAI",                      url: "https://api.openai.com/v1" },
    { id: "azure",      label: "Azure OpenAI (custom endpoint)", url: "__custom__" },
    { id: "openrouter", label: "OpenRouter",                  url: "https://openrouter.ai/api/v1" },
    { id: "deepseek",   label: "DeepSeek",                    url: "https://api.deepseek.com/v1" },
    { id: "groq",       label: "Groq",                        url: "https://api.groq.com/openai/v1" },
    { id: "together",   label: "Together AI",                 url: "https://api.together.xyz/v1" },
    { id: "ollama",     label: "Ollama (local)",              url: "http://127.0.0.1:11434/v1" },
    { id: "custom",     label: "Custom URL…",                 url: "__custom__" }
  ];

  var MODEL_PRESETS_BY_PROVIDER = {
    host: [
      { id: "host",          label: "Author default model",   value: "" },
      { id: "gpt-4o-mini",   label: "GPT-4o mini",            value: "gpt-4o-mini" },
      { id: "gpt-4o",        label: "GPT-4o",                 value: "gpt-4o" },
      { id: "deepseek-chat", label: "DeepSeek Chat",          value: "deepseek-chat" },
      { id: "custom",        label: "Custom model id…",       value: "__custom__" }
    ],
    openai: [
      { id: "host",          label: "Author default model",   value: "" },
      { id: "gpt-4o-mini",   label: "GPT-4o mini",            value: "gpt-4o-mini" },
      { id: "gpt-4o",        label: "GPT-4o",                 value: "gpt-4o" },
      { id: "gpt-4.1-mini",  label: "GPT-4.1 mini",           value: "gpt-4.1-mini" },
      { id: "gpt-4.1",       label: "GPT-4.1",                value: "gpt-4.1" },
      { id: "o4-mini",       label: "o4-mini",                value: "o4-mini" },
      { id: "custom",        label: "Custom model id…",       value: "__custom__" }
    ],
    openrouter: [
      { id: "host",                label: "Author default model",            value: "" },
      { id: "or-gpt-4o-mini",      label: "OpenAI GPT-4o mini",              value: "openai/gpt-4o-mini" },
      { id: "or-gpt-4o",           label: "OpenAI GPT-4o",                   value: "openai/gpt-4o" },
      { id: "or-deepseek-chat",    label: "DeepSeek Chat",                   value: "deepseek/deepseek-chat" },
      { id: "or-deepseek-r1",      label: "DeepSeek R1",                     value: "deepseek/deepseek-r1" },
      { id: "or-llama-3.3",        label: "Llama 3.3 70B",                   value: "meta-llama/llama-3.3-70b-instruct" },
      { id: "or-qwen",             label: "Qwen2.5 72B",                     value: "qwen/qwen-2.5-72b-instruct" },
      { id: "or-free",             label: "Free: Llama 3.3 70B",             value: "meta-llama/llama-3.3-70b-instruct:free" },
      { id: "custom",              label: "Custom model id…",                value: "__custom__" }
    ],
    deepseek: [
      { id: "host",             label: "Author default model", value: "" },
      { id: "deepseek-chat",    label: "DeepSeek Chat",       value: "deepseek-chat" },
      { id: "deepseek-reasoner",label: "DeepSeek Reasoner",   value: "deepseek-reasoner" },
      { id: "custom",           label: "Custom model id…",    value: "__custom__" }
    ],
    groq: [
      { id: "host",          label: "Author default model",      value: "" },
      { id: "llama-3.3-70b", label: "Llama 3.3 70B",             value: "llama-3.3-70b-versatile" },
      { id: "llama-3.1-8b",  label: "Llama 3.1 8B",              value: "llama-3.1-8b-instant" },
      { id: "custom",        label: "Custom model id…",          value: "__custom__" }
    ],
    together: [
      { id: "host",          label: "Author default model", value: "" },
      { id: "custom",        label: "Custom model id…", value: "__custom__" }
    ],
    ollama: [
      { id: "host",          label: "Author default model", value: "" },
      { id: "llama3.2",      label: "llama3.2", value: "llama3.2" },
      { id: "custom",        label: "Custom model id…", value: "__custom__" }
    ],
    azure: [
      { id: "host",          label: "Author default model", value: "" },
      { id: "custom",        label: "Custom deployment name…", value: "__custom__" }
    ],
    custom: [
      { id: "host",          label: "Author default model", value: "" },
      { id: "custom",        label: "Custom model id…", value: "__custom__" }
    ]
  };

  function modelPresetsFor(apiPreset) {
    return MODEL_PRESETS_BY_PROVIDER[apiPreset] || MODEL_PRESETS_BY_PROVIDER.host;
  }

  function allModelPresets() {
    var out = [];
    Object.keys(MODEL_PRESETS_BY_PROVIDER).forEach(function (k) {
      MODEL_PRESETS_BY_PROVIDER[k].forEach(function (p) { out.push(p); });
    });
    return out;
  }

  var state = {
    open: true,
    settingsOpen: false,
    pendingProposal: null,
    busy: false,
    drag: null,
    busyTimer: null,
    handlersBound: false,
    statusRequested: false,
    localProposals: {}
  };

  function $(sel, root) { return (root || document).querySelector(sel); }
  function el(tag, cls, html) {
    var n = document.createElement(tag);
    if (cls) n.className = cls;
    if (html != null) n.innerHTML = html;
    return n;
  }

  function loadCfg() {
    try { return JSON.parse(localStorage.getItem(STORAGE_KEY) || "{}") || {}; }
    catch (e) { return {}; }
  }
  function saveCfg(cfg) {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(cfg || {}));
    if (window.Shiny) {
      Shiny.setInputValue("ai_user_cfg", cfg || {}, { priority: "event" });
    }
  }

  function pushCfgToShiny() {
    if (window.Shiny) Shiny.setInputValue("ai_user_cfg", loadCfg(), { priority: "event" });
  }

  function optionsHtml(presets) {
    return presets.map(function (p) {
      return '<option value="' + p.id + '">' + p.label + "</option>";
    }).join("");
  }

  function resolveApiUrl(cfg) {
    var preset = API_PRESETS.find(function (p) { return p.id === (cfg.api_preset || "host"); });
    if (!preset || preset.id === "host") return (cfg.base_url || "").trim();
    if (preset.url === "__custom__") return (cfg.base_url_custom || cfg.base_url || "").trim();
    return preset.url;
  }

  function resolveModel(cfg) {
    var presets = modelPresetsFor(cfg.api_preset || "host");
    var preset = presets.find(function (p) { return p.id === (cfg.model_preset || "host"); });
    if (!preset || preset.id === "host") return (cfg.model || "").trim();
    if (preset.value === "__custom__") return (cfg.model_custom || cfg.model || "").trim();
    return preset.value;
  }

  function refreshModelOptions(keepId) {
    var apiSel = $("#ai-cfg-api-preset");
    var modelSel = $("#ai-cfg-model-preset");
    if (!apiSel || !modelSel) return;
    var presets = modelPresetsFor(apiSel.value || "host");
    var prev = keepId || modelSel.value || "host";
    modelSel.innerHTML = optionsHtml(presets);
    var exists = presets.some(function (p) { return p.id === prev; });
    modelSel.value = exists ? prev : "host";
    var ph = $("#ai-cfg-model-custom");
    if (ph) {
      ph.placeholder = (apiSel.value === "openrouter")
        ? "e.g. openai/gpt-4o-mini"
        : "e.g. gpt-4o-2024-08-06";
    }
  }

  function syncCustomVisibility() {
    var apiSel = $("#ai-cfg-api-preset");
    var modelSel = $("#ai-cfg-model-preset");
    var apiCustom = $("#ai-cfg-base-custom-wrap");
    var modelCustom = $("#ai-cfg-model-custom-wrap");
    if (!apiSel || !modelSel) return;
    var apiNeeds = apiSel.value === "custom" || apiSel.value === "azure";
    var modelNeeds = modelSel.value === "custom";
    if (apiCustom) apiCustom.classList.toggle("is-visible", apiNeeds);
    if (modelCustom) modelCustom.classList.toggle("is-visible", modelNeeds);
  }

  function readFormCfg() {
    var apiPreset = ($("#ai-cfg-api-preset") && $("#ai-cfg-api-preset").value) || "host";
    var modelPreset = ($("#ai-cfg-model-preset") && $("#ai-cfg-model-preset").value) || "host";
    var baseCustom = ($("#ai-cfg-base-custom") && $("#ai-cfg-base-custom").value.trim()) || "";
    var modelCustom = ($("#ai-cfg-model-custom") && $("#ai-cfg-model-custom").value.trim()) || "";
    var key = ($("#ai-cfg-key") && $("#ai-cfg-key").value.trim()) || "";
    var cfg = {
      api_preset: apiPreset,
      model_preset: modelPreset,
      base_url_custom: baseCustom,
      model_custom: modelCustom,
      api_key: key,
      base_url: "",
      model: ""
    };
    cfg.base_url = resolveApiUrl(cfg);
    cfg.model = resolveModel(cfg);
    return cfg;
  }

  // Shared vector marks stay crisp at every display scale.
  function aiIcon(name) {
    var paths = {
      assistant: '<path d="M6 4.5h8.5M19.5 13v3a3 3 0 0 1-3 3H9l-4.5 3v-6a3 3 0 0 1-1-2.2V7.5a3 3 0 0 1 3-3"/><path d="m18 2 1.2 3.8L23 7l-3.8 1.2L18 12l-1.2-3.8L13 7l3.8-1.2L18 2Z"/><path d="M8 12h4M8 15.5h7"/>',
      settings: '<path d="M4 7h16M4 17h16"/><circle cx="9" cy="7" r="3"/><circle cx="15" cy="17" r="3"/>',
      close: '<path d="m6 6 12 12M18 6 6 18"/>'
    };
    return '<svg class="ai-symbol" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" focusable="false">' + paths[name] + '</svg>';
  }

  function xmosomeIcon(extraClass) {
    var cls = "ai-xmosome-icon" + (extraClass ? " " + extraClass : "");
    var src = "xmosome-assets/xmosome-icon.png";
    var scripts = document.getElementsByTagName("script");
    for (var i = 0; i < scripts.length; i++) {
      var s = scripts[i].src || "";
      if (s.indexOf("ai-chat.js") !== -1) {
        src = s.replace(/ai-chat\.js(\?.*)?$/, "xmosome-icon.png");
        break;
      }
    }
    return '<img class="' + cls + '" src="' + src + '" alt="" width="48" height="48" decoding="async" aria-hidden="true">';
  }

  function hasSavedOverrides(cfg) {
    cfg = cfg || loadCfg();
    return !!(
      (cfg.api_key && String(cfg.api_key).length) ||
      (cfg.api_preset && cfg.api_preset !== "host") ||
      (cfg.model_preset && cfg.model_preset !== "host") ||
      (cfg.base_url_custom && String(cfg.base_url_custom).length) ||
      (cfg.model_custom && String(cfg.model_custom).length) ||
      (cfg.base_url && String(cfg.base_url).length) ||
      (cfg.model && String(cfg.model).length)
    );
  }

  function updateSettingsChrome() {
    var btn = $("#ai-settings-btn");
    if (btn) btn.classList.toggle("is-configured", hasSavedOverrides());
    updateConnectionSummary();
  }

  function updateConnectionSummary() {
    var summary = $("#ai-cfg-summary");
    if (!summary) return;
    var cfg = readFormCfg();
    var api = API_PRESETS.find(function (p) { return p.id === (cfg.api_preset || "host"); });
    var models = modelPresetsFor(cfg.api_preset || "host");
    var model = models.find(function (p) { return p.id === (cfg.model_preset || "host"); });
    var providerLabel = (api && api.label) || "Author configuration";
    var modelLabel = cfg.model || ((model && model.id !== "host" && model.id !== "custom") ? model.label : "Author default model");
    var endpoint = cfg.base_url || "Author endpoint";
    var keyState = cfg.api_key
      ? "Using browser-saved key (no daily limit)"
      : "Using author key (daily IP quota)";
    summary.innerHTML =
      '<div class="ai-summary-row"><span>Provider</span><strong>' + escapeHtml(providerLabel) + '</strong></div>' +
      '<div class="ai-summary-row"><span>Model</span><strong>' + escapeHtml(modelLabel || "Author default model") + '</strong></div>' +
      '<div class="ai-summary-row"><span>Endpoint</span><strong title="' + escapeAttr(endpoint) + '">' + escapeHtml(shortUrl(endpoint)) + '</strong></div>' +
      '<div class="ai-summary-row"><span>Key</span><strong>' + escapeHtml(keyState) + '</strong></div>';
  }

  function shortUrl(url) {
    if (!url || url === "Author endpoint") return url || "Author endpoint";
    try {
      var u = new URL(url);
      return u.host + (u.pathname === "/" ? "" : u.pathname.replace(/\/$/, ""));
    } catch (e) {
      return url.length > 42 ? url.slice(0, 39) + "…" : url;
    }
  }

  function escapeHtml(s) {
    return String(s == null ? "" : s)
      .replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }
  function escapeAttr(s) {
    return escapeHtml(s).replace(/'/g, "&#39;");
  }

  function setHostStatus(text, level) {
    var hint = $("#ai-host-status");
    if (!hint) return;
    hint.textContent = text || "";
    hint.classList.remove("is-ok", "is-warn", "is-info");
    hint.classList.add(level === "ok" ? "is-ok" : level === "warn" ? "is-warn" : "is-info");
  }

  function setProviderTestBusy(busy) {
    var btn = $("#ai-cfg-test");
    if (!btn) return;
    btn.disabled = !!busy;
    btn.classList.toggle("is-busy", !!busy);
    btn.textContent = busy ? "Testing…" : "Test provider";
  }

  function buildUI() {
    if ($("#ai-fab")) return;

    var fab = el("button", "ai-fab", xmosomeIcon());
    fab.id = "ai-fab";
    fab.type = "button";
    fab.title = "Close BLUPF90 AI assistant";
    fab.setAttribute("aria-label", "Close BLUPF90 AI assistant");
    fab.setAttribute("aria-controls", "ai-panel");
    fab.setAttribute("aria-expanded", "true");

    var panel = el("div", "ai-panel is-open");
    panel.id = "ai-panel";
    panel.setAttribute("role", "region");
    panel.setAttribute("aria-label", "BLUPF90 AI assistant");
    panel.innerHTML = [
      '<div class="ai-panel-head" id="ai-drag-handle">',
      '  <span class="ai-header-mark">' + xmosomeIcon("ai-xmosome-icon--sm") + '</span>',
      '  <div class="ai-title-wrap">',
      '    <span class="ai-title">Xmosome AI</span>',
      '    <span class="ai-subtitle">Parameter card assistant</span>',
      '  </div>',
      '  <button type="button" class="ai-icon-btn" id="ai-settings-btn" title="Settings" aria-label="AI settings" aria-controls="ai-settings" aria-expanded="false">' + aiIcon("settings") + '</button>',
      '  <button type="button" class="ai-icon-btn" id="ai-close-btn" title="Close" aria-label="Close AI assistant">' + aiIcon("close") + '</button>',
      '</div>',
      '<div class="ai-settings" id="ai-settings" hidden>',
      '  <div class="ai-settings-top">',
      '    <div>',
      '      <p class="ai-settings-heading">Connection</p>',
      '      <p class="ai-settings-sub">Provider, model and optional API key</p>',
      '    </div>',
      '    <button type="button" class="ai-text-btn" id="ai-settings-done">Done</button>',
      '  </div>',
      '  <div class="ai-host-status is-info" id="ai-host-status" role="status">Checking server connection…</div>',
      '  <div class="ai-settings-card">',
      '    <label class="ai-field"><span class="ai-field-label">API provider</span>',
      '      <select id="ai-cfg-api-preset">' + optionsHtml(API_PRESETS) + '</select>',
      '      <span class="ai-field-hint">Author configuration uses the key and endpoint set by the app author. Limited to 10 questions per IP per day (UTC) unless you add your own key.</span>',
      '    </label>',
      '    <div class="ai-custom-wrap" id="ai-cfg-base-custom-wrap">',
      '      <label class="ai-field"><span class="ai-field-label">Custom base URL</span>',
      '        <input id="ai-cfg-base-custom" spellcheck="false" placeholder="https://YOUR-RESOURCE.openai.azure.com/openai/v1">',
      '        <span class="ai-field-hint">OpenAI-compatible HTTPS endpoint only.</span>',
      '      </label>',
      '    </div>',
      '    <label class="ai-field"><span class="ai-field-label">Model</span>',
      '      <select id="ai-cfg-model-preset">' + optionsHtml(modelPresetsFor("host")) + '</select>',
      '      <span class="ai-field-hint">Options update when the provider changes.</span>',
      '    </label>',
      '    <div class="ai-custom-wrap" id="ai-cfg-model-custom-wrap">',
      '      <label class="ai-field"><span class="ai-field-label">Custom model id</span>',
      '        <input id="ai-cfg-model-custom" spellcheck="false" placeholder="e.g. openai/gpt-4o-mini">',
      '      </label>',
      '    </div>',
      '    <label class="ai-field"><span class="ai-field-label">API key</span>',
      '      <div class="ai-key-row">',
      '        <input id="ai-cfg-key" type="password" autocomplete="off" placeholder="Optional — own key bypasses daily quota">',
      '        <button type="button" class="ai-text-btn" id="ai-cfg-key-toggle" aria-pressed="false">Show</button>',
      '      </div>',
      '      <span class="ai-field-hint">Stored in this browser only. Leave blank to use the author key (rate-limited).</span>',
      '    </label>',
      '  </div>',
      '  <div class="ai-cfg-summary" id="ai-cfg-summary" aria-live="polite"></div>',
      '  <details class="ai-help ai-settings-help"><summary>Connection help</summary>',
      '    <p><b>Author configuration</b> uses the author-provided key and endpoint. Each IP may send 10 questions per day (UTC). Provider tests do not consume that quota.</p>',
      '    <p>Add your own API key to remove the daily limit and choose any approved provider/model.</p>',
      '    <p>Settings, including the key, stay in this browser and are sent to the app server only when used.</p>',
      '    <p>Hosted apps cannot reach local Ollama. OpenRouter free models end with <code>:free</code>.</p>',
      '  </details>',
      '  <div class="ai-settings-actions">',
      '    <button type="button" class="ai-btn ai-btn-ghost" id="ai-cfg-clear">Clear saved</button>',
      '    <button type="button" class="ai-btn ai-btn-secondary" id="ai-cfg-test">Test provider</button>',
      '    <button type="button" class="ai-btn ai-btn-primary" id="ai-cfg-save">Save connection</button>',
      '  </div>',
      '</div>',
      '<div class="ai-messages" id="ai-messages">',
      '  <div class="ai-welcome" id="ai-welcome">',
      '    <strong>How can I help?</strong>',
      '    <p>Check, explain or draft a BLUPF90 parameter card.</p>',
      '    <div class="ai-quick-prompts" id="ai-quick-prompts">',
      '      <button type="button" data-prompt="Explain the current parameter card in plain language.">Explain card</button>',
      '      <button type="button" data-prompt="Validate the current parameter card and list any problems.">Validate card</button>',
      '      <button type="button" data-prompt="Suggest a clean renumf90 animal model card from my current selections.">Suggest card</button>',
      '    </div>',
      '    <details class="ai-help"><summary>What the assistant can see</summary>',
      '      <p>Your current card, phenotype column names and model selections are included with your question.</p>',
      '      <p>Suggested edits are applied only after you review and accept them. Replies follow the language of your question.</p>',
      '    </details>',
      '  </div>',
      '</div>',
      '<div class="ai-notice" id="ai-notice" role="status" hidden></div>',
      '<div class="ai-compose">',
      '  <textarea id="ai-input" aria-label="Message to AI assistant" placeholder="Ask about your model or parameter card…" rows="2"></textarea>',
      '  <button type="button" class="ai-send" id="ai-send">Send</button>',
      '</div>',
      '<div class="ai-compose-hint">Enter to send · Shift+Enter for a new line</div>'
    ].join("");

    var overlay = el("div", "ai-diff-overlay");
    overlay.id = "ai-diff-overlay";
    overlay.innerHTML = [
      '<div class="ai-diff-card">',
      '  <div class="ai-diff-head"><h3>AI proposal — review changes</h3>',
      '    <button type="button" class="ai-icon-btn" id="ai-diff-x" aria-label="Close proposal">' + aiIcon("close") + '</button></div>',
      '  <div class="ai-diff-body" id="ai-diff-body"></div>',
      '  <div class="ai-diff-foot">',
      '    <button type="button" id="ai-diff-reject">Reject</button>',
      '    <button type="button" class="ai-accept" id="ai-diff-accept">Accept & insert</button>',
      '  </div>',
      '</div>'
    ].join("");

    document.body.appendChild(fab);
    document.body.appendChild(panel);
    document.body.appendChild(overlay);

    function setSettingsOpen(open) {
      state.settingsOpen = !!open;
      var sheet = $("#ai-settings");
      var btn = $("#ai-settings-btn");
      if (sheet) {
        sheet.classList.toggle("is-open", state.settingsOpen);
        if (state.settingsOpen) sheet.removeAttribute("hidden");
        else sheet.setAttribute("hidden", "");
      }
      if (btn) btn.setAttribute("aria-expanded", String(state.settingsOpen));
      panel.classList.toggle("settings-open", state.settingsOpen);
      if (state.settingsOpen) {
        fillSettingsForm();
        updateSettingsChrome();
      }
    }

    fab.addEventListener("click", function () {
      state.open = !state.open;
      panel.classList.toggle("is-open", state.open);
      fab.setAttribute("aria-expanded", String(state.open));
      fab.setAttribute("aria-label", state.open ? "Close BLUPF90 AI assistant" : "Open BLUPF90 AI assistant");
      fab.title = fab.getAttribute("aria-label");
      if (state.open) {
        pushCfgToShiny();
        updateSettingsChrome();
      }
    });
    $("#ai-close-btn").addEventListener("click", function () {
      state.open = false;
      setSettingsOpen(false);
      panel.classList.remove("is-open");
      fab.setAttribute("aria-expanded", "false");
      fab.setAttribute("aria-label", "Open BLUPF90 AI assistant");
      fab.title = fab.getAttribute("aria-label");
      fab.focus();
    });
    $("#ai-settings-btn").addEventListener("click", function () {
      setSettingsOpen(!state.settingsOpen);
    });
    $("#ai-settings-done").addEventListener("click", function () {
      setSettingsOpen(false);
      $("#ai-settings-btn").focus();
    });
    $("#ai-cfg-api-preset").addEventListener("change", function () {
      refreshModelOptions();
      syncCustomVisibility();
      updateConnectionSummary();
    });
    $("#ai-cfg-model-preset").addEventListener("change", function () {
      syncCustomVisibility();
      updateConnectionSummary();
    });
    ["ai-cfg-base-custom", "ai-cfg-model-custom", "ai-cfg-key"].forEach(function (id) {
      var node = $("#" + id);
      if (node) node.addEventListener("input", updateConnectionSummary);
    });
    $("#ai-cfg-key-toggle").addEventListener("click", function () {
      var input = $("#ai-cfg-key");
      var btn = $("#ai-cfg-key-toggle");
      if (!input || !btn) return;
      var show = input.type === "password";
      input.type = show ? "text" : "password";
      btn.textContent = show ? "Hide" : "Show";
      btn.setAttribute("aria-pressed", String(show));
    });
    $("#ai-cfg-save").addEventListener("click", function () {
      var cfg = readFormCfg();
      saveCfg(cfg);
      updateSettingsChrome();
      showNotice("Connection settings saved in this browser.");
      setSettingsOpen(false);
      $("#ai-settings-btn").focus();
    });
    $("#ai-cfg-clear").addEventListener("click", function () {
      saveCfg({});
      fillSettingsForm();
      updateSettingsChrome();
      showNotice("Saved overrides cleared. Author configuration will be used.");
    });
    $("#ai-cfg-test").addEventListener("click", function () {
      if (!window.Shiny || !Shiny.setInputValue) {
        setHostStatus("Not connected to the app server.", "warn");
        return;
      }
      onShinyReady();
      var btn = $("#ai-cfg-test");
      if (btn && btn.disabled) return;
      var cfg = readFormCfg();
      Shiny.setInputValue("ai_user_cfg", cfg, { priority: "event" });
      setProviderTestBusy(true);
      setHostStatus("Testing provider…", "info");
      // Defer the test trigger so ai_user_cfg is flushed first.
      setTimeout(function () {
        Shiny.setInputValue("ai_test_provider", { nonce: Date.now() }, { priority: "event" });
      }, 40);
    });
    $("#ai-send").addEventListener("click", sendMessage);
    $("#ai-input").addEventListener("keydown", function (e) {
      if (e.key === "Enter" && !e.shiftKey && !e.isComposing && e.keyCode !== 229) {
        e.preventDefault();
        sendMessage();
      }
    });
    var prompts = $("#ai-quick-prompts");
    if (prompts) {
      prompts.addEventListener("click", function (e) {
        var btn = e.target.closest("[data-prompt]");
        if (!btn) return;
        var ta = $("#ai-input");
        if (ta) {
          ta.value = btn.getAttribute("data-prompt") || "";
          ta.focus();
        }
      });
    }

    $("#ai-diff-x").addEventListener("click", closeDiff);
    $("#ai-diff-reject").addEventListener("click", function () {
      closeDiff();
      if (window.Shiny) Shiny.setInputValue("ai_diff_reject", Date.now(), { priority: "event" });
      showNotice("Proposal dismissed. Card unchanged.");
    });
    $("#ai-diff-accept").addEventListener("click", function () {
      if (window.Shiny) Shiny.setInputValue("ai_diff_accept", Date.now(), { priority: "event" });
    });

    document.addEventListener("keydown", function (e) {
      if (e.key !== "Escape" || !state.open) return;
      if ($("#ai-diff-overlay") && $("#ai-diff-overlay").classList.contains("is-open")) {
        closeDiff();
        return;
      }
      if (state.settingsOpen) {
        setSettingsOpen(false);
        return;
      }
      $("#ai-close-btn").click();
    });

    enableDrag(panel, $("#ai-drag-handle"));
    updateSettingsChrome();
    // Start expanded on first paint; push cfg once Shiny is ready via bindShiny.
    pushCfgToShiny();
  }

  function fillSettingsForm() {
    var cfg = loadCfg();
    var apiPreset = cfg.api_preset || "host";
    var modelPreset = cfg.model_preset || "host";

    // Migrate legacy free-text saves
    if (!cfg.api_preset && cfg.base_url) {
      var hit = API_PRESETS.find(function (p) { return p.url && p.url === cfg.base_url; });
      apiPreset = hit ? hit.id : "custom";
      if (apiPreset === "custom") cfg.base_url_custom = cfg.base_url;
    }
    if (!cfg.model_preset && cfg.model) {
      var mhit = allModelPresets().find(function (p) { return p.value && p.value === cfg.model; });
      modelPreset = mhit ? mhit.id : "custom";
      if (modelPreset === "custom") cfg.model_custom = cfg.model;
    }

    $("#ai-cfg-api-preset").value = apiPreset;
    refreshModelOptions(modelPreset);
    $("#ai-cfg-base-custom").value = cfg.base_url_custom || "";
    $("#ai-cfg-model-custom").value = cfg.model_custom || "";
    $("#ai-cfg-key").value = cfg.api_key || "";
    syncCustomVisibility();
    updateConnectionSummary();
  }

  // Routine feedback stays in one compact status area, outside the conversation.
  function showNotice(text) {
    var notice = $("#ai-notice");
    if (!notice) return;
    notice.textContent = text;
    notice.hidden = !text;
  }

  function looksLikeParamCard(text) {
    var t = String(text || "");
    return /\bDATAFILE\b/.test(t) && /\bTRAITS\b/.test(t) && /\bEFFECT\b/.test(t);
  }

  // Recover a card the model forgot to wrap in <<<PAR_PROPOSAL>>> markers.
  function extractClientProposal(text) {
    var raw = String(text || "");
    var marked = raw.match(/<<<PAR_PROPOSAL>>>\s*([\s\S]*?)\s*<<<END_PAR_PROPOSAL>>>/);
    if (marked && looksLikeParamCard(marked[1])) {
      return {
        reply: raw.replace(/<<<PAR_PROPOSAL>>>[\s\S]*<<<END_PAR_PROPOSAL>>>/, "").trim() ||
          "I prepared an updated parameter card. Click Review changes to preview the diff.",
        proposal: marked[1].trim()
      };
    }
    var fence = raw.match(/```(?:[a-zA-Z0-9_-]+)?\s*\n([\s\S]*?)```/);
    if (fence && looksLikeParamCard(fence[1])) {
      return {
        reply: raw.replace(fence[0], "").trim() ||
          "I prepared an updated parameter card. Click Review changes to preview the diff.",
        proposal: fence[1].trim()
      };
    }
    return null;
  }

  function escapeHtml(s) {
    return String(s == null ? "" : s)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  // Small, dependency-free Markdown subset for assistant bubbles.
  function renderMarkdown(src) {
    var text = String(src == null ? "" : src).replace(/\r\n/g, "\n");
    var blocks = [];
    text = text.replace(/```([a-zA-Z0-9_-]*)\s*\n([\s\S]*?)```/g, function (_, lang, code) {
      var i = blocks.length;
      blocks.push(
        '<pre class="ai-md-pre"><code class="ai-md-code"' +
          (lang ? ' data-lang="' + escapeHtml(lang) + '"' : "") +
          ">" + escapeHtml(code.replace(/\n$/, "")) + "</code></pre>"
      );
      return "\n%%AI_BLOCK_" + i + "%%\n";
    });
    text = escapeHtml(text);
    text = text.replace(/`([^`\n]+)`/g, '<code class="ai-md-inline">$1</code>');
    text = text.replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
    text = text.replace(/(^|[^*\w])\*([^*\n]+)\*(?!\*)/g, "$1<em>$2</em>");
    text = text.replace(/^### (.+)$/gm, "<h4>$1</h4>");
    text = text.replace(/^## (.+)$/gm, "<h3>$1</h3>");
    text = text.replace(/^# (.+)$/gm, "<h3>$1</h3>");
    text = text.replace(/\[([^\]]+)\]\((https?:\/\/[^)\s]+)\)/g, '<a href="$2" target="_blank" rel="noopener noreferrer">$1</a>');

    var lines = text.split("\n");
    var out = [];
    var listType = null;
    function closeList() {
      if (!listType) return;
      out.push(listType === "ol" ? "</ol>" : "</ul>");
      listType = null;
    }
    for (var i = 0; i < lines.length; i++) {
      var line = lines[i];
      var bm = line.match(/^%%AI_BLOCK_(\d+)%%$/);
      if (bm) {
        closeList();
        out.push(blocks[Number(bm[1])] || "");
        continue;
      }
      var ul = line.match(/^[-*•]\s+(.+)$/);
      if (ul) {
        if (listType !== "ul") { closeList(); out.push("<ul>"); listType = "ul"; }
        out.push("<li>" + ul[1] + "</li>");
        continue;
      }
      var ol = line.match(/^\d+\.\s+(.+)$/);
      if (ol) {
        if (listType !== "ol") { closeList(); out.push("<ol>"); listType = "ol"; }
        out.push("<li>" + ol[1] + "</li>");
        continue;
      }
      closeList();
      if (/^<h[34]>/.test(line) || !line.trim()) {
        out.push(line);
      } else {
        out.push("<p>" + line + "</p>");
      }
    }
    closeList();
    return out.join("\n");
  }

  function appendBubble(role, text, proposalId) {
    var box = $("#ai-messages");
    if (!box) return;
    var welcome = $("#ai-welcome");
    if (welcome && role === "user") welcome.remove();
    var displayText = text || "";
    var localProposal = null;
    if (role === "assistant" && !proposalId) {
      localProposal = extractClientProposal(displayText);
      if (localProposal) {
        displayText = localProposal.reply;
        proposalId = "local-" + Date.now();
        state.localProposals = state.localProposals || {};
        state.localProposals[proposalId] = localProposal.proposal;
      }
    }
    var b = el("div", "ai-bubble " + role);
    if (role === "assistant") {
      b.className += " ai-md";
      b.innerHTML = renderMarkdown(displayText);
    } else if (role === "system") {
      b.className += " ai-md";
      b.innerHTML = renderMarkdown(displayText);
    } else {
      b.textContent = displayText || "";
    }
    if (proposalId) {
      var actions = el("div", "ai-proposal-actions");
      var insert = el("button", "ai-insert");
      insert.type = "button";
      insert.textContent = "Review changes";
      insert.addEventListener("click", function () {
        if (state.localProposals && state.localProposals[proposalId]) {
          // Local recovery: ask server to stage this card text, then show diff.
          if (window.Shiny) {
            Shiny.setInputValue("ai_local_proposal", {
              id: proposalId,
              text: state.localProposals[proposalId],
              nonce: Date.now()
            }, { priority: "event" });
          }
          return;
        }
        if (window.Shiny) {
          Shiny.setInputValue("ai_request_diff", { id: proposalId, nonce: Date.now() }, { priority: "event" });
        }
      });
      var dismiss = el("button");
      dismiss.type = "button";
      dismiss.textContent = "Dismiss";
      dismiss.addEventListener("click", function () {
        actions.remove();
        if (state.localProposals) delete state.localProposals[proposalId];
        if (window.Shiny) Shiny.setInputValue("ai_dismiss_proposal", proposalId, { priority: "event" });
      });
      actions.appendChild(insert);
      actions.appendChild(dismiss);
      b.appendChild(actions);
    }
    box.appendChild(b);
    box.scrollTop = box.scrollHeight;
  }

  function clearBusyTimer() {
    if (state.busyTimer) {
      clearTimeout(state.busyTimer);
      state.busyTimer = null;
    }
  }

  function setBusy(busy) {
    state.busy = !!busy;
    clearBusyTimer();
    var btn = $("#ai-send");
    var fab = $("#ai-fab");
    var box = $("#ai-messages");
    if (btn) {
      btn.disabled = state.busy;
      btn.textContent = state.busy ? "Waiting" : "Send";
    }
    if (fab) fab.classList.toggle("is-busy", state.busy);
    if (box) box.setAttribute("aria-busy", String(state.busy));
    var pending = $("#ai-waiting");
    if (!state.busy) {
      if (pending) pending.remove();
      return;
    }
    // Client watchdog: server HTTP timeout is 90s; unlock UI if reply never arrives.
    state.busyTimer = setTimeout(function () {
      if (!state.busy) return;
      setBusy(false);
      appendBubble("system", "⚠ No response after 95s. The AI reply handler may not be connected, or the provider timed out — reopen the panel and try again.");
    }, 95000);
    if (box && !pending) {
      pending = el("div", "ai-waiting",
        '<span class="ai-wait-mark">' + xmosomeIcon("ai-xmosome-icon--sm") + '</span>' +
        '<span class="ai-wait-copy"><strong>Waiting for a response</strong></span>' +
        '<span class="ai-wait-dots" aria-hidden="true"><i></i><i></i><i></i></span>');
      pending.id = "ai-waiting";
      box.appendChild(pending);
      box.scrollTop = box.scrollHeight;
    }
  }

  function sendMessage() {
    if (state.busy) return;
    var ta = $("#ai-input");
    var msg = (ta.value || "").trim();
    if (!msg) return;
    if (!window.Shiny || !Shiny.setInputValue) {
      appendBubble("system", "Not connected. Please reconnect before sending.");
      return;
    }
    // Guarantee reply handlers exist (Shiny can wipe early registrations during init).
    onShinyReady();
    showNotice("");
    ta.value = "";
    appendBubble("user", msg);
    setBusy(true);
    if (state.settingsOpen) saveCfg(readFormCfg());
    else pushCfgToShiny();
    if (window.Shiny) {
      Shiny.setInputValue("ai_user_message", { text: msg, nonce: Date.now() }, { priority: "event" });
    }
  }

  function showDiff(rows) {
    var body = $("#ai-diff-body");
    body.innerHTML = "";
    (rows || []).forEach(function (row) {
      var line = el("div", "ai-diff-line " + (row.type || "ctx"));
      var g = el("span", "gutter");
      g.textContent = row.type === "add" ? "+" : row.type === "del" ? "−" : " ";
      var t = el("span");
      t.textContent = row.text == null ? "" : String(row.text);
      line.appendChild(g);
      line.appendChild(t);
      body.appendChild(line);
    });
    $("#ai-diff-overlay").classList.add("is-open");
  }

  function closeDiff() {
    $("#ai-diff-overlay").classList.remove("is-open");
  }

  function enableDrag(panel, handle) {
    handle.addEventListener("mousedown", function (e) {
      if (e.button !== 0) return;
      // Buttons in the header must receive click; don't start a drag from them.
      if (e.target && e.target.closest && e.target.closest("button, a, input, select, textarea, label")) {
        return;
      }
      var rect = panel.getBoundingClientRect();
      state.drag = {
        dx: e.clientX - rect.left,
        dy: e.clientY - rect.top
      };
      panel.style.right = "auto";
      panel.style.bottom = "auto";
      panel.style.left = rect.left + "px";
      panel.style.top = rect.top + "px";
      e.preventDefault();
    });
    window.addEventListener("mousemove", function (e) {
      if (!state.drag) return;
      var x = Math.max(8, Math.min(window.innerWidth - 80, e.clientX - state.drag.dx));
      var y = Math.max(8, Math.min(window.innerHeight - 80, e.clientY - state.drag.dy));
      panel.style.left = x + "px";
      panel.style.top = y + "px";
    });
    window.addEventListener("mouseup", function () { state.drag = null; });
  }

  function bindShinyHandlers(force) {
    if (state.handlersBound && !force) return true;
    if (!window.Shiny || !Shiny.addCustomMessageHandler) return false;
    // Re-register every time: early registration can be wiped when shiny.js finishes init.
    Shiny.addCustomMessageHandler("ai_chat_reply", function (msg) {
      setBusy(false);
      appendBubble("assistant", (msg && msg.reply) || "", (msg && msg.proposal_id) || null);
    });
    Shiny.addCustomMessageHandler("ai_chat_error", function (msg) {
      setBusy(false);
      appendBubble("system", "⚠ " + ((msg && msg.error) || "AI request failed"));
    });
    Shiny.addCustomMessageHandler("ai_show_diff", function (msg) {
      showDiff((msg && msg.rows) || []);
    });
    Shiny.addCustomMessageHandler("ai_diff_applied", function (msg) {
      closeDiff();
      showNotice("Changes applied. Manual lock is on.");
      var ed = document.getElementById("param_editor");
      if (ed) {
        ed.classList.add("ai-just-patched");
        setTimeout(function () { ed.classList.remove("ai-just-patched"); }, 1400);
      }
    });
    Shiny.addCustomMessageHandler("ai_status", function (msg) {
      if (!msg || !msg.text) return;
      var level = msg.level || (/no author key|limit reached|no server key/i.test(msg.text) ? "warn" : "ok");
      setHostStatus(msg.text, level);
      if (msg.quota && msg.quota.limit != null) {
        var q = msg.quota;
        var elQuota = $("#ai-quota-chip");
        if (!elQuota) {
          var head = $("#ai-drag-handle");
          var title = head && head.querySelector(".ai-title-wrap");
          if (title) {
            elQuota = document.createElement("span");
            elQuota.id = "ai-quota-chip";
            elQuota.className = "ai-quota-chip";
            title.appendChild(elQuota);
          }
        }
        if (elQuota) {
          elQuota.hidden = false;
          elQuota.textContent = (q.remaining != null ? q.remaining : "?") + "/" + q.limit + " left today";
          elQuota.title = "Author-funded questions remaining for this IP today (UTC)";
        }
      }
    });
    Shiny.addCustomMessageHandler("ai_test_result", function (msg) {
      setProviderTestBusy(false);
      if (!msg) return;
      var level = msg.level || (msg.ok ? "ok" : "warn");
      var text = msg.text || (msg.ok ? "Provider OK." : "Provider test failed.");
      setHostStatus(text, level);
      showNotice(text);
    });
    state.handlersBound = true;
    return true;
  }

  function requestStatusOnce() {
    if (state.statusRequested) return;
    if (!window.Shiny || !Shiny.setInputValue) return;
    state.statusRequested = true;
    try {
      Shiny.setInputValue("ai_client_ready", Date.now(), { priority: "event" });
    } catch (e) { state.statusRequested = false; }
    pushCfgToShiny();
  }

  function onShinyReady() {
    // Force rebind: Shiny's handler map can be recreated during startup.
    state.handlersBound = false;
    bindShinyHandlers(true);
    requestStatusOnce();
    if (window.jQuery) {
      window.jQuery(document).off("shiny:disconnected.xmosomeAi");
      window.jQuery(document).on("shiny:disconnected.xmosomeAi", function () {
        state.handlersBound = false;
        state.statusRequested = false;
        if (!state.busy) return;
        setBusy(false);
        appendBubble("system", "Connection lost. Reconnect and resend your request.");
      });
    }
  }

  function bindShiny() {
    if (window.jQuery) {
      window.jQuery(document).off("shiny:connected.xmosomeAi");
      window.jQuery(document).on("shiny:connected.xmosomeAi", onShinyReady);
    }
    document.addEventListener("shiny:sessioninitialized", onShinyReady);
    // Poll until websocket is up, then keep re-binding briefly (init can wipe handlers).
    var tries = 0;
    var timer = setInterval(function () {
      tries += 1;
      var ready = window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket &&
        Shiny.shinyapp.$socket.readyState === 1;
      if (ready) {
        onShinyReady();
        if (tries > 60) clearInterval(timer); // ~3s of rebinds after connect
      } else if (tries > 300) {
        clearInterval(timer);
      }
    }, 50);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () { buildUI(); bindShiny(); });
  } else {
    buildUI();
    bindShiny();
  }
})();
