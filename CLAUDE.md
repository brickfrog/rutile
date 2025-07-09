rutile — product spec v0.3
*(stack-based agent dsl atop racket, wired for llm orchestration + commercial http stacks)*

---

### 1. the pitch

build autonomous workflows by **composing verbs, not nesting yaml**. rutile is a minimalist concatenative lang (think forth x joy) that lives inside racket, speaks http out of the box, and treats llm calls as first-class words. scripts = binary-size gemstones you can hot-swap into prod.

---

### 2. core pillars

| pillar                | what it means                                                           | why anyone cares                                                         |
| --------------------- | ----------------------------------------------------------------------- | ------------------------------------------------------------------------ |
| **concatenative dna** | single implicit stack, words = pure funcs, composition by juxtaposition | zero boilerplate, trivial metaprogramming, agents can self-rewrite       |
| **racket host**       | macro nirvana, places/futures concurrency, embeddable bytecode vm       | instant prototyping, cross-plat binaries, macro-driven domain extensions |
| **llm primitive**     | `llm.call` word: `{prompt cfg}` → streamed tokens on stack              | no glue code; chain, map, or fork llm calls like any other op            |
| **http native**       | client: async chunked; server: micro http router in stdlib              | integrate slack, webhooks, whatever, w/o leaving rutile                  |
| **phos bridge**       | word‐level drivers for redis-style store (`phos.get`, `phos.set`)       | durable state for multi-agent coordination                               |
| **open-core**         | MPL-2.0 runtime, enterprise license adds observability + policy         | free hacker adoption, paid SRE niceties                                  |

---

### 3. language sketch

```rut
: summarize
  llm.default_cfg swap merge       # push cfg, merge w/ custom
  llm.call                         # {text cfg} -> summary

: pipeline
  "fetch updates" {temperature 0.2} llm.call
  summarize
  phos.set

"http://callback" @webhook.listen  # http server in 1 word
```

*syntax*: words space-delimited, `:` … `;` define new words, `#` comments, `{}` maps, `@` sigils for event sources.

---

### 4. runtime architecture

```
┌─────────────────┐
│ rutile vm       │  byte-code, 2 stacks (data / return)
├─────────────────┤
│ ffi bridge      │  racket ffi → rust nifs (http, tls, blake3, simd json)
├─────────────────┤
│ stdlib          │  http, llm, phos, time, json, crypto
└─────────────────┘
```

concurrency: each **agent** = racket place; message passing by immutable queues; supervisor tree auto-restarts crashed agents (erlang-lite).

---

### 5. dev workflow

```
$ rut new bot
$ rut repl
> :reload
```

* hot reload via racket’s compile-incremental
* `rut test` runs property tests (check-expect style)
* `rut build --static` = one file, no external deps

---

### 6. commercial hooks

* **rutile cloud** (SaaS): hosted llm endpoints + phos kv + tracing ui
* **enterprise agent firewall**: rate-limit, redact, sign prompts
* **obsidian panel**: flamegraphs of word execution, stack snapshots in real time

---

### 7. roadmap (aggressive)

| month | milestone                                      |
| ----- | ---------------------------------------------- |
| m1    | public repo, repl, `llm.call` via openai api   |
| m3    | http server/client, phos driver, docker image  |
| m6    | place-supervisor, wasm target beta             |
| m9    | enterprise observability, on-prem installer    |
| m12   | 1.0 ga, formal spec frozen, plugin marketplace |

---

### 8. risks / mitigations

* **beam envy**: racket places < erlang sched. → spike perf early, accept cpu-bound limits.
* **llm churn**: adapter layer isolates vendor apis.
* **dev adoption**: ship killer demos (slack bot <50 loc, cronless newsletter, etc).

---

### 9. why now

post-chatgpt, everyone is shell-scripting llms in yaml. boring & brittle. rutile offers *composable, inspectable, REPL-able* agent logic with zero json foot-guns. gems > glue.

---

done. shove it on gitee, tape a lisa frank sticker on the readme, profit.

